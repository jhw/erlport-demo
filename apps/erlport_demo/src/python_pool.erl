-module(python_pool).
-behaviour(gen_server).

%% API
-export([start_link/3, call_python/3, call_and_await/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    pool_name :: atom(),
    script_name :: atom(),
    pool_size :: integer(),
    request_queue :: queue:queue(),
    available_workers :: queue:queue(),
    busy_workers :: sets:set()
}).

%%====================================================================
%% API functions
%%====================================================================

start_link(PoolName, ScriptName, PoolSize) ->
    gen_server:start_link({local, PoolName}, ?MODULE, [PoolName, ScriptName, PoolSize], []).

%% Call Python function asynchronously - result sent as {python_result, WorkerPid, Result}
call_python(PoolName, {Module, Function, Args}, CallerPid) ->
    gen_server:cast(PoolName, {call_python, Module, Function, Args, CallerPid}).

%% Call Python function and await result synchronously
%% Returns {ok, Result} | {error, Reason}
%%
%% This is a convenience wrapper that:
%% 1. Calls call_python/3
%% 2. Receives {python_result, WorkerPid, Result}
%% 3. Handles timeout
%%
%% Example:
%%   case python_pool:call_and_await(bbc_pool, {bbc_scraper, parse, [Body]}, 30000) of
%%       {ok, ParsedResults} -> ...;
%%       {error, timeout} -> ...;
%%       {error, Reason} -> ...
%%   end
%%
call_and_await(PoolName, {Module, Function, Args}, Timeout) ->
    call_python(PoolName, {Module, Function, Args}, self()),
    receive
        {python_result, _WorkerPid, Result} -> Result
    after Timeout ->
        {error, timeout}
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([PoolName, ScriptName, PoolSize]) ->
    process_flag(trap_exit, true),

    % Spawn workers directly
    Workers = lists:map(
        fun(_) ->
            {ok, Pid} = python_worker:start_link(ScriptName),
            erlang:monitor(process, Pid),
            Pid
        end,
        lists:seq(1, PoolSize)
    ),

    AvailableWorkers = queue:from_list(Workers),

    error_logger:info_msg("Python pool ~p started with ~p workers~n",
        [PoolName, PoolSize]),

    {ok, #state{
        pool_name = PoolName,
        script_name = ScriptName,
        pool_size = PoolSize,
        request_queue = queue:new(),
        available_workers = AvailableWorkers,
        busy_workers = sets:new()
    }}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({call_python, Module, Function, Args, CallerPid}, State) ->
    case queue:out(State#state.available_workers) of
        {{value, Worker}, NewAvailable} ->
            % Worker available, assign immediately
            python_worker:call_python(Worker, Module, Function, Args),
            NewBusy = sets:add_element(Worker, State#state.busy_workers),
            {noreply, State#state{
                available_workers = NewAvailable,
                busy_workers = NewBusy
            }};
        {empty, _} ->
            % No workers available, queue the request
            Request = {Module, Function, Args, CallerPid},
            NewQueue = queue:in(Request, State#state.request_queue),
            {noreply, State#state{request_queue = NewQueue}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({python_result, WorkerPid, _Result}, State) ->
    % Worker finished, mark as available
    case sets:is_element(WorkerPid, State#state.busy_workers) of
        true ->
            NewBusy = sets:del_element(WorkerPid, State#state.busy_workers),

            % Check if there are queued requests
            case queue:out(State#state.request_queue) of
                {{value, {Module, Function, Args, _CallerPid}}, NewQueue} ->
                    % Assign queued request to this worker
                    python_worker:call_python(WorkerPid, Module, Function, Args),
                    {noreply, State#state{
                        request_queue = NewQueue,
                        busy_workers = sets:add_element(WorkerPid, NewBusy)
                    }};
                {empty, _} ->
                    % No queued requests, mark worker as available
                    NewAvailable = queue:in(WorkerPid, State#state.available_workers),
                    {noreply, State#state{
                        available_workers = NewAvailable,
                        busy_workers = NewBusy
                    }}
            end;
        false ->
            % Worker wasn't tracked as busy, ignore
            {noreply, State}
    end;

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    % A worker died - restart it ourselves
    case Reason of
        normal -> ok;
        _ -> error_logger:warning_msg("Python worker ~p died: ~p, restarting~n", [Pid, Reason])
    end,

    % Remove from busy set if present
    NewBusy = sets:del_element(Pid, State#state.busy_workers),

    % Remove from available queue if present
    AvailableList = queue:to_list(State#state.available_workers),
    NewAvailableList = lists:delete(Pid, AvailableList),
    NewAvailable = queue:from_list(NewAvailableList),

    % Spawn replacement worker
    case python_worker:start_link(State#state.script_name) of
        {ok, NewPid} ->
            erlang:monitor(process, NewPid),

            % Check if there are queued requests
            case queue:out(State#state.request_queue) of
                {{value, {Module, Function, Args, _CallerPid}}, NewQueue} ->
                    % Assign queued request to new worker
                    python_worker:call_python(NewPid, Module, Function, Args),
                    {noreply, State#state{
                        available_workers = NewAvailable,
                        busy_workers = sets:add_element(NewPid, NewBusy),
                        request_queue = NewQueue
                    }};
                {empty, _} ->
                    % No queued requests, add to available
                    {noreply, State#state{
                        available_workers = queue:in(NewPid, NewAvailable),
                        busy_workers = NewBusy
                    }}
            end;
        {error, RestartReason} ->
            error_logger:error_msg("Failed to restart Python worker: ~p~n", [RestartReason]),
            {noreply, State#state{
                available_workers = NewAvailable,
                busy_workers = NewBusy
            }}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
