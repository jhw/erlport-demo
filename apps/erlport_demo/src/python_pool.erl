-module(python_pool).
-behaviour(gen_server).

%% API
-export([start_link/3, call_python/3, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    script_name :: atom(),
    pool_size :: integer(),
    python_instances :: [pid()],
    request_queue :: queue:queue(),
    available_workers :: queue:queue()
}).

%%====================================================================
%% API functions
%%====================================================================

start_link(Name, ScriptName, PoolSize) ->
    gen_server:start_link({local, Name}, ?MODULE, [ScriptName, PoolSize], []).

%% Call Python function asynchronously with callback
call_python(PoolName, {Module, Function, Args}, Callback) ->
    gen_server:cast(PoolName, {call_python, Module, Function, Args, Callback, self()}).

stop(PoolName) ->
    gen_server:stop(PoolName).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([ScriptName, PoolSize]) ->
    process_flag(trap_exit, true),

    % Get the path to the priv/python directory
    PrivDir = code:priv_dir(erlport_demo),
    PythonPath = filename:join(PrivDir, "python"),

    % Start Python instances
    Instances = lists:map(
        fun(_) ->
            {ok, Pid} = python:start([{python_path, PythonPath}]),
            Pid
        end,
        lists:seq(1, PoolSize)
    ),

    % All workers start as available
    AvailableWorkers = queue:from_list(Instances),

    error_logger:info_msg("Python pool ~p started with ~p workers~n", [ScriptName, PoolSize]),

    {ok, #state{
        script_name = ScriptName,
        pool_size = PoolSize,
        python_instances = Instances,
        request_queue = queue:new(),
        available_workers = AvailableWorkers
    }}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({call_python, Module, Function, Args, Callback, CallerPid}, State) ->
    case queue:out(State#state.available_workers) of
        {{value, Worker}, NewAvailable} ->
            % Worker available, process immediately
            spawn_link(fun() ->
                process_request(Worker, Module, Function, Args, Callback, CallerPid, self())
            end),
            {noreply, State#state{available_workers = NewAvailable}};
        {empty, _} ->
            % No workers available, queue the request
            Request = {Module, Function, Args, Callback, CallerPid},
            NewQueue = queue:in(Request, State#state.request_queue),
            {noreply, State#state{request_queue = NewQueue}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({worker_available, Worker}, State) ->
    % Worker finished, check if there are queued requests
    case queue:out(State#state.request_queue) of
        {{value, {Module, Function, Args, Callback, CallerPid}}, NewQueue} ->
            % Process queued request
            spawn_link(fun() ->
                process_request(Worker, Module, Function, Args, Callback, CallerPid, self())
            end),
            {noreply, State#state{request_queue = NewQueue}};
        {empty, _} ->
            % No queued requests, add worker back to available pool
            NewAvailable = queue:in(Worker, State#state.available_workers),
            {noreply, State#state{available_workers = NewAvailable}}
    end;

handle_info({'EXIT', Pid, Reason}, State) ->
    case lists:member(Pid, State#state.python_instances) of
        true ->
            error_logger:error_msg("Python worker ~p died: ~p~n", [Pid, Reason]),
            % Restart the worker
            PrivDir = code:priv_dir(erlport_demo),
            PythonPath = filename:join(PrivDir, "python"),
            {ok, NewPid} = python:start([{python_path, PythonPath}]),

            % Replace in instances list
            NewInstances = lists:map(
                fun(P) when P =:= Pid -> NewPid;
                   (P) -> P
                end,
                State#state.python_instances
            ),

            % Add to available workers
            NewAvailable = queue:in(NewPid, State#state.available_workers),

            {noreply, State#state{
                python_instances = NewInstances,
                available_workers = NewAvailable
            }};
        false ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Stop all Python instances
    lists:foreach(
        fun(Pid) ->
            catch python:stop(Pid)
        end,
        State#state.python_instances
    ),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

process_request(Worker, Module, Function, Args, Callback, CallerPid, PoolPid) ->
    try
        Result = python:call(Worker, Module, Function, Args),
        % Call the callback with the result
        Callback(CallerPid, {ok, Result})
    catch
        Error:Reason:Stacktrace ->
            error_logger:error_msg(
                "Python call failed: ~p:~p~nStacktrace: ~p~n",
                [Error, Reason, Stacktrace]
            ),
            Callback(CallerPid, {error, {Error, Reason}})
    after
        % Notify pool that worker is available
        PoolPid ! {worker_available, Worker}
    end.
