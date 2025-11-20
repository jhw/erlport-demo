-module(python_pool).
-behaviour(gen_server).

%% API
-export([start_link/2, call_python/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    pool_name :: atom(),
    supervisor_pid :: pid(),
    request_queue :: queue:queue(),
    available_workers :: queue:queue(),
    busy_workers :: sets:set()
}).

%%====================================================================
%% API functions
%%====================================================================

start_link(PoolName, SupervisorPid) ->
    gen_server:start_link({local, PoolName}, ?MODULE, [PoolName, SupervisorPid], []).

%% Call Python function asynchronously - result sent as {python_result, WorkerPid, Result}
call_python(PoolName, {Module, Function, Args}, CallerPid) ->
    gen_server:cast(PoolName, {call_python, Module, Function, Args, CallerPid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([PoolName, SupervisorPid]) ->
    process_flag(trap_exit, true),

    % Get initial workers from supervisor
    Workers = supervisor:which_children(SupervisorPid),
    WorkerPids = [Pid || {_Id, Pid, _Type, _Modules} <- Workers, is_pid(Pid)],

    % Monitor all workers
    lists:foreach(fun(Pid) -> erlang:monitor(process, Pid) end, WorkerPids),

    AvailableWorkers = queue:from_list(WorkerPids),

    error_logger:info_msg("Python pool ~p started with ~p workers~n",
        [PoolName, length(WorkerPids)]),

    {ok, #state{
        pool_name = PoolName,
        supervisor_pid = SupervisorPid,
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
    % A worker died - supervisor will restart it
    error_logger:warning_msg("Python worker ~p died: ~p, will be restarted by supervisor~n",
        [Pid, Reason]),

    % Remove from busy set if present
    NewBusy = sets:del_element(Pid, State#state.busy_workers),

    % Remove from available queue if present
    AvailableList = queue:to_list(State#state.available_workers),
    NewAvailableList = lists:delete(Pid, AvailableList),
    NewAvailable = queue:from_list(NewAvailableList),

    % Wait a bit then query supervisor for new worker
    erlang:send_after(100, self(), check_new_workers),

    {noreply, State#state{
        available_workers = NewAvailable,
        busy_workers = NewBusy
    }};

handle_info(check_new_workers, State) ->
    % Get current workers from supervisor
    Workers = supervisor:which_children(State#state.supervisor_pid),
    WorkerPids = [Pid || {_Id, Pid, _Type, _Modules} <- Workers, is_pid(Pid)],

    % Find workers we're not tracking yet
    CurrentAvailable = sets:from_list(queue:to_list(State#state.available_workers)),
    AllTracked = sets:union(CurrentAvailable, State#state.busy_workers),
    AllWorkers = sets:from_list(WorkerPids),
    NewWorkers = sets:to_list(sets:subtract(AllWorkers, AllTracked)),

    % Monitor new workers and add to available queue
    lists:foreach(fun(Pid) -> erlang:monitor(process, Pid) end, NewWorkers),
    NewAvailable = lists:foldl(
        fun(Pid, Q) -> queue:in(Pid, Q) end,
        State#state.available_workers,
        NewWorkers
    ),

    case length(NewWorkers) of
        0 -> ok;
        N -> error_logger:info_msg("Python pool ~p discovered ~p new workers~n",
            [State#state.pool_name, N])
    end,

    {noreply, State#state{available_workers = NewAvailable}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
