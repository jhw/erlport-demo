-module(python_pool).
-behaviour(gen_server).

%%% Python Worker Pool Manager
%%%
%%% Manages a pool of Python workers for concurrent task execution.
%%% Follows OTP best practices with clear separation of concerns:
%%%
%%% ARCHITECTURE:
%%%   python_pool_sup (rest_for_one)
%%%     ├─ python_worker_sup (simple_one_for_one) - Manages worker lifecycle
%%%     └─ python_pool (gen_server) - Manages work distribution
%%%
%%% RESPONSIBILITIES:
%%%   - python_worker_sup: Spawns, monitors, restarts workers (lifecycle)
%%%   - python_pool: Tracks busy/available workers, queues requests (work distribution)
%%%   - python_worker: Executes Python code, self-registers with pool
%%%
%%% MONITORING PATTERN:
%%%   - Supervisor monitors workers for LIFECYCLE (restart on crash, SASL reports)
%%%   - Pool monitors workers for WORK STATE (remove from busy set if dies mid-task)
%%%   - Dual monitoring is intentional - different purposes, clean separation
%%%
%%% WORKER LIFECYCLE:
%%%   1. Pool requests worker from supervisor
%%%   2. Supervisor spawns worker (permanent restart strategy)
%%%   3. Worker initializes, sends {worker_available, self()} to pool
%%%   4. Pool monitors worker and adds to available queue
%%%   5. If worker dies: supervisor auto-restarts, pool updates tracking
%%%   6. New worker re-registers, pool assigns queued work

%% API
-export([start_link/3, call_python/3, call_and_await/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    pool_name :: atom(),
    script_name :: atom(),
    pool_size :: integer(),
    worker_sup_pid :: pid(),
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

    % Get worker supervisor PID (our sibling in the supervision tree)
    % We are started by python_pool_sup which started python_worker_sup first
    PoolSupName = list_to_atom(atom_to_list(PoolName) ++ "_sup"),
    WorkerSupPid = case supervisor:which_children(PoolSupName) of
        [{worker_sup, SupPid, supervisor, _} | _] when is_pid(SupPid) -> SupPid;
        Children ->
            logger:error("~p: Could not find worker_sup in children: ~p", [PoolName, Children]),
            error({worker_sup_not_found, Children})
    end,

    % Request workers from supervisor - they will auto-register when ready
    % This is the proper OTP pattern: supervisor spawns, workers report ready
    lists:foreach(
        fun(_) ->
            {ok, _WorkerPid} = python_worker_sup:start_worker(WorkerSupPid)
            % Worker will send {worker_available, self()} when initialized
        end,
        lists:seq(1, PoolSize)
    ),

    logger:info("Python pool ~p started, requested ~p workers from supervisor ~p",
        [PoolName, PoolSize, WorkerSupPid]),

    {ok, #state{
        pool_name = PoolName,
        script_name = ScriptName,
        pool_size = PoolSize,
        worker_sup_pid = WorkerSupPid,
        request_queue = queue:new(),
        available_workers = queue:new(),  % Workers will register themselves
        busy_workers = sets:new()
    }}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({call_python, Module, Function, Args, CallerPid}, State) ->
    logger:info("~p: Request ~p:~p from ~p", [State#state.pool_name, Module, Function, CallerPid]),

    case queue:out(State#state.available_workers) of
        {{value, Worker}, NewAvailable} ->
            % Worker available, assign immediately
            logger:debug("~p: Assigned to worker ~p", [State#state.pool_name, Worker]),
            python_worker:call_python(Worker, Module, Function, Args, CallerPid, self()),
            NewBusy = sets:add_element(Worker, State#state.busy_workers),
            {noreply, State#state{
                available_workers = NewAvailable,
                busy_workers = NewBusy
            }};
        {empty, _} ->
            % No workers available, queue the request
            QueueLen = queue:len(State#state.request_queue),
            logger:warning("~p: All workers busy, queuing request (queue length: ~p)",
                [State#state.pool_name, QueueLen + 1]),
            Request = {Module, Function, Args, CallerPid},
            NewQueue = queue:in(Request, State#state.request_queue),
            {noreply, State#state{request_queue = NewQueue}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({worker_done, WorkerPid, Result}, State) ->
    % Worker finished, log result
    case Result of
        {ok, _} ->
            logger:debug("~p: Worker ~p completed successfully", [State#state.pool_name, WorkerPid]);
        {error, Reason} ->
            logger:error("~p: Worker ~p error: ~p", [State#state.pool_name, WorkerPid, Reason])
    end,

    % Mark as available
    case sets:is_element(WorkerPid, State#state.busy_workers) of
        true ->
            NewBusy = sets:del_element(WorkerPid, State#state.busy_workers),

            % Check if there are queued requests
            case queue:out(State#state.request_queue) of
                {{value, {Module, Function, Args, CallerPid}}, NewQueue} ->
                    % Assign queued request to this worker
                    logger:info("~p: Assigning queued request ~p:~p to worker ~p",
                        [State#state.pool_name, Module, Function, WorkerPid]),
                    python_worker:call_python(WorkerPid, Module, Function, Args, CallerPid, self()),
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
    % Worker died - supervisor will automatically restart it (permanent restart strategy)
    % Pool's responsibility: Update work tracking (remove from busy/available sets)
    % This is WORK STATE management, not lifecycle management - that's supervisor's job
    %
    % Why we monitor: If worker dies mid-task, we need to:
    % 1. Remove from busy_workers (won't send {worker_done, ...})
    % 2. Remove from available_workers if it was idle
    % 3. Wait for supervisor to restart and worker to re-register
    WasBusy = sets:is_element(Pid, State#state.busy_workers),
    QueueLen = queue:len(State#state.request_queue),

    case Reason of
        normal ->
            logger:debug("~p: Worker ~p terminated normally", [State#state.pool_name, Pid]);
        {python_died, PythonReason} ->
            logger:error("~p: Worker ~p died due to Python process crash: ~p~n"
                        "    Worker state: ~s, Queued requests: ~p~n"
                        "    Supervisor will auto-restart worker",
                        [State#state.pool_name, Pid, PythonReason,
                         case WasBusy of true -> "busy"; false -> "idle" end,
                         QueueLen]);
        _ ->
            logger:error("~p: Worker ~p died unexpectedly: ~p~n"
                        "    Worker state: ~s, Queued requests: ~p~n"
                        "    Supervisor will auto-restart worker",
                        [State#state.pool_name, Pid, Reason,
                         case WasBusy of true -> "busy"; false -> "idle" end,
                         QueueLen])
    end,

    % Remove dead worker from tracking
    NewBusy = sets:del_element(Pid, State#state.busy_workers),
    AvailableList = queue:to_list(State#state.available_workers),
    NewAvailableList = lists:delete(Pid, AvailableList),
    NewAvailable = queue:from_list(NewAvailableList),

    % Supervisor will restart the worker automatically
    % When new worker starts, it will send {worker_available, NewPid} and we'll add it back
    {noreply, State#state{
        available_workers = NewAvailable,
        busy_workers = NewBusy
    }};

handle_info({worker_available, WorkerPid}, State) ->
    % New worker started (either at init or after restart) and is ready for work
    logger:info("~p: New worker ~p available", [State#state.pool_name, WorkerPid]),

    % Monitor worker for WORK TRACKING, not lifecycle management
    % - Supervisor handles lifecycle (spawn, monitor, restart, SASL reports)
    % - Pool handles work distribution (busy/available tracking)
    % - We need 'DOWN' message to remove dead workers from busy_workers set
    %   if they die mid-task before sending {worker_done, ...}
    erlang:monitor(process, WorkerPid),

    % Check if there are queued requests
    case queue:out(State#state.request_queue) of
        {{value, {Module, Function, Args, CallerPid}}, NewQueue} ->
            % Assign queued request immediately
            logger:info("~p: Worker ~p immediately assigned queued request ~p:~p",
                [State#state.pool_name, WorkerPid, Module, Function]),
            python_worker:call_python(WorkerPid, Module, Function, Args, CallerPid, self()),
            {noreply, State#state{
                request_queue = NewQueue,
                busy_workers = sets:add_element(WorkerPid, State#state.busy_workers)
            }};
        {empty, _} ->
            % No queued work, add to available pool
            logger:debug("~p: Worker ~p added to available pool", [State#state.pool_name, WorkerPid]),
            {noreply, State#state{
                available_workers = queue:in(WorkerPid, State#state.available_workers)
            }}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
