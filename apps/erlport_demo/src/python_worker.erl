-module(python_worker).
-behaviour(gen_server).

%% API
-export([start_link/2, call_python/7]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    python_pid :: pid(),
    pool_name :: atom()
}).

%%====================================================================
%% API functions
%%====================================================================

start_link(ScriptName, PoolName) ->
    gen_server:start_link(?MODULE, [ScriptName, PoolName], []).

%% Call Python function - result sent to CallerPid, completion notification to PoolPid
call_python(WorkerPid, Module, Function, Args, CallerPid, PoolPid, TimeoutMs) ->
    gen_server:cast(WorkerPid, {call_python, Module, Function, Args, CallerPid, PoolPid, TimeoutMs}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([ScriptName, PoolName]) ->
    process_flag(trap_exit, true),

    % Get the path to the priv/python directory
    PrivDir = code:priv_dir(erlport_demo),
    PythonPath = filename:join(PrivDir, "python"),

    % Start Python instance
    {ok, PythonPid} = python:start([{python_path, PythonPath}]),

    logger:info("Python worker ~p started for script ~p with Python process ~p, pool ~p",
        [self(), ScriptName, PythonPid, PoolName]),

    % Notify pool we're ready (both for initial start and after supervisor restart)
    % Pool is registered by name, so we can always reach it
    PoolName ! {worker_available, self()},

    {ok, #state{python_pid = PythonPid, pool_name = PoolName}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({call_python, Module, Function, Args, CallerPid, PoolPid, TimeoutMs}, State) ->
    WorkerPid = self(),
    % Execute in a separate process with timeout protection
    % Since python:call/4 has no timeout parameter, we implement timeout ourselves
    spawn_link(fun() ->
        % Create a unique reference for this call
        CallRef = make_ref(),

        % Spawn the actual Python call in a separate process
        CallPid = spawn_link(fun() ->
            try
                Result = python:call(State#state.python_pid, Module, Function, Args),
                WorkerPid ! {CallRef, {ok, Result}}
            catch
                Error:Reason:Stacktrace ->
                    logger:error(
                        "Python call failed: ~p:~p~nStacktrace: ~p",
                        [Error, Reason, Stacktrace]
                    ),
                    WorkerPid ! {CallRef, {error, {Error, Reason}}}
            end
        end),

        % Wait for result or timeout
        receive
            {CallRef, Result} ->
                % Python call completed in time
                case Result of
                    {ok, Value} ->
                        CallerPid ! {python_result, WorkerPid, {ok, Value}},
                        PoolPid ! {worker_done, WorkerPid, {ok, Value}};
                    {error, ErrorReason} ->
                        CallerPid ! {python_result, WorkerPid, {error, ErrorReason}},
                        PoolPid ! {worker_done, WorkerPid, {error, ErrorReason}}
                end
        after TimeoutMs ->
            % Timeout - kill the call process
            logger:error(
                "Python worker ~p: Call to ~p:~p timed out after ~pms, killing call process ~p",
                [WorkerPid, Module, Function, TimeoutMs, CallPid]
            ),
            exit(CallPid, kill),
            % Send timeout error to caller and pool
            CallerPid ! {python_result, WorkerPid, {error, worker_timeout}},
            PoolPid ! {worker_done, WorkerPid, {error, worker_timeout}}
        end
    end),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, normal}, State) ->
    % Linked process finished normally
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) when Pid =:= State#state.python_pid ->
    % Python process died - worker becomes useless without it
    % Let it crash: pool manager will detect our death and restart us with fresh Python
    logger:error("Python worker ~p detected Python process ~p crash: ~p~n"
                "    Worker will terminate - pool manager will restart both worker and Python process",
                [self(), Pid, Reason]),
    {stop, {python_died, Reason}, State};

handle_info({'EXIT', _Pid, Reason}, State) ->
    % A task process died
    logger:warning("Task process died: ~p", [Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    % Stop Python instance gracefully
    case Reason of
        normal ->
            logger:info("Python worker ~p shutting down normally", [self()]);
        shutdown ->
            logger:info("Python worker ~p shutting down (supervisor shutdown)", [self()]);
        {python_died, _} ->
            % Python already dead, logged in handle_info
            ok;
        _ ->
            logger:warning("Python worker ~p terminating unexpectedly: ~p", [self(), Reason])
    end,
    catch python:stop(State#state.python_pid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
