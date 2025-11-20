-module(python_worker).
-behaviour(gen_server).

%% API
-export([start_link/1, call_python/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    python_pid :: pid()
}).

%%====================================================================
%% API functions
%%====================================================================

start_link(ScriptName) ->
    gen_server:start_link(?MODULE, [ScriptName], []).

%% Call Python function - returns immediately, result sent to caller
call_python(WorkerPid, Module, Function, Args) ->
    gen_server:cast(WorkerPid, {call_python, Module, Function, Args, self()}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([ScriptName]) ->
    process_flag(trap_exit, true),

    % Get the path to the priv/python directory
    PrivDir = code:priv_dir(erlport_demo),
    PythonPath = filename:join(PrivDir, "python"),

    % Start Python instance
    {ok, PythonPid} = python:start([{python_path, PythonPath}]),

    logger:debug("Python worker started for ~p (pid: ~p, python_pid: ~p)",
        [ScriptName, self(), PythonPid]),

    {ok, #state{python_pid = PythonPid}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({call_python, Module, Function, Args, CallerPid}, State) ->
    % Execute in a linked process so we don't block the worker
    spawn_link(fun() ->
        try
            Result = python:call(State#state.python_pid, Module, Function, Args),
            CallerPid ! {python_result, self(), {ok, Result}}
        catch
            Error:Reason:Stacktrace ->
                logger:error(
                    "Python call failed: ~p:~p~nStacktrace: ~p",
                    [Error, Reason, Stacktrace]
                ),
                CallerPid ! {python_result, self(), {error, {Error, Reason}}}
        end
    end),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, normal}, State) ->
    % Linked process finished normally
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State) when Pid =:= State#state.python_pid ->
    % Python process died, we should die too and let supervisor restart us
    logger:error("Python process ~p died: ~p", [Pid, Reason]),
    {stop, {python_died, Reason}, State};

handle_info({'EXIT', _Pid, Reason}, State) ->
    % A task process died
    logger:warning("Task process died: ~p", [Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    % Stop Python instance
    logger:debug("Python worker ~p terminating: ~p", [self(), Reason]),
    catch python:stop(State#state.python_pid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
