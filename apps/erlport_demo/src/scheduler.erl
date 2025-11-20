-module(scheduler).
-behaviour(gen_server).

%% API
-export([start_link/0, schedule_task/4, cancel_task/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(task, {
    id :: term(),
    interval_ms :: integer(),
    mfa :: {module(), atom(), list()},
    timer_ref :: reference() | undefined,
    in_progress :: boolean(),
    worker_pid :: pid() | undefined,
    worker_monitor :: reference() | undefined
}).

-record(state, {
    tasks :: #{term() => #task{}}
}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Schedule a recurring task
%% Id: unique identifier for this task
%% IntervalMs: interval in milliseconds
%% MFA: {Module, Function, Args} to call
%% InitialDelayMs: delay before first execution
schedule_task(Id, IntervalMs, MFA, InitialDelayMs) ->
    gen_server:call(?MODULE, {schedule_task, Id, IntervalMs, MFA, InitialDelayMs}).

%% Cancel a scheduled task
cancel_task(Id) ->
    gen_server:call(?MODULE, {cancel_task, Id}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    error_logger:info_msg("Scheduler started~n"),
    {ok, #state{tasks = #{}}}.

handle_call({schedule_task, Id, IntervalMs, MFA, InitialDelayMs}, _From, State) ->
    % Cancel existing task if any
    NewState = case maps:get(Id, State#state.tasks, undefined) of
        undefined -> State;
        OldTask ->
            case OldTask#task.timer_ref of
                undefined -> ok;
                OldTimer -> erlang:cancel_timer(OldTimer)
            end,
            State#state{tasks = maps:remove(Id, State#state.tasks)}
    end,

    % Schedule first execution
    TimerRef = erlang:send_after(InitialDelayMs, self(), {execute_task, Id}),

    Task = #task{
        id = Id,
        interval_ms = IntervalMs,
        mfa = MFA,
        timer_ref = TimerRef,
        in_progress = false,
        worker_pid = undefined,
        worker_monitor = undefined
    },

    error_logger:info_msg("Scheduled task ~p to run every ~p ms~n", [Id, IntervalMs]),

    {reply, ok, NewState#state{tasks = maps:put(Id, Task, NewState#state.tasks)}};

handle_call({cancel_task, Id}, _From, State) ->
    case maps:get(Id, State#state.tasks, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Task ->
            case Task#task.timer_ref of
                undefined -> ok;
                TimerRef -> erlang:cancel_timer(TimerRef)
            end,
            error_logger:info_msg("Cancelled task ~p~n", [Id]),
            {reply, ok, State#state{tasks = maps:remove(Id, State#state.tasks)}}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({execute_task, Id}, State) ->
    case maps:get(Id, State#state.tasks, undefined) of
        undefined ->
            % Task was cancelled
            {noreply, State};
        Task ->
            % Check if task is already in progress (visibility timeout)
            case Task#task.in_progress of
                true ->
                    % Task still running, skip this execution and reschedule
                    error_logger:warning_msg("Task ~p still in progress, skipping execution~n", [Id]),
                    TimerRef = erlang:send_after(Task#task.interval_ms, self(), {execute_task, Id}),
                    UpdatedTask = Task#task{timer_ref = TimerRef},
                    {noreply, State#state{tasks = maps:put(Id, UpdatedTask, State#state.tasks)}};
                false ->
                    % Execute task in separate monitored process
                    {M, F, A} = Task#task.mfa,
                    Self = self(),
                    WorkerPid = spawn(fun() ->
                        try
                            apply(M, F, A),
                            Self ! {task_complete, Id, self()}
                        catch
                            Error:Reason:Stacktrace ->
                                error_logger:error_msg(
                                    "Task ~p failed: ~p:~p~nStacktrace: ~p~n",
                                    [Id, Error, Reason, Stacktrace]
                                ),
                                Self ! {task_complete, Id, self()}
                        end
                    end),

                    % Monitor the worker process
                    MonitorRef = erlang:monitor(process, WorkerPid),

                    % Mark task as in progress with worker info
                    UpdatedTask = Task#task{
                        in_progress = true,
                        worker_pid = WorkerPid,
                        worker_monitor = MonitorRef
                    },
                    NewState = State#state{tasks = maps:put(Id, UpdatedTask, State#state.tasks)},

                    {noreply, NewState}
            end
    end;

handle_info({task_complete, Id, _WorkerPid}, State) ->
    case maps:get(Id, State#state.tasks, undefined) of
        undefined ->
            {noreply, State};
        Task ->
            % Demonitor the worker
            case Task#task.worker_monitor of
                undefined -> ok;
                MonitorRef -> erlang:demonitor(MonitorRef, [flush])
            end,

            % Mark task as not in progress and schedule next execution
            TimerRef = erlang:send_after(Task#task.interval_ms, self(), {execute_task, Id}),
            UpdatedTask = Task#task{
                in_progress = false,
                timer_ref = TimerRef,
                worker_pid = undefined,
                worker_monitor = undefined
            },
            {noreply, State#state{tasks = maps:put(Id, UpdatedTask, State#state.tasks)}}
    end;

handle_info({'DOWN', MonitorRef, process, _Pid, Reason}, State) ->
    % A task worker died - find which task and reschedule
    TaskEntry = maps:fold(
        fun(Id, Task, Acc) ->
            case Task#task.worker_monitor of
                MonitorRef -> {found, Id, Task};
                _ -> Acc
            end
        end,
        not_found,
        State#state.tasks
    ),

    case TaskEntry of
        {found, Id, Task} ->
            case Reason of
                normal ->
                    ok;
                _ ->
                    error_logger:warning_msg(
                        "Task ~p worker died abnormally: ~p~n",
                        [Id, Reason]
                    )
            end,

            % Mark task as not in progress and schedule next execution
            TimerRef = erlang:send_after(Task#task.interval_ms, self(), {execute_task, Id}),
            UpdatedTask = Task#task{
                in_progress = false,
                timer_ref = TimerRef,
                worker_pid = undefined,
                worker_monitor = undefined
            },
            {noreply, State#state{tasks = maps:put(Id, UpdatedTask, State#state.tasks)}};
        not_found ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Cancel all timers
    maps:foreach(
        fun(_Id, Task) ->
            case Task#task.timer_ref of
                undefined -> ok;
                TimerRef -> erlang:cancel_timer(TimerRef)
            end
        end,
        State#state.tasks
    ),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
