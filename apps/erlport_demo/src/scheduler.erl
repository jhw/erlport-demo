-module(scheduler).
-behaviour(gen_server).

%% API
-export([start_link/1, schedule_task/4, cancel_task/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(task, {
    id :: term(),
    interval_ms :: integer(),
    mfa :: {module(), atom(), list()},
    timer_ref :: reference() | undefined
}).

-record(state, {
    tasks :: #{term() => #task{}},
    scraper_config :: map()
}).

%%====================================================================
%% API functions
%%====================================================================

start_link(ScraperConfig) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ScraperConfig], []).

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

init([ScraperConfigs]) ->
    logger:info("Scheduler started with config: ~p", [ScraperConfigs]),

    % Load all leagues and schedule their scraper tasks
    % This replaces the league_worker layer - scheduler owns all tasks directly
    {ok, Leagues} = config_service:get_all_leagues(),
    lists:foreach(
        fun(League) -> schedule_league_tasks(League, ScraperConfigs) end,
        Leagues
    ),

    logger:info("Scheduler initialized with tasks for ~p leagues", [length(Leagues)]),
    {ok, #state{tasks = #{}, scraper_config = ScraperConfigs}}.

%% Internal function to schedule scraper tasks for a league
schedule_league_tasks(League, ScraperConfigs) ->
    LeagueCode = maps:get(<<"code">>, League),

    % Helper to get scraper timing config
    GetScraperConfig = fun(ScraperName) ->
        case lists:keyfind(ScraperName, 1, ScraperConfigs) of
            {ScraperName, Config} -> Config;
            false -> #{initial_delay_ms => 1000, interval_ms => 60000}  % Defaults
        end
    end,

    % Schedule BBC scraper if league has bbcName
    case maps:get(<<"bbcName">>, League, undefined) of
        undefined -> ok;
        _ ->
            BbcConfig = GetScraperConfig(bbc_scraper),
            BbcTaskId = {bbc_scraper, LeagueCode},
            schedule_task(
                BbcTaskId,
                maps:get(interval_ms, BbcConfig),
                {bbc_scraper, scrape, [LeagueCode]},
                maps:get(initial_delay_ms, BbcConfig)
            )
    end,

    % Schedule Fishy scraper if league has thefishyId
    case maps:get(<<"thefishyId">>, League, undefined) of
        undefined -> ok;
        _ ->
            FishyConfig = GetScraperConfig(fishy_scraper),
            FishyTaskId = {fishy_scraper, LeagueCode},
            schedule_task(
                FishyTaskId,
                maps:get(interval_ms, FishyConfig),
                {fishy_scraper, scrape, [LeagueCode]},
                maps:get(initial_delay_ms, FishyConfig)
            )
    end.

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
        timer_ref = TimerRef
    },

    logger:info("Scheduled task ~p to run every ~p ms", [Id, IntervalMs]),

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
            logger:info("Cancelled task ~p", [Id]),
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
            % Fire and forget: spawn task and immediately reschedule
            % Scrapers handle their own errors and logging
            {M, F, A} = Task#task.mfa,
            logger:info("Executing task ~p: ~p:~p", [Id, M, F]),
            proc_lib:spawn_link(fun() -> apply(M, F, A) end),

            % Schedule next execution
            TimerRef = erlang:send_after(Task#task.interval_ms, self(), {execute_task, Id}),
            UpdatedTask = Task#task{timer_ref = TimerRef},
            {noreply, State#state{tasks = maps:put(Id, UpdatedTask, State#state.tasks)}}
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
