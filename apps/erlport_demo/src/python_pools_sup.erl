-module(python_pools_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_workers/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    % Start the actual Python workers after supervisor tree is up
    start_workers(),
    {ok, Pid}.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 5,
        period => 10
    },

    % Each pool has:
    % 1. A simple_one_for_one supervisor for workers (first)
    % 2. A pool manager that coordinates worker usage (second, depends on supervisor)

    Children = [
        % BBC pool worker supervisor
        #{
            id => bbc_pool_worker_sup,
            start => {python_pool_worker_sup, start_link, [bbc_pool, 2]},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [python_pool_worker_sup]
        },
        % BBC pool manager
        #{
            id => bbc_pool,
            start => {python_pool, start_link, [bbc_pool, bbc_pool_worker_sup]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [python_pool]
        },

        % Fishy pool worker supervisor
        #{
            id => fishy_pool_worker_sup,
            start => {python_pool_worker_sup, start_link, [fishy_pool, 2]},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [python_pool_worker_sup]
        },
        % Fishy pool manager
        #{
            id => fishy_pool,
            start => {python_pool, start_link, [fishy_pool, fishy_pool_worker_sup]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [python_pool]
        },

        % Matcher pool worker supervisor
        #{
            id => matcher_pool_worker_sup,
            start => {python_pool_worker_sup, start_link, [matcher_pool, 2]},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [python_pool_worker_sup]
        },
        % Matcher pool manager
        #{
            id => matcher_pool,
            start => {python_pool, start_link, [matcher_pool, matcher_pool_worker_sup]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [python_pool]
        }
    ],

    {ok, {SupFlags, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================

% Called after python_pools_sup is started to spawn the actual workers
start_workers() ->
    start_pool_workers(bbc_pool_worker_sup, 2),
    start_pool_workers(fishy_pool_worker_sup, 2),
    start_pool_workers(matcher_pool_worker_sup, 2),
    ok.

start_pool_workers(SupName, Count) ->
    lists:foreach(
        fun(_) ->
            supervisor:start_child(SupName, [])
        end,
        lists:seq(1, Count)
    ).
