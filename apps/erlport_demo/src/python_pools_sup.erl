-module(python_pools_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    % Load pool configuration from sys.config
    {ok, PoolConfigs} = application:get_env(erlport_demo, python_pools),

    % Helper to get pool configuration
    GetPoolConfig = fun(PoolName) ->
        case lists:keyfind(PoolName, 1, PoolConfigs) of
            {PoolName, Config} -> Config;
            false -> #{pool_size => 2, worker_timeout_ms => 45000}  % Default fallback
        end
    end,

    % Each child is a supervision subtree: worker_sup + pool
    % Proper OTP pattern: supervisors manage workers, pools distribute work
    Children = [
        % BBC pool subtree (worker supervisor + pool manager)
        #{
            id => bbc_pool_sup,
            start => {python_pool_sup, start_link, [bbc_pool, bbc_scraper, GetPoolConfig(bbc_pool)]},
            restart => permanent,
            shutdown => infinity,  % Supervisor
            type => supervisor,
            modules => [python_pool_sup]
        },
        % Fishy pool subtree
        #{
            id => fishy_pool_sup,
            start => {python_pool_sup, start_link, [fishy_pool, fishy_scraper, GetPoolConfig(fishy_pool)]},
            restart => permanent,
            shutdown => infinity,  % Supervisor
            type => supervisor,
            modules => [python_pool_sup]
        },
        % Matcher pool subtree
        #{
            id => matcher_pool_sup,
            start => {python_pool_sup, start_link, [matcher_pool, name_matcher, GetPoolConfig(matcher_pool)]},
            restart => permanent,
            shutdown => infinity,  % Supervisor
            type => supervisor,
            modules => [python_pool_sup]
        }
    ],

    {ok, {SupFlags, Children}}.
