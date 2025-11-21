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

    % Each child is a supervision subtree: worker_sup + pool
    % Proper OTP pattern: supervisors manage workers, pools distribute work
    Children = [
        % BBC pool subtree (worker supervisor + pool manager)
        #{
            id => bbc_pool_sup,
            start => {python_pool_sup, start_link, [bbc_pool, bbc_scraper, 2]},
            restart => permanent,
            shutdown => infinity,  % Supervisor
            type => supervisor,
            modules => [python_pool_sup]
        },
        % Fishy pool subtree
        #{
            id => fishy_pool_sup,
            start => {python_pool_sup, start_link, [fishy_pool, fishy_scraper, 2]},
            restart => permanent,
            shutdown => infinity,  % Supervisor
            type => supervisor,
            modules => [python_pool_sup]
        },
        % Matcher pool subtree
        #{
            id => matcher_pool_sup,
            start => {python_pool_sup, start_link, [matcher_pool, name_matcher, 2]},
            restart => permanent,
            shutdown => infinity,  % Supervisor
            type => supervisor,
            modules => [python_pool_sup]
        }
    ],

    {ok, {SupFlags, Children}}.
