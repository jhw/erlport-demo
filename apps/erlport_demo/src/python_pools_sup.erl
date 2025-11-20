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

    Children = [
        #{
            id => bbc_pool,
            start => {python_pool, start_link, [bbc_pool, bbc_scraper, 2]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [python_pool]
        },
        #{
            id => fishy_pool,
            start => {python_pool, start_link, [fishy_pool, fishy_scraper, 2]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [python_pool]
        },
        #{
            id => matcher_pool,
            start => {python_pool, start_link, [matcher_pool, name_matcher, 2]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [python_pool]
        }
    ],

    {ok, {SupFlags, Children}}.
