-module(infrastructure_sup).
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
        % Event store
        #{
            id => event_store,
            start => {event_store, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [event_store]
        },
        % Centralized scheduler
        #{
            id => scheduler,
            start => {scheduler, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [scheduler]
        },
        % Python pools supervisor
        #{
            id => python_pools_sup,
            start => {python_pools_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [python_pools_sup]
        }
    ],

    {ok, {SupFlags, Children}}.
