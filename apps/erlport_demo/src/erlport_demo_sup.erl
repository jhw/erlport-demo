-module(erlport_demo_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    % Use rest_for_one strategy:
    % If infrastructure fails, restart infrastructure + leagues
    % If leagues fail, only restart leagues
    % Infrastructure includes: event_store, scheduler, python_pools
    % This ensures dependencies are properly restarted
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 10,
        period => 10
    },

    Children = [
        % Infrastructure supervisor - event_store, scheduler, python_pools
        #{
            id => infrastructure_sup,
            start => {infrastructure_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [infrastructure_sup]
        },
        % Leagues supervisor - depends on infrastructure
        #{
            id => leagues_sup,
            start => {leagues_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [leagues_sup]
        }
    ],

    {ok, {SupFlags, Children}}.
