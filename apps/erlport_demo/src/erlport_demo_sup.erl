-module(erlport_demo_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    % Simple one_for_one strategy - only infrastructure supervisor
    % Scheduler loads league configuration and schedules tasks directly
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10
    },

    Children = [
        % Infrastructure supervisor - event_store, scheduler, python_pools
        % Scheduler handles league task scheduling in its init/1
        #{
            id => infrastructure_sup,
            start => {infrastructure_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [infrastructure_sup]
        }
    ],

    {ok, {SupFlags, Children}}.
