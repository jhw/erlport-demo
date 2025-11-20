-module(league_worker_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link(LeagueCode) ->
    SupName = list_to_atom("league_worker_sup_" ++ binary_to_list(LeagueCode)),
    supervisor:start_link({local, SupName}, ?MODULE, [LeagueCode]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([LeagueCode]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    % Create a worker for this league that handles both BBC and Fishy scraping
    Children = [
        #{
            id => list_to_atom("league_worker_" ++ binary_to_list(LeagueCode)),
            start => {league_worker, start_link, [LeagueCode]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [league_worker]
        }
    ],

    {ok, {SupFlags, Children}}.
