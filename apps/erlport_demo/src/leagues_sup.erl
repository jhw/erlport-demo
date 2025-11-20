-module(leagues_sup).
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

    % Load league data and create a supervisor for each league
    PrivDir = code:priv_dir(erlport_demo),
    LeaguesFile = filename:join([PrivDir, "data", "leagues", "leagues.json"]),
    {ok, LeaguesJson} = file:read_file(LeaguesFile),
    Leagues = thoas:decode(LeaguesJson),

    % Create child spec for each league
    Children = lists:map(fun(League) ->
        LeagueCode = maps:get(<<"code">>, League),
        #{
            id => list_to_atom("league_sup_" ++ binary_to_list(LeagueCode)),
            start => {league_worker_sup, start_link, [LeagueCode]},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [league_worker_sup]
        }
    end, Leagues),

    {ok, {SupFlags, Children}}.
