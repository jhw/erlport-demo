-module(scraper_utils).

%% API
-export([load_league_data/1]).

%%====================================================================
%% API functions
%%====================================================================

load_league_data(LeagueCode) ->
    try
        PrivDir = code:priv_dir(erlport_demo),

        % Load leagues
        LeaguesFile = filename:join([PrivDir, "data", "leagues", "leagues.json"]),
        {ok, LeaguesJson} = file:read_file(LeaguesFile),
        Leagues = thoas:decode(LeaguesJson),

        % Find this league
        LeagueData = lists:foldl(fun(L, Acc) ->
            case maps:get(<<"code">>, L) of
                LeagueCode -> L;
                _ -> Acc
            end
        end, undefined, Leagues),

        case LeagueData of
            undefined ->
                {error, league_not_found};
            _ ->
                % Load teams
                TeamsFile = filename:join([PrivDir, "data", "teams", binary_to_list(LeagueCode) ++ ".json"]),
                {ok, TeamsJson} = file:read_file(TeamsFile),
                Teams = thoas:decode(TeamsJson),
                {ok, LeagueData, Teams}
        end
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.
