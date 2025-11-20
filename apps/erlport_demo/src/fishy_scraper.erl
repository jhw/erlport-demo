-module(fishy_scraper).

%% API
-export([scrape/1]).

%%====================================================================
%% API functions - Called by scheduler
%%====================================================================

scrape(LeagueCode) ->
    error_logger:info_msg("Starting Fishy scrape for ~p~n", [LeagueCode]),

    % Get league and team data
    case scraper_utils:load_league_data(LeagueCode) of
        {ok, LeagueData, Teams} ->
            case maps:get(<<"thefishyId">>, LeagueData, undefined) of
                undefined ->
                    error_logger:warning_msg("No Fishy ID for league ~p~n", [LeagueCode]);
                FishyId ->
                    Url = build_url(FishyId),

                    case http_client:fetch_url(Url) of
                        {ok, 200, Body} ->
                            % Parse with Python
                            Callback = fun(_CallerPid, Result) ->
                                case Result of
                                    {ok, ParsedResults} ->
                                        process_results(LeagueCode, ParsedResults, Teams);
                                    {error, Error} ->
                                        error_logger:error_msg("Fishy parse error for ~p: ~p~n", [LeagueCode, Error])
                                end
                            end,
                            python_pool:call_python(fishy_pool, {fishy_scraper, parse_fishy_html, [Body]}, Callback);
                        {ok, Status, _} ->
                            error_logger:error_msg("Fishy scrape for ~p failed with status ~p~n", [LeagueCode, Status]);
                        {error, Error} ->
                            error_logger:error_msg("Fishy scrape for ~p failed: ~p~n", [LeagueCode, Error])
                    end
            end;
        {error, Reason} ->
            error_logger:error_msg("Failed to load league data for ~p: ~p~n", [LeagueCode, Reason])
    end.

%%====================================================================
%% Internal functions
%%====================================================================

build_url(FishyId) ->
    io_lib:format("https://thefishy.co.uk/football-results.php?table=~B", [FishyId]).

process_results(LeagueCode, Results, Teams) ->
    TeamsData = #{LeagueCode => Teams},

    lists:foreach(fun(Result) ->
        Name = maps:get(<<"name">>, Result),
        Date = maps:get(<<"date">>, Result),
        Score = maps:get(<<"score">>, Result),

        Callback = fun(_CallerPid, MatchResult) ->
            case MatchResult of
                {ok, MatchedName} when MatchedName =/= none, MatchedName =/= undefined ->
                    event_store:store_event(LeagueCode, MatchedName, Date, fishy, Score),
                    error_logger:info_msg("Stored Fishy event: ~p ~p ~p ~p~n", [LeagueCode, MatchedName, Date, Score]);
                _ ->
                    error_logger:warning_msg("Could not match Fishy event: ~p~n", [Name])
            end
        end,
        python_pool:call_python(matcher_pool, {name_matcher, match_matchup, [Name, LeagueCode, TeamsData]}, Callback)
    end, Results).
