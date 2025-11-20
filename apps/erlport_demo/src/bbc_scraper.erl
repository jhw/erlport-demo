-module(bbc_scraper).

%% API
-export([scrape/1]).

%%====================================================================
%% API functions - Called by scheduler
%%====================================================================

scrape(LeagueCode) ->
    error_logger:info_msg("Starting BBC scrape for ~p~n", [LeagueCode]),

    % Get league and team data
    case scraper_utils:load_league_data(LeagueCode) of
        {ok, LeagueData, Teams} ->
            case maps:get(<<"bbcName">>, LeagueData, undefined) of
                undefined ->
                    error_logger:warning_msg("No BBC name for league ~p~n", [LeagueCode]);
                BbcName ->
                    Url = build_url(BbcName),

                    case http_client:fetch_url(Url) of
                        {ok, 200, Body} ->
                            % Parse with Python
                            Callback = fun(_CallerPid, Result) ->
                                case Result of
                                    {ok, ParsedResults} ->
                                        process_results(LeagueCode, ParsedResults, Teams);
                                    {error, Error} ->
                                        error_logger:error_msg("BBC parse error for ~p: ~p~n", [LeagueCode, Error])
                                end
                            end,
                            python_pool:call_python(bbc_pool, {bbc_scraper, parse_bbc_html, [Body]}, Callback);
                        {ok, Status, _} ->
                            error_logger:error_msg("BBC scrape for ~p failed with status ~p~n", [LeagueCode, Status]);
                        {error, Error} ->
                            error_logger:error_msg("BBC scrape for ~p failed: ~p~n", [LeagueCode, Error])
                    end
            end;
        {error, Reason} ->
            error_logger:error_msg("Failed to load league data for ~p: ~p~n", [LeagueCode, Reason])
    end.

%%====================================================================
%% Internal functions
%%====================================================================

build_url(BbcName) ->
    {{Year, Month, _}, _} = calendar:local_time(),
    YearMonth = io_lib:format("~4..0B-~2..0B", [Year, Month]),
    lists:flatten(io_lib:format("https://www.bbc.co.uk/sport/football/~s/scores-fixtures/~s?filter=results",
        [binary_to_list(BbcName), YearMonth])).

process_results(LeagueCode, Results, Teams) ->
    TeamsData = #{LeagueCode => Teams},

    lists:foreach(fun(Result) ->
        Name = maps:get(<<"name">>, Result),
        Date = maps:get(<<"date">>, Result),
        Score = maps:get(<<"score">>, Result),

        Callback = fun(_CallerPid, MatchResult) ->
            case MatchResult of
                {ok, MatchedName} when MatchedName =/= none, MatchedName =/= undefined ->
                    event_store:store_event(LeagueCode, MatchedName, Date, bbc, Score),
                    error_logger:info_msg("Stored BBC event: ~p ~p ~p ~p~n", [LeagueCode, MatchedName, Date, Score]);
                _ ->
                    error_logger:warning_msg("Could not match BBC event: ~p~n", [Name])
            end
        end,
        python_pool:call_python(matcher_pool, {name_matcher, match_matchup, [Name, LeagueCode, TeamsData]}, Callback)
    end, Results).
