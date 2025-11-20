-module(bbc_scraper).

%% API
-export([scrape/1]).

%%====================================================================
%% API functions - Called by scheduler
%%====================================================================

scrape(LeagueCode) ->
    logger:info("BBC scraper: Starting scrape for ~p", [LeagueCode]),

    % Get league and team data from config service
    case config_service:get_league_data(LeagueCode) of
        {ok, LeagueData, Teams} ->
            case maps:get(<<"bbcName">>, LeagueData, undefined) of
                undefined ->
                    logger:warning("BBC scraper: No BBC name for league ~p", [LeagueCode]);
                BbcName ->
                    Url = build_url(BbcName),
                    logger:info("BBC scraper: Fetching URL for ~p", [LeagueCode]),

                    case http_client:fetch_url(Url) of
                        {ok, 200, Body} ->
                            logger:info("BBC scraper: Parsing HTML for ~p", [LeagueCode]),
                            % Parse with Python
                            case python_pool:call_and_await(bbc_pool, {bbc_scraper, parse_bbc_html, [Body]}, 30000) of
                                {ok, ParsedResults} ->
                                    logger:info("BBC scraper: Parsed ~p results for ~p", [length(ParsedResults), LeagueCode]),
                                    process_results(LeagueCode, ParsedResults, Teams);
                                {error, timeout} ->
                                    logger:error("BBC scraper: Parse timeout for ~p", [LeagueCode]);
                                {error, Error} ->
                                    logger:error("BBC scraper: Parse error for ~p: ~p", [LeagueCode, Error])
                            end;
                        {ok, Status, _} ->
                            logger:error("BBC scraper: HTTP request for ~p failed with status ~p", [LeagueCode, Status]);
                        {error, Error} ->
                            logger:error("BBC scraper: HTTP request for ~p failed: ~p", [LeagueCode, Error])
                    end
            end;
        {error, Reason} ->
            logger:error("BBC scraper: Failed to load league data for ~p: ~p", [LeagueCode, Reason])
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

    % Spawn a linked process for each result to handle matching concurrently
    % This allows all matches to happen in parallel using both matcher workers
    lists:foreach(fun(Result) ->
        spawn_link(fun() ->
            match_and_store_result(Result, LeagueCode, TeamsData)
        end)
    end, Results).

match_and_store_result(Result, LeagueCode, TeamsData) ->
    Name = maps:get(<<"name">>, Result),
    Date = maps:get(<<"date">>, Result),
    Score = maps:get(<<"score">>, Result),

    % Call matcher pool
    case python_pool:call_and_await(matcher_pool, {name_matcher, match_matchup, [Name, LeagueCode, TeamsData]}, 10000) of
        {ok, MatchedName} when MatchedName =/= none, MatchedName =/= undefined ->
            gen_server:cast(event_store, {store_event, LeagueCode, MatchedName, Date, bbc, Score}),
            logger:info("BBC scraper: Stored event ~p: ~p ~p ~p", [LeagueCode, MatchedName, Date, Score]);
        {ok, _} ->
            logger:warning("BBC scraper: Could not match event: ~p", [Name]);
        {error, timeout} ->
            logger:warning("BBC scraper: Match timeout for event: ~p", [Name]);
        {error, Error} ->
            logger:error("BBC scraper: Match error for ~p: ~p", [Name, Error])
    end.
