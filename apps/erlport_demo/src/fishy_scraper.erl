-module(fishy_scraper).

%% API
-export([scrape/1]).

%%====================================================================
%% API functions - Called by scheduler
%%====================================================================

scrape(LeagueCode) ->
    logger:info("Fishy scraper: Starting scrape for ~p", [LeagueCode]),

    % Get league and team data from config service
    case config_service:get_league_data(LeagueCode) of
        {ok, LeagueData, Teams} ->
            case maps:get(<<"thefishyId">>, LeagueData, undefined) of
                undefined ->
                    logger:warning("Fishy scraper: No Fishy ID for league ~p", [LeagueCode]);
                FishyId ->
                    Url = build_url(FishyId),
                    logger:info("Fishy scraper: Fetching URL for ~p", [LeagueCode]),

                    case http_client:fetch_url(Url) of
                        {ok, 200, Body} ->
                            logger:info("Fishy scraper: Parsing HTML for ~p", [LeagueCode]),
                            % Parse with Python
                            case python_pool:call_and_await(fishy_pool, {fishy_scraper, parse_fishy_html, [Body]}, 30000) of
                                {ok, ParsedResults} ->
                                    logger:info("Fishy scraper: Parsed ~p results for ~p", [length(ParsedResults), LeagueCode]),
                                    process_results(LeagueCode, ParsedResults, Teams);
                                {error, timeout} ->
                                    logger:error("Fishy scraper: Parse timeout for ~p", [LeagueCode]);
                                {error, Error} ->
                                    logger:error("Fishy scraper: Parse error for ~p: ~p", [LeagueCode, Error])
                            end;
                        {ok, Status, _} ->
                            logger:error("Fishy scraper: HTTP request for ~p failed with status ~p", [LeagueCode, Status]);
                        {error, Error} ->
                            logger:error("Fishy scraper: HTTP request for ~p failed: ~p", [LeagueCode, Error])
                    end
            end;
        {error, Reason} ->
            logger:error("Fishy scraper: Failed to load league data for ~p: ~p", [LeagueCode, Reason])
    end.

%%====================================================================
%% Internal functions
%%====================================================================

build_url(FishyId) ->
    io_lib:format("https://thefishy.co.uk/football-results.php?table=~B", [FishyId]).

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
            gen_server:cast(event_store, {store_event, LeagueCode, MatchedName, Date, fishy, Score}),
            logger:info("Fishy scraper: Stored event ~p: ~p ~p ~p", [LeagueCode, MatchedName, Date, Score]);
        {ok, _} ->
            logger:warning("Fishy scraper: Could not match event: ~p", [Name]);
        {error, timeout} ->
            logger:warning("Fishy scraper: Match timeout for event: ~p", [Name]);
        {error, Error} ->
            logger:error("Fishy scraper: Match error for ~p: ~p", [Name, Error])
    end.
