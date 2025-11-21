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
                            % Parse with Python - returns JSON string
                            case python_pool:call_and_await(fishy_pool, {fishy_scraper, parse_fishy_html, [Body]}, 30000) of
                                {ok, JsonResults} ->
                                    % Decode JSON to Erlang terms (with binary keys)
                                    {ok, ParsedResults} = thoas:decode(JsonResults),
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
    % Extract all event names for batch matching (now with binary keys from thoas)
    EventNames = [maps:get(<<"name">>, Result) || Result <- Results],

    % Prepare request data for Python name matcher - encode as JSON
    RequestData = #{
        <<"matchup_texts">> => EventNames,
        <<"league_code">> => LeagueCode,
        <<"teams_data">> => #{LeagueCode => Teams}
    },
    JsonRequest = thoas:encode(RequestData),

    % Batch match all event names at once - send/receive JSON
    case python_pool:call_and_await(matcher_pool, {name_matcher, match_matchups_batch, [JsonRequest]}, 30000) of
        {ok, JsonResponse} ->
            {ok, MatchResult} = thoas:decode(JsonResponse),
            Matched = maps:get(<<"matched">>, MatchResult, #{}),
            Unmatched = maps:get(<<"unmatched">>, MatchResult, []),

            % Log unmatched events
            lists:foreach(fun(UnmatchedName) ->
                logger:warning("Fishy scraper: Could not match event: ~p", [UnmatchedName])
            end, Unmatched),

            % Filter and store only matched results
            MatchedCount = maps:size(Matched),
            logger:info("Fishy scraper: Matched ~p/~p events for ~p", [MatchedCount, length(Results), LeagueCode]),

            lists:foreach(fun(Result) ->
                Name = maps:get(<<"name">>, Result),
                case maps:get(Name, Matched, undefined) of
                    undefined ->
                        % Event was unmatched, skip it
                        ok;
                    CanonicalName ->
                        % Event matched, store it (all binaries now)
                        Date = maps:get(<<"date">>, Result),
                        Score = maps:get(<<"score">>, Result),
                        gen_server:cast(event_store, {store_event, LeagueCode, CanonicalName, Date, fishy, Score}),
                        logger:info("Fishy scraper: Stored event ~p: ~p ~p ~p", [LeagueCode, CanonicalName, Date, Score])
                end
            end, Results);
        {error, timeout} ->
            logger:error("Fishy scraper: Batch match timeout for ~p", [LeagueCode]);
        {error, Error} ->
            logger:error("Fishy scraper: Batch match error for ~p: ~p", [LeagueCode, Error])
    end.
