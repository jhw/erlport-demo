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
                            % Wrap HTML in JSON object for ErlCracker transport
                            JsonInput = #{<<"html">> => Body},
                            % Parse with Python via ErlCracker - returns JSON string
                            case erlcracker:call(bbc_pool, bbc_scraper, parse_bbc_html, [JsonInput], 30000) of
                                {ok, JsonResults} ->
                                    % Decode JSON to Erlang terms (with binary keys)
                                    {ok, ParsedResults} = thoas:decode(JsonResults),
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
    % Extract all event names for batch matching (now with binary keys from thoas)
    EventNames = [maps:get(<<"name">>, Result) || Result <- Results],

    % Prepare request data for Python name matcher
    % ErlCracker automatically handles JSON encoding/decoding
    RequestData = #{
        <<"matchup_texts">> => EventNames,
        <<"league_code">> => LeagueCode,
        <<"teams_data">> => #{LeagueCode => Teams}
    },

    % Batch match all event names at once - send/receive JSON via ErlCracker
    case erlcracker:call(matcher_pool, name_matcher, match_matchups_batch, [RequestData], 30000) of
        {ok, JsonResponse} ->
            {ok, MatchResult} = thoas:decode(JsonResponse),
            Matched = maps:get(<<"matched">>, MatchResult, #{}),
            Unmatched = maps:get(<<"unmatched">>, MatchResult, []),

            % Log unmatched events
            lists:foreach(fun(UnmatchedName) ->
                logger:warning("BBC scraper: Could not match event: ~p", [UnmatchedName])
            end, Unmatched),

            % Filter and store only matched results
            MatchedCount = maps:size(Matched),
            logger:info("BBC scraper: Matched ~p/~p events for ~p", [MatchedCount, length(Results), LeagueCode]),

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
                        gen_server:cast(event_store, {store_event, LeagueCode, CanonicalName, Date, bbc, Score}),
                        logger:info("BBC scraper: Stored event ~p: ~p ~p ~p", [LeagueCode, CanonicalName, Date, Score])
                end
            end, Results);
        {error, timeout} ->
            logger:error("BBC scraper: Batch match timeout for ~p", [LeagueCode]);
        {error, Error} ->
            logger:error("BBC scraper: Batch match error for ~p: ~p", [LeagueCode, Error])
    end.
