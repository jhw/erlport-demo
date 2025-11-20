-module(fishy_scraper).

%% API
-export([scrape/1]).

%%====================================================================
%% API functions - Called by scheduler
%%====================================================================

scrape(LeagueCode) ->
    error_logger:info_msg("Starting Fishy scrape for ~p~n", [LeagueCode]),

    % Get league and team data from config service
    case config_service:get_league_data(LeagueCode) of
        {ok, LeagueData, Teams} ->
            case maps:get(<<"thefishyId">>, LeagueData, undefined) of
                undefined ->
                    error_logger:warning_msg("No Fishy ID for league ~p~n", [LeagueCode]);
                FishyId ->
                    Url = build_url(FishyId),

                    case http_client:fetch_url(Url) of
                        {ok, 200, Body} ->
                            % Parse with Python - result comes back as message
                            python_pool:call_python(fishy_pool, {fishy_scraper, parse_fishy_html, [Body]}, self()),

                            % Wait for result with timeout
                            receive
                                {python_result, _WorkerPid, {ok, ParsedResults}} ->
                                    process_results(LeagueCode, ParsedResults, Teams);
                                {python_result, _WorkerPid, {error, Error}} ->
                                    error_logger:error_msg("Fishy parse error for ~p: ~p~n", [LeagueCode, Error])
                            after 30000 ->
                                error_logger:error_msg("Fishy parse timeout for ~p~n", [LeagueCode])
                            end;
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

    % Call matcher pool with message passing
    python_pool:call_python(matcher_pool, {name_matcher, match_matchup, [Name, LeagueCode, TeamsData]}, self()),

    % Wait for match result with timeout
    receive
        {python_result, _WorkerPid, {ok, MatchedName}} when MatchedName =/= none, MatchedName =/= undefined ->
            % Use cast instead of call to avoid blocking
            gen_server:cast(event_store, {store_event, LeagueCode, MatchedName, Date, fishy, Score}),
            error_logger:info_msg("Stored Fishy event: ~p ~p ~p ~p~n", [LeagueCode, MatchedName, Date, Score]);
        {python_result, _WorkerPid, {ok, _}} ->
            error_logger:warning_msg("Could not match Fishy event: ~p~n", [Name]);
        {python_result, _WorkerPid, {error, Error}} ->
            error_logger:error_msg("Match error for ~p: ~p~n", [Name, Error])
    after 10000 ->
        error_logger:warning_msg("Match timeout for Fishy event: ~p~n", [Name])
    end.
