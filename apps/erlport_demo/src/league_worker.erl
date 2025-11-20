-module(league_worker).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    league_code :: binary(),
    league_data :: map()
}).

%%====================================================================
%% API functions
%%====================================================================

start_link(LeagueCode) ->
    Name = list_to_atom("league_worker_" ++ binary_to_list(LeagueCode)),
    gen_server:start_link({local, Name}, ?MODULE, [LeagueCode], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([LeagueCode]) ->
    % Get league data from config service
    case config_service:get_league_data(LeagueCode) of
        {error, league_not_found} ->
            logger:error("League ~p not found", [LeagueCode]),
            {stop, {error, league_not_found}};
        {ok, LeagueData, _Teams} ->
            % Schedule scrapers using centralized scheduler
            % BBC scraper
            case maps:get(<<"bbcName">>, LeagueData, undefined) of
                undefined -> ok;
                _ ->
                    BbcTaskId = {bbc_scraper, LeagueCode},
                    scheduler:schedule_task(
                        BbcTaskId,
                        60000,  % 60 second interval
                        {bbc_scraper, scrape, [LeagueCode]},
                        5000    % Start after 5 seconds
                    )
            end,

            % Fishy scraper
            case maps:get(<<"thefishyId">>, LeagueData, undefined) of
                undefined -> ok;
                _ ->
                    FishyTaskId = {fishy_scraper, LeagueCode},
                    scheduler:schedule_task(
                        FishyTaskId,
                        60000,  % 60 second interval
                        {fishy_scraper, scrape, [LeagueCode]},
                        5000    % Start after 5 seconds
                    )
            end,

            logger:info("League worker started for ~p", [LeagueCode]),

            {ok, #state{
                league_code = LeagueCode,
                league_data = LeagueData
            }}
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Cancel scheduled tasks
    scheduler:cancel_task({bbc_scraper, State#state.league_code}),
    scheduler:cancel_task({fishy_scraper, State#state.league_code}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
