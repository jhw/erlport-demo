-module(config_service).
-behaviour(gen_server).

%% API
-export([start_link/0, get_league_data/1, get_all_leagues/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    leagues :: [map()],
    teams :: #{binary() => [map()]}
}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Get league data and team data for a specific league
get_league_data(LeagueCode) ->
    gen_server:call(?MODULE, {get_league_data, LeagueCode}, 5000).

%% Get all leagues
get_all_leagues() ->
    gen_server:call(?MODULE, get_all_leagues, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    % Load all data once at startup
    % This happens before any other services start, so it's OK to block here briefly
    PrivDir = code:priv_dir(erlport_demo),

    % Load leagues
    LeaguesFile = filename:join([PrivDir, "data", "leagues", "leagues.json"]),
    Leagues = case file:read_file(LeaguesFile) of
        {ok, LeaguesJson} ->
            thoas:decode(LeaguesJson);
        {error, Reason} ->
            error_logger:error_msg("Failed to load leagues.json: ~p~n", [Reason]),
            []
    end,

    % Load teams for each league
    Teams = lists:foldl(fun(League, Acc) ->
        LeagueCode = maps:get(<<"code">>, League),
        TeamsFile = filename:join([PrivDir, "data", "teams", <<LeagueCode/binary, ".json">>]),
        case file:read_file(TeamsFile) of
            {ok, TeamsJson} ->
                TeamsList = thoas:decode(TeamsJson),
                maps:put(LeagueCode, TeamsList, Acc);
            {error, TeamsReason} ->
                error_logger:warning_msg("Failed to load teams for ~p: ~p~n", [LeagueCode, TeamsReason]),
                Acc
        end
    end, #{}, Leagues),

    error_logger:info_msg("Config service initialized with ~p leagues~n", [length(Leagues)]),

    {ok, #state{leagues = Leagues, teams = Teams}}.

handle_call({get_league_data, LeagueCode}, _From, State) ->
    % Find league data
    LeagueData = lists:foldl(fun(L, Acc) ->
        case maps:get(<<"code">>, L) of
            LeagueCode -> L;
            _ -> Acc
        end
    end, undefined, State#state.leagues),

    % Get teams
    Teams = maps:get(LeagueCode, State#state.teams, []),

    case LeagueData of
        undefined ->
            {reply, {error, league_not_found}, State};
        _ ->
            {reply, {ok, LeagueData, Teams}, State}
    end;

handle_call(get_all_leagues, _From, State) ->
    {reply, {ok, State#state.leagues}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
