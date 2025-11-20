-module(event_store).
-behaviour(gen_server).

%% API
-export([start_link/0, store_event/5, get_events/1, get_all_events/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(event, {
    key :: {binary(), binary(), binary()},  % {League, Name, Date}
    league :: binary(),
    name :: binary(),
    date :: binary(),
    bbc_score :: binary() | undefined,
    fishy_score :: binary() | undefined,
    created_at :: integer(),
    updated_at :: integer()
}).

-record(state, {
    table :: ets:tid()
}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Store or update an event
%% League: binary() - league code like <<"ENG1">>
%% Name: binary() - normalized event name like <<"Arsenal vs Liverpool">>
%% Date: binary() - date string like <<"2025-11-20">>
%% Source: bbc | fishy
%% Score: binary() - score like <<"2-1">>
store_event(League, Name, Date, Source, Score) ->
    gen_server:call(?MODULE, {store_event, League, Name, Date, Source, Score}).

%% Get all events for a league
get_events(League) ->
    gen_server:call(?MODULE, {get_events, League}).

%% Get all events
get_all_events() ->
    gen_server:call(?MODULE, get_all_events).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    TableName = events_table,
    Table = ets:new(TableName, [set, protected, {keypos, #event.key}]),
    error_logger:info_msg("Event store initialized with table ~p~n", [TableName]),
    {ok, #state{table = Table}}.

handle_call({store_event, League, Name, Date, Source, Score}, _From, State) ->
    Key = {League, Name, Date},
    Timestamp = erlang:system_time(millisecond),

    Event = case ets:lookup(State#state.table, Key) of
        [] ->
            % Create new event
            NewEvent = case Source of
                bbc ->
                    #event{
                        key = Key,
                        league = League,
                        name = Name,
                        date = Date,
                        bbc_score = Score,
                        fishy_score = undefined,
                        created_at = Timestamp,
                        updated_at = Timestamp
                    };
                fishy ->
                    #event{
                        key = Key,
                        league = League,
                        name = Name,
                        date = Date,
                        bbc_score = undefined,
                        fishy_score = Score,
                        created_at = Timestamp,
                        updated_at = Timestamp
                    }
            end,
            ets:insert(State#state.table, NewEvent),
            NewEvent;
        [ExistingEvent] ->
            % Update existing event
            UpdatedEvent = case Source of
                bbc ->
                    ExistingEvent#event{
                        bbc_score = Score,
                        updated_at = Timestamp
                    };
                fishy ->
                    ExistingEvent#event{
                        fishy_score = Score,
                        updated_at = Timestamp
                    }
            end,
            ets:insert(State#state.table, UpdatedEvent),
            UpdatedEvent
    end,

    {reply, {ok, event_to_map(Event)}, State};

handle_call({get_events, League}, _From, State) ->
    MatchSpec = [{
        #event{key = {'$1', '_', '_'}, _ = '_'},
        [{'==', '$1', League}],
        ['$_']
    }],
    Events = ets:select(State#state.table, MatchSpec),
    EventMaps = lists:map(fun event_to_map/1, Events),
    {reply, {ok, EventMaps}, State};

handle_call(get_all_events, _From, State) ->
    Events = ets:tab2list(State#state.table),
    EventMaps = lists:map(fun event_to_map/1, Events),
    {reply, {ok, EventMaps}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ets:delete(State#state.table),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

event_to_map(#event{
    league = League,
    name = Name,
    date = Date,
    bbc_score = BbcScore,
    fishy_score = FishyScore,
    created_at = CreatedAt,
    updated_at = UpdatedAt
}) ->
    #{
        league => League,
        name => Name,
        date => Date,
        bbc_score => BbcScore,
        fishy_score => FishyScore,
        created_at => CreatedAt,
        updated_at => UpdatedAt
    }.
