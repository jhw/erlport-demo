-module(api_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),

    Req = case {Method, Path} of
        {<<"GET">>, <<"/events/", League/binary>>} ->
            handle_get_events(Req0, League);
        _ ->
            respond_json(Req0, 404, #{error => <<"Not found">>})
    end,

    {ok, Req, State}.

handle_get_events(Req, League) ->
    case event_store:get_events(League) of
        {ok, Events} ->
            respond_json(Req, 200, #{
                league => League,
                count => length(Events),
                events => Events
            });
        {error, Reason} ->
            respond_json(Req, 500, #{error => Reason})
    end.

respond_json(Req, Status, Data) ->
    Body = thoas:encode(Data),
    cowboy_req:reply(Status, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req).
