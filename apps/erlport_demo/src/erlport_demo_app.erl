-module(erlport_demo_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Start supervisor
    {ok, Pid} = erlport_demo_sup:start_link(),

    % Start Cowboy HTTP server
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/events/:league", api_handler, []}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

    logger:info("HTTP API started on port 8080"),
    logger:info("League workers started automatically"),

    {ok, Pid}.

stop(_State) ->
    ok = cowboy:stop_listener(http_listener),
    ok.
