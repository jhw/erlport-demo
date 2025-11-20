-module(http_client).

-export([fetch_url/1, fetch_url/2]).

%%====================================================================
%% API functions
%%====================================================================

%% Fetch URL with default timeout of 30 seconds
fetch_url(Url) ->
    fetch_url(Url, 30000).

%% Fetch URL with custom timeout
fetch_url(Url, Timeout) ->
    {ok, {Scheme, _, Host, Port, Path, Query}} = parse_url(Url),

    ConnectOpts = case Scheme of
        "https" -> #{transport => tls};
        "http" -> #{}
    end,

    % Start gun connection
    {ok, ConnPid} = gun:open(Host, Port, ConnectOpts),

    % Wait for connection
    case gun:await_up(ConnPid, Timeout) of
        {ok, _Protocol} ->
            % Build full path with query
            FullPath = case Query of
                "" -> Path;
                _ -> Path ++ "?" ++ Query
            end,

            % Make GET request
            StreamRef = gun:get(ConnPid, FullPath, [
                {<<"user-agent">>, <<"Mozilla/5.0 (compatible; ErlangScraper/1.0)">>}
            ]),

            % Wait for response
            case gun:await(ConnPid, StreamRef, Timeout) of
                {response, fin, Status, _Headers} ->
                    gun:close(ConnPid),
                    {ok, Status, <<>>};
                {response, nofin, Status, _Headers} ->
                    % Receive body
                    case gun:await_body(ConnPid, StreamRef, Timeout) of
                        {ok, Body} ->
                            gun:close(ConnPid),
                            {ok, Status, Body};
                        {error, Reason} ->
                            gun:close(ConnPid),
                            {error, {body_error, Reason}}
                    end;
                {error, Reason} ->
                    gun:close(ConnPid),
                    {error, {response_error, Reason}}
            end;
        {error, Reason} ->
            gun:close(ConnPid),
            {error, {connection_error, Reason}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

parse_url(Url) ->
    % Simple URL parsing
    case string:split(Url, "://", leading) of
        [Scheme, Rest] ->
            {HostPort, PathQuery} = case string:split(Rest, "/", leading) of
                [HP, PQ] -> {HP, "/" ++ PQ};
                [HP] -> {HP, "/"}
            end,

            {Host, Port} = case string:split(HostPort, ":", leading) of
                [H, P] -> {H, list_to_integer(P)};
                [H] ->
                    DefaultPort = case Scheme of
                        "https" -> 443;
                        "http" -> 80
                    end,
                    {H, DefaultPort}
            end,

            {Path, Query} = case string:split(PathQuery, "?", leading) of
                [Path0, Query0] -> {Path0, Query0};
                [Path0] -> {Path0, ""}
            end,

            {ok, {Scheme, "", Host, Port, Path, Query}};
        _ ->
            {error, invalid_url}
    end.
