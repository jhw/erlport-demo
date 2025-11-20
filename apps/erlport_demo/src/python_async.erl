-module(python_async).

%% API - Helper for async Python calls with receive pattern
-export([call_and_await/3]).

%%====================================================================
%% API functions
%%====================================================================

%% Call Python function and await result
%% Returns {ok, Result} | {error, Reason}
%%
%% This helper abstracts the common pattern of:
%% 1. Call python_pool:call_python/3
%% 2. Receive {python_result, WorkerPid, Result}
%% 3. Handle timeout
%%
%% Example:
%%   case python_async:call_and_await(bbc_pool, {bbc_scraper, parse_bbc_html, [Body]}, 30000) of
%%       {ok, ParsedResults} -> ...;
%%       {error, timeout} -> ...;
%%       {error, Reason} -> ...
%%   end
%%
call_and_await(Pool, {Module, Function, Args}, Timeout) ->
    python_pool:call_python(Pool, {Module, Function, Args}, self()),
    receive
        {python_result, _WorkerPid, Result} -> Result
    after Timeout ->
        {error, timeout}
    end.
