-module(python_worker_sup).
-behaviour(supervisor).

%% API
-export([start_link/2, start_worker/1]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link(ScriptName, PoolName) ->
    supervisor:start_link(?MODULE, [ScriptName, PoolName]).

%% Start a new worker under this supervisor
start_worker(SupPid) ->
    supervisor:start_child(SupPid, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([ScriptName, PoolName]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,  % Allow up to 10 worker restarts
        period => 60      % Within 60 seconds
    },

    % Worker spec - ScriptName and PoolName bound in closure, passed to each worker
    % PoolName is registered, so workers can send messages to it
    WorkerSpec = #{
        id => python_worker,
        start => {python_worker, start_link, [ScriptName, PoolName]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [python_worker]
    },

    {ok, {SupFlags, [WorkerSpec]}}.
