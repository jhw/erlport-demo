-module(python_pool_worker_sup).
-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link(PoolName, PoolSize) ->
    SupName = list_to_atom(atom_to_list(PoolName) ++ "_worker_sup"),
    supervisor:start_link({local, SupName}, ?MODULE, [PoolName, PoolSize]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([PoolName, _PoolSize]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 10
    },

    % Template for python workers
    WorkerSpec = #{
        id => python_worker,
        start => {python_worker, start_link, [PoolName]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [python_worker]
    },

    {ok, {SupFlags, [WorkerSpec]}}.
