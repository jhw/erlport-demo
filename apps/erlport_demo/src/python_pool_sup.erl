-module(python_pool_sup).
-behaviour(supervisor).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

%% Start a supervision subtree for one pool
%% PoolName - registered name for the pool (e.g., bbc_pool)
%% ScriptName - Python script name (e.g., bbc_scraper)
%% PoolConfig - map with pool_size and worker_timeout_ms
start_link(PoolName, ScriptName, PoolConfig) ->
    SupName = list_to_atom(atom_to_list(PoolName) ++ "_sup"),
    supervisor:start_link({local, SupName}, ?MODULE, [PoolName, ScriptName, PoolConfig]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([PoolName, ScriptName, PoolConfig]) ->
    % rest_for_one: If worker_sup dies, pool dies too (loses worker refs)
    %               If pool dies, worker_sup dies too (clean slate)
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 5,
        period => 10
    },

    % Start worker supervisor first, then pool
    Children = [
        % Worker supervisor - manages the actual Python workers
        #{
            id => worker_sup,
            start => {python_worker_sup, start_link, [ScriptName, PoolName]},
            restart => permanent,
            shutdown => infinity,  % Supervisor
            type => supervisor,
            modules => [python_worker_sup]
        },
        % Pool manager - distributes work to workers
        #{
            id => pool,
            start => {python_pool, start_link, [PoolName, ScriptName, PoolConfig]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [python_pool]
        }
    ],

    {ok, {SupFlags, Children}}.
