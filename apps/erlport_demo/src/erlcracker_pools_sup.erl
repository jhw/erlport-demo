-module(erlcracker_pools_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    % Load pool configuration from sys.config
    {ok, PoolConfigs} = application:get_env(erlport_demo, erlcracker_pools),

    % Get the path to the priv/python directory
    PrivDir = code:priv_dir(erlport_demo),
    PythonPath = filename:join(PrivDir, "python"),

    % Helper to get pool configuration
    GetPoolConfig = fun(PoolName) ->
        case lists:keyfind(PoolName, 1, PoolConfigs) of
            {PoolName, Config} ->
                % Add python_path to config
                Config#{python_path => PythonPath};
            false ->
                % Default fallback with python_path
                #{pool_size => 2, worker_timeout_ms => 45000, python_path => PythonPath}
        end
    end,

    % Start ErlCracker pools directly (no custom supervisors needed)
    % ErlCracker provides its own supervision trees
    Children = [
        % BBC pool
        #{
            id => bbc_pool,
            start => {erlcracker, start_pool, [bbc_pool, erlcracker_python_runtime, GetPoolConfig(bbc_pool)]},
            restart => permanent,
            shutdown => infinity,  % Supervisor
            type => supervisor,
            modules => [erlcracker_pool_sup]
        },
        % Fishy pool
        #{
            id => fishy_pool,
            start => {erlcracker, start_pool, [fishy_pool, erlcracker_python_runtime, GetPoolConfig(fishy_pool)]},
            restart => permanent,
            shutdown => infinity,  % Supervisor
            type => supervisor,
            modules => [erlcracker_pool_sup]
        },
        % Matcher pool
        #{
            id => matcher_pool,
            start => {erlcracker, start_pool, [matcher_pool, erlcracker_python_runtime, GetPoolConfig(matcher_pool)]},
            restart => permanent,
            shutdown => infinity,  % Supervisor
            type => supervisor,
            modules => [erlcracker_pool_sup]
        }
    ],

    {ok, {SupFlags, Children}}.
