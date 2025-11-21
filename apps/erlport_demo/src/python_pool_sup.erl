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
    % rest_for_one: worker_sup starts first, pool depends on it
    %               If worker_sup dies, pool restarts too (loses worker refs)
    %               If pool dies, worker_sup stays up (workers fine)
    SupFlags = #{
        strategy => rest_for_one,
        intensity => 5,
        period => 10
    },

    % STANDARD OTP PATTERN - Process Registration for Coordination:
    %
    % WHY: Pool needs to know worker_sup PID to spawn workers
    %
    % OPTIONS CONSIDERED:
    %   1. Pass PID as argument: Requires complex dynamic child addition after init
    %   2. Query parent supervisor: Can't call supervisor during its own init
    %   3. Registered names: Standard OTP coordination mechanism ✓
    %
    % REGISTERED NAMES ARE THE CORRECT OTP PATTERN:
    %   - Used throughout OTP (gen_server, supervisor, etc. all register)
    %   - Documented in "Designing for Scalability with Erlang/OTP"
    %   - rest_for_one ensures worker_sup registers BEFORE pool starts
    %   - No timing issues, no dynamic complexity
    %
    % Similar patterns in standard libraries:
    %   - Poolboy: Uses registered names for pool coordination
    %   - Ranch: Uses registered names for acceptor coordination
    %   - Many OTP applications: Process registration is the standard way
    %
    Children = [
        % Worker supervisor - manages Python worker lifecycle
        % Registers as <PoolName>_worker_sup for pool to find
        #{
            id => worker_sup,
            start => {python_worker_sup, start_link, [ScriptName, PoolName]},
            restart => permanent,
            shutdown => infinity,  % Supervisor
            type => supervisor,
            modules => [python_worker_sup]
        },
        % Pool manager - distributes work to workers
        % Looks up worker_sup by registered name (guaranteed to exist by rest_for_one)
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
