-module(infrastructure_sup).
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

    % Load scraper timing configuration from sys.config
    {ok, ScraperConfigs} = application:get_env(erlport_demo, scrapers),

    Children = [
        % Config service - must start first (loads data for everyone)
        #{
            id => config_service,
            start => {config_service, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [config_service]
        },
        % Event store
        #{
            id => event_store,
            start => {event_store, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [event_store]
        },
        % Centralized scheduler - receives per-scraper timing configuration
        #{
            id => scheduler,
            start => {scheduler, start_link, [ScraperConfigs]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [scheduler]
        },
        % ErlCracker pools supervisor
        #{
            id => erlcracker_pools_sup,
            start => {erlcracker_pools_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlcracker_pools_sup]
        }
    ],

    {ok, {SupFlags, Children}}.
