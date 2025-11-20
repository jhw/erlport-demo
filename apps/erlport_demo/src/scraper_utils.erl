-module(scraper_utils).

%% API
-export([load_league_data/1]).

%%====================================================================
%% API functions
%%====================================================================

load_league_data(LeagueCode) ->
    % Use config service instead of file I/O
    config_service:get_league_data(LeagueCode).
