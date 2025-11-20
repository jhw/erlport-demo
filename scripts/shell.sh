#!/bin/bash
# Start Erlang shell with application loaded

set -e

cd "$(dirname "$0")/.."

echo "Starting Erlang shell with erlport_demo..."
echo ""
echo "Quick start commands:"
echo "  application:start(erlport_demo).       % Start the application"
echo "  bbc_scraper:scrape(<<\"ENG1\">>).         % Manual BBC scrape"
echo "  fishy_scraper:scrape(<<\"ENG1\">>).       % Manual Fishy scrape"
echo "  event_store:get_events(<<\"ENG1\">>).     % Get events for ENG1"
echo "  observer:start().                      % Launch observer GUI"
echo ""

rebar3 shell
