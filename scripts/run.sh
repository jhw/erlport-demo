#!/bin/bash
# Run the application in foreground

set -e

cd "$(dirname "$0")/.."

echo "Starting erlport_demo application..."
echo ""
echo "HTTP API will be available at http://localhost:8080"
echo "Example: curl http://localhost:8080/events/ENG1"
echo ""
echo "Press Ctrl+C to stop"
echo ""

rebar3 shell --apps erlport_demo
