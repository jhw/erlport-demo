#!/bin/bash
# Clean build artifacts

set -e

cd "$(dirname "$0")/.."

echo "Cleaning erlport_demo..."
rebar3 clean

echo ""
echo "Clean successful"
