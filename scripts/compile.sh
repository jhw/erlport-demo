#!/bin/bash
# Compile the application

set -e

cd "$(dirname "$0")/.."

echo "Compiling erlport_demo..."
rebar3 compile

echo ""
echo "Compilation successful"
