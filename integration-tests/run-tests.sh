#!/bin/bash
#
#    Rampant Integration Test Runner
#
#    This script launches Factorio headless server with the integration test
#    scenario to validate the Rampant mod within the actual Factorio game engine.
#
#    Usage:
#        ./run-tests.sh                    # Run with default settings
#        FACTORIO_PATH=/path/to/factorio ./run-tests.sh  # Custom Factorio path
#        MOD_PATH=/path/to/mod ./run-tests.sh            # Custom mod path
#

set -e

FACTORIO_PATH="${FACTORIO_PATH:-/opt/factorio}"
MOD_PATH="${MOD_PATH:-$(pwd)}"
SCENARIO_PATH="$MOD_PATH/integration-tests/scenario"
RESULTS_FILE="${RESULTS_FILE:-/tmp/rampant-integration-results.log}"
SERVER_SETTINGS="$FACTORIO_PATH/data/server-settings.json"

if [ ! -f "$SERVER_SETTINGS" ]; then
    if [ -f "$FACTORIO_PATH/data/server-settings.example.json" ]; then
        echo "Creating server-settings.json from example..."
        cp "$FACTORIO_PATH/data/server-settings.example.json" "$SERVER_SETTINGS"
    fi
fi

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "=========================================="
echo "  Rampant Integration Test Runner"
echo "=========================================="
echo ""

echo "Configuration:"
echo "  Factorio Path: $FACTORIO_PATH"
echo "  Mod Path:      $MOD_PATH"
echo "  Scenario:      $SCENARIO_PATH"
echo "  Results File:  $RESULTS_FILE"
echo ""

if [ ! -d "$FACTORIO_PATH" ]; then
    echo -e "${RED}ERROR: Factorio directory not found at $FACTORIO_PATH${NC}"
    exit 1
fi

if [ ! -f "$FACTORIO_PATH/bin/x64/factorio" ]; then
    echo -e "${RED}ERROR: Factorio binary not found at $FACTORIO_PATH/bin/x64/factorio${NC}"
    exit 1
fi

if [ ! -d "$SCENARIO_PATH" ]; then
    echo -e "${RED}ERROR: Scenario directory not found at $SCENARIO_PATH${NC}"
    exit 1
fi

echo "Checking required files..."
if [ ! -f "$SCENARIO_PATH/info.json" ]; then
    echo -e "${RED}ERROR: Scenario info.json not found${NC}"
    exit 1
fi

if [ ! -f "$SCENARIO_PATH/control.lua" ]; then
    echo -e "${RED}ERROR: Scenario control.lua not found${NC}"
    exit 1
fi

if [ ! -f "$MOD_PATH/control.lua" ]; then
    echo -e "${RED}ERROR: Rampant control.lua not found${NC}"
    exit 1
fi

if [ ! -f "$MOD_PATH/info.json" ]; then
    echo -e "${RED}ERROR: Rampant info.json not found${NC}"
    exit 1
fi

echo -e "${GREEN}All required files found.${NC}"
echo ""

echo "Preparing scenario..."
SCENARIO_DIR="$FACTORIO_PATH/scenarios/rampant-integration-tests"
if [ -d "$SCENARIO_DIR" ]; then
    rm -rf "$SCENARIO_DIR"
fi
mkdir -p "$SCENARIO_DIR"
cp -r "$SCENARIO_PATH"/* "$SCENARIO_DIR/"
echo "Scenario installed to $SCENARIO_DIR"
echo ""

echo "Copying Rampant mod to Factorio mods folder..."
MODS_PATH="$FACTORIO_PATH/mods"
RAMPANT_MOD_PATH="$MODS_PATH/Rampant"

if [ -d "$RAMPANT_MOD_PATH" ]; then
    rm -rf "$RAMPANT_MOD_PATH"
fi

mkdir -p "$RAMPANT_MOD_PATH"

cp -r "$MOD_PATH"/*.lua "$RAMPANT_MOD_PATH/" 2>/dev/null || true
cp -r "$MOD_PATH"/*.png "$RAMPANT_MOD_PATH/" 2>/dev/null || true
cp -r "$MOD_PATH"/*.json "$RAMPANT_MOD_PATH/" 2>/dev/null || true
cp -r "$MOD_PATH"/sounds "$RAMPANT_MOD_PATH/" 2>/dev/null || true
cp -r "$MOD_PATH"/locale "$RAMPANT_MOD_PATH/" 2>/dev/null || true
cp -r "$MOD_PATH"/libs "$RAMPANT_MOD_PATH/" 2>/dev/null || true
cp -r "$MOD_PATH"/graphics "$RAMPANT_MOD_PATH/" 2>/dev/null || true
cp -r "$MOD_PATH"/prototypes "$RAMPANT_MOD_PATH/" 2>/dev/null || true
cp -r "$MOD_PATH"/migrations "$RAMPANT_MOD_PATH/" 2>/dev/null || true
cp -r "$MOD_PATH"/COPYING "$RAMPANT_MOD_PATH/" 2>/dev/null || true
cp -r "$MOD_PATH"/changelog.txt "$RAMPANT_MOD_PATH/" 2>/dev/null || true
cp -r "$MOD_PATH"/README.md "$RAMPANT_MOD_PATH/" 2>/dev/null || true

echo "Rampant mod copied to $RAMPANT_MOD_PATH"

# Create marker file to enable test mode
MARKER_FILE="$RAMPANT_MOD_PATH/rampant-test-mode.marker"
echo "test-mode-enabled" > "$MARKER_FILE"

# Create mod-settings.json (setting type must be startup for mod loading)
SETTINGS_FILE="$RAMPANT_MOD_PATH/mod-settings.json"
cat > "$SETTINGS_FILE" << 'EOF'
{
  "startup": {
    "rampant--testMode": {
      "value": true
    }
  }
}
EOF
echo "Test mode enabled via mod-settings.json"
echo ""

echo "Starting Factorio headless server with integration test scenario..."
echo ""

FACTORIO_PID=""
TEST_COMPLETE=false
TEST_PASSED=false

# Cleanup function - only kills Factorio if not left running
cleanup() {
    if [ -n "$FACTORIO_PID" ] && kill -0 "$FACTORIO_PID" 2>/dev/null; then
        if [ "$TEST_COMPLETE" = true ]; then
            echo ""
            echo "Tests completed. Factorio is still running for manual inspection."
            echo "Close the Factorio server manually when done."
            echo ""
            echo "To stop: kill $FACTORIO_PID"
            return 0
        fi
        echo "Shutting down Factorio server..."
        kill "$FACTORIO_PID" 2>/dev/null || true
        wait "$FACTORIO_PID" 2>/dev/null || true
    fi
}
trap cleanup EXIT

"$FACTORIO_PATH/bin/x64/factorio" \
    --start-server-load-scenario "rampant-integration-tests" \
    --server-settings "$SERVER_SETTINGS" \
    --port 34197 \
    --rcon-port 27015 \
    --rcon-password "rampant-test" \
    --console-log "$RESULTS_FILE" \
    2>&1 &
FACTORIO_PID=$!

echo "Factorio started with PID: $FACTORIO_PID"
echo ""

echo "Waiting for integration tests to complete..."
echo "(Tests will signal completion via console output)"
echo ""
echo "Press Ctrl+C to stop and exit."
echo ""

LAST_LINE=""
while kill -0 "$FACTORIO_PID" 2>/dev/null; do
    if [ -f "$RESULTS_FILE" ]; then
        NEW_LAST_LINE=$(tail -3 "$RESULTS_FILE" 2>/dev/null || echo "")

        if echo "$NEW_LAST_LINE" | grep -q "TESTS_COMPLETE"; then
            TEST_COMPLETE=true
            TEST_PASSED=true
            break
        elif echo "$NEW_LAST_LINE" | grep -q "TESTS_FAILED"; then
            TEST_COMPLETE=true
            TEST_PASSED=false
            break
        fi
    fi
    sleep 0.5
done

if [ "$TEST_COMPLETE" = true ]; then
    echo ""
    echo "=========================================="
    echo "  Test Execution Complete"
    echo "=========================================="
    echo ""

    if [ "$TEST_PASSED" = true ]; then
        echo -e "${GREEN}=========================================="
        echo "  ALL INTEGRATION TESTS PASSED!"
        echo "==========================================${NC}"
        echo ""
        echo "Results saved to: $RESULTS_FILE"
        exit 0
    else
        echo -e "${RED}=========================================="
        echo "  INTEGRATION TESTS FAILED!"
        echo "==========================================${NC}"
        echo ""
        echo "Check $RESULTS_FILE for details."
        exit 1
    fi
fi

echo ""
echo "=========================================="
echo "  Test Execution Timed Out or Failed"
echo "=========================================="
echo ""
echo "Factorio may still be running. Check logs at: $RESULTS_FILE"
exit 1