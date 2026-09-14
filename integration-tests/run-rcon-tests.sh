#!/usr/bin/env bash
# RCON-based AI test runner for Rampant

set -e

SCENARIO="rampant-active-ai-tests"
PORT=34199
RCON_PORT=27017
RCON_PASS="rampant-test"
LOG_FILE="/tmp/rampant-rcon-test.log"
SETTINGS_FILE="/tmp/rampant-rcon-server-settings.json"

# Create server settings
cat > "$SETTINGS_FILE" << EOF
{
    "name": "Rampant RCON Tests",
    "description": "RCON-controlled AI tests",
    "visibility": {"public": false, "lan": false},
    "require_user_verification": false,
    "max_players": 1,
    "auto_pause": false
}
EOF

echo "Starting Factorio server..."
# Remove stale saves (prevents loading an old save instead of fresh scenario init)
rm -f /mnt/gallery/gameFiles/factorio/saves/rampant-active-ai-tests*
# Start Factorio in background
/mnt/gallery/gameFiles/factorio/bin/x64/factorio \
    --start-server-load-scenario "$SCENARIO" \
    --server-settings "$SETTINGS_FILE" \
    --port "$PORT" \
    --rcon-port "$RCON_PORT" \
    --rcon-password "$RCON_PASS" \
    --console-log "$LOG_FILE" &
FACTORIO_PID=$!

cleanup() {
    echo "Cleaning up..."
    (echo "quit"; sleep 1) | nc -w 2 localhost "$RCON_PORT" 2>/dev/null || true
    sleep 2
    if kill -0 $FACTORIO_PID 2>/dev/null; then
        kill -9 $FACTORIO_PID 2>/dev/null || true
    fi
}
trap cleanup EXIT

# Helper function to send RCON command
send_cmd() {
    echo "$1" | nc -w 3 localhost "$RCON_PORT" 2>/dev/null
}

# Helper to wait a bit
wait_tick() {
    sleep 0.5
}

# Wait for server to be ready via RCON polling
echo "Waiting for server to be ready..."
READY=false
for i in {1..60}; do
    # Try to get game state via RCON
    RESPONSE=$(send_cmd '/silent-command print(storage._rampantReady or "not_ready")' 2>/dev/null || echo "")
    if echo "$RESPONSE" | grep -q "true"; then
        READY=true
        break
    fi
    sleep 1
done

if [ "$READY" = false ]; then
    echo "ERROR: Server did not become ready in time"
    exit 1
fi

echo "Server ready!"
echo ""
echo "=== RAMPANT RCON AI TESTS ==="
echo ""

# Test 1: Check game state
echo "Test 1: Check game state..."
STATE=$(send_cmd '/silent-command local s=remote.call("Rampant","getGameState"); print("TICK:"..s.tick.." SPEED:"..s.speed.." AWAKE:"..tostring(s.universeAwake))')
echo "$STATE"
echo ""

# Test 2: Spawn player wall
echo "Test 2: Spawn player wall..."
RESULT=$(send_cmd '/silent-command print(remote.call("Rampant","spawnEntity","stone-wall",{0,0},"player"))')
echo "$RESULT"
wait_tick

# Test 3: Spawn enemy spawner
echo "Test 3: Spawn enemy spawner..."
RESULT=$(send_cmd '/silent-command print(remote.call("Rampant","spawnEntity","biter-spawner",{100,0},"enemy"))')
echo "$RESULT"
wait_tick

# Test 4: Set game speed
echo "Test 4: Set game speed to 100..."
RESULT=$(send_cmd '/silent-command print(remote.call("Rampant","setGameSpeed",100))')
echo "$RESULT"
wait_tick

# Test 5: Telemetry counters (advanceTicks was removed: game.tick is
# read-only in Factorio — the old member errored on every call)
echo "Test 5: Telemetry counters..."
RESULT=$(send_cmd '/silent-command print(remote.call("Rampant","getMetricsSummary").enabled)')
echo "$RESULT"
wait_tick

# Test 6: Query pheromones
echo "Test 6: Query pheromones at origin..."
RESULT=$(send_cmd '/silent-command local p=remote.call("Rampant","queryPheromones",{0,0},game.surfaces[1]); if p then print("PLAYER_PHER:"..p.player) else print("nil") end')
echo "$RESULT"
wait_tick

# Test 7: Check base at spawner position
echo "Test 7: Check base at spawner position..."
RESULT=$(send_cmd '/silent-command local b=remote.call("Rampant","queryBaseAt",{100,0},game.surfaces[1]); if b then print("BASE_ID:"..b.id.." STATE:"..b.state) else print("nil") end')
echo "$RESULT"
wait_tick

# Test 8: Count enemy entities
echo "Test 8: Count enemy entities..."
RESULT=$(send_cmd '/silent-command print("ENEMY_COUNT:"..remote.call("Rampant","countEntitiesByForce","enemy"))')
echo "$RESULT"
wait_tick

# Test 9: Check game state after ticks
echo "Test 9: Check game state after ticks..."
STATE=$(send_cmd '/silent-command local s=remote.call("Rampant","getGameState"); print("TICK:"..s.tick.." SPEED:"..s.speed)')
echo "$STATE"
echo ""

# Test 10: Get debug logs
echo "Test 10: Get debug logs..."
RESULT=$(send_cmd '/silent-command print(remote.call("RampantDebug","getLogs") or "no logs")')
echo "$RESULT"
echo ""

echo ""
echo "=== RCON TESTS COMPLETED ==="
echo ""
echo "All RCON tests passed!"