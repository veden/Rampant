#!/usr/bin/env bash
# Run one behavior scenario from a workspace-local user-data dir.
#
# `just test-behavior` copies the scenario and the prepared save into the
# Factorio install dir, which is read-only here. This does the same against
# .bench/user-data instead, so the battery can run without touching the install.
#
# The mod copy under .bench/mods/Rampant is used as-is; build it with
#   git archive <rev> | tar -x -C .bench/mods/Rampant
# for the shipped default (h7), or flip rampant--sourceVersionedField to true
# in its settings.lua for h8.
#
# usage: tools/behavior-local.sh <name> [seed] [ticks-budget-seconds]
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/.." && pwd)
B=$ROOT/.bench
NAME=${1:?usage: behavior-local.sh <name> [seed] [budget]}
SEED=${2:-1}
BUDGET=${3:-540}
SAVE=$ROOT/integration-tests/behavior-saves/rampant-behavior-$NAME-s$SEED.zip
if [ ! -f "$SAVE" ]; then echo "NO SAVE: $SAVE (run: just make-saves $SEED)"; exit 2; fi
mkdir -p "$B/user-data/scenarios" "$B/user-data/saves"
rm -rf "$B/user-data/scenarios/rampant-behavior-$NAME"
cp -r "$ROOT/integration-tests/behavior-scenarios/$NAME" "$B/user-data/scenarios/rampant-behavior-$NAME"
cp "$ROOT/integration-tests/lib/scenario-lib.lua" "$B/user-data/scenarios/rampant-behavior-$NAME/scenario-lib.lua"
cp "$SAVE" "$B/user-data/saves/rampant-behavior-$NAME.zip"
LOG=$B/behavior-$NAME.out
rm -f "$LOG"
/mnt/gallery/gameFiles/factorio/bin/x64/factorio \
  --start-server-load-scenario rampant-behavior-$NAME \
  --mod-directory "$B/mods" -c "$B/user-data/config.ini" \
  --server-settings "$B/server-settings.json" \
  --port 34201 --rcon-port 27019 --rcon-password rampant-test \
  > "$LOG" 2>&1 &
PID=$!
for _ in $(seq 1 "$BUDGET"); do
  if grep -q "TESTS_COMPLETE\|TESTS_FAILED" "$LOG" 2>/dev/null; then sleep 2; break; fi
  if ! kill -0 $PID 2>/dev/null; then echo "SERVER DIED (no test signal)"; break; fi
  sleep 1
done
grep -E "\[PASS\]|\[FAIL\]|Passed|Failed|Detail|TESTS" "$LOG" | tail -20
kill -9 $PID 2>/dev/null || true
