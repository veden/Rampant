#!/usr/bin/env bash
# Run a save headless against the current working tree.
#
# Rules this enforces:
#   1. The save given is COPIED and only the copy is loaded. The original is never
#      opened by Factorio, so it cannot be written.
#   2. The server is always terminated with SIGKILL. SIGTERM makes Factorio save
#      the map in place ("Saving map as <path>"), which is how a test run can
#      overwrite a real save.
#
# Usage: tools/headless.sh <save.zip> [seconds] [rcon-port]
set -euo pipefail

SAVE="$1"
SECONDS_TO_RUN="${2:-60}"
PORT="${3:-26900}"

[ -f "$SAVE" ] || { echo "no such save: $SAVE" >&2; exit 1; }

REPO="$(cd "$(dirname "$0")/.." && pwd)"
FAC="${FACTORIO_BIN:-/mnt/gallery/gameFiles/factorio/bin/x64/factorio}"
WORK="$(mktemp -d /tmp/rampant-headless.XXXXXX)"
MODS="$REPO/.bench/mods"

ORIG_MD5=$(md5sum "$SAVE" | cut -d' ' -f1)
ORIG_MTIME=$(stat -c %Y "$SAVE")

mkdir -p "$WORK/mods/Rampant" "$WORK/ud"
cp -r "$REPO"/*.lua "$REPO"/*.png "$REPO"/*.json "$WORK/mods/Rampant/" 2>/dev/null || true
for d in sounds locale libs graphics prototypes migrations; do
    cp -r "$REPO/$d" "$WORK/mods/Rampant/" 2>/dev/null || true
done
cp "$SAVE" "$WORK/world.zip"                 # <- only the copy is ever loaded

printf 'write-data=%s/ud\n' "$WORK" > "$WORK/config.ini"
printf '{"name":"headless","description":"","visibility":{"public":false,"lan":false},"require_user_verification":false,"max_players":5,"auto_pause":false,"autosave_interval":0}\n' \
    > "$WORK/server.json"

"$FAC" --start-server "$WORK/world.zip" --mod-directory "$WORK/mods" \
       -c "$WORK/config.ini" --server-settings "$WORK/server.json" \
       --rcon-port "$PORT" --rcon-password test > "$WORK/run.log" 2>&1 &
PID=$!

for _ in $(seq 1 180); do
    if python3 "$REPO/tools/rcon_client.py" query "$PORT" test \
        '/silent-command rcon.print(remote.call("Rampant","isInitialized"))' 2>/dev/null | grep -qi true; then
        break
    fi
    sleep 1
done

sleep "$SECONDS_TO_RUN"
kill -9 $PID 2>/dev/null || true     # SIGKILL: never SIGTERM, that saves the map
wait $PID 2>/dev/null || true

NEW_MD5=$(md5sum "$SAVE" | cut -d' ' -f1)
NEW_MTIME=$(stat -c %Y "$SAVE")
if [ "$ORIG_MD5" != "$NEW_MD5" ] || [ "$ORIG_MTIME" != "$NEW_MTIME" ]; then
    echo "WARNING: the original save changed during the run: $SAVE" >&2
else
    echo "original untouched: $(basename "$SAVE") (md5 $ORIG_MD5)"
fi
echo "log:     $WORK/run.log"
echo "workdir: $WORK   (contains world.zip, the copy that was loaded)"
