#!/usr/bin/env bash
set -e
MOD=/mnt/gallery/gameFiles/factorio/mods/Rampant_3.4.0
REPO=/mnt/backpack/code/games/factorio/Rampant
FAC=/mnt/gallery/gameFiles/factorio/bin/x64/factorio
cd /tmp/opencode
run() {
  $FAC --benchmark /tmp/opencode/real-save.zip --benchmark-ticks 3000 2>&1 | grep -E "^  avg:" | sed "s/^/$1:/"
  cd $REPO && just copy > /dev/null 2>&1
}
echo "=== inside the pheromone processing (baseline all-on avg 23.005) ==="
# B: skip the 8-neighbor accumulation loop
sed -i 's/^    for i = 1, 8 do$/    for i = 1, 0 do/' $MOD/libs/MapUtils.lua
grep -c "for i = 1, 0 do" $MOD/libs/MapUtils.lua
run "B neighbor-loop-off   "
# C: + skip the write path
sed -i 's/^    MapUtils.applyPheromoneUpdates(chunk, updates, tick)$/    -- MapUtils.applyPheromoneUpdates(chunk, updates, tick)/' $MOD/libs/MapUtils.lua
grep -c "MapUtils.applyPheromoneUpdates(chunk, updates, tick)" $MOD/libs/MapUtils.lua || true
run "C +write-path-off     "
echo "done"
