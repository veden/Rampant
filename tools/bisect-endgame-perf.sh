#!/usr/bin/env bash
set -e
MOD=/mnt/gallery/gameFiles/factorio/mods/Rampant_3.4.0
REPO=/mnt/backpack/code/games/factorio/Rampant
FAC=/mnt/gallery/gameFiles/factorio/bin/x64/factorio
cd /tmp/opencode
run() {
  $FAC --benchmark /tmp/opencode/real-save.zip --benchmark-ticks 3000 2>&1 | grep -E "^  (avg|min|max):" | tr '\n' ' ' | sed "s/^/$1:/"
  echo ""
  cd $REPO && just copy > /dev/null 2>&1
}
echo "=== cumulative bisect on the real late-game save (3000 ticks) ==="
echo "baseline (all on):      avg 23.005  min 1.010  max 1144.6"
# B: sweep off
sed -i 's/constants.PROCESS_SWEEP_CHUNKS_PER_TICK = 40/constants.PROCESS_SWEEP_CHUNKS_PER_TICK = 0/' $MOD/libs/Constants.lua
run "B sweep-off          "
# C: + pheromone/dirty processing off
sed -i 's/constants.PROCESS_DIRTY_CHUNKS_PER_TICK = 200/constants.PROCESS_DIRTY_CHUNKS_PER_TICK = 0/' $MOD/libs/Constants.lua
run "C +aco-off            "
# D: + the always-on per-tick AI (baseAIs/nests/cleanSquads)
sed -i 's/^                    processBaseAIs(tick)/                    -- processBaseAIs(tick)/' $MOD/control.lua
sed -i 's/^                    processNests(tick)/                    -- processNests(tick)/' $MOD/control.lua
sed -i 's/^                    cleanSquads(tick)/                    -- cleanSquads(tick)/' $MOD/control.lua
run "D +perTickAI-off      "
echo "D done (all on_tick work off)"
