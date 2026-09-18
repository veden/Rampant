# Endgame Performance: Measurement & Bisect (2026-09-17)

## The problem

Rampant's historical complaint: by end-game it consumes too much CPU and players
drop the mod. This document records the first *measurement* of that claim, and the
bisect that localizes the cost.

## Method

- **Input**: a real late-game Space Age save (24 MB, played with Rampant; its
  migrations ran on load). The save's own cost is the control.
- **Instrument**: the engine CLI benchmark —
  `factorio --benchmark <save> --benchmark-ticks 3000`. (The Lua profiler's
  per-function table renders only in the in-game GUI; headless has no players, and
  `rcon.print`/`log` render only the Duration summary.)
- **Control**: the same save with Rampant disabled via a scratch `--mod-directory`
  and mod-list. Same save, same ticks, one variable.
- **Bisect**: cumulative — each step disables one subsystem in the installed mod
  copy (restored with `just copy` after each run). Script: `/tmp/opencode/bisect.sh`.

## Measurements (3000 ticks each)

| Configuration | avg | min | max |
|---|---|---|---|
| **Without Rampant** (control) | 0.654 ms | 0.470 ms | 4.6 ms |
| **With Rampant** (baseline) | 23.005 ms | 1.010 ms | 1144.6 ms |
| — sweep marking off | 19.488 ms | 0.632 ms | 1152.8 ms |
| — + pheromone/dirty processing off | 1.376 ms | 0.669 ms | 1135.8 ms |
| — + per-tick AI off | 23.167 ms | 1.123 ms | 1163.1 ms |

(The last row's patches did not apply — it is a no-op run at baseline, which
confirms the run-to-run spread is small relative to the effects above.)

## Attribution

| Component | Cost | Share |
|---|---|---|
| **ACO pheromone processing** (`processDirtyChunk` → `processPheromone` → `calculatePheromoneUpdates`) | **18.1 ms/tick** | **81%** |
| Sweep marking (the round-robin that feeds it) | 3.5 ms/tick | 16% |
| Rampant total (vs the no-mod control) | ~22.4 ms/tick | — |

At 60 UPS the budget is 16.7 ms/tick: this save runs at **~45 UPS with Rampant
and ~1500 UPS without**. The complaint is reproduced and quantified.

## What this means

- The **cost is the processing, not the marking**: the sweep only *marks* chunks
  (3.5 ms); the expensive part is the per-chunk pheromone math it and the event
  paths trigger.
- Per-chunk cost is **0.09–0.45 ms** depending on the marked-chunk throughput
  (the sweep's constant 40/tick plus event-driven marks up to the 200/tick budget).
  That is very expensive for a neighbor loop — `calculatePheromoneUpdates` is
  call-heavy per chunk (8 neighbors × `getCombinedDeathGenerator`,
  `canMoveChunkDirection`, `getEnemyStructureCount`, plus the query-table writes).

## Deep bisect: inside the pheromone processing

Second cumulative bisect, same save, same 3000 ticks (baseline avg 23.005 ms):

| Configuration | avg | attribution |
|---|---|---|
| Baseline | 23.005 ms | — |
| **8-neighbor accumulation loop off** | **2.539 ms** | **the loop = 20.5 ms (89% of total)** |
| + write path (applyPheromoneUpdates) off | 2.089 ms | the write path = 0.45 ms (2%) |

**The 8-iteration neighbor loop in `calculatePheromoneUpdates` is the cost** —
~91% of the end-game total. The write path is 2%.

Inside the loop, per neighbor: `canMoveChunkDirection` (call), 
`getCombinedDeathGenerator` (call), ~5 pheromone field reads, 3 max-comparisons.
The lookup-table change proved the ~24 comparisons/chunk are not the cost, which
leaves the **16 function calls + ~40 field reads per chunk** (8 neighbors each).

Per-chunk price: 20.5 ms / chunks-per-tick. With the sweep's constant 40/tick
that is ~0.5 ms/chunk; if the event-driven marks saturate the 200/tick budget,
~0.1 ms/chunk. The throughput measurement (server + RCON counters) still needs a
clean run — the achievements confirmation prompt intercepts the first console command
and the server window was missed twice.

Script: tools/bisect-pheromone-loop.sh.

## Next steps

1. **Throughput**: read `dirty.chunksProcessed` and `dirty.sweepMarks` per tick on
   the live save (server + RCON; note the achievements confirmation prompt
   intercepts the first console command) to split sweep-driven from event-driven
   processing.
2. **Per-chunk cost**: `18.1 ms / chunks-per-tick` to size the optimization.
3. **Optimize `calculatePheromoneUpdates`**: inline the per-neighbor accessors, avoid
   repeated query-table indirection, cache the passability checks. Target: bring the
   81% share down.
4. **Re-bisect after optimization** to confirm the shift.

## Why this matters for the refactor

The v4 architecture bounds the *marking* cost (constant 40/tick sweep, independent
of map size), but the *processing* it triggers is still the dominant end-game cost.
The historical wave-based system scaled with scanned map size; this system scales with
activity and the sweep rate. Both are addressable, but the measurement says the
per-chunk pheromone math is where the time actually goes.
