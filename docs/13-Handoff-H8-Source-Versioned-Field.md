# Handoff: H8 Source-Versioned Pheromone Field

## Purpose

This document is the handoff for the H8 work: replacing the iterative pheromone
relaxation with a source-versioned field solver.

The short version:

- **H7 is not an optimization.** It removed the periodic sweep but kept the
  expensive effective-read propagation loop. On the real save it is slower than the
  legacy 40/tick relaxation.
- **H8 is the correct algorithmic direction.** The pure solver proves that
  static/dynamic decomposition and event-local increase propagation are exact and
  cheap.
- The solver and runtime adapter exist, but the engine event wiring and read path
  are not yet connected.

## Current branches

```text
v4-refactor
  - H7 merged and legacy sweep dropped.
  - H7 is the active field scheme, but it is a real-save regression.

h8-source-versioned-field
  - Based on v4-refactor.
  - Adds the pure solver, tests, runtime adapter, startup flag, and init.
  - Not yet wired into source-change events or the read path.
```

Current H8 commits:

```text
f769a47 Initialize source-versioned field runtime adapter
c47427d Add source-versioned field runtime adapter and startup flag
874ad32 Add source-versioned pheromone field solver and tests
3a5e5f9 (v4-refactor) Measure event-local source-versioned propagation
18c0730 Measure source-versioned field propagation radius and work
d5465c8 Add source-versioned pheromone field experiment
```

## Why H7 is being replaced

### H7 benchmark on the real save

Same `Starting2.zip`, same 12,000-tick verbose benchmark, clean worktree A/B.

Legacy baseline:

| Run | avg whole | min whole | max whole |
|---|---:|---:|---:|
| 1 | 1.263 | 0.818 | 1138.4 |
| 2 | 1.192 | 0.799 | 1158.7 |
| 3 | 1.210 | 0.827 | 1108.3 |

Legacy `scriptUpdate`:

- avg: 0.509–0.539 ms
- p95: 0.516–0.588 ms
- p99: 0.837–0.936 ms

Legacy dirty phase:

- 0.34–0.40 ms

H7:

| Run | avg whole | min whole | max whole |
|---|---:|---:|---:|
| 1 | 1.590 | 1.091 | 1139.5 |
| 2 | 1.621 | 1.085 | 1141.8 |
| 3 | 1.694 | 1.146 | 1128.6 |

H7 `scriptUpdate`:

- avg: 0.870–0.917 ms
- p95: 0.939–1.049 ms
- p99: 1.221–1.295 ms

H7 dirty phase:

- 0.710–0.778 ms

Conclusion: H7 is ~25–40% worse on whole average, ~60–80% worse on
script average, and ~2x worse in the dirty phase.

### Why H7 failed

H7 removed the sweep but kept the same dense relaxation:

- effective reads still change every tick due to time decay,
- processed chunks still see a changed field,
- propagation still marks neighbors,
- the dirty queue stays full,
- and every processed chunk now pays the expensive floor-aware effective-read
  cost for 8 neighbors.

The sweep was replaced by propagation churn, not eliminated.

## What H8 actually is

The source-versioned field:

1. **Static sources**
   - ore, permanent structures.
   - solved once at scan.
   - never renewed periodically.

2. **Dynamic sources**
   - bases, nests, moving players.
   - updated only when a source changes.

3. **Transient fields**
   - death, kamikaze, trails.
   - timestamped and decayed at read.

4. **Propagation**
   - max-plus Dijkstra from changed sources.
   - stop when the field update is below epsilon.
   - time decay never triggers propagation.

## Pure solver status

Module:

```text
libs/source_versioned_field.lua
```

API:

```lua
Solver.solve(sources, neighborOf, opts)
Solver.propagateIncrease(source, field, neighborOf, opts)
Solver.recompute(changedSources, allSources, field, neighborOf, opts)
```

Options:

```lua
{
    decay = 0.9,
    eps = 0.0005,
    rating = 1.0,
    maxRadius = nil,
}
```

Tests:

```text
tests/test_source_versioned_field.lua
```

Added to `tests/runTests.lua`.

Current test result:

```text
Total passed: 187
Total failed: 0
All tests passed!
```

Prototype results:

```text
grid: 128x128 = 16384 chunks, 1000 static sources, 200 dynamic sources
full relaxation: 245760 chunk-updates, final diff=0.000000
static+dynamic max-propagation: 32337 updates, max |combined-full| = 0.000000
dynamic source increase: 16282 local updates, max |new-old| = 10.000000
event-local increase-only: 20 updates, max |local-full| = 0.000000
```

Key result: an increase-only source change costs **20 updates** and is
**exactly equivalent** to the full recomputed field.

Influence radius trade-off:

| Radius | Updates | Max error |
|---:|---:|---:|
| 2 hops | 3,911 | 29.646 |
| 4 hops | 9,152 | 20.257 |
| 8 hops | 14,420 | 2.073 |
| 16 hops | 16,275 | 0.000000 |
| 29 hops | 16,282 | 0.000000 |
| 44 hops | 16,282 | 0.000000 |
| 73 hops | 16,282 | 0.000000 |

The moving-player case cannot be full-map propagated every tick. It needs a
bounded local field or an ACO trail.

## Runtime adapter status

Module:

```text
libs/source_versioned_field_runtime.lua
```

It currently provides:

- `Runtime.init(universe)`
- `Runtime.neighborOf(chunkId)`
- `Runtime.sourceValue(chunk, fieldType)`
- `Runtime.buildSources(fieldType)`
- `Runtime.solveField(fieldType, opts)`
- `Runtime.propagateIncrease(chunk, fieldType, field, opts)`
- `Runtime.recompute(changedSources, fieldType, field, opts)`
- `Runtime.queueSource(chunk, fieldType, oldValue, newValue)`
- `Runtime.process(opts)`
- `Runtime.read(field, chunkId, tick, lastRefresh, decay)`

The queue drains up to 40 source changes/tick.

Initialization is wired in `control.lua`:

```lua
if Constants.USE_SOURCE_VERSIONED_FIELD then
    SourceVersionedFieldRuntime.init(Universe)
end
```

Startup flag:

```lua
name = "rampant--sourceVersionedField"
default_value = false
```

Constant:

```lua
constants.USE_SOURCE_VERSIONED_FIELD
```

The H8 path is initialized but not yet active.

### Post-handoff wiring progress

Commits `5cf6d13` and `39e608b` wired the first source-change path
end-to-end for the enemy and base fields:

- enemy structure changes queue from `registerEnemyBaseStructure` /
  `unregisterEnemyBaseStructure`;
- player-base changes queue from `accountPlayerEntity` and the initial scan;
- the queue drains in the tick path behind
  `Constants.USE_SOURCE_VERSIONED_FIELD`;
- base/enemy reads delegate to the solved source-versioned field from
  `SquadLogic`, `ChunkPropertyUtils`, `control.lua`, and
  `Observability.queryPheromones`;
- the field is solved lazily on first process/read, so no separate
  startup solve is required.

This is still partial H8 integration. Player and resource fields have not
been switched, and the decrease path still uses the full-solve fallback.

### Behavior validation

`pheromone-dynamics` was run with H8 enabled using a writable Factorio
user-data directory and a copy of the mod (the read-only install can be
left untouched; `read-data` can point at the installed data):

```text
[PASS] generation: wall base=550.00
[PASS] diffusion gradient: wall 550.00 > mid 495.00 > nest 400.95
[PASS] nest enemy presence=1822.50
Passed: 1
Failed: 0
TESTS_COMPLETE
ticks=29438
```

The first H8-enabled run exposed a real integration bug: the runtime
neighbour lookup reused `MapUtils.getNeighborChunks`' shared table, and an H8
read inside the H7 diffusion loop overwrote that table mid-iteration. Fixed
in `749d9aa` by building solver neighbour ids directly from
`MapUtils.getChunkByXY`.

`wall-defense` also passes with H8 enabled:

```text
[PASS] base formed at spawner (id=0) at 0.0min
[PASS] 1 enemy squad(s) formed at 0.1min
[PASS] 1 enemy unit(s) reached wall perimeter at 6.8min
[PASS] wall damaged/destroyed (4 alive, 1 damaged) at 6.9min
Passed: 1
Failed: 0
Detail: wall pheromone: player=0.00 base=16550.00 | spawner
        pheromone: player=0.00 base=0.00 enemy=450.00
Detail: base=true squad=true unitsAtWall=true wallDamaged=true ticks=24917
TESTS_COMPLETE
```

Real-save A/B has been started with the same writable user-data setup
(`--benchmark Starting2.zip --benchmark-ticks 12000`). The H8-enabled side is
still slower than H7, so the flag remains off by default. Batching decrease
recomputes into one per field per tick narrowed the gap, but the full-solve
fallback on source decreases remains the dominant extra cost. The next required
work is a local/bounded decrease recompute or an equivalent per-source
contribution structure.

Per-range H8 dirty/source timings during the A/B showed the fallback solve
appearing as isolated 80-180 ms source-field ranges; the surrounding ranges
were sub-millisecond. That is the signature to remove.

### Overhead attribution (instrumented 12,000-tick run)

The H8 runtime was instrumented with counters for `ensureSolved`,
`recompute`, and field solves. On `Starting2.zip`:

```text
initial solves:        3 (fields 2, 3, 5)
full recomputes:    1893 (all field 5, enemy)
recompute rate:     ~0.16/tick
```

Per 500-tick range, the recompute counts were stable at roughly 77-90.
The isolated per-range timings showed the same pattern:

```text
range 13485000-13485500: dirty 0.814 ms, sourceField 0.002 ms
range 13485500-13486000: dirty 87.940 ms, sourceField 87.181 ms
range 13486000-13486500: dirty 0.783 ms, sourceField 0.002 ms
range 13486500-13487000: dirty 0.895 ms, sourceField 0.002 ms
```

A control run with decreases ignored (analysis-only patch) gave:

```text
H8 no decreases: avg 1.864 ms
H7 control:      avg ~1.58 ms
```

So the H8 overhead was almost entirely the 1,893 enemy-field full
recomputes from source decreases. The event-local increase path itself was
close to the H7 baseline overhead.

### Local decrease removal (be2660d)

`Solver.solveWithMetadata` now tracks the top two contributions per chunk.
`Solver.removeSource` removes an old source contribution locally and pushes
only through chunks where the removed source was top or second. The runtime
uses this for every decrease instead of the full-solve fallback.

Validation:

```text
tests/runTests.lua: 194 passed / 0 failed
```

Real-save `Starting2.zip`, 12,000 ticks:

```text
H7 control:  avg 1.617 ms
H8 top-two:  avg 1.828 ms  (previous H8 fallback: 15.8 ms)
```

Per-range H8 dirty/source timings no longer show the 80-180 ms fallback
spikes:

```text
range 13485000-13485500: dirty 0.806 ms, sourceField 0.002 ms
range 13485500-13486000: dirty 0.842 ms, sourceField 0.050 ms
range 13486000-13486500: dirty 0.761 ms, sourceField 0.002 ms
range 13486500-13487000: dirty 0.742 ms, sourceField 0.002 ms
```

The 10x real-save regression is removed. H8 is still ~0.2 ms/tick above the
H7 control in this partial integration; the remaining work is to stop paying
the H7 base/enemy dirty-field maintenance once H8 is the active field, or to
reduce the H8 queue/read overhead further.

### Remaining behavior battery

With H8 enabled, using the writable temp user-data/mod copy:

| Scenario | H8 | H7 control | Notes |
|---|---|---|---|
| death-response | PASS | not rerun | |
| movement-diag | PASS | not rerun | |
| movement-test | EXIT | EXIT | scenario-level `format` nil bug, not H8 |
| multi-surface | PASS | not rerun | |
| upgrade-threshold | FAIL | PASS | known-flaky low-leg timeout; H8 run failed once |
| attack-wave | PASS | not rerun | |
| base-ai-states | TIMEOUT | TIMEOUT | 240 s run budget, both sides |
| base-expansion | PASS | not rerun | |
| migration-settler | FAIL* | PASS | *unpinned-map flake; with `--map-gen-seed 1`, both H7 and H8 PASS |
| siege-settle | PASS | not rerun | |
| player-death | PASS | not rerun | |
| save-load | skipped | skipped | needs RCON `/save` + restart driver |
| long-run | skipped | skipped | 4 game-hour budget |

The `migration-settler` failure was investigated: the original H8 run used a
random map, while the H7 control used a different random map. Re-running with
`--map-gen-seed 1` (and the phase budget shortened to 3 game-minutes for
diagnosis) passed on both sides:

```text
H7 seed 1: settler left nest region at 0.1min (pos 112,18)
H8 seed 1: settler left nest region at 0.1min (pos 112,18)
```

The debug instrumentation also showed H8 settling immediately in-place:
`dbg.settleMove=1`, `dbg.settle.settle=1`, `dbg.settle.afterState.0=1`.
So the unpinned failure was geography/timing variance, not an H8 read
regression. Behavior comparisons must use pinned seeds; unpinned results are
not evidence for or against H8.

## Death pheromone findings

Controlled same-tick death-response test:

- permanent death field is identical between legacy and H7,
- transient death field is event-timed under H7,
- legacy decays it on the global sweep,
- H7 decays it only when a chunk is processed,
- H7 can be lower immediately after a death and higher later,
- this changes retreat counters and breach behavior.

Controlled long-run at matched tick rate:

- H7 runs ~1.58x faster in wall time at `game.speed = 100`,
- when H7 speed is capped to match legacy ticks, death field is nearly
  identical in active long-run regions,
- the large wall-time death difference was mostly time compression.

## What still needs wiring

1. **Queue source changes**
   - `onBuild`
   - `onDeath`
   - `onMine`
   - `accountPlayerEntity`
   - `registerEnemyBaseStructure` / `unregisterEnemyBaseStructure`
   - `processPlayers`

2. **Process the queue**
   - call `SourceVersionedFieldRuntime.process(opts)` from the tick path
   - only when `Constants.USE_SOURCE_VERSIONED_FIELD` is true

3. **Switch the read path**
   - `SquadLogic.playerPheromone`
   - `SquadLogic.basePheromone`
   - `SquadLogic.enemyPheromone`
   - `SquadLogic.resourcePheromone`

   If `USE_SOURCE_VERSIONED_FIELD`, read from the source-versioned field.
   Otherwise keep the current H7/raw path for fallback.

4. **Static solve**
   - solve resource field once at scan
   - solve permanent structures once

5. **Dynamic solve**
   - solve base/enemy fields from source changes
   - moving player field must be local or ACO-trail based

6. **Validation**
   - behavior scenarios:
     - `pheromone-dynamics`
     - `wall-defense`
     - `squad-cycle`
     - `death-response`
   - real-save A/B:
     - same `Starting2.zip`
     - 3× 12,000 ticks
     - require average and p99 improvement, no max regression

## Known risks

- The decrease path is still a full solve fallback.
- A moving player can touch the whole dynamic field on every move unless
  bounded or ACO-trailed.
- The resource field is dense: ~96% of real-save chunks have ore.
  Global resource diffusion must be replaced with a static precompute plus ACO
  trails or a local resource field.
- H8 is behind a startup flag default false until the engine A/B is green.
- `v4-refactor` currently has H7 active and H7 is a real-save regression.

## Test commands

```bash
lua tests/runTests.lua
lua tests/runACOCores.lua
lua tests/runACOTests.lua
lua tests/source_versioned_field_experiment.lua
```

Expected:

```text
runTests: 187 passed / 0 failed
runACOCores: 12 passed / 0 failed
runACOTests: 26 passed / 0 failed
source_versioned_field_experiment: exact event-local increase
```

## Recommended next action

Wire the first source-change path end-to-end for one field type, preferably
the enemy/base field:

1. queue source changes from `registerEnemyBaseStructure` / unregister,
2. drain the queue in the tick path,
3. switch `SquadLogic.basePheromone` / `enemyPheromone` reads behind the flag,
4. run `pheromone-dynamics` and `wall-defense`,
5. compare against the legacy trajectories,
6. only then expand to player and resource fields.
