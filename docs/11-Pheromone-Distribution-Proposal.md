# Proposal: Pheromone Distribution & Sweep Consolidation

**Status**: proposed
**Scope**: the pheromone field maintenance (the sweep) and its write path
**Baseline**: `v4-refactor` @ `15e398b`

---

## 1. Problem

The mod's per-tick overhead on a real late-game save is **~0.49 ms/tick** (with
Rampant 1.109 ms vs without 0.613 ms, measured at 12000 ticks x 3 runs — the
spread is ~±0.01 ms, so the resolution is well under a millisecond).

Of that 0.49 ms, the **background sweep is 0.288 ms — 59%** (precise cumulative
bisect). The sweep re-marks 40 chunks/tick from the whole chunk grid so the fields
do not decay away.

**The regression**: the v4 core commit raised `PROCESS_DIRTY_CHUNKS_PER_TICK` from 40
to 200 (5x the pre-2.0 rate). Restoring 40 cut the end-game cost **23.005 -> 3.730
ms (6.2x)**, and an O(1) dirty queue cut it further to **1.143 ms**. That regression
is fixed. What remains is the sweep.

**Why the sweep exists**: decay-on-read evaporates a stored field in ~6.6 ticks
(0.90/tick), so something must re-write it. The event-driven marks only fire on
activity, so static sources' fields would decay to zero in quiet regions.

---

## 2. Findings (measured)

### 2.1 The source model, extracted from the real save

Via the UDP exec channel (`tools/dump-save-model.lua`):

```
chunks: 10739
sources: 10300  (96% of chunks!)
byClass: ore 8117 (79%), nest 1760 (17%), base 423 (4%), death 0, player 0
```

### 2.2 Sources within a reader's reach

Sampled on the real save (`tools/dump-radius`):

| Radius | Sources in reach |
|---|---|
| 4 chunks | **~61** |
| 8 chunks | ~200 |
| 10 chunks | ~294 |

### 2.3 What that implies

- **The source set is not sparse — it is 96% of the map, and the ore is 96% of it.**
- At 0.9/hop with epsilon 0.0005 the decay range is **73 hops** — larger than the
  map. So a naive read-time flood covers the *entire grid* per read.
- A small-scale experiment (`tests/lazy_field_experiment.lua`, 24x24 grid) confirms:
  - the read-time derivation is **exactly** the write-time field (`max |lazy-stored| =
    0.000000`);
  - the naive flood is **576 chunk-visits/read — 14x worse** than the write-time;
  - the indexed read is **4 source-visits/read — 144x better than the flood, 10x
    better than the write-time maintenance**.

### 2.4 What is static and what is not

| Class | Examples | Behavior |
|---|---|---|
| **Static** (piecewise-constant) | ore, nests, spawners, worms, buildings | constant until a build/destroy event |
| **Transient** | death scent, kamikaze, scout trails | decays/moves every tick |
| **Moving** | the player's character scent | the source moves |

**Nearly every source is static** — so nearly every source's field needs no periodic
renewal. The one genuine exception is the **death scent**, whose decay is
*process-driven* in the current code (it decays only while the chunk is processed).

---

## 3. Design

### 3.1 The generator floor (done)

A source chunk's read is `max(generator x rating, decayed)`. The write path already
applies this floor (`newBase = max(generator, neighborMax*0.9) * rating`); the read
accessors did not. Added in `libs/squad_logic.lua` for all four accessors. This is a
correctness fix (read/write consistency) and the base case of any derivation.

### 3.2 The static/dynamic layering

Mark `(chunk, layer)`, not `(chunk)`:

| Layer | Maintenance |
|---|---|
| RESOURCE (ore) | **once at scan** — never renewed |
| BASE (player buildings) | on build/destroy |
| ENEMY (nests/turrets) | on register/unregister |
| PLAYER | on move |
| Death / kamikaze | on death + decay |

The queue becomes layer-aware so a build does not recompute the ore.

### 3.3 The death decay made read-driven

Store the death generator with a timestamp and apply `value x rate^elapsed` at read, as
the other fields do. This removes the **one real dependency on periodic processing** —
the death cloud's decay. (The spread remains: `newDeathGenerator = neighborAvg x 0.75`,
an *average*, not a max — a different derivation operator, and the one to verify first.)

### 3.4 The dynamic-source index

Maintain an index of dynamic-source chunks at the generator setters. The sweep iterates
the index with the rate scaled to the dynamic fraction. **The filter must not be applied
per-chunk at a fixed rate** — measured, that is *worse* (+0.19 ms), because the rate
stays fixed and the test costs more than the processing it gates.

### 3.5 The pure sweep scheduling (done)

`libs/sweep_logic.lua` — the slice/cycle/rebuild arithmetic and the dynamic-source
predicate, extracted pure. `tests/test_sweep_logic.lua` proves the coverage invariant
exhaustively (every entry exactly once per cycle, 9 sizes x 6 rates).

---

## 4. Measured budget

| Component | Cost | Share | Proposal |
|---|---|---|---|
| **The sweep** | **0.288 ms** | **59%** | the split + the index -> ~0.06 ms |
| The pick-based AI | 0.071 ms | 14% | not addressed |
| The event processing | 0.038 ms | 8% | already activity-proportional |
| The always-on AI | 0.037 ms | 8% | not addressed |
| Remainder (handlers, metrics) | ~0.056 ms | 11% | not isolated |

**Ceiling from the split alone: ~0.23 ms** (a ~1.9x total reduction, mod overhead
0.49 -> ~0.26 ms). The read-time derivation was the elegant answer and this save's
density rules it out (61 sources within 4 chunks).

---

## 5. Acceptance criteria

Every change is gated by all of:

1. **The scenario suite** — 6/6 passing (`just test-behavior ...`).
2. **The trajectory baselines** — `git diff integration-tests/trajectories/` shows only
   the intended change. The committed per-(scenario, seed) trajectories are the behavioral
   contract.
3. **The long-run** — the session invariants (the queue drains, index misses zero, UPS).
4. **The CRC gate** — the engine's save/load check must pass. `on_load` must never write
   storage (this caught a real save-breaking bug during the work).
5. **The unit tests** — 187/187; every pure extraction adds exhaustive tests.
6. **The cost** — 12000 ticks x 3 runs; a change must be beyond the ~±0.01 ms spread.

---

## 6. Risks

| Risk | Mitigation |
|---|---|
| The index maintenance is the source-driven trial's problem (ore made it dense) | the index holds DYNAMIC sources only; the static layer is separate |
| The death scent spreads by *average*, not max — a different derivation operator | verify the averaging derivation in the experiment before relying on it |
| The behavior moves (the sweep-elimination trial moved 5/6 trajectories) | the committed trajectories are the gate; the derivation must restore them |
| A new storage structure breaks save/load (CRC) | derive structures from the chunk grid; never create/migrate in `on_load` |

---

## 7. Phasing

1. **The static/dynamic layering + the index** — the ~0.23 ms ceiling, and the
   prerequisite for everything else.
2. **The death decay read-driven** — removes the last real sweep dependency.
3. **Re-measure** at 12000 ticks x 3 runs; diff the trajectories.
4. **The read-time derivation** — only for the rare/long-range readers (formation,
   target selection), if the index makes it pay; not for the frequent/local movement reads.

---

## 8. Non-goals

- The read-time derivation as a general replacement for the sweep — falsified on this
  save by the source density.
- Touching the pick-based AI (0.071 ms) or the always-on AI (0.037 ms) in this proposal.
- Changing the field math (the max-with-decay, the ratings) — prod-parity, verified.

---

## 9. Hypothesis test: scale the sweep rate (recorded)

**Hypothesis**: the split's cost ceiling is the sweep rate x the dynamic fraction, so
scaling the rate (40 -> 8, the dynamic fraction) bounds it.

**Cost — CONFIRMED.** Same save, 12000 ticks x 3 runs:

| Sweep rate | avg (3 runs) | spread |
|---|---|---|
| 40 | 1.105 / 1.110 / 1.112 ms | ±0.004 |
| **8** | 0.903 / 0.894 / 0.896 ms | ±0.005 |

Saving **0.211 ms** (predicted 0.23). The sweep's cost is its **rate**, not its list
size — this is the confirmation.

**Behavior — BROKEN.** All six scenarios **fail** at rate 8. The fields refresh 5x
less often and the AI can no longer steer. **The rate cannot drop globally.**

**Max — UNACCEPTABLE.** The max tick is **~15 ms at rate 8** (vs ~32 ms at 40) — near
the 16.7 ms frame budget, i.e. a visible stutter. The max scales with the rate, which
points at the sweep's periodic **burst** (the cycle-rollover list rebuild over the whole
chunk grid via `pairs()`) rather than the per-tick slice.

**Conclusion**: the static/dynamic split is the answer *precisely because* it keeps the
rate at 40 for the **dynamic** chunks (behavior preserved) while excluding the static
ones (cost down). Dropping the rate globally is not viable — it breaks the behavior and
leaves the max unacceptable. The max burst is its own investigation (the rebuild).

**Revised phasing**: (1) the static/dynamic split, (2) the rebuild burst (the max), then
(3) the death decay, then (4) the derivation for the rare/long-range readers.

## 10. Hypothesis test: the rebuild burst (recorded)

**Hypothesis**: the max tick (~32 ms) is the cycle-rollover sweep-list rebuild — a
`pairs()` over the whole chunk grid (~10,739 chunks) in one tick.

**Change**: replaced `pairs()` with `next()` in bounded steps
(`SWEEP_REBUILD_STEPS_PER_TICK = 40`), the old list swept meanwhile so the sweep
never stalls. This is correct regardless (per the "pairs is unacceptable" rule) and it
is a net improvement for the rebuild — but it is **not** the max.

**Measured, same save, 12000 ticks x 3 runs:**

| | avg | max |
|---|---|---|
| baseline | 1.109 | 31–33 ms |
| incremental rebuild | 1.101 | 26–30 ms |

**FALSIFIED**: the max is essentially unchanged, so the cycle-rollover rebuild is NOT
the burst. The max is therefore elsewhere — the prime suspect is the **load/init path**
(`initialScan`/`rescanAllChunks` over the whole grid in one tick, or the engine's own
chunk generation), which is a one-time cost at load, not a per-tick burst.

**Next for the max**: measure WHERE in the window it lands (first ticks vs mid-run) and
isolate the load path. The incremental rebuild stays (correct, and it removes a
map-size-proportional one-tick spike from the design regardless).

## 11. Hot-path scan: pairs / concat / allocation (recorded)

Scanned libs/ for `pairs()`, string concatenation, and per-call allocation in the
hot path.

**1. `Metrics.inc` string-keyed counters — FIXED.** Called four times per chunk in
the hot path (`dirty.propagationMarks`, `dirty.epsilonSkips`, `dirty.sweepMarks`,
`dirty.chunksProcessed`); each was a string-keyed hash lookup on `counters[name]` —
the same class as the queue keys. Now the hot path increments a per-tick accumulator
table passed in by the caller, flushed to Metrics once per tick (public metric names
preserved). Measured: avg 1.102 vs 1.109 (~0.007 ms) — small, as expected from
~4 lookups/chunk.

**2. `MapUtils.getEffectivePheromone` — DEAD.** It builds `type .. "LastRefresh"`
(a concat per call) and compares `type == "PLAYER_PHEROMONE"` etc. (string compares).
No callers (only the `ChunkPropertyUtils` wrapper, itself uncalled). Delete or leave;
it is not hot.

**3. `pairs()` — FIXED.** The only per-tick `pairs()` was the sweep-list rebuild,
now `next()` in bounded steps. The remaining `pairs()` are in on-demand paths
(Observability queries) or one-time migrations.

**4. Allocations — mostly addressed.** The per-mark `{id=,priority=}` tables and the
two snapshot tables are gone (direct marking, locals). The remaining per-chunk
allocations are the queue's `table.insert` (necessary) and the metrics ring buffer.

**The max remains ~28–29 ms** — unchanged by all of the above, so it is not the sweep,
not the rebuild, and not the metrics. The two candidates left are the **load/init scan**
(`initialScan`/`rescanAllChunks` over the whole grid in one tick) and the **Lua GC**.

## 12. Upper-end isolation: a SIGSEGV at ~25,400 ticks (recorded)

Long-window benchmark (`--benchmark-ticks 50000`) on the real save:

```
Running update 25400
timeout: the monitored command dumped core
exit=139 (SIGSEGV)
```

**The upper end is not a slow tick — it is a hard crash (SIGSEGV) at ~25,400 ticks
(~7 game-minutes in), with no Lua error message (a C-level crash).**

The log showed repeated `[DEPLOY] centroid failed: centroid=(n,n) units=0`, i.e. NaN
centroids. Those are a **red herring**: the centroid division IS guarded
(`if unitCount > 0`), so the NaN only ever existed in the diagnostic `print` (now
removed — it was debug scaffolding producing a ~7 MB log). Removing it did not stop
the crash (exit 139 again).

Candidates for the crash: the engine's own instability at long runs (the save is Space
Age with platforms), the evolution-threshold rescan burst (`rescanAllChunks` over the whole
grid in one tick), or a mod engine call receiving a bad value. **No Lua traceback means a
C-level fault, so the next step is the core dump's stack (`coredumpctl`/gdb) or a narrower
repro at the ~25,400-tick mark.**

Note: `--benchmark` on the /tmp copy shows map version 2.1.14-1 and a larger file than
the original (25.7 MB vs 24.2 MB) — the earlier UDP server runs saved the /tmp COPY. The
original save is untouched (it was copied out first).
