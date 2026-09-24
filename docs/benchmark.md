# Benchmark — reference save

One row per measured commit. `h7` = `rampant--sourceVersionedField` off
(shipped default); `h8` = on. Whole/script are min-max across runs; spike counts
are from one exact-telemetry run and are not stable run-to-run.

Config: `Starting2.zip` (load tick 13,475,000), Factorio 2.1.14, 12,000 ticks.
Machine: 32 cores, load 1.1-5.0 (self-inflicted, no drift across runs).

## Per commit

| sha | date | side | whole avg | script avg | whole p99 | dirty >1ms | dirty max | srcFld >1ms | srcFld max | pending |
|---|---|---|---|---|---|---|---|---|---|---|
| a284c07 | 2026-09-22 | h7 | 1.671-1.745 | 0.928-0.951 | 2.878-3.323 | 352 | 7.27 | - | - | 10.9-12.8k |
| a284c07 | 2026-09-22 | h8 | 1.204-1.269 | 0.462-0.485 | 2.112-2.458 | 6 | 4.25 | 56 | 202.65 | ~1.0k |
| cae9b52 | 2026-09-23 | h8 | 1.245-1.280 | 0.476-0.489 | - | 24 | 169.58 | - | - | - |
| 2dd08e7 | 2026-09-23 | h8 | 0.782-0.797 | 0.118-0.119 | - | 69 | 2.25 | - | - | - |

## Per-range whole/script, a284c07 (original save)

The whole-run average is startup-inflated: the scan flush and the first solves
land in range 0-999. Steady state is the 1000+ rows.

| range | whole avg | script avg |
|---|---|---|
| 0-999 | 3.008 | 2.041 |
| 1000-1999 | 1.210 | 0.335 |
| 2000-2999 | 1.062 | 0.289 |
| 3000-3999 | 0.992 | 0.293 |
| 4000-4999 | 1.059 | 0.357 |
| 5000-5999 | 0.984 | 0.294 |
| 6000-6999 | 1.178 | 0.339 |
| 7000-7999 | 1.085 | 0.333 |
| 8000-8999 | 1.111 | 0.364 |
| 9000-9999 | 1.044 | 0.332 |
| 10000-10999 | 0.971 | 0.314 |
| 11000-11999 | 1.042 | 0.371 |

So the whole-run averages in the per-commit table (1.204-1.269 / 0.462-0.485) are
not steady state; steady state is ~1.0 / ~0.33. The 2dd08e7 row's 0.782-0.797 /
0.118-0.119 is a settled save (already-scanned, already-solved, drained queue),
so it is not comparable to the a284c07 rows either.

`pending` = `Universe.dirtyCount` at steady state. h7 sits at the passable map
size (12,861 chunks; engine `surface.get_chunks()` = 19,494 generated) and never
drains; h8 drains to ~1,045. That is the structural difference: h7's decay-on-read
changes every read, so the epsilon gate never fires and the propagation cascade never
terminates.

## Per-field cost (exact telemetry, cae9b52, 12,000 ticks)

The resource field is **not** the cost. Dense sources are cheap — many sources
saturate each node quickly, so the flood does few relaxations per node. Sparse sources
are expensive — each wavefront travels far before being dominated.

| field | range-avg ms/tick | range-max | ticks >1ms | first solve |
|---|---|---|---|---|
| enemy | 0.032-0.105 | 0.718-3.919 | 24-37 | 141-192 ms |
| base | 0.002 | 0.008-0.078 | 36 (all range 0) | 51-53 ms |
| resource | 0.003 | 0.007-0.077 | 1 | ~4.4 ms |
| player | 0.002 | 0.005-0.106 | 0 | ~0.03 ms |

## Persisted solved state (cae9b52, 3,000 ticks)

Same content: the original save (no solved state) vs a save written after the first
solve. Eliminating the first solve is the largest single win.

| field | original max / total ms | persisted max / total ms |
|---|---|---|
| base | 51.484 / 446.60 | 0.040 / 5.62 |
| enemy | 141.120 / 247.25 | 1.056 / 168.07 |
| resource | 0.075 / 7.99 | 0.037 / 7.35 |
| player | 0.064 / 5.66 | 0.011 / 5.56 |

## dirty per 500-tick range, a284c07 (avg / max, ms)

| range | h7 | h8 |
|---|---|---|
| 0-499 | 0.815 / 1.629 | 0.211 / 0.522 |
| 500-999 | 0.724 / 1.019 | 0.212 / 0.344 |
| 1000-1499 | 0.772 / 1.175 | 0.261 / 3.005 |
| 1500-1999 | 0.803 / 2.036 | 0.217 / 0.337 |
| 2000-2499 | 0.764 / 1.241 | 0.211 / 0.359 |
| 2500-2999 | 0.760 / 1.611 | 0.226 / 0.715 |
| 3000-3499 | 0.731 / 0.987 | 0.213 / 0.397 |
| 3500-3999 | 0.756 / 1.179 | 0.214 / 0.326 |
| 4000-4499 | 0.821 / 5.990 | 0.205 / 0.280 |
| 4500-4999 | 0.751 / 1.113 | 0.215 / 0.691 |
| 5000-5499 | 0.745 / 1.046 | 0.209 / 0.345 |
| 5500-5999 | 0.771 / 4.443 | 0.203 / 0.456 |
| 6000-6499 | 0.759 / 1.151 | 0.222 / 0.727 |
| 6500-6999 | 0.827 / 7.266 | 0.210 / 0.371 |
| 7000-7499 | 0.758 / 1.181 | 0.220 / 2.335 |
| 7500-7999 | 0.766 / 1.127 | 0.227 / 3.225 |
| 8000-8499 | 0.793 / 4.064 | 0.204 / 0.348 |
| 8500-8999 | 0.760 / 1.234 | 0.211 / 0.358 |
| 9000-9499 | 0.766 / 1.216 | 0.226 / 2.975 |
| 9500-9999 | 0.769 / 1.668 | 0.208 / 0.355 |
| 10000-10499 | 0.746 / 1.080 | 0.210 / 0.323 |
| 10500-10999 | 0.761 / 1.661 | 0.233 / 4.251 |
| 11000-11499 | 0.758 / 1.062 | 0.230 / 0.390 |
| 11500-11999 | 0.766 / 1.394 | 0.214 / 0.383 |

## Behavior battery — H8, seed 1

`tools/behavior-local.sh <name> <seed>` stages the scenario and the prepared
save into `.bench/user-data` (the install dir is read-only, so `just
test-behavior` cannot run here). This made the battery runnable and it immediately
caught a regression the cost benchmark and the unit tests could not see: the decay
decoupling had `setDeathGenerator` clear the decay timestamp on every processing
pass, so the death generator never decayed, the death rating stayed suppressed and
the base pheromone collapsed to 0. Fixed in bce2ab8.

| scenario | H8 (bce2ab8) | committed a284c07 (h7) |
|---|---|---|
| pheromone-dynamics | PASS wall=550 mid=495 nest=400.95 | PASS wall=550 mid=495 nest=285.32 |
| wall-defense | PASS base squad unitsAtWall wallDamaged | PASS (baseline) |
| death-response | PASS base retreat pointsDropped preKillPoints=2000 | PASS (baseline) |
| squad-cycle | PASS base attack retreated reEngaged statusAfter=4 | PASS (baseline) |
| migration-settler | PASS base settlerFormed dispatched moved site:2 | PASS (baseline) |

## The h7/h8 field-value difference, and its cause

`pheromone-dynamics` passes on both sides but the nest value differs: h7 285.32
against h8 400.95 (+40%). h8's value is exactly `550 * 0.9^3`, i.e. the wall's
value diffused three hops with no time decay; h7's is that value times a further
~0.71, i.e. the decay-on-read at the nest chunk.

Cause: the read-time decay is **not wired**. `Runtime.read(field, chunkId, tick,
lastRefresh, decay)` exists and the solver's docstring says "time decay is applied
by the read path, not by propagation", but `Runtime.readChunk` returns
`field[chunk.id] or 0` undecayed, and `Universe.sourceVersionedRefresh` is
created and never used. So h8's source-versioned reads are systematically higher
than h7's effective reads by exactly the elapsed-time factor.

Whether that is a bug or the intended semantics is a design question the code does
not settle: the base/enemy/resource fields are maintained by source changes, so they
do not need a time decay, while the solver docstring says the read path applies one.

Note on sensitivity: `pheromone-dynamics` asserts the gradient ordering
(wall > mid > nest), which both sides satisfy, yet H8's nest value is 400.95 against
h7's 285.32 (+40%). An ordinal assertion does not catch a 40% value shift, so the
battery is a regression guard, not a parity check.

## The load confound, and the corrected bound A/B (ade13e5)

A measurement taken while the machine was at load 24.95 read the propagation
bound as a regression (enemy avg 0.0572 -> 0.0796, max 2.250 -> 8.069) and it was
reverted. That measurement was confounded: the rows above were taken at load 1.1.

Re-measured interleaved (both sides in the same session, same save, same 12000
ticks, load 6.9-11.2):

| pair | no-bound avg / max / >1ms | bound avg / max / >1ms |
|---|---|---|
| 1 | 0.0712 / 4.896 / 188 | 0.0614 / 5.203 / 16 |
| 2 | 0.0569 / 3.598 / 74 | 0.0525 / 1.691 / 5 |
| 3 | 0.0582 / 2.871 / 80 | 0.0524 / 1.530 / 4 |

The spike count is separated in all three pairs (74-188 -> 4-16, a 5-47x cut) and the
average overlaps, with the bound slightly better in the two cleaner pairs. So the bound
does what it was designed to do: it trades a marginally higher average for a much
lower spike count. Restored in ade13e5.

Three bugs in it were caught by other instruments: the frontier was only resumed when a
new change arrived and process() returned early on an empty queue (both by
pheromone-dynamics), and ensureStorage never created the frontier table for an existing
save (a crash on the load path, caught by the benchmark).

**Always check `uptime` first and prefer an interleaved A/B.** A load shift of this
size moves the floor by more than the effect being measured.

## Caveats

- Spike counts are not stable run-to-run on the same config (`dirty >1ms`
  measured 352 and 718). Whole/script averages are stable (<5% spread over 5 runs).
- Cost only. Richness = the committed `integration-tests/trajectories/`.

## Reproduce

Factorio's install dir is read-only, so benchmark from a mod copy with its own
user-data dir (`config.ini`: `read-data=<install>/data`, `write-data=<scratch>`).
`h8` = set `rampant--sourceVersionedField` `default_value = true` in the copy.
A persisted-state save needs `<scratch>/user-data/saves/` to exist, then RCON
`/save <name>` on a loaded server.

    python3 tools/benchmark-breakdown.py --save <install>/saves/Starting2.zip \
      --mod-directory <scratch>/mods --config <scratch>/user-data/config.ini \
      --ticks 12000 --bucket 1000 --label <sha>-<side> --output <scratch>/out/<label>.out \
      [--subsystem-telemetry]
