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
