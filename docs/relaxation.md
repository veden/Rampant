# Relaxation fields — design and acceptance

Status: experiment. Implemented behind `FIELD_OPTS[field].relax`; fields without the
flag keep the Dijkstra + provenance solver untouched.

## The algorithm

The field is the fixed point of one equation:

```
value(v) = max( ownSource(v), max over neighbours n of ( value(n) * decay ) )
```

- a source write marks that chunk dirty and pushes it on a FIFO frontier
- per tick, pop up to `RELAX_POPS_PER_TICK`; recompute the node from its own source and
  its neighbours' values; if the result differs from the stored value by more than the
  field's `eps`, store it and push the neighbours
- the frontier empties when nothing changes: quiescent, zero cost per tick
- distance is carried per node (`dist = neighbourDist + 1`) so `maxRadius` still caps
  reach; the field stays sparse (`nil` instead of 0)

Two properties make it work, both caught by the benchmark when they were wrong:

- **recompute REPLACES the value.** `max(old, computed)` never falls.
- **a dirty node is pushed without a value.** Seeding the final value makes the first
  recompute a no-op and the frontier quiesces with an empty field.
- the frontier needs an explicit `tail`: `queue[head] = nil` makes `#queue` ambiguous
  and silently truncates the wave to one ring.

## What it deletes for a relaxed field

`sourceVersionedProvenance`, `sourceVersionedSecond`, `Solver.removeSource`, the removal
heaps and their resume budget, the 5% report throttle, the periodic resync, and the
epoch/stale-build handling — and with them the drift bug class (stranded removals, lost
increases, compounding throttle, rebuild-swap loss, unpropagated chunk removal).

## Knobs

```
GAMEPLAY   decay per field (reach), maxRadius (hard cap), source magnitudes, eps
PERFORMANCE RELAX_POPS_PER_TICK, drain pops, reconcile interval, throttle
```

`eps` must be chosen per field: measured ranges on the user's save are base min 167,
enemy 206, resource 2,353 (a 5e-4 eps is ~1e-6 of those) but the death field spans
1e-5..1e-1, where 5e-4 sits inside the range and hides a single death. Reach is
`log(eps/source)/log(decay)`, so eps, decay and magnitude are coupled.

## Acceptance

A change is accepted only if all four hold.

```
1 exactness      verifyField(name, 0.01): max|diff| <= eps, zero chunks off by >1%
                 (the oracle solves from sources with the current code, so it is
                  independent of which solver built the live field)
2 determinism    fieldDigest(name) identical across two independent runs from the
                 same save; verified for the current build:
                   base  n=751 sum=3982488.352 min=314.63 max=23220 hash=925937918
                   enemy n=7723 sum=34875577.764 min=0 max=19500 hash=3358846350
3 cost           matched A/B, two runs per side: [SUB] field.solve/sourceFields/
                 sf.drain/sf.resume, allocMark/allocRead, heapMark/heapCensus,
                 [TICK] avg/p50/p90/p99/max/over5ms
4 gameplay       pheromone-dynamics 550/495/400.95, base-expansion, multi-surface,
                 wall-defense (known flake: reachedState4=nil on some runs)
```

## Diagnostics

```
fieldDigest(name)              n/sum/min/max/order-independent hash
fieldExport(name)              script-output/rampant/field-<name>.csv
fieldRanges()                  per-field count/min/under1/underEps
verifyField(name, tol)         counts vs a fresh exact solve
fieldDiff(name, n)             sample mismatched chunks with source state
fieldDiag(name, chunkId)       one chunk: live, exact, decay, radius, top/second
rebuildField(name)             one-shot exact solve now
tools/relax-bench.lua          standalone comparison of the three algorithms
```

## Toolbench reading (128x128, 200 sources, decay 0.9, eps 5e-4)

```
                              ms      pops/recomputes
dijkstra full solve        25.64       16,469
relax full settle          18.65       40,071
wavefront full settle      47.96      212,992   (13 passes)
dijkstra increase (1 src)   0.23          276
relax increase (1 src)      0.30          665
relax agrees with dijkstra exactly: max|diff| = 0.000
```

Relaxation costs ~2.4x the operations but ~30% less wall time (no heap, no provenance
arithmetic); the original wavefront costs 5-13x either because it recomputes settled
nodes every pass. The case for relaxation is deleting machinery, not raw speed.
