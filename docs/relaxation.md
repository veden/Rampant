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

## Convergence and the residual

A decrease converges geometrically: each pass multiplies the far value by `decay`, and a
two-node loop by `decay^2`. The frontier stops when a pass moves a value by less than
`eps`, so it settles at a **bounded residual of about `eps / (1 - decay)`** (measured
0.00246 for eps 5e-4, decay 0.9, i.e. `eps/(1-decay^2)` in the two-node case) rather
than exactly zero. Against field values in the hundreds that is ~1e-5 relative; against
the death field's 1e-4..1e-1 it is ~1e-2 relative, which is why death gets its own eps.
Decreases are therefore bounded-accurate, not exact, and slower than increases (73
passes for 1.0 to reach eps at decay 0.9).

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

## Removals are the expensive direction — truncate, do not decay

An increase propagates outward from the new source and is exact after one wave. A removal
leaves a FOSSIL DISC: the cone that the vanished source was holding up is a ring of
mutually supporting values, each exactly one decay factor below its neighbour. Re-popping
the peak of that ring lowers it by one decay factor; the next node follows; and the disc
needs ~10 rounds (at decay 0.9) before it falls from 15,438 to the 5,243 the exact solve
justifies. Every round is a pop per node in the disc.

Measured on the `Starting2` save (radius-24 resource field, ~1,800 nodes per disc):

```
single removed source chunk      73 -> ~600 mismatched chunks, maxDelta 10,194
                                 (live 15,438 where the exact solve justifies 5,243)
rate of healing                  ~10% per propagation round, i.e. minutes
after `reseedAll` (clear+rebuild) 0 mismatched chunks
```

So `Runtime.dropChunk` CLEARS every entry within `maxRadius` of the removed chunk (bounded
BFS) and re-queues it, and the wave refills from the sources that are still there in one
pass. Capped at `DISC_CLEARS_PER_TICK` discs: a mass removal (an idle surface sweep) has
already cleared the area after the first few, and the single-node drop is equivalent there.

Two consequences worth knowing:

- The sweep that clears a disc also re-queues it, so the refill is bounded by the
  propagation budget. A disc refills in ~40 ticks at a 32-pop slice, which is why the
  cleared side heals in a second or two while the fossil side took minutes.
- A *source decrease* that is reported through `queueSource` still takes the slow path. In
  the save measured, no resource source decreased during play (reports flat at 774), so the
  removals were the whole story; if a decrease ever shows up as drift, it takes the same
  truncation treatment.

## Diagnostics

```
fieldDigest(name)              n/sum/min/max/order-independent hash
fieldExport(name)              script-output/rampant/field-<name>.csv
fieldRanges()                  per-field count/min/under1/underEps
verifyField(name, tol)         counts vs a fresh exact solve
fieldDiff(name, n)             sample mismatched chunks with source state
fieldDiag(name, chunkId)       one chunk: live, exact, decay, radius, and the LIVE
                               support chain (root/verdict/grounded) for relaxed fields
fieldSupport(name, n)          classify mismatches vs the true distance to a source:
                               sameDist/carriedTooFar/carriedTooNear/noDist/outOfReach,
                               plus distTooNear (carried distance BELOW the true one,
                               which is impossible and therefore a bug), distTooFar,
                               orphans, and the worst n with their chains
fieldNeighborhood(name, id, r) the live field as a grid of id:value/dist/gen
fieldLoad()                    per field: frontier depth, cumulative pops and reports,
                               audit ghosts/starved/orphans (lag vs wrong, per field)
certify(name)                  rebuild exactly, compare, report liveOnly and peaks
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
