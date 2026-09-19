# Pheromone Architecture: ACO Exploration + Local Resource Field

Status: proposal, not implemented. Needs the behaviour suite.

## The problem the save properties exposed

The endgame save shows the pheromone field is not stationary and not evenly
loaded:

| Field | Sources | Density | Character |
|---|---|---|---|
| `RESOURCE_PHEROMONE` | ore | 8,117 / 10,292 chunks (**96%**) | near-uniform |
| `BASE_PHEROMONE` | bases | 423 | sparse |
| `ENEMY_PHEROMONE` | structures | 1,760 | sparse |
| `PLAYER_PHEROMONE` | 1 player | 1 moving | sparse, dynamic |
| `KAMIKAZE_PHEROMONE` | derived | — | dynamic |
| `deathGenerator` | deaths | — | dynamic |

Measured on the reference save:

- `chunkDeathRating` ~0.94, **static** (the dynamic `deathGenerator` term is ~0)
- `chunkCount` ~6.07 (the movable-neighbour divisor, static terrain)
- the dirty phase is ~70% of the mod's script time and is flat per range
- the sweep drives it: 40 chunks/tick, ~199.5k marks per 5,000-tick range

**The resource field is near-uniform because 96% of chunks emit.** A near-uniform
field has almost no gradient, so the resource field buys almost no directional
information per unit of diffusion cost. But it cannot simply be sparsified: the
long-range propagation is what lets squads **migrate to distant ore**, which is a
core behaviour.

## The design

Split discovery from exploitation. ACO does the long-range discovery; the resource
field does the local approach.

| Mechanism | Role | Reach | Cost |
|---|---|---|---|
| **ACO trail** (scouts) | discovery of new ore | long-range, but only along the scouts' paths | sparse: a few units depositing |
| **Resource field** | exploitation of known ore | **local**, only near the ore | local diffusion, no global reach |

This is textbook ACO: scouts explore and lay a trail, the colony follows the trail,
and the resource's own emission guides the final approach. The expensive global
diffusion was standing in for the trail.

## Why this is the right shape

- The long-range discovery moves from the field (which had to propagate everywhere)
  to the trails (inherently sparse — a few units laying pheromone).
- The resource field stops needing to reach across the map; its per-tick propagation
  collapses to a local neighbourhood.
- The trail field already exists in the code (`chunk.trailLastRefresh`,
  `DECAY_TRAIL`) but is **written once and never read** — it is the
  intended-but-unfinished exploration mechanism, not dead code.

## Open questions to resolve before implementing

1. **The trail's intended role** — confirm `trail`/`DECAY_TRAIL` were meant as the
   exploration path (versus a removed feature).
2. **The decay-on-read** — `getEffectivePheromone` has no callers while its
   `*LastRefresh` timestamps are written every tick. Either it should be hooked up
   (the field is stale between sweeps) or the timestamps should be removed. The trail
   decay makes this live again.
3. **Discovery latency** — how long a scout takes to find ore, and whether that is
   acceptable versus the global field's instant reach.
4. **The exploration cost** — a few scouts wandering and depositing; the mod already
   has scouting (`processScanChunks`) and squads.

## Implementation sketch

1. Make the resource field local: the ore's emission propagates only a bounded
   radius, not the whole map.
2. Hook up the trail: scouts deposit `trail` on their path; the colony reads the
   trail gradient to follow.
3. Keep the base/nest/player fields as they are (sparse, real gradients).
4. Re-run the source-driven sweep experiment that was falsified: it lost because the
   sources were dense (96%). With a local resource field it should win.

## Measurement plan

- **Read census per field** — count how often each field is read per range. A field
  written every tick and never read is free to remove; this also answers whether
  `getEffectivePheromone` and the trail are live.
- **Field staleness between sweeps** — the field decays only when a chunk is
  processed (every ~268 ticks via the sweep). Measure the staleness over that window.
- **Discovery latency** — with scouts as the discovery path, how long to first contact
  with distant ore, versus the global field.
- All per-range, per-subsystem; never a lump-sum average (see AGENTS.md).

## Cost sketch

| | Global resource field (today) | ACO trail + local field |
|---|---|---|
| Resource propagation | whole map, every tick | local only |
| Long-range discovery | the field's reach | the scouts' trails |
| Emission | ore entities | ore entities + scouts |
| Dirty pass | ~40 chunks/tick, dense | local + a few trail chunks |

## The honest caveat

This is an architecture change and a behaviour change. It is the first proposal
where the cost reduction follows from the mechanism change rather than fighting the
existing structure, and it turns the "dead" trail code into the intended design. It
needs the behaviour suite, and the read census first to confirm which fields the AI
actually consumes.
