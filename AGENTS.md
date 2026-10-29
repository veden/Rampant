# AGENTS.md

This file provides guidance for AI coding agents working in the Rampant Factorio mod repository.

## Project Overview

Rampant is a Factorio mod that implements advanced enemy AI using pheromone-based potential fields for coordinated attacks, retreats, and player tracking. Mod version: 3.4.0, requires Factorio >= 2.0, License: GPLv3.

## How the AI Works

1. **Pheromone System**: Enemies use potential fields (not standard pathfinding) for single-step pathfinding
2. **Source-Versioned Fields**: every field (base, enemy, resource, player, kamikaze, and the signed death gain/loss pair) is a max-plus potential field. The PRODUCTION solver is relaxation (`libs/source_versioned_field_runtime.lua`): the field is kept as the fixed point of `value(v) = max(ownSource(v), max(neighbour) * decay)`, recomputed only where something changed, drained from a per-field frontier on a shared per-tick budget. The ordered solver (`libs/source_versioned_field.lua`) is the ORACLE - it runs only in diagnostics (`verifyField`, `rebuildField`, `fieldDiag`) and is what makes every claim about the field measurable. Fields live in `storage.universe.sourceVersionedFields`. See "Fields Are Event-Driven, With a Self-Healing Sweep".
3. **Squads**: Unit groups that probe defenses, retreat when outnumbered, call reinforcements, and hunt players
4. **AI States**: Raiding (pollution-based attacks), Migration (resource-seeking), Sieging (builds toward player), Onslaught (double credits)
5. **Tactical Behaviors**: Swarming, breaching, player hunting, rallying death cry, counterattacks

## Build Commands

### Copy Mod to Factorio
```fish
./action.fish copy --dir=/mnt/gallery/gameFiles/factorio/mods
```

### Create Distribution Zip
```fish
./action.fish zip
```

### Alternative with Just
```bash
just copy           # Copy to Factorio mods folder
just zip            # Create mod zip
just play           # Launch Factorio
```

## Change Classes — say which restart is needed

When telling a human to pick up a change, always state which restart it needs,
and prefer the cheaper one:

- **Data stage** — `data.lua`, `data-updates.lua`, `data-final-fixes.lua`,
  `prototypes/**`, `settings.lua`, `migrations/**`, `info.json`.
  The game must be **restarted completely** (quit the process and relaunch).
  Prototypes are built once per launch; loading a map cannot pick them up.
- **Control / runtime** — `control.lua`, `libs/**`, `tests/**` (anything that
  is not the above). **Restarting the level** (new game, or load a save) is
  enough: the control stage is re-initialised per session and re-reads the
  files. This is much faster, so do not ask for a full reboot for these.

When a single change set mixes both, say so explicitly: the data-stage part
needs one full restart, after which control-stage changes only need level
restarts.

## Event Telemetry (what happened, not how slow)

Per-tick timings answer "how expensive"; the event stream answers "what did it
do". It is off by default and writes one line per event:

```
t=331|cmd.CREATE_BASE|chunk=147|name=neutral-biter-spawner-v1-t1-rampant|id=6
t=331|base.points|base=0|delta=250|tag=built|total=250|state=1
```

- **Enable:** startup setting `rampant--eventTelemetry` (full game restart).
  Optional `rampant--eventTelemetryUdpPort` mirrors each batch to
  `helpers.send_udp` for a live listener instead of reading a file.
- **Where:** `script-output/rampant/events.jsonl` under the game's write-data,
  appended in batches every 300 ticks. For RAM instead of disk, point the
  run's write-data at `/dev/shm` (a `-c` config with
  `write-data=/dev/shm/factorio`) or symlink `script-output` there.
- **Read:** `python3 tools/events.py FILE` summarises by kind and per 5-minute
  bucket; `--kind cmd.DISPATCH_SQUAD`, `--tick FROM TO`, `--grep base=4` narrow
  it down.
- **Covered today:** every executed command (type plus base/chunk/entity/
  position/amount/reason), base point grants and spends, upgrade creation and
  blocked creations, player build/mine with positions, resource registration,
  field solve and reconcile.

When a change breaks behaviour, this stream is the thing to read: it shows the
order of decisions and the values they were made on, which a tick average
cannot.

## Testing Against a Real Save — NEVER OPEN THE ORIGINAL

A headless run that is stopped with SIGTERM **saves the map in place**
("Saving map as <path>") and overwrites the file. SIGKILL does not. Always:

- copy the save first and load only the copy (`tools/headless.sh` does this);
- terminate with `kill -9`, never `pkill`/`kill` (SIGTERM);
- check the original's md5 and mtime before and after a run.

`tools/headless.sh <save.zip> [seconds] [rcon-port]` enforces all three and
prints the workdir it used.

## Chunk Records — key-addressed, id-referenced

- A chunk record holds `key` (position encoded as one number), `id`, `map` and
  its per-chunk state. `x`/`y` are **derived** from the key
  (`MapUtils.chunkX`/`chunkY`, or `Utils.chunkX` in leaf modules); derive once
  per function into locals. Never add `x`/`y` back as fields.
- `key = cx * 2^27 + cy`, so the inverse **rounds**, not floors (cy can be
  negative).
- **Validity is presence in `chunkById`**. There is no `valid` flag. Anything
  that holds a chunk across calls stores the **id** and resolves through
  `MapUtils.getChunk(id)`; when it returns nil the chunk is gone, so drop the
  entry. `MapUtils.isCurrent(chunk)` is the identity check for a chunk object
  obtained elsewhere (guards id recycling).
- Registries (`chunkToNests`, `chunkToPassScan`, `deathGeneratorChunks`, …) are
  keyed by id and store `true`; they are membership sets, not chunk stores.
- Ids are recycled from an aged free list and compacted before the provenance
  packing bound, so the id space tracks live chunks, not lifetime creations.
- **Dependencies are injected from `control.lua`.** `ChunkUtils`,
  `ChunkPropertyUtils` and `MapUtils` each hold the field runtime in a module local with a
  `setSourceVersionedFieldRuntime` setter, and every use of it is nil-guarded. A missing
  setter is therefore SILENT: `removeChunkFromMap` queued no source removal for a year of
  field work without anyone noticing. When you add a module that has to talk to the field
  runtime, add the setter AND the call AND a test that observes the effect.

## Fields Are Event-Driven, With a Self-Healing Sweep

Every write queues a source change, and the drain propagates from those sources on a
shared budget (`GLOBAL_FIELD_WORK_PER_TICK` = 96 pops split in 32-pop slices, rotating
priority). Two things keep the field honest on top of that:

- **The audit sweep** (`relaxAudit`): a resumable cursor walk, 128 nodes per tick, one
  field per tick, alternating between two sets - chunk records (a value that is missing,
  above its justification, or below it) and field ids (entries with no chunk record). Any
  node that disagrees with its own recompute is RE-POPPED, which clamps it and dirties its
  neighbours, so the repair spreads exactly as far as it needs to. It only runs on a
  QUIESCENT field: mid-wave every node's stored value legitimately lags its neighbours.
- **Truncation on removal**: a vanished source leaves a fossil disc - a ring of mutually
  supporting values each one decay factor below its neighbour - and re-popping its peak
  lowers the disc by one decay factor per round, so the truth takes ~10 rounds. A removal
  therefore CLEARS the disc (`clearDisc` in `dropChunk`, bounded BFS to `maxRadius`) and
  re-queues it; the wave refills from the sources that are still there in one pass.

Measured on the `Starting2` save (10,000 tracked chunks, md5 `697ee0a8...`), comparing the
live field against a fresh exact solve every 15 s through continuous chunk removal:

```
play, 120 s                  resource 0 mismatched chunks (3 blips), distTooNear=0
rebuild, then 90 s           resource 0 mismatches; liveOnly 185 of 5,019 compared
base field, always          95 chunks missing at the radius fringe, maxDelta
                             267 = 1.7% of peak (a tie at the cap, see below)
```

Before the removal path was wired up the same measurement read 73 -> 606 mismatched
chunks with the live field up to 2x the exact solve, healing ~10% per round and exact
again only after a reseed. Two separate bugs produced that, and both were silent:

- `MapUtils.removeChunkFromMap` reached the runtime through a module local that nothing
  injected (every use nil-guarded), so a chunk removal queued no source change at all.
  `MapUtils.setSourceVersionedFieldRuntime` now exists and `control.lua` calls it;
  `tests/test_source_versioned_field_runtime_relax.lua` exercises the path end to end.
- even when queued, a removal only re-popped the removed node, so the disc its value was
  holding up stayed and decayed one factor per round (`clearDisc` now truncates it).

Rules that a measurement has already broken once:

- **A field entry belongs to a chunk.** No chunk record, no value. A pop queued before a
  removal used to recompute from the neighbours and leave a value at an id nothing would
  ever visit again; `Runtime.dropChunk` runs at removal time, while the record still
  resolves, because `neighborOf` goes through the record.
- **A carried distance is the length of the path that delivered the value**, so it can
  never be shorter than the hop distance to the nearest source. Check it with
  `fieldSupport`, which reports `distTooNear` (a violation) and `distTooFar` (normal).
- **A ring of values supporting each other is not a fixed point** unless the values
  satisfy every node's own recompute; see the fossil disc above.
- The base field's 95-chunk fringe difference is not a drift: relaxation and the ordered
  solver break an exact tie between two equally good paths at the radius cap differently.
  It is 1.7% of peak and it survives a reseed, so it is a model difference, not a bug.

Diagnostics (RCON/UDP remotes, see `control.lua`):

```
certify <field>            rebuild exactly, compare, and say liveOnly/peaks
verifyField <field> [tol]  drain, compare against the oracle, count mismatches
fieldSupport <field> [n]   classify mismatches: sameDist / carriedTooFar /
                           carriedTooNear / noDist / outOfReach, plus distTooNear,
                           distTooFar, orphans, and the worst n with their chains
fieldDiag <field> <id>     one chunk: live, expected, own source, and the LIVE support
                           chain (root, verdict, grounded) - relaxed fields carry no
                           provenance, so the chain is the only support evidence
fieldNeighborhood <f> <id> [r]  the live field as a grid around a chunk: id:value/dist/gen
fieldLoad                  per field: frontier depth, cumulative pops and source
                           reports, audit ghosts/starved/orphans - tells LAG (deep
                           frontier still being fed) apart from a wrong field
fieldMax / fieldDiff / fieldRanges / fieldDigest / fieldExport / rebuildField
reseedAll                  clear every relaxed field and rebuild from sources
```

## Chunk Pipeline — the two phases, and neither clocks itself

`pendingChunks` and the surface release are the same kind of work (per-chunk queries over a
surface) and both used to be unbounded in one tick. Both are now sliced, and both slices are
COUNTS, because **the control stage has no readable clock**:

- `helpers.create_profiler()` is opaque — `tostring()` gives `"[LuaProfiler]"`, and the docs
  say the raw values "don't allow reading ... from Lua" (`doc-html/classes/LuaProfiler.html`).
  You can log it, not read it.
- `os` is not exposed at all (`os.clock` is nil), `io` likewise.
- `game.tick` only resolves whole ticks, which is far too coarse for a sub-millisecond slice.

So size a slice from the measured cost distribution instead:

```
per-chunk chunk scan, Starting2 load (30,334 chunks)
  p50 0.031ms   p90 0.100ms   p99 0.331ms   max 4.04ms   over 1ms: 12 chunks
load flush at 64 chunks/tick (474 ticks, 7.9 s)
  p50 3.02ms    p90 6.03ms    p99 9.49ms    max 17.91ms
load flush at 16 chunks/tick (769 ticks, 12.8 s)
  p50 0.62ms    p90 1.28ms    p99 2.15ms    max 2.77ms   over 4ms: none
surface release, whole surface in one call
  fulgora 1,665 chunks = 155ms in ONE tick, vulcanus 1,910 = 75ms   (~93us/chunk)
surface release at 12 chunks/call (~1.1ms), drained every tick
```

Rules that bite:

- **`SCAN_PER_TICK` = 1 in steady state** (a moving player generates a few chunks/tick;
  one dense chunk is already 4ms). `FLUSH_SCAN_PER_TICK` = 16 only for the load flush, so
  the map is consistent in ~13 s instead of being half-blind for an hour.
- **`MapUtils.releaseSurface` is resumable.** It builds the chunk list once, removes
  `RELEASE_CHUNKS_PER_CALL` per call, and finishes the teardown (grid, bases, hives, the map
  entry) when the list empties. `MapUtils.processSurfaceReleases` drains it from the tick
  loop; keep that call, or a release simply stalls half-done.
- **Do not judge a surface idle while its chunk scans are queued** (`hasPendingScans`). The
  idle gate is the nest/hive/turret registries, and those are only filled BY the scan: at
  load the main surface was released with `chunks=0` while 5,042 of its scans were still
  queued, then re-acquired and re-scanned.
- **A release drops that surface's queued scans** (`dropPendingScans`). Otherwise the flush
  keeps scanning chunks of a map that is no longer tracked, and every one of them comes
  back as an orphan record to be reclaimed one call at a time.

Read the pipeline from the log, not from intuition: `[SCAN]` gives one line per chunk with
its coordinates and cost, `[REL]` gives a surface release with its chunk and dropped-scan
totals. `tools/measure-chunk-pipeline.py` drives a copy of the save, parses both, and reports
per-tick ranges for the flush, the release and the buckets after it.

Field correctness is measured the same way: `tools/measure-field-drift.py` compares the live
field against a fresh exact solve every 15 s through play and through a rebuild.
`tools/save_drive.py` is the shared piece - it copies the save, starts the server, talks to
the mod over UDP exec (`d.call("fieldLoad")`, `d.lua("return game.tick")`), and on close
SIGKILLs it and verifies the original save's md5 and mtime.

**Separate the load aftermath from steady state.** On the Starting2 save the tick tail is
p99 10-12ms for the first few game-minutes after load and settles to p99 1.3ms / max 1.4ms
ten minutes in, with the map still shedding chunk records in between:

```
whole-tick samples, non-diagnose, after the load-path fixes below
  first 60 s of load    n=121  p50 1.83  p90 3.54  p99 5.51  max 5.58   over5ms 3, over10ms 0
  during the flush      n= 63  p50 2.63  p90 4.26  p99 5.58  max 5.58
  after the flush       n= 99  p50 0.79  p90 2.41  p99 4.70  max 4.70   over5ms 0
  scan work per flush tick n=1692 p50 0.80 p90 1.92 p99 3.89 max 6.04
```

Before those fixes the same window read p99 10-12ms / max 25.46ms with 35 ticks over 10ms,
and four adjacent windows alternating the GC setting showed the tail was the map still
settling rather than the collector:

```
4 adjacent 100 s windows, same run, GC setting alternating stock/spread
  stock    p99 11.72  max 13.98  over5ms 4
  spread   p99  9.63  max 11.26  over5ms 3
  stock    p99  1.32  max  1.38  over5ms 0
  spread   p99  0.72  max  0.77  over5ms 0
```

Two lessons: a single A/B window pair cannot tell an improvement from the map quietening
down (a first pass read the GC spread as 6x, and it is 15-45% at best), and "steady state"
has to be defined as *after* the load work drains (`chunkQueueStatus` shows
`pending=0 releases=0`) or the number measures the load.

`tuneGarbageCollector` (`control.lua`, setpause 100 / setstepmul 100) is kept because it
helped in both directions in the adjacent pairs above, but it is a small effect: the mod's
~40MB of Lua storage makes the collector visible, and nothing per-subsystem can see it — the
subsystem timers account for ~1.3ms of a 19ms tick.

**Load-path work is bounded too** (same rule, same reason — each of these was one tick):

```
queueChunks at load    55.6ms in the init tick (5 surfaces, 26,622 chunks)  -> 512 chunks/call
                        from the tick loop, ~0.1ms/tick until each backlog is queued
disc clear on removal  11-24.5ms inside chunkReconcile                     -> resumable, 384
                        nodes/tick (a disc is up to 3,200 nodes PER FIELD, and a removal
                        clears one per field)
dropPendingScans       18-20ms inside surfaceSweep                         -> mark the map
                        and let the pops drop its events (O(1)); hasPendingScans went from
                        a queue walk (5.7ms) to a per-map counter
per-removal upgrade scan 5-6ms/tick during a release                       -> one sweep per
                        12-chunk slice, not per chunk
rescan loop            10,616 [SCAN] lines for 1,431 unique chunks = 7.4x the work
                       -> the loop picked the next event before removing the previous one,
                       so pickPendingChunk kept handing back an event that was still queued
                       and the same chunk was scanned again (exactly 2x on one load, 7.4x on
                       another). Pop before scanning: 26,622 scans -> 15,168, flush 31.6s ->
                       15.8s, and `scan.repeat` should now be zero.
dense chunk scan       NOT the query: see below. The worst single chunk scan is 5.1ms, and one
                       chunk's fixed cost is ~0.04ms median.
```

**A chunk's cost is fixed per-chunk work, not the entities it returns.** Measured with
`benchQueries` / `benchTopChunks` (remotes in `control.lua`) on the densest chunks the save
has (10,370 enemy structures over 1,385 chunks; the densest holds 33-38):

```
chunk      entities   find    count   findLimit1   spawners  turrets  resources
-15,-32       38     0.009    0.003     0.002        0.005     0.005    0.003
-31,23        39     0.009    0.004     0.002        0.006     0.006    0.003
29,-11        33     0.008    0.003     0.002        0.005     0.004    0.003
-26,18         7     0.007    0.004     0.003        0.004     0.004    0.003
0,0            0     0.008    0.006     0.006        0.006     0.005    0.005
```

All in MICROseconds, and an empty chunk costs about as much as a dense one: the per-query
overhead dominates and the entity count adds ~0.02us per structure. So:

- `count_entities_filtered` is ~2.5x cheaper than `find` (0.003 vs 0.009ms) but the saving is
  ~6us per chunk, ~0.1s over a 15,000-chunk load — not worth changing a filter that the scan
  needs entities from anyway (it creates the base and registers each structure).
- `limit = 1` is the cheapest variant (the engine stops early), ~0.002ms.
- Splitting a chunk query into four quadrants costs MORE than one query (0.014 vs 0.009ms),
  because per-call overhead dominates: do not split areas to smooth a tick.
- What a chunk actually costs is its fixed work — paths scan, four queries, the record, the
  registrations — about 0.04ms median, p99 0.5ms.

**Measurement caveat:** a `rampantTests.diagnose(n)` window logs every subsystem every tick
(~20 profilers plus log lines per tick) and roughly doubles the tick it measures — p50
0.86ms with a diagnose window versus 0.39-0.60ms sampled without one. Use range sampling
(no diagnose) for gameplay numbers and a diagnose window only to attribute a phase. The
first-minute figures above are non-diagnose; the attribution that found the four phases
above needed one.

## Iteration Order — Factorio's `pairs()` is Deterministic

`doc-html/auxiliary/libraries.html` ("pairs() / next()"): Factorio reimplements
`next()` so iteration is deterministic in **insertion order** (keys inserted first
are iterated first), with the first 1024 numbered keys always iterated 1..1024.
Consequences for this codebase:

- Resuming a walk with `next(t, cursor)` is sound: newly inserted keys sort after the
  cursor and are visited in the same pass.
- Every client iterates identically, so anything that queues work while iterating
  (upgrade waves, registries) is deterministic in multiplayer.
- Do not assume numeric key order: only ids 1..1024 are ordered numerically, larger
  ids follow insertion order.

## Linting

```bash
luacheck .         # Uses .luacheckrc (declared globals; .bench/ scratch excluded)
```

## Testing

### Unit Tests (Headless)
```bash
lua tests/runTests.lua          # Run all unit tests
```

### Integration Tests (Factorio Game Engine)
```bash
just test                        # Run integration tests via Factorio headless server
```

**Note**: Integration tests require Factorio 2.0 installed.

### Benchmark Reporting — RANGES ONLY (mandatory)

**Never report a single lump-sum or average tick figure for a run.** A run's
`avg`/`min`/`max` hides where the time actually goes, and a non-stationary cost
(the dirty-queue ramp was 0.42 -> 4.47 ms within one run) makes the average
meaningless. Always report **per-tick-range** numbers, broken down by subsystem.

- Per-range, per-phase: the mod logs `[RANGE] ... phase=...` every
  `RANGE_TICKS` (see `control.lua`), gated by `rampant--telemetryEnabled`.
- Per-range, per-subsystem: `[SUB] tick=N name=<subsystem> Elapsed: Nms`, 24
  subsystems timed independently (`sourceField.*`, `pendingUpgrades`, `players`, ...).
- Parse with `tools/range-telemetry.py` (bucket by tick range).
- `--benchmark-verbose` also emits per-tick CSV but SIGSEGVs at teardown for long
  runs; prefer the in-mod telemetry.
- A/B comparisons: run the same tick count N times on each side, on the same machine
  state, and report the **ranges** (they must not overlap for the result to be real).
  Check `uptime` load average first — a loaded machine shifts the floor (observed
  1.27 ms quiet vs 1.74 ms at load 5.8).

### Integration Test Architecture

The integration testing framework consists of:

1. **Test Mode Detection** (`control.lua`)
   - Guards in `onInit()` and `on_configuration_changed()`
   - Detects scenario context via `pcall()` on `game.forces.enemy.evolution_factor`
   - When forces not ready, skips full Rampant initialization

2. **Test Scenario** (`integration-tests/scenario/`)
   - `control.lua` - Self-contained entity lifecycle tests
   - No dependency on Rampant mod initialization
   - Tests spawn vanilla Factorio entities and validates behavior

3. **Test Runner** (`integration-tests/run-tests.sh`)
   - Creates private server settings (public=false, lan=false)
   - Launches Factorio headless with test scenario
   - Polls log for completion signal

4. **Just Integration** (`justfile`)
   - `just test` - Runs full integration test suite
   - Creates private server, monitors completion, sends RCON quit

### Current Integration Tests (Phase 1)

**Entity Lifecycle Tests** (`integration-tests/scenario/control.lua`):
- **Setup**: Surface, enemy force, player force availability
- **Spawning**: Biter, spitter, spawner, worm, character, turrets, walls (8 tests)
- **Damage Flow**: Health reduction, fatal damage, spawner damage (4 tests)
- **Death Handler**: Unit/spawner death invalidation (2 tests)
- **Unit Groups**: Multiple units, unit numbers (2 tests)
- **Buildings**: Inserter, belt, accumulator, solar panel (4 tests)
- **Prototypes**: Biter/spitter/worm spawner validation (6 tests)

**Total: 29 tests, all passing**

### Known Issues with Integration Tests

**Settings Visibility in Scenarios:**
- `settings.startup` is not accessible in scenario context
- `settings.global` exists but may be empty in fresh scenarios
- Workaround: Test mode detection uses `pcall()` on forces instead of settings

**Forces Not Ready in Fresh Scenarios:**
- `game.forces.enemy` doesn't exist until the game creates it
- `onConfigChanged` fires automatically and may fail
- Solution: Added guards in `onInit()`, `on_configuration_changed()`, and `Upgrade.attempt()`

**Data Stage Testing:**
- `data.raw` is not accessible at runtime in scenarios
- Solution: Data stage tests remain in headless unit tests (`tests/runTests.lua`)

### Test Execution Flow

1. `just test` creates private server settings (must include `"auto_pause": false` — see below)
2. Purges stale saves (a save with the scenario name hijacks loading, firing `on_load` instead of `on_init`)
3. Copies scenario to Factorio scenarios directory
4. Starts Factorio headless with scenario, capturing **stdout** (NOT `--console-log` — on 2.1.x it captures only the header; Lua prints go to stdout)
5. Scenario runs entity lifecycle tests against vanilla Factorio APIs
6. Runner polls captured stdout for `TESTS_COMPLETE`/`TESTS_FAILED` and sends RCON quit
7. Factorio shuts down (force-killed on failure — never exit without killing, the backgrounded server holds the shell pipe open)

### Engine Testing on Factorio 2.1.x — READ THIS FIRST

The hard-won engine knowledge that cost hours to discover:

- **`auto_pause` is the #1 test-killer**: default `true` — with no players
  connected the server advances ~1 tick per 3.6s (0.276 UPS). `pause_when_no_players`
  is NOT a valid key (silently ignored). Server settings must use `"auto_pause": false`.
- `storage` is per-mod: scenarios can't read the mod's storage; poll readiness
  via `remote.call("Rampant", "isInitialized")` from `on_tick` (never busy-wait,
  never read `storage._rampantReady` from a scenario).
- Scenario `on_init` runs BEFORE mod `on_init` on 2.1.x.
- `LuaEntityPrototype` is strict: removed fields (`build_base_evolution_requirement`)
  **error on access** instead of returning nil; an error in `on_tick` is a
  non-recoverable server shutdown.
- RCON is a binary protocol: use `tools/rcon_client.py`, not `nc` text.
- `factorio_version` in `info.json` must match the engine major version (2.1).
- Rampant must be enabled in `mods/mod-list.json` for `just test-active`/`test-rcon`.

### Future Enhancements

1. **Phase 2: Squad AI Tests**: Test unit group formation, dispatch, retreat, attack logic

2. **Phase 3: Base AI State Tests**: Test AI state transitions (RAIDING, SIEGE, MIGRATION, ONSLAUGHT)

3. **Phase 4: Pheromone Tests**: Validate potential field calculations and diffusion

4. **Phase 5: Event Handler Tests**: Test handler registration and event data validation

5. **RCON-based Testing**: Use RCON to send commands and validate responses for more comprehensive testing

6. **Pre-generated Map**: Use a save file with Rampant already initialized for full runtime testing

## Code Style Guidelines

### File Structure

Each Lua file should begin with the GPLv3 license header:
```lua
-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>
```

### Imports

Use `require()` for local modules:
```lua
local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
local Processor = require("libs/Processor")
local Constants = require("libs/Constants")
```

### Module Pattern

Libraries use a module pattern with conditional return for globals:
```lua
if UtilsG then
    return UtilsG
end
local Utils = {}

-- Functions
function Utils.isMember(str, set)
    -- implementation
end

return Utils
```

### Naming Conventions

- **Files**: snake_case.lua (e.g., `ChunkPropertyUtils.lua`)
- **Variables**: snake_case (e.g., `enemy_pheromone`, `chunk_size`)
- **Constants**: SCREAMING_SNAKE_CASE (e.g., `CHUNK_SIZE`, `TICKS_A_MINUTE`)
- **Functions**: snake_case (e.g., `process_clouds()`, `find_base_alignment()`)
- **Tables**: PascalCase for class-like tables (e.g., `TestCollection`, `WorldBuilder`)
- **Private module locals**: prefix with `_` (e.g., `local _chunkSize`)

### Local Caching

Cache frequently-used globals and libraries locally:
```lua
local mFloor = math.floor
local sSub = string.sub
local sGMatch = string.gmatch
```

### Function Declarations

Use colon syntax for methods:
```lua
function Utils.split(str)
    -- implementation
end

function TestCollection:new()
    local instance = { tests = {} }
    setmetatable(instance, TestCollection)
    return instance
end
```

### Error Handling

**No pcall in mod logic. Ever.** pcall silently swallows errors — including
mod crashes inside `entity.die()` handlers — and turned a real engine
crash into hours of false "regression" debugging (the death-response
retreat crash: `set_multi_command` receiving a string instead of a
LuaEntity). Errors must fail loudly:

- Guard explicitly instead of catching: nil-checks (`game.forces.enemy`),
  `if chunk.map` guards, constant fallbacks for removed prototype fields.
- Tests: assertion failures raise and abort the suite immediately
  (`tests/framework.lua` run_all calls tests directly).
- If an error is genuinely possible, structure the code so it surfaces.

**The one exception: the RCON/debug interface** (`Observability.lua` exec:).
Its contract is returning the executed code's error to the caller over UDP —
reporting, not swallowing. Marked with a DELIBERATE pcall comment.

For Factorio API calls, handle nil gracefully since tables may be empty.

### Types and Type Checking

No static typing - use descriptive names and runtime checks:
```lua
-- Check type before use
if type(mapData) == "table" and mapData.playerPheromones then
    -- safe to use
end
```

### Constants

Constants are defined in `libs/Constants.lua` and imported:
```lua
local CHUNK_SIZE = Constants.CHUNK_SIZE
local FACTION_SET = Constants.FACTION_SET
```

### Event Handlers

Register handlers using Factorio's `script.on_event()`:
```lua
local function onBuilt(event)
    -- handle built entity
end

script.on_event(defines.events.on_built_entity, onBuilt)
```

### Global Access

Factorio globals are defined in `.luacheckrc`:
- `game`, `data`, `script`, `remote`, `settings`, `defines`
- Module globals: `ProcessorG`, `ConstantsG`, `SquadG`, etc.

**Globals rule: allowed only if CONSTANT.** A global must be set exactly
once at module load and never mutated afterwards — the module-cache pattern
(`if XG then return XG end ... XG = X; return X`) is the sanctioned form.
All mutable mod state lives in `storage` (`storage.universe`). The legacy
`global` table is the 1.x name — nil in 2.x (accessing it errors); never
use it.

### Formatting

- **Indentation**: 4 spaces per level
- **Line length**: No hard limit (disabled in .luacheckrc)
- **Blank lines**: Single blank line between logical sections
- **Comments**: Minimal - code should be self-documenting

### Test Framework

Use the custom `TestCollection` class:
```lua
local test = TestCollection:new()
test:add("test_name", function()
    -- assertions
end)
test:set_setup(function() end)
test:set_teardown(function() end)
test:run_all()
```

### Directory Structure

- `libs/` - Core AI libraries (Processor, Squad, BaseUtils, etc.)
- `prototypes/` - Entity/faction definitions and utilities
- `tests/` - Unit tests with simulation layer
- `integration-tests/` - Game engine integration tests
- `locale/` - Localization files
- `sounds/` - Audio files
- `graphics/` - Sprite/icon files

## Factorio API Reference

**Version matters — the engine is 2.1.x.** The authoritative runtime API docs
ship WITH the install: `/mnt/gallery/gameFiles/factorio/doc-html/` (Version 2.1.14;
`classes/<ClassName>.html` per class). Use it for any version-sensitive API
question.

`/mnt/backpack/code/games/factorio/luaDefinitions/` is generated from
**2.0.73** and is STALE relative to the engine — it is fine for general shapes
and types, but APIs MOVED between 2.0 and 2.1 (e.g. `game.create_profiler` moved
to `helpers.create_profiler`; the defs claim the profiler class exists but not the
`game`-level creation method). When a static conclusion from the defs conflicts
with a runtime error, **the runtime error is authoritative** — and re-check the defs
against the 2.1 doc-html before concluding an API was removed.

See `/mnt/gallery/gameFiles/factorio/data/` for game implementation details.

## Working Directories

### `/mnt/backpack/code/games/factorio/luaDefinitions`
**Generated from Factorio 2.0.73 — STALE relative to the 2.1.x engine.** Use the install's `doc-html` (2.1.14) for version-sensitive API questions; the runtime error wins over these defs when they conflict.

### `/mnt/gallery/gameFiles/factorio/data/`
Contains the actual Lua representation of Factorio's game code. Use to understand actual game implementation details.

### `/mnt/gallery/gameFiles/factorio/`
Factorio installation directory. Contains:
- `bin/x64/factorio` - Factorio binary
- `mods/` - Mod directory for testing
- `scenarios/` - Scenario directory
- `data/` - Game data and settings examples