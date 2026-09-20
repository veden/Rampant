# AGENTS.md

This file provides guidance for AI coding agents working in the Rampant Factorio mod repository.

## Project Overview

Rampant is a Factorio mod that implements advanced enemy AI using pheromone-based potential fields for coordinated attacks, retreats, and player tracking. Mod version: 3.4.0, requires Factorio >= 2.0, License: GPLv3.

## How the AI Works

1. **Pheromone System**: Enemies use potential fields (not standard pathfinding) for single-step pathfinding
2. **H7 Effective Field**: Neighbor reads use floor-aware effective/decayed values. There is no periodic global sweep; the field is maintained by event and propagation marks.
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

## Linting

```bash
luacheck .         # Uses .luacheckrc (24 globals defined)
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
- Per-range, per-subsystem: `[SUB] tick=N name=<subsystem> Elapsed: Nms`, 17
  subsystems timed independently (`dirty`, `pendingUpgrades`, `players`, ...).
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

**Read `docs/09-Engine-Testing.md` before launching any headless test.** It
contains the hard-won engine knowledge that cost hours to discover:

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