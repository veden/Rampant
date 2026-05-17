# AGENTS.md

This file provides guidance for AI coding agents working in the Rampant Factorio mod repository.

## Project Overview

Rampant is a Factorio mod that implements advanced enemy AI using pheromone-based potential fields for coordinated attacks, retreats, and player tracking. Mod version: 3.4.0, requires Factorio >= 2.0, License: GPLv3.

## How the AI Works

1. **Pheromone System**: Enemies use potential fields (not standard pathfinding) for single-step pathfinding
2. **Squads**: Unit groups that probe defenses, retreat when outnumbered, call reinforcements, and hunt players
3. **AI States**: Raiding (pollution-based attacks), Migration (resource-seeking), Sieging (builds toward player), Onslaught (double credits)
4. **Tactical Behaviors**: Swarming, breaching, player hunting, rallying death cry, counterattacks

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

1. `just test` creates private server settings
2. Copies scenario to Factorio scenarios directory
3. Starts Factorio headless with scenario
4. Scenario runs entity lifecycle tests against vanilla Factorio APIs
5. Tests complete and print results to console log
6. Test runner detects completion and sends RCON quit command
7. Factorio gracefully shuts down

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

Use `pcall()` for protected calls in tests:
```lua
local success, err = pcall(test)
if not success then
    self.failed_tests[name] = err
end
```

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
- `global` (mod state storage)
- Module globals: `ProcessorG`, `ConstantsG`, `SquadG`, etc.

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

See `/mnt/backpack/code/games/factorio/luaDefinitions/` for comprehensive API documentation.
See `/mnt/gallery/gameFiles/factorio/data/` for game implementation details.

## Working Directories

### `/mnt/backpack/code/games/factorio/luaDefinitions`
LSP directory containing comprehensive Factorio APIs, data models, and game element definitions. Use as the primary reference for Lua API documentation and type information.

### `/mnt/gallery/gameFiles/factorio/data/`
Contains the actual Lua representation of Factorio's game code. Use to understand actual game implementation details.

### `/mnt/gallery/gameFiles/factorio/`
Factorio installation directory. Contains:
- `bin/x64/factorio` - Factorio binary
- `mods/` - Mod directory for testing
- `scenarios/` - Scenario directory
- `data/` - Game data and settings examples