# Rampant Integration Tests

Integration tests for the Rampant mod that run within the actual Factorio game engine.

> **IMPORTANT — read `docs/09-Engine-Testing.md` before launching any headless
> test.** It documents the engine 2.1.x behaviors that silently break test runs:
> `auto_pause` (server pauses with no players), `--console-log` not capturing
> Lua prints, stale saves hijacking scenario loading, per-mod `storage` scoping,
> and the scenario authoring contract.

## Current Canonical Flow (2.1.x)

- **`just test`** — entity lifecycle suite (48 tests, self-contained scenario,
  runs in `on_init`; does not require Rampant to be enabled)
- **`just test-active`** — full Rampant initialization + tick loop at 60 UPS
  (requires Rampant enabled in `mods/mod-list.json`)
- **`just test-rcon`** — RCON-driven AI behavior tests (`tools/rcon_client.py`
  is the working RCON client)
- **`just test-behavior <scenario> [seed] [strict]`** — staged behavior
  scenarios on a **pinned map seed**. Seeds make runs reproducible: saves are
  generated per seed (`just make-saves seed=N`, cached base maps), and the
  mod's RNG is fixed-seed (`Constants.ENEMY_SEED`), so same seed + same mod =
  same simulation. `seed=random` loads the scenario dir fresh, unpinned.
- **Logs**: `/tmp/rampant-behavior-<name>.out`, previous run kept as `.prev` —
  failure evidence is never overwritten by the next run.
- **Flaky registry**: `integration-tests/behavior-flaky.list` — listed
  scenarios failing print `KNOWN-FLAKY` + the recorded reason and exit 0
  (batteries don't block on tracked issues); `strict=1` overrides.

All recipes: purge stale saves, launch with `"auto_pause": false` in server
settings, capture **stdout** (not `--console-log`) and poll it for
`TESTS_COMPLETE` / `TESTS_FAILED`, kill factorio on failure.

### Scenario Authoring Contract

Any test scenario must:
- Print `TESTS_COMPLETE` (pass) or `TESTS_FAILED` (fail) to **stdout** within the
  runner's budget
- Poll mod readiness via `remote.call("Rampant", "isInitialized")` from
  `on_tick` with a tick budget — never busy-wait, never read mod `storage`
- Keep `on_load` safe (stale saves fire it instead of `on_init`)

See `active-ai-scenario/control.lua` for the working pattern.

### Shared scenario library

`integration-tests/lib/scenario-lib.lua` carries the staging physics and
diagnostics as code (bundled into every scenario save by `make-saves`, and copied
next to `control.lua` for the scenario-dir load path). Scenarios `require()`
it as `scenario-lib`. Key helpers and the failure signature each encodes:

- `waitForScanStability` — the initial scan's unit purge destroys units in
  scanned chunks; staging must wait for the scan to go idle.
- `pickDryPosition` — water pockets freeze formations (path-end settle).
- `buildBeacon` — player-structure beacons need `destructible=false` (squad
  units chew them down in ~1 game-minute, wiping `playerBaseGenerator`); the
  `factoryProxy` option reproduces a real factory's B-gradient regime so it
  dominates ore-density noise.
- `sustainNest` — the activeness check reads `getPollution(chunk)` = the chunk's
  **top-left corner** (not the spawner point), and spawners absorb pollution:
  a continuous stream at the corner is required (pulses leave the nest inactive
  ~97% of the time and formation starves).
- `churnSnapshot` / `churnLine` — per-phase squad-lifecycle counter deltas.

Two traps worth repeating (both cost an engine run each):
- **Evolution is cached.** The mod reads `Universe.evolutionLevel` refreshed on the
  `planning()` pass (every 3rd tick). After `setEvolution`, wait ~60 ticks
  (arm delay) before staging — same-tick staging queues the stale evolution.
- **The upgrade replacement lands at the original's position.** Match it by
  proximity (within ~16 tiles), not by nearest — otherwise the second leg
  matches the first leg's replacement.

### Behavioral parity coverage

| Behavior | Verified by |
|---|---|
| Pheromone generation/diffusion/gradient | pheromone-dynamics |
| Squad attack / retreat / re-engage | squad-cycle |
| Squad formation + wave-size scaling (`evo^1.4`) | attack-wave |
| Base states (RAIDING/ONSLAUGHT economy/MIGRATING) | base-ai-states |
| Expansion (settler builds a new base) | base-expansion |
| Siege builds toward the player | siege-settle |
| Evolution-gated upgrade tiers (t1 → t10) | upgrade-threshold |
| Death responses (points, retreat) | death-response, player-death |
| Save/load continuity | save-load |
| Multi-surface | multi-surface |
| Session-scale development + UPS | long-run |

Not covered headless (structurally unreachable — both triggers need a connected
player or another mod):
- **Rallying death cry** (`Squad.rallyUnits`) is called only from the
  ion-cannon blast handler (`control.lua`, a Space-Age mod event).
- **Counterattacks** (`Processor.processVengence`) are queued by the rally cry or
  by the player-quadrant scan (`vengence=true`, which iterates
  `game.connected_players`).
- **Player hunting** is covered indirectly: attack squads target player pheromone
  (wall-defense verifies units reaching the player's wall).

A scenario for these needs a connected virtual player and/or the ion-cannon mod
(the long-run harness's virtual player is the closest existing machinery).

---

## Overview

This test suite validates that the Rampant mod functions correctly when loaded into a real Factorio game instance. Unlike the headless unit tests (`tests/runTests.lua`), these integration tests use Factorio's actual APIs to:

- Validate data stage prototype creation
- Test entity spawning and behavior
- Verify event handler registration
- Check AI system initialization
- Test state serialization/deserialization

## Requirements

- **Factorio 2.0** (headless server recommended)
- **Factorio license key** (required for server mode)
- **Lua 5.3+** (for local testing without Factorio)

## Directory Structure

```
integration-tests/
├── README.md                  # This file
├── run-tests.sh               # Shell script to run tests
├── integration-tests.lua      # Main test framework
├── test-data-stage.lua        # Data stage validation tests
├── test-settings.lua          # Settings validation tests
├── test-entities.lua          # Entity prototype tests
├── test-events.lua            # Event handler tests
├── test-ai-behavior.lua       # AI squad/behavior tests
├── test-pheromones.lua        # Pheromone system tests
├── test-save-load.lua         # Persistence tests
└── scenario/
    ├── info.json              # Scenario definition
    └── control.lua            # Scenario control script
```

## Test Categories

### Data Stage Tests (10 tests)
Validates that all prototypes are created correctly during Factorio's data stage:
- Prototype categories exist (unit, turret, projectile, etc.)
- Rampant prototypes are registered
- Prototype structure is valid

### Settings Tests (15 tests)
Validates that all mod settings are accessible and properly configured:
- Global settings are accessible
- Player settings are accessible
- Settings have valid types and ranges
- Required settings exist

### Entity Tests (10 tests)
Tests entity creation and manipulation:
- Surface availability
- Spawner, unit, and turret entity creation
- Entity position and force validation
- Entity searching by name and force
- Entity destruction

### Event Handler Tests (8 tests)
Verifies event handler registration and execution:
- Event definitions are accessible
- Handler registration functions work
- Multiple handlers can be registered
- Event filters are supported

### AI Behavior Tests (12 tests)
Tests the AI system initialization:
- Universe structure exists
- Maps, squads, and bases are initialized
- Configuration values are present
- Iteration over AI structures works

### Pheromone Tests (8 tests)
Validates the pheromone system:
- Player and enemy pheromones exist
- Pheromone data structures are tables
- Chunk properties are initialized
- Enemy and resource maps exist

### Save/Load Tests (5 tests)
Tests state persistence:
- Universe can be serialized
- Serialized state is a valid string
- State can be deserialized
- Deep copy operations work

## Running Tests

### Quick Start

```bash
cd /path/to/Rampant

# Run with default settings
./integration-tests/run-tests.sh
```

### Custom Factorio Path

```bash
FACTORIO_PATH=/path/to/factorio ./integration-tests/run-tests.sh
```

### Manual Execution

```bash
# 1. Package the scenario
cd integration-tests/scenario
zip -r ../test-scenario.zip info.json control.lua
cd ../..

# 2. Run Factorio with the scenario
factorio --start-server-load-scenario "rampant-integration-tests" --test-mode
```

## Expected Output

### Successful Run

```
==========================================
  Rampant Integration Test Runner
==========================================

Configuration:
  Factorio Path: /opt/factorio
  Mod Path:      /home/user/Rampant
  Scenario:      /home/user/Rampant/integration-tests/scenario
  Results File:  /tmp/rampant-integration-results.log

...

=== Rampant Integration Tests ===
Factorio Version: 2.0
Rampant Version: 3.4.0

--- Data Stage Tests ---
  Testing: prototype categories exist
  Testing: Rampant prototypes exist
  ...
Data Stage Tests: PASSED (10 tests)

...

=== Integration Test Summary ===
Passed: 68
Failed: 0
Total Tests: 68
================================

ALL INTEGRATION TESTS PASSED!

==========================================
  Test Execution Complete
==========================================
```

### Failed Run

```
...

INTEGRATION TESTS FAILED

FAILED TESTS:
  1. entity position validation
     Error: Entity position x mismatch

==========================================
  Test Execution Complete
==========================================
```

## Test Framework

The test framework provides the following functions:

### `IntegrationTests.runAll()`
Runs all integration test categories and returns `true` if all tests pass.

### `IntegrationTests:assert(condition, message)`
Asserts that a condition is true. Fails the test with the given message if false.

### `IntegrationTests:setCurrentTest(name)`
Sets the current test name for error reporting.

### Test Categories
Each category is implemented as a function:
- `IntegrationTests:testDataStage()`
- `IntegrationTests:testSettings()`
- `IntegrationTests:testEntities()`
- `IntegrationTests:testEventHandlers()`
- `IntegrationTests:testAIBehavior()`
- `IntegrationTests:testPheromones()`
- `IntegrationTests:testSaveLoad()`

## Troubleshooting

### Factorio Not Found

```
ERROR: Factorio binary not found at /opt/factorio/bin/x64/factorio
```

Make sure Factorio is installed and set `FACTORIO_PATH` environment variable.

### No License

Factorio requires a license key for server mode. Set up your license or use Factorio in non-server mode.

### Scenario Not Found

```
ERROR: Scenario info.json not found
```

Ensure the scenario files are in `integration-tests/scenario/` directory.

### Mod Not Loading

Ensure the Rampant mod files are present in the parent directory (control.lua, info.json, etc.).

## Adding New Tests

### Adding Tests to Existing Category

To add a test to an existing category (e.g., `testEntities`), add a new `self:setCurrentTest()` and `self:assert()` call:

```lua
function IntegrationTests:testEntities()
    print("--- Entity Tests ---")
    self:setCurrentTest("my new test")
    self.assert(someCondition, "My test failed")
    -- ... existing tests
end
```

### Adding a New Test Category

1. Create a new test function in `integration-tests.lua`:

```lua
function IntegrationTests:testMyNewCategory()
    print("--- My New Category Tests ---")
    self:setCurrentTest("test 1")
    self.assert(condition1, "Test 1 failed")
    self:setCurrentTest("test 2")
    self.assert(condition2, "Test 2 failed")
end
```

2. Add the call in `runAll()`:

```lua
function IntegrationTests.runAll()
    -- ... existing tests
    self:testMyNewCategory()
    -- ... report
end
```

## Performance

- **Unit tests** (headless): < 1 second
- **Integration tests** (Factorio engine): ~30-60 seconds

## Limitations

- These tests run in a single-player headless Factorio instance
- Multiplayer scenarios are not tested
- Visual/rendering tests are not included
- Performance under extreme load is not measured

## See Also

- [Unit Tests](../tests/runTests.lua) - Fast headless tests using mocks
- [Simulation Tests](../tests/sim/) - Factorio API simulation layer
- [Factorio Lua API](https://lua-api.factorio.com/) - Official API documentation