# Rampant Integration Tests

Integration tests for the Rampant mod that run within the actual Factorio game engine.

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