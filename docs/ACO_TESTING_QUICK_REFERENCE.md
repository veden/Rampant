# ACO Testing Quick Reference

## Running Tests

### Unit Tests (Headless)
```bash
# Run all tests including ACO
lua tests/runTests.lua

# Run only ACO tests
lua -e "require('tests.test_aco_core'):run_all()"
lua -e "require('tests.test_aco_trails'):run_all()"
```

### Integration Tests (Factorio Headless)
```bash
# Using just
just test

# Direct Factorio command
factorio --headless --load-scenario integration-tests/scenario
```

### Manual Testing in Factorio
```lua
-- Test decay tables
/c log(#Constants.DECAY_090)  -- Should be 10001

-- Test trail spreading
/c local chunk = game.surfaces[1].get_chunk(0,0); 
   chunk.positiveTrail = 100; 
   chunk.trailLastRefresh = game.tick;
   log("Trail set: " .. chunk.positiveTrail)

-- Test dirty chunk marking
/c log(#game.forces[1].surface.get_chunks())
```

## Test Categories

| Test File | Purpose | Count |
|-----------|---------|-------|
| `test_aco_core.lua` | Decay tables, dirty chunks, processing | 10 |
| `test_aco_trails.lua` | Trail spreading, decay, scoring | 11 |
| **Total** | | **21** |

## Expected Results

### Decay Table Tests
- All 5 decay tables created (0.90, 0.92, 0.95, 0.97, 0.98)
- Each table has 10,001 entries (0-10,000)
- Values match `rate ^ tick` calculations

### Dirty Chunk Tests
- Chunks added to correct priority queue
- Duplicates prevented
- Priority upgrades work
- Invalid chunks ignored

### Trail Tests
- Spreading uses 0.95 factor
- Decay uses 0.97/tick
- Scouts lay 2.5× stronger trails
- Behavior-dependent scoring works

## Success Criteria

| Metric | Target | Test |
|--------|--------|------|
| Decay table accuracy | < 0.0001 error | `decay_tables_created_correctly` |
| Dirty chunk processing | 40 chunks/tick | `processDirtyChunks_respects_budget` |
| Trail spreading | 0.95 factor | `trail_spreading_uses_095_factor` |
| Trail decay | 500-tick half-life | `trail_decay_applied_correctly` |
| Scout trail strength | 2.5× normal | `scout_lays_stronger_trails` |
| Score weighting | 0.7/0.3 split | `trail_score_weighted_correctly` |

## Debugging Failed Tests

### Common Issues

**"DECAY_090 table should exist"**
- Constants.lua decay table initialization not running
- Check `createDecayTable()` function

**"Expected playerGenerator after registration"**
- Event handler not marking chunk dirty
- Check `onBuild()` event registration

**"Average time per tick: 10.5 ms"**
- Too many chunks in dirty queue
- Check priority queue processing logic

**"Scout rate should be ~7% but got 15%"**
- Random selection logic incorrect
- Check `math.random() < 0.07` condition

### Test Output Examples

```
=== Running Headless Tests ===

--- MathUtils Tests ---
All 15 tests passed!

--- ACO Core Tests ---
All 10 tests passed!

--- ACO Trails Tests ---
All 11 tests passed!

=== Test Summary ===
Total passed: 142
Total failed: 0
===================
All tests passed!
```

## Performance Benchmarks

Run performance tests after implementation:

```lua
-- CPU Budget Test
local startTime = os.clock()
for tick = 1, 100 do
    Processor.processDirtyChunks(tick)
end
local endTime = os.clock()
local avgTime = (endTime - startTime) / 100 * 1000
print("Average: " .. avgTime .. " ms/tick")  -- Should be < 5ms
```

## Integration Test Scenarios

### Scenario 1: Death Pheromone Spreading
1. Spawn enemy unit
2. Kill enemy
3. Verify death pheromone spreads to 8 neighbors
4. Verify spreading uses 0.75 factor

### Scenario 2: Scout Trail Discovery
1. Place scout in empty zone
2. Move scout to new location
3. Verify trail laid
4. Spawn follower unit
5. Verify follower follows trail

### Scenario 3: Real-time Propagation
1. Place player base
2. Measure time for pheromone to reach 50 chunks away
3. Should be < 60 ticks (1 second)

## Test Coverage Goals

| Component | Coverage | Tests |
|-----------|----------|-------|
| Decay tables | 100% | 2 |
| Dirty chunks | 100% | 4 |
| Decay-on-read | 100% | 3 |
| Trail spreading | 100% | 2 |
| Trail decay | 100% | 2 |
| Scout selection | 100% | 2 |
| Trail scoring | 100% | 3 |
| Diagonal movement | 100% | 1 |
| **Total** | **100%** | **19** |

## Next Steps After Tests Pass

1. ✅ Unit tests pass (100%)
2. ✅ Integration tests pass (100%)
3. ⏳ Performance tests (< 5ms/tick)
4. ⏳ In-game testing (emergent behavior)
5. ⏳ Tune parameters based on gameplay

## Reporting Test Results

When tests run, you should see:
```
--- ACO Core Tests ---
All 10 tests passed!

--- ACO Trails Tests ---
All 11 tests passed!
```

If tests fail, check:
1. Decay table initialization
2. Dirty chunk queue logic
3. Trail spreading factor (0.95)
4. Trail decay rate (0.97)
5. Scout selection rate (0.07)