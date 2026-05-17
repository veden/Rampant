# ACO Testing Strategy

## Overview

Testing the event-driven multi-layer pheromone system with ACO requires multiple testing approaches:

1. **Unit Tests**: Test individual components (decay tables, dirty chunks, trail spreading)
2. **Integration Tests**: Test component interactions (event → dirty → process → trail)
3. **Simulation Tests**: Test full system behavior in simulated game environment
4. **Performance Tests**: Validate CPU usage and propagation speed
5. **In-Game Tests**: Validate emergent behavior in actual Factorio gameplay

---

## Test Categories

### 1. Unit Tests (Headless)

**Location**: `tests/test_aco_core.lua`, `tests/test_aco_trails.lua`, `tests/test_aco_decay.lua`

#### Decay Table Tests
```lua
-- Test decay table creation
test('decay_tables_created_correctly', function()
    assert(#Constants.DECAY_090 == 10001)
    assert(math.abs(Constants.DECAY_090[100] - (0.90 ^ 100)) < 0.0001)
end)

-- Test decay-on-read calculation
test('getEffectivePheromone_applies_decay', function()
    local chunk = { PLAYER_PHEROMONE = 100, playerLastRefresh = 0 }
    local effective = ChunkPropertyUtils.getEffectivePheromone(chunk, 'PLAYER_PHEROMONE', 100)
    assert(math.abs(effective - (100 * 0.98^100)) < 0.01)
end)
```

#### Dirty Chunk Tests
```lua
-- Test dirty chunk marking
test('markChunkDirty_adds_to_priority_queue', function()
    MapUtils.markChunkDirty(123, 1, 1000)
    assert(#Universe.dirtyChunks.priority1 == 1)
    assert(Universe.dirtyChunkSet[123] == 1)
end)

-- Test duplicate prevention
test('markChunkDirty_prevents_duplicates', function()
    MapUtils.markChunkDirty(123, 1, 1000)
    MapUtils.markChunkDirty(123, 2, 1000)
    assert(#Universe.dirtyChunks.priority1 == 1)  -- Still only one
end)

-- Test priority upgrade
test('markChunkDirty_upgrades_priority', function()
    MapUtils.markChunkDirty(123, 3, 1000)
    MapUtils.markChunkDirty(123, 1, 1000)  -- Higher priority
    assert(Universe.dirtyChunkSet[123] == 1)
end)
```

#### Trail Spreading Tests
```lua
-- Test minimal trail spreading
test('trail_spreading_uses_095_factor', function()
    -- Create 3x3 grid of chunks with center having trail = 100
    -- After spreading, center should be ~75 (100 * 0.75 from neighbors)
    -- Neighbors should have ~11.25 (100 * 0.95 / 8)
end)

-- Test trail decay
test('trail_decay_applied_correctly', function()
    local chunk = { positiveTrail = 100, trailLastRefresh = 0 }
    -- After 100 ticks: 100 * 0.97^100 = 4.76
    -- After 500 ticks: 100 * 0.97^500 = 0.0002 (effectively zero)
end)
```

---

### 2. Integration Tests (Factorio Engine)

**Location**: `integration-tests/scenario/control.lua`

#### Event → Dirty → Process Flow
```lua
-- Test: Unit death marks chunk dirty and spreads death pheromone
script.on_init(function()
    local surface = game.surfaces[1]
    local player = game.players[1]
    
    -- Spawn enemy unit
    local enemy = surface.create_entity({
        name = "biter-unit",
        position = { x = 50, y = 50 },
        force = "enemy"
    })
    
    -- Kill enemy
    enemy.die()
    
    -- Verify chunk marked dirty
    -- Verify death pheromone spread to neighbors
    game.print("Test 1: Death pheromone spreading - " .. (success and "PASS" or "FAIL"))
end)
```

#### Scout Trail Integration
```lua
-- Test: Scout unit lays trail when moving
script.on_init(function()
    local surface = game.surfaces[1]
    
    -- Create squad and mark as scout
    local squad = createTestSquad()
    squad.isScout = true
    
    -- Move squad to new chunk
    moveSquadToChunk(squad, { x = 100, y = 100 })
    
    -- Verify trail laid
    local chunk = getChunkAt(100, 100)
    assert(chunk.positiveTrail > 0, "Expected positive trail from scout")
    assert(chunk.positiveTrail > 20, "Expected scout trail > normal unit trail")
end)
```

---

### 3. Simulation Tests

**Location**: `tests/runSimulationTests.lua`

#### Propagation Speed Test
```lua
-- Simulate pheromone propagation across map
local function testPropagationSpeed()
    local world = WorldBuilder:new()
    world:addPlayerBase(0, 0)
    
    local startTick = 1000
    local targetChunk = world:getChunk(50, 50)  -- 50 chunks away
    
    -- Run simulation
    for tick = startTick, startTick + 100 do
        Processor.processDirtyChunks(tick)
        
        if targetChunk.PLAYER_PHEROMONE > 0 then
            local propagationTime = tick - startTick
            print("Pheromone reached 50 chunks in " .. propagationTime .. " ticks")
            assert(propagationTime <= 60, "Expected real-time propagation (< 1 second)")
            return
        end
    end
    
    error("Pheromone did not propagate within 100 ticks")
end
```

#### Trail Discovery Test
```lua
-- Test trail-based exploration in empty zones
local function testTrailDiscovery()
    local world = WorldBuilder:new()
    -- No pheromones in test area
    
    -- Place scout unit
    local scout = world:addUnit({ x = 0, y = 0 }, isScout = true)
    scout:moveTo({ x = 32, y = 32 })  -- Move to next chunk
    
    -- Verify trail laid
    local trailChunk = world:getChunk(32, 32)
    assert(trailChunk.positiveTrail > 0, "Expected trail from scout")
    
    -- Place following unit
    local follower = world:addUnit({ x = 0, y = 0 })
    local nextMove = follower:chooseNextMove()
    
    -- Should follow trail
    assert(nextMove.x == 32, "Expected follower to follow trail")
end)
```

---

### 4. Performance Tests

**Location**: `tests/test_aco_performance.lua`

#### CPU Budget Test
```lua
-- Measure CPU time for processDirtyChunks
local function testCPUBudget()
    local world = WorldBuilder:new()
    
    -- Create realistic scenario: 1000 dirty chunks
    for i = 1, 1000 do
        world:markChunkDirty(i, math.random(1, 5))
    end
    
    -- Measure processing time
    local startTime = os.clock()
    for tick = 1, 100 do
        Processor.processDirtyChunks(tick)
    end
    local endTime = os.clock()
    
    local avgTimePerTick = (endTime - startTime) / 100 * 1000  -- Convert to ms
    print("Average time per tick: " .. avgTimePerTick .. " ms")
    assert(avgTimePerTick < 5, "Expected < 5ms per tick")
end)
```

#### Memory Usage Test
```lua
-- Measure memory overhead
local function testMemoryOverhead()
    local world = WorldBuilder:new()
    world:createMap(100, 100)  -- 10,000 chunks
    
    local before = collectgarbage("count")
    
    -- Initialize ACO system
    Constants.initDecayTables()
    Universe.dirtyChunks = { priority1 = {}, priority2 = {}, ... }
    
    local after = collectgarbage("count")
    local overhead = (after - before) / 1024  -- Convert to MB
    
    print("ACO memory overhead: " .. overhead .. " MB")
    assert(overhead < 1.0, "Expected < 1MB memory overhead")
end)
```

---

### 5. In-Game Tests (Factorio Headless)

**Location**: `integration-tests/scenario/`

#### Emergent Behavior Test
```lua
-- Test: AI discovers hidden base through scouting
script.on_init(function()
    local surface = game.surfaces[1]
    
    -- Create player base in remote location
    local playerBasePos = { x = 1000, y = 1000 }
    surface.create_entity({ name = "player-base", position = playerBasePos })
    
    -- Wait for AI to discover (should happen within 10-20 minutes of game time)
    script.on_event(defines.events.on_tick, function(event)
        if event.tick % 3600 == 0 then  -- Every minute
            local nearbyEnemies = surface.count_entities_filtered({
                position = playerBasePos,
                radius = 100,
                type = "unit"
            })
            
            if nearbyEnemies > 0 then
                game.print("AI discovered hidden base after " .. (event.tick / 60) .. " minutes")
                script.on_event(defines.events.on_tick, nil)  -- Stop monitoring
            end
        end
    end)
end)
```

#### Trail Following Test
```lua
-- Test: Units follow scout trails
script.on_init(function()
    local surface = game.surfaces[1]
    
    -- Create scout path
    for i = 1, 10 do
        local chunk = getChunkAt(i * 32, 0)
        chunk.positiveTrail = 50
        chunk.trailLastRefresh = game.tick
    end
    
    -- Spawn unit at start of trail
    local unit = surface.create_entity({
        name = "biter-unit",
        position = { x = 0, y = 0 },
        force = "enemy"
    })
    
    -- Monitor movement
    script.on_event(defines.events.on_tick, function(event)
        if event.tick % 60 == 0 then
            local position = unit.position
            if position.x > 100 then
                game.print("Unit successfully followed trail")
                script.on_event(defines.events.on_tick, nil)
            end
        end
    end)
end)
```

---

## Test Execution Commands

### Unit Tests (Headless)
```bash
# Run all unit tests
lua tests/runTests.lua

# Run ACO-specific tests
lua tests/runSimulationTests.lua --filter=aco

# Run performance tests
lua tests/test_aco_performance.lua
```

### Integration Tests (Factorio Headless)
```bash
# Run integration tests via Factorio
just test

# Run specific scenario
factorio --headless --load-scenario integration-tests/scenario
```

### Manual In-Game Testing
```lua
-- In Factorio console (/c prefix)

-- Test decay tables
/c log(#game.forces[1].surface.get_chunks())

-- Test trail laying
/c local chunk = game.forces[1].surface.get_chunk(0, 0); chunk.positiveTrail = 100; chunk.trailLastRefresh = game.tick

-- Test dirty chunk processing
/c log(#game.forces[1].surface.get_chunks())
```

---

## Success Criteria

| Test Type | Metric | Target |
|-----------|--------|--------|
| **Unit Tests** | All pass | 100% |
| **Integration Tests** | All pass | 100% |
| **Propagation Speed** | Time to reach 50 chunks | < 60 ticks (1 sec) |
| **CPU Usage** | Average per tick | < 5ms |
| **Memory Overhead** | Total additional memory | < 1MB |
| **Trail Discovery** | Time to discover hidden base | < 20 minutes |
| **Trail Following** | Units follow trails | 90% success rate |

---

## Test Data Collection

During testing, collect:

1. **Propagation curves**: Time vs distance for pheromone spread
2. **Trail formation**: Trail strength vs time vs distance from source
3. **CPU profiles**: Time breakdown by component
4. **Memory snapshots**: Before/after ACO implementation
5. **Gameplay metrics**: Discovery times, path efficiency, squad coordination

This comprehensive testing strategy ensures all aspects of the ACO system are validated before deployment.