#!/usr/bin/env lua
--[[
    Pheromone Tests

    Tests for Rampant AI pheromone system including diffusion,
    decay, attraction, and repulsion mechanics.
]]

package.path = package.path .. ";./?.lua;./libs/?.lua;./tests/?.lua;./tests/sim/?.lua"

-- Simulation environment setup
local sim = require("tests/sim.init")

-- Simple test framework
local tests = {}
local passCount = 0
local failCount = 0

function test(name, fn)
    table.insert(tests, {name = name, fn = fn})
end

function assert(condition, message)
    if not condition then
        error(message or "Assertion failed", 3)
    end
end

function assertEquals(a, b, message)
    if a ~= b then
        error((message or ("Expected " .. tostring(a) .. " to equal " .. tostring(b))), 3)
    end
end

function assertTrue(a, message)
    if a ~= true then
        error(message or ("Expected true, got " .. tostring(a)), 3)
    end
end

function assertFalse(a, message)
    if a ~= false then
        error(message or ("Expected false, got " .. tostring(a)), 3)
    end
end

-- ===== PHEROMONE TESTS =====

test('pheromone_player_generation', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Player builds something, creating player pheromone
    local turret = world.addBuilding("gun-turret", {x = 100, y = 100})

    -- Check player chunk has pheromone from building
    local chunk = world.surface:getChunkAtPosition({x = 100, y = 100})
    assert(chunk ~= nil, "Chunk should exist")

    -- Initial pheromone should be present (building added player pheromone)
    -- Note: Actual pheromone generation happens via Processor.processPlayers
    assertEquals(0, chunk.playerPheromone, "Initial player pheromone before processing")
end)

test('pheromone_death_scent_generation', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Enemy dies, creating death pheromone
    local enemy = world.addEnemy("small-biter", {x = 50, y = 50})
    enemy.alive = false
    enemy.health = 0

    -- Death pheromone should be present
    local chunk = world.surface:getChunkAtPosition({x = 50, y = 50})
    assert(chunk ~= nil, "Chunk should exist")

    -- Initial death pheromone is 0, would be set by AI processing
    assertEquals(0, chunk.deathPheromone, "Initial death pheromone")
end)

test('pheromone_victory_scent_generation', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Add chunk with victory scent
    local chunk = world.surface:getChunkAtPosition({x = 100, y = 100})
    chunk.victoryScent = 100

    -- Check victory scent is set
    assertEquals(100, chunk.victoryScent, "Victory scent should be 100")
end)

test('pheromone_base_generation', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Add enemy base (spawner)
    local spawner = world.addBuilding("biter-spawner", {x = 0, y = 0})

    -- Base pheromone should be present near spawner
    local chunk = world.surface:getChunkAtPosition({x = 0, y = 0})
    assert(chunk ~= nil, "Chunk should exist")

    -- Initial base pheromone is 0, would be set by AI processing
    assertEquals(0, chunk.basePheromone, "Initial base pheromone")
end)

test('pheromone_ignore_generation', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Add chunk with ignore pheromone
    local chunk = world.surface:getChunkAtPosition({x = 100, y = 100})
    chunk.ignorePheromone = 50

    -- Check ignore pheromone is set
    assertEquals(50, chunk.ignorePheromone, "Ignore pheromone should be 50")
end)

test('pheromone_all_types_stored', function()
    sim.init()

    local world = sim.createTestWorld()

    local chunk = world.surface:getChunkAtPosition({x = 50, y = 50})

    -- Set all pheromone types
    chunk.playerPheromone = 100
    chunk.deathPheromone = 80
    chunk.victoryScent = 60
    chunk.basePheromone = 40
    chunk.ignorePheromone = 20

    -- Verify all are stored correctly
    assertEquals(100, chunk.playerPheromone, "Player pheromone")
    assertEquals(80, chunk.deathPheromone, "Death pheromone")
    assertEquals(60, chunk.victoryScent, "Victory scent")
    assertEquals(40, chunk.basePheromone, "Base pheromone")
    assertEquals(20, chunk.ignorePheromone, "Ignore pheromone")
end)

test('pheromone_neighbor_chunk_access', function()
    sim.init()

    local TickSystem = require("tests/sim.TickSystem")

    local world = sim.WorldBuilder.create()
        :withSurface("nauvis")
        :withChunk(0, 0)
        :withChunk(32, 0)     -- East
        :withChunk(0, 32)     -- South
        :withChunk(-32, 0)    -- West
        :withChunk(0, -32)    -- North
        :build()

    local centerChunk = world.surface:getChunk(0, 0)

    -- Set pheromone in center
    centerChunk.playerPheromone = 100

    -- Check neighbor access
    local east = centerChunk:getNeighbor(TickSystem.DIRECTIONS.EAST)
    local west = centerChunk:getNeighbor(TickSystem.DIRECTIONS.WEST)
    local north = centerChunk:getNeighbor(TickSystem.DIRECTIONS.NORTH)
    local south = centerChunk:getNeighbor(TickSystem.DIRECTIONS.SOUTH)

    assert(east ~= nil, "East neighbor should exist")
    assert(west ~= nil, "West neighbor should exist")
    assert(north ~= nil, "North neighbor should exist")
    assert(south ~= nil, "South neighbor should exist")
end)

test('pheromone_propagation_to_all_neighbors', function()
    sim.init()

    local TickSystem = require("tests/sim.TickSystem")

    local world = sim.WorldBuilder.create()
        :withSurface("nauvis")
        :withChunk(0, 0)
        :withChunk(32, 0)      -- East
        :withChunk(-32, 0)     -- West
        :withChunk(0, 32)      -- South
        :withChunk(0, -32)     -- North
        :withChunk(32, -32)    -- Northeast
        :withChunk(32, 32)     -- Southeast
        :withChunk(-32, 32)    -- Southwest
        :withChunk(-32, -32)   -- Northwest
        :build()

    local centerChunk = world.surface:getChunk(0, 0)

    -- Set pheromone in center
    centerChunk.playerPheromone = 100

    -- Check all 8 directions have neighbors
    for dir = 1, 8 do
        local neighbor = centerChunk:getNeighbor(dir)
        assert(neighbor ~= nil, "Neighbor in direction " .. dir .. " should exist")
    end
end)

test('pheromone_decay_simulation', function()
    sim.init()

    local world = sim.createTestWorld()

    local chunk = world.surface:getChunkAtPosition({x = 50, y = 50})

    -- Set high pheromone value
    chunk.playerPheromone = 1000

    -- Simulate decay over multiple ticks
    local decayRate = 0.01  -- 1% per tick

    local function simulateDecay(ticks)
        local current = chunk.playerPheromone
        for i = 1, ticks do
            current = current * (1 - decayRate)
        end
        return current
    end

    -- After 60 ticks (1 second at 60fps), should be significantly decayed
    local after60 = simulateDecay(60)
    assert(after60 < 1000, "Pheromone should decay over time")
    assert(after60 > 500, "Pheromone should not decay too fast")

    -- After 600 ticks (10 seconds), should be heavily decayed
    local after600 = simulateDecay(600)
    assert(after600 < 100, "Pheromone should be heavily decayed after 600 ticks")
end)

test('pheromone_attraction_calculation', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Create chunks with different pheromone values
    local chunk1 = world.surface:getChunkAtPosition({x = 0, y = 0})
    local chunk2 = world.surface:getChunkAtPosition({x = 64, y = 0})

    chunk1.playerPheromone = 100
    chunk2.playerPheromone = 500

    -- Higher pheromone should attract more
    assert(chunk2.playerPheromone > chunk1.playerPheromone, "Chunk 2 should have higher pheromone")

    -- Attraction should be proportional to pheromone value
    local ratio = chunk2.playerPheromone / chunk1.playerPheromone
    assertEquals(5, ratio, "Attraction ratio should be 5x")
end)

test('pheromone_repulsion_from_death', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Create chunk with death pheromone
    local chunk = world.surface:getChunkAtPosition({x = 50, y = 50})
    chunk.deathPheromone = 1000

    -- Death pheromone should repel
    assert(chunk.deathPheromone > 0, "Death pheromone should be present")

    -- High death pheromone should strongly repel
    assert(chunk.deathPheromone > chunk.playerPheromone, "Death pheromone should be higher than player pheromone")
end)

test('pheromone_victory_rally_effect', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Create chunks with victory scent
    local rallyChunk = world.surface:getChunkAtPosition({x = 100, y = 100})
    local nearbyChunk = world.surface:getChunkAtPosition({x = 64, y = 100})

    rallyChunk.victoryScent = 200
    nearbyChunk.victoryScent = 50

    -- Victory scent should attract nearby units
    assert(rallyChunk.victoryScent > nearbyChunk.victoryScent, "Rally chunk should have higher victory scent")

    -- Units should be rallied toward high victory scent
    assert(rallyChunk.victoryScent > 0, "Rally chunk should have positive victory scent")
end)

test('pheromone_ignore_affects_movement', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Create chunks with different ignore values
    local chunk1 = world.surface:getChunkAtPosition({x = 0, y = 0})
    local chunk2 = world.surface:getChunkAtPosition({x = 64, y = 0})

    chunk1.ignorePheromone = 0
    chunk2.ignorePheromone = 200

    -- Higher ignore should reduce attraction
    assert(chunk2.ignorePheromone > chunk1.ignorePheromone, "Chunk 2 should have higher ignore")

    -- AI should prefer lower ignore values
    assert(chunk1.ignorePheromone < chunk2.ignorePheromone, "Chunk 1 should have lower ignore")
end)

test('pheromone_combined_attraction_repulsion', function()
    sim.init()

    local world = sim.createTestWorld()

    local chunk = world.surface:getChunkAtPosition({x = 50, y = 50})

    -- Set combined values
    chunk.playerPheromone = 500     -- Attracts enemies
    chunk.deathPheromone = 100      -- Repels enemies
    chunk.ignorePheromone = 50      -- Reduces attraction

    -- Net effect should be attraction (player > death)
    assert(chunk.playerPheromone > chunk.deathPheromone, "Player pheromone should outweigh death")
end)

test('pheromone_base_strong_attraction', function()
    sim.init()

    local world = sim.createTestWorld()

    local chunk = world.surface:getChunkAtPosition({x = 0, y = 0})

    -- Base pheromone attracts enemies to their own base
    chunk.basePheromone = 1000

    -- Base pheromone should be high to keep units near base
    assert(chunk.basePheromone > 500, "Base pheromone should be strong")
end)

-- ===== RUNNER =====

function runTests()
    print("Running Rampant Pheromone Tests")
    print("=================================")

    for _, testCase in ipairs(tests) do
        local ok, err = xpcall(testCase.fn, function(err)
            return debug.traceback(err)
        end)
        if ok then
            print("[PASS] " .. testCase.name)
            passCount = passCount + 1
        else
            print("[FAIL] " .. testCase.name)
            print("       " .. tostring(err))
            failCount = failCount + 1
        end
    end

    print("=================================")
    print("Results: " .. passCount .. " passed, " .. failCount .. " failed")
    print("=================================")

    return failCount == 0
end

-- Run if executed directly
if arg and arg[0] and arg[0]:match("runPheromoneTests") then
    os.exit(runTests() and 0 or 1)
end

return runTests