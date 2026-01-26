#!/usr/bin/env lua
--[[
    Simulation Test Runner

    Runs simulation tests for Rampant AI without requiring Factorio.
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

function assertNil(a, message)
    if a ~= nil then
        error(message or ("Expected nil, got " .. tostring(a)), 3)
    end
end

-- ===== SIMULATION TESTS =====

test('tick_system_advancement', function()
    sim.init()

    assertEquals(0, sim.TickSystem.now(), "Initial tick should be 0")

    sim.TickSystem.advance(10)
    assertEquals(10, sim.TickSystem.now(), "Tick should advance to 10")

    sim.TickSystem.advance(5)
    assertEquals(15, sim.TickSystem.now(), "Tick should advance to 15")
end)

test('tick_system_listeners', function()
    sim.init()

    local callCount = 0
    sim.TickSystem.onTick(function(t)
        callCount = callCount + 1
    end)

    sim.TickSystem.advance(5)
    assertEquals(5, callCount, "Listener should be called 5 times")
end)

test('chunk_creation', function()
    sim.init()

    local world = sim.WorldBuilder.create()
        :withSurface("nauvis")
        :withChunk(0, 0)
        :withChunk(32, 32)
        :build()

    local surface = world.surface

    -- Check chunks exist
    local chunk1 = surface:getChunk(0, 0)
    assert(chunk1 ~= nil, "Chunk at 0,0 should exist")

    local chunk2 = surface:getChunk(32, 32)
    assert(chunk2 ~= nil, "Chunk at 32,32 should exist")

    -- Check positions
    assertEquals(0, chunk1.position.x, "Chunk 1 X position")
    assertEquals(0, chunk1.position.y, "Chunk 1 Y position")

    assertEquals(32, chunk2.position.x, "Chunk 2 X position")
    assertEquals(32, chunk2.position.y, "Chunk 2 Y position")
end)

test('entity_creation', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Create a unit
    local unit = world.addEnemy("small-biter", {x = 50, y = 50})

    assert(unit ~= nil, "Unit should be created")
    assertEquals("small-biter", unit.name, "Unit name should match")
    assertEquals("enemy", unit.force, "Unit force should be enemy")
    assert(unit.alive == true, "Unit should be alive")
    assertEquals(100, unit.health, "Unit should have 100 health")
end)

test('entity_chunk_assignment', function()
    sim.init()

    local world = sim.createTestWorld()

    local unit = world.addEnemy("small-biter", {x = 16, y = 16})

    local chunk = world.surface:getChunkAtPosition(unit.position)
    assert(chunk ~= nil, "Chunk should exist")
    assert(chunk:contains(unit.position), "Unit should be in chunk")

    local entitiesInChunk = chunk:findEntities(function(e)
        return e == unit
    end)
    assertEquals(1, #entitiesInChunk, "Chunk should contain the unit")
end)

test('distance_calculation', function()
    sim.init()

    local world = sim.createTestWorld()

    local unit = world.addEnemy("small-biter", {x = 0, y = 0})
    local player = {position = {x = 30, y = 40}}

    local dist = unit:distanceTo(player.position)
    assertTrue(dist > 49 and dist < 51, "Distance should be ~50 (3-4-5 triangle)")
end)

test('surface_radius_query', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Create units at various positions
    world.addEnemy("small-biter", {x = 100, y = 100})
    world.addEnemy("small-biter", {x = 105, y = 100})
    world.addEnemy("small-biter", {x = 200, y = 200})

    local found = world.surface:findInRadius({x = 100, y = 100}, 10)

    assertEquals(2, #found, "Should find 2 units within radius 10")
end)

test('world_builder_chaining', function()
    sim.init()

    local world = sim.WorldBuilder.create()
        :withSurface("nauvis")
        :withChunk(0, 0)
        :withChunk(32, 0)
        :withBase({x = 0, y = 0})
        :withPlayer({x = 50, y = 50})
        :withEnemyEntity("small-biter", {x = 10, y = 10})
        :withPlayerBuilding("gun-turret", {x = 50, y = 50})
        :build()

    assert(world.surface ~= nil, "Surface should exist")
    assertEquals(1, world.getPlayerCount(), "Should have 1 player")
    assertEquals(1, world.getBaseCount(), "Should have 1 base")
    assertTrue(world.countEnemies() >= 1, "Should have enemies")
end)

test('unit_group_creation', function()
    sim.init()

    local world = sim.createTestWorld()

    local group = world.spawnSquad({x = 100, y = 100})
    assert(group ~= nil, "Group should be created")
    assertEquals(0, group:memberCount(), "Group should start empty")
    assertEquals("gathering", group.state, "Initial state should be gathering")
end)

test('unit_group_membership', function()
    sim.init()

    local world = sim.createTestWorld()

    local group = world.spawnSquad({x = 100, y = 100})

    local unit1 = world.addEnemy("small-biter", {x = 100, y = 100})
    local unit2 = world.addEnemy("small-biter", {x = 101, y = 100})

    group:addMember(unit1)
    assertTrue(group:hasMember(unit1), "Unit 1 should be member")

    group:addMember(unit2)
    assertTrue(group:hasMember(unit2), "Unit 2 should be member")

    assertEquals(2, group:memberCount(), "Group should have 2 members")

    group:removeMember(unit1)
    assertEquals(1, group:memberCount(), "Group should have 1 member after removal")
end)

test('entity_damage', function()
    sim.init()

    local world = sim.createTestWorld()

    local unit = world.addEnemy("small-biter", {x = 50, y = 50})

    assertEquals(100, unit.health, "Initial health")

    unit:damage(30)
    assertEquals(70, unit.health, "Health after 30 damage")

    unit:damage(50)
    assertEquals(20, unit.health, "Health after 50 more damage")

    unit:damage(30)
    assertEquals(0, unit.health, "Health should be 0 after fatal damage")
    assert(unit.alive == false, "Unit should be dead")
end)

test('surface_entity_tracking', function()
    sim.init()

    local world = sim.createTestWorld()

    world.addEnemy("small-biter", {x = 10, y = 10})
    world.addEnemy("small-biter", {x = 20, y = 20})
    world.addEnemy("medium-biter", {x = 30, y = 30})

    assertEquals(3, world.countEnemies(), "Should have 3 enemies")

    -- Find by name
    local smallBiters = world.surface._byName["small-biter"]
    assertEquals(2, #smallBiters, "Should have 2 small biters")
end)

test('game_adapter_forces', function()
    sim.init()

    local world = sim.createTestWorld()

    assert(_G.game ~= nil, "Game global should exist")
    assert(_G.game.forces ~= nil, "Game forces should exist")
    assert(_G.game.forces.enemy ~= nil, "Enemy force should exist")
    assert(_G.game.forces.player ~= nil, "Player force should exist")
    assertEquals("enemy", _G.game.forces.enemy.name, "Enemy force name")
end)

test('script_adapter_events', function()
    sim.init()

    assert(_G.script ~= nil, "Script global should exist")
    assert(_G.script.on_event ~= nil, "on_event should exist")

    local eventCalled = false
    _G.script.on_event(_G.script.events.on_tick, function()
        eventCalled = true
    end)

    assertEquals(1, #_G.script._eventHandlers[_G.script.events.on_tick], "Handler should be registered")
end)

test('remote_adapter_interfaces', function()
    sim.init()

    assert(_G.remote ~= nil, "Remote global should exist")

    _G.remote.add_interface("TestAPI", {
        test = function() return "hello" end
    })

    local api = _G.remote.get_interface("TestAPI")
    assert(api ~= nil, "Interface should be retrievable")
    assertEquals("hello", api.test(), "Interface function should work")
end)

test('chunk_neighbor_lookups', function()
    sim.init()

    local TickSystem = require("tests/sim.TickSystem")

    local world = sim.WorldBuilder.create()
        :withSurface("nauvis")
        :withChunk(0, 0)
        :withChunk(32, 0)     -- East
        :withChunk(-32, 0)    -- West
        :withChunk(0, 32)     -- South
        :withChunk(0, -32)    -- North
        :build()

    local centerChunk = world.surface:getChunk(0, 0)
    assert(centerChunk ~= nil, "Center chunk should exist")

    -- Check neighbors exist using TickSystem direction constants
    local eastChunk = centerChunk:getNeighbor(TickSystem.DIRECTIONS.EAST)
    assert(eastChunk ~= nil, "East neighbor should exist")
    assertEquals(32, eastChunk.position.x, "East chunk X position")

    local westChunk = centerChunk:getNeighbor(TickSystem.DIRECTIONS.WEST)
    assert(westChunk ~= nil, "West neighbor should exist")
    assertEquals(-32, westChunk.position.x, "West chunk X position")

    local northChunk = centerChunk:getNeighbor(TickSystem.DIRECTIONS.NORTH)
    assert(northChunk ~= nil, "North neighbor should exist")
    assertEquals(-32, northChunk.position.y, "North chunk Y position")

    local southChunk = centerChunk:getNeighbor(TickSystem.DIRECTIONS.SOUTH)
    assert(southChunk ~= nil, "South neighbor should exist")
    assertEquals(32, southChunk.position.y, "South chunk Y position")
end)

test('pheromone_storage_retrieval', function()
    sim.init()

    local world = sim.createTestWorld()

    local chunk = world.surface:getChunk(0, 0)
    assert(chunk ~= nil, "Chunk should exist")

    -- Check initial pheromone values
    assertEquals(0, chunk.playerPheromone, "Initial player pheromone should be 0")
    assertEquals(0, chunk.deathPheromone, "Initial death pheromone should be 0")
    assertEquals(0, chunk.victoryScent, "Initial victory scent should be 0")
    assertEquals(0, chunk.basePheromone, "Initial base pheromone should be 0")

    -- Set pheromone values
    chunk.playerPheromone = 100
    chunk.deathPheromone = 50
    chunk.victoryScent = 25
    chunk.basePheromone = 200

    -- Verify retrieval
    assertEquals(100, chunk.playerPheromone, "Player pheromone should be 100")
    assertEquals(50, chunk.deathPheromone, "Death pheromone should be 50")
    assertEquals(25, chunk.victoryScent, "Victory scent should be 25")
    assertEquals(200, chunk.basePheromone, "Base pheromone should be 200")
end)

test('surface_entity_indexing_by_force', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Add enemies
    local enemy1 = world.addEnemy("small-biter", {x = 10, y = 10})
    local enemy2 = world.addEnemy("medium-biter", {x = 20, y = 20})

    -- Add player building
    local playerBuilding = world.addBuilding("gun-turret", {x = 100, y = 100})

    -- Check enemy force entities via surface indexing
    local enemyEntities = world.surface._byForce["enemy"]
    assert(#enemyEntities >= 2, "Enemy force should have entities")

    -- Check player force entities
    local playerEntities = world.surface._byForce["player"]
    assert(#playerEntities >= 1, "Player force should have entities")

    -- Verify entities are tracked correctly
    assertEquals("enemy", enemy1.force, "Enemy1 force should be enemy")
    assertEquals("enemy", enemy2.force, "Enemy2 force should be enemy")
    assertEquals("player", playerBuilding.force, "Player building force should be player")
end)

test('unit_group_state_transitions', function()
    sim.init()

    local world = sim.createTestWorld()

    local group = world.spawnSquad({x = 100, y = 100})

    -- Check initial state
    assertEquals("gathering", group.state, "Initial state should be gathering")

    -- Test state transitions
    group:setCommand({x = 200, y = 200})
    assertEquals("moving", group.state, "State should be moving after setCommand")

    group:setCommandAttack({x = 150, y = 150})
    assertEquals("attacking", group.state, "State should be attacking after setCommandAttack")

    group:setState("stationary")
    assertEquals("stationary", group.state, "State should be stationary")

    group:setState("fleeing")
    assertEquals("fleeing", group.state, "State should be fleeing")
end)

-- ===== RUNNER =====

function runTests()
    print("Running Rampant Simulation Tests")
    print("================================")

    for _, t in ipairs(tests) do
        local ok, err = xpcall(t.fn, function(e)
            return debug.traceback(e)
        end)

        if ok then
            print("[PASS] " .. t.name)
            passCount = passCount + 1
        else
            print("[FAIL] " .. t.name)
            print("       " .. tostring(err))
            failCount = failCount + 1
        end
    end

    print("================================")
    print("Results: " .. passCount .. " passed, " .. failCount .. " failed")
    print("================================")

    return failCount == 0
end

-- Run if executed directly
if arg and arg[0] and arg[0]:match("runSimulationTests") then
    local success = runTests()
    os.exit(success and 0 or 1)
end

-- Export for use
return {
    test = test,
    assert = assert,
    assertEquals = assertEquals,
    assertTrue = assertTrue,
    assertNil = assertNil,
    runTests = runTests
}