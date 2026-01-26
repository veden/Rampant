#!/usr/bin/env lua
--[[
    AI Behavior Tests

    Tests for Rampant AI behavior including pheromone processing,
    squad formation, movement, and attack logic.
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

-- ===== AI BEHAVIOR TESTS =====

test('find_entities_filtered_by_force', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Add enemies
    local enemy1 = world.addEnemy("small-biter", {x = 10, y = 10})
    local enemy2 = world.addEnemy("medium-biter", {x = 20, y = 20})

    -- Add player building
    local turret = world.addBuilding("gun-turret", {x = 100, y = 100})

    -- Find enemy entities
    local enemies = world.surface:findEntitiesFiltered({force = "enemy"})
    assertEquals(2, #enemies, "Should find 2 enemy entities")

    -- Find player entities
    local playerEntities = world.surface:findEntitiesFiltered({force = "player"})
    assertEquals(1, #playerEntities, "Should find 1 player entity")
    assertEquals("gun-turret", playerEntities[1].name, "Should be gun-turret")
end)

test('find_entities_filtered_by_type', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Add various entities
    world.addEnemy("small-biter", {x = 10, y = 10})
    world.addEnemy("medium-biter", {x = 20, y = 20})
    world.addBuilding("gun-turret", {x = 100, y = 100})

    -- Find by type
    local units = world.surface:findEntitiesFiltered({type = "unit"})
    assertEquals(2, #units, "Should find 2 units")

    local buildings = world.surface:findEntitiesFiltered({type = "building"})
    assertEquals(1, #buildings, "Should find 1 building")
end)

test('find_entities_filtered_by_area', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Add entities in different areas
    world.addEnemy("small-biter", {x = 10, y = 10})   -- In area
    world.addEnemy("small-biter", {x = 15, y = 15})  -- In area
    world.addEnemy("small-biter", {x = 100, y = 100}) -- Outside area

    -- Find in area
    local entities = world.surface:findEntitiesFiltered({
        area = {
            left_top = {x = 0, y = 0},
            right_bottom = {x = 32, y = 32}
        }
    })
    assertEquals(2, #entities, "Should find 2 entities in area")
end)

test('find_entities_filtered_by_radius', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Add entities at different distances
    world.addEnemy("small-biter", {x = 10, y = 10})   -- Within 20 tiles
    world.addEnemy("small-biter", {x = 15, y = 15})  -- Within 20 tiles
    world.addEnemy("small-biter", {x = 50, y = 50})  -- Outside 20 tiles

    -- Find in radius
    local entities = world.surface:findEntitiesFiltered({
        position = {x = 10, y = 10},
        radius = 20
    })
    assertEquals(2, #entities, "Should find 2 entities within radius")
end)

test('find_entities_filtered_combined', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Add various entities
    world.addEnemy("small-biter", {x = 10, y = 10})
    world.addEnemy("medium-biter", {x = 12, y = 12})
    world.addEnemy("small-biter", {x = 100, y = 100})

    -- Find by combined filter (force AND type AND area)
    local entities = world.surface:findEntitiesFiltered({
        force = "enemy",
        type = "unit",
        area = {
            left_top = {x = 0, y = 0},
            right_bottom = {x = 32, y = 32}
        }
    })
    assertEquals(2, #entities, "Should find 2 small biters in area")
end)

test('set_multi_command_spawns_units', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Count enemies via surface query (not WorldBuilder's internal tracking)
    local initialEnemies = #world.surface:findEntitiesFiltered({force = "enemy"})

    -- Spawn 5 units via multi-command
    local spawned = world.surface:setMultiCommand({
        unit_count = 5,
        unit_template = "small-biter",
        position = {x = 50, y = 50},
        force = "enemy"
    })

    assertEquals(5, #spawned, "Should spawn 5 entities")

    -- Check entities are registered via surface
    local enemies = world.surface:findEntitiesFiltered({force = "enemy"})
    assertEquals(initialEnemies + 5, #enemies, "Enemy count should increase by 5")

    -- Check entities are registered by name
    local biters = world.surface:findEntitiesFiltered({
        name = "small-biter",
        force = "enemy"
    })
    assertEquals(5, #biters, "Should find 5 small-biters")
end)

test('set_multi_command_with_group', function()
    sim.init()

    local world = sim.createTestWorld()

    local group = world.spawnSquad({x = 50, y = 50})

    -- Spawn units directly into group
    local spawned = world.surface:setMultiCommand({
        unit_count = 3,
        unit_template = "small-biter",
        position = {x = 50, y = 50},
        force = "enemy",
        group = group
    })

    assertEquals(3, #spawned, "Should spawn 3 entities")
    assertEquals(3, group:memberCount(), "Group should have 3 members")

    -- Verify all spawned entities are group members
    for _, unit in ipairs(spawned) do
        assertTrue(group:hasMember(unit), "Unit should be group member")
    end
end)

test('entity_position_chunk_assignment', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Add enemy
    local enemy = world.addEnemy("small-biter", {x = 10, y = 10})

    -- Check it was added to correct chunk
    local chunk = world.surface:getChunk(0, 0)  -- Chunk at 0,0 contains position 10,10
    assert(chunk ~= nil, "Chunk should exist")

    local entitiesInChunk = chunk:findEntities(function(e)
        return e == enemy
    end)
    assertEquals(1, #entitiesInChunk, "Enemy should be in chunk 0,0")
end)

test('squad_state_machine', function()
    sim.init()

    local world = sim.createTestWorld()

    local group = world.spawnSquad({x = 100, y = 100})

    -- Initial state
    assertEquals("gathering", group.state, "Initial state should be gathering")

    -- Add some members
    local unit1 = world.addEnemy("small-biter", {x = 100, y = 100})
    local unit2 = world.addEnemy("small-biter", {x = 101, y = 100})
    group:addMember(unit1)
    group:addMember(unit2)

    -- Set command to move
    group:setCommand({x = 200, y = 200})
    assertEquals("moving", group.state, "State should be moving after command")

    -- Set attack command
    group:setCommandAttack({x = 150, y = 150})
    assertEquals("attacking", group.state, "State should be attacking")

    -- Set stationary
    group:setState("stationary")
    assertEquals("stationary", group.state, "State should be stationary")
end)

test('chunk_pheromone_initial_values', function()
    sim.init()

    local world = sim.createTestWorld()

    local chunk = world.surface:getChunk(0, 0)

    -- Check initial pheromone values
    assertEquals(0, chunk.playerPheromone, "Initial player pheromone should be 0")
    assertEquals(0, chunk.deathPheromone, "Initial death pheromone should be 0")
    assertEquals(0, chunk.victoryScent, "Initial victory scent should be 0")
    assertEquals(0, chunk.basePheromone, "Initial base pheromone should be 0")
    assertEquals(0, chunk.ignorePheromone, "Initial ignore pheromone should be 0")
end)

test('chunk_pheromone_modification', function()
    sim.init()

    local world = sim.createTestWorld()

    local chunk = world.surface:getChunk(0, 0)

    -- Modify pheromones
    chunk.playerPheromone = 100
    chunk.deathPheromone = 50
    chunk.victoryScent = 25
    chunk.basePheromone = 200
    chunk.ignorePheromone = 10

    -- Verify
    assertEquals(100, chunk.playerPheromone, "Player pheromone should be 100")
    assertEquals(50, chunk.deathPheromone, "Death pheromone should be 50")
    assertEquals(25, chunk.victoryScent, "Victory scent should be 25")
    assertEquals(200, chunk.basePheromone, "Base pheromone should be 200")
    assertEquals(10, chunk.ignorePheromone, "Ignore pheromone should be 10")
end)

test('chunk_neighbor_pheromone_access', function()
    sim.init()

    local TickSystem = require("tests/sim.TickSystem")

    local world = sim.WorldBuilder.create()
        :withSurface("nauvis")
        :withChunk(0, 0)
        :withChunk(32, 0)     -- East
        :build()

    local centerChunk = world.surface:getChunk(0, 0)
    local eastChunk = world.surface:getChunk(32, 0)

    -- Set pheromone in center chunk
    centerChunk.playerPheromone = 100

    -- Access neighbor using TickSystem direction constants
    local neighbor = centerChunk:getNeighbor(TickSystem.DIRECTIONS.EAST)
    assert(neighbor ~= nil, "East neighbor should exist")
    assertEquals(32, neighbor.position.x, "Neighbor chunk X should be 32")
end)

-- ===== RUNNER =====

function runTests()
    print("Running Rampant AI Behavior Tests")
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
if arg and arg[0] and arg[0]:match("runAITests") then
    os.exit(runTests() and 0 or 1)
end

return runTests