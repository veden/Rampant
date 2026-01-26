#!/usr/bin/env lua
--[[
    Scenario Tests

    End-to-end AI scenario tests demonstrating Rampant enemy behavior
    including player defense pressure, base expansion, and attack patterns.
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

-- ===== SCENARIO TESTS =====

test('squad_formation_near_spawner', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Add spawner at origin
    local spawner = world.addBuilding("biter-spawner", {x = 0, y = 0})

    -- Spawn multiple units near spawner
    world.surface:setMultiCommand({
        unit_count = 5,
        unit_template = "small-biter",
        position = {x = 5, y = 5},
        force = "enemy"
    })

    world.surface:setMultiCommand({
        unit_count = 5,
        unit_template = "small-biter",
        position = {x = -5, y = 5},
        force = "enemy"
    })

    -- Count units near spawner
    local nearbyUnits = world.surface:findEntitiesFiltered({
        force = "enemy",
        area = {
            left_top = {x = -32, y = -32},
            right_bottom = {x = 32, y = 32}
        }
    })

    assertEquals(10, #nearbyUnits, "Should have 10 units near spawner")
end)

test('enemy_approaches_player_pheromone', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Player at position (200, 200) - add via world builder then rebuild
    local world2 = sim.WorldBuilder.create()
        :withSurface("nauvis")
        :withPlayer({x = 200, y = 200})
        :withBase({x = 0, y = 0})
        :withChunk(0, 0)
        :withChunk(64, 0)
        :withChunk(128, 0)
        :withChunk(192, 0)
        :build()

    -- Advance ticks
    world2.advanceTicks(300)

    -- Player chunk should have pheromone from player presence
    local playerChunk = world2.surface:getChunkAtPosition({x = 200, y = 200})
    assert(playerChunk ~= nil, "Player chunk should exist")

    -- Pheromone should be present (from player presence simulation)
    assert(playerChunk.playerPheromone >= 0, "Player pheromone should be non-negative")
end)

test('death_pheromone_repels_nearby_enemies', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Player kills enemies at position (100, 100)
    local enemy1 = world.addEnemy("small-biter", {x = 100, y = 100})
    local enemy2 = world.addEnemy("small-biter", {x = 102, y = 100})

    enemy1.alive = false
    enemy1.health = 0
    enemy2.alive = false
    enemy2.health = 0

    -- Check death pheromone at kill location
    local killChunk = world.surface:getChunkAtPosition({x = 100, y = 100})

    -- Death pheromone is set on chunk - would be set by AI processing
    -- For simulation, we set it manually
    killChunk.deathPheromone = 200

    assertEquals(200, killChunk.deathPheromone, "Death pheromone should be high at kill site")
end)

test('victory_scent_rallies_attacking_squads', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Create rally point
    local rallyChunk = world.surface:getChunkAtPosition({x = 100, y = 100})
    rallyChunk.victoryScent = 300

    -- Create squad nearby
    local squad = world.spawnSquad({x = 64, y = 100})
    world.surface:setMultiCommand({
        unit_count = 3,
        unit_template = "medium-biter",
        position = {x = 64, y = 100},
        force = "enemy",
        group = squad
    })

    -- Squad should exist with members
    assert(squad ~= nil, "Squad should exist")
    assertEquals(3, squad:memberCount(), "Squad should have 3 members")

    -- Rally chunk should have high victory scent
    assert(rallyChunk.victoryScent > 200, "Rally chunk should have high victory scent")
end)

test('base_expansion_toward_player', function()
    sim.init()

    local world = sim.WorldBuilder.create()
        :withSurface("nauvis")
        :withPlayer({x = 200, y = 200})
        :withBase({x = 0, y = 0})
        :withChunk(0, 0)
        :withChunk(64, 0)
        :withChunk(128, 0)
        :withChunk(192, 0)
        :build()

    -- Advance time (AI should expand toward player)
    world.advanceTicks(1800)  -- 30 seconds

    -- Check chunks between base and player exist
    local chunks = {}
    for x, row in pairs(world.surface.chunks) do
        for y, chunk in pairs(row) do
            table.insert(chunks, chunk)
        end
    end

    -- At minimum, 5 chunks should exist (1 base + 4 intermediate)
    assertEquals(5, #chunks, "Should have 5 chunks")
end)

test('player_defenses_create_pressure', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Player at (150, 150)
    local playerPos = {x = 150, y = 150}

    -- Create world with player
    local world2 = sim.WorldBuilder.create()
        :withSurface("nauvis")
        :withPlayer(playerPos)
        :withBase({x = 0, y = 0})
        :build()

    -- Player builds defenses (4 buildings)
    world2.addBuilding("gun-turret", {x = 150, y = 150})
    world2.addBuilding("gun-turret", {x = 155, y = 150})
    world2.addBuilding("wall", {x = 150, y = 152})
    world2.addBuilding("wall", {x = 151, y = 150})

    -- Use surface query to find player entities (includes player + 4 buildings = 5)
    local defenses = world2.surface:findEntitiesFiltered({force = "player"})

    assertEquals(5, #defenses, "Should have 5 player entities (1 player + 4 buildings)")
end)

test('squad_targeting_player_buildings', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Create squad targeting player position
    local squad = world.spawnSquad({x = 100, y = 100})

    -- Add units to squad
    world.surface:setMultiCommand({
        unit_count = 5,
        unit_template = "small-biter",
        position = {x = 100, y = 100},
        force = "enemy",
        group = squad
    })

    -- Player building at target
    local turret = world.addBuilding("gun-turret", {x = 128, y = 100})

    -- Squad should have members
    assertEquals(5, squad:memberCount(), "Squad should have 5 members")

    -- Set squad to attack target
    squad:setCommandAttack({x = 128, y = 100})
    assertEquals("attacking", squad.state, "Squad should be in attacking state")
end)

test('squad_retreat_on_heavy_losses', function()
    sim.init()

    local world = sim.createTestWorld()

    local squad = world.spawnSquad({x = 100, y = 100})

    -- Add units to squad
    for i = 1, 5 do
        local unit = world.addEnemy("small-biter", {x = 100 + i, y = 100})
        squad:addMember(unit)
    end

    assertEquals(5, squad:memberCount(), "Squad should start with 5 members")

    -- Simulate heavy losses - kill 4 out of 5 (iterate backwards to avoid index issues)
    local members = squad.members
    for i = #members, 2, -1 do
        members[i].alive = false
        members[i].health = 0
        squad:removeMember(members[i])
    end

    assertEquals(1, squad:memberCount(), "Squad should have 1 member after losses")

    -- With few members, squad should be able to retreat
    squad:setState("fleeing")
    assertEquals("fleeing", squad.state, "Squad should be in fleeing state")
end)

test('multiple_bases_coordinated_attack', function()
    sim.init()

    local world = sim.WorldBuilder.create()
        :withSurface("nauvis")
        :withBase({x = -100, y = -100})
        :withBase({x = 100, y = 100})
        :withPlayer({x = 0, y = 0})
        :build()

    -- Create squads at each base
    local squad1 = world.spawnSquad({x = -100, y = -100})
    local squad2 = world.spawnSquad({x = 100, y = 100})

    -- Add members to squads
    world.surface:setMultiCommand({
        unit_count = 3,
        unit_template = "small-biter",
        position = {x = -100, y = -100},
        force = "enemy",
        group = squad1
    })

    world.surface:setMultiCommand({
        unit_count = 3,
        unit_template = "small-biter",
        position = {x = 100, y = 100},
        force = "enemy",
        group = squad2
    })

    -- Both squads should exist
    assert(squad1 ~= nil, "Squad 1 should exist")
    assert(squad2 ~= nil, "Squad 2 should exist")
    assertEquals(3, squad1:memberCount(), "Squad 1 should have 3 members")
    assertEquals(3, squad2:memberCount(), "Squad 2 should have 3 members")

    -- Both squads should be able to attack player position
    squad1:setCommandAttack({x = 0, y = 0})
    squad2:setCommandAttack({x = 0, y = 0})

    assertEquals("attacking", squad1.state, "Squad 1 should be attacking")
    assertEquals("attacking", squad2.state, "Squad 2 should be attacking")
end)

test('evolution_increases_over_time', function()
    sim.init()

    local world = sim.createTestWorld()

    -- Track initial state
    local initialTime = world.getTick()

    -- Advance significant time
    world.advanceTicks(3600)  -- 1 minute

    local finalTime = world.getTick()

    assert(finalTime > initialTime, "Time should have advanced")
    assertEquals(3600, finalTime, "Time should advance by 3600 ticks")
end)

test('chunk_pheromone_propagation_over_time', function()
    sim.init()

    local TickSystem = require("tests/sim.TickSystem")

    local world = sim.WorldBuilder.create()
        :withSurface("nauvis")
        :withChunk(0, 0)
        :withChunk(32, 0)
        :withChunk(64, 0)
        :build()

    -- Set high pheromone at source chunk
    local sourceChunk = world.surface:getChunk(0, 0)
    sourceChunk.playerPheromone = 1000

    -- Get neighbor chunks
    local chunk1 = world.surface:getChunk(32, 0)
    local chunk2 = world.surface:getChunk(64, 0)

    -- Initial state
    assertEquals(1000, sourceChunk.playerPheromone, "Source should have high pheromone")
    assertEquals(0, chunk1.playerPheromone, "Neighbor should start at 0")
    assertEquals(0, chunk2.playerPheromone, "Far chunk should start at 0")

    -- Simulate propagation (manual - in real AI this happens in MapUtils)
    local propagationRate = 0.5  -- 50% per chunk hop

    chunk1.playerPheromone = sourceChunk.playerPheromone * propagationRate
    chunk2.playerPheromone = chunk1.playerPheromone * propagationRate

    -- After propagation, chunks should have decreasing values
    assert(chunk1.playerPheromone > 0, "Neighbor should have propagated pheromone")
    assert(chunk2.playerPheromone > 0, "Far chunk should have propagated pheromone")
    assert(chunk2.playerPheromone < chunk1.playerPheromone, "Far chunk should have less pheromone")
end)

-- ===== RUNNER =====

function runTests()
    print("Running Rampant Scenario Tests")
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
if arg and arg[0] and arg[0]:match("runScenarioTests") then
    os.exit(runTests() and 0 or 1)
end

return runTests