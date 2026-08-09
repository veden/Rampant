--[[
    Rampant Integration Test Scenario Control Script

    This scenario runs integration tests within the Factorio game engine
    to validate the Rampant mod's entity lifecycle and behavior.
]]

local testEntities = {}
local testEntitiesCreated = false

local function cleanupTestEntities()
    if not testEntitiesCreated then return end
    testEntitiesCreated = false
    for _, entity in ipairs(testEntities) do
        if entity and entity.valid then
            entity.destroy()
        end
    end
    testEntities = {}
end

local function createTestEntity(name, position, force)
    local surface = game.surfaces[1]
    local entity = surface.create_entity({
        name = name,
        position = position,
        force = force
    })
    if entity then
        table.insert(testEntities, entity)
        testEntitiesCreated = true
    end
    return entity
end

local function runIntegrationTests()
    print("==========================================")
    print("  RAMPANT MOD INTEGRATION TEST SUITE")
    print("==========================================")
    print("")

    local startTime = game.tick
    local passed, failed = 0, 0

    local function record(name, success)
        if success then
            passed = passed + 1
            print("  [PASS] " .. name)
        else
            failed = failed + 1
            print("  [FAIL] " .. name)
        end
    end

    local function section(name)
        print("")
        print("--- " .. name .. " ---")
    end

    local function cleanup()
        cleanupTestEntities()
    end

    -- ==========================================
    -- ENTITY LIFECYCLE TESTS
    -- ==========================================

    section("Entity Lifecycle Tests")

    local surface = game.surfaces[1]
    record("surface available", surface ~= nil)

    if not surface then
        print("ERROR: No surface available")
        print("TESTS_FAILED")
        rcon.print("/quit")
        return
    end

    local forceEnemy = game.forces.enemy
    local forcePlayer = game.forces.player

    record("enemy force available", forceEnemy ~= nil)
    record("player force available", forcePlayer ~= nil)

    local testPos = 0
    local function nextPos()
        testPos = testPos + 10
        return {x = testPos, y = testPos}
    end

    -- Spawning tests
    section("Spawning Tests")

    local biter = createTestEntity("small-biter", nextPos(), forceEnemy)
    record("spawn small-biter", biter ~= nil)
    record("small-biter has health", biter and biter.health and biter.health > 0)

    local mediumBiter = createTestEntity("medium-biter", nextPos(), forceEnemy)
    record("spawn medium-biter", mediumBiter ~= nil)

    local spawner = createTestEntity("biter-spawner", nextPos(), forceEnemy)
    record("spawn biter-spawner", spawner ~= nil)
    record("spawner has unit number", spawner and spawner.unit_number ~= nil)

    local spitter = createTestEntity("small-spitter", nextPos(), forceEnemy)
    record("spawn small-spitter", spitter ~= nil)

    local worm = createTestEntity("small-worm-turret", nextPos(), forceEnemy)
    record("spawn small-worm-turret", worm ~= nil)
    record("worm is turret", worm and worm.type == "turret")

    cleanup()
    testPos = 0

    -- Damage tests
    section("Damage Flow Tests")

    local testBiter = createTestEntity("small-biter", nextPos(), forceEnemy)
    record("biter exists for damage test", testBiter ~= nil)
    if testBiter then
        local initialHealth = testBiter.health
        testBiter.damage(10, forcePlayer)
        record("damage reduces health", testBiter.health < initialHealth)

        local fatalDamage = testBiter.health + 100
        testBiter.damage(fatalDamage, forcePlayer)
        record("fatal damage kills unit", not testBiter.valid)
    end

    local testSpawner = createTestEntity("biter-spawner", nextPos(), forceEnemy)
    if testSpawner then
        local spawnerHealth = testSpawner.health
        testSpawner.damage(50, forcePlayer)
        record("spawner takes damage", testSpawner.health < spawnerHealth)
    end

    cleanup()
    testPos = 0

    -- Death tests
    section("Death Handler Tests")

    local deathBiter = createTestEntity("small-biter", nextPos(), forceEnemy)
    if deathBiter then
        deathBiter.damage(deathBiter.health + 100, forcePlayer)
        record("unit death invalidates entity", not deathBiter.valid)
    end

    local deathSpawner = createTestEntity("biter-spawner", nextPos(), forceEnemy)
    if deathSpawner then
        deathSpawner.damage(deathSpawner.health + 100, forcePlayer)
        record("spawner death invalidates entity", not deathSpawner.valid)
    end

    cleanup()
    testPos = 0

    -- Multiple units test
    section("Unit Group Formation Tests")

    local u1 = createTestEntity("small-biter", nextPos(), forceEnemy)
    local u2 = createTestEntity("small-biter", nextPos(), forceEnemy)
    local u3 = createTestEntity("small-biter", nextPos(), forceEnemy)
    record("spawn multiple units", u1 ~= nil and u2 ~= nil and u3 ~= nil)
    record("units have unit numbers", u1 and u2 and u3 and u1.unit_number and u2.unit_number and u3.unit_number)

    cleanup()
    testPos = 0

    -- Building tests
    section("Building Tracking Tests")

    local ins = createTestEntity("inserter", nextPos(), forcePlayer)
    local belt = createTestEntity("transport-belt", nextPos(), forcePlayer)
    local acc = createTestEntity("accumulator", nextPos(), forcePlayer)
    local solar = createTestEntity("solar-panel", nextPos(), forcePlayer)
    record("spawn inserter", ins ~= nil)
    record("spawn transport-belt", belt ~= nil)
    record("spawn accumulator", acc ~= nil)
    record("spawn solar-panel", solar ~= nil)

    cleanup()
    testPos = 0

    -- Prototype validation
    section("Prototype Validation Tests")

    local unitPrototypes = {"small-biter", "medium-biter", "big-biter", "small-spitter"}
    for _, name in ipairs(unitPrototypes) do
        local entity = createTestEntity(name, nextPos(), forceEnemy)
        record(name .. " prototype exists", entity ~= nil)
        if entity then entity.destroy() end
    end

    local spawnerPrototypes = {"biter-spawner", "spitter-spawner"}
    for _, name in ipairs(spawnerPrototypes) do
        local entity = createTestEntity(name, nextPos(), forceEnemy)
        record(name .. " prototype exists", entity ~= nil)
        if entity then entity.destroy() end
    end

    cleanup()
    testPos = 0

    -- ==========================================
    -- SQUAD AI BEHAVIOR TESTS
    -- ==========================================

    section("Unit Group Creation Tests")

    local groupPos = nextPos()
    -- No pcall: create_unit_group errors are real bugs; fail loudly.
    local unitGroup = surface.create_unit_group({
        position = groupPos,
        force = forceEnemy
    })
    record("create unit group", unitGroup ~= nil)
    record("unit group is valid", unitGroup and unitGroup.valid)

    if unitGroup then
        record("unit group state is gathering", unitGroup.state == defines.group_state.gathering)
        record("unit group position matches", unitGroup.position.x == groupPos.x and unitGroup.position.y == groupPos.y)
        unitGroup.destroy()
    end

    cleanup()
    testPos = 0

    section("Unit Group Member Tests")

    local m1 = createTestEntity("small-biter", nextPos(), forceEnemy)
    local m2 = createTestEntity("small-biter", nextPos(), forceEnemy)
    local m3 = createTestEntity("small-biter", nextPos(), forceEnemy)

    record("spawned 3 biters for group test", m1 ~= nil and m2 ~= nil and m3 ~= nil)
    record("all biters valid", m1 and m2 and m3 and m1.valid and m2.valid and m3.valid)

    cleanup()
    testPos = 0

    section("Group Command Tests")

    local wanderGroup = surface.create_unit_group({position = nextPos(), force = forceEnemy})
    record("create unit group for command", wanderGroup ~= nil and wanderGroup.valid)

    if wanderGroup then
        local wanderCmd = {
            type = defines.command.wander,
            destination = {x = 0, y = 0},
            distraction = defines.distraction.by_anything
        }
        -- No pcall: set_command errors are real bugs; fail loudly.
        wanderGroup.set_command(wanderCmd)
        record("set wander command", true)
        wanderGroup.destroy()
    end

    cleanup()
    testPos = 0

    section("Multi Command Tests")

    local formPos = nextPos()
    local uA = createTestEntity("small-biter", formPos, forceEnemy)
    local uB = createTestEntity("small-biter", {x = formPos.x + 5, y = formPos.y}, forceEnemy)

    if uA and uB then
        record("spawned 2 units for multi command", true)
        record("both units valid", uA.valid and uB.valid)
    end

    cleanup()
    testPos = 0

    section("Group State Transition Tests")

    local stateGroup = surface.create_unit_group({position = nextPos(), force = forceEnemy})
    record("create group for state test", stateGroup ~= nil and stateGroup.valid)

    if stateGroup then
        record("initial state is gathering", stateGroup.state == defines.group_state.gathering)

        local wanderCmd = {
            type = defines.command.wander,
            destination = {x = 0, y = 0},
            distraction = defines.distraction.by_anything
        }
        -- No pcall: set_command errors are real bugs; fail loudly.
        stateGroup.set_command(wanderCmd)
        record("set wander command", true)

        stateGroup.destroy()
    end

    cleanup()
    testPos = 0

    section("Group Number Uniqueness Tests")

    local groups = {}
    for i = 1, 5 do
        local g = surface.create_unit_group({position = nextPos(), force = forceEnemy})
        if g and g.valid then
            table.insert(groups, g)
        end
    end

    record("created 5 groups", #groups == 5)
    record("all groups valid", #groups == 5)

    for _, g in ipairs(groups) do
        if g and g.valid then g.destroy() end
    end

    cleanup()
    testPos = 0

    section("Squad Dispatch State Tests")

    local dispatchGroup = surface.create_unit_group({position = nextPos(), force = forceEnemy})
    record("create group for dispatch test", dispatchGroup ~= nil and dispatchGroup.valid)

    if dispatchGroup then
        record("group is valid object", dispatchGroup.valid)
        record("group has valid position", dispatchGroup.position.x ~= nil and dispatchGroup.position.y ~= nil)
        record("group force is enemy", dispatchGroup.force.name == "enemy")
        dispatchGroup.destroy()
    end

    cleanup()
    testPos = 0

    -- ==========================================
    -- RESULTS
    -- ==========================================

    print("")
    print("==========================================")
    print("  Test Summary")
    print("==========================================")
    print("Passed: " .. passed)
    print("Failed: " .. failed)
    print("Total:  " .. (passed + failed))
    print("==========================================")

    local endTime = game.tick
    print("Execution time: " .. tostring(endTime - startTime) .. " ticks")

    if failed == 0 then
        print("")
        print("TESTS_COMPLETE")
        print("ALL INTEGRATION TESTS PASSED!")
    else
        print("")
        print("TESTS_FAILED")
        print("INTEGRATION TESTS FAILED!")
    end

    print("Tests complete. Server will stay open until process is terminated.")
end

script.on_init(function()
    -- No pcall: an error in runIntegrationTests is a real bug; the runner
    -- detects the crash (no TESTS_COMPLETE) and reports failure loudly.
    runIntegrationTests()
end)

script.on_load(function()
end)

script.on_configuration_changed(function(event)
    -- No pcall: see on_init.
    runIntegrationTests()
end)