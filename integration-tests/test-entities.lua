--[[
    Entity Integration Tests

    Tests entity creation, manipulation, and searching within
    the Factorio game engine.
]]

local EntityTests = {
    name = "Entity",
    tests = {}
}

function EntityTests.run()
    print("--- Entity Tests ---")

    local passed = 0
    local failed = 0

    local function test(name, condition, errorMsg)
        print("  Testing: " .. name)
        if not condition then
            failed = failed + 1
            print("    FAILED: " .. (errorMsg or "assertion failed"))
            return false
        end
        passed = passed + 1
        return true
    end

    local surface = game.surfaces[1]
    test("surface available", surface ~= nil, "no surface available")

    if not surface then
        print("Entity Tests: SKIPPED (no surface)")
        return true
    end

    local spawner = surface.create_entity({
        name = "rampant-ai-spawner",
        position = {x = 0, y = 0},
        force = "enemy"
    })
    test("create spawner", spawner ~= nil, "spawner creation failed")
    if spawner then spawner.destroy() end

    local unit = surface.create_entity({
        name = "rampant-acidic-biter",
        position = {x = 10, y = 10},
        force = "enemy"
    })
    test("create unit", unit ~= nil, "unit creation failed")
    if unit then unit.destroy() end

    local turret = surface.create_entity({
        name = "rampant-acid-turret",
        position = {x = 20, y = 20},
        force = "enemy"
    })
    test("create turret", turret ~= nil, "turret creation failed")
    if turret then turret.destroy() end

    local entity = surface.create_entity({
        name = "rampant-ai-spawner",
        position = {x = 100, y = 100},
        force = "enemy"
    })
    test("position validation", entity ~= nil and entity.position.x == 100 and entity.position.y == 100,
        "position validation failed")
    if entity then entity.destroy() end

    local enemyEntity = surface.create_entity({
        name = "rampant-ai-spawner",
        position = {x = 30, y = 30},
        force = "enemy"
    })
    test("force assignment", enemyEntity ~= nil and enemyEntity.force.name == "enemy",
        "force assignment failed")
    if enemyEntity then enemyEntity.destroy() end

    local spawner1 = surface.create_entity({
        name = "rampant-ai-spawner",
        position = {x = 40, y = 40},
        force = "enemy"
    })
    local found = surface.find_entities_filtered({
        name = "rampant-ai-spawner",
        radius = 50,
        position = {x = 40, y = 40}
    })
    test("find by name", #found > 0, "find by name failed")
    if spawner1 then spawner1.destroy() end

    local enemyUnit = surface.create_entity({
        name = "rampant-acidic-biter",
        position = {x = 50, y = 50},
        force = "enemy"
    })
    local foundByForce = surface.find_entities_filtered({
        force = "enemy",
        radius = 50,
        position = {x = 50, y = 50}
    })
    test("find by force", #foundByForce > 0, "find by force failed")
    if enemyUnit then enemyUnit.destroy() end

    local tempEntity = surface.create_entity({
        name = "rampant-ai-spawner",
        position = {x = 60, y = 60},
        force = "enemy"
    })
    test("entity not nil before destroy", tempEntity ~= nil, "entity creation for destroy test failed")
    if tempEntity then
        tempEntity.destroy()
        local afterDestroy = surface.find_entities_filtered({
            name = "rampant-ai-spawner",
            position = {x = 60, y = 60}
        })
        test("entity destroyed", #afterDestroy == 0, "entity not destroyed")
    end

    print("Entity Tests: " .. (failed == 0 and "PASSED" or "FAILED") .. " (" .. passed .. " passed, " .. failed .. " failed)")

    return failed == 0
end

return EntityTests