--[[
    Data Stage Integration Tests

    Validates that all prototypes are created correctly during
    Factorio's data stage.
]]

local DataStageTests = {
    name = "DataStage",
    tests = {}
}

function DataStageTests.run()
    print("--- Data Stage Tests ---")

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

    test("prototype categories exist", data.raw["unit"] ~= nil, "unit category missing")
    test("prototype categories: unit-spawner", data.raw["unit-spawner"] ~= nil, "unit-spawner missing")
    test("prototype categories: turret", data.raw["turret"] ~= nil, "turret missing")
    test("prototype categories: projectile", data.raw["projectile"] ~= nil, "projectile missing")
    test("prototype categories: corpse", data.raw["corpse"] ~= nil, "corpse missing")
    test("prototype categories: item", data.raw["item"] ~= nil, "item missing")
    test("prototype categories: recipe", data.raw["recipe"] ~= nil, "recipe missing")

    local rampantPrototypes = 0
    for cat, tbl in pairs(data.raw) do
        for name, _ in pairs(tbl) do
            if string.find(name, "rampant") or string.find(name, "Rampant") then
                rampantPrototypes = rampantPrototypes + 1
            end
        end
    end
    test("Rampant prototypes exist", rampantPrototypes > 0, "no Rampant prototypes found")

    test("acid splash corpse", data.raw.corpse["rampant-acid-splash-corpse"] ~= nil, "acid splash corpse missing")
    test("AI spawner", data.raw["unit-spawner"]["rampant-ai-spawner"] ~= nil, "AI spawner missing")
    test("spawner variants", data.raw["unit-spawner"]["rampant-ai-spawner-acidic"] ~= nil, "acid spawner missing")

    test("unit: acidic biter", data.raw.unit["rampant-acidic-biter"] ~= nil, "acidic biter missing")
    test("unit: laser biter", data.raw.unit["rampant-laser-biter"] ~= nil, "laser biter missing")
    test("unit: fire biter", data.raw.unit["rampant-fire-biter"] ~= nil, "fire biter missing")
    test("unit: inferno biter", data.raw.unit["rampant-inferno-biter"] ~= nil, "inferno biter missing")
    test("unit: poison biter", data.raw.unit["rampant-poison-biter"] ~= nil, "poison biter missing")

    test("turret: acid turret", data.raw.turret["rampant-acid-turret"] ~= nil, "acid turret missing")
    test("turret: laser turret", data.raw.turret["rampant-laser-turret"] ~= nil, "laser turret missing")

    test("projectile: acid", data.raw.projectile["rampant-acid-projectile"] ~= nil, "acid projectile missing")
    test("projectile: fire", data.raw.projectile["rampant-fire-projectile"] ~= nil, "fire projectile missing")

    print("Data Stage Tests: " .. (failed == 0 and "PASSED" or "FAILED") .. " (" .. passed .. " passed, " .. failed .. " failed)")

    return failed == 0
end

return DataStageTests