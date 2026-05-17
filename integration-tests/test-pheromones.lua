--[[
    Pheromone System Integration Tests

    Validates the pheromone system initialization and structure
    within the Factorio game engine.
]]

local PheromoneTests = {
    name = "Pheromone",
    tests = {}
}

function PheromoneTests.run()
    print("--- Pheromone Tests ---")

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

    if not global.universe or not global.universe.maps then
        print("  Pheromone Tests: SKIPPED (no universe)")
        return true
    end

    local mapCount = 0
    for name, mapData in pairs(global.universe.maps) do
        mapCount = mapCount + 1

        test("playerPheromones exists in " .. name, mapData.playerPheromones ~= nil,
            "playerPheromones nil in " .. name)
        test("enemyPheromones exists in " .. name, mapData.enemyPheromones ~= nil,
            "enemyPheromones nil in " .. name)
        test("playerPheromones is table in " .. name, type(mapData.playerPheromones) == "table",
            "playerPheromones not table in " .. name)
        test("enemyPheromones is table in " .. name, type(mapData.enemyPheromones) == "table",
            "enemyPheromones not table in " .. name)
        test("chunkProperties exists in " .. name, mapData.chunkProperties ~= nil,
            "chunkProperties nil in " .. name)
        test("chunkProperties is table in " .. name, type(mapData.chunkProperties) == "table",
            "chunkProperties not table in " .. name)
        test("enemyMap exists in " .. name, mapData.enemyMap ~= nil,
            "enemyMap nil in " .. name)
        test("resourceMap exists in " .. name, mapData.resourceMap ~= nil,
            "resourceMap nil in " .. name)
        test("playerMap exists in " .. name, mapData.playerMap ~= nil,
            "playerMap nil in " .. name)
    end

    test("maps not empty", mapCount > 0, "no maps found")

    print("Pheromone Tests: " .. (failed == 0 and "PASSED" or "FAILED") .. " (" .. passed .. " passed, " .. failed .. " failed)")

    return failed == 0
end

return PheromoneTests