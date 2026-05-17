--[[
    AI Behavior Integration Tests

    Tests the AI system initialization and structure within
    the Factorio game engine.
]]

local AIBehaviorTests = {
    name = "AIBehavior",
    tests = {}
}

function AIBehaviorTests.run()
    print("--- AI Behavior Tests ---")

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

    test("universe initialized", global.universe ~= nil, "universe nil")
    test("maps initialized", global.universe.maps ~= nil, "maps nil")
    test("squads initialized", global.universe.squads ~= nil, "squads nil")
    test("bases initialized", global.universe.bases ~= nil, "bases nil")

    test("maps is table", type(global.universe.maps) == "table", "maps not table")
    test("squads is table", type(global.universe.squads) == "table", "squads not table")
    test("bases is table", type(global.universe.bases) == "table", "bases not table")

    test("activeMaps exists", global.universe.activeMaps ~= nil, "activeMaps nil")
    test("chunkToPassScan exists", global.universe.chunkToPassScan ~= nil, "chunkToPassScan nil")

    test("temperamentRateModifier exists", global.universe.temperamentRateModifier ~= nil, "temperamentRateModifier nil")
    test("raidAIToggle exists", global.universe.raidAIToggle ~= nil, "raidAIToggle nil")

    local squadCount = 0
    for _ in pairs(global.universe.squads) do squadCount = squadCount + 1 end
    test("squad count is number", type(squadCount) == "number", "squad count not number")

    local baseCount = 0
    for _ in pairs(global.universe.bases) do baseCount = baseCount + 1 end
    test("base count is number", type(baseCount) == "number", "base count not number")

    print("AI Behavior Tests: " .. (failed == 0 and "PASSED" or "FAILED") .. " (" .. passed .. " passed, " .. failed .. " failed)")

    return failed == 0
end

return AIBehaviorTests