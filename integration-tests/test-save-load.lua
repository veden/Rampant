--[[
    Save/Load Integration Tests

    Tests state serialization and deserialization within
    the Factorio game engine.
]]

local SaveLoadTests = {
    name = "SaveLoad",
    tests = {}
}

function SaveLoadTests.run()
    print("--- Save/Load Tests ---")

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

    test("universe serializable", pcall(function()
        return textutils.serialize(global.universe)
    end), "universe not serializable")

    local serialized = textutils.serialize(global.universe)
    test("serialized is string", type(serialized) == "string", "serialized not string")
    test("serialized not empty", #serialized > 0, "serialized string empty")

    local function testDeserialize(ser)
        return pcall(function()
            return textutils.deserialize(ser)
        end)
    end

    test("universe deserializable", testDeserialize(serialized), "universe not deserializable")

    local restored = textutils.deserialize(serialized)
    test("restored has maps", restored.maps ~= nil, "restored missing maps")
    test("restored has bases", restored.bases ~= nil, "restored missing bases")
    test("restored has squads", restored.squads ~= nil, "restored missing squads")

    local copy = util.deepcopy(global.universe)
    test("deepcopy works", copy ~= nil, "deepcopy failed")
    test("deepcopy has maps", copy.maps ~= nil, "deepcopy missing maps")

    local size = util.table_size(global.universe.maps)
    test("table_size works", type(size) == "number", "table_size not number")
    test("table_size >= 0", size >= 0, "table_size negative")

    print("Save/Load Tests: " .. (failed == 0 and "PASSED" or "FAILED") .. " (" .. passed .. " passed, " .. failed .. " failed)")

    return failed == 0
end

return SaveLoadTests