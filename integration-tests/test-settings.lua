--[[
    Settings Integration Tests

    Validates that all mod settings are accessible and properly configured.
]]

local SettingsTests = {
    name = "Settings",
    tests = {}
}

function SettingsTests.run()
    print("--- Settings Tests ---")

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

    test("global settings accessible", settings.global ~= nil, "global settings nil")
    test("runtime-per-player settings accessible", settings.player ~= nil, "player settings nil")

    local requiredSettings = {
        "rampant-aggressiveness",
        "rampant-expansion-rate",
        "rampant-enemy-group-size",
        "rampant-peako-num",
        "rampant-peako-window",
        "rampant-retarget-frequency"
    }

    for _, name in ipairs(requiredSettings) do
        test("required setting: " .. name, settings.global[name] ~= nil, name .. " missing")
    end

    local numericSettings = {
        "rampant-aggressiveness", "rampant-expansion-rate", "rampant-enemy-group-size"
    }
    for _, name in ipairs(numericSettings) do
        local s = settings.global[name]
        if s then
            test("numeric type: " .. name, type(s.value) == "number", name .. " not numeric")
        end
    end

    local boolSettings = {
        "rampant-night-attack", "rampant-disable-evasion", "rampant-enable-adaptation"
    }
    for _, name in ipairs(boolSettings) do
        local s = settings.global[name]
        if s then
            test("boolean type: " .. name, type(s.value) == "boolean", name .. " not boolean")
        end
    end

    local settingsWithDefaults = 0
    for name, setting in pairs(settings.global) do
        if string.find(name, "rampant") then
            if setting.default_value ~= nil then
                settingsWithDefaults = settingsWithDefaults + 1
            end
        end
    end
    test("settings have defaults", settingsWithDefaults > 0, "no settings with defaults found")

    local runtimeGlobalFound = false
    for name, setting in pairs(settings.global) do
        if setting.setting_type == "runtime-global" then
            runtimeGlobalFound = true
            break
        end
    end
    test("runtime-global settings exist", runtimeGlobalFound, "no runtime-global settings")

    local hasRuntimePerPlayer = false
    for name, setting in pairs(settings.player) do
        hasRuntimePerPlayer = true
        break
    end
    test("runtime-per-player settings exist", hasRuntimePerPlayer, "no runtime-per-player settings")

    print("Settings Tests: " .. (failed == 0 and "PASSED" or "FAILED") .. " (" .. passed .. " passed, " .. failed .. " failed)")

    return failed == 0
end

return SettingsTests