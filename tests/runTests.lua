package.path = "libs/?.lua;tests/?.lua;?.lua;" .. package.path

-- Polyfills for Factorio's extended Lua runtime (must be before any production requires)
require("polyfills")
-- Set up __Rampant__ path prefix mapping
local _Rampant_prefix = ""

-- Capture settings from data:extend() calls
local capturedSettings = {}

-- Mock Factorio data global - CAPTURE what gets extended
data = {
    extend = function(self, x) 
        -- Capture each setting definition
        for _, setting in ipairs(x) do
            if setting.setting_type then
                local settingType = setting.setting_type
                if settingType == "runtime-global" then 
                    settingType = "global" 
                end
                
                if not capturedSettings[settingType] then 
                    capturedSettings[settingType] = {} 
                end
                capturedSettings[settingType][setting.name] = { 
                    value = setting.default_value 
                }
            end
        end
    end,
}

-- Load actual settings.lua - this will populate capturedSettings
require("settings")

-- Now create the settings global from captured data
settings = capturedSettings

package.preload["__Rampant__/Constants"] = function()
    return require("Constants")
end
package.preload["__Rampant__/libs/MathUtils"] = function()
    return require("libs/MathUtils")
end
package.preload["__Rampant__/libs/Utils"] = function()
    return require("libs/Utils")
end
package.preload["__Rampant__/libs/MapUtils"] = function()
    return require("libs/MapUtils")
end
package.preload["__Rampant__/tests"] = function()
    return {}
end
package.preload["__Rampant__/libs/Processor"] = function()
    return require("libs/Processor")
end
package.preload["__Rampant__/libs/Squad"] = function()
    return require("libs/Squad")
end
package.preload["__Rampant__/libs/UnitUtils"] = function()
    return require("libs/UnitUtils")
end
package.preload["__Rampant__/libs/Upgrade"] = function()
    return require("libs/Upgrade")
end

---@diagnostic disable

function refreshingRequire(path)
    package.loaded[path] = nil
    return require(path)
end

local function runIt()
    data = {
        ["extend"] = function(self, x) end,
    }

    -- Settings already loaded at top level from mock_settings.lua
    -- No need to transform again

    local c = ((2 ^ 32) - 1)
    bit32 = {}
    bit32["bxor"] = function(x, y) return (x ~ y) & c end

    bit32["lshift"] = function(x, y) return (x << y) & c end

    bit32["rshift"] = function(x, y) return (x >> y) & c end

    defines = {}
    defines["wire_type"] = {
        ["red"] = "redWire",
        ["green"] = "greenWire",
    }
    
    -- Mock remote with interfaces table
    remote = {
        interfaces = {}
    }

    defines["command"] = {
        ["group"] = "group",
        ["wander"] = "wander",
        ["build_base"] = "build_base",
        ["attack_area"] = "attack_area",
        ["go_to_location"] = "go_to_location",
        ["compound"] = "compound",
        ["flee"] = "flee",
        ["stop"] = "stop",
    }

    defines["compound_command"] = {
        ["return_last"] = "return_last",
    }

    defines["distraction"] = {
        ["none"] = "none",
        ["by_enemy"] = "by_enemy",
        ["by_anything"] = "by_anything",
    }

    defines["group_state"] = {
        ["attacking_distraction"] = "attacking_distraction",
        ["attacking_target"] = "attacking_target",
        ["gathering"] = "gathering",
    }

    defines["events"] = {
        ["on_tick"] = "on_tick",
    }

    defines.events["on_chunk_deleted"] = "on_chunk_deleted"
    defines.events["on_surface_deleted"] = "on_surface_deleted"
    defines.events["on_surface_cleared"] = "on_surface_cleared"
    defines.events["on_surface_created"] = "on_surface_created"

    defines.events["on_runtime_mod_setting_changed"] = "on_runtime_mod_setting_changed"

    defines.events["on_resource_depleted"] = "on_resource_depleted"
    defines.events["on_player_built_tile"] = "on_player_built_tile"
    defines.events["on_robot_built_tile"] = "on_robot_built_tile"
    defines.events["script_raised_set_tiles"] = "script_raised_set_tiles"
    defines.events["on_player_used_capsule"] = "on_player_used_capsule"
    defines.events["on_script_trigger_effect"] = "on_script_trigger_effect"
    defines.events["on_pre_robot_exploded_cliff"] = "on_pre_robot_exploded_cliff"

    defines.events["on_player_mined_entity"] = "on_player_mined_entity"
    defines.events["on_robot_mined_entity"] = "on_robot_mined_entity"
    defines.events["on_built_entity"] = "on_built_entity"
    defines.events["on_robot_built_entity"] = "on_robot_built_entity"
    defines.events["script_raised_built"] = "script_raised_built"
    defines.events["script_raised_revive"] = "script_raised_revive"
    defines.events["on_biter_base_built"] = "on_biter_base_built"

    defines.events["on_rocket_launched"] = "on_rocket_launched"
    defines.events["on_entity_died"] = "on_entity_died"
    defines.events["script_raised_destroy}"] = "script_raised_destroy}"

    defines.events["on_chunk_generated"] = "on_chunk_generated"
    defines.events["on_unit_group_created"] = "on_unit_group_created"
    defines.events["on_force_created"] = "on_force_created"
    defines.events["on_forces_merged"] = "on_forces_merged"
    defines.events["on_unit_group_finished_gathering"] = "on_unit_group_finished_gathering"

    defines.events["on_build_base_arrived"] = "on_build_base_arrived"

    local function printTable(tbl, indent)
        indent = indent or ""

        for k, v in pairs(tbl) do
            if type(v) == "table" then
                print(indent .. "  " .. tostring(k) .. ":")
                printTable(v, indent .. "  " .. "  ")
            else
                print(indent .. "  " .. tostring(k) .. ":", tostring(v))
            end
        end
    end

    local function printTableRoot(tbl, printTrace)
        if type(tbl) == "table" then
            printTable(tbl, "")
        else
            print(tbl)
        end
    end

    -- Define a metatable for our table so we can intercept indexing operations
    local function mkMockMetatable(printTrace)
        local data = {}
        local calls = {}
        local infinite = {}
        local consts = {}
        local invokers = {}

        local reset = function(self)
            data = {}
            calls = {}
            infinite = {}
            consts = {}
            return self
        end

        local setInvoker = function(self, key, fn)
            invokers[key] = fn
            return self
        end

        local setReturnFnValue = function(self, key, times, arg1)
            local entries = data[key]
            if not data[key] then entries = {} end
            local args = arg1
            if times == -1 then infinite[key] = args end
            for _ = 1, times do
                entries[#entries + 1] = args
            end
            return self
        end

        local setReturnConstValue = function(self, key, x)
            consts[key] = x
            return self
        end

        local fns = {}
        fns["reset"] = reset
        fns["setReturnFnValue"] = setReturnFnValue
        fns["setReturnConstValue"] = setReturnConstValue
        fns["setInvoker"] = setInvoker

        local mt = {
            __index = function(self, key)
                if key == "_data" then return data end
                if key == "_calls" then return calls end
                if key == "_infinite" then return infinite end
                if key == "_invoker" then return invokers end
                if fns[key] then return fns[key] end
                if consts[key] then
                    if printTrace then
                        print("Const:", key)
                        printTableRoot(consts[key], printTrace)
                        print("--------------")
                    end
                    return consts[key]
                end

                -- Create a varadic function that logs its arguments and returns a value from the table
                local fn = function(...)
                    local result
                    local entries = data[key]
                    if infinite[key] then
                        result = infinite[key]
                    elseif invokers[key] then
                        result = invokers[key](...)
                    elseif not entries then
                        result = nil
                    else
                        local lastEntryIndex = #entries
                        local entry = entries[lastEntryIndex]
                        entries[lastEntryIndex] = nil
                        if lastEntryIndex == 1 then data[key] = nil end
                        result = entry
                    end
                    if printTrace or not result then
                        print("mock:", self.hname, "Function call:", key)
                        print("args:")
                        local args = { ... }
                        if #args > 0 then printTableRoot(args, printTrace) end
                        print("Result:")
                        printTableRoot(result, printTrace)
                        print("--------------")
                    end
                    return result
                end

                return fn
            end,

            __tostring = function(self) return self.hname end,
        }

        return mt
    end

    MockObject = function(trace, hname, x)
        local r = x or {}
        r.hname = hname
        setmetatable(r, mkMockMetatable(trace))
        return r
    end

    local mathUtils = refreshingRequire("libs/MathUtils")

    script = MockObject(false, "script")

    remote = MockObject(false, "remote", {
        interfaces = {}
    })

    commands = MockObject(false, "commands")

    game = MockObject(false, "game")
        :setInvoker("create_random_generator", function(seed) return mathUtils.xorRandom(seed) end)
        :setReturnConstValue("tick", 0)
        :setReturnConstValue("active_mods", {})

    local generateSurfaceChunks = function(chunkHeight, chunkWidth, tileXY)
        local chunkMap = {}
        local acc = {}
        local multipler = 1
        if tileXY then multipler = 32 end
        chunkHeight = chunkHeight * multipler
        chunkWidth = chunkWidth * multipler

        for hI = -chunkHeight, chunkHeight, multipler do
            for wI = -chunkWidth, chunkWidth, multipler do
                if not chunkMap[hI] then chunkMap[hI] = {} end
                local chunk = {
                    x = hI,
                    y = wI,
                }
                chunkMap[hI][wI] = chunk
                acc[#acc + 1] = chunk
            end
        end
        return acc, chunkMap
    end

    local generateIterator = function(tbl)
        local i = 1
        return function()
            if i <= #tbl then
                local e = tbl[i]
                i = i + 1
                return e
            end
            return nil
        end
    end

    local nauvisSurfaceChunks, nauvisChunkMap = generateSurfaceChunks(20, 20)

    game:setReturnConstValue("surfaces", {
        ["nauvis"] = MockObject(false, "nauvisSurface")
            :setReturnConstValue("name", "nauvis")
            :setInvoker("is_chunk_generated", function(position)
                if not nauvisChunkMap[position.x] then return false end
                return nauvisChunkMap[position.x][position.y] ~= nil
            end)
            :setReturnFnValue("get_chunks", -1, generateIterator(nauvisSurfaceChunks)),
    })

    game:setReturnConstValue("forces", {
        neutral = MockObject(false, "neutralForce"),
        enemy = MockObject(false, "enemyForce"),
        player = MockObject(false, "playerForce"),
    })

    game:setReturnConstValue("map_settings", {
        pollution = {
            min_to_diffuse = 15,
        },
        enemy_expansion = {
            enabled = true,
            max_expansion_distance = 7,
            min_expansion_distance = 1,
            min_expansion_cooldown = 4 * 3600,
            max_expansion_cooldown = 60 * 3600,
        },
        unit_group = {
            max_unit_group_size = 150,
        },
    })

    global = {}

    local Control = refreshingRequire("control")
    local TestCollection = refreshingRequire("tests/framework")

    print("=== Running Headless Tests ===")

    -- Load and run all test collections
local test_collections = {}

    -- Load test collections
    -- Core tests to keep
    local test_math = require("tests.test_math")
    table.insert(test_collections, { name = "MathUtils", collection = test_math })

    local test_utils = require("tests.test_utils")
    table.insert(test_collections, { name = "Utils", collection = test_utils })

    -- ACO Tests (NEW - our focus)
    local test_aco_core = require("tests.test_aco_core")
    table.insert(test_collections, { name = "ACO Core", collection = test_aco_core })

    local test_aco_trails = require("tests.test_aco_trails")
    table.insert(test_collections, { name = "ACO Trails", collection = test_aco_trails })

    local test_pheromone_pure = require("tests.test_pheromone_pure")
    table.insert(test_collections, { name = "Pheromone Pure", collection = test_pheromone_pure })

    local test_chunk_property_pure = require("tests.test_chunk_property_pure")
    table.insert(test_collections, { name = "ChunkProperty Pure", collection = test_chunk_property_pure })

    local test_processor_pure = require("tests.test_processor_pure")
    table.insert(test_collections, { name = "Processor Pure", collection = test_processor_pure })

    local test_integration_pheromone = require("tests.test_integration_pheromone_flow")
    table.insert(test_collections, { name = "Integration Pheromone Flow", collection = test_integration_pheromone })

    local test_squad_pure = require("tests.test_squad_pure")
    table.insert(test_collections, { name = "Squad Pure", collection = test_squad_pure })

    local test_baseutils_pure = require("tests.test_baseutils_pure")
    table.insert(test_collections, { name = "BaseUtils Pure", collection = test_baseutils_pure })

    -- TODO: Commented out - will be replaced/refactored for ACO system
    -- local test_map = require("tests.test_map")
    -- table.insert(test_collections, { name = "MapUtils", collection = test_map })

    -- local test_base = require("tests.test_base")
    -- table.insert(test_collections, { name = "Base", collection = test_base })

    -- local test_pheromones = require("tests.test_pheromones")
    -- table.insert(test_collections, { name = "Pheromones", collection = test_pheromones })

    -- Run all tests
    local total_passed = 0
    local total_failed = 0

    for _, tc in ipairs(test_collections) do
        print("\n--- " .. tc.name .. " Tests ---")
        tc.collection:run_all()

        local failures = tc.collection:get_failures()
        local count = 0
        for _ in pairs(failures) do count = count + 1 end

        if count == 0 then
            total_passed = total_passed + #tc.collection.tests
            print("All " .. tostring(#tc.collection.tests) .. " tests passed!")
        else
            total_failed = total_failed + count
            print(tostring(count) .. " test(s) failed:")
            for name, err in pairs(failures) do
                print("  FAILED: " .. name)
                print("    Error: " .. tostring(err))
            end
        end
    end

    print("\n=== Test Summary ===")
    print("Total passed: " .. tostring(total_passed))
    print("Total failed: " .. tostring(total_failed))
    print("===================")

    if total_failed == 0 then
        print("All tests passed!")
    else
        print("Some tests failed!")
    end

    -- Optionally run onInit for integration tests
    -- print("\n--- Running onInit ---")
    -- Control.onInit()
    -- print("Completed onInit")
end

runIt()