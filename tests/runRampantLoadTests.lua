-- tests/runRampantLoadTests.lua
-- Tests that Rampant modules can be loaded in simulation context

local sim = require("tests/sim.init")
sim.init()

local passed = 0
local failed = 0

local function test(name, fn)
    local ok, err = pcall(fn)
    if ok then
        print("[PASS] " .. name)
        passed = passed + 1
    else
        print("[FAIL] " .. name .. ": " .. tostring(err))
        failed = failed + 1
    end
end

print("Running Rampant Module Load Tests")
print("==================================")

test("Constants loads", function()
    local Constants = require("Constants")
    assert(Constants.TICKS_A_SECOND == 60, "TICKS_A_SECOND should be 60")
    assert(Constants.AI_MAX_POINTS ~= nil, "AI_MAX_POINTS should exist")
    assert(type(Constants.CHUNK_SIZE) == "number", "CHUNK_SIZE should be a number")
    print("     TICKS_A_SECOND = " .. tostring(Constants.TICKS_A_SECOND))
    print("     AI_MAX_POINTS = " .. tostring(Constants.AI_MAX_POINTS))
    print("     CHUNK_SIZE = " .. tostring(Constants.CHUNK_SIZE))
end)

test("Utils loads", function()
    local Utils = require("libs/Utils")
    assert(Utils ~= nil, "Utils should not be nil")
    assert(type(Utils.validPlayer) == "function", "validPlayer function should exist")
    assert(type(Utils.getTimeStringFromTick) == "function", "getTimeStringFromTick should exist")
end)

test("MathUtils loads", function()
    local MathUtils = require("libs/MathUtils")
    assert(MathUtils ~= nil, "MathUtils should not be nil")
    assert(type(MathUtils.xorRandom) == "function", "xorRandom function should exist")
end)

test("MapUtils loads", function()
    local MapUtils = require("libs/MapUtils")
    assert(MapUtils ~= nil, "MapUtils should not be nil")
    assert(type(MapUtils.getChunkByXY) == "function", "getChunkByXY should exist")
    assert(type(MapUtils.positionToChunkXY) == "function", "positionToChunkXY should exist")
end)

test("UnitUtils loads", function()
    local UnitUtils = require("libs/UnitUtils")
    assert(UnitUtils ~= nil, "UnitUtils should not be nil")
end)

test("Squad loads", function()
    local Squad = require("libs/Squad")
    assert(Squad ~= nil, "Squad should not be nil")
    assert(type(Squad.createSquad) == "function", "createSquad should exist")
    assert(type(Squad.cleanSquads) == "function", "cleanSquads should exist")
end)

test("Processor loads", function()
    local Processor = require("libs/Processor")
    assert(Processor ~= nil, "Processor should not be nil")
end)

test("Upgrade loads", function()
    local Upgrade = require("libs/Upgrade")
    assert(Upgrade ~= nil, "Upgrade should not be nil")
end)

print("==================================")
print("Results: " .. passed .. " passed, " .. failed .. " failed")
print("==================================")

os.exit(failed > 0 and 1 or 0)