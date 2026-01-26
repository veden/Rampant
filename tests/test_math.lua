-- Unit tests for MathUtils module
-- These tests require no game mocking

local TestCollection = require("tests/framework")

local MathUtils = require("libs/MathUtils")

local my_collection = TestCollection:new()

-- Test euclideanDistancePoints basic
my_collection:add('euclideanDistancePoints_basic', function()
    local result = MathUtils.euclideanDistancePoints(0, 0, 3, 4)
    assert(result == 5, "Expected 3-4-5 triangle, got " .. tostring(result))
end)

-- Test euclideanDistancePoints with negative coordinates
my_collection:add('euclideanDistancePoints_negative', function()
    local result = MathUtils.euclideanDistancePoints(-3, -4, 0, 0)
    assert(result == 5, "Expected 5 for negative coordinates, got " .. tostring(result))
end)

-- Test euclideanDistancePoints same point
my_collection:add('euclideanDistancePoints_same_point', function()
    local result = MathUtils.euclideanDistancePoints(10, 10, 10, 10)
    assert(result == 0, "Expected 0 for same point, got " .. tostring(result))
end)

-- Test euclideanDistancePoints horizontal
my_collection:add('euclideanDistancePoints_horizontal', function()
    local result = MathUtils.euclideanDistancePoints(0, 0, 5, 0)
    assert(result == 5, "Expected 5 for horizontal distance, got " .. tostring(result))
end)

-- Test euclideanDistancePoints vertical
my_collection:add('euclideanDistancePoints_vertical', function()
    local result = MathUtils.euclideanDistancePoints(0, 0, 0, 7)
    assert(result == 7, "Expected 7 for vertical distance, got " .. tostring(result))
end)

-- Test euclideanDistanceNamed
my_collection:add('euclideanDistanceNamed_basic', function()
    local result = MathUtils.euclideanDistanceNamed({ x = 0, y = 0 }, { x = 3, y = 4 })
    assert(result == 5, "Expected 5 for named position, got " .. tostring(result))
end)

-- Test gaussianRandomRangeRG bounds - mean 0
my_collection:add('gaussianRandomRangeRG_mean_0', function()
    local rg = MathUtils.xorRandom(12345)
    local count = 100
    local min = -1
    local max = 1
    local allInBounds = true

    for i = 1, count do
        local val = MathUtils.gaussianRandomRangeRG(0, 0.2, min, max, rg)
        if val < min or val > max then
            allInBounds = false
            break
        end
    end

    assert(allInBounds, "Expected all values in bounds [-1, 1]")
end)

-- Test gaussianRandomRangeRG handles min >= max (returns min)
my_collection:add('gaussianRandomRangeRG_min_max_swapped', function()
    local rg = MathUtils.xorRandom(12345)
    local result = MathUtils.gaussianRandomRangeRG(5, 1, 10, 5, rg)
    assert(result == 10, "Expected min when min >= max, got " .. tostring(result))
end)

-- Test xorRandom produces values in [0, 1)
my_collection:add('xorRandom_in_range', function()
    local rg = MathUtils.xorRandom(54321)
    local allInRange = true

    for i = 1, 100 do
        local val = rg()
        if val < 0 or val >= 1 then
            allInRange = false
            break
        end
    end

    assert(allInRange, "Expected all values in range [0, 1)")
end)

-- Test xorRandom deterministic
my_collection:add('xorRandom_deterministic', function()
    local rg1 = MathUtils.xorRandom(11111)
    local rg2 = MathUtils.xorRandom(11111)

    local same = true
    for i = 1, 10 do
        if rg1() ~= rg2() then
            same = false
            break
        end
    end

    assert(same, "Expected deterministic sequence for same seed")
end)

-- Test roundToFloor
my_collection:add('roundToFloor_basic', function()
    local result = MathUtils.roundToFloor(37, 32)
    assert(result == 32, "Expected 32, got " .. tostring(result))
end)

-- Test roundToFloor negative
my_collection:add('roundToFloor_negative', function()
    local result = MathUtils.roundToFloor(-37, 32)
    assert(result == -64, "Expected -64, got " .. tostring(result))
end)

-- Test roundToNearest
my_collection:add('roundToNearest_basic', function()
    local result = MathUtils.roundToNearest(17, 10)
    assert(result == 20, "Expected 20, got " .. tostring(result))
end)

-- Test roundToNearest exact midpoint
my_collection:add('roundToNearest_midpoint', function()
    local result = MathUtils.roundToNearest(15, 10)
    assert(result == 20, "Expected 20 at midpoint (rounds up), got " .. tostring(result))
end)

-- Test linearInterpolation
my_collection:add('linearInterpolation_basic', function()
    local result = MathUtils.linearInterpolation(0.5, 0, 100)
    assert(result == 50, "Expected 50 at 50%, got " .. tostring(result))
end)

-- Test linearInterpolation at bounds
my_collection:add('linearInterpolation_bounds', function()
    assert(MathUtils.linearInterpolation(0, 0, 100) == 0, "Expected 0 at 0%")
    assert(MathUtils.linearInterpolation(1, 0, 100) == 100, "Expected 100 at 100%")
end)

-- Test distort function produces value in range
my_collection:add('distort_in_range', function()
    local rg = MathUtils.xorRandom(99999)
    local result = MathUtils.distort(rg, 100, 0.1, 50, 150)
    assert(result >= 50 and result <= 150, "Expected value in range [50, 150], got " .. tostring(result))
end)

return my_collection