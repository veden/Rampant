if mathUtilsG then
    return mathUtilsG
end
local mathUtils = {}

-- imports

local constants = require("Constants")

-- constants

local TICKS_A_MINUTE = constants.TICKS_A_MINUTE

local INTERVAL_LOGIC = constants.INTERVAL_LOGIC

-- imported functions

local mMax = math.max
local mSqrt = math.sqrt
local mLog10 = math.log10

local mRandom = math.random
local mFloor = math.floor
local mAbs = math.abs

-- module code

function mathUtils.roundToFloor(number, multiple)
    return mFloor(number / multiple) * multiple
end

function mathUtils.roundToNearest(number, multiple)
    local num = number + (multiple * 0.5)
    return num - (num % multiple)
end

function mathUtils.randomTickEvent(tick, low, high)
    local range = high - low
    local minutesToTick = (range * mRandom()) + low
    -- local nextTick = mathUtils.roundToNearest(, INTERVAL_LOGIC)
    return tick + (TICKS_A_MINUTE * minutesToTick)
end

function mathUtils.linearInterpolation(percent, min, max)
    return ((max - min) * percent) + min
end

function mathUtils.xorRandom(state)
    local xor = bit32.bxor
    local lshift = bit32.lshift
    local rshift = bit32.rshift

    state = state + 21594771

    return function()
	state = xor(state, lshift(state, 13))
	state = xor(state, rshift(state, 17))
	state = xor(state, lshift(state, 5))
	state = state % 2147483647
	return state * 4.65661287525e-10
    end
end

function mathUtils.linearInterpolation(percent, min, max)
    return ((max - min) * percent) + min
end

--[[
    Used for gaussian random numbers
--]]
function mathUtils.gaussianRandom(mean, std_dev)
    -- marsagliaPolarMethod
    local iid1
    local iid2
    local q
    repeat
	iid1 = 2 * mRandom() + -1
	iid2 = 2 * mRandom() + -1
	q = (iid1 * iid1) + (iid2 * iid2)
    until (q ~= 0) and (q < 1)
    local s = mSqrt((-2 * mLog10(q)) / q)
    local v = iid1 * s

    return mean + (v * std_dev)
end

function mathUtils.gaussianRandomRange(mean, std_dev, min, max)
    if (min == max) then
	return min
    end
    local r
    repeat
	local iid1
	local iid2
	local q
	repeat
	    iid1 = 2 * mRandom() + -1
	    iid2 = 2 * mRandom() + -1
	    q = (iid1 * iid1) + (iid2 * iid2)
	until (q ~= 0) and (q < 1)
	local s = mSqrt((-2 * mLog10(q)) / q)
	local v = iid1 * s

	r = mean + (v * std_dev)
    until (r >= min) and (r <= max)
    return r
end

function mathUtils.gaussianRandomRG(mean, std_dev, rg)
    -- marsagliaPolarMethod
    local iid1
    local iid2
    local q
    repeat
	iid1 = 2 * rg() + -1
	iid2 = 2 * rg() + -1
	q = (iid1 * iid1) + (iid2 * iid2)
    until (q ~= 0) and (q < 1)
    local s = mSqrt((-2 * mLog10(q)) / q)
    local v = iid1 * s

    return mean + (v * std_dev)
end

function mathUtils.gaussianRandomRangeRG(mean, std_dev, min, max, rg)
    local r
    if (min == max) then
	return min
    end
    repeat
	local iid1
	local iid2
	local q
	repeat
	    iid1 = 2 * rg() + -1
	    iid2 = 2 * rg() + -1
	    q = (iid1 * iid1) + (iid2 * iid2)
	until (q ~= 0) and (q < 1)
	local s = mSqrt((-2 * mLog10(q)) / q)
	local v = iid1 * s
	r = mean + (v * std_dev)
    until (r >= min) and (r <= max)
    return r
end

function mathUtils.euclideanDistanceNamed(p1, p2)
    local xs = p1.x - p2.x
    local ys = p1.y - p2.y
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

function mathUtils.euclideanDistancePoints(x1, y1, x2, y2)
    local xs = x1 - x2
    local ys = y1 - y2
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

function mathUtils.mahattenDistancePoints(x1, y1, x2, y2)
    local xs = x1 - x2
    local ys = y1 - y2
    return mAbs(xs + ys)
end

function mathUtils.euclideanDistanceArray(p1, p2)
    local xs = p1[1] - p2[1]
    local ys = p1[2] - p2[2]
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

function mathUtils.distortPosition(position)
    local xDistort = mathUtils.gaussianRandomRange(1, 0.5, 0, 2) - 1
    local yDistort = mathUtils.gaussianRandomRange(1, 0.5, 0, 2) - 1
    position.x = position.x + (xDistort * 16)
    position.y = position.y + (yDistort * 16)
    return position
end

mathUtilsG = mathUtils
return mathUtils
