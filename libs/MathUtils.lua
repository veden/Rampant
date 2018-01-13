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

local mFloor = math.floor

local mRandom = math.random

-- module code

function mathUtils.roundToNearest(number, multiple)
    local num = number + (multiple * 0.5)
    return num - (num % multiple)
end

function mathUtils.randomTickEvent(tick, low, high)
    local minutesToTick = mMax(high * mRandom(), low)
    local nextTick = mathUtils.roundToNearest(TICKS_A_MINUTE * minutesToTick, INTERVAL_LOGIC)
    return tick + nextTick
end

--[[
    Used for gaussian random numbers
--]]
local function marsagliaPolarMethod(rg)
    local iid1 
    local iid2 
    local q 
    repeat
	if rg then
	    iid1 = 2 * rg() + -1
	    iid2 = 2 * rg() + -1
	else
	    iid1 = 2 * mRandom() + -1
	    iid2 = 2 * mRandom() + -1
	end
	q = (iid1 * iid1) + (iid2 * iid2)
    until (q ~= 0) and (q < 1)
    local s = mSqrt((-2 * mLog10(q)) / q)
    return iid1 * s
end

function mathUtils.gaussianRandom(mean, std_dev, rg) 
    return mean + (marsagliaPolarMethod(rg) * std_dev)
end

function mathUtils.gaussianRandomRange(mean, std_dev, min, max, rg)
    local q
    repeat
	q = mathUtils.gaussianRandom(mean, std_dev, rg)
    until (q >= min) and (q <= max)
    return q
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

function mathUtils.euclideanDistanceArray(p1, p2)
    local xs = p1[1] - p2[1]
    local ys = p1[2] - p2[2]
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

function mathUtils.distortPosition(position)
    local xDistort = mathUtils.gaussianRandomRange(1, 0.5, 0, 2) - 1
    local yDistort = mathUtils.gaussianRandomRange(1, 0.5, 0, 2) - 1
    position.x = position.x + (xDistort * 48)
    position.y = position.y + (yDistort * 48)
end

return mathUtils
