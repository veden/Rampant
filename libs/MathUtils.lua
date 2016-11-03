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

function mathUtils.roundToNearest(number, multiple)
    local num = number + (multiple * 0.5)
    return num - (num % multiple)
end

function mathUtils.randomTickEvent(tick, low, high)
    local minutesToTick = mMax(high * math.random(), low)
    local nextTick = mathUtils.roundToNearest(TICKS_A_MINUTE * minutesToTick, INTERVAL_LOGIC)
    return tick + nextTick
end

--[[
    Used for gaussian random numbers
--]]
local function marsagliaPolarMethod()
    local iid1 
    local iid2 
    local q 
    repeat
	iid1 = 2 * math.random() + -1
	iid2 = 2 * math.random() + -1
	q = (iid1 * iid1) + (iid2 * iid2)
    until (q ~= 0) and (q < 1)
    local s = mSqrt((-2 * mLog10(q)) / q)
    return iid1 * s
end

function mathUtils.gaussianRandom(mean, std_dev) 
    return mean + (marsagliaPolarMethod() * std_dev)
end

function mathUtils.gaussianRandomRange(mean, std_dev, min, max)
    local q
    repeat
	q = mathUtils.gaussianRandom(mean, std_dev)
    until (q >= min) and (q <= max)
    return q
end

return mathUtils
