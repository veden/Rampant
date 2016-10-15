local mathUtils = {}

-- imports

local constants = require("Constants")

-- constants

local TICKS_A_MINUTE = constants.TICKS_A_MINUTE

local INTERVAL_LOGIC = constants.INTERVAL_LOGIC 

-- imported functions

local mMax = math.max

function mathUtils.roundToNearest(number, multiple)
    local num = number + (multiple * 0.5)
    return num - (num % multiple)
end

function mathUtils.randomTickEvent(tick, low, high)
    local minutesToTick = mMax(high * math.random(), low)
    local nextTick = mathUtils.roundToNearest(TICKS_A_MINUTE * minutesToTick, INTERVAL_LOGIC)
    return tick + nextTick
end

return mathUtils
