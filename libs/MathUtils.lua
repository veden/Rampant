-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


if mathUtilsG then
    return mathUtilsG
end
local mathUtils = {}

-- imports

local constants = require("Constants")

-- constants

local TICKS_A_MINUTE = constants.TICKS_A_MINUTE

-- imported functions

local mSqrt = math.sqrt
local mLog10 = math.log10

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

function mathUtils.randomTickEvent(rg, tick, low, high)
    local range = high - low
    local minutesToTick = (range * rg()) + low
    return tick + mathUtils.roundToNearest(TICKS_A_MINUTE * minutesToTick, 1)
end

function mathUtils.distort(xorRandom, num, stdDev, min, max)
    local amin = min or num * 0.70
    local amax = max or num * 1.30
    local sd = stdDev or 0.17
    if (num < 0) then
        local t = amin
        amin = amax
        amax = t
    end
    return mathUtils.roundToNearest(mathUtils.gaussianRandomRangeRG(num, num * sd, amin, amax, xorRandom), 0.01)
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

--[[
    Used for gaussian random numbers
--]]
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
    if (min >= max) then
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

function mathUtils.manhattenDistancePoints(x1, y1, x2, y2)
    return mAbs((x1 - x2) + (y1 - y2))
end

function mathUtils.euclideanDistanceArray(p1, p2)
    local xs = p1[1] - p2[1]
    local ys = p1[2] - p2[2]
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

function mathUtils.distortPosition(rg, position, size)
    local xDistort = mathUtils.gaussianRandomRangeRG(1, 0.5, 0, 2, rg) - 1
    local yDistort = mathUtils.gaussianRandomRangeRG(1, 0.5, 0, 2, rg) - 1
    position.x = position.x + (xDistort * size)
    position.y = position.y + (yDistort * size)
    return position
end

mathUtilsG = mathUtils
return mathUtils
