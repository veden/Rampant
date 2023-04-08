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


if MathUtilsG then
    return MathUtilsG
end
local MathUtils = {}

-- imports

-- constants

local TICKS_A_MINUTE = 60 * 60

-- imported functions

local mSqrt = math.sqrt
local mLog10 = math.log10

local mFloor = math.floor
local mAbs = math.abs

-- module code

function MathUtils.roundToFloor(number, multiple)
    return mFloor(number / multiple) * multiple
end

function MathUtils.roundToNearest(number, multiple)
    local num = number + (multiple * 0.5)
    return num - (num % multiple)
end

function MathUtils.randomTickEvent(rg, tick, low, high)
    return tick + MathUtils.randomTickDuration(rg, low, high)
end

function MathUtils.randomTickDuration(rg, low, high)
    local range = high - low
    local minutesToTick = (range * rg()) + low
    return MathUtils.roundToNearest(TICKS_A_MINUTE * minutesToTick, 1)
end

function MathUtils.distort(xorRandom, num, stdDev, min, max)
    local amin = min or num * 0.70
    local amax = max or num * 1.30
    local sd = stdDev or 0.17
    if (num < 0) then
        local t = amin
        amin = amax
        amax = t
    end
    return MathUtils.roundToNearest(MathUtils.gaussianRandomRangeRG(num, num * sd, amin, amax, xorRandom), 0.01)
end

function MathUtils.linearInterpolation(percent, min, max)
    return ((max - min) * percent) + min
end

function MathUtils.xorRandom(state)
    local xor = bit32.bxor
    local lshift = bit32.lshift
    local rshift = bit32.rshift

    local seed = state + 32685453

    return function()
        seed = xor(seed, lshift(seed, 13))
        seed = xor(seed, rshift(seed, 17))
        seed = xor(seed, lshift(seed, 5))
        return seed * 2.32830643654e-10 -- 2.32830643654e-10 = 1 / 2^32, 2.32830643708e-10 = 1 / ((2^32)-1)
    end
end

--[[
    Used for gaussian random numbers
--]]
function MathUtils.gaussianRandomRG(mean, std_dev, rg)
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

function MathUtils.gaussianRandomRangeRG(mean, std_dev, min, max, rg)
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

function MathUtils.euclideanDistanceNamed(p1, p2)
    local xs = p1.x - p2.x
    local ys = p1.y - p2.y
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

function MathUtils.euclideanDistancePoints(x1, y1, x2, y2)
    local xs = x1 - x2
    local ys = y1 - y2
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

function MathUtils.manhattenDistancePoints(x1, y1, x2, y2)
    return mAbs((x1 - x2) + (y1 - y2))
end

function MathUtils.euclideanDistanceArray(p1, p2)
    local xs = p1[1] - p2[1]
    local ys = p1[2] - p2[2]
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

function MathUtils.distortPosition(rg, position, size)
    local xDistort = MathUtils.gaussianRandomRangeRG(1, 0.5, 0, 2, rg) - 1
    local yDistort = MathUtils.gaussianRandomRangeRG(1, 0.5, 0, 2, rg) - 1
    position.x = position.x + (xDistort * size)
    position.y = position.y + (yDistort * size)
end

function MathUtils.distortPositionConcentricCircles(rg, position, size, min)
    local xDistort = MathUtils.gaussianRandomRangeRG(1, 0.5, 0, 2, rg) - 1
    local yDistort = MathUtils.gaussianRandomRangeRG(1, 0.5, 0, 2, rg) - 1
    local xModifier = (xDistort * size)
    if xModifier < 0 then
        xModifier = xModifier + -min
    else
        xModifier = xModifier + min
    end
    local yModifier = (yDistort * size)
    if yModifier < 0 then
        yModifier = yModifier + -min
    else
        yModifier = yModifier + min
    end

    position.x = position.x + xModifier
    position.y = position.y + yModifier
end

MathUtilsG = MathUtils
return MathUtils
