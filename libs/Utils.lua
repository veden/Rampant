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


if UtilsG then
    return UtilsG
end
local Utils = {}

--

local Constants = require("Constants")

--

local CHUNK_SIZE = Constants.CHUNK_SIZE

--

local mFloor = math.floor
local sSub = string.sub
local sGMatch = string.gmatch

--

function Utils.isRampantSetting(str)
    return sSub(str, 1, #"rampant--") == "rampant--"
end

function Utils.split(str)
    local result = {}
    for i in sGMatch(str, "[a-zA-Z-]+") do
        result[#result+1] = i
    end
    return result
end

function Utils.isMember(str, set)
    for _,s in pairs(set) do
        if str == s then
            return true
        end
    end
    return false
end

function Utils.intersection(set1, set2)
    local result = {}
    for s1 in pairs(set1) do
        for s2 in pairs(set2) do
            if s1 == s2 then
                result[#result+1] = s1
                break
            end
        end
    end
    return result
end

-- Resolve a coordinate without letting a missing field silently keep the
-- table's initialiser. The query/command tables are created as {0,0}, so
-- `point[1] = position.x` with position.x == nil writes nil, Lua keeps the
-- old 0, and the engine is faithfully told (0,0) with no error anywhere.
-- That is how a base could be built at the origin. Accept either position
-- form, and on a genuine miss count it and log the caller once.
-- Cached "position." .. reason names (anomaly path, but it runs per setter).
local POSITION_METRIC = {}
local function positionMetric(reason)
    local name = POSITION_METRIC[reason]
    if not name then
        name = "position." .. reason
        POSITION_METRIC[reason] = name
    end
    return name
end

local function resolveXY(position)
    if not position then return nil, nil, "nil" end
    local x, y = position.x, position.y
    if x == nil then x = position[1] end
    if y == nil then y = position[2] end
    if not x or not y then return nil, nil, "missing" end
    return x, y
end

local function writeXY(point, position)
    local x, y, why = resolveXY(position)
    if not x then
        if MetricsG then MetricsG.inc(positionMetric(why)) end
        log({"", "[POS] " .. why .. " coordinates, keeping previous value: ",
             debug.traceback("", 1):gsub("\n", " | ")})
        return false
    end
    point[1] = x
    point[2] = y
    -- Keep the named form coherent too: the engine accepts either, but a
    -- stale {0,0} is exactly what must never survive a set.
    point.x = x
    point.y = y
    return true
end

function Utils.setPositionInQuery(query, position)
    if not query or not query.position then return end
    writeXY(query.position, position)
end

function Utils.setPositionInCommand(cmd, position)
    if not cmd or not cmd.destination then return end
    writeXY(cmd.destination, position)
end

function Utils.setPositionXYInQuery(query, x, y)
    local point = query.position
    point[1] = x
    point[2] = y
end

function Utils.setAreaInQuery(query, topLeftPosition, size)
    local area = query.area
    area[1][1] = topLeftPosition.x
    area[1][2] = topLeftPosition.y
    area[2][1] = topLeftPosition.x + size
    area[2][2] = topLeftPosition.y + size
end

--- Takes the corner as two numbers: chunk records no longer carry x/y (they are
--- derived from the key), and this avoids building a position table per call.
--- next(t, cursor), tolerating a cursor whose key has been REMOVED since the last
--- call. Lua raises "invalid key to 'next'" for a stale key - the same error that
--- killed an earlier iterator after an id compaction - and every cross-tick cursor
--- in this mod walks a table that removals can shrink (bases, structures, chunk
--- registries). A vanished cursor restarts the pass from the beginning, which is
--- safe because every such pass is idempotent per entry.
function Utils.nextFrom(t, cursor)
    if cursor ~= nil and t[cursor] == nil then
        return next(t, nil)
    end
    return next(t, cursor)
end

-- Chunk key <-> corner coordinates. The key is cx * 2^27 + cy, so the inverse must
-- ROUND (cy can be negative and moves the quotient by up to 0.25), and it works for
-- engine LuaChunkPosition objects too, which carry x/y and no key.
local CHUNK_KEY_X_STEP = 134217728
local CHUNK_SIZE = Constants.CHUNK_SIZE
local mFloor = math.floor

function Utils.chunkKeyToXY(key)
    local cx = mFloor(key / CHUNK_KEY_X_STEP + 0.5)
    return cx * CHUNK_SIZE, (key - cx * CHUNK_KEY_X_STEP) * CHUNK_SIZE
end

function Utils.chunkX(chunk)
    local key = chunk.key
    if not key then return chunk.x or 0 end
    return mFloor(key / CHUNK_KEY_X_STEP + 0.5) * CHUNK_SIZE
end

function Utils.chunkY(chunk)
    local key = chunk.key
    if not key then return chunk.y or 0 end
    local cx = mFloor(key / CHUNK_KEY_X_STEP + 0.5)
    return (key - cx * CHUNK_KEY_X_STEP) * CHUNK_SIZE
end

function Utils.setAreaInQueryChunkSize(query, x, y)
    local area = query.area
    area[1][1] = x
    area[1][2] = y
    area[2][1] = x + CHUNK_SIZE
    area[2][2] = y + CHUNK_SIZE
end

function Utils.setPointAreaInQuery(query, position, size)
    local area = query.area
    area[1][1] = position.x - size
    area[1][2] = position.y - size
    area[2][1] = position.x + size
    area[2][2] = position.y + size
end

function Utils.setAreaYInQuery(query, y1, y2)
    local area = query.area
    area[1][2] = y1
    area[2][2] = y2
end

function Utils.setAreaXInQuery(query, x1, x2)
    local area = query.area
    area[1][1] = x1
    area[2][1] = x2
end

function Utils.validPlayer(player)
    if player and player.valid then
        local char = player.character
        return char and char.valid
    end
    return false
end

function Utils.getTimeStringFromTick(tick)

    local tickToSeconds = tick / 60

    local days = mFloor(tickToSeconds / 86400)
    local hours = mFloor((tickToSeconds % 86400) / 3600)
    local minutes = mFloor((tickToSeconds % 3600) / 60)
    local seconds = mFloor(tickToSeconds % 60)
    return days .. "d " .. hours .. "h " .. minutes .. "m " .. seconds .. "s"
end

UtilsG = Utils
return Utils
