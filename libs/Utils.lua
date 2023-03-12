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

function Utils.setPositionInQuery(query, position)
    local point = query.position
    point[1] = position.x
    point[2] = position.y
end

function Utils.setPositionInCommand(cmd, position)
    local point = cmd.destination
    point[1] = position.x
    point[2] = position.y
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

function Utils.setAreaInQueryChunkSize(query, topLeftPosition)
    local area = query.area
    area[1][1] = topLeftPosition.x
    area[1][2] = topLeftPosition.y
    area[2][1] = topLeftPosition.x + CHUNK_SIZE
    area[2][2] = topLeftPosition.y + CHUNK_SIZE
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
