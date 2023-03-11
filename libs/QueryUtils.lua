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


if QueryUtilsG then
    return QueryUtilsG
end
local QueryUtils = {}

local Constants = require("Constants")

local CHUNK_SIZE = Constants.CHUNK_SIZE

function QueryUtils.setPositionInQuery(query, position)
    local point = query.position
    point[1] = position.x
    point[2] = position.y
end

function QueryUtils.setPositionInCommand(cmd, position)
    local point = cmd.destination
    point[1] = position.x
    point[2] = position.y
end

function QueryUtils.setPositionXYInQuery(query, x, y)
    local point = query.position
    point[1] = x
    point[2] = y
end

function QueryUtils.setAreaInQuery(query, topLeftPosition, size)
    local area = query.area
    area[1][1] = topLeftPosition.x
    area[1][2] = topLeftPosition.y
    area[2][1] = topLeftPosition.x + size
    area[2][2] = topLeftPosition.y + size
end

function QueryUtils.setAreaInQueryChunkSize(query, topLeftPosition)
    local area = query.area
    area[1][1] = topLeftPosition.x
    area[1][2] = topLeftPosition.y
    area[2][1] = topLeftPosition.x + CHUNK_SIZE
    area[2][2] = topLeftPosition.y + CHUNK_SIZE
end

function QueryUtils.setPointAreaInQuery(query, position, size)
    local area = query.area
    area[1][1] = position.x - size
    area[1][2] = position.y - size
    area[2][1] = position.x + size
    area[2][2] = position.y + size
end

function QueryUtils.setAreaYInQuery(query, y1, y2)
    local area = query.area
    area[1][2] = y1
    area[2][2] = y2
end

function QueryUtils.setAreaXInQuery(query, x1, x2)
    local area = query.area
    area[1][1] = x1
    area[2][1] = x2
end

QueryUtilsG = QueryUtils
return QueryUtils
