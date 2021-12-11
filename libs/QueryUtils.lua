if queryUtilsG then
    return queryUtilsG
end
local queryUtils = {}

local constants = require("Constants")

local CHUNK_SIZE = constants.CHUNK_SIZE

function queryUtils.setPositionInQuery(query, position)
    local point = query.position
    point[1] = position.x
    point[2] = position.y
end

function queryUtils.setPositionInCommand(cmd, position)
    local point = cmd.destination
    point[1] = position.x
    point[2] = position.y
end

function queryUtils.setPositionXYInQuery(query, x, y)
    local point = query.position
    point[1] = x
    point[2] = y
end

function queryUtils.setAreaInQuery(query, topLeftPosition, size)
    local area = query.area
    area[1][1] = topLeftPosition.x
    area[1][2] = topLeftPosition.y
    area[2][1] = topLeftPosition.x + size
    area[2][2] = topLeftPosition.y + size
end

function queryUtils.setAreaInQueryChunkSize(query, topLeftPosition)
    local area = query.area
    area[1][1] = topLeftPosition.x
    area[1][2] = topLeftPosition.y
    area[2][1] = topLeftPosition.x + CHUNK_SIZE
    area[2][2] = topLeftPosition.y + CHUNK_SIZE
end

function queryUtils.setPointAreaInQuery(query, position, size)
    local area = query.area
    area[1][1] = position.x - size
    area[1][2] = position.y - size
    area[2][1] = position.x + size
    area[2][2] = position.y + size
end

function queryUtils.setAreaYInQuery(query, y1, y2)
    local area = query.area
    area[1][2] = y1
    area[2][2] = y2
end

function queryUtils.setAreaXInQuery(query, x1, x2)
    local area = query.area
    area[1][1] = x1
    area[2][1] = x2
end

queryUtilsG = queryUtils
return queryUtils
