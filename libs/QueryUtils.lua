if queryUtilsG then
    return queryUtilsG
end
local queryUtils = {}

function queryUtils.setPositionInQuery(query, position)
    local point = query.position
    point[1] = position.x
    point[2] = position.y
end

function queryUtils.setAreaInQuery(query, topLeftPosition, size)
    local area = query.area
    area[1][1] = topLeftPosition.x
    area[1][2] = topLeftPosition.y
    area[2][1] = topLeftPosition.x + size
    area[2][2] = topLeftPosition.y + size
end

queryUtilsG = queryUtils
return queryUtils
