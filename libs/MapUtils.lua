local mapUtils = {}

local constants = require("Constants")

local mFloor = math.floor
local mAbs = math.abs

function mapUtils.getChunkByPosition(regionMap, x, y)
    local cX = mFloor(x * 0.03125)
    local chunkX = regionMap[cX]
    if (chunkX ~= nil) then
        return chunkX[mFloor(y * 0.03125)]
    end
    return nil
end

function mapUtils.getChunkByIndex(regionMap, x, y)
    local chunkX = regionMap[x]
    if (chunkX ~= nil) then
        return chunkX[y]
    end
    return nil
end

function mapUtils.positionToChunkOffset(position)
    return mFloor(position.x * 0.03125), mFloor(position.y * 0.03125)
end

function mapUtils.getEntityOverlapChunks(regionMap, entity)
    local mathFloor = mFloor
    local mathAbs = mAbs
    
    local boundingBox = entity.prototype.selection_box;
    
    local leftTopChunk
    local rightTopChunk
    local leftBottomChunk
    local rightBottomChunk
    
    if (boundingBox ~= nil) then
        local center = entity.position
        local topXOffset
        local topYOffset
        
        local bottomXOffset
        local bottomYOffset
        
        if (entity.direction == defines.direction.east) then
            topXOffset = boundingBox.left_top.y
            topYOffset = boundingBox.left_top.x
            bottomXOffset = boundingBox.right_bottom.y
            bottomYOffset = boundingBox.right_bottom.x
        else
            topXOffset = boundingBox.left_top.x
            topYOffset = boundingBox.left_top.y
            bottomXOffset = boundingBox.right_bottom.x
            bottomYOffset = boundingBox.right_bottom.y
        end
        
        local leftTopChunkX = mathFloor((center.x + topXOffset) * 0.03125)
        local leftTopChunkY = mathFloor((center.y + topYOffset) * 0.03125)
        
        -- used to force things on chunk boundary to not spill over 0.0001
        local rightTopChunkX = mathFloor((center.x + bottomXOffset - 0.0001) * 0.03125)
        local rightTopChunkY = leftTopChunkY
        
        -- used to force things on chunk boundary to not spill over 0.0001
        local leftBottomChunkX = leftTopChunkX
        local leftBottomChunkY = mathFloor((center.y + bottomYOffset - 0.0001) * 0.03125)
            
        local rightBottomChunkX = rightTopChunkX 
        local rightBottomChunkY = leftBottomChunkY
        
        leftTopChunk = mapUtils.getChunkByIndex(regionMap, leftTopChunkX, leftTopChunkY)
        if (leftTopChunkX ~= rightTopChunkX) then
            rightTopChunk = mapUtils.getChunkByIndex(regionMap, rightTopChunkX, rightTopChunkY)
        end
        if (leftTopChunkY ~= leftBottomChunkY) then
            leftBottomChunk = mapUtils.getChunkByIndex(regionMap, leftBottomChunkX, leftBottomChunkY)
        end
        if (leftTopChunkX ~= rightBottomChunkX) and (leftTopChunkY ~= rightBottomChunkY) then
            rightBottomChunk = mapUtils.getChunkByIndex(regionMap, rightBottomChunkX, rightBottomChunkY)
        end
    end
    return leftTopChunk, rightTopChunk, leftBottomChunk, rightBottomChunk
end

function mapUtils.addRemoveObject(regionMap, entity, addObject)
    local leftTop, rightTop, leftBottom, rightBottom
    local entityValue
    local pheromoneType
    if (constants.buildingPheromones[entity.type] ~= nil) then
        entityValue = constants.buildingPheromones[entity.type]
        pheromoneType = constants.PLAYER_BASE_GENERATOR
    elseif (constants.defensePheromones[entity.type] ~= nil) then
        entityValue = constants.defensePheromones[entity.type]
        pheromoneType = constants.PLAYER_DEFENSE_GENERATOR
    elseif (entity.type == "unit-spawner") and (entity.force.name == "enemy") then
        entityValue = constants.ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT
        pheromoneType = constants.ENEMY_BASE_GENERATOR
    end
    if (entityValue ~= nil) then
        leftTop, rightTop, leftBottom, rightBottom = mapUtils.getEntityOverlapChunks(regionMap, entity)
        if not addObject then
            entityValue = -entityValue
        end
        if (leftTop ~= nil) then
            leftTop[pheromoneType] = leftTop[pheromoneType] + entityValue
        end
        if (rightTop ~= nil) then
            rightTop[pheromoneType] = rightTop[pheromoneType] + entityValue
        end
        if (leftBottom ~= nil) then
            leftBottom[pheromoneType] = leftBottom[pheromoneType] + entityValue
        end
        if (rightBottom ~= nil) then
            rightBottom[pheromoneType] = rightBottom[pheromoneType] + entityValue
        end
    end
end

--[[
    1 2 3
     \|/
    4- -5
     /|\
    6 7 8
]]--
function mapUtils.getNeighborChunks(regionMap, chunkX, chunkY, neighbors)   
    local xChunks = regionMap[chunkX-1]
    if (xChunks ~= nil) then
        neighbors[1] = xChunks[chunkY-1]
        neighbors[4] = xChunks[chunkY]
        neighbors[6] = xChunks[chunkY+1]
    end
    
    xChunks = regionMap[chunkX+1]
    if (xChunks ~= nil) then
        neighbors[3] = xChunks[chunkY-1]
        neighbors[5] = xChunks[chunkY]
        neighbors[8] = xChunks[chunkY+1]
    end
    
    xChunks = regionMap[chunkX]
    if (xChunks ~= nil) then
        neighbors[2] = xChunks[chunkY-1]
        neighbors[7] = xChunks[chunkY+1]
    end
end

--[[
      1
      |
    2- -3
      |
      4 
]]--
function mapUtils.getCardinalChunks(regionMap, chunkX, chunkY, neighbors)   
    local xChunks = regionMap[chunkX]
    if (xChunks ~= nil) then
        neighbors[1] = xChunks[chunkY-1]
        neighbors[4] = xChunks[chunkY+1]
    end
    
    xChunks = regionMap[chunkX-1]
    if (xChunks ~= nil) then
        neighbors[2] = xChunks[chunkY]
    end
    
    xChunks = regionMap[chunkX+1]
    if (xChunks ~= nil) then
        neighbors[3] = xChunks[chunkY]
    end
    

end

return mapUtils