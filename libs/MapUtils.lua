local mapUtils = {}

-- imports

local constants = require("Constants")

-- constants

local BUILDING_PHEROMONES = constants.BUILDING_PHEROMONES
local DEFENSE_PHEROMONES = constants.DEFENSE_PHEROMONES

local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR
local PLAYER_DEFENSE_GENERATOR = constants.PLAYER_DEFENSE_GENERATOR
local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR

local ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT = constants.ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT

local NORTH_SOUTH_PASSABLE = constants.NORTH_SOUTH_PASSABLE
local EAST_WEST_PASSABLE = constants.EAST_WEST_PASSABLE

local HALF_CHUNK_SIZE = constants.HALF_CHUNK_SIZE
local CHUNK_SIZE = constants.CHUNK_SIZE

-- imported functions

local mFloor = math.floor

-- module code

function mapUtils.getChunkByPosition(regionMap, x, y)
    local chunkX = regionMap[mFloor(x * 0.03125)]
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
        
        local leftTopChunkX = mFloor((center.x + topXOffset) * 0.03125)
        local leftTopChunkY = mFloor((center.y + topYOffset) * 0.03125)
        
        -- used to force things on chunk boundary to not spill over 0.0001
        local rightTopChunkX = mFloor((center.x + bottomXOffset - 0.0001) * 0.03125)
        local rightTopChunkY = leftTopChunkY
        
        -- used to force things on chunk boundary to not spill over 0.0001
        local leftBottomChunkX = leftTopChunkX
        local leftBottomChunkY = mFloor((center.y + bottomYOffset - 0.0001) * 0.03125)
            
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

function mapUtils.addRemoveObject(regionMap, entity, natives, addObject)
    local leftTop, rightTop, leftBottom, rightBottom
    local entityValue
    local pheromoneType
    if (BUILDING_PHEROMONES[entity.type] ~= nil) then
        entityValue = BUILDING_PHEROMONES[entity.type]
        pheromoneType = PLAYER_BASE_GENERATOR
    elseif (DEFENSE_PHEROMONES[entity.type] ~= nil) then
        entityValue = DEFENSE_PHEROMONES[entity.type]
        pheromoneType = PLAYER_DEFENSE_GENERATOR
    elseif (entity.type == "unit-spawner") and (entity.force.name == "enemy") then
        entityValue = ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT
        pheromoneType = ENEMY_BASE_GENERATOR
    end
    if (entityValue ~= nil) then
        leftTop, rightTop, leftBottom, rightBottom = mapUtils.getEntityOverlapChunks(regionMap, entity)
        if not addObject then
            entityValue = -entityValue
        elseif (pheromoneType ~= ENEMY_BASE_GENERATOR) then
            natives.points = natives.points + entityValue
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
function mapUtils.getNeighborChunks(regionMap, chunkX, chunkY)   
    local neighbors = {1,2,3,4,5,6,7,8}
    local xChunks = regionMap[chunkX-1]
    if (xChunks ~= nil) then
        neighbors[1] = xChunks[chunkY-1]
        neighbors[4] = xChunks[chunkY]
        neighbors[6] = xChunks[chunkY+1]
    else
        neighbors[1] = nil
        neighbors[4] = nil
        neighbors[6] = nil
    end
    
    xChunks = regionMap[chunkX+1]
    if (xChunks ~= nil) then
        neighbors[3] = xChunks[chunkY-1]
        neighbors[5] = xChunks[chunkY]
        neighbors[8] = xChunks[chunkY+1]
    else
        neighbors[3] = nil
        neighbors[5] = nil
        neighbors[8] = nil
    end
    
    xChunks = regionMap[chunkX]
    if (xChunks ~= nil) then
        neighbors[2] = xChunks[chunkY-1]
        neighbors[7] = xChunks[chunkY+1]
    else
        neighbors[2] = nil
        neighbors[7] = nil
    end
    return neighbors
end

--[[
      1
      |
    2- -3
      |
      4 
]]--
function mapUtils.getCardinalChunksWithDirection(regionMap, chunkX, chunkY)   
    local neighbors = {{d=1},{d=2},{d=3},{d=4}}
    local xChunks = regionMap[chunkX]
    if (xChunks ~= nil) then
        neighbors[1].c = xChunks[chunkY-1]
        neighbors[4].c = xChunks[chunkY+1]
    end
    
    xChunks = regionMap[chunkX-1]
    if (xChunks ~= nil) then
        neighbors[2].c = xChunks[chunkY]
    end
    
    xChunks = regionMap[chunkX+1]
    if (xChunks ~= nil) then
        neighbors[3].c = xChunks[chunkY]
    end
    return neighbors
end

function mapUtils.getCardinalChunks(regionMap, chunkX, chunkY)   
    local neighbors = {1,2,3,4}
    local xChunks = regionMap[chunkX]
    if (xChunks ~= nil) then
        neighbors[1] = xChunks[chunkY-1]
        neighbors[4] = xChunks[chunkY+1]
    else
        neighbors[1] = nil
        neighbors[4] = nil
    end
    
    xChunks = regionMap[chunkX-1]
    if (xChunks ~= nil) then
        neighbors[2] = xChunks[chunkY]
    else
        neighbors[2] = nil
    end
    
    xChunks = regionMap[chunkX+1]
    if (xChunks ~= nil) then
        neighbors[3] = xChunks[chunkY]
    else
        neighbors[3] = nil
    end
    return neighbors
end

function mapUtils.euclideanDistanceNamed(p1, p2)
    local xs = p1.x - p2.x
    local ys = p1.y - p2.y
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

function mapUtils.euclideanDistanceArray(p1, p2)
    local xs = p1[1] - p2[1]
    local ys = p1[2] - p2[2]
    return ((xs * xs) + (ys * ys)) ^ 0.5
end

--[[
      1
      |
    2- -3
      |
      4 
]]--
function mapUtils.canMoveChunkDirectionCardinal(direction, startChunk, endChunk)
    local canMove = false
    if ((direction == 1) or (direction == 4)) and startChunk[NORTH_SOUTH_PASSABLE] and endChunk[NORTH_SOUTH_PASSABLE] then
        canMove = true
    elseif ((direction == 2) or (direction == 3)) and startChunk[EAST_WEST_PASSABLE] and endChunk[EAST_WEST_PASSABLE] then
        canMove = true
    end
    return canMove
end

function mapUtils.positionDirectionToChunkCornerCardinal(direction, chunk)
    local position = {x=0,y=0}
    if (direction == 1) then
        position.x = chunk.pX + HALF_CHUNK_SIZE
        position.y = chunk.pY 
    elseif (direction == 2) then
        position.x = chunk.pX 
        position.y = chunk.pY + HALF_CHUNK_SIZE
    elseif (direction == 3) then
        position.x = chunk.pX + CHUNK_SIZE
        position.y = chunk.pY + HALF_CHUNK_SIZE
    elseif (direction == 4) then
        position.x = chunk.pX + HALF_CHUNK_SIZE
        position.y = chunk.pY + CHUNK_SIZE
    end
    return position
end

function mapUtils.positionDirectionToChunkCorner(direction, chunk)
    local position = {x=0, y=0}
    if (direction == 1) then
        position.x = chunk.pX  
        position.y = chunk.pY 
    elseif (direction == 2) then
        position.x = chunk.pX + HALF_CHUNK_SIZE
        position.y = chunk.pY 
    elseif (direction == 3) then
        position.x = chunk.pX + CHUNK_SIZE
        position.y = chunk.pY
    elseif (direction == 4) then
        position.x = chunk.pX
        position.y = chunk.pY + HALF_CHUNK_SIZE
    elseif (direction == 5) then
        position.x = chunk.pX + CHUNK_SIZE
        position.y = chunk.pY + HALF_CHUNK_SIZE
    elseif (direction == 6) then
        position.x = chunk.pX 
        position.y = chunk.pY + CHUNK_SIZE
    elseif (direction == 7) then
        position.x = chunk.pX + HALF_CHUNK_SIZE
        position.y = chunk.pY + CHUNK_SIZE
    elseif (direction == 8) then
        position.x = chunk.pX + CHUNK_SIZE
        position.y = chunk.pY + CHUNK_SIZE
    end
    return position
end

return mapUtils