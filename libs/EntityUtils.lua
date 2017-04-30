local entityUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")

-- constants

local BUILDING_PHEROMONES = constants.BUILDING_PHEROMONES

local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR
local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR

local ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT = constants.ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT

local CHUNK_SIZE = constants.CHUNK_SIZE

-- imported functions

local getChunkByIndex = mapUtils.getChunkByIndex

local mFloor = math.floor

-- module code

local function getEntityOverlapChunks(regionMap, entity)
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
        
        leftTopChunk = getChunkByIndex(regionMap, leftTopChunkX, leftTopChunkY)
        if (leftTopChunkX ~= rightTopChunkX) then
            rightTopChunk = getChunkByIndex(regionMap, rightTopChunkX, rightTopChunkY)
        end
        if (leftTopChunkY ~= leftBottomChunkY) then
            leftBottomChunk = getChunkByIndex(regionMap, leftBottomChunkX, leftBottomChunkY)
        end
        if (leftTopChunkX ~= rightBottomChunkX) and (leftTopChunkY ~= rightBottomChunkY) then
            rightBottomChunk = getChunkByIndex(regionMap, rightBottomChunkX, rightBottomChunkY)
        end
    end
    return leftTopChunk, rightTopChunk, leftBottomChunk, rightBottomChunk
end

function entityUtils.addRemoveEntity(regionMap, entity, natives, addObject, creditNatives)
    local leftTop, rightTop, leftBottom, rightBottom
    local entityValue
    local pheromoneType
    if (BUILDING_PHEROMONES[entity.type] ~= nil) and (entity.force.name == "player") then
        entityValue = BUILDING_PHEROMONES[entity.type]
        pheromoneType = PLAYER_BASE_GENERATOR
    elseif (entity.type == "unit-spawner") and (entity.force.name == "enemy") then
        entityValue = ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT
        pheromoneType = ENEMY_BASE_GENERATOR
    elseif (entity.type == "turret") and (entity.force.name == "enemy") then
        entityValue = 1
        pheromoneType = ENEMY_BASE_GENERATOR
    end
    if (entityValue ~= nil) then
        leftTop, rightTop, leftBottom, rightBottom = getEntityOverlapChunks(regionMap, entity)
        if not addObject then
	    if creditNatives and (pheromoneType ~= ENEMY_BASE_GENERATOR) then
		natives.points = natives.points + entityValue
	    end
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


function entityUtils.makeImmortalEntity(surface, entity)
    local repairPosition = entity.position
    local repairName = entity.name
    local repairForce = entity.force
    local repairDirection = entity.direction
    entity.destroy()
    local entityRepaired = surface.create_entity({position=repairPosition,
						  name=repairName,
						  direction=repairDirection,
						  force=repairForce})
    entityRepaired.destructible = false
end

return entityUtils
