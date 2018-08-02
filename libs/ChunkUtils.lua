local chunkUtils = {}

-- imports

local stringUtils = require("StringUtils")
local baseUtils = require("BaseUtils")
local constants = require("Constants")
local mapUtils = require("MapUtils")
local chunkPropertyUtils = require("ChunkPropertyUtils")

-- constants

local DEFINES_DIRECTION_EAST = defines.direction.east
local DEFINES_WIRE_TYPE_RED = defines.wire_type.red
local DEFINES_WIRE_TYPE_GREEN = defines.wire_type.green

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE
local BUILDING_PHEROMONES = constants.BUILDING_PHEROMONES

local CHUNK_SIZE = constants.CHUNK_SIZE
local CHUNK_SIZE_DIVIDER = constants.CHUNK_SIZE_DIVIDER

local CHUNK_NORTH_SOUTH = constants.CHUNK_NORTH_SOUTH
local CHUNK_EAST_WEST = constants.CHUNK_EAST_WEST

local CHUNK_ALL_DIRECTIONS = constants.CHUNK_ALL_DIRECTIONS
local CHUNK_IMPASSABLE = constants.CHUNK_IMPASSABLE

local RESOURCE_NORMALIZER = constants.RESOURCE_NORMALIZER

local CHUNK_TICK = constants.CHUNK_TICK

local BASE_ALIGNMENT_DEADZONE = constants.BASE_ALIGNMENT_DEADZONE

local PATH_RATING = constants.PATH_RATING

local PASSABLE = constants.PASSABLE

-- imported functions

local isRampant = stringUtils.isRampant
local setNestCount = chunkPropertyUtils.setNestCount
local setPlayerBaseGenerator = chunkPropertyUtils.setPlayerBaseGenerator
local addPlayerBaseGenerator = chunkPropertyUtils.addPlayerBaseGenerator
local setResourceGenerator = chunkPropertyUtils.setResourceGenerator
local addResourceGenerator = chunkPropertyUtils.addResourceGenerator
local setWormCount = chunkPropertyUtils.setWormCount
local getPlayerBaseGenerator = chunkPropertyUtils.getPlayerBaseGenerator
local getNestCount = chunkPropertyUtils.getNestCount

local findNearbyBase = baseUtils.findNearbyBase
local createBase = baseUtils.createBase

local upgradeEntity = baseUtils.upgradeEntity

local setChunkBase = chunkPropertyUtils.setChunkBase
local getEnemyStructureCount = chunkPropertyUtils.getEnemyStructureCount

local getChunkByXY = mapUtils.getChunkByXY
local getChunkByPosition = mapUtils.getChunkByPosition

local mFloor = math.floor

-- module code

local function fullScan(chunk, can_place_entity, canPlaceQuery)
    local x = chunk.x
    local y = chunk.y
    
    local passableNorthSouth = false
    local passableEastWest = false

    canPlaceQuery.name = "chunk-scanner-ns-rampant"

    local position = canPlaceQuery.position
    position[2] = y
    
    for xi=x, x + 32 do
	position[1] = xi
	if can_place_entity(canPlaceQuery) then
	    passableNorthSouth = true
	    break
	end
    end

    position[1] = x
    canPlaceQuery.name = "chunk-scanner-ew-rampant"
    
    for yi=y, y + 32 do
	position[2] = yi
	if can_place_entity(canPlaceQuery) then
	    passableEastWest = true
	    break
	end
    end
    return passableNorthSouth, passableEastWest
end

local function addEnemyStructureToChunk(map, chunk, entity, base)
    local lookup
    if (entity.type == "unit-spawner") then
	lookup = map.chunkToNests
    elseif (entity.type == "turret") then
	lookup = map.chunkToWorms
    else
	return
    end
    
    local entityCollection = lookup[chunk]
    if not entityCollection then
	lookup[chunk] = 0
    end
    
    lookup[chunk] = lookup[chunk] + 1

    setChunkBase(map, chunk, base)    
end

local function removeEnemyStructureFromChunk(map, chunk, entity)
    local lookup
    if (entity.type == "unit-spawner") then
	lookup = map.chunkToNests
    elseif (entity.type == "turret") then
	lookup = map.chunkToWorms
    else
	return
    end

    if (getEnemyStructureCount(map, chunk) == 0) then
	setChunkBase(map, chunk, nil)
    end
    
    if lookup[chunk] then
	if ((lookup[chunk] - 1) <= 0) then
	    lookup[chunk] = nil
	else
	    lookup[chunk] = lookup[chunk] - 1
	end
    end
end

local function getEntityOverlapChunks(map, entity)
    local boundingBox = entity.prototype.collision_box or entity.prototype.selection_box;
    
    local leftTopChunk = SENTINEL_IMPASSABLE_CHUNK
    local rightTopChunk = SENTINEL_IMPASSABLE_CHUNK
    local leftBottomChunk = SENTINEL_IMPASSABLE_CHUNK
    local rightBottomChunk = SENTINEL_IMPASSABLE_CHUNK
    
    if boundingBox then
        local center = entity.position
        local topXOffset
        local topYOffset
        
        local bottomXOffset
        local bottomYOffset
        
        -- if (entity.direction == DEFINES_DIRECTION_EAST) then
        --     topXOffset = boundingBox.left_top.y
        --     topYOffset = boundingBox.left_top.x
        --     bottomXOffset = boundingBox.right_bottom.y
        --     bottomYOffset = boundingBox.right_bottom.x
        -- else
            topXOffset = boundingBox.left_top.x
            topYOffset = boundingBox.left_top.y
            bottomXOffset = boundingBox.right_bottom.x
            bottomYOffset = boundingBox.right_bottom.y
        -- end
        
        local leftTopChunkX = mFloor((center.x + topXOffset) * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE 
        local leftTopChunkY = mFloor((center.y + topYOffset) * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE
        
        local rightTopChunkX = mFloor((center.x + bottomXOffset) * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE	
        local leftBottomChunkY = mFloor((center.y + bottomYOffset) * CHUNK_SIZE_DIVIDER) * CHUNK_SIZE
	
        leftTopChunk = getChunkByXY(map, leftTopChunkX, leftTopChunkY)
        if (leftTopChunkX ~= rightTopChunkX) then
            rightTopChunk = getChunkByXY(map, rightTopChunkX, leftTopChunkY)
        end
        if (leftTopChunkY ~= leftBottomChunkY) then
            leftBottomChunk = getChunkByXY(map, leftTopChunkX, leftBottomChunkY)
        end
        if (leftTopChunkX ~= rightTopChunkX) and (leftTopChunkY ~= leftBottomChunkY) then
            rightBottomChunk = getChunkByXY(map, rightTopChunkX, leftBottomChunkY)
        end
    end
    return leftTopChunk, rightTopChunk, leftBottomChunk, rightBottomChunk
end

-- external functions

function chunkUtils.calculatePassScore(surface, map)
    local passScore = surface.count_tiles_filtered(map.filteredTilesQuery)
    return 1 - (passScore * 0.0009765625)
end

function chunkUtils.scanChunkPaths(chunk, surface, map)
    local pass = CHUNK_IMPASSABLE
    local passableNorthSouth, passableEastWest = fullScan(chunk,
							  surface.can_place_entity,
							  map.canPlaceQuery)
    
    if passableEastWest and passableNorthSouth then
	pass = CHUNK_ALL_DIRECTIONS
    elseif passableEastWest then
	pass = CHUNK_EAST_WEST
    elseif passableNorthSouth then
	pass = CHUNK_NORTH_SOUTH
    end
    return pass
end

function chunkUtils.scorePlayerBuildings(surface, map, natives)
    local entities = surface.find_entities_filtered(map.filteredEntitiesPlayerQuery)

    local playerObjects = 0
    local safeEntities = natives.safeEntities
    local safeEntityName = natives.safeEntityName
    if natives.safeBuildings then
	for i=1, #entities do
	    local entity = entities[i]
	    local entityType = entity.type

	    if safeEntities[entityType] or safeEntityName[entity.name] then
		entity.destructible = false
	    end

	    playerObjects = playerObjects + (BUILDING_PHEROMONES[entityType] or 0)
	end
    else
	for i=1, #entities do
	    playerObjects = playerObjects + (BUILDING_PHEROMONES[entities[i].type] or 0)
	end
    end

    return playerObjects
end

function chunkUtils.scoreEnemyBuildings(surface, map)
    local nests = surface.find_entities_filtered(map.filteredEntitiesUnitSpawnereQuery)
    local worms = surface.find_entities_filtered(map.filteredEntitiesWormQuery)

    return nests, worms
end

function chunkUtils.initialScan(chunk, natives, surface, map, tick, evolutionFactor, rebuilding)
    local passScore = chunkUtils.calculatePassScore(surface, map)

    if (passScore >= 0.40) then
	local pass = chunkUtils.scanChunkPaths(chunk, surface, map)
	
	local playerObjects = chunkUtils.scorePlayerBuildings(surface, map, natives)

	local nests, worms = chunkUtils.scoreEnemyBuildings(surface, map)

	local resources = surface.count_entities_filtered(map.countResourcesQuery) * RESOURCE_NORMALIZER
	
	if ((playerObjects > 0) or (#nests > 0)) and (pass == CHUNK_IMPASSABLE) then
	    pass = CHUNK_ALL_DIRECTIONS
	end

	if natives.newEnemies and ((#nests > 0) or (#worms > 0)) then
	    local nestCount = 0
	    local wormCount = 0
	    local base = findNearbyBase(map, chunk, natives)
	    if base then
		if (base.alignment ~= BASE_ALIGNMENT_DEADZONE) then
		    setChunkBase(map, chunk, base)
		end
	    else
		base = createBase(map, natives, evolutionFactor, chunk, surface, tick, rebuilding)
	    end
	    local alignment = base.alignment
	    if (#nests > 0) then
		for i = 1, #nests do
		    if rebuilding then
			if not isRampant(nests[i].name) then
			    if upgradeEntity(nests[i], surface, alignment, natives, evolutionFactor) then
				nestCount = nestCount + 1
			    end
			else
			    nestCount = nestCount + 1
			end
		    else
			if upgradeEntity(nests[i], surface, alignment, natives, evolutionFactor) then
			    nestCount = nestCount + 1
			end
		    end
		end
	    end
	    if (#worms > 0) then
		for i = 1, #worms do
		    if rebuilding then
			if not isRampant(worms[i].name) then
			    if upgradeEntity(worms[i], surface, alignment, natives, evolutionFactor) then
				wormCount = wormCount + 1
			    end
			else
			    wormCount = wormCount + 1
			end
		    else
			if upgradeEntity(worms[i], surface, alignment, natives, evolutionFactor) then
			    wormCount = wormCount + 1
			end
		    end
		end
	    end
	    setNestCount(map, chunk, nestCount)
	    setWormCount(map, chunk, wormCount)
	else
	    setNestCount(map, chunk, #nests)
	    setWormCount(map, chunk, #worms)
	end
	
	setPlayerBaseGenerator(map, chunk, playerObjects)
	setResourceGenerator(map, chunk, resources)
	

	chunk[PASSABLE] = pass
	chunk[PATH_RATING] = passScore

	return chunk
    end

    return SENTINEL_IMPASSABLE_CHUNK
end

function chunkUtils.chunkPassScan(chunk, surface, map)
    local passScore = chunkUtils.calculatePassScore(surface, map)

    if (passScore >= 0.40) then
	local pass = chunkUtils.scanChunkPaths(chunk, surface, map)
	
	local playerObjects = getPlayerBaseGenerator(map, chunk)

	local nests = getNestCount(map, chunk)

	if ((playerObjects > 0) or (nests > 0)) and (pass == CHUNK_IMPASSABLE) then
	    pass = CHUNK_ALL_DIRECTIONS
	end

	chunk[PASSABLE] = pass
	chunk[PATH_RATING] = passScore

	return chunk
    end

    return SENTINEL_IMPASSABLE_CHUNK
end

function chunkUtils.analyzeChunk(chunk, natives, surface, map)    
    local playerObjects = chunkUtils.scorePlayerBuildings(surface, map, natives)
    setPlayerBaseGenerator(map, chunk, playerObjects)
    local resources = surface.count_entities_filtered(map.countResourcesQuery) * RESOURCE_NORMALIZER
    setResourceGenerator(map, chunk, resources)
end

function chunkUtils.createChunk(topX, topY)
    local chunk = {
	x = topX,
	y = topY
    }
    chunk[MOVEMENT_PHEROMONE] = 0
    chunk[BASE_PHEROMONE] = 0
    chunk[PLAYER_PHEROMONE] = 0
    chunk[RESOURCE_PHEROMONE] = 0
    chunk[PASSABLE] = 0
    chunk[CHUNK_TICK] = 0
    chunk[PATH_RATING] = 0
    
    return chunk
end

function chunkUtils.colorChunk(x, y, tileType, surface)
    local tiles = {}
    for xi=x+5, x + 27 do
	for yi=y+5, y + 27 do
	    tiles[#tiles+1] = {name=tileType, position={xi, yi}}
	end
    end
    surface.set_tiles(tiles, false)
end

function chunkUtils.entityForPassScan(map, entity)
    local leftTop, rightTop, leftBottom, rightBottom = getEntityOverlapChunks(map, entity)

    if (leftTop ~= SENTINEL_IMPASSABLE_CHUNK) then
	map.chunkToPassScan[leftTop] = true
    end
    if (rightTop ~= SENTINEL_IMPASSABLE_CHUNK) then
	map.chunkToPassScan[rightTop] = true
    end
    if (leftBottom ~= SENTINEL_IMPASSABLE_CHUNK) then
	map.chunkToPassScan[leftBottom] = true
    end
    if (rightBottom ~= SENTINEL_IMPASSABLE_CHUNK) then
	map.chunkToPassScan[rightBottom] = true
    end
end

function chunkUtils.registerEnemyBaseStructure(map, entity, natives, evolutionFactor, surface, tick)    
    local entityType = entity.type
    if ((entityType == "unit-spawner") or (entityType == "turret")) and (entity.force.name == "enemy") then
	local chunk = getChunkByPosition(map, entity.position)
	local base
	if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) and natives.newEnemies then
	    base = findNearbyBase(map, chunk, natives)
	    if not base then
		base = createBase(map, natives, evolutionFactor, chunk, surface, tick)
	    end
	end

	local leftTop, rightTop, leftBottom, rightBottom = getEntityOverlapChunks(map, entity)
	
	if (leftTop ~= SENTINEL_IMPASSABLE_CHUNK) then
	    addEnemyStructureToChunk(map, leftTop, entity, base)
	end
	if (rightTop ~= SENTINEL_IMPASSABLE_CHUNK) then
	    addEnemyStructureToChunk(map, rightTop, entity, base)
	end
	if (leftBottom ~= SENTINEL_IMPASSABLE_CHUNK) then
	    addEnemyStructureToChunk(map, leftBottom, entity, base)
	end
	if (rightBottom ~= SENTINEL_IMPASSABLE_CHUNK) then
	    addEnemyStructureToChunk(map, rightBottom, entity, base)
	end	
    end
    
    return entity
end

function chunkUtils.unregisterEnemyBaseStructure(map, entity)
    local entityType = entity.type
    if ((entityType == "unit-spawner") or (entityType == "turret")) and (entity.force.name == "enemy") then
	local leftTop, rightTop, leftBottom, rightBottom = getEntityOverlapChunks(map, entity)
	
	if (leftTop ~= SENTINEL_IMPASSABLE_CHUNK) then
	    removeEnemyStructureFromChunk(map, leftTop, entity)
	end
	if (rightTop ~= SENTINEL_IMPASSABLE_CHUNK) then
	    removeEnemyStructureFromChunk(map, rightTop, entity)
	end
	if (leftBottom ~= SENTINEL_IMPASSABLE_CHUNK) then
	    removeEnemyStructureFromChunk(map, leftBottom, entity)
	end
	if (rightBottom ~= SENTINEL_IMPASSABLE_CHUNK) then
	    removeEnemyStructureFromChunk(map, rightBottom, entity)
	end
    end
end

function chunkUtils.addRemovePlayerEntity(map, entity, natives, addObject, creditNatives)
    local leftTop, rightTop, leftBottom, rightBottom
    local entityValue
    
    if (BUILDING_PHEROMONES[entity.type] ~= nil) and (entity.force.name ~= "enemy") then
        entityValue = BUILDING_PHEROMONES[entity.type]

        leftTop, rightTop, leftBottom, rightBottom = getEntityOverlapChunks(map, entity)
        if not addObject then
    	    if creditNatives then
    		natives.points = natives.points + entityValue
    	    end
    	    entityValue = -entityValue
    	end
    	if (leftTop ~= SENTINEL_IMPASSABLE_CHUNK) then
	    addPlayerBaseGenerator(map, leftTop, entityValue)
    	end
    	if (rightTop ~= SENTINEL_IMPASSABLE_CHUNK) then
	    addPlayerBaseGenerator(map, rightTop, entityValue)
    	end
    	if (leftBottom ~= SENTINEL_IMPASSABLE_CHUNK) then
	    addPlayerBaseGenerator(map, leftBottom, entityValue)
    	end
    	if (rightBottom ~= SENTINEL_IMPASSABLE_CHUNK) then
	    addPlayerBaseGenerator(map, rightBottom, entityValue)
    	end
    end
    return entity
end

function chunkUtils.unregisterResource(entity, map)
    if entity.prototype.infinite_resource then
	return
    end
    local leftTop, rightTop, leftBottom, rightBottom = getEntityOverlapChunks(map, entity)
    
    if (leftTop ~= SENTINEL_IMPASSABLE_CHUNK) then
	addResourceGenerator(map, leftTop, -RESOURCE_NORMALIZER)
    end
    if (rightTop ~= SENTINEL_IMPASSABLE_CHUNK) then
	addResourceGenerator(map, rightTop, -RESOURCE_NORMALIZER)
    end
    if (leftBottom ~= SENTINEL_IMPASSABLE_CHUNK) then
	addResourceGenerator(map, leftBottom, -RESOURCE_NORMALIZER)
    end
    if (rightBottom ~= SENTINEL_IMPASSABLE_CHUNK) then
	addResourceGenerator(map, rightBottom, -RESOURCE_NORMALIZER)
    end
end

function chunkUtils.makeImmortalEntity(surface, entity)
    local repairPosition = entity.position
    local repairName = entity.name
    local repairForce = entity.force
    local repairDirection = entity.direction

    local wires
    if (entity.type == "electric-pole") then
	wires = entity.neighbours
    end
    entity.destroy()
    local newEntity = surface.create_entity({position=repairPosition,
					     name=repairName,
					     direction=repairDirection,
					     force=repairForce})
    if wires then
	for connectType,neighbourGroup in pairs(wires) do
	    if connectType == "copper" then
		for _,v in pairs(neighbourGroup) do
		    newEntity.connect_neighbour(v);
		end
	    elseif connectType == "red" then
		for _,v in pairs(neighbourGroup) do
		    newEntity.connect_neighbour({wire = DEFINES_WIRE_TYPE_RED, target_entity = v});
		end
	    elseif connectType == "green" then
		for _,v in pairs(neighbourGroup) do
		    newEntity.connect_neighbour({wire = DEFINES_WIRE_TYPE_GREEN, target_entity = v});
		end
	    end
	end
    end

    newEntity.destructible = false
end

return chunkUtils
