local chunkUtils = {}

-- imports

local constants = require("Constants")

local baseUtils = require("BaseUtils")

local baseRegisterUtils = require("BaseRegisterUtils")

-- constants

local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local RESOURCE_PHEROMONE = constants.RESOURCE_PHEROMONE
local BUILDING_PHEROMONES = constants.BUILDING_PHEROMONES
local NEST_BASE = constants.NEST_BASE
local WORM_BASE = constants.WORM_BASE
local NEST_COUNT = constants.NEST_COUNT
local WORM_COUNT = constants.WORM_COUNT

local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR
local RESOURCE_GENERATOR = constants.RESOURCE_GENERATOR

local NORTH_SOUTH = constants.NORTH_SOUTH
local EAST_WEST = constants.EAST_WEST

local EAST_WEST_PASSABLE = constants.EAST_WEST_PASSABLE
local NORTH_SOUTH_PASSABLE = constants.NORTH_SOUTH_PASSABLE

local CHUNK_TICK = constants.CHUNK_TICK

local PATH_RATING = constants.PATH_RATING

local RETREAT_TRIGGERED = constants.RETREAT_TRIGGERED
local RALLY_TRIGGERED = constants.RALLY_TRIGGERED

-- imported functions

local findNearbyBase = baseUtils.findNearbyBase
local createBase = baseUtils.createBase
local addEnemyStructureToChunk = baseRegisterUtils.addEnemyStructureToChunk

-- module code

local function checkForDeadendTiles(constantCoordinate, iteratingCoordinate, direction, get_tile)
    local tile
    
    for x=iteratingCoordinate, iteratingCoordinate + 31 do
        if (direction == NORTH_SOUTH) then
            tile = get_tile(constantCoordinate, x)
        else
            tile = get_tile(x, constantCoordinate)
        end
        if tile.collides_with("player-layer") then
            return true
        end
    end
    return false
end

local function checkForValidTiles(x, y, get_tile)
    local count = 0
    for xi=x,x+31 do
	for yi=y,y+31 do
	    if not get_tile(xi, yi).collides_with("player-layer") then
		count = count + 1
	    end
	end
    end
    return count / 1024
end

function chunkUtils.checkChunkPassability(chunk, surface)   
    local x = chunk.x
    local y = chunk.y
    local get_tile = surface.get_tile
    
    local passableNorthSouth = false
    local passableEastWest = false
    for xi=x, x + 31 do
        if not checkForDeadendTiles(xi, y, NORTH_SOUTH, get_tile) then
            passableNorthSouth = true
            break
        end
    end
    for yi=y, y + 31 do
        if not checkForDeadendTiles(yi, x, EAST_WEST, get_tile) then
            passableEastWest = true
            break
        end
    end
    if passableEastWest or passableNorthSouth then
	if (checkForValidTiles(x, y, get_tile) < 0.6) then
	    passableEastWest = false
	    passableNorthSouth = false
	end
    end

    -- if passableEastWest or passableNorthSouth then
    -- 	chunk[PATH_RATING] = checkForValidTiles(x, y, get_tile)
    -- end
    chunk[PATH_RATING] = checkForValidTiles(x, y, get_tile)
    if (chunk[PATH_RATING] < 0.99) then
	chunk[EAST_WEST_PASSABLE] = false
	chunk[NORTH_SOUTH_PASSABLE] = false
    else
	chunk[EAST_WEST_PASSABLE] = true
	chunk[NORTH_SOUTH_PASSABLE] = true
    end
    -- chunk[EAST_WEST_PASSABLE] = passableEastWest
    -- chunk[NORTH_SOUTH_PASSABLE] = passableNorthSouth
end

function chunkUtils.scoreChunk(regionMap, chunk, surface, natives, tick, tempQuery)
    local useCustomAI = natives.useCustomAI
    
    tempQuery.force = "enemy"
    local enemies = surface.find_entities_filtered(tempQuery)

    local nests = 0
    local worms = 0
    local biters = 0

    for i=1, #enemies do
	local entityType = enemies[i].type
	if (entityType == "unit-spawner") then
	    nests = nests + 1
	elseif (entityType == "turret") then
	    worms = worms + 1
	elseif useCustomAI and (entityType == "unit") then
	    biters = biters + 1
	end
    end

    if useCustomAI then
	if (nests > 0) or (worms > 0) or (biters > 0) then
	    for f=1, #enemies do
		local enemy = enemies[f]
		if (enemy.name ~= "biter-spawner-hive") then
		    enemy.destroy()
		else
		    nests = nests - 1
		end
	    end
	    local foundBase = findNearbyBase(natives, chunk) or createBase(regionMap, natives, chunk, surface, tick)
	    if foundBase then
		foundBase.upgradePoints = foundBase.upgradePoints + (nests*3) + (worms*2) + biters
	    end
	end
    else
	for i=1, #enemies do
	    local enemy = enemies[i]
	    local enemyType = enemy.type
	    if (enemyType == "unit-spawner") or (enemyType == "turret") then
		addEnemyStructureToChunk(chunk, enemy, nil)
	    end
	end
    end

    tempQuery.force = nil
    tempQuery.type = "resource"

    chunk[RESOURCE_GENERATOR] = surface.count_entities_filtered(tempQuery)
    
    tempQuery.type = nil
    tempQuery.force = "player"
    local entities = surface.find_entities_filtered(tempQuery)

    local playerObjects = 0
    local safeBuildings = natives.safeBuildings
    for i=1, #entities do
	local entity = entities[i]
	local entityType = entity.type

	if safeBuildings then
	    if natives.safeEntities[entityType] or natives.safeEntityName[entity.name] then
		entity.destructible = false
	    end
	end

	local entityScore = BUILDING_PHEROMONES[entityType]
	if (entityScore ~= nil) then
	    playerObjects = playerObjects + entityScore
	end
    end

    chunk[PLAYER_BASE_GENERATOR] = playerObjects
end

function chunkUtils.createChunk(topX, topY)
    local chunk = {
	x = topX,
	y = topY,
	cX = topX * 0.03125,
	cY = topY * 0.03125
    }
    chunk[MOVEMENT_PHEROMONE] = 0
    chunk[BASE_PHEROMONE] = 0
    chunk[PLAYER_PHEROMONE] = 0
    chunk[RESOURCE_PHEROMONE] = 0
    chunk[PLAYER_BASE_GENERATOR] = 0
    chunk[NORTH_SOUTH_PASSABLE] = false
    chunk[EAST_WEST_PASSABLE] = false
    chunk[CHUNK_TICK] = 0
    chunk[RETREAT_TRIGGERED] = 0
    chunk[RALLY_TRIGGERED] = 0
    chunk[NEST_BASE] = {}
    chunk[WORM_BASE] = {}
    chunk[NEST_COUNT] = 0
    chunk[WORM_COUNT] = 0
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

return chunkUtils
