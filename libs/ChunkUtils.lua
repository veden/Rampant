local chunkUtils = {}

-- imports

local constants = require("Constants")

local baseUtils = require("BaseUtils")

-- constants

local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local BUILDING_PHEROMONES = constants.BUILDING_PHEROMONES
local NEST_BASE = constants.NEST_BASE
local WORM_BASE = constants.WORM_BASE

local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR

local NORTH_SOUTH = constants.NORTH_SOUTH
local EAST_WEST = constants.EAST_WEST

local EAST_WEST_PASSABLE = constants.EAST_WEST_PASSABLE
local NORTH_SOUTH_PASSABLE = constants.NORTH_SOUTH_PASSABLE

local CHUNK_TICK = constants.CHUNK_TICK

local RETREAT_TRIGGERED = constants.RETREAT_TRIGGERED
local RALLY_TRIGGERED = constants.RALLY_TRIGGERED

-- imported functions

local annexNest = baseUtils.annexNest
local createBase = baseUtils.createBase

-- module code

local function checkForDeadendTiles(constantCoordinate, iteratingCoordinate, direction, surface)
    local get_tile = surface.get_tile
    
    for x=iteratingCoordinate, iteratingCoordinate + 31 do
        local tile
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

function chunkUtils.checkChunkPassability(chunk, surface)   
    local x = chunk.pX
    local y = chunk.pY
    
    local passableNorthSouth = false
    local passableEastWest = false
    for xi=x, x + 31 do
        if (not checkForDeadendTiles(xi, y, NORTH_SOUTH, surface)) then
            passableNorthSouth = true
            break
        end
    end
    for yi=y, y + 31 do
        if (not checkForDeadendTiles(yi, x, EAST_WEST, surface)) then
            passableEastWest = true
            break
        end
    end
    
    chunk[EAST_WEST_PASSABLE] = passableEastWest
    chunk[NORTH_SOUTH_PASSABLE] = passableNorthSouth
end

function chunkUtils.scoreChunk(regionMap, chunk, surface, natives, tick)
    local x = chunk.pX
    local y = chunk.pY

    local chunkPosition = {x=x, y=y}
    local areaBoundingBox = {
	chunkPosition,
	{x + 32, y + 32}
    }
    local enemyChunkQuery = {area=areaBoundingBox,
                             force="enemy"}
    local playerChunkQuery = {area=areaBoundingBox,
                              force="player"}
    
    local enemies = surface.find_entities_filtered(enemyChunkQuery)

    local nestsRemoved = 0
    local wormsRemoved = 0
    local bitersRemoved = 0
    
    for i=1, #enemies do
        local entityType = enemies[i].type
	if (entityType == "unit-spawner") then
	    nestsRemoved = nestsRemoved + 3
	elseif (entityType == "turret") then
	    wormsRemoved = wormsRemoved + 2
	elseif (entityType == "unit") then
	    bitersRemoved = bitersRemoved + 1
	end
    end

    entities = surface.find_entities_filtered(playerChunkQuery)

    local safeBuildings = natives.safeBuildings    
    for i=1, #entities do
	local entity = entities[i]
        local entityType = entity.type

	if safeBuildings then
	    if natives.safeEntities[entityType] or natives.safeEntityName[entity.name] then
		entity.destructible = false
	    end
	end
	
	if (nestsRemoved > 0) or (wormsRemoved > 0) or (bitersRemoved > 0) then
	    for i=1, #enemies do
		enemies[i].destroy()
	    end
	    local foundBase = annexNest(natives, chunkPosition) or createBase(regionMap, natives, chunkPosition, surface, tick)
	    if foundBase then
		foundBase.upgradePoints = foundBase.upgradePoints + nestsRemoved + wormsRemoved + bitersRemoved
	    end
	end
    end
    
    local playerObjects = 0
    local entities = surface.find_entities_filtered(playerChunkQuery)
    
    for i=1, #entities do
	local entityScore = BUILDING_PHEROMONES[entities[i].type]
	if (entityScore ~= nil) then
	    playerObjects = playerObjects + entityScore
	end
    end
    
    chunk[PLAYER_BASE_GENERATOR] = playerObjects
end

function chunkUtils.createChunk(topX, topY)
    local chunk = {
	pX = topX,
	pY = topY,
	cX = topX * 0.03125,
	cY = topY * 0.03125
    }
    chunk[MOVEMENT_PHEROMONE] = 0
    chunk[BASE_PHEROMONE] = 0
    chunk[PLAYER_PHEROMONE] = 0
    chunk[PLAYER_BASE_GENERATOR] = 0
    chunk[NORTH_SOUTH_PASSABLE] = false
    chunk[EAST_WEST_PASSABLE] = false
    chunk[CHUNK_TICK] = 0
    chunk[RETREAT_TRIGGERED] = 0
    chunk[RALLY_TRIGGERED] = 0
    chunk[NEST_BASE] = {}
    chunk[WORM_BASE] = {}
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
