local chunkUtils = {}

-- imports

local constants = require("Constants")

-- constants

local BASE_PHEROMONE = constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE
local BUILDING_PHEROMONES = constants.BUILDING_PHEROMONES

local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR
local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR

local NORTH_SOUTH = constants.NORTH_SOUTH
local EAST_WEST = constants.EAST_WEST

local EAST_WEST_PASSABLE = constants.EAST_WEST_PASSABLE
local NORTH_SOUTH_PASSABLE = constants.NORTH_SOUTH_PASSABLE

local ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT = constants.ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT

local CHUNK_TICK = constants.CHUNK_TICK

local RETREAT_TRIGGERED = constants.RETREAT_TRIGGERED

local CHUNK_BASE = constants.CHUNK_BASE

-- module code

function chunkUtils.checkForDeadendTiles(constantCoordinate, iteratingCoordinate, direction, surface)
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
        if (not chunkUtils.checkForDeadendTiles(xi, y, NORTH_SOUTH, surface)) then
            passableNorthSouth = true
            break
        end
    end
    for yi=y, y + 31 do
        if (not chunkUtils.checkForDeadendTiles(yi, x, EAST_WEST, surface)) then
            passableEastWest = true
            break
        end
    end
    
    chunk[EAST_WEST_PASSABLE] = passableEastWest
    chunk[NORTH_SOUTH_PASSABLE] = passableNorthSouth
end

function chunkUtils.scoreChunk(chunk, surface)   
    local x = chunk.pX
    local y = chunk.pY
    
    local areaBoundingBox = {
	{x, y},
	{x + 32, y + 32}
    }
    local enemyChunkQuery = {area=areaBoundingBox,
                             type="unit-spawner",
                             force="enemy"}
    local enemyWormChunkQuery = {area=areaBoundingBox,
				 type="turret",
				 force="enemy"}
    local playerChunkQuery = {area=areaBoundingBox,
                              force="player"}
    
    local entities = surface.count_entities_filtered(enemyChunkQuery)
    local worms = surface.count_entities_filtered(enemyWormChunkQuery)
    local playerObjects = 0
    
    chunk[ENEMY_BASE_GENERATOR] = (entities * ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT) + worms

    entities = surface.find_entities_filtered(playerChunkQuery)
    
    for i=1, #entities do
        local entityType = entities[i].type
        
        local entityScore = BUILDING_PHEROMONES[entityType]
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
    chunk[ENEMY_BASE_GENERATOR] = 0
    chunk[PLAYER_BASE_GENERATOR] = 0
    chunk[NORTH_SOUTH_PASSABLE] = false
    chunk[EAST_WEST_PASSABLE] = false
    chunk[CHUNK_TICK] = 0
    chunk[RETREAT_TRIGGERED] = 0
    chunk[CHUNK_BASE] = nil
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
