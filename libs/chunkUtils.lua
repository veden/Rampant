local chunkUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")

-- constants

local DEATH_PHEROMONE = constants.DEATH_PHEROMONE
local ENEMY_BASE_PHEROMONE = constants.ENEMY_BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local PLAYER_BASE_PHEROMONE = constants.PLAYER_BASE_PHEROMONE
local PLAYER_DEFENSE_PHEROMONE = constants.PLAYER_DEFENSE_PHEROMONE
local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE

local DEFENSE_PHEROMONES = constants.DEFENSE_PHEROMONES
local BUILDING_PHEROMONES = constants.BUILDING_PHEROMONES

local PLAYER_DEFENSE_GENERATOR = constants.PLAYER_DEFENSE_GENERATOR
local PLAYER_BASE_GENERATOR = constants.PLAYER_BASE_GENERATOR
local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR

local NORTH_SOUTH = constants.NORTH_SOUTH
local EAST_WEST = constants.EAST_WEST

local EAST_WEST_PASSABLE = constants.EAST_WEST_PASSABLE
local NORTH_SOUTH_PASSABLE = constants.NORTH_SOUTH_PASSABLE

local CHUNK_SIZE = constants.CHUNK_SIZE

local ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT = constants.ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT

-- module code
                          
function chunkUtils.checkForDeadendTiles(constantCoordinate, iteratingCoordinate, direction, chunkSize, surface)
    local get_tile = surface.get_tile
    
    local deadEnd = false
    local x = iteratingCoordinate
    while not deadEnd and (x < iteratingCoordinate + chunkSize) do
        local tile
        if (direction == NORTH_SOUTH) then
            tile = get_tile(constantCoordinate, x)
        else
            tile = get_tile(x, constantCoordinate)
        end
        if (tile.collides_with("player-layer")) then
            deadEnd = true
        -- else
            -- surface.set_tiles({{name="sand-dark", position=tile.position}}, false)
        end
        x = x + 1
    end
    return deadEnd
end

function chunkUtils.checkChunkPassability(chunk, surface, natives)   
    local x = chunk.pX
    local y = chunk.pY
    
    local passableNorthSouth = false
    local passableEastWest = false
    local xi = x
    while not passableNorthSouth and (xi < x + CHUNK_SIZE) do
        if (not chunkUtils.checkForDeadendTiles(xi, y, NORTH_SOUTH, CHUNK_SIZE, surface)) then
            passableNorthSouth = true
        end
        xi = xi + 1
    end
    local yi = y
    while not passableEastWest and (yi < y + CHUNK_SIZE) do
        if (not chunkUtils.checkForDeadendTiles(yi, x, EAST_WEST, CHUNK_SIZE, surface)) then
            passableEastWest = true
        end
        yi = yi + 1
    end
    -- if passableNorthSouth and passableEastWest then
        -- chunkUtils.colorChunk(x, y, "grass", surface)
    -- elseif passableNorthSouth then
        -- chunkUtils.colorChunk(x, y, "dirt", surface)
    -- elseif passableEastWest then
        -- chunkUtils.colorChunk(x, y, "sand", surface)
    -- else
        -- chunkUtils.colorChunk(x, y, "concrete", surface)
    -- end
    
    if passableEastWest then
        chunk[EAST_WEST_PASSABLE] = true
    else
        chunk[EAST_WEST_PASSABLE] = false
    end
    if passableNorthSouth then
        chunk[NORTH_SOUTH_PASSABLE] = true
    else
        chunk[NORTH_SOUTH_PASSABLE] = false
    end
end

function chunkUtils.scoreChunk(chunk, surface, natives)   
    local x = chunk.pX
    local y = chunk.pY
    
    local areaBoundingBox = {{x, y},
                             {x + CHUNK_SIZE, y + CHUNK_SIZE}}
    local enemyChunkQuery = {area=areaBoundingBox,
                             type="unit-spawner",
                             force="enemy"}
    local playerChunkQuery = {area=areaBoundingBox,
                              force="player"}
                   
    local entities = surface.count_entities_filtered(enemyChunkQuery)
    local spawners = 0
    local playerObjects = 0
    local playerDefenses = 0
    
    spawners = entities * ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT
                   
    entities = surface.find_entities_filtered(playerChunkQuery)
    
    for i=1, #entities do
        local entityType = entities[i].type
        
        local entityScore = DEFENSE_PHEROMONES[entityType]
        if (entityScore ~= nil) then
            playerDefenses = playerDefenses + entityScore
        end
        entityScore = BUILDING_PHEROMONES[entityType]
        if (entityScore ~= nil) then
            playerObjects = playerObjects + entityScore
        end
    end
    
    chunk[PLAYER_BASE_GENERATOR] = playerObjects
    chunk[PLAYER_DEFENSE_GENERATOR] = playerDefenses
    chunk[ENEMY_BASE_GENERATOR] = spawners
end

function chunkUtils.createChunk(topX, topY)
    local chunk = { 
                    pX = topX,
                    pY = topY,
                    cX = topX * 0.03125,
                    cY = topY * 0.03125
                  }
    chunk[DEATH_PHEROMONE] = 0
    chunk[ENEMY_BASE_PHEROMONE] = 0
    chunk[PLAYER_PHEROMONE] = 0
    chunk[PLAYER_BASE_PHEROMONE] = 0
    chunk[PLAYER_DEFENSE_PHEROMONE] = 0
    chunk[MOVEMENT_PHEROMONE] = 0
    return chunk
end

function chunkUtils.colorChunk(x, y, tileType, surface)
    local tiles = {}
    for xi=x+5, x + CHUNK_SIZE-5 do
        for yi=y+5, y + CHUNK_SIZE-5 do
            tiles[#tiles+1] = {name=tileType, position={xi, yi}}
        end
    end
    surface.set_tiles(tiles, false)
end

return chunkUtils