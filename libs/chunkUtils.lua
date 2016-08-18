local chunkUtils = {}

local mapUtils = require("MapUtils")
local constants = require("Constants")

-- optimize table creation and referencing
local areaBoundingBox = {{1,2},
                         {3,4}}
local enemyChunkQuery = {area=areaBoundingBox,
                         type="unit-spawner",
                         force="enemy"}
local playerChunkQuery = {area=areaBoundingBox,
                          force="player"}


function chunkUtils.checkForDeadendTiles(constantCoordinate, iteratingCoordinate, direction, chunkSize, surface)
    local NORTH_SOUTH = constants.NORTH_SOUTH
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
    local checkForDeadendTiles = chunkUtils.checkForDeadendTiles
    local MOVEMENT_PHEROMONE_GENERATOR_AMOUNT = constants.MOVEMENT_PHEROMONE_GENERATOR_AMOUNT
    local NORTH_SOUTH = constants.NORTH_SOUTH
    local EAST_WEST = constants.EAST_WEST
    local CHUNK_SIZE = constants.CHUNK_SIZE
    
    local x = chunk.pX
    local y = chunk.pY
    
    local passableNorthSouth = false
    local passableEastWest = false
    local xi = x
    while not passableNorthSouth and (xi < x + CHUNK_SIZE) do
        if (not checkForDeadendTiles(xi, y, NORTH_SOUTH, CHUNK_SIZE, surface)) then
            passableNorthSouth = true
        end
        xi = xi + 1
    end
    local yi = y
    while not passableEastWest and (yi < y + CHUNK_SIZE) do
        if (not checkForDeadendTiles(yi, x, EAST_WEST, CHUNK_SIZE, surface)) then
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
        chunk[constants.EAST_WEST_PASSABLE] = true
    else
        chunk[constants.EAST_WEST_PASSABLE] = false
    end
    if passableNorthSouth then
        chunk[constants.NORTH_SOUTH_PASSABLE] = true
    else
        chunk[constants.NORTH_SOUTH_PASSABLE] = false
    end
end

function chunkUtils.scoreChunk(chunk, surface, natives)
    local PLAYER_DEFENSE_PHEROMONES = constants.defensePheromones
    local PLAYER_BASE_PHEROMONES = constants.buildingPheromones
    
    local x = chunk.pX
    local y = chunk.pY

    areaBoundingBox[1][1] = x
    areaBoundingBox[1][2] = y
    areaBoundingBox[2][1] = x+constants.CHUNK_SIZE
    areaBoundingBox[2][2] = y+constants.CHUNK_SIZE
                   
    local entities = surface.count_entities_filtered(enemyChunkQuery)
    local spawners = 0
    local playerObjects = 0
    local playerDefenses = 0
    
    spawners = entities * constants.ENEMY_BASE_PHEROMONE_GENERATOR_AMOUNT
                   
    entities = surface.find_entities_filtered(playerChunkQuery)
    
    for i=1, #entities do
        local entityType = entities[i].type
        
        local entityScore = PLAYER_DEFENSE_PHEROMONES[entityType]
        if (entityScore ~= nil) then
            playerDefenses = playerDefenses + entityScore
        end
        entityScore = PLAYER_BASE_PHEROMONES[entityType]
        if (entityScore ~= nil) then
            playerObjects = playerObjects + entityScore
        end
    end
    
    chunk[constants.PLAYER_BASE_GENERATOR] = playerObjects
    chunk[constants.PLAYER_DEFENSE_GENERATOR] = playerDefenses
    chunk[constants.ENEMY_BASE_GENERATOR] = spawners
end

function chunkUtils.createChunk(topX, topY)
    local chunk = { 
                    pX = topX,
                    pY = topY,
                    cX = topX * 0.03125,
                    cY = topY * 0.03125
                  }
    chunk[constants.DEATH_PHEROMONE] = 0
    chunk[constants.ENEMY_BASE_PHEROMONE] = 0
    chunk[constants.PLAYER_PHEROMONE] = 0
    chunk[constants.PLAYER_BASE_PHEROMONE] = 0
    chunk[constants.PLAYER_DEFENSE_PHEROMONE] = 0
    return chunk
end

function chunkUtils.colorChunk(x, y, tileType, surface)
    local CHUNK_SIZE = constants.CHUNK_SIZE
    
    local tiles = {}
    for xi=x+5, x + CHUNK_SIZE-5 do
        for yi=y+5, y + CHUNK_SIZE-5 do
            tiles[#tiles+1] = {name=tileType, position={xi, yi}}
        end
    end
    surface.set_tiles(tiles, false)
end

return chunkUtils