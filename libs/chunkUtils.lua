local chunkUtils = {}

local mapUtils = require("MapUtils")
local constants = require("Constants")

local natives
local regionMaps
local chunkProcessingQueue

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
    if passableNorthSouth and passableEastWest then
        chunkUtils.colorChunk(x, y, "grass", surface)
    elseif passableNorthSouth then
        chunkUtils.colorChunk(x, y, "dirt", surface)
    elseif passableEastWest then
        chunkUtils.colorChunk(x, y, "sand", surface)
    else
        chunkUtils.colorChunk(x, y, "concrete", surface)
    end
    
    chunk.eW = passableEastWest
    chunk.nS = passableNorthSouth
end

function chunkUtils.scoreChunk(chunk, surface, natives)
    local x = chunk.pX
    local y = chunk.pY
    local cX = chunk.cX
    local cY = chunk.cY
    local CHUNK_SIZE = constants.CHUNK_SIZE
    
    local spawners = surface.count_entities_filtered({area={{x, y},
                                                            {x+CHUNK_SIZE, y+CHUNK_SIZE}},
                                                      type="unit-spawner",
                                                      force="enemy"})
    if (natives.bases[cX] == nil) then
        natives.bases[cX] = {}
    end
    local nativeX = natives.bases[cX]
    if (nativeX[cY] == nil) then
        nativeX[cY] = {}
    end
    nativeX[cY].bG = spawners
end

function chunkUtils.createChunk(topX, topY)
    local chunk = { 
                    pX = topX,
                    pY = topY,
                    cX = topX * 0.03125,
                    cY = topY * 0.03125
                  }
    chunk[constants.BASE_PHEROMONE] = 0
    chunk[constants.PLAYER_PHEROMONE] = 0
    chunk[constants.DEATH_PHEROMONE] = 0
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