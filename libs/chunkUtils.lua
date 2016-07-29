local CHUNK_SIZE = 32
local NORTH_SOUTH = true
local EAST_WEST = false

function checkForDeadendTiles(constantCoordinate, iteratingCoordinate, direction, chunkSize, surface)
    local deadEnd = false
    local x = iteratingCoordinate
    while not deadEnd and (x < iteratingCoordinate + chunkSize) do
        local tile
        if (direction == NORTH_SOUTH) then
            tile = surface.get_tile(constantCoordinate, x)
        else
            tile = surface.get_tile(x, constantCoordinate)
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

function checkChunkPassability(x, y, surface)
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
        colorChunk(x, y, "grass", surface)
    elseif passableNorthSouth then
        colorChunk(x, y, "dirt", surface)
    elseif passableEastWest then
        colorChunk(x, y, "sand", surface)
    else
        colorChunk(x, y, "concrete", surface)
    end
    return {eastWest = passableEastWest, northSouth = passableNorthSouth}
end

function checkChunkValues(x, y, surface)
    local pV = 0
    local pDV = 0
    local eV = 0
    local eDV = 0
    return { 
            pV = pV,
            pDV = pDV,
            eV = eV,
            eDV = eDV,
           }
end

function createChunk(topX, topY, surface)
    local directions = checkChunkPassability(topX, topY, surface)
    local scores = checkChunkValues(topX, topY, surface)
    return { 
             pNS = directions.northSouth, -- passable north_south
             pEW = directions.eastWest, -- passable east_west
             pV = scores.pV, -- value of player structures
             pDV = scores.pDV, -- value of player defenses
             eV = scores.eV, -- value of enemy structures
             eDV = scores.eDV, -- value of enemy defenses
             rV = scores.rV -- value of resources
           }
end

function addChunkToRegionMap(regionMap, x, y, surface)

    local chunkX = x * 0.03125
    local chunkY = y * 0.03125    
    
    local key = hashChunkCoordinates(chunkX, chunkY)
    regionMap[key] = createChunk(x, y, surface)
end

function hashChunkCoordinates(x, y)
    return tostring(x) .. "," .. tostring(y)
end

-- aux

function showGrid(regionMap, surface)
    for i, chunk in pairs(regionMap) do        
        local x = chunk.x --math.floor(game.players[1].position.x / 32) * 32
        local y = chunk.y --math.floor(game.players[1].position.y / 32) * 32
    end
end

function colorChunk(x, y, tileType, surface)
    local tiles = {}
    for xi=x+5, x + CHUNK_SIZE-5 do
        for yi=y+5, y + CHUNK_SIZE-5 do
            tiles[#tiles+1] = {name=tileType, position={xi, yi}}
        end
    end
    surface.set_tiles(tiles, false)
end