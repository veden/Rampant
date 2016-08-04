local chunkUtils = {}

local regionUtils = require("RegionUtils")
local constants = require("Constants")
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

function chunkUtils.checkChunkPassability(x, y, surface)
    local checkForDeadendTiles = chunkUtils.checkForDeadendTiles
    local NORTH_SOUTH = constants.NORTH_SOUTH
    local EAST_WEST = constants.EAST_WEST
    local CHUNK_SIZE = constants.CHUNK_SIZE
    
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
    return {eastWest = passableEastWest, northSouth = passableNorthSouth}
end

function chunkUtils.checkChunkValues(x, y, surface)
    local CHUNK_SIZE = constants.CHUNK_SIZE
    
    local spawnerCount = surface.count_entities_filtered({area={{x, y},
                                                                {x+CHUNK_SIZE, y+CHUNK_SIZE}},
                                                          type="unit-spawner",
                                                          force="enemy"})
    return { 
            base = spawnerCount
           }
end

function chunkUtils.processChunks(regionMaps, stack)
    local createChunk = chunkUtils.createChunk
    local addChunkToRegionMap = regionUtils.addChunkToRegionMap
    local checkChunkPassability = chunkUtils.checkChunkPassability
    local checkChunkValues = chunkUtils.checkChunkValues
    
    local count = 0
    while (#stack > 0) do
        local event = stack[#stack]
        stack[#stack] = nil
        local surface = event.surface
        local surfaceIndex = surface.index
        
        if (surfaceIndex == 1) then
            if (regionMaps[surfaceIndex] == nil) then
                regionMaps[surfaceIndex] = {}
            end
            
            local topX = event.area.left_top.x
            local topY = event.area.left_top.y
            
            local directions = checkChunkPassability(topX, topY, surface)
            local scores = checkChunkValues(topX, topY, surface)
            local chunk = createChunk(topX,
                                      topY, 
                                      directions,
                                      scores)
            
            addChunkToRegionMap(regionMaps[surfaceIndex],
                                chunk)
        end
        count = count + 1
        if (count % 7 == 0) and (#stack < 70) then
            coroutine.yield()
        end
    end
end

function chunkUtils.createChunk(topX, topY, directions, scores)
    return { 
                0,
                0,
                0,
                x = topX,
                y = topY,
                nS = directions.northSouth, -- passable north_south
                eW = directions.eastWest, -- passable east_west
                bG = scores.base, -- value of pheromone base generator
           }
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

function chunkUtils.init(maps, chunkQueue)
    regionMaps = maps
    chunkProcessingQueue = chunkQueue
end

function chunkUtils.chunkProcess()
    chunkUtils.processChunks(regionMaps, chunkProcessingQueue)
end

return chunkUtils