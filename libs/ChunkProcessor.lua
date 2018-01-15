local chunkProcessor = {}

-- imports

local chunkUtils = require("ChunkUtils")
local constants = require("Constants")

-- constants

local CHUNK_SIZE = constants.CHUNK_SIZE

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

-- imported functions

local createChunk = chunkUtils.createChunk
local initialScan = chunkUtils.initialScan
local chunkPassScan = chunkUtils.chunkPassScan

-- module code

function chunkProcessor.processPendingChunks(natives, map, surface, pendingStack, tick)
    local processQueue = map.processQueue

    local area = map.area
    
    local topOffset = area[1]
    local bottomOffset = area[2]
    
    for i=#pendingStack, 1, -1 do
        local event = pendingStack[i]
        pendingStack[i] = nil

	local topLeft = event.area.left_top
	local x = topLeft.x
	local y = topLeft.y
        local chunk = createChunk(x, y)

	topOffset[1] = x
	topOffset[2] = y
	bottomOffset[1] = x + CHUNK_SIZE
	bottomOffset[2] = y + CHUNK_SIZE

        chunk = initialScan(chunk, natives, surface, map, tick)

	if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then
	    local chunkX = chunk.x
	    
	    if map[chunkX] == nil then
		map[chunkX] = {}
	    end
	    map[chunkX][chunk.y] = chunk

	    processQueue[#processQueue+1] = chunk
	end
    end
end

function chunkProcessor.processScanChunks(map, surface)
    local area = map.area
    
    local topOffset = area[1]
    local bottomOffset = area[2]

    local removals = {}

    for chunk,_ in pairs(map.chunkToPassScan) do
	local x = chunk.x
	local y = chunk.y
	
	topOffset[1] = x
	topOffset[2] = y
	bottomOffset[1] = x + CHUNK_SIZE
	bottomOffset[2] = y + CHUNK_SIZE

        chunk = chunkPassScan(chunk, surface, map)

	if (chunk == SENTINEL_IMPASSABLE_CHUNK) then
	    map[x][y] = nil

	    removals[#removals+1] = chunk
	end
    end

    for i=#removals,1,-1 do
	table.remove(map.processQueue, i)
    end
   
    return {}
end

return chunkProcessor
