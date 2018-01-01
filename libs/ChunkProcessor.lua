local chunkProcessor = {}

-- imports

local chunkUtils = require("ChunkUtils")
local constants = require("Constants")
local squadDefense = require("SquadDefense")

-- constants

local CHUNK_SIZE = constants.CHUNK_SIZE

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

-- imported functions

local createChunk = chunkUtils.createChunk
local analyzeChunk = chunkUtils.analyzeChunk

-- module code

function chunkProcessor.processPendingChunks(natives, regionMap, surface, pendingStack)
    local processQueue = regionMap.processQueue

    local area = regionMap.area
    
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

        chunk = analyzeChunk(chunk, natives, surface, regionMap)

	if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then	    	    
	    local chunkX = chunk.x
	    
	    if regionMap[chunkX] == nil then
		regionMap[chunkX] = {}
	    end
	    regionMap[chunkX][chunk.y] = chunk

	    processQueue[#processQueue+1] = chunk
	end
    end
end

return chunkProcessor
