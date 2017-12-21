local chunkProcessor = {}

-- imports

local chunkUtils = require("ChunkUtils")
local constants = require("Constants")

-- constants

local CHUNK_SIZE = constants.CHUNK_SIZE

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

-- imported functions

local createChunk = chunkUtils.createChunk
local checkChunkPassability = chunkUtils.checkChunkPassability
local scoreChunk = chunkUtils.scoreChunk
local registerChunkEnemies = chunkUtils.registerChunkEnemies

-- module code

function chunkProcessor.processPendingChunks(natives, regionMap, surface, pendingStack)
    local processQueue = regionMap.processQueue

    local filteredEntitiesQuery = regionMap.filteredEntitiesQuery
    
    local topOffset = filteredEntitiesQuery.area[1]
    local bottomOffset = filteredEntitiesQuery.area[2]
    
    local filteredTilesQuery = regionMap.filteredTilesQuery
    local cliffQuery = regionMap.cliffQuery
    
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

        chunk = checkChunkPassability(chunk, surface, filteredTilesQuery, cliffQuery)

	if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then	    
	    registerChunkEnemies(regionMap, chunk, surface, filteredEntitiesQuery)
	    scoreChunk(regionMap, chunk, surface, natives, filteredEntitiesQuery)
	    
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
