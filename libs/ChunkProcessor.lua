local chunkProcessor = {}

-- imports

local chunkUtils = require("ChunkUtils")
local constants = require("Constants")

-- constants

local CHUNK_SIZE = constants.CHUNK_SIZE

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

-- imported functions

local remakeChunk = chunkUtils.remakeChunk
local createChunk = chunkUtils.createChunk
local checkChunkPassability = chunkUtils.checkChunkPassability
local scoreChunk = chunkUtils.scoreChunk
local registerChunkEnemies = chunkUtils.registerChunkEnemies

-- module code

function chunkProcessor.processPendingChunks(natives, regionMap, surface, pendingStack, tick)
    local processQueue = regionMap.processQueue

    local offset = {0, 0}
    local areaBoundingBox = {false, offset}
    local query = {area=areaBoundingBox,
		   force=false}
    
    local vanillaAI = not natives.useCustomAI
    
    local chunkTiles = regionMap.chunkTiles
    
    for i=#pendingStack, 1, -1 do
        local event = pendingStack[i]
        pendingStack[i] = nil

	local topLeft = event.area.left_top
	local x = topLeft.x
	local y = topLeft.y
        local chunk = createChunk(x, y)
	
        chunk = checkChunkPassability(chunkTiles, chunk, surface)

	if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then

	    areaBoundingBox[1] = chunk
	    offset[1] = x + CHUNK_SIZE
	    offset[2] = y + CHUNK_SIZE	    
	    
	    if vanillaAI then
		registerChunkEnemies(regionMap, chunk, surface, query)
	    else
		remakeChunk(regionMap, chunk, surface, natives, tick, query)
	    end
	    scoreChunk(regionMap, chunk, surface, natives, query)
	    
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
