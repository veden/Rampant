local chunkProcessor = {}

-- imports

local chunkUtils = require("ChunkUtils")
local constants = require("Constants")

-- constants

local CHUNK_QUEUE_SIZE = constants.CHUNK_QUEUE_SIZE

local CHUNK_SIZE = constants.CHUNK_SIZE

-- imported functions

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
    
    local count = 0

    local vanillaAI = not natives.useCustomAI
    
    local chunkTiles = regionMap.chunkTiles
    
    local start = #pendingStack
    for i=start, 1, -1 do
        local event = pendingStack[i]
	count = count + 1
        pendingStack[i] = nil

	local x = event.area.left_top.x
	local y = event.area.left_top.y
        local chunk = createChunk(x, y)

	areaBoundingBox[1] = chunk
	offset[1] = x + CHUNK_SIZE
	offset[2] = y + CHUNK_SIZE
	
        local chunkX = chunk.cX
        
        if regionMap[chunkX] == nil then
            regionMap[chunkX] = {}
        end
        regionMap[chunkX][chunk.cY] = chunk
        
        checkChunkPassability(chunkTiles, chunk, surface)
	if vanillaAI then
	    registerChunkEnemies(chunk, surface, query)
	end
	scoreChunk(chunk, surface, natives, query)
        processQueue[#processQueue+1] = chunk
	if (count >= CHUNK_QUEUE_SIZE) then
	    break
	end
    end
    if (count >= CHUNK_QUEUE_SIZE) and (start > 0) then
	surface.print("Rampant - " .. (start - count) .. " Remaining chunks to process")
    end
end

return chunkProcessor
