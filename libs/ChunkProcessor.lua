local chunkProcessor = {}

-- imports

local chunkUtils = require("ChunkUtils")
local mathUtils = require("MathUtils")
local constants = require("Constants")
local squadDefense = require("SquadDefense")
local unitGroupUtils = require("UnitGroupUtils")

-- constants

local CHUNK_SIZE = constants.CHUNK_SIZE

local RETREAT_GRAB_RADIUS = constants.RETREAT_GRAB_RADIUS
local SPAWNER_EGG_TIMEOUT = constants.SPAWNER_EGG_TIMEOUT

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

-- imported functions

local retreatUnits = squadDefense.retreatUnits

local createChunk = chunkUtils.createChunk
local initialScan = chunkUtils.initialScan
local chunkPassScan = chunkUtils.chunkPassScan

local euclideanDistanceNamed = mathUtils.euclideanDistanceNamed

local tSort = table.sort

local abs = math.abs

-- module code

local origin = {x=0,y=0}

local function sorter(a, b)
    local aDistance = euclideanDistanceNamed(a, origin)
    local bDistance = euclideanDistanceNamed(b, origin)

    if (aDistance == bDistance) then
        if (a.x == b.x) then
            return (abs(a.y) < abs(b.y))
        else
            return (abs(a.x) < abs(b.x))
        end
    end

    return (aDistance < bDistance)
end

function chunkProcessor.processPendingChunks(natives, map, surface, pendingStack, tick, evolutionFactor, rebuilding)
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

        chunk = initialScan(chunk, natives, surface, map, tick, evolutionFactor, rebuilding)

	if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then
	    local chunkX = chunk.x

	    if map[chunkX] == nil then
		map[chunkX] = {}
	    end
	    map[chunkX][chunk.y] = chunk

	    processQueue[#processQueue+1] = chunk
	end
    end

    if (#processQueue > natives.nextChunkSort) then
        natives.nextChunkSort = #processQueue + 75
        tSort(processQueue, sorter)
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

function chunkProcessor.processSpawnerChunks(map, surface, natives, tick)
    local queue = map.queueSpawners

    local result = {}

    for i=1, #queue do
	local o = queue[i]
	if ((tick - o[1]) > SPAWNER_EGG_TIMEOUT) then
	    local chunk = o[2]
	    local position = o[3]

	    retreatUnits(chunk,
			 position,
			 nil,
			 map,
			 surface,
			 natives,
			 tick,
			 RETREAT_GRAB_RADIUS,
			 false,
			 true)
	else
	    result[#result+1] = o
	end
    end

    return result
end


return chunkProcessor
