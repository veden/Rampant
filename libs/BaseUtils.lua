local baseUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")

-- constants

local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR

local BASE_DISTANCE_THRESHOLD = constants.BASE_DISTANCE_THRESHOLD

local BASE_ALIGNMENT_NEUTRAL = constants.BASE_ALIGNMENT_NEUTRAL

local CHUNK_BASE = constants.CHUNK_BASE

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

-- imported functions

local euclideanDistancePoints = mapUtils.euclideanDistancePoints

local tableRemove = table.remove

-- module code

local function calculateBaseMean(base)
    local chunks = base.chunks
    local xs = 0
    local ys = 0
    local strength = 0
    for _,aChunk in pairs(chunks) do
	xs = xs + aChunk.cX
	ys = ys + aChunk.cY
	strength = strength + aChunk[ENEMY_BASE_GENERATOR]
    end
    local count = #chunks
    if (count ~= 0) then
	base.cX = xs / count
	base.cY = ys / count
    end
    base.strength = strength
end

function baseUtils.annexNest(natives, chunk, tick)
    if (chunk[CHUNK_BASE] == nil) and (chunk[ENEMY_BASE_GENERATOR] ~= 0) then
	local bases = natives.bases
	local annex = nil
	local closest = MAGIC_MAXIMUM_NUMBER
	for i=1,#bases do
	    local base = bases[i]
	    local score = euclideanDistancePoints(base.cX, base.cY, chunk.cX, chunk.cY)
	    if (score <= BASE_DISTANCE_THRESHOLD) and (score < closest) then
		closest = score
		annex = base
	    end
	end
	if (annex ~= nil) then
	    local chunks = annex.chunks
	    chunks[#chunks+1] = chunk
	    chunk[CHUNK_BASE] = annex
	    calculateBaseMean(annex)	    
	else
	    baseUtils.createBase(natives, chunk, tick)
	end
    end
end

function baseUtils.removeNest(natives, chunk)
    local base = chunk[CHUNK_BASE]
    local chunks = base.chunks
    for i=1, #chunks do
	local aChunk = chunks[i]
	if (chunk == aChunk) then
	    tableRemove(chunks, i)
	    break
	end
    end
    if (#chunks == 0) then
	local bases = natives.bases
	for i=1, #bases do
	    local aBase = bases[i]
	    if (base == aBase) then
		tableRemove(bases, i)
		break
	    end
	end
    else
	calculateBaseMean(base)
    end
    chunk[CHUNK_BASE] = nil
end

function baseUtils.buildOrder(base)
    
end

function baseUtils.createBase(natives, chunk, tick)
    local bases = natives.bases
    local chunks = {}
    local base = { cX = chunk.cX,
		   cY = chunk.cY,
		   created = tick,
		   alignment = BASE_ALIGNMENT_NEUTRAL,
		   strength = chunk[ENEMY_BASE_GENERATOR],
		   chunks = chunks }
    chunks[#chunks+1] = chunk
    chunk[CHUNK_BASE] = base
    bases[#bases+1] = base
end

function baseUtils.mergeBases(natives)
    local bases = natives.bases
    for x=#bases,1,-1 do
	local outerBase = bases[x]
	if (outerBase ~= nil) then
	    local outerChunks = outerBase.chunks
	    for y=x-1,1,-1 do
		local innerBase = bases[y]
		if (innerBase ~= nil) then
		    if (euclideanDistancePoints(outerBase.cX, outerBase.cY, innerBase.cX, innerBase.cY) < BASE_DISTANCE_THRESHOLD) then
			local innerChunks = innerBase.chunks
			for i=1,#innerChunks do
			    local innerChunk = innerChunks[i]
			    outerChunks[#outerChunks+1] = innerChunk
			    innerChunk[CHUNK_BASE] = outerBase
			end
			bases[y] = nil
		    end
		end 
	    end
	end
    end
    for x=#bases,1,-1 do
	if (bases[x] == nil) then
	    tableRemove(bases, x)
	else
	    calculateBaseMean(bases[x])
	end
    end
end

return baseUtils

