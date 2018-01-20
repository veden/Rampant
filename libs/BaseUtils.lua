local baseUtils = {}

-- imports

local stringUtils = require("StringUtils")
local mathUtils = require("MathUtils")
local constants = require("Constants")
local mapUtils = require("MapUtils")
local chunkPropertyUtils = require("ChunkPropertyUtils")

-- constants

local BASE_DISTANCE_THRESHOLD = constants.BASE_DISTANCE_THRESHOLD
local BASE_DISTANCE_LEVEL_BONUS = constants.BASE_DISTANCE_LEVEL_BONUS

local BASE_ALIGNMENT_NEUTRAL = constants.BASE_ALIGNMENT_NEUTRAL

local CHUNK_SIZE = constants.CHUNK_SIZE

local EVOLUTION_INCREMENTS = constants.EVOLUTION_INCREMENTS

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER
local MAGIC_MAXIMUM_BASE_NUMBER = constants.MAGIC_MAXIMUM_BASE_NUMBER

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

-- imported functions

local euclideanDistancePoints = mathUtils.euclideanDistancePoints

local isRampant = stringUtils.isRampant

local gaussianRandomRange = mathUtils.gaussianRandomRange

local mFloor = math.floor

local getChunkByPosition = mapUtils.getChunkByPosition
local getChunkByXY = mapUtils.getChunkByXY
local getChunkBase = chunkPropertyUtils.getChunkBase
local setChunkBase = chunkPropertyUtils.setChunkBase

local mRandom = math.random

-- module code

function baseUtils.findNearbyBase(map, chunk, chunkRadius)
    if (chunk == SENTINEL_IMPASSABLE_CHUNK) then
	return nil
    end

    local tested = {}
    
    local x = chunk.x
    local y = chunk.y

    local foundBase = getChunkBase(map, chunk)
    if foundBase then
	return foundBase
    end
    
    local closest = MAGIC_MAXIMUM_NUMBER
    for xi = x-chunkRadius, x+chunkRadius, CHUNK_SIZE do
	for yi = y-chunkRadius, y+chunkRadius, CHUNK_SIZE do
	    if (xi ~= x) and (yi ~= y)  then
		local base = getChunkBase(map, getChunkByXY(map, xi, yi))
		if base then
		    if not tested[base] then
			tested[base] = true
			local distance = euclideanDistancePoints(base.x, base.y, x, y)
			if (distance <= (BASE_DISTANCE_THRESHOLD + (base.level * BASE_DISTANCE_LEVEL_BONUS))) and (distance < closest) then
			    closest = distance
			    foundBase = base
			end
		    end
		end
	    end
	end
    end
    
    return foundBase
end

local function findUpgrade(base, evoIndex, natives)
    local used = { }
    local alignments = base.alignments

    local alignmentPick = base.alignments[mRandom(#base.alignments)]
    
    while alignmentPick do
	used[alignmentPick] = true

	for evo=evoIndex, 0, -EVOLUTION_INCREMENTS do
	    local entitySet = natives.evolutionTable[alignmentPick][evo]
	    if (#entitySet > 0) then
		return entitySet[mRandom(#entitySet)]
	    end
	end

	alignmentPick = nil
	for x = #alignments, 1, -1 do
	    if not used[alignments[x]] then
		alignmentPick = alignments[x]
		break
	    end
	end
    end
    
    return nil
end

function baseUtils.upgradeEntity(map, entity, surface, natives, evolutionFactor, tick)
    if not isRampant(entity.name) then
	local position = entity.position
	entity.die()
	local chunk = getChunkByPosition(map, position)
	local base = getChunkBase(map, chunk)

	if not base then
	    base = baseUtils.createBase(map, natives, evolutionFactor, chunk, surface, tick)
	end

	-- local distance = euclideanDistancePoints(position.x, position.y, 0, 0)
	local evoIndex = mFloor(evolutionFactor/EVOLUTION_INCREMENTS) * EVOLUTION_INCREMENTS

	local spawnerName = findUpgrade(base, evoIndex, natives)
	if spawnerName then
	    entity = surface.create_entity({name = spawnerName, position = position})
	end
    end

    return entity
end

function baseUtils.createBase(map, natives, evolutionFactor, chunk, surface, tick)
    local x = chunk.x
    local y = chunk.y
    local distance = euclideanDistancePoints(x, y, 0, 0)

    local meanLevel = mFloor(distance / 200)

    local alignments = { BASE_ALIGNMENT_NEUTRAL }
    
    local base = {
	x = x,
	y = y,
	tick = tick,
	evolution = evolutionFactor,
	alignments = alignments,
	pattern = mRandom(MAGIC_MAXIMUM_BASE_NUMBER),
	level = gaussianRandomRange(meanLevel, meanLevel * 0.15, meanLevel * 0.50, meanLevel * 1.50)
    }

    setChunkBase(map, chunk, base)
    
    -- if not buildHive(map, base, surface) then
    -- 	return nil
    -- end

    natives.bases[#natives.bases+1] = base
    
    return base
end

return baseUtils

