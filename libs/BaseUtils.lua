local baseUtils = {}

-- imports

local stringUtils = require("StringUtils")
local mathUtils = require("MathUtils")
local constants = require("Constants")
local mapUtils = require("MapUtils")
local chunkPropertyUtils = require("ChunkPropertyUtils")

-- constants

local BASE_DISTANCE_THRESHOLD = constants.BASE_DISTANCE_THRESHOLD

local BASE_ALIGNMENT_NEUTRAL = constants.BASE_ALIGNMENT_NEUTRAL

local CHUNK_SIZE = constants.CHUNK_SIZE

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER
local MAGIC_MAXIMUM_BASE_NUMBER = constants.MAGIC_MAXIMUM_BASE_NUMBER

-- imported functions

local euclideanDistancePoints = mathUtils.euclideanDistancePoints

local isRampant = stringUtils.isRampant

local mFloor = math.floor

local positionToChunkXY = mapUtils.positionToChunkXY
local getChunkBase = chunkPropertyUtils.getChunkBase
local setChunkBase = chunkPropertyUtils.setChunkBase

local mRandom = math.random

-- module code

function baseUtils.findNearbyBase(map, chunk, chunkRadius)
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
		local base = getChunkBase(map, positionToChunkXY(map, xi, yi))
		if base then
		    local distance = euclideanDistancePoints(base.x, base.y, x, y)
		    if (distance <= (BASE_DISTANCE_THRESHOLD + (base.level * 100))) and (distance < closest) then
			closest = distance
			foundBase = base
		    end
		end
	    end
	end
    end
    
    return foundBase
end

function baseUtils.upgradeEntity(map, entity, surface, natives, evolutionFactor, tick)
    if not isRampant(entity.name) then
	local position = entity.position
	entity.die()
	local chunk = positionToChunkXY(map, position)
	local base = getChunkBase(map, chunk)

	if not base then
	    baseUtils.createBase(map, natives, evolutionFactor, chunk, surface, tick)
	end

	entity = surface.creaate_entity({name = "rampant-suicide-nest-v" .. mRandom(5) .. "-t1",
					 position = position})
	
    end

    return entity
end

function baseUtils.createBase(map, natives, evolutionFactor, chunk, surface, tick)
    local x = chunk.x
    local y = chunk.y
    local distance = euclideanDistancePoints(x, y, 0, 0)
    local base = {
	x = x,
	y = y,
	created = tick,
	alignment = { BASE_ALIGNMENT_NEUTRAL },
	pattern = mRandom(MAGIC_MAXIMUM_BASE_NUMBER),
	level = mFloor(distance / 200)
    }

    setChunkBase(map, chunk, base)
    
    -- if not buildHive(map, base, surface) then
    -- 	return nil
    -- end

    natives.bases[natives.bases+1] = base
    
    return base
end

return baseUtils

