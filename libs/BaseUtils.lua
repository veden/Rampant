local baseUtils = {}

-- imports

local stringUtils = require("StringUtils")
local mathUtils = require("MathUtils")
local constants = require("Constants")

-- constants

local BASE_DISTANCE_THRESHOLD = constants.BASE_DISTANCE_THRESHOLD

local BASE_ALIGNMENT_NEUTRAL = constants.BASE_ALIGNMENT_NEUTRAL

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER
local MAGIC_MAXIMUM_BASE_NUMBER = constants.MAGIC_MAXIMUM_BASE_NUMBER

-- imported functions

local euclideanDistancePoints = mathUtils.euclideanDistancePoints

local isRampant = stringUtils.isRampant

local mFloor = math.floor

-- local buildTendril = tendrilUtils.buildTendril

local mRandom = math.random

-- module code

function baseUtils.findNearbyBase(natives, position)
    local bases = natives.bases
    local foundBase
    local closest = MAGIC_MAXIMUM_NUMBER
    for i=1,#bases do
	local base = bases[i]
	local distance = euclideanDistancePoints(base.x, base.y, position.x, position.y)
	if (distance <= (BASE_DISTANCE_THRESHOLD + (base.level * 100))) and (distance < closest) then
	    closest = distance
	    foundBase = base
	end
    end
    return foundBase
end

function baseUtils.upgradeEntity(map, entity, surface, natives)
    if not isRampant(entity.name) then
	local position = entity.position
	-- build_base_evolution_requirement
	entity.die()
	entity = surface.create_entity({name = "rampant-suicide-nest-v" .. mRandom(5) .. "-t1",
					position = position})
    end

    return entity
end

function baseUtils.createBase(map, natives, position, surface, tick)
    local bases = natives.bases
    local distance = euclideanDistancePoints(position.x, position.y, 0, 0)
    local base = {
	x = position.x,
	y = position.y,
	created = tick,
	alignment = { BASE_ALIGNMENT_NEUTRAL },
	hives = {},
	tendrils = {},
	nests = {},
	worms = {},
	eggs = {},
	upgradePoints = 0,
	growth = tick,
	pattern = mRandom(MAGIC_MAXIMUM_BASE_NUMBER),
	level = mFloor(distance / 200)
    }
    if not buildHive(map, base, surface) then
	return nil
    end
    -- buildTendril(map, natives, base, surface, tick)
    bases[#bases+1] = base
    return base
end

return baseUtils

