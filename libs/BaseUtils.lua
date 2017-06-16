local baseUtils = {}

-- imports

local mathUtils = require("MathUtils")
local constants = require("Constants")

local tendrilUtils = require("TendrilUtils")

local buildUtils = require("BuildUtils")

-- constants

local BASE_DISTANCE_THRESHOLD = constants.BASE_DISTANCE_THRESHOLD

local BASE_ALIGNMENT_NEUTRAL = constants.BASE_ALIGNMENT_NEUTRAL

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER
local MAGIC_MAXIMUM_BASE_NUMBER = constants.MAGIC_MAXIMUM_BASE_NUMBER

-- imported functions

local euclideanDistancePoints = mathUtils.euclideanDistancePoints

local buildHive = buildUtils.buildHive

local mFloor = math.floor

local buildTendril = tendrilUtils.buildTendril

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

function baseUtils.createBase(regionMap, natives, position, surface, tick)
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
	pattern = math.random(MAGIC_MAXIMUM_BASE_NUMBER),
	level = mFloor(distance / 200)
    }
    if not buildHive(regionMap, base, surface) then
	return nil
    end
    buildTendril(regionMap, natives, base, surface, tick)
    bases[#bases+1] = base
    return base
end

return baseUtils

