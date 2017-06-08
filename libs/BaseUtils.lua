local baseUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")
local mathUtils = require("MathUtils")

local baseRegisterUtils = require("BaseRegisterUtils")

local tendrilUtils = require("TendrilUtils")

-- constants

local BASE_DISTANCE_THRESHOLD = constants.BASE_DISTANCE_THRESHOLD

local BASE_ALIGNMENT_NEUTRAL = constants.BASE_ALIGNMENT_NEUTRAL

local AI_NEST_COST = constants.AI_NEST_COST
local AI_WORM_COST = constants.AI_WORM_COST

local DOUBLE_CHUNK_SIZE = constants.DOUBLE_CHUNK_SIZE

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER
local MAGIC_MAXIMUM_BASE_NUMBER = constants.MAGIC_MAXIMUM_BASE_NUMBER

-- imported functions

local euclideanDistancePoints = mapUtils.euclideanDistancePoints

local gaussianRandomRange = mathUtils.gaussianRandomRange

local mFloor = math.floor

local buildTendril = tendrilUtils.buildTendril

local registerEnemyBaseStructure = baseRegisterUtils.registerEnemyBaseStructure

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

function baseUtils.buildHive(regionMap, base, surface)
    local valid = false
    local position = surface.find_non_colliding_position("biter-spawner-hive", base, DOUBLE_CHUNK_SIZE, 2)
    if position then
	local biterSpawner = {name="biter-spawner-hive", position=position}
	local hive = surface.create_entity(biterSpawner)
	if (#base.hives == 0) then
	    base.x = hive.position.x
	    base.y = hive.position.y
	end
	base.hives[#base.hives+1] = hive
	registerEnemyBaseStructure(regionMap, hive, base)
	valid = true
    end
    return valid
end

function baseUtils.buildOutpost(natives, base, surface, tick, position)
    
end

function baseUtils.buildOrder(regionMap, natives, base, surface, tick)
    local foundHive = false
    for _,_ in pairs(base.hives) do
	foundHive = true
	break
    end
    if not foundHive or (base.upgradePoints < 10) then
	return
    end
    
    local generator = natives.randomGenerator
    generator.re_seed(base.pattern)

    for level=0,base.level do
	local slices = (level * 3)
	slices = gaussianRandomRange(slices, slices * 0.1, slices * 0.6, slices * 1.4, generator)
	local slice = (2 * math.pi) / slices
	local pos = 0
	local thing
	local cost
	local radiusAdjustment
	if (generator() < 0.3) then
	    thing = "small-worm-turret"
	    cost = AI_WORM_COST
	    radiusAdjustment = -4
	else
	    thing = "biter-spawner"
	    cost = AI_NEST_COST
	    radiusAdjustment = 0
	end
	for _ = 1, slices do
	    if (base.upgradePoints < 10) then
		return
	    end
	    local radius = 10 * level
	    local distortion = gaussianRandomRange(radius, 10, radius - 7.5 + radiusAdjustment, radius + 7.5 + radiusAdjustment, generator)
	    local nestPosition = {x = base.x + (distortion * math.cos(pos)),
				  y = base.y + (distortion * math.sin(pos))}
	    local biterSpawner = {name=thing, position=nestPosition}
	    if surface.can_place_entity(biterSpawner)  then
		registerEnemyBaseStructure(regionMap, surface.create_entity(biterSpawner), base)
		base.upgradePoints = base.upgradePoints - cost
	    end
	    pos = pos + slice 
	end
    end    
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
    if not baseUtils.buildHive(regionMap, base, surface) then
	return nil
    end
    buildTendril(regionMap, natives, base, surface, tick)
    bases[#bases+1] = base
    return base
end

return baseUtils

