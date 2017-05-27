local baseUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")
local mathUtils = require("MathUtils")
local entityUtils = require("EntityUtils")

-- constants

local BASE_DISTANCE_THRESHOLD = constants.BASE_DISTANCE_THRESHOLD

local BASE_ALIGNMENT_NEUTRAL = constants.BASE_ALIGNMENT_NEUTRAL

local AI_NEST_COST = constants.AI_NEST_COST
local AI_WORM_COST = constants.AI_WORM_COST

local CHUNK_SIZE = constants.CHUNK_SIZE

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER
local MAGIC_MAXIMUM_BASE_NUMBER = constants.MAGIC_MAXIMUM_BASE_NUMBER

-- imported functions

local euclideanDistancePoints = mapUtils.euclideanDistancePoints

local gaussianRandomRange = mathUtils.gaussianRandomRange

local addEnemyBase = entityUtils.addEnemyBase

-- module code

function baseUtils.annexNest(natives, position)
    local bases = natives.bases
    local annex = nil
    local closest = MAGIC_MAXIMUM_NUMBER
    for i=1,#bases do
	local base = bases[i]
	local distance = euclideanDistancePoints(base.x, base.y, position.x, position.y)
	if (distance <= BASE_DISTANCE_THRESHOLD) and (distance < closest) then
	    closest = distance
	    annex = base
	end
    end
    return annex
end

function baseUtils.buildHive(regionMap, base, surface)
    local valid = false
    local position = surface.find_non_colliding_position("biter-spawner", {x=base.x, y=base.y}, 2*CHUNK_SIZE, 10)
    if position then
	local biterSpawner = {name="biter-spawner", position=position}
	base.hive = surface.create_entity(biterSpawner)
	addEnemyBase(regionMap, base.hive, base)
	valid = true
    end
    return valid
end

function baseUtils.buildOutpost(natives, base, surface, tick, position)
    
end

function baseUtils.buildTendril(natives, base, surface, tick, startPosition, endPosition)
    
end

function baseUtils.buildOrder(regionMap, natives, base, surface, tick)
    if not base.hive or (base.upgradePoints < 10) then
	return
    end
    
    local generator = natives.randomGenerator
    generator.re_seed(base.pattern)

    for level=0,base.level do
	local slices = (level * 3)
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
	    if surface.can_place_entity(biterSpawner) then
		addEnemyBase(regionMap, surface.create_entity(biterSpawner), base)
		base.upgradePoints = base.upgradePoints - cost
	    end
	    pos = pos + slice 
	end
    end    
end

function baseUtils.createBase(regionMap, natives, position, surface, tick)
    local bases = natives.bases
    local base = {
	x = position.x,
	y = position.y,
	created = tick,
	alignment = { BASE_ALIGNMENT_NEUTRAL },
	hive = nil,
	nests = {},
	worms = {},
	eggs = {},
	upgradePoints = 0,
	growth = tick,
	pattern = math.random(MAGIC_MAXIMUM_BASE_NUMBER),
	level = 3
    }
    if not baseUtils.buildHive(regionMap, base, surface) then
	return nil
    end	
    bases[#bases+1] = base
    return base
end

return baseUtils

