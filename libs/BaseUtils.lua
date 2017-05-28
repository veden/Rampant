local baseUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")
local mathUtils = require("MathUtils")

local entityUtils = require("EntityUtils")

-- constants

local BASE_DISTANCE_THRESHOLD = constants.BASE_DISTANCE_THRESHOLD

local BASE_ALIGNMENT_NEUTRAL = constants.BASE_ALIGNMENT_NEUTRAL

local NEST_COUNT = constants.NEST_COUNT
local WORM_COUNT = constants.WORM_COUNT

local NEST_BASE = constants.NEST_BASE
local WORM_BASE = constants.WORM_BASE

local AI_NEST_COST = constants.AI_NEST_COST
local AI_WORM_COST = constants.AI_WORM_COST

local CHUNK_SIZE = constants.CHUNK_SIZE

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER
local MAGIC_MAXIMUM_BASE_NUMBER = constants.MAGIC_MAXIMUM_BASE_NUMBER

-- imported functions

local euclideanDistancePoints = mapUtils.euclideanDistancePoints

local getEntityOverlapChunks = entityUtils.getEntityOverlapChunks

local gaussianRandomRange = mathUtils.gaussianRandomRange

-- module code

function baseUtils.findNearbyBase(natives, position)
    local bases = natives.bases
    local foundBase
    local closest = MAGIC_MAXIMUM_NUMBER
    for i=1,#bases do
	local base = bases[i]
	local distance = euclideanDistancePoints(base.x, base.y, position.x, position.y)
	if (distance <= BASE_DISTANCE_THRESHOLD) and (distance < closest) then
	    closest = distance
	    foundBase = base
	end
    end
    return foundBase
end

function baseUtils.buildHive(regionMap, base, surface)
    local valid = false
    local position = surface.find_non_colliding_position("biter-spawner-hive", {x=base.x, y=base.y}, 2*CHUNK_SIZE, 2)
    if position then
	local biterSpawner = {name="biter-spawner-hive", position=position}
	base.hives[#base.hives+1] = surface.create_entity(biterSpawner)
	baseUtils.registerEnemyBaseStructure(regionMap, base.hive, base)
	valid = true
    end
    return valid
end

function baseUtils.buildOutpost(natives, base, surface, tick, position)
    
end

function baseUtils.buildTendril(natives, base, surface, tick, startPosition, endPosition)
    
end

function baseUtils.buildOrder(regionMap, natives, base, surface, tick)
    if (#base.hives == 0) or (base.upgradePoints < 10) then
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
		baseUtils.registerEnemyBaseStructure(regionMap, surface.create_entity(biterSpawner), base)
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
	hives = {},
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

function baseUtils.addEnemyStructureToChunk(chunk, entity, base)
    local indexChunk
    local indexBase
    local countChunk
    if (entity.type == "unit-spawner") then
	indexChunk = chunk[NEST_BASE]
	if base then
	    indexBase = base.nests
	end
	countChunk = NEST_COUNT
    elseif (entity.type == "turret") then
	indexChunk = chunk[WORM_BASE]
	if base then
	    indexBase = base.worms
	end
	countChunk = WORM_COUNT
    end
    chunk[countChunk] = chunk[countChunk] + 1
    if indexBase then
	indexChunk[entity.unit_number] = base
	indexBase[entity.unit_number] = entity
    end
end

function baseUtils.removeEnemyStructureFromChunk(chunk, entity)
    local indexChunk
    local countChunk
    if (entity.type == "unit-spawner") then
	indexChunk = chunk[NEST_BASE]
	countChunk = NEST_COUNT
    elseif (entity.type == "turret") then
	indexChunk = chunk[WORM_BASE]
	countChunk = WORM_COUNT
    end
    local base = indexChunk[entity.unit_number]
    local indexBase
    if base then
	if (entity.type == "unit-spawner") then
	    indexBase = base.nests
	elseif (entity.type == "turret") then
	    indexBase = base.worms
	end
	indexBase[entity.unit_number] = nil
    end
    chunk[countChunk] = chunk[countChunk] - 1
end

function baseUtils.registerEnemyBaseStructure(regionMap, entity, base)
    local entityType = entity.type
    if ((entityType == "unit-spawner") or (entityType == "turret")) and (entity.force.name == "enemy") then
        local leftTop, rightTop, leftBottom, rightBottom = getEntityOverlapChunks(regionMap, entity)
	
	if leftTop then
	    baseUtils.addEnemyStructureToChunk(leftTop, entity, base)
	end
	if rightTop then
	    baseUtils.addEnemyStructureToChunk(rightTop, entity, base)
	end
	if leftBottom then
	    baseUtils.addEnemyStructureToChunk(leftBottom, entity, base)
	end
	if rightBottom then
	    baseUtils.addEnemyStructureToChunk(rightBottom, entity, base)
	end	
    end
end

function baseUtils.unregisterEnemyBaseStructure(regionMap, entity)
    local entityType = entity.type
    if ((entityType == "unit-spawner") or (entityType == "turret")) and (entity.force.name == "enemy") then
	local leftTop, rightTop, leftBottom, rightBottom = getEntityOverlapChunks(regionMap, entity)
	
	if leftTop then
	    baseUtils.removeEnemyStructureFromChunk(leftTop, entity)
	end
	if rightTop then
	    baseUtils.removeEnemyStructureFromChunk(rightTop, entity)
	end
	if leftBottom then
	    baseUtils.removeEnemyStructureFromChunk(leftBottom, entity)
	end
	if rightBottom then
	    baseUtils.removeEnemyStructureFromChunk(rightBottom, entity)
	end
    end
end


return baseUtils

