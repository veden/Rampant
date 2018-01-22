local baseUtils = {}

-- imports

local stringUtils = require("StringUtils")
local mathUtils = require("MathUtils")
local constants = require("Constants")
local mapUtils = require("MapUtils")
local chunkPropertyUtils = require("ChunkPropertyUtils")

-- constants

local NEUTRAL_WORM_TIERS = constants.NEUTRAL_WORM_TIERS
local NEUTRAL_WORM_VARIATIONS = constants.NEUTRAL_WORM_VARIATIONS
local NEUTRAL_NEST_TIERS = constants.NEUTRAL_NEST_TIERS
local NEUTRAL_NEST_VARIATIONS = constants.NEUTRAL_NEST_VARIATIONS

local PHYSICAL_WORM_TIERS = constants.PHYSICAL_WORM_TIERS
local PHYSICAL_WORM_VARIATIONS = constants.PHYSICAL_WORM_VARIATIONS
local PHYSICAL_NEST_TIERS = constants.PHYSICAL_NEST_TIERS
local PHYSICAL_NEST_VARIATIONS = constants.PHYSICAL_NEST_VARIATIONS

local ELECTRIC_WORM_TIERS = constants.ELECTRIC_WORM_TIERS
local ELECTRIC_WORM_VARIATIONS = constants.ELECTRIC_WORM_VARIATIONS
local ELECTRIC_NEST_TIERS = constants.ELECTRIC_NEST_TIERS
local ELECTRIC_NEST_VARIATIONS = constants.ELECTRIC_NEST_VARIATIONS

local ACID_WORM_TIERS = constants.ACID_WORM_TIERS
local ACID_WORM_VARIATIONS = constants.ACID_WORM_VARIATIONS
local ACID_NEST_TIERS = constants.ACID_NEST_TIERS
local ACID_NEST_VARIATIONS = constants.ACID_NEST_VARIATIONS

local SUICIDE_WORM_TIERS = constants.SUICIDE_WORM_TIERS
local SUICIDE_WORM_VARIATIONS = constants.SUICIDE_WORM_VARIATIONS
local SUICIDE_NEST_TIERS = constants.SUICIDE_NEST_TIERS
local SUICIDE_NEST_VARIATIONS = constants.SUICIDE_NEST_VARIATIONS

local NUCLEAR_WORM_TIERS = constants.NUCLEAR_WORM_TIERS
local NUCLEAR_WORM_VARIATIONS = constants.NUCLEAR_WORM_VARIATIONS
local NUCLEAR_NEST_TIERS = constants.NUCLEAR_NEST_TIERS
local NUCLEAR_NEST_VARIATIONS = constants.NUCLEAR_NEST_VARIATIONS


local BASE_ALIGNMENT_NEUTRAL = constants.BASE_ALIGNMENT_NEUTRAL
local BASE_ALIGNMENT_ACID = constants.BASE_ALIGNMENT_ACID
local BASE_ALIGNMENT_ELECTRIC = constants.BASE_ALIGNMENT_ELECTRIC
local BASE_ALIGNMENT_PHYSICAL = constants.BASE_ALIGNMENT_PHYSICAL
local BASE_ALIGNMENT_SUICIDE = constants.BASE_ALIGNMENT_SUICIDE
local BASE_ALIGNMENT_NUCLEAR = constants.BASE_ALIGNMENT_NUCLEAR

local BASE_WORM_UPGRADE = constants.BASE_WORM_UPGRADE
local BASE_SPAWNER_UPGRADE = constants.BASE_SPAWNER_UPGRADE
local BASE_UPGRADE = constants.BASE_UPGRADE

local BASE_DISTANCE_THRESHOLD = constants.BASE_DISTANCE_THRESHOLD
local BASE_DISTANCE_LEVEL_BONUS = constants.BASE_DISTANCE_LEVEL_BONUS
local BASE_DISTANCE_TO_EVO_INDEX = constants.BASE_DISTANCE_TO_EVO_INDEX

local CHUNK_SIZE = constants.CHUNK_SIZE

local BASE_ALIGNMENT_PATHS = constants.BASE_ALIGNMENT_PATHS

local EVOLUTION_INCREMENTS = constants.EVOLUTION_INCREMENTS

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

-- imported functions

local euclideanDistancePoints = mathUtils.euclideanDistancePoints
local roundToFloor = mathUtils.roundToFloor

local isRampant = stringUtils.isRampant

local gaussianRandomRange = mathUtils.gaussianRandomRange

local mFloor = math.floor

local mMin = math.min
local mMax = math.max

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

local function findUpgrade(base, evoIndex, natives, evolutionTable)
    -- local used = { }
    -- local alignments = base.alignments

    -- local alignmentPick = base.alignments[mRandom(#base.alignments)]
    local alignmentPick = base.alignment
    
    -- while alignmentPick do
    -- 	used[alignmentPick] = true

    if not evolutionTable then
	print(alignmentPick)
    end
    
    for evo=evoIndex, 0, -EVOLUTION_INCREMENTS do
	local entitySet = evolutionTable[alignmentPick][evo]
	if entitySet and (#entitySet > 0) then
	    return entitySet[mRandom(#entitySet)]
	end
    end

    -- alignmentPick = nil
    -- for x = #alignments, 1, -1 do
    --     if not used[alignments[x]] then
    -- 	alignmentPick = alignments[x]
    -- 	break
    --     end
    -- end
    -- end
    
    return nil
end

function baseUtils.upgradeEntity(map, entity, surface, natives, evolutionFactor, tick)
    local position = entity.position
    local entityType = entity.type
    entity.destroy()
    local chunk = getChunkByPosition(map, position)
    local base = getChunkBase(map, chunk)

    if not base then
	base = baseUtils.createBase(map, natives, evolutionFactor, chunk, surface, tick)
    end

    local distance = roundToFloor(mMin(1,
				       euclideanDistancePoints(position.x, position.y, 0, 0) * BASE_DISTANCE_TO_EVO_INDEX),
				  EVOLUTION_INCREMENTS)
    local evoIndex = mMax(distance, roundToFloor(evolutionFactor, EVOLUTION_INCREMENTS))

    local spawnerName = findUpgrade(base, evoIndex, natives, ((entityType == "unit-spawner") and natives.evolutionTableUnitSpawner) or natives.evolutionTableWorm)
    if spawnerName then
	local newPosition = surface.find_non_colliding_position(spawnerName, position, CHUNK_SIZE, 4)
	if newPosition then
	    entity = surface.create_entity({name = spawnerName, position = newPosition})
	end
    end

    return entity
end

function baseUtils.upgradeBase(base)
    local paths = BASE_ALIGNMENT_PATHS[base.alignment]
    if paths then
	base.alignment = paths[mRandom(#paths)]
	return true
    end
    return false
end

function baseUtils.processBase(map, surface, natives, tick, base, evolutionFactor)
    local areaTop = map.position2Top
    local areaBottom = map.position2Bottom

    areaTop[1] = base.x
    areaTop[2] = base.y

    areaBottom[1] = base.x + CHUNK_SIZE
    areaBottom[2] = base.y + CHUNK_SIZE
    
    local entity
    local cost
    local choice = mRandom()
    if (choice <= 0.3) then
	if (base.points >= BASE_SPAWNER_UPGRADE) then
	    entity = surface.find_entities_filtered(map.filteredEntitiesSpawnerQueryLimited)
	    cost = BASE_SPAWNER_UPGRADE
	end
    elseif (choice <= 0.6) then
	if (base.points >= BASE_WORM_UPGRADE) then
	    entity = surface.find_entities_filtered(map.filteredEntitiesWormQueryLimited)
	    cost = BASE_WORM_UPGRADE
	end
    elseif (choice >= 0.995) then
	if (base.points >= BASE_UPGRADE) then
	    if baseUtils.upgradeBase(base) then
		base.points = base.points - BASE_UPGRADE
	    end
	end
    end

    if entity and (#entity > 0) then
	baseUtils.upgradeEntity(map, entity[mRandom(#entity)], surface, natives, evolutionFactor, tick)
	base.points = base.points - cost
    end

    base.points = base.points + natives.baseIncrement

    base.tick = tick
end

function baseUtils.createBase(map, natives, evolutionFactor, chunk, surface, tick)
    local x = chunk.x
    local y = chunk.y
    local distance = euclideanDistancePoints(x, y, 0, 0)

    local meanLevel = mFloor(distance / 200)

    -- if then
    -- end
    --local alignments = { BASE_ALIGNMENT_NEUTRAL }
    local alignment = BASE_ALIGNMENT_NEUTRAL
    
    local base = {
	x = x,
	y = y,
	tick = tick,
	alignment = alignment,
	points = 0,
	level = gaussianRandomRange(meanLevel, meanLevel * 0.3, meanLevel * 0.50, meanLevel * 1.50)
    }

    setChunkBase(map, chunk, base)
    
    -- if not buildHive(map, base, surface) then
    -- 	return nil
    -- end

    natives.bases[#natives.bases+1] = base
    
    return base
end

local function fileEntity(baseAlignment, entity, evolutionTable)
    local evoRequirement = mFloor(entity.prototype.build_base_evolution_requirement/EVOLUTION_INCREMENTS) * EVOLUTION_INCREMENTS
    local eTable = evolutionTable[baseAlignment]
    if not eTable then
	eTable = {}
	evolutionTable[baseAlignment] = eTable
    end
    local aTable = eTable[evoRequirement]
    if not aTable then
	aTable = {}
	eTable[evoRequirement] = aTable
    end
    aTable[#aTable+1] = entity.name
end

local function processUnitClass(biterVariation, biterTier, spitterVariation, spitterTier, wormVariation, wormTier, surface, natives, baseAlignment, baseAlignmentString)
    local position = { x = 0, y = 0 }
    
    for v=1,biterVariation do
    	for t=1,biterTier do
	    local entity = surface.create_entity({
		    name= baseAlignmentString .. "-biter-nest-v" .. v .. "-t" .. t .. "-rampant",
		    position = position
	    })
	    fileEntity(baseAlignment, entity, natives.evolutionTableUnitSpawner)
	    entity.destroy()
    	end
    end
    for v=1,spitterVariation do
    	for t=1,spitterTier do
	    local entity = surface.create_entity({
		    name=baseAlignmentString .. "-spitter-nest-v" .. v .. "-t" .. t .. "-rampant",
		    position = position
	    })
	    fileEntity(baseAlignment, entity, natives.evolutionTableUnitSpawner)
	    entity.destroy()
    	end
    end
    for v=1,wormVariation do
    	for t=1,wormTier do
	    local entity = surface.create_entity({
		    name=baseAlignmentString .. "-worm-v" .. v .. "-t" .. t .. "-rampant",
		    position = position
	    })
	    fileEntity(baseAlignment, entity, natives.evolutionTableWorm)
	    entity.destroy()
    	end
    end
end

function baseUtils.rebuildNativeTables(natives, surface)
    natives.evolutionTableUnitSpawner = {}
    natives.evolutionTableWorm = {}
    
    processUnitClass(NEUTRAL_NEST_VARIATIONS,
		     NEUTRAL_NEST_TIERS,
		     NEUTRAL_NEST_VARIATIONS,
		     NEUTRAL_NEST_TIERS,
		     NEUTRAL_WORM_VARIATIONS,
		     NEUTRAL_WORM_TIERS,
		     surface,
		     natives,
		     BASE_ALIGNMENT_NEUTRAL,
		     "neutral")

    processUnitClass(ACID_NEST_VARIATIONS,
		     ACID_NEST_TIERS,
		     ACID_NEST_VARIATIONS,
		     ACID_NEST_TIERS,
		     ACID_WORM_VARIATIONS,
		     ACID_WORM_TIERS,
		     surface,
		     natives,
		     BASE_ALIGNMENT_ACID,
		     "acid")

    processUnitClass(PHYSICAL_NEST_VARIATIONS,
		     PHYSICAL_NEST_TIERS,
		     0,
		     0,
		     PHYSICAL_WORM_VARIATIONS,
		     PHYSICAL_WORM_TIERS,
		     surface,
		     natives,
		     BASE_ALIGNMENT_PHYSICAL,
		     "physical")

    -- processUnitClass(FIRE_NEST_VARIATIONS,
    -- 		     FIRE_NEST_TIERS,
    -- 		     FIRE_NEST_VARIATIONS,
    -- 		     FIRE_NEST_TIERS,
    -- 		     FIRE_WORM_VARIATIONS,
    -- 		     FIRE_WORM_TIERS,
    -- 		     surface,
    -- 		     natives,
    -- 		     BASE_ALIGNMENT_FIRE,
    -- 		     "fire")

    processUnitClass(ELECTRIC_NEST_VARIATIONS,
		     ELECTRIC_NEST_TIERS,
		     0,
		     0,
		     ELECTRIC_WORM_VARIATIONS,
		     ELECTRIC_WORM_TIERS,
		     surface,
		     natives,
		     BASE_ALIGNMENT_ELECTRIC,
		     "electric")

    processUnitClass(SUICIDE_NEST_VARIATIONS,
    		     SUICIDE_NEST_TIERS,
		     0,
		     0,
    		     SUICIDE_WORM_VARIATIONS,
    		     SUICIDE_WORM_TIERS,
    		     surface,
    		     natives,
    		     BASE_ALIGNMENT_SUICIDE,
    		     "suicide")

    processUnitClass(NUCLEAR_NEST_VARIATIONS,
    		     NUCLEAR_NEST_TIERS,
		     0,
		     0,
    		     NUCLEAR_WORM_VARIATIONS,
    		     NUCLEAR_WORM_TIERS,
    		     surface,
    		     natives,
    		     BASE_ALIGNMENT_NUCLEAR,
    		     "nuclear")

    -- processUnitClass(TROLL_NEST_VARIATIONS,
    -- 		     TROLL_NEST_TIERS,
    -- 		     TROLL_NEST_VARIATIONS,
    -- 		     TROLL_NEST_TIERS,
    -- 		     TROLL_WORM_VARIATIONS,
    -- 		     TROLL_WORM_TIERS,
    -- 		     surface,
    -- 		     natives,
    -- 		     BASE_ALIGNMENT_TROLL,
    -- 		     "troll")

    -- processUnitClass(INFERNO_NEST_VARIATIONS,
    -- 		     INFERNO_NEST_TIERS,
    -- 		     INFERNO_NEST_VARIATIONS,
    -- 		     INFERNO_NEST_TIERS,
    -- 		     INFERNO_WORM_VARIATIONS,
    -- 		     INFERNO_WORM_TIERS,
    -- 		     surface,
    -- 		     natives,
    -- 		     BASE_ALIGNMENT_INFERNO,
    -- 		     "inferno")

    -- processUnitClass(FAST_NEST_VARIATIONS,
    -- 		     FAST_NEST_TIERS,
    -- 		     FAST_NEST_VARIATIONS,
    -- 		     FAST_NEST_TIERS,
    -- 		     FAST_WORM_VARIATIONS,
    -- 		     FAST_WORM_TIERS,
    -- 		     surface,
    -- 		     natives,
    -- 		     BASE_ALIGNMENT_FAST,
    -- 		     "fast")

    -- processUnitClass(DECAYING_NEST_VARIATIONS,
    -- 		     DECAYING_NEST_TIERS,
    -- 		     DECAYING_NEST_VARIATIONS,
    -- 		     DECAYING_NEST_TIERS,
    -- 		     DECAYING_WORM_VARIATIONS,
    -- 		     DECAYING_WORM_TIERS,
    -- 		     surface,
    -- 		     natives,
    -- 		     BASE_ALIGNMENT_DECAYING,
    -- 		     "decaying")

    -- processUnitClass(UNDYING_NEST_VARIATIONS,
    -- 		     UNDYING_NEST_TIERS,
    -- 		     UNDYING_NEST_VARIATIONS,
    -- 		     UNDYING_NEST_TIERS,
    -- 		     UNDYING_WORM_VARIATIONS,
    -- 		     UNDYING_WORM_TIERS,
    -- 		     surface,
    -- 		     natives,
    -- 		     BASE_ALIGNMENT_UNDYING,
    -- 		     "undying")

    -- processUnitClass(POSION_NEST_VARIATIONS,
    -- 		     POSION_NEST_TIERS,
    -- 		     POSION_NEST_VARIATIONS,
    -- 		     POSION_NEST_TIERS,
    -- 		     POSION_WORM_VARIATIONS,
    -- 		     POSION_WORM_TIERS,
    -- 		     surface,
    -- 		     natives,
    -- 		     BASE_ALIGNMENT_POSION,
    -- 		     "posion")

    -- processUnitClass(LASER_NEST_VARIATIONS,
    -- 		     LASER_NEST_TIERS,
    -- 		     LASER_NEST_VARIATIONS,
    -- 		     LASER_NEST_TIERS,
    -- 		     LASER_WORM_VARIATIONS,
    -- 		     LASER_WORM_TIERS,
    -- 		     surface,
    -- 		     natives,
    -- 		     BASE_ALIGNMENT_LASER,
    -- 		     "laser")

    -- processUnitClass(WASP_NEST_VARIATIONS,
    -- 		     WASP_NEST_TIERS,
    -- 		     WASP_NEST_VARIATIONS,
    -- 		     WASP_NEST_TIERS,
    -- 		     WASP_WORM_VARIATIONS,
    -- 		     WASP_WORM_TIERS,
    -- 		     surface,
    -- 		     natives,
    -- 		     BASE_ALIGNMENT_WASP,
    -- 		     "wasp")
end

return baseUtils

