if baseUtilsG then
    return baseUtilsG
end
local baseUtils = {}

-- imports

local mathUtils = require("MathUtils")
local constants = require("Constants")
local chunkPropertyUtils = require("ChunkPropertyUtils")
local neUnits = require("NEBaseUtils")
local bobsUnits = require("BobsBaseUtils")

-- constants

local BUILDING_SPACE_LOOKUP = constants.BUILDING_SPACE_LOOKUP

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

local BASE_AI_STATE_DORMANT = constants.BASE_AI_STATE_DORMANT
local BASE_AI_STATE_ACTIVE = constants.BASE_AI_STATE_ACTIVE
local BASE_AI_STATE_WORMS = constants.BASE_AI_STATE_WORMS
local BASE_AI_STATE_NESTS = constants.BASE_AI_STATE_NESTS
local BASE_AI_STATE_OVERDRIVE = constants.BASE_AI_STATE_OVERDRIVE
local BASE_AI_STATE_MUTATE = constants.BASE_AI_STATE_MUTATE

local BASE_DEADZONE_TTL = constants.BASE_DEADZONE_TTL

local ENABLED_BOBS_UNITS = constants.ENABLED_BOBS_UNITS
local ENABLED_NE_UNITS = constants.ENABLED_NE_UNITS

local BASE_ALIGNMENT_SPAWNER = constants.BASE_ALIGNMENT_SPAWNER
local BASE_ALIGNMENT_WASP = constants.BASE_ALIGNMENT_WASP
local BASE_ALIGNMENT_NEUTRAL = constants.BASE_ALIGNMENT_NEUTRAL
local BASE_ALIGNMENT_ACID = constants.BASE_ALIGNMENT_ACID
local BASE_ALIGNMENT_ELECTRIC = constants.BASE_ALIGNMENT_ELECTRIC
local BASE_ALIGNMENT_PHYSICAL = constants.BASE_ALIGNMENT_PHYSICAL
local BASE_ALIGNMENT_SUICIDE = constants.BASE_ALIGNMENT_SUICIDE
local BASE_ALIGNMENT_NUCLEAR = constants.BASE_ALIGNMENT_NUCLEAR
local BASE_ALIGNMENT_INFERNO = constants.BASE_ALIGNMENT_INFERNO
local BASE_ALIGNMENT_FIRE = constants.BASE_ALIGNMENT_FIRE
local BASE_ALIGNMENT_FAST = constants.BASE_ALIGNMENT_FAST
local BASE_ALIGNMENT_LASER = constants.BASE_ALIGNMENT_LASER
local BASE_ALIGNMENT_TROLL = constants.BASE_ALIGNMENT_TROLL
local BASE_ALIGNMENT_ENERGY_THIEF = constants.BASE_ALIGNMENT_ENERGY_THIEF
local BASE_ALIGNMENT_DEADZONE = constants.BASE_ALIGNMENT_DEADZONE
local BASE_ALIGNMENT_POISON = constants.BASE_ALIGNMENT_POISON

local BASE_AI_MIN_STATE_DURATION = constants.BASE_AI_MIN_STATE_DURATION
local BASE_AI_MIN_TEMPERAMENT_DURATION = constants.BASE_AI_MIN_TEMPERAMENT_DURATION
local BASE_AI_MAX_STATE_DURATION = constants.BASE_AI_MAX_STATE_DURATION
local BASE_AI_MAX_TEMPERAMENT_DURATION = constants.BASE_AI_MAX_TEMPERAMENT_DURATION

local BASE_WORM_UPGRADE = constants.BASE_WORM_UPGRADE
local BASE_SPAWNER_UPGRADE = constants.BASE_SPAWNER_UPGRADE
local BASE_UPGRADE = constants.BASE_UPGRADE

local BASE_DISTANCE_THRESHOLD = constants.BASE_DISTANCE_THRESHOLD
local BASE_DISTANCE_LEVEL_BONUS = constants.BASE_DISTANCE_LEVEL_BONUS
local BASE_DISTANCE_TO_EVO_INDEX = constants.BASE_DISTANCE_TO_EVO_INDEX

local BASE_ALIGNMENT_EVOLUTION_BASELINE = constants.BASE_ALIGNMENT_EVOLUTION_BASELINE

local BASE_QUEUE_SIZE = constants.BASE_QUEUE_SIZE
local BASE_COLLECTION_THRESHOLD = constants.BASE_COLLECTION_THRESHOLD

local CHUNK_SIZE = constants.CHUNK_SIZE
local CHUNK_AND_HALF_SIZE = constants.CHUNK_AND_HALF_SIZE

local EVOLUTION_INCREMENTS = constants.EVOLUTION_INCREMENTS

-- local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

-- imported functions

local randomTickEvent = mathUtils.randomTickEvent
local euclideanDistancePoints = mathUtils.euclideanDistancePoints
local roundToFloor = mathUtils.roundToFloor

local processNEUnitClass = neUnits.processNEUnitClass
local processBobsUnitClass = bobsUnits.processBobsUnitClass

local gaussianRandomRange = mathUtils.gaussianRandomRange
local gaussianRandomRangeRG = mathUtils.gaussianRandomRangeRG

local mFloor = math.floor

local mMin = math.min
local mMax = math.max

local getChunkBase = chunkPropertyUtils.getChunkBase
local setChunkBase = chunkPropertyUtils.setChunkBase

local tRemove = table.remove

local mRandom = math.random

-- module code

local function nonRepeatingRandom(natives, evoTable, rg)
    local ordering = natives.baseOrdering
    local baseCount = 0
    for evo in pairs(evoTable) do
        baseCount = baseCount + 1
        ordering[baseCount] = evo
    end
    for i=baseCount,1,-1 do
        local s = rg(i)
        local t = ordering[i]
        ordering[i] = ordering[s]
        ordering[s] = t
    end
    ordering.len = baseCount
    return ordering
end

local function sortNormals(a, b)
    return a[1] < b[1]
end

local function normalizeProbabilities(probabilityTable)
    local result = {}

    for alignment,probabilitySet in pairs(probabilityTable) do
        local max = 0
        local min = MAGIC_MAXIMUM_NUMBER

        for probability, _ in pairs(probabilitySet) do
            if (probability > max) then
                max = probability
            end
            if (probability < min) then
                min = probability
            end
        end

        local alignmentResult = {}
        for probability, entities in pairs(probabilitySet) do
            local normalizeProbability = 0
            if (probability ~= 0) then
                normalizeProbability = mMin(mFloor(((probability - min) / (max - min)) * 100), 97)
            end
            local set = alignmentResult[normalizeProbability]
            if (not set) then
                set = {}
                alignmentResult[normalizeProbability] = set
            end
            for i=1,#entities do
                set[#set+1] = entities[i]
            end
        end

        local paired = {}
        for probability, entities in pairs(alignmentResult) do
            paired[#paired+1] = {probability, entities}
        end
        result[alignment] = paired

        table.sort(paired, sortNormals)
    end

    return result
end

function baseUtils.findNearbyBase(map, chunk, natives)
    local x = chunk.x
    local y = chunk.y

    local foundBase = getChunkBase(map, chunk)
    if foundBase then
        return foundBase
    end

    local bases = natives.bases
    local closet = MAGIC_MAXIMUM_NUMBER
    for i=1, #bases do
        local base = bases[i]
        local distance = euclideanDistancePoints(base.x, base.y, x, y)
        if (distance <= base.distanceThreshold) and (distance < closet) then
            closet = distance
            foundBase = base
        end
    end

    return foundBase
end

local function findEntityUpgrade(baseAlignment, currentEvo, evoIndex, entityAlignment, evolutionTable)

    local alignments = evolutionTable[baseAlignment]

    local adjCurrentEvo = mMax(
        ((baseAlignment ~= entityAlignment) and 0) or currentEvo,
        0
    )

    if not alignments or (adjCurrentEvo > evoIndex) then
        return nil
    end

    local entity = nil

    if (evoIndex >= 0.5) then
        for i=#alignments,1,-1 do
            local pair = alignments[i]
            local evo = pair[1]
            local entitySet = pair[2]
            if (evo <= evoIndex) then
                if (evo < adjCurrentEvo) then
                    break
                end
                entity = entitySet[mRandom(#entitySet)]
                if (mRandom() < 0.25) then
                    break
                end
            end
        end
    else
        for i=1,#alignments do
            local pair = alignments[i]
            local evo = pair[1]
            local entitySet = pair[2]
            if (adjCurrentEvo <= evo) then
                if (evo > evoIndex) then
                    break
                end
                entity = entitySet[mRandom(#entitySet)]
                if (mRandom() < 0.25) then
                    break
                end
            end
        end
    end

    return entity
end

local function findBaseInitialAlignment(evoIndex, natives, evolutionTable)

    local evoTop = gaussianRandomRange(evoIndex, evoIndex * 0.3, 0, evoIndex)

    local pickedEvo
    local alignment
    local alignmentTable = natives.evolutionTableAlignmentOrder
    for i=1,alignmentTable.len do
        local evo = alignmentTable[i]
        local entitySet = evolutionTable[evo]
        if (evo <= evoTop) and entitySet and (#entitySet > 0) then
            if not pickedEvo then
                alignment = entitySet[mRandom(#entitySet)]
                pickedEvo = evo
            else
                local diff = pickedEvo - evo
                if (diff > 0.2) then
                    if (mRandom() < 0.10) then
                        alignment = entitySet[mRandom(#entitySet)]
                        break
                    end
                elseif (diff >= 0) then
                    if (mRandom() < 0.15)then
                        alignment = entitySet[mRandom(#entitySet)]
                        break
                    end
                elseif (diff <= -0.2) then
                    if (mRandom() < 0.35)then
                        alignment = entitySet[mRandom(#entitySet)]
                        break
                    end
                elseif (diff < 0) then
                    if (mRandom() < 0.25)then
                        alignment = entitySet[mRandom(#entitySet)]
                        break
                    end
                end
            end
        end
    end

    return alignment
end

function baseUtils.recycleBases(natives, tick)
    local baseIndex = natives.baseIndex
    local bases = natives.bases

    local endIndex = mMin(baseIndex+BASE_QUEUE_SIZE, #bases)
    for index = endIndex, baseIndex, -1 do
        local base = bases[index]

        if ((tick - base.tick) > BASE_COLLECTION_THRESHOLD) then
            tRemove(bases, index)
        end
    end

    if (endIndex == #bases) then
        natives.baseIndex = 1
    else
        natives.baseIndex = endIndex + 1
    end
end


function baseUtils.upgradeEntity(entity, surface, baseAlignment, natives, evolutionFactor)
    local position = entity.position
    local entityType = entity.type
    -- local entityName = entity.name
    local currentEvo = entity.prototype.build_base_evolution_requirement or 0

    if not baseAlignment or (baseAlignment == BASE_ALIGNMENT_DEADZONE) then
        entity.destroy()
        return nil
    end

    local distance = mMin(1, euclideanDistancePoints(position.x, position.y, 0, 0) * BASE_DISTANCE_TO_EVO_INDEX)
    local evoIndex = mMax(distance, evolutionFactor)

    local spawnerName = findEntityUpgrade(baseAlignment,
                                          mFloor(currentEvo * 100),
                                          mFloor(evoIndex * 100),
                                          natives.enemyAlignmentLookup[entity.name],
                                          ((entityType == "unit-spawner") and natives.evolutionTableUnitSpawner) or
                                              natives.evolutionTableWorm)

    if spawnerName then
        entity.destroy()
        local newPosition = surface.find_non_colliding_position(
            BUILDING_SPACE_LOOKUP[spawnerName] or spawnerName,
            position,
            CHUNK_SIZE,
            1,
            true
        )
        if newPosition then
            return surface.create_entity({name = spawnerName, position = newPosition})
        end
    end

    return nil
end

local function findMutation(natives, evolutionFactor)
    local shuffled = nonRepeatingRandom(natives, natives.evolutionTableAlignment, natives.randomGenerator)
    local evoTable = natives.evolutionTableAlignment
    local alignment

    for i=1,shuffled.len do
        local evo = shuffled[i]
        if (evo <= evolutionFactor) then
            local alignmentSet = evoTable[evo]
            alignment = alignmentSet[mRandom(#alignmentSet)]
            break
        end
    end

    return alignment
end

local function upgradeBase(natives, evolutionFactor, base)
    local alignmentCount = #base.alignment

    local roll = mRandom()
    if alignmentCount == 2 then
        if (roll < 0.05) then
            base.alignment = {findMutation(natives, evolutionFactor)}
        elseif (roll < 0.4) then
            base.alignment = {findMutation(natives, evolutionFactor), base.alignment[2]}
        else
            base.alignment = {base.alignment[1], findMutation(natives, evolutionFactor)}
        end
        return true
    elseif alignmentCount == 1 then
        if (roll < 0.85) then
            base.alignment = {findMutation(natives, evolutionFactor)}
        else
            base.alignment = {base.alignment[1], findMutation(natives, evolutionFactor)}
        end
        return true
    end

    return false
end

function baseUtils.processBase(map, chunk, surface, natives, tick, base, evolutionFactor)

    if (base.alignment[1] == BASE_ALIGNMENT_DEADZONE) then
        base.state = BASE_AI_STATE_DORMANT
        return
    end

    local areaTop = map.position2Top
    local areaBottom = map.position2Bottom

    areaTop[1] = chunk.x
    areaTop[2] = chunk.y

    areaBottom[1] = chunk.x + CHUNK_SIZE
    areaBottom[2] = chunk.y + CHUNK_SIZE

    local entity
    local cost
    local choice = mRandom()

    if (base.state == BASE_AI_STATE_NESTS) or ((base.state == BASE_AI_STATE_ACTIVE) and (choice < 0.5)) then
        if (base.points >= BASE_SPAWNER_UPGRADE) then
            entity = surface.find_entities_filtered(map.filteredEntitiesSpawnerQueryLimited)
            cost = BASE_SPAWNER_UPGRADE
        end
    elseif (base.state == BASE_AI_STATE_WORMS) or (base.state == BASE_AI_STATE_ACTIVE) then
        if (base.points >= BASE_WORM_UPGRADE) then
            entity = surface.find_entities_filtered(map.filteredEntitiesWormQueryLimited)
            cost = BASE_WORM_UPGRADE
        end
    elseif (base.state == BASE_AI_STATE_MUTATE) then
        if (base.points >= BASE_UPGRADE) then
            if upgradeBase(natives, evolutionFactor, base) then
                base.points = base.points - BASE_UPGRADE
            end
        end
    end

    if entity and (#entity > 0) then
        baseUtils.upgradeEntity(entity[mRandom(#entity)], surface, base.alignment[mRandom(#base.alignment)], natives, evolutionFactor)
        base.points = base.points - cost
    end

    if (base.state == BASE_AI_STATE_OVERDRIVE) then
        base.points = base.points + (natives.baseIncrement * 5)
    elseif (base.state ~= BASE_AI_STATE_DORMANT) then
        base.points = base.points + natives.baseIncrement
    end

    if (base.temperamentTick <= tick) then
        base.temperament = mRandom()
        base.temperamentTick = randomTickEvent(tick, BASE_AI_MIN_TEMPERAMENT_DURATION, BASE_AI_MAX_TEMPERAMENT_DURATION)
    end

    if (base.stateTick <= tick) then
        local roll = mRandom() * mMax(1 - evolutionFactor, 0.15)
        if (roll > natives.temperament) then
            base.state = BASE_AI_STATE_DORMANT
        else
            roll = mRandom()
            if (roll < 0.70) then
                base.state = BASE_AI_STATE_ACTIVE
            elseif (roll < 0.80) then
                base.state = BASE_AI_STATE_NESTS
            elseif (roll < 0.90) then
                base.state = BASE_AI_STATE_WORMS
            elseif (roll < 0.975) then
                base.state = BASE_AI_STATE_OVERDRIVE
            else
                base.state = BASE_AI_STATE_MUTATE
            end
        end
        base.stateTick = randomTickEvent(tick, BASE_AI_MIN_STATE_DURATION, BASE_AI_MAX_STATE_DURATION)
    end

    base.tick = tick
end

function baseUtils.createBase(map, natives, evolutionFactor, chunk, tick, rebuilding)
    local x = chunk.x
    local y = chunk.y
    local distance = euclideanDistancePoints(x, y, 0, 0)

    local meanLevel = mFloor(distance * 0.005) -- / 200

    local distanceIndex = mMin(1, distance * BASE_DISTANCE_TO_EVO_INDEX)
    local evoIndex = mMax(distanceIndex, evolutionFactor)

    local alignment
    if (not rebuilding) and (mRandom() < natives.deadZoneFrequency) then
        alignment = BASE_ALIGNMENT_DEADZONE
    else
        alignment = findBaseInitialAlignment(evoIndex, natives, natives.evolutionTableAlignment) or BASE_ALIGNMENT_NEUTRAL
    end

    local baseLevel = gaussianRandomRange(meanLevel, meanLevel * 0.3, meanLevel * 0.50, meanLevel * 1.50)
    local baseDistanceThreshold = gaussianRandomRange(BASE_DISTANCE_THRESHOLD, BASE_DISTANCE_THRESHOLD * 0.2, BASE_DISTANCE_THRESHOLD * 0.75, BASE_DISTANCE_THRESHOLD * 1.50)
    local distanceThreshold = (baseLevel * BASE_DISTANCE_LEVEL_BONUS) + baseDistanceThreshold

    local base = {
        x = x,
        y = y,
        distanceThreshold = distanceThreshold,
        tick = ((alignment == BASE_ALIGNMENT_DEADZONE) and BASE_DEADZONE_TTL) or tick,
        alignment = {alignment},
        state = BASE_AI_STATE_DORMANT,
        stateTick = 0,
        temperamentTick = 0,
        createdTick = tick,
        temperament = 0,
        points = 0
    }

    setChunkBase(map, chunk, base)

    natives.bases[#natives.bases+1] = base

    return base
end

local function fileEntity(baseAlignment, entity, evolutionTable)
    local evoRequirement = mMin(entity.prototype.build_base_evolution_requirement, 1)
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

local function fileAlignment(baseAlignment, evolution, evolutionTable)
    local evoRequirement = mMin(evolution, 1)
    local eTable = evolutionTable[evolution]
    if not eTable then
        eTable = {}
        evolutionTable[evoRequirement] = eTable
    end
    eTable[#eTable+1] = baseAlignment
end

local function processUnitClass(hasBiter, hasSpitter, surface, natives, baseAlignment, baseAlignmentString)
    local position = { x = 0, y = 0 }

    if hasBiter then
        for tier=1,10 do
            for v=1,natives.NEST_VARIATIONS do
                local entityName = baseAlignmentString .. "-biter-spawner-v" .. v .. "-t" .. tier .. "-rampant"
                local entity = surface.create_entity({
                        name= entityName,
                        position = position
                })
                natives.enemyAlignmentLookup[entityName] = baseAlignment
                fileEntity(baseAlignment, entity, natives.evolutionTableUnitSpawner)
                entity.destroy()
            end
        end
    end
    if hasSpitter then
        for tier=1,10 do
            for v=1,natives.NEST_VARIATIONS do
                local entityName = baseAlignmentString .. "-spitter-spawner-v" .. v .. "-t" .. tier .. "-rampant"
                local entity = surface.create_entity({
                        name = entityName,
                        position = position
                })
                natives.enemyAlignmentLookup[entityName] = baseAlignment
                fileEntity(baseAlignment, entity, natives.evolutionTableUnitSpawner)
                entity.destroy()
            end
        end
    end
    for tier=1,10 do
        for v=1,natives.WORM_VARIATIONS do
            local entityName = baseAlignmentString .. "-worm-v" .. v .. "-t" .. tier .. "-rampant"
            local entity = surface.create_entity({
                    name=entityName,
                    position = position
            })
            natives.enemyAlignmentLookup[entityName] = baseAlignment
            fileEntity(baseAlignment, entity, natives.evolutionTableWorm)
            entity.destroy()
        end
    end
end

function baseUtils.rebuildNativeTables(natives, surface, rg)
    natives.evolutionTableUnitSpawner = {}
    natives.evolutionTableWorm = {}
    natives.evolutionTableAlignment = {}

    -- todo fill out alignment evolution levels
    for alignment,evo in pairs(BASE_ALIGNMENT_EVOLUTION_BASELINE) do
        fileAlignment(alignment,
                      gaussianRandomRangeRG(evo, evo * 0.2, evo * 0.5, evo * 1.5, rg),
                      natives.evolutionTableAlignment)
    end

    natives.evolutionTableAlignmentOrder = nonRepeatingRandom(natives,
                                                              natives.evolutionTableAlignment,
                                                              natives.randomGenerator)

    if ENABLED_NE_UNITS then
        processNEUnitClass(natives, surface)
    end

    if ENABLED_BOBS_UNITS then
        processBobsUnitClass(natives, surface)
    end

    processUnitClass(true,
                     true,
                     surface,
                     natives,
                     BASE_ALIGNMENT_NEUTRAL,
                     "neutral")

    if settings.startup["rampant-acidEnemy"].value then
        processUnitClass(true,
                         true,
                         surface,
                         natives,
                         BASE_ALIGNMENT_ACID,
                         "acid")
    end

    if settings.startup["rampant-physicalEnemy"].value then
        processUnitClass(true,
                         false,
                         surface,
                         natives,
                         BASE_ALIGNMENT_PHYSICAL,
                         "physical")
    end

    if settings.startup["rampant-fireEnemy"].value then
        processUnitClass(true,
                         true,
                         surface,
                         natives,
                         BASE_ALIGNMENT_FIRE,
                         "fire")
    end

    if settings.startup["rampant-electricEnemy"].value then
        processUnitClass(true,
                         false,
                         surface,
                         natives,
                         BASE_ALIGNMENT_ELECTRIC,
                         "electric")
    end

    if settings.startup["rampant-suicideEnemy"].value then
        processUnitClass(true,
                         false,
                         surface,
                         natives,
                         BASE_ALIGNMENT_SUICIDE,
                         "suicide")
    end

    if settings.startup["rampant-nuclearEnemy"].value then
        processUnitClass(true,
                         false,
                         surface,
                         natives,
                         BASE_ALIGNMENT_NUCLEAR,
                         "nuclear")
    end

    if settings.startup["rampant-trollEnemy"].value then
        processUnitClass(true,
                         true,
                         surface,
                         natives,
                         BASE_ALIGNMENT_TROLL,
                         "troll")
    end

    if settings.startup["rampant-infernoEnemy"].value then
        processUnitClass(false,
                         true,
                         surface,
                         natives,
                         BASE_ALIGNMENT_INFERNO,
                         "inferno")
    end

    if settings.startup["rampant-fastEnemy"].value then
        processUnitClass(true,
                         true,
                         surface,
                         natives,
                         BASE_ALIGNMENT_FAST,
                         "fast")
    end

    if settings.startup["rampant-laserEnemy"].value then
        processUnitClass(true,
                         true,
                         surface,
                         natives,
                         BASE_ALIGNMENT_LASER,
                         "laser")
    end

    if settings.startup["rampant-waspEnemy"].value then
        processUnitClass(false,
                         true,
                         surface,
                         natives,
                         BASE_ALIGNMENT_WASP,
                         "wasp")
    end

    if settings.startup["rampant-spawnerEnemy"].value then
        processUnitClass(false,
                         true,
                         surface,
                         natives,
                         BASE_ALIGNMENT_SPAWNER,
                         "spawner")
    end

    if settings.startup["rampant-energyThiefEnemy"].value then
        processUnitClass(true,
                         false,
                         surface,
                         natives,
                         BASE_ALIGNMENT_ENERGY_THIEF,
                         "energy-thief")
    end

    if settings.startup["rampant-poisonEnemy"].value then
        processUnitClass(true,
                         false,
                         surface,
                         natives,
                         BASE_ALIGNMENT_POISON,
                         "poison")
    end

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

    natives.evolutionTableUnitSpawner = normalizeProbabilities(natives.evolutionTableUnitSpawner)
    natives.evolutionTableWorm = normalizeProbabilities(natives.evolutionTableWorm)

end

baseUtilsG = baseUtils
return baseUtils
