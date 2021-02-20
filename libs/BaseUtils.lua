if baseUtilsG then
    return baseUtilsG
end
local baseUtils = {}

-- imports

local mathUtils = require("MathUtils")
local constants = require("Constants")
local chunkPropertyUtils = require("ChunkPropertyUtils")
local mapUtils = require("MapUtils")

-- constants

local FACTION_MUTATION_MAPPING = constants.FACTION_MUTATION_MAPPING

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

local BASE_AI_STATE_DORMANT = constants.BASE_AI_STATE_DORMANT
local BASE_AI_STATE_ACTIVE = constants.BASE_AI_STATE_ACTIVE
local BASE_AI_STATE_WORMS = constants.BASE_AI_STATE_WORMS
local BASE_AI_STATE_NESTS = constants.BASE_AI_STATE_NESTS
local BASE_AI_STATE_OVERDRIVE = constants.BASE_AI_STATE_OVERDRIVE
local BASE_AI_STATE_MUTATE = constants.BASE_AI_STATE_MUTATE

local FACTION_SET = constants.FACTION_SET

local BASE_DEADZONE_TTL = constants.BASE_DEADZONE_TTL

local BASE_AI_MIN_STATE_DURATION = constants.BASE_AI_MIN_STATE_DURATION
local BASE_AI_MIN_TEMPERAMENT_DURATION = constants.BASE_AI_MIN_TEMPERAMENT_DURATION
local BASE_AI_MAX_STATE_DURATION = constants.BASE_AI_MAX_STATE_DURATION
local BASE_AI_MAX_TEMPERAMENT_DURATION = constants.BASE_AI_MAX_TEMPERAMENT_DURATION

local BASE_UPGRADE = constants.BASE_UPGRADE

local HIVE_BUILDINGS_COST = constants.HIVE_BUILDINGS_COST

local BASE_DISTANCE_THRESHOLD = constants.BASE_DISTANCE_THRESHOLD
local BASE_DISTANCE_LEVEL_BONUS = constants.BASE_DISTANCE_LEVEL_BONUS
local BASE_DISTANCE_TO_EVO_INDEX = constants.BASE_DISTANCE_TO_EVO_INDEX

local BASE_COLLECTION_THRESHOLD = constants.BASE_COLLECTION_THRESHOLD

local CHUNK_SIZE = constants.CHUNK_SIZE

-- imported functions

local randomTickEvent = mathUtils.randomTickEvent
local euclideanDistancePoints = mathUtils.euclideanDistancePoints

local getChunkByPosition = mapUtils.getChunkByPosition

local gaussianRandomRange = mathUtils.gaussianRandomRange

local linearInterpolation = mathUtils.linearInterpolation

local mFloor = math.floor

local mMin = math.min
local mMax = math.max
local distort = mathUtils.distort

local getChunkBase = chunkPropertyUtils.getChunkBase
local setChunkBase = chunkPropertyUtils.setChunkBase

local getResourceGenerator = chunkPropertyUtils.getResourceGenerator

local next = next

local mRandom = math.random

-- module code

local function evoToTier(native, evolutionFactor)
    local v
    for i=10,1,-1 do
        if native.evoToTierMapping[i] <= evolutionFactor then
            v = i
            if mRandom() <= 0.65 then
                break
            end
        end
    end
    return v
end

function baseUtils.findNearbyBase(map, chunk)
    local x = chunk.x
    local y = chunk.y

    local foundBase = getChunkBase(map, chunk)
    if foundBase then
        return foundBase
    end

    local bases = map.native.bases
    local closest = MAGIC_MAXIMUM_NUMBER
    for _, base in pairs(bases) do
        local distance = euclideanDistancePoints(base.x, base.y, x, y)
        if (distance <= base.distanceThreshold) and (distance < closest) then
            closest = distance
            foundBase = base
        end
    end

    return foundBase
end

local function findBaseMutation(native, targetEvolution)
    local tier = evoToTier(native, targetEvolution or native.evolutionLevel)
    local alignments = native.evolutionTableAlignment[tier]

    local roll = mRandom()
    for i=1,#alignments do
        local alignment = alignments[i]

        roll = roll - alignment[1]

        if (roll <= 0) then
            return alignment[2]
        end
    end
    return alignments[#alignments]
end

local function initialEntityUpgrade(baseAlignment, tier, maxTier, native, useHiveType)
    local evolutionTable = native.buildingEvolveLookup
    local entity

    local useTier

    local tierRoll = mRandom()
    if (tierRoll < 0.4) then
        useTier = maxTier
    elseif (tierRoll < 0.7) then
        useTier = mMax(maxTier - 1, tier)
    elseif (tierRoll < 0.9) then
        useTier = mMax(maxTier - 2, tier)
    else
        useTier = mMax(maxTier - 3, tier)
    end

    local upgrades = evolutionTable[baseAlignment][useTier]

    if upgrades then
        if useHiveType then
            for ui=1,#upgrades do
                local upgrade = upgrades[ui]
                if upgrade[3] == useHiveType then
                    entity = upgrade[2][mRandom(#upgrade[2])]
                    break
                end
            end
        end
        if not entity then
            local roll = mRandom()

            for ui=1,#upgrades do
                local upgrade = upgrades[ui]

                roll = roll - upgrade[1]

                if (roll <= 0) then
                    entity = upgrade[2][mRandom(#upgrade[2])]
                    break
                end
            end
        end
    end

    return entity
end

local function entityUpgrade(baseAlignment, tier, maxTier, originalEntity, native)
    local buildingHiveTypeLookup = native.buildingHiveTypeLookup
    local evolutionTable = native.upgradeLookup
    local entity

    local hiveType = buildingHiveTypeLookup[originalEntity.name]

    for t=maxTier,tier,-1 do
        local factionLookup = evolutionTable[baseAlignment][t]
        local upgrades = factionLookup[hiveType]
        if not upgrades then
            local mapTypes = FACTION_MUTATION_MAPPING[hiveType]
            for i=1, #mapTypes do
                local upgrade = factionLookup[mapTypes[i]]
                if upgrade and (#upgrade > 0) then
                    entity = upgrade[mRandom(#upgrade)]
                    if mRandom() < 0.55 then
                        return entity
                    end
                end
            end
        elseif (#upgrades > 0) then
            entity = upgrades[mRandom(#upgrades)]
            if mRandom() < 0.55 then
                return entity
            end
        end
    end
    return entity
end

local function findEntityUpgrade(baseAlignment, currentEvo, evoIndex, originalEntity, native, evolve)

    local adjCurrentEvo = mMax(
        ((baseAlignment ~= native.enemyAlignmentLookup[originalEntity.name]) and 0) or currentEvo,
        0
    )

    local tier = evoToTier(native, adjCurrentEvo)
    local maxTier = evoToTier(native, evoIndex)

    if (tier > maxTier) then
        return nil
    end

    if evolve then
        local chunk = getChunkByPosition(native.map, originalEntity.position)
        local makeHive = (chunk ~= -1) and (getResourceGenerator(native.map, chunk) > 0) and (mRandom() < 0.2)
        return initialEntityUpgrade(baseAlignment, tier, maxTier, native, (makeHive and "hive"))
    else
        return entityUpgrade(baseAlignment, tier, maxTier, originalEntity, native)
    end
end

local function findBaseInitialAlignment(native, evoIndex)
    local dev = evoIndex * 0.3
    local evoTop = gaussianRandomRange(evoIndex - dev, dev, 0, evoIndex)

    local result
    if mRandom() < 0.05 then
        result = {findBaseMutation(native, evoTop), findBaseMutation(native, evoTop)}
    else
        result = {findBaseMutation(native, evoTop)}
    end

    return result
end

function baseUtils.recycleBases(native, tick)
    local bases = native.bases
    local id, base = next(bases, native.map.recycleBaseIterator)
    -- for _=1,2 do
    if not id then
        native.map.recycleBaseIterator = nil
        return
    else
        if ((tick - base.tick) > BASE_COLLECTION_THRESHOLD) then
            local nextId
            nextId = next(bases, id)
            bases[id] = nil
            id = nextId
        else
            id = next(bases, id)
        end
    end
    -- end
    native.map.recycleBaseIterator = id
end


function baseUtils.upgradeEntity(entity, baseAlignment, native, disPos, evolve)
    local surface = native.surface
    local position = entity.position
    local currentEvo = entity.prototype.build_base_evolution_requirement or 0

    if not baseAlignment[1] then
        entity.destroy()
        return nil
    end

    local distance = mMin(1, euclideanDistancePoints(position.x, position.y, 0, 0) * BASE_DISTANCE_TO_EVO_INDEX)
    local evoIndex = mMax(distance, native.evolutionLevel)

    local spawnerName = findEntityUpgrade(baseAlignment[mRandom(#baseAlignment)],
                                          currentEvo,
                                          evoIndex,
                                          entity,
                                          native,
                                          evolve)

    if spawnerName then
        entity.destroy()
        local name = native.buildingSpaceLookup[spawnerName] or spawnerName
        local query = native.map.upgradeEntityQuery
        query.name = name
        query.position = disPos or position

        if not surface.can_place_entity(query) then
            local newPosition = surface.find_non_colliding_position(
                name,
                disPos or position,
                CHUNK_SIZE,
                1,
                true
            )
            query.position = newPosition or disPos or position
        end

        query.name = spawnerName
        if remote.interfaces["kr-creep"] then
            remote.call("kr-creep", "spawn_creep_at_position", surface, query.position)
        end
        return surface.create_entity(query)
    end
    return entity
end

local function upgradeBase(native, base)
    local baseAlignment = base.alignment

    local roll = mRandom()
    if baseAlignment[2] then
        if (roll < 0.05) then
            baseAlignment[2] = nil
            baseAlignment[1] = findBaseMutation(native)
        elseif (roll < 0.25) then
            baseAlignment[1] = findBaseMutation(native)
        else
            baseAlignment[2] = findBaseMutation(native)
        end
        return true
    else
        if (roll < 0.85) then
            base.alignment[1] = findBaseMutation(native)
        else
            base.alignment[2] = findBaseMutation(native)
        end
        return true
    end
end

function baseUtils.processBase(chunk, native, tick, base)
    if not base.alignment[1] then
        base.state = BASE_AI_STATE_DORMANT
        return
    end

    local surface = native.surface
    local map = native.map
    local point = map.position

    point.x = chunk.x + (CHUNK_SIZE * mRandom())
    point.y = chunk.y + (CHUNK_SIZE * mRandom())

    if (base.state == BASE_AI_STATE_ACTIVE) then
        local entity = surface.find_entities_filtered(map.filteredEntitiesPointQueryLimited)
        local cost = (native.costLookup[entity.name] or MAGIC_MAXIMUM_NUMBER)
        if entity and (base.points >= cost) then
            local newEntity = baseUtils.upgradeEntity(entity,
                                                      surface,
                                                      base.alignment,
                                                      native)
            if newEntity then
                base.points = base.points - cost
            end
        end
    elseif (base.state == BASE_AI_STATE_MUTATE) then
        if (base.points >= BASE_UPGRADE) then
            if upgradeBase(native, base) then
                base.points = base.points - BASE_UPGRADE
            end
        end
    end

    if (base.state == BASE_AI_STATE_OVERDRIVE) then
        base.points = base.points + (native.baseIncrement * 5)
    elseif (base.state ~= BASE_AI_STATE_DORMANT) then
        base.points = base.points + native.baseIncrement
    end

    if (base.temperamentTick <= tick) then
        base.temperament = mRandom()
        base.temperamentTick = randomTickEvent(tick,
                                               BASE_AI_MIN_TEMPERAMENT_DURATION,
                                               BASE_AI_MAX_TEMPERAMENT_DURATION)
    end

    if (base.stateTick <= tick) then
        local roll = mRandom() * mMax(1 - native.evolutionLevel, 0.15)
        if (roll > native.temperament) then
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
        base.stateTick = randomTickEvent(tick,
                                         BASE_AI_MIN_STATE_DURATION,
                                         BASE_AI_MAX_STATE_DURATION)
    end

    base.tick = tick
end

function baseUtils.createBase(native, chunk, tick, rebuilding)
    local x = chunk.x
    local y = chunk.y
    local distance = euclideanDistancePoints(x, y, 0, 0)

    local meanLevel = mFloor(distance * 0.005)

    local distanceIndex = mMin(1, distance * BASE_DISTANCE_TO_EVO_INDEX)
    local evoIndex = mMax(distanceIndex, native.evolutionLevel)

    local baseTick = tick

    local alignment
    if (not rebuilding) and (mRandom() < native.deadZoneFrequency) then
        alignment = {}
        baseTick = BASE_DEADZONE_TTL
    else
        alignment = findBaseInitialAlignment(native, evoIndex) or {"neutral"}
    end

    local baseLevel = gaussianRandomRange(meanLevel, meanLevel * 0.3, meanLevel * 0.50, meanLevel * 1.50)
    local baseDistanceThreshold = gaussianRandomRange(BASE_DISTANCE_THRESHOLD,
                                                      BASE_DISTANCE_THRESHOLD * 0.2,
                                                      BASE_DISTANCE_THRESHOLD * 0.75,
                                                      BASE_DISTANCE_THRESHOLD * 1.50)
    local distanceThreshold = (baseLevel * BASE_DISTANCE_LEVEL_BONUS) + baseDistanceThreshold

    local base = {
        x = x,
        y = y,
        distanceThreshold = distanceThreshold,
        tick = baseTick,
        alignment = alignment,
        state = BASE_AI_STATE_DORMANT,
        stateTick = 0,
        temperamentTick = 0,
        createdTick = tick,
        temperament = 0,
        points = 0,
        id = native.baseId
    }
    native.baseId = native.baseId + 1

    setChunkBase(native.map, chunk, base)

    native.bases[base.id] = base

    return base
end

function baseUtils.rebuildNativeTables(native, rg)
    local alignmentSet = {}
    native.evolutionTableAlignment = alignmentSet
    local buildingSpaceLookup = {}
    native.buildingSpaceLookup = buildingSpaceLookup
    local enemyAlignmentLookup = {}
    native.enemyAlignmentLookup = enemyAlignmentLookup
    local evoToTierMapping = {}
    native.evoToTierMapping = evoToTierMapping
    local upgradeLookup = {}
    native.upgradeLookup = upgradeLookup
    local buildingEvolveLookup = {}
    native.buildingEvolveLookup = buildingEvolveLookup
    local costLookup = {}
    native.costLookup = costLookup
    local buildingHiveTypeLookup = {}
    native.buildingHiveTypeLookup = buildingHiveTypeLookup

    for i=1,10 do
        evoToTierMapping[#evoToTierMapping+1] = (((i - 1) * 0.1) ^ 0.5) - 0.05
    end

    for i=1,#FACTION_SET do
        local faction = FACTION_SET[i]

        local factionUpgradeLookup = {}
        upgradeLookup[faction.type] = factionUpgradeLookup
        local factionBuildingPicker = {}
        buildingEvolveLookup[faction.type] = factionBuildingPicker

        for t=1,10 do
            local alignments = alignmentSet[t]
            if not alignments then
                alignments = {}
                alignmentSet[t] = alignments
            end

            --[[
                alignments table is a table that is used for selecting what factions are available
                to pick given an evolution level.

                evolutionTable is a table that given a faction allows the selection of a building
                type based on the propabilities given. Once the the building type is selected given
                a faction, then the evolution decides what level of building to select
            --]]
            local factionAcceptRate = faction.acceptRate

            local low = factionAcceptRate[1]
            local high = factionAcceptRate[2]
            if (low <= t) and (t <= high) then
                alignments[#alignments+1] = {
                    distort(rg,
                            linearInterpolation((t - low) / (high - low), factionAcceptRate[3], factionAcceptRate[4])),
                    faction.type
                }
            end

            local tieredUpgradeBuildingSet = factionUpgradeLookup[t]
            if not tieredUpgradeBuildingSet then
                tieredUpgradeBuildingSet = {}
                factionUpgradeLookup[t] = tieredUpgradeBuildingSet
            end

            local tieredBuildingPickerSet = factionBuildingPicker[t]
            if not tieredBuildingPickerSet then
                tieredBuildingPickerSet = {}
                factionBuildingPicker[t] = tieredBuildingPickerSet
            end

            for b=1,#faction.buildings do
                local building = faction.buildings[b]

                local buildingSet = tieredUpgradeBuildingSet[building.type]
                if not buildingSet then
                    buildingSet = {}
                    tieredUpgradeBuildingSet[building.type] = buildingSet
                end

                local variationSet = {}
                for v=1,native.ENEMY_VARIATIONS do
                    local entry = faction.type .. "-" .. building.name .. "-v" .. v .. "-t" .. t .. "-rampant"
                    enemyAlignmentLookup[entry] = faction.type
                    local proxyEntity = "entity-proxy-" .. building.type .. "-t" .. t .. "-rampant"
                    buildingSpaceLookup[entry] = proxyEntity
                    costLookup[entry] = HIVE_BUILDINGS_COST[building.type]
                    buildingHiveTypeLookup[entry] = building.type
                    if not buildingHiveTypeLookup[proxyEntity] then
                        buildingHiveTypeLookup[proxyEntity] = building.type
                    end
                    variationSet[#variationSet+1] = entry
                end

                local buildingAcceptRate = building.acceptRate

                local buildingLow = buildingAcceptRate[1]
                local buildingHigh = buildingAcceptRate[2]
                if (buildingLow <= t) and (t <= buildingHigh) then
                    for vi=1,#variationSet do
                        local variation = variationSet[vi]
                        buildingSet[#buildingSet+1] = variation
                    end
                    tieredBuildingPickerSet[#tieredBuildingPickerSet+1] = {
                        distort(rg,
                                linearInterpolation((t - buildingLow) / (buildingHigh - buildingLow),
                                    buildingAcceptRate[3],
                                    buildingAcceptRate[4])),
                        variationSet,
                        building.type
                    }
                end

            end
        end
    end

    for t=1,10 do
        local alignments = alignmentSet[t]
        local totalAlignment = 0
        for i=1,#alignments do
            totalAlignment = totalAlignment + alignments[i][1]
        end
        for i=1,#alignments do
            alignments[i][1] = alignments[i][1] / totalAlignment
        end

        for fi=1,#FACTION_SET do
            local faction = FACTION_SET[fi]
            local factionBuildingSet = buildingEvolveLookup[faction.type][t]
            local totalBuildingSet = 0
            for i=1,#factionBuildingSet do
                totalBuildingSet = totalBuildingSet + factionBuildingSet[i][1]
            end
            for i=1,#factionBuildingSet do
                factionBuildingSet[i][1] = factionBuildingSet[i][1] / totalBuildingSet
            end
        end
    end

    local evoIndex = evoToTier(native, native.evolutionLevel)

    for _,base in pairs(native.bases) do
        for x=1,#base.alignment do
            local alignment = base.alignment[x]
            if not native.buildingEvolveLookup[alignment] then
                base.alignment = findBaseInitialAlignment(native, evoIndex)
                break
            end
        end
    end
end

baseUtilsG = baseUtils
return baseUtils
