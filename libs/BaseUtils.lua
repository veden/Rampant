-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.


if baseUtilsG then
    return baseUtilsG
end
local baseUtils = {}

-- imports

local mathUtils = require("MathUtils")
local constants = require("Constants")
local chunkPropertyUtils = require("ChunkPropertyUtils")
local mapUtils = require("MapUtils")
local queryUtils = require("QueryUtils")

-- constants

local MINIMUM_BUILDING_COST = constants.MINIMUM_BUILDING_COST
local FACTION_MUTATION_MAPPING = constants.FACTION_MUTATION_MAPPING

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

local FACTIONS_BY_DAMAGE_TYPE = constants.FACTIONS_BY_DAMAGE_TYPE

local BASE_GENERATION_STATE_ACTIVE = constants.BASE_GENERATION_STATE_ACTIVE

local FACTION_SET = constants.FACTION_SET

local HIVE_BUILDINGS_COST = constants.HIVE_BUILDINGS_COST

local BASE_DISTANCE_THRESHOLD = constants.BASE_DISTANCE_THRESHOLD
local BASE_DISTANCE_LEVEL_BONUS = constants.BASE_DISTANCE_LEVEL_BONUS
local BASE_DISTANCE_TO_EVO_INDEX = constants.BASE_DISTANCE_TO_EVO_INDEX

local CHUNK_SIZE = constants.CHUNK_SIZE

local BASE_AI_STATE_PEACEFUL = constants.BASE_AI_STATE_PEACEFUL

-- imported functions

local setPositionXYInQuery = queryUtils.setPositionXYInQuery

local euclideanDistancePoints = mathUtils.euclideanDistancePoints

local getChunkByPosition = mapUtils.getChunkByPosition

local gaussianRandomRangeRG = mathUtils.gaussianRandomRangeRG

local linearInterpolation = mathUtils.linearInterpolation

local mFloor = math.floor

local mMin = math.min
local mMax = math.max
local distort = mathUtils.distort

local getResourceGenerator = chunkPropertyUtils.getResourceGenerator

local next = next

-- module code

local function evoToTier(universe, evolutionFactor, maxSkips)
    local v
    local skipsRemaining = maxSkips
    for i=10,1,-1 do
        if universe.evoToTierMapping[i] <= evolutionFactor then
            v = i
            if (skipsRemaining == 0) or (universe.random() <= 0.75) then
                break
            end
            skipsRemaining = skipsRemaining - 1
        end
    end
    return v
end

local function findBaseMutation(universe, targetEvolution)
    local tier = evoToTier(universe, targetEvolution or universe.evolutionLevel, 2)
    local alignments = universe.evolutionTableAlignment[tier]

    local roll = universe.random()
    for i=1,#alignments do
        local alignment = alignments[i]

        roll = roll - alignment[1]

        if (roll <= 0) then
            return alignment[2]
        end
    end
    return alignments[#alignments]
end

local function initialEntityUpgrade(baseAlignment, tier, maxTier, map, useHiveType, entityType)
    local evolutionTable = map.universe.buildingEvolveLookup
    local entity

    local useTier

    local tierRoll = map.random()
    if (tierRoll < 0.4) then
        useTier = maxTier
    elseif (tierRoll < 0.7) then
        useTier = mMax(maxTier - 1, tier)
    elseif (tierRoll < 0.9) then
        useTier = mMax(maxTier - 2, tier)
    else
        useTier = mMax(maxTier - 3, tier)
    end

    local alignmentTable = evolutionTable[baseAlignment]
    local upgrades = alignmentTable[useTier]

    if upgrades then
        if useHiveType then
            for ui=1,#upgrades do
                local upgrade = upgrades[ui]
                if upgrade[3] == useHiveType then
                    entity = upgrade[2][map.random(#upgrade[2])]
                    break
                end
            end
        end
        if not entity then
            for ui=1,#upgrades do
                local upgrade = upgrades[ui]
                if upgrade[3] == entityType then
                    entity = upgrade[2][map.random(#upgrade[2])]
                end
            end
            if not entity then
                local mapTypes = FACTION_MUTATION_MAPPING[entityType]
                for i=1, #mapTypes do
                    local mappedType = mapTypes[i]
                    for ui=1,#upgrades do
                        local upgrade = upgrades[ui]
                        if upgrade[3] == mappedType then
                            return upgrade[2][map.random(#upgrade[2])]
                        end
                    end
                end
            end
        end
    end

    return entity
end

local function entityUpgrade(baseAlignment, tier, maxTier, originalEntity, map)
    local universe = map.universe
    local buildingHiveTypeLookup = universe.buildingHiveTypeLookup
    local evolutionTable = universe.upgradeLookup
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
                    entity = upgrade[map.random(#upgrade)]
                    if map.random() < 0.55 then
                        return entity
                    end
                end
            end
        elseif (#upgrades > 0) then
            entity = upgrades[map.random(#upgrades)]
            if map.random() < 0.55 then
                return entity
            end
        end
    end
    return entity
end

local function findEntityUpgrade(baseAlignment, currentEvo, evoIndex, originalEntity, map, evolve)
    local universe = map.universe
    local adjCurrentEvo = mMax(
        ((baseAlignment ~= universe.enemyAlignmentLookup[originalEntity.name]) and 0) or currentEvo,
        0
    )

    local tier = evoToTier(universe, adjCurrentEvo, 5)
    local maxTier = evoToTier(universe, evoIndex, 4)

    if (tier > maxTier) then
        maxTier = tier
    end

    if evolve then
        local chunk = getChunkByPosition(map, originalEntity.position)
        local entityName = originalEntity.name
        local entityType = map.universe.buildingHiveTypeLookup[entityName]
        if not entityType then
            if map.random() < 0.5 then
                entityType = "biter-spawner"
            else
                entityType = "spitter-spawner"
            end
        end
        local roll = map.random()
        local makeHive = (chunk ~= -1) and
            (
                (entityType == "biter-spawner") or (entityType == "spitter-spawner")
            )
            and
            (
                (
                    (roll <= 0.002) and
                    not map.universe.proxyEntityLookup[entityName]
                )
                or
                (
                    (roll <= 0.202) and
                    (getResourceGenerator(map, chunk) > 0)
                )
            )
        return initialEntityUpgrade(baseAlignment, tier, maxTier, map, (makeHive and "hive"), entityType)
    else
        return entityUpgrade(baseAlignment, tier, maxTier, originalEntity, map)
    end
end

local function findBaseInitialAlignment(universe, evoIndex)
    local dev = evoIndex * 0.15
    local evoTop = gaussianRandomRangeRG(evoIndex - (evoIndex * 0.075), dev, 0, evoIndex, universe.random)

    local result
    if universe.random() < 0.05 then
        result = {findBaseMutation(universe, evoTop), findBaseMutation(universe, evoTop)}
    else
        result = {findBaseMutation(universe, evoTop)}
    end

    return result
end

function baseUtils.recycleBases(universe)
    local bases = universe.bases
    local id = universe.recycleBaseIterator
    local base
    if not id then
        id, base = next(bases, nil)
    else
        base = bases[id]
    end
    if not id then
        universe.recycleBaseIterator = nil
    else
        universe.recycleBaseIterator = next(bases, id)
        local map = base.map
        if (base.chunkCount == 0) or not map.surface.valid then
            bases[id] = nil
            if universe.processBaseAIIterator == id then
                universe.processBaseAIIterator = nil
            end
            if map.surface.valid then
                map.universe.bases[id] = nil
            end
        end
    end
end

function baseUtils.upgradeEntity(entity, base, map, disPos, evolve, register, timeDelay)
    local position = entity.position
    local currentEvo = entity.prototype.build_base_evolution_requirement or 0

    local distance = mMin(1, euclideanDistancePoints(position.x, position.y, 0, 0) * BASE_DISTANCE_TO_EVO_INDEX)
    local evoIndex = mMax(distance, map.universe.evolutionLevel)
    local baseAlignment = base.alignment

    local pickedBaseAlignment
    if baseAlignment[2] then
        if map.random() < 0.75 then
            pickedBaseAlignment = baseAlignment[2]
        else
            pickedBaseAlignment = baseAlignment[1]
        end
    else
        pickedBaseAlignment = baseAlignment[1]
    end

    local spawnerName = findEntityUpgrade(pickedBaseAlignment,
                                          currentEvo,
                                          evoIndex,
                                          entity,
                                          map,
                                          evolve)

    if spawnerName and (spawnerName ~= entity.name) then
        local entityData = {
            ["name"] = spawnerName,
            ["position"] = disPos,
            ["register"] = register,
            ["map"] = map,
            ["base"] = base,
            ["entity"] = entity,
            ["delayTLL"] = timeDelay
        }
        map.universe.pendingUpgrades[entity.unit_number] = entityData
        return spawnerName
    end
    if entity.valid then
        if map.universe.proxyEntityLookup[entity.name] then
            entity.destroy()
        end
    end
    return nil
end

local function pickMutationFromDamageType(universe, damageType, roll, base)
    local baseAlignment = base.alignment

    local damageFactions = FACTIONS_BY_DAMAGE_TYPE[damageType]
    local mutation

    if (damageFactions and (#damageFactions > 0)) then
        mutation = damageFactions[universe.random(#damageFactions)]
        if baseAlignment[2] then
            if (roll < 0.05) then
                baseAlignment[2] = nil
                baseAlignment[1] = mutation
            elseif (roll < 0.25) then
                baseAlignment[1] = mutation
            else
                baseAlignment[2] = mutation
            end
        else
            if (roll < 0.85) then
                base.alignment[1] = mutation
            else
                base.alignment[2] = mutation
            end
        end
    else
        mutation = findBaseMutation(universe)
        if baseAlignment[2] then
            if (roll < 0.05) then
                baseAlignment[2] = nil
                baseAlignment[1] = mutation
            elseif (roll < 0.25) then
                baseAlignment[1] = mutation
            else
                baseAlignment[2] = mutation
            end
        else
            if (roll < 0.85) then
                base.alignment[1] = mutation
            else
                base.alignment[2] = mutation
            end
        end
    end
    if (universe.printBaseAdaptation) then
        if baseAlignment[2] then
            game.print({"description.rampant--adaptation2DebugMessage",
                        damageType,
                        {"description.rampant--"..baseAlignment[1].."EnemyName"},
                        {"description.rampant--"..baseAlignment[2].."EnemyName"},
                        base.x,
                        base.y,
                        base.mutations,
                        universe.MAX_BASE_MUTATIONS})
        else
            game.print({"description.rampant--adaptation1DebugMessage",
                        damageType,
                        {"description.rampant--"..baseAlignment[1].."EnemyName"},
                        base.x,
                        base.y,
                        base.mutations,
                        universe.MAX_BASE_MUTATIONS})
        end
    end
end

function baseUtils.upgradeBaseBasedOnDamage(universe, base)

    local total = 0

    for _,amount in pairs(base.damagedBy) do
        total = total + amount
    end
    local mutationAmount = total * 0.176471
    base.damagedBy["RandomMutation"] = mutationAmount
    total = total + mutationAmount
    local pickedDamage
    local roll = universe.random()
    for damageTypeName,amount in pairs(base.damagedBy) do
        base.damagedBy[damageTypeName] = amount / total
    end
    for damageType,amount in pairs(base.damagedBy) do
        roll = roll - amount
        if (roll <= 0) then
            pickedDamage = damageType
            break
        end
    end

    pickMutationFromDamageType(universe, pickedDamage, roll, base)
end

function baseUtils.processBaseMutation(chunk, map, base)
    if not base.alignment[1] or
        (base.stateGeneration ~= BASE_GENERATION_STATE_ACTIVE) or
        (map.random() >= 0.30)
    then
        return
    end

    if (base.points >= MINIMUM_BUILDING_COST) then
        local surface = map.surface
        local universe = map.universe
        setPositionXYInQuery(universe.pbFilteredEntitiesPointQueryLimited,
                             chunk.x + (CHUNK_SIZE * map.random()),
                             chunk.y + (CHUNK_SIZE * map.random()))

        local entities = surface.find_entities_filtered(universe.pbFilteredEntitiesPointQueryLimited)
        if #entities ~= 0 then
            local entity = entities[1]
            local cost = (universe.costLookup[entity.name] or MAGIC_MAXIMUM_NUMBER)
            if (base.points >= cost) then
                local newEntity = baseUtils.upgradeEntity(entity, base, map)
                if newEntity then
                    if universe.printBaseUpgrades then
                        surface.print("[gps=".. entity.position.x ..",".. entity.position.y .."] " .. "Scheduled upgrade for ".. entity.name .. " to " .. newEntity)
                    end
                    base.points = base.points - cost
                end
            end
        end
    end
end

function baseUtils.createBase(map, chunk, tick)
    local x = chunk.x
    local y = chunk.y
    local distance = euclideanDistancePoints(x, y, 0, 0)

    local meanLevel = mFloor(distance * 0.005)

    local universe = map.universe
    local distanceIndex = mMin(1, distance * BASE_DISTANCE_TO_EVO_INDEX)
    local evoIndex = mMax(distanceIndex, universe.evolutionLevel)

    local alignment = (universe.NEW_ENEMIES and findBaseInitialAlignment(universe, evoIndex)) or {"neutral"}

    local baseLevel = gaussianRandomRangeRG(meanLevel,
                                            meanLevel * 0.3,
                                            meanLevel * 0.50,
                                            meanLevel * 1.50,
                                            universe.random)
    local baseDistanceThreshold = gaussianRandomRangeRG(BASE_DISTANCE_THRESHOLD,
                                                        BASE_DISTANCE_THRESHOLD * 0.2,
                                                        BASE_DISTANCE_THRESHOLD * 0.75,
                                                        BASE_DISTANCE_THRESHOLD * 1.50,
                                                        universe.random)
    local distanceThreshold = (baseLevel * BASE_DISTANCE_LEVEL_BONUS) + baseDistanceThreshold

    local base = {
        x = x,
        y = y,
        distanceThreshold = distanceThreshold * universe.baseDistanceModifier,
        tick = tick,
        alignment = alignment,
        damagedBy = {},
        deathEvents = 0,
        mutations = 0,
        stateGeneration = BASE_GENERATION_STATE_ACTIVE,
        stateGenerationTick = 0,
        chunkCount = 0,
        createdTick = tick,
        points = 0,
        unitPoints = 0,
        stateAI = BASE_AI_STATE_PEACEFUL,
        stateAITick = 0,
        maxAggressiveGroups = 0,
        sentAggressiveGroups = 0,
        maxSiegeGroups = 0,
        sentSiegeGroups = 0,
        activeRaidNests = 0,
        activeNests = 0,
        destroyPlayerBuildings = 0,
        lostEnemyUnits = 0,
        lostEnemyBuilding = 0,
        rocketLaunched = 0,
        builtEnemyBuilding = 0,
        ionCannonBlasts = 0,
        artilleryBlasts = 0,
        temperament = 0.5,
        temperamentScore = 0,
        universe = universe,
        map = map,
        id = universe.baseId
    }
    universe.baseId = universe.baseId + 1

    map.bases[base.id] = base
    universe.bases[base.id] = base

    return base
end

local function isMember(lst, x)
    for _,l in pairs(lst) do
        if l == x then
            return true
        end
    end
    return false
end

function baseUtils.rebuildNativeTables(universe, rg)
    local alignmentSet = {}
    universe.evolutionTableAlignment = alignmentSet
    local buildingSpaceLookup = {}
    universe.buildingSpaceLookup = buildingSpaceLookup
    local enemyAlignmentLookup = {}
    universe.enemyAlignmentLookup = enemyAlignmentLookup
    local upgradeLookup = {}
    universe.upgradeLookup = upgradeLookup
    local buildingEvolveLookup = {}
    universe.buildingEvolveLookup = buildingEvolveLookup
    local costLookup = {}
    universe.costLookup = costLookup
    local buildingHiveTypeLookup = {}
    universe.buildingHiveTypeLookup = buildingHiveTypeLookup
    local proxyEntityLookup = {}
    universe.proxyEntityLookup = proxyEntityLookup
    local vanillaEntityLookup = {}
    universe.vanillaEntityTypeLookup = vanillaEntityLookup
    local entitySkipCountLookup = {}
    universe.entitySkipCountLookup = entitySkipCountLookup

    buildingHiveTypeLookup["biter-spawner"] = "biter-spawner"
    buildingHiveTypeLookup["spitter-spawner"] = "spitter-spawner"
    buildingHiveTypeLookup["small-worm-turret"] = "turret"
    buildingHiveTypeLookup["medium-worm-turret"] = "turret"
    buildingHiveTypeLookup["big-worm-turret"] = "turret"
    buildingHiveTypeLookup["behemoth-worm-turret"] = "turret"

    vanillaEntityLookup["biter-spawner"] = true
    vanillaEntityLookup["spitter-spawner"] = true
    vanillaEntityLookup["small-worm-turret"] = true
    vanillaEntityLookup["medium-worm-turret"] = true
    vanillaEntityLookup["big-worm-turret"] = true
    vanillaEntityLookup["behemoth-worm-turret"] = true

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
                for v=1,universe.ENEMY_VARIATIONS do
                    local entry = faction.type .. "-" .. building.name .. "-v" .. v .. "-t" .. t .. "-rampant"
                    enemyAlignmentLookup[entry] = faction.type
                    local proxyEntity = "entity-proxy-" .. building.type .. "-t" .. t .. "-rampant"
                    proxyEntityLookup[proxyEntity] = true
                    buildingSpaceLookup[entry] = proxyEntity
                    costLookup[entry] = HIVE_BUILDINGS_COST[building.type]
                    buildingHiveTypeLookup[entry] = building.type
                    if not buildingHiveTypeLookup[proxyEntity] then
                        buildingHiveTypeLookup[proxyEntity] = building.type
                    end
                    variationSet[#variationSet+1] = entry
                    for _,unit in pairs(faction.units) do
                        if isMember(unit.attributes, "skipKillCount") then
                            local name = faction.type .. "-" .. unit.name .. "-v" .. v .. "-t" .. t .. "-rampant"
                            universe.entitySkipCountLookup[name] = true
                        end
                    end
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

    local evoIndex = evoToTier(universe, universe.evolutionLevel, 2)

    if universe.bases then
        for _,base in pairs(universe.bases) do
            for x=1,2 do
                local alignment = base.alignment[x]
                if alignment and not universe.buildingEvolveLookup[alignment] then
                    base.alignment = findBaseInitialAlignment(universe, evoIndex)
                    break
                elseif not alignment and (x == 1) then
                    base.alignment = findBaseInitialAlignment(universe, evoIndex)
                    break
                end
            end
        end
    end
end

baseUtilsG = baseUtils
return baseUtils
