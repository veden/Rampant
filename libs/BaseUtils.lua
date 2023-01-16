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

local stringUtils = require("StringUtils")
local mathUtils = require("MathUtils")
local constants = require("Constants")
local chunkPropertyUtils = require("ChunkPropertyUtils")
local mapUtils = require("MapUtils")
local queryUtils = require("QueryUtils")

-- constants

local TIERS = constants.TIERS
local EVO_TO_TIER_MAPPING = constants.EVO_TO_TIER_MAPPING
local PROXY_ENTITY_LOOKUP = constants.PROXY_ENTITY_LOOKUP
local BUILDING_HIVE_TYPE_LOOKUP = constants.BUILDING_HIVE_TYPE_LOOKUP
local COST_LOOKUP = constants.COST_LOOKUP
local UPGRADE_LOOKUP = constants.UPGRADE_LOOKUP

local ENEMY_ALIGNMENT_LOOKUP = constants.ENEMY_ALIGNMENT_LOOKUP

local EVOLUTION_TABLE_ALIGNMENT = constants.EVOLUTION_TABLE_ALIGNMENT
local BUILDING_EVOLVE_LOOKUP = constants.BUILDING_EVOLVE_LOOKUP

local MINIMUM_BUILDING_COST = constants.MINIMUM_BUILDING_COST
local FACTION_MUTATION_MAPPING = constants.FACTION_MUTATION_MAPPING

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

local FACTIONS_BY_DAMAGE_TYPE = constants.FACTIONS_BY_DAMAGE_TYPE

local BASE_GENERATION_STATE_ACTIVE = constants.BASE_GENERATION_STATE_ACTIVE

local BASE_DISTANCE_THRESHOLD = constants.BASE_DISTANCE_THRESHOLD
local BASE_DISTANCE_LEVEL_BONUS = constants.BASE_DISTANCE_LEVEL_BONUS
local BASE_DISTANCE_TO_EVO_INDEX = constants.BASE_DISTANCE_TO_EVO_INDEX

local CHUNK_SIZE = constants.CHUNK_SIZE

local BASE_AI_STATE_PEACEFUL = constants.BASE_AI_STATE_PEACEFUL

-- imported functions

local isMember = stringUtils.isMember

local setPositionXYInQuery = queryUtils.setPositionXYInQuery

local euclideanDistancePoints = mathUtils.euclideanDistancePoints

local getChunkByPosition = mapUtils.getChunkByPosition

local gaussianRandomRangeRG = mathUtils.gaussianRandomRangeRG

local mFloor = math.floor

local tableRemove = table.remove
local mMin = math.min
local mMax = math.max

local getResourceGenerator = chunkPropertyUtils.getResourceGenerator

local next = next

-- module code

local function evoToTier(universe, evolutionFactor, maxSkips)
    local v
    local skipsRemaining = maxSkips
    for i=TIERS,1,-1 do
        if EVO_TO_TIER_MAPPING[i] <= evolutionFactor then
            v = i
            if (skipsRemaining == 0) or (universe.random() <= 0.75) then
                break
            end
            skipsRemaining = skipsRemaining - 1
        end
    end
    return v
end

local function findBaseMutation(universe, targetEvolution, excludeFactions)
    local tier = evoToTier(universe, targetEvolution or universe.evolutionLevel, 2)
    local availableAlignments
    local alignments = EVOLUTION_TABLE_ALIGNMENT[tier]

    if excludeFactions then
        availableAlignments = {}
        for _,alignment in pairs(alignments) do
            if not isMember(alignment[2], excludeFactions) then
                availableAlignments[#availableAlignments+1] = alignment
            end
        end
    else
        availableAlignments = alignments
    end

    local roll = universe.random()
    for i=1,#availableAlignments do
        local alignment = availableAlignments[i]

        roll = roll - alignment[1]

        if (roll <= 0) then
            return alignment[2]
        end
    end
    return availableAlignments[#availableAlignments]
end

local function initialEntityUpgrade(baseAlignment, tier, maxTier, map, useHiveType, entityType)
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

    local alignmentTable = BUILDING_EVOLVE_LOOKUP[baseAlignment]
    if not alignmentTable then
        alignmentTable = BUILDING_EVOLVE_LOOKUP["neutral"]
    end
    local upgrades = alignmentTable[useTier]

    if alignmentTable and upgrades then
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
    local entity

    local hiveType = BUILDING_HIVE_TYPE_LOOKUP[originalEntity.name]

    for t=maxTier,tier,-1 do
        local factionLookup = UPGRADE_LOOKUP[baseAlignment][t]
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

function baseUtils.findEntityUpgrade(baseAlignment, currentEvo, evoIndex, originalEntity, map, evolve)
    local universe = map.universe
    local adjCurrentEvo = mMax(
        ((baseAlignment ~= ENEMY_ALIGNMENT_LOOKUP[originalEntity.name]) and 0) or currentEvo,
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
        local entityType = BUILDING_HIVE_TYPE_LOOKUP[entityName]
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
                    (roll <= 0.01) and
                    not PROXY_ENTITY_LOOKUP[entityName]
                )
                or
                (
                    (roll <= 0.210) and
                    (getResourceGenerator(map, chunk) > 0)
                )
            )
        return initialEntityUpgrade(baseAlignment, tier, maxTier, map, (makeHive and "hive"), entityType)
    else
        return entityUpgrade(baseAlignment, tier, maxTier, originalEntity, map)
    end
end

-- local
function baseUtils.findBaseInitialAlignment(universe, evoIndex, excludeFactions)
    local dev = evoIndex * 0.15
    local evoTop = gaussianRandomRangeRG(evoIndex - (evoIndex * 0.075), dev, 0, evoIndex, universe.random)

    local result
    if universe.random() < 0.05 then
        result = {findBaseMutation(universe, evoTop, excludeFactions), findBaseMutation(universe, evoTop, excludeFactions)}
    else
        result = {findBaseMutation(universe, evoTop, excludeFactions)}
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

function baseUtils.queueUpgrade(entity, base, disPos, evolve, register, timeDelay)
    base.universe.pendingUpgrades[entity.unit_number] = {
        ["position"] = disPos,
        ["register"] = register,
        ["evolve"] = evolve,
        ["base"] = base,
        ["entity"] = entity,
        ["delayTLL"] = timeDelay
    }
end

local function pickMutationFromDamageType(universe, damageType, roll, base)
    local baseAlignment = base.alignment

    local damageFactions = FACTIONS_BY_DAMAGE_TYPE[damageType]
    local mutation
    local mutated = false

    if damageFactions and (#damageFactions > 0) then
        mutation = damageFactions[universe.random(#damageFactions)]
        if not isMember(mutation, base.alignmentHistory) then
            if baseAlignment[2] then
                if (baseAlignment[1] ~= mutation) and (baseAlignment[2] ~= mutation) then
                    mutated = true
                    if (roll < 0.05) then
                        base.alignmentHistory[#base.alignmentHistory+1] = baseAlignment[1]
                        base.alignmentHistory[#base.alignmentHistory+1] = baseAlignment[2]
                        baseAlignment[1] = mutation
                        baseAlignment[2] = nil
                    elseif (roll < 0.75) then
                        base.alignmentHistory[#base.alignmentHistory+1] = baseAlignment[1]
                        baseAlignment[1] = mutation
                    else
                        baseAlignment[2] = mutation
                    end
                end
            elseif (baseAlignment[1] ~= mutation) then
                mutated = true
                if (roll < 0.85) then
                    base.alignmentHistory[#base.alignmentHistory+1] = baseAlignment[1]
                    baseAlignment[1] = mutation
                else
                    baseAlignment[2] = mutation
                end
            end
        end
    else
        mutation = findBaseMutation(universe)
        if not isMember(mutation, base.alignmentHistory) then
            if baseAlignment[2] then
                if (baseAlignment[1] ~= mutation) and (baseAlignment[2] ~= mutation) then
                    mutated = true
                    if (roll < 0.05) then
                        base.alignmentHistory[#base.alignmentHistory+1] = baseAlignment[1]
                        base.alignmentHistory[#base.alignmentHistory+1] = baseAlignment[2]
                        baseAlignment[1] = mutation
                        baseAlignment[2] = nil
                    elseif (roll < 0.75) then
                        base.alignmentHistory[#base.alignmentHistory+1] = baseAlignment[1]
                        baseAlignment[1] = mutation
                    else
                        baseAlignment[2] = mutation
                    end
                end
            elseif (baseAlignment[1] ~= mutation) then
                mutated = true
                if (roll < 0.85) then
                    base.alignmentHistory[#base.alignmentHistory+1] = baseAlignment[1]
                    base.alignment[1] = mutation
                else
                    base.alignment[2] = mutation
                end
            end
        end
    end
    if mutated and universe.printBaseAdaptation then
        if baseAlignment[2] then
            game.print({"description.rampant--adaptation2DebugMessage",
                        base.id,
                        base.map.surface.name,
                        damageType,
                        {"description.rampant--"..baseAlignment[1].."EnemyName"},
                        {"description.rampant--"..baseAlignment[2].."EnemyName"},
                        base.x,
                        base.y,
                        base.mutations,
                        universe.MAX_BASE_MUTATIONS})
        else
            game.print({"description.rampant--adaptation1DebugMessage",
                        base.id,
                        base.map.surface.name,
                        damageType,
                        {"description.rampant--"..baseAlignment[1].."EnemyName"},
                        base.x,
                        base.y,
                        base.mutations,
                        universe.MAX_BASE_MUTATIONS})
        end
    end
    local alignmentCount = table_size(base.alignmentHistory)
    while (alignmentCount > universe.MAX_BASE_ALIGNMENT_HISTORY) do
        tableRemove(base.alignmentHistory, 1)
        alignmentCount = alignmentCount - 1
    end
    return mutated
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

    return pickMutationFromDamageType(universe, pickedDamage, roll, base)
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
            local cost = (COST_LOOKUP[entity.name] or MAGIC_MAXIMUM_NUMBER)
            if (base.points >= cost) then
                local position = entity.position
                baseUtils.modifyBaseSpecialPoints(base, -cost, "Scheduling Entity upgrade", position.x, position.y)
                baseUtils.queueUpgrade(entity, base, nil, false, true)
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

    local alignment =
        (universe.NEW_ENEMIES and baseUtils.findBaseInitialAlignment(universe, evoIndex))
        or {"neutral"}

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
        alignmentHistory = {},
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
        resetExpensionGroupsTick = 0,
        maxExpansionGroups = 0,
        sentExpansionGroups = 0,
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

function baseUtils.modifyBaseUnitPoints(base, points, tag, x, y)

    if points > 0 and base.stateAI == BASE_AI_STATE_PEACEFUL then
        return
    end

    tag = tag or ""
    x = x or nil
    y = y or nil

    base.unitPoints = base.unitPoints + points

    local universe = base.universe
    local overflowMessage = ""
    if base.unitPoints > universe.maxOverflowPoints then
        base.unitPoints = universe.maxOverflowPoints
        overflowMessage = " [Point cap reached]"
    end

    local printPointChange = ""
    if points > 0 and universe.aiPointsPrintGainsToChat then
        printPointChange =  "+" .. string.format("%.2f", points)
    elseif points < 0 and universe.aiPointsPrintSpendingToChat then
        printPointChange = string.format("%.2f", points)
    end

    if printPointChange ~= "" then
        local gps = ""
        if x ~= nil then
            gps = " [gps=" .. x .. "," .. y .. "]"
        end
        game.print("[" .. base.id .. "]:" .. base.map.surface.name .. " " .. printPointChange .. " [" .. tag .. "] Unit Total:" .. string.format("%.2f", base.unitPoints) .. overflowMessage .. gps)
    end
end

function baseUtils.modifyBaseSpecialPoints(base, points, tag, x, y)

    if points > 0 and base.stateAI == BASE_AI_STATE_PEACEFUL then
        return
    end

    tag = tag or ""
    x = x or nil
    y = y or nil

    base.points = base.points + points

    local universe = base.universe
    local overflowMessage = ""
    if base.points > universe.maxOverflowPoints then
        base.points = universe.maxOverflowPoints
        overflowMessage = " [Point cap reached]"
    end

    local printPointChange = ""
    if points > 0 and universe.aiPointsPrintGainsToChat then
        printPointChange =  "+" .. string.format("%.2f", points)
    elseif points < 0 and universe.aiPointsPrintSpendingToChat then
        printPointChange = string.format("%.2f", points)
    end

    if printPointChange ~= "" then
        local gps = ""
        if x ~= nil then
            gps = " [gps=" .. x .. "," .. y .. "]"
        end
        game.print("[" .. base.id .. "]:" .. base.map.surface.name .. " " .. printPointChange .. " [" .. tag .. "] Special Total:" .. string.format("%.2f", base.points) .. overflowMessage .. gps)
    end
end

baseUtilsG = baseUtils
return baseUtils
