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


if BaseUtilsG then
    return BaseUtilsG
end
local BaseUtils = {}

--

local Universe

-- imports

local Utils = require("Utils")
local MathUtils = require("MathUtils")
local Constants = require("Constants")
local MapUtils = require("MapUtils")
local BaseLogic = require("base_logic")
local Executor = require("runtime/Executor")
local Command = require("command")

-- Constants

local PENDING_UPGRADE_CREATION_THESHOLD = Constants.PENDING_UPGRADE_CREATION_THESHOLD

local TIERS = Constants.TIERS
local EVO_TO_TIER_MAPPING = Constants.EVO_TO_TIER_MAPPING
local BUILDING_HIVE_TYPE_LOOKUP = Constants.BUILDING_HIVE_TYPE_LOOKUP
local COST_LOOKUP = Constants.COST_LOOKUP
local UPGRADE_LOOKUP = Constants.UPGRADE_LOOKUP

local ENEMY_ALIGNMENT_LOOKUP = Constants.ENEMY_ALIGNMENT_LOOKUP

local EVOLUTION_TABLE_ALIGNMENT = Constants.EVOLUTION_TABLE_ALIGNMENT
local BUILDING_EVOLVE_LOOKUP = Constants.BUILDING_EVOLVE_LOOKUP

local BASE_AI_STATE_RAIDING = Constants.BASE_AI_STATE_RAIDING
local BASE_AI_STATE_AGGRESSIVE = Constants.BASE_AI_STATE_AGGRESSIVE
local BASE_AI_STATE_MIGRATING = Constants.BASE_AI_STATE_MIGRATING
local BASE_AI_STATE_SIEGE = Constants.BASE_AI_STATE_SIEGE
local BASE_AI_STATE_ONSLAUGHT = Constants.BASE_AI_STATE_ONSLAUGHT

local MINIMUM_BUILDING_COST = Constants.MINIMUM_BUILDING_COST
local FACTION_MUTATION_MAPPING = Constants.FACTION_MUTATION_MAPPING

local MAGIC_MAXIMUM_NUMBER = Constants.MAGIC_MAXIMUM_NUMBER

local FACTIONS_BY_DAMAGE_TYPE = Constants.FACTIONS_BY_DAMAGE_TYPE

local BASE_GENERATION_STATE_ACTIVE = Constants.BASE_GENERATION_STATE_ACTIVE

local BASE_DISTANCE_THRESHOLD = Constants.BASE_DISTANCE_THRESHOLD
local BASE_DISTANCE_LEVEL_BONUS = Constants.BASE_DISTANCE_LEVEL_BONUS
local BASE_DISTANCE_TO_EVO_INDEX = Constants.BASE_DISTANCE_TO_EVO_INDEX

local CHUNK_SIZE = Constants.CHUNK_SIZE

local BASE_AI_STATE_PEACEFUL = Constants.BASE_AI_STATE_PEACEFUL

local BASE_RALLY_CHANCE = Constants.BASE_RALLY_CHANCE
local BONUS_RALLY_CHANCE = Constants.BONUS_RALLY_CHANCE

local RETREAT_MOVEMENT_PHEROMONE_LEVEL_MIN = Constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL_MIN
local RETREAT_MOVEMENT_PHEROMONE_LEVEL_MAX = Constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL_MAX
local MINIMUM_AI_POINTS = Constants.MINIMUM_AI_POINTS

local AI_POINT_GENERATOR_AMOUNT = Constants.AI_POINT_GENERATOR_AMOUNT

local BASE_AI_MIN_STATE_DURATION = Constants.BASE_AI_MIN_STATE_DURATION
local BASE_AI_MAX_STATE_DURATION = Constants.BASE_AI_MAX_STATE_DURATION
local NO_RETREAT_BASE_PERCENT = Constants.NO_RETREAT_BASE_PERCENT
local NO_RETREAT_EVOLUTION_BONUS_MAX = Constants.NO_RETREAT_EVOLUTION_BONUS_MAX

local AI_UNIT_REFUND = Constants.AI_UNIT_REFUND
local AI_MAX_POINTS = Constants.AI_MAX_POINTS
local BASE_GENERATION_MIN_STATE_DURATION = Constants.BASE_GENERATION_MIN_STATE_DURATION
local BASE_GENERATION_MAX_STATE_DURATION = Constants.BASE_GENERATION_MAX_STATE_DURATION
local BASE_GENERATION_STATE_DORMANT = Constants.BASE_GENERATION_STATE_DORMANT
local STATE_ENGLISH = Constants.STATE_ENGLISH

local ACTIVE_NESTS_PER_AGGRESSIVE_GROUPS = Constants.ACTIVE_NESTS_PER_AGGRESSIVE_GROUPS
local ALL_NESTS_PER_EXPANSION_GROUPS = Constants.ALL_NESTS_PER_EXPANSION_GROUPS
local TEMPERAMENT_RANGE_MAX = Constants.TEMPERAMENT_RANGE_MAX
local TEMPERAMENT_RANGE_MIN = Constants.TEMPERAMENT_RANGE_MIN
local TEMPERAMENT_DIVIDER = Constants.TEMPERAMENT_DIVIDER
local UNIT_DEATH_POINT_COST = Constants.UNIT_DEATH_POINT_COST
local BASE_PROCESS_INTERVAL = Constants.BASE_PROCESS_INTERVAL

-- imported functions

local getTimeStringFromTick = Utils.getTimeStringFromTick

local randomTickEvent = MathUtils.randomTickEvent
local linearInterpolation = MathUtils.linearInterpolation
local randomTickDuration = MathUtils.randomTickDuration

local isMember = Utils.isMember

local setPositionXYInQuery = Utils.setPositionXYInQuery

local euclideanDistancePoints = MathUtils.euclideanDistancePoints

local getChunkByPosition = MapUtils.getChunkByPosition

local gaussianRandomRangeRG = MathUtils.gaussianRandomRangeRG

local mFloor = math.floor
local mCeil = math.ceil

local tableSize = table_size

local tableRemove = table.remove
local mMin = math.min
local mMax = math.max

local next = next

-- module code

local function evoToTier(evolutionFactor, maxSkips)
    local v
    local skipsRemaining = maxSkips
    for i=TIERS,1,-1 do
        if EVO_TO_TIER_MAPPING[i] <= evolutionFactor then
            v = i
            if (skipsRemaining == 0) or (Universe.random() <= 0.75) then
                break
            end
            skipsRemaining = skipsRemaining - 1
        end
    end
    return v
end

local function findBaseMutation(targetEvolution, excludeFactions)
    local tier = evoToTier(targetEvolution or Universe.evolutionLevel or 0, 2)
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

    local roll = Universe.random()
    for i=1,#availableAlignments do
        local alignment = availableAlignments[i]

        roll = roll - alignment[1]

        if (roll <= 0) then
            return alignment[2]
        end
    end
    return availableAlignments[#availableAlignments]
end

local function initialEntityUpgrade(baseAlignment, tier, maxTier, useHiveType, entityType)
    local entity

    local useTier

    local tierRoll = Universe.random()
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
                    entity = upgrade[2][Universe.random(#upgrade[2])]
                    break
                end
            end
        end
        if not entity then
            for ui=1,#upgrades do
                local upgrade = upgrades[ui]
                if upgrade[3] == entityType then
                    entity = upgrade[2][Universe.random(#upgrade[2])]
                end
            end
            if not entity then
                local mapTypes = FACTION_MUTATION_MAPPING[entityType]
                for i=1, #mapTypes do
                    local mappedType = mapTypes[i]
                    for ui=1,#upgrades do
                        local upgrade = upgrades[ui]
                        if upgrade[3] == mappedType then
                            return upgrade[2][Universe.random(#upgrade[2])]
                        end
                    end
                end
            end
        end
    end

    return entity
end

local function entityUpgrade(baseAlignment, tier, maxTier, originalEntity)
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
                    entity = upgrade[Universe.random(#upgrade)]
                    if Universe.random() < 0.55 then
                        return entity
                    end
                end
            end
        elseif (#upgrades > 0) then
            entity = upgrades[Universe.random(#upgrades)]
            if Universe.random() < 0.55 then
                return entity
            end
        end
    end
    return entity
end

local function findEntityCreation(baseAlignment, evoIndex, position, map, entityType)
    local tier = evoToTier(evoIndex, 5)
    local maxTier = evoToTier(evoIndex, 4)

    if (tier > maxTier) then
        maxTier = tier
    end

    local chunk = getChunkByPosition(map, position)
    if not entityType then
        if Universe.random() < 0.5 then
            entityType = "biter-spawner"
        else
            entityType = "spitter-spawner"
        end
    end
    local roll = Universe.random()
    local makeHive = (chunk ~= -1) and
        (
            (entityType == "biter-spawner") or (entityType == "spitter-spawner")
        )
        and
        (
            (
                (roll <= 0.01)
            )
            or
            (
                (roll <= 0.210) and
                chunk.resourceGenerator
            )
        )
    -- New structures start at the bottom and grow through the base's waves:
    -- a nest built by a tier-10 base is tier 1, not tier 10. Combined with the
    -- +1-per-wave step this makes the population ramped by age rather than
    -- spawned at the cap.
    local startTier = 1
    local name = baseAlignment .. "-" .. (makeHive and "hive" or entityType)
        .. "-v1-t" .. startTier .. "-rampant"
    if Constants.IDENTITY_LOOKUP and Constants.IDENTITY_LOOKUP[name] then
        return name
    end
    return initialEntityUpgrade(baseAlignment, tier, maxTier, (makeHive and "hive"), entityType)
end

local function findEntityUpgrade(baseAlignment, currentEvo, evoIndex, originalEntity, map, evolve, targetTier)
    local adjCurrentEvo = mMax(
        ((baseAlignment ~= ENEMY_ALIGNMENT_LOOKUP[originalEntity.name]) and 0) or currentEvo,
        0
    )

    local tier = evoToTier(adjCurrentEvo, 5)
    local maxTier = evoToTier(evoIndex, 4)

    if (tier > maxTier) then
        maxTier = tier
    end

    if evolve then
        local chunk = getChunkByPosition(map, originalEntity.position)
        local entityName = originalEntity.name
        local entityType = BUILDING_HIVE_TYPE_LOOKUP[entityName]
        -- Deterministic per structure, NOT Universe.random(): the evolve path is
        -- re-run every time a structure is re-queued (chunk scan, re-register),
        -- and with a fresh random roll each time the target tier/type changed, so
        -- the same structure was upgraded again and again (measured 14,869
        -- upgrades at 6,062 positions in 12.7 game-minutes, up to 15 times at one
        -- position, oscillating t9 -> t6 -> t9 -> t8). Keyed on the position so a
        -- re-queue reproduces the same target and the name==current check stops
        -- it.
        local roll0 = mFloor(originalEntity.position.x)
        local roll1 = mFloor(originalEntity.position.y)
        -- Factorio's Lua is 5.2: no bitwise operators. Plain arithmetic hash.
        local h = (roll0 * 73856093 + roll1 * 19349663) % 1000
        local firstRoll = h / 1000
        local secondRoll = ((h * 37) % 1000) / 1000
        if not entityType then
            if firstRoll < 0.5 then
                entityType = "biter-spawner"
            else
                entityType = "spitter-spawner"
            end
        end
        local roll = secondRoll
        local makeHive = (chunk ~= -1) and
            (
                (entityType == "biter-spawner") or (entityType == "spitter-spawner")
            )
            and
            (
                (
                    (roll <= 0.01)
                )
                or
                (
                    (roll <= 0.210) and
                    chunk.resourceGenerator
                )
            )
        -- The tier is the base's version, not a roll: the name is built the same
        -- way the data stage builds it, so IDENTITY_LOOKUP has it.
        local useTier = targetTier or maxTier or 0
        if useTier > 0 and useTier < tier then useTier = tier end
        if useTier <= 0 then
            -- Nothing to upgrade to: a no-op, not a devolution. The structure
            -- keeps its identity and is not queued again until a version change.
            return nil
        end
        local typeName = makeHive and "hive" or entityType
        local name = baseAlignment .. "-" .. typeName .. "-v1-t" .. useTier .. "-rampant"
        if Constants.IDENTITY_LOOKUP and Constants.IDENTITY_LOOKUP[name] then
            return name
        end
        return nil
    else
        return entityUpgrade(baseAlignment, tier, maxTier, originalEntity)
    end
end

-- local
function BaseUtils.findBaseInitialAlignment(evoIndex, excludeFactions)
    local dev = evoIndex * 0.15
    local evoTop = gaussianRandomRangeRG(evoIndex - (evoIndex * 0.075), dev, 0, evoIndex, Universe.random)

    local result
    if Universe.random() < 0.05 then
        result = {findBaseMutation(evoTop, excludeFactions), findBaseMutation(evoTop, excludeFactions)}
    else
        result = {findBaseMutation(evoTop, excludeFactions)}
    end

    return result
end

function BaseUtils.recycleBases()
    local bases = Universe.bases
    local id = Universe.recycleBaseIterator
    local base
    if not id then
        id, base = next(bases, nil)
    else
        base = bases[id]
    end
    if not id then
        Universe.recycleBaseIterator = nil
    else
        Universe.recycleBaseIterator = next(bases, id)
        local map = base.map
        if (base.chunkCount == 0) or not map.surface.valid then
            bases[id] = nil
            if MetricsG then
                MetricsG.inc("base.recycled")
            end
            if Universe.processBaseAIIterator == id then
                Universe.processBaseAIIterator = nil
            end
            if map.surface.valid then
                Universe.bases[id] = nil
            end
        end
    end
end

-- PURE: Check if base can attack (testable)
function BaseUtils.canAttackBase(stateAI, sentAggressiveGroups, maxAggressiveGroups, peacefulMode, aiNocturnalMode, darkness, raidAIToggle, sentExpansionGroups, maxExpansionGroups)
    return BaseLogic.canAttack({
        stateAI = stateAI,
        sentAggressiveGroups = sentAggressiveGroups,
        maxAggressiveGroups = maxAggressiveGroups,
        peacefulMode = peacefulMode,
        aiNocturnalMode = aiNocturnalMode,
        darkness = darkness,
        raidAIToggle = raidAIToggle,
        sentExpansionGroups = sentExpansionGroups,
        maxExpansionGroups = maxExpansionGroups,
    })
end

-- PUBLIC: Wrapper that reads storage.universe
function BaseUtils.canAttack(base)
    local universe = storage.universe
    local surface = base.map.surface
    return BaseUtils.canAttackBase(
        base.stateAI,
        base.sentAggressiveGroups,
        base.maxAggressiveGroups,
        surface.peaceful_mode,
        universe.aiNocturnalMode,
        surface.darkness,
        universe.raidAIToggle,
        base.sentExpansionGroups,
        base.maxExpansionGroups
    )
end

-- PURE: Check if base can migrate (testable)
function BaseUtils.canMigrateBase(stateAI, expansion, peacefulMode, aiNocturnalMode, darkness)
    return BaseLogic.canMigrate({
        stateAI = stateAI,
        expansion = expansion,
        peacefulMode = peacefulMode,
        aiNocturnalMode = aiNocturnalMode,
        darkness = darkness,
    })
end

-- PUBLIC: Wrapper that reads storage.universe
function BaseUtils.canMigrate(base)
    local universe = storage.universe
    local surface = base.map.surface
    return BaseUtils.canMigrateBase(
        base.stateAI,
        universe.expansion,
        surface.peaceful_mode,
        universe.aiNocturnalMode,
        surface.darkness
    )
end

function BaseUtils.queueUpgrade(entity, base, disPos, evolve, timeDelay)
    local map = base.map
    local baseAlignment = base.alignment
    local position = disPos or entity.position

    if entity and entity.valid then
        local st = Universe.spawnTelemetry
        if not st then
            st = { total = 0, near64 = 0, maxShift = 0, blocked = 0 }
            Universe.spawnTelemetry = st
        end
        local epos = entity.position
        st.enq = (st.enq or 0) + 1
        if (math.abs(epos.x) < 64) and (math.abs(epos.y) < 64) then
            st.enqNear64 = (st.enqNear64 or 0) + 1
        end
        local un = entity.unit_number
        if string.find(entity.name, "rampant", 1, true) then
            st.enqRampant = (st.enqRampant or 0) + 1
        else
            st.enqVanilla = (st.enqVanilla or 0) + 1
        end
    end

    if (base.alignmentHistory and (Universe.evolutionLevel or 0) > 0.5) then
        if #base.alignmentHistory > 1 then
            if (tick and (tick % 2 == 0)) then
                pickedBaseAlignment = baseAlignment[2]
            else
                pickedBaseAlignment = baseAlignment[1]
            end
        else
            pickedBaseAlignment = baseAlignment[1]
        end
    else
        pickedBaseAlignment = baseAlignment[1]
    end

    -- The tier target is the base's version, capped by evolution/distance. A
    -- structure is a candidate only when the identity it holds differs from the
    -- base's: one upgrade per identity change, no per-queue random reroll.
    local distance = mMin(
        1,
        euclideanDistancePoints(position.x, position.y, 0, 0) * BASE_DISTANCE_TO_EVO_INDEX
    )
    local evoIndex = mMax(distance, Universe.evolutionLevel or 0)
    -- Evolution is the CAP and also raises the version when it crosses a tier
    -- boundary; points can raise it further but never past the cap. The version
    -- itself never falls here (devolution is an explicit event).
    local capTier = evoToTier(evoIndex, 4) or 0
    if not base.upgradeVersion then
        base.upgradeVersion = capTier
    end
    if base.upgradeVersion < 0 then
        base.upgradeVersion = 0
    end
    -- Monotone: evolution may raise the version, never lower it. Version 0 means
    -- "no upgrade available", which is a valid resting state.
    if capTier > base.upgradeVersion then
        base.upgradeVersion = capTier
    end
    if base.upgradeVersion > capTier then
        capTier = base.upgradeVersion
    end

    local factionId = Constants.FACTION_INDEX[pickedBaseAlignment] or base.factionId or 0
    local oldIdentity = base.identity
    base.factionId = factionId
    base.identity = factionId * Constants.IDENTITY_TIER_STRIDE + base.upgradeVersion
    if oldIdentity and oldIdentity ~= base.identity then
        base.versionCounter = (base.versionCounter or 0) + 1
        if TelemetryG and TelemetryG.isEnabled() then
            TelemetryG.emit("base.version", {
                base = base.id, old = oldIdentity, new = base.identity,
                tier = base.upgradeVersion, faction = factionId,
            })
        end
        -- One wave per identity change: every structure of the base becomes a
        -- candidate once. Guarded so the nested queueUpgrade calls do not wave
        -- again.
        if not base.waveInProgress then
            base.waveInProgress = true
            for unitNumber, packed in pairs(base.structures or {}) do
                local member = base.structureEntities and base.structureEntities[unitNumber]
                if packed ~= base.identity and member and member.valid then
                    BaseUtils.queueUpgrade(member, base, nil, true)
                end
            end
            base.waveInProgress = nil
        end
    end

    local unitNumber = entity.unit_number
    -- 0 means "no rampant identity yet" (a vanilla structure); it never equals a
    -- real identity, so vanilla structures convert on their first eligible wave.
    local packed = (unitNumber and base.structures and base.structures[unitNumber])
        or (Constants.IDENTITY_LOOKUP and Constants.IDENTITY_LOOKUP[entity.name])
        or 0
    local currentTier = packed % Constants.IDENTITY_TIER_STRIDE
    local wave = base.versionCounter or 0
    base.structureAttempted = base.structureAttempted or {}
    if unitNumber and base.structureAttempted[unitNumber] == wave then
        return -- already rolled this wave
    end

    -- The roll is a STEP from the structure's own tier, capped by the base tier,
    -- not an absolute target: +0..+3 so a tier-1 nest reaches at most tier 4 in
    -- one wave instead of jumping straight to the cap. A vanilla structure (tier
    -- 0) always takes at least +1 so it gains a rampant identity.
    local gap = base.upgradeVersion - currentTier
    local rolledTier = currentTier
    if currentTier == 0 and gap > 0 then
        -- A vanilla structure enters at the bottom of its base's range - three
        -- tiers under the base version, never below 1 - and then grows one wave
        -- at a time with everything else. A high-evolution base therefore does
        -- not spawn tier-1 nests forever, but it also does not spawn at the cap.
        rolledTier = mMax(1, base.upgradeVersion - 3)
    elseif gap > 0 then
        local h = (mFloor(position.x) * 73856093 + mFloor(position.y) * 19349663
            + wave * 2654435761) % 1000
        local r = h / 1000
        -- Growth is meant to be slow and uneven, not a race to the base tier:
        -- +1 is the common case, +2 less common, +3 rare, and a small share
        -- simply does not move this wave. A 4 -> 7 jump is a big jump.
        local step
        if r < 0.10 then
            step = 0
        elseif r < 0.70 then
            step = 1
        elseif r < 0.95 then
            step = 2
        else
            step = 3
        end
        if step > gap then
            step = gap
        end
        rolledTier = currentTier + step
    end
    if unitNumber then
        base.structureAttempted[unitNumber] = wave
    end
    if rolledTier <= currentTier then
        if TelemetryG and TelemetryG.isEnabled() then
            TelemetryG.emit("upgrade.skipped", {
                base = base.id, name = entity.name, tier = currentTier, wave = wave,
            })
        end
        return
    end
    local targetFactionId = Constants.FACTION_INDEX[pickedBaseAlignment] or base.factionId or 0
    if unitNumber and base.structures then
        base.structures[unitNumber] = targetFactionId * Constants.IDENTITY_TIER_STRIDE + rolledTier
    end

    local name = findEntityUpgrade(pickedBaseAlignment,
                                   capTier,
                                   evoIndex,
                                   entity,
                                   map,
                                   evolve,
                                   rolledTier)

    local entityName = entity.name
    if not name or (name == entityName) then
        return
    end

    local surface = map.surface
    if not evolve and Universe.printBaseUpgrades then
        surface.print(
            "["..base.id.."]:"..surface.name.." Upgrading "
            .. entityName .. " to " .. name
            .. " [gps=".. position.x ..",".. position.y .."]"
        )
    end

    local unitNumber = entity.unit_number

    -- The placement search happens at execution time (Processor state 2 already
    -- retries with find_non_colliding_position radius 16). Doing it here as well
    -- cost ~0.9ms per queued upgrade, measured inside the chunk scan.

    Universe.pendingUpgrades[Universe.upgradeId] = {
        ["kind"] = "upgrade",
        ["position"] = position,
        ["map"] = map,
        ["name"] = name,
        ["entity"] = entity,
        ["delayTLL"] = timeDelay,
        ["hive"] = Universe.hives[unitNumber] or Universe.hiveData[unitNumber],
        ["base"] = base,
        ["chunkId"] = (MapUtils.getChunkByPosition(map, position) ~= -1)
            and MapUtils.getChunkByPosition(map, position).id or nil,
        ["oldUnitNumber"] = unitNumber,
        ["state"] = 1
    }
    Universe.pendingUpgradesLength = Universe.pendingUpgradesLength + 1
    Universe.upgradeId = Universe.upgradeId + 1
end

function BaseUtils.queueCreation(base, position, entityType, timeDelay, hiveData)
    local map = base.map
    local baseAlignment = base.alignment

    if Universe.pendingUpgradesLength > PENDING_UPGRADE_CREATION_THESHOLD then
        return
    end

    local pickedBaseAlignment
    if baseAlignment[2] then
        if Universe.random() < 0.75 then
            pickedBaseAlignment = baseAlignment[2]
        else
            pickedBaseAlignment = baseAlignment[1]
        end
    else
        pickedBaseAlignment = baseAlignment[1]
    end

    local distance = mMin(
        1,
        euclideanDistancePoints(position.x, position.y, 0, 0) * BASE_DISTANCE_TO_EVO_INDEX
    )
    local evoIndex = mMax(distance, Universe.evolutionLevel or 0)

    local name = findEntityCreation(
        pickedBaseAlignment,
        evoIndex,
        position,
        map,
        entityType
    )

    if not name then
        return
    end

    position = map.surface.find_non_colliding_position(
        name,
        position,
        8,
        1,
        true
    )

    if not position then
        return
    end

    Universe.pendingUpgrades[Universe.upgradeId] = {
        ["kind"] = (entityType == "hive" and "hive") or (entityType == "turret" and "turret") or "nest",
        ["position"] = position,
        ["map"] = map,
        ["name"] = name,
        ["delayTLL"] = timeDelay,
        ["hive"] = hiveData,
        ["state"] = 2
    }
    Universe.pendingUpgradesLength = Universe.pendingUpgradesLength + 1
    Universe.upgradeId = Universe.upgradeId + 1
end

local function pickMutationFromDamageType(damageType, roll, base)
    local baseAlignment = base.alignment

    local damageFactions = FACTIONS_BY_DAMAGE_TYPE[damageType]
    local mutation
    local mutated = false

    if damageFactions and (#damageFactions > 0) then
        mutation = damageFactions[Universe.random(#damageFactions)]
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
        mutation = findBaseMutation()
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
    if mutated and Universe.printBaseAdaptation then
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
                        Universe.MAX_BASE_MUTATIONS})
        else
            game.print({"description.rampant--adaptation1DebugMessage",
                        base.id,
                        base.map.surface.name,
                        damageType,
                        {"description.rampant--"..baseAlignment[1].."EnemyName"},
                        base.x,
                        base.y,
                        base.mutations,
                        Universe.MAX_BASE_MUTATIONS})
        end
    end
    local alignmentCount = tableSize(base.alignmentHistory)
    while (alignmentCount > Universe.MAX_BASE_ALIGNMENT_HISTORY) do
        tableRemove(base.alignmentHistory, 1)
        alignmentCount = alignmentCount - 1
    end
    return mutated
end

function BaseUtils.upgradeBaseBasedOnDamage(base)

    local total = 0

    for _,amount in pairs(base.damagedBy) do
        total = total + amount
    end
    local mutationAmount = total * 0.176471
    base.damagedBy["RandomMutation"] = mutationAmount
    total = total + mutationAmount
    local pickedDamage
    local roll = Universe.random()
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

    return pickMutationFromDamageType(pickedDamage, roll, base)
end

function BaseUtils.processBaseMutation(chunk, map, base)
    if not base.alignment[1]
        or (base.stateGeneration ~= BASE_GENERATION_STATE_ACTIVE)
        or (Universe.random() >= 0.30)
        or (Universe.pendingUpgradesLength > PENDING_UPGRADE_CREATION_THESHOLD)
    then
        return
    end

    -- Points buy a base version bump, not a random structure upgrade: the base
    -- "researches" the next tier up to the evolution cap, and every structure it
    -- owns becomes a candidate exactly once.
    if (base.points >= MINIMUM_BUILDING_COST) then
        local distance = mMin(
            1,
            euclideanDistancePoints(chunk.x, chunk.y, 0, 0) * BASE_DISTANCE_TO_EVO_INDEX
        )
        local evoIndex = mMax(distance, Universe.evolutionLevel or 0)
        local capTier = evoToTier(evoIndex, 4)
        local version = base.upgradeVersion or 0
        if version < capTier then
            local cost = (COST_LOOKUP[chunk.nestIds and "biter-spawner"] or MINIMUM_BUILDING_COST)
            if base.points >= cost then
                BaseUtils.modifyBaseSpecialPoints(base, -cost, "Base tier research", chunk.x, chunk.y)
                local oldIdentity = base.identity
                base.upgradeVersion = version + 1
                base.identity = (base.factionId or 0) * Constants.IDENTITY_TIER_STRIDE + base.upgradeVersion
                base.versionCounter = (base.versionCounter or 0) + 1
                if TelemetryG and TelemetryG.isEnabled() then
                    TelemetryG.emit("base.version", {
                        base = base.id, old = oldIdentity, new = base.identity,
                        tier = base.upgradeVersion, reason = "points",
                    })
                end
                for unitNumber, packed in pairs(base.structures or {}) do
                    local entity = base.structureEntities and base.structureEntities[unitNumber]
                    if packed ~= base.identity and entity and entity.valid then
                        BaseUtils.queueUpgrade(entity, base, nil, true)
                    end
                end
            end
        end
    end
end

function BaseUtils.createBase(map, chunk, tick)
    local x = chunk.x
    local y = chunk.y
    local distance = euclideanDistancePoints(x, y, 0, 0)

    local meanLevel = mFloor(distance * 0.005)

    local distanceIndex = mMin(1, distance * BASE_DISTANCE_TO_EVO_INDEX)
    local evoIndex = mMax(distanceIndex, Universe.evolutionLevel or 0)

    local alignment =
        (Universe.NEW_ENEMIES and BaseUtils.findBaseInitialAlignment(evoIndex))
        or {"neutral"}

    local baseLevel = gaussianRandomRangeRG(meanLevel,
                                            meanLevel * 0.3,
                                            meanLevel * 0.50,
                                            meanLevel * 1.50,
                                            Universe.random)
    local baseDistanceThreshold = gaussianRandomRangeRG(BASE_DISTANCE_THRESHOLD,
                                                        BASE_DISTANCE_THRESHOLD * 0.2,
                                                        BASE_DISTANCE_THRESHOLD * 0.75,
                                                        BASE_DISTANCE_THRESHOLD * 1.50,
                                                        Universe.random)
    local distanceThreshold = (baseLevel * BASE_DISTANCE_LEVEL_BONUS) + baseDistanceThreshold

    -- The base owns a versioned identity: factionId * IDENTITY_TIER_STRIDE + tier
    -- target (tiers are capped at 10 and asserted below the stride in Constants). Every
    -- structure it owns stores the identity it was built with, so "is this a
    -- candidate" is an integer comparison and each structure upgrades at most
    -- once per identity change.
    local baseFactionType = alignment[1] or "neutral"
    local baseFactionId = Constants.FACTION_INDEX[baseFactionType] or 0
    local baseTier = evoToTier(evoIndex, 4) or 0

    local base = {
        x = x,
        y = y,
        factionId = baseFactionId,
        upgradeVersion = baseTier,
        identity = baseFactionId * Constants.IDENTITY_TIER_STRIDE + baseTier,
        versionCounter = 0,
        structures = {},
        structureAttempted = {},
        structureEntities = {},
        totalX = 0,
        totalY = 0,
        distanceThreshold = distanceThreshold * Universe.baseDistanceModifier,
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
        resourceChunks = {},
        resourceChunkCount = 0,
        temperament = 0.5,
        temperamentScore = 0,
        map = map,
        id = Universe.baseId
    }
    Universe.baseId = Universe.baseId + 1

    map.bases[base.id] = base
    Universe.bases[base.id] = base
    if MetricsG then
        MetricsG.inc("base.created")
    end

    return base
end

function BaseUtils.modifyBaseUnitPoints(base, points, tag, x, y)

    if points > 0 and base.stateAI == BASE_AI_STATE_PEACEFUL then
        return
    end

    tag = tag or ""
    x = x or nil
    y = y or nil

    base.unitPoints = base.unitPoints + points

    local overflowMessage = ""
    if base.unitPoints > Universe.maxOverflowPoints then
        base.unitPoints = Universe.maxOverflowPoints
        overflowMessage = " [Point cap reached]"
    end

    local printPointChange = ""
    if points > 0 and Universe.aiPointsPrintGainsToChat then
        printPointChange =  "+" .. string.format("%.2f", points)
    elseif points < 0 and Universe.aiPointsPrintSpendingToChat then
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

function BaseUtils.modifyBaseSpecialPoints(base, points, tag, x, y)

    if points > 0 and base.stateAI == BASE_AI_STATE_PEACEFUL then
        return
    end

    tag = tag or ""
    x = x or nil
    y = y or nil

    if TelemetryG and TelemetryG.isEnabled() then
        TelemetryG.emit("base.points", {
            base = base.id, delta = points, tag = tag,
            total = base.points + points, x = x, y = y,
            state = base.stateAI,
        })
    end
    base.points = base.points + points

    local overflowMessage = ""
    if base.points > Universe.maxOverflowPoints then
        base.points = Universe.maxOverflowPoints
        overflowMessage = " [Point cap reached]"
    end

    local printPointChange = ""
    if points > 0 and Universe.aiPointsPrintGainsToChat then
        printPointChange =  "+" .. string.format("%.2f", points)
    elseif points < 0 and Universe.aiPointsPrintSpendingToChat then
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

function BaseUtils.planning(evolutionLevel)
    Universe.evolutionLevel = evolutionLevel
    local maxPoints = mMax(AI_MAX_POINTS * evolutionLevel, MINIMUM_AI_POINTS)
    Universe.maxPoints = maxPoints

    local maxOverflowPoints = maxPoints * 3
    Universe.maxOverflowPoints = maxOverflowPoints

    local attackWaveMaxSize = Universe.attackWaveMaxSize
    Universe.retreatThreshold = linearInterpolation(evolutionLevel,
                                                    RETREAT_MOVEMENT_PHEROMONE_LEVEL_MIN,
                                                    RETREAT_MOVEMENT_PHEROMONE_LEVEL_MAX)
    Universe.rallyThreshold = BASE_RALLY_CHANCE + (evolutionLevel * BONUS_RALLY_CHANCE)
    Universe.formSquadThreshold = mMax((0.35 * evolutionLevel), 0.1)

    Universe.attackWaveSize = attackWaveMaxSize * (evolutionLevel ^ 1.4)
    Universe.attackWaveDeviation = (Universe.attackWaveSize * 0.333)
    Universe.attackWaveUpperBound = Universe.attackWaveSize + (Universe.attackWaveSize * 0.35)

    if (Universe.attackWaveSize < 1) then
        Universe.attackWaveSize = 2
        Universe.attackWaveDeviation = 1
        Universe.attackWaveUpperBound = 3
    end

    Universe.settlerWaveSize = linearInterpolation(evolutionLevel ^ 1.66667,
                                                   Universe.expansionMinSize,
                                                   Universe.expansionMaxSize)
    Universe.settlerWaveDeviation = (Universe.settlerWaveSize * 0.33)

    Universe.unitRefundAmount = AI_UNIT_REFUND * evolutionLevel
    Universe.kamikazeThreshold = NO_RETREAT_BASE_PERCENT + (evolutionLevel * NO_RETREAT_EVOLUTION_BONUS_MAX)
end

local function processBase(base, tick)

    base.maxAggressiveGroups = mCeil(base.activeNests / ACTIVE_NESTS_PER_AGGRESSIVE_GROUPS)
    base.maxExpansionGroups = mCeil((base.activeNests + base.activeRaidNests) / ALL_NESTS_PER_EXPANSION_GROUPS)

    if (base.stateAI == BASE_AI_STATE_MIGRATING or base.stateAI == BASE_AI_STATE_SIEGE)
        and base.resetExpensionGroupsTick <= tick
    then
        local randomDuration = randomTickDuration(Universe.random,
                                                  Universe.expansionMinTime,
                                                  mFloor(linearInterpolation(Universe.evolutionLevel ^ 1.66667,
                                                                             Universe.expansionMaxTime,
                                                                             Universe.expansionMinTime)))
        base.resetExpensionGroupsTick = tick + randomDuration
        base.sentExpansionGroups = 0
    end

    local points = (AI_POINT_GENERATOR_AMOUNT * Universe.random()) +
        (base.activeNests * 0.144) +
        (AI_POINT_GENERATOR_AMOUNT * mMax(Universe.evolutionLevel ^ 2.5, 0.1))

    if (base.temperament == 0) or (base.temperament == 1) then
        points = points + 24
    elseif (base.temperament < 0.20) or (base.temperament > 0.80) then
        points = points + 14.4
    elseif (base.temperament < 0.35) or (base.temperament > 0.65) then
        points = points + 9.6
    elseif (base.temperament < 0.45) or (base.temperament > 0.55) then
        points = points + 4.8
    end

    if (base.stateAI == BASE_AI_STATE_ONSLAUGHT) then
        points = points * 2
    end

    points = points * Universe.aiPointsScaler

    local currentPoints = base.unitPoints

    if (currentPoints <= 0) then
        currentPoints = 0
    end

    if (currentPoints < Universe.maxPoints) then
        BaseUtils.modifyBaseUnitPoints(base, points, "Logic Cycle", base.x, base.y)
    end

    if (base.points < Universe.maxPoints) then
        BaseUtils.modifyBaseSpecialPoints(base, (points * 0.75), "Logic Cycle", base.x, base.y)
    end

    if Universe.NEW_ENEMIES then
        local deathThreshold = 0
        local evolutionLevel = Universe.evolutionLevel
        if (evolutionLevel < Universe.minimumAdaptationEvolution) then
            base.deathEvents = 0
        elseif (evolutionLevel < 0.5) then
            deathThreshold = 17000
        elseif (evolutionLevel < 0.7) then
            deathThreshold = 34000
        elseif (evolutionLevel < 0.9) then
            deathThreshold = 60000
        else
            deathThreshold = 100000
        end

        deathThreshold = Universe.adaptationModifier * deathThreshold
        if ((base.deathEvents > deathThreshold) and (Universe.random() > 0.95)) then
            if (base.mutations < Universe.MAX_BASE_MUTATIONS) then
                if BaseUtils.upgradeBaseBasedOnDamage(base) then
                    base.mutations = base.mutations + 1
                end
            elseif (base.mutations == Universe.MAX_BASE_MUTATIONS) then
                local roll = Universe.random()
                if (roll < 0.001) then
                    base.mutations = 0
                    if (Universe.printBaseAdaptation) then
                        game.print({"description.rampant--adaptationResetDebugMessage",
                                    base.x,
                                    base.y,
                                    base.mutations,
                                    Universe.MAX_BASE_MUTATIONS})
                    end
                elseif (roll > 0.999) then
                    base.mutations = base.mutations + 1
                    if (Universe.printBaseAdaptation) then
                        game.print({"description.rampant--adaptationFrozenDebugMessage",
                                    base.x,
                                    base.y})
                    end
                end
            end
            base.damagedBy = {}
            base.deathEvents = 0
        end
    else
        base.damagedBy = {}
        base.deathEvents = 0
    end

    if (base.stateGenerationTick <= tick) then
        local roll = Universe.random()
        if (roll < 0.85) then
            base.stateGeneration = BASE_GENERATION_STATE_ACTIVE
        else
            base.stateGeneration = BASE_GENERATION_STATE_DORMANT
        end
        base.stateGenerationTick = randomTickEvent(Universe.random,
                                                   tick,
                                                   BASE_GENERATION_MIN_STATE_DURATION,
                                                   BASE_GENERATION_MAX_STATE_DURATION)
    end

    base.tick = tick
end

local function temperamentPlanner(base, evolutionLevel)
    local destroyPlayerBuildings = base.destroyPlayerBuildings
    local lostEnemyUnits = base.lostEnemyUnits
    local lostEnemyBuilding = base.lostEnemyBuilding
    local rocketLaunched = base.rocketLaunched
    local builtEnemyBuilding = base.builtEnemyBuilding
    local ionCannonBlasts = base.ionCannonBlasts
    local artilleryBlasts = base.artilleryBlasts
    local activeNests = base.activeNests
    local activeRaidNests = base.activeRaidNests

    local currentTemperament = base.temperamentScore
    local delta = 0

    if activeNests > 0 then
        local val = (5.76 * activeNests)
        delta = delta + val
    else
        delta = delta - 5.553792
    end

    if destroyPlayerBuildings > 0 then
        if currentTemperament > 0 then
            delta = delta - (5.553792 * destroyPlayerBuildings)
        else
            delta = delta + (5.553792 * destroyPlayerBuildings)
        end
    end

    if activeRaidNests > 0 then
        local val = (0.2304 * activeRaidNests)
        delta = delta - val
    else
        delta = delta - 3.84
    end

    if lostEnemyUnits > 0 then
        local multipler
        if evolutionLevel < 0.3 then
            multipler = 0.083328
        elseif evolutionLevel < 0.5 then
            multipler = 0.041472
        elseif evolutionLevel < 0.7 then
            multipler = 0.020736
        elseif evolutionLevel < 0.9 then
            multipler = 0.010368
        elseif evolutionLevel < 0.9 then
            multipler = 0.005184
        else
            multipler = 0.002592
        end
        local val = (multipler * lostEnemyUnits)
        if (currentTemperament > 0) then
            delta = delta - val
        else
            delta = delta + val
        end
    end

    if lostEnemyBuilding > 0 then
        local val = (0.576 * lostEnemyBuilding)
        if (currentTemperament > 0) then
            delta = delta - val
        else
            delta = delta + val
        end
    end

    if builtEnemyBuilding > 0 then
        local val = (0.261952 * builtEnemyBuilding)
        if (currentTemperament > 0) then
            delta = delta - val
        else
            delta = delta + val
        end
    else
        delta = delta - 2.777088
    end

    if (rocketLaunched > 0) then
        local val = (27.76 * rocketLaunched)
        delta = delta + val
    end

    if (ionCannonBlasts > 0) then
        local val = (13.924864 * ionCannonBlasts)
        delta = delta + val
    end

    if (artilleryBlasts > 0) then
        local val = (13.924864 * artilleryBlasts)
        delta = delta + val
    end

    delta = delta * Universe.temperamentRateModifier
    base.temperamentScore = mMin(TEMPERAMENT_RANGE_MAX, mMax(TEMPERAMENT_RANGE_MIN, currentTemperament + delta))
    base.temperament = ((base.temperamentScore + TEMPERAMENT_RANGE_MAX) * TEMPERAMENT_DIVIDER)

    if Universe.debugTemperament then
        local strConsole = "Rampant Stats:\nbaseId:".. base.id .. ", aN:" .. base.activeNests ..
            ", aRN:" .. base.activeRaidNests .. ", dPB:" ..
            base.destroyPlayerBuildings .. ", lEU:" .. base.lostEnemyUnits .. ", lEB:" ..
            base.lostEnemyBuilding .. ", rL:" .. base.rocketLaunched .. ", bEB:" ..
            base.builtEnemyBuilding .. ", iCB:" .. base.ionCannonBlasts .. ", aB:" ..
            base.artilleryBlasts .. ", temp:" .. base.temperament .. ", tempScore:" .. base.temperamentScore ..
            ", points:" .. base.points .. ", unitPoints:" .. base.unitPoints .. ", state:" ..
            STATE_ENGLISH[base.stateAI] .. ", surface:" .. base.map.surface.index .. " [" ..
            base.map.surface.name .. "]" .. ", aS:" .. Universe.squadCount .. ", aB:" .. Universe.builderCount ..
            ", atkSize:" .. Universe.attackWaveSize .. ", stlSize:" .. Universe.settlerWaveSize ..
            ", formGroup:" .. Universe.formSquadThreshold .. ", sAgg:".. base.sentAggressiveGroups ..
            ", mAgg:" .. base.maxAggressiveGroups .. ", baseState:" .. base.stateGeneration .. ", sE:"
            .. base.sentExpansionGroups .. ", mE:" .. base.maxExpansionGroups
        game.print(strConsole)
        print(strConsole)
    end
end

local function processState(base, tick)

    if (base.stateAITick > tick) or not Universe.awake then
        if (not Universe.awake) and (tick >= Universe.initialPeaceTime) then
            Universe.awake = true
            if Universe.printAwakenMessage then
                game.print({"description.rampant--planetHasAwoken"})
            end
        else
            return
        end
    end
    local roll = Universe.random()

    -- Pure state selection
    local newState = BaseLogic.selectState({
        temperament = base.temperament,
        roll = roll,
        enabledMigration = Universe.enabledMigration,
        raidAIToggle = Universe.raidAIToggle,
        siegeAIToggle = Universe.siegeAIToggle,
        peacefulAIToggle = Universe.peacefulAIToggle,
    })

    local commands = {}
    table.insert(commands, Command.changeBaseState(base.id, newState, "temperament_cycle"))

    Executor.run(commands)

    local remainingUnits = base.lostEnemyUnits % 20
    if remainingUnits ~= 0 then
        BaseUtils.modifyBaseUnitPoints(base, -(remainingUnits*UNIT_DEATH_POINT_COST), remainingUnits.." Units Lost")
    end

    base.destroyPlayerBuildings = 0
    base.lostEnemyUnits = 0
    base.lostEnemyBuilding = 0
    base.rocketLaunched = 0
    base.builtEnemyBuilding = 0
    base.ionCannonBlasts = 0
    base.artilleryBlasts = 0

    base.stateAITick = randomTickEvent(Universe.random, tick, BASE_AI_MIN_STATE_DURATION, BASE_AI_MAX_STATE_DURATION)

    if Universe.printAIStateChanges then
        game.print(base.id .. ": AI is now: " .. STATE_ENGLISH[base.stateAI] .. ", Next state change is in "
                   .. string.format("%.2f", (base.stateAITick - tick) / (60*60)) .. " minutes @ " ..
                   getTimeStringFromTick(base.stateAITick) .. " playtime")
    end

end

function BaseUtils.processBaseAIs(tick)
    local baseId = Universe.processBaseAIIterator
    local base
    if not baseId then
        baseId, base = next(Universe.bases, nil)
    else
        base = Universe.bases[baseId]
    end
    if not baseId then
        Universe.processBaseAIIterator = nil
        return
    end
    -- A base can disappear between two calls (surface release, base recycle):
    -- the stored iterator key then resolves to nil. Restart the walk instead of
    -- indexing nil.
    if not base then
        Universe.processBaseAIIterator = nil
        return
    end
    if true then
        Universe.processBaseAIIterator = next(Universe.bases, baseId)
        if (tick - base.tick) <= BASE_PROCESS_INTERVAL then
            return
        end
        temperamentPlanner(base, Universe.evolutionLevel)
        processState(base, tick)
        processBase(base, tick)
    end
end

function BaseUtils.init(universe)
    Universe = universe
end

BaseUtilsG = BaseUtils
return BaseUtils
