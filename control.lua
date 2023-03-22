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

-- imports

local ChunkPropertyUtils = require("libs/ChunkPropertyUtils")
local UnitUtils = require("libs/UnitUtils")
local BaseUtils = require("libs/BaseUtils")
local MathUtils = require("libs/MathUtils")
local Processor = require("libs/Processor")
local Constants = require("libs/Constants")
local MapUtils = require("libs/MapUtils")
local Squad = require("libs/Squad")
local tests = require("tests")
local ChunkUtils = require("libs/ChunkUtils")
local Upgrade = require("libs/Upgrade")
local Utils = require("libs/Utils")

-- Constants

local FACTION_SET = Constants.FACTION_SET

local ENEMY_ALIGNMENT_LOOKUP = Constants.ENEMY_ALIGNMENT_LOOKUP
local VANILLA_ENTITY_TYPE_LOOKUP = Constants.VANILLA_ENTITY_TYPE_LOOKUP
local ENTITY_SKIP_COUNT_LOOKUP = Constants.ENTITY_SKIP_COUNT_LOOKUP
local BUILDING_HIVE_TYPE_LOOKUP = Constants.BUILDING_HIVE_TYPE_LOOKUP

local TICKS_A_MINUTE = Constants.TICKS_A_MINUTE
local COMMAND_TIMEOUT = Constants.COMMAND_TIMEOUT

local RECOVER_NEST_COST = Constants.RECOVER_NEST_COST
local RECOVER_WORM_COST = Constants.RECOVER_WORM_COST

local RETREAT_GRAB_RADIUS = Constants.RETREAT_GRAB_RADIUS

local RETREAT_SPAWNER_GRAB_RADIUS = Constants.RETREAT_SPAWNER_GRAB_RADIUS
local BASE_PHEROMONE = Constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = Constants.PLAYER_PHEROMONE

local UNIT_DEATH_POINT_COST = Constants.UNIT_DEATH_POINT_COST

local PENDING_UPGRADE_CREATION_THESHOLD = Constants.PENDING_UPGRADE_CREATION_THESHOLD

local MAX_HIVE_TTL = Constants.MAX_HIVE_TTL
local MIN_HIVE_TTL = Constants.MIN_HIVE_TTL
local DEV_HIVE_TTL = Constants.DEV_HIVE_TTL

local SETTLE_CLOUD_WARMUP = Constants.SETTLE_CLOUD_WARMUP

-- imported functions

local isMember = Utils.isMember
local split = Utils.split

local planning = BaseUtils.planning

local addExcludeSurface = MapUtils.addExcludedSurface
local removeExcludeSurface = MapUtils.removeExcludedSurface

local setPointAreaInQuery = Utils.setPointAreaInQuery

local nextMap = MapUtils.nextMap

local processClouds = Processor.processClouds

local distortPosition = MathUtils.distortPosition
local linearInterpolation = MathUtils.linearInterpolation
local gaussianRandomRangeRG = MathUtils.gaussianRandomRangeRG
local prepMap = MapUtils.prepMap
local activateMap = MapUtils.activateMap

local findBaseInitialAlignment = BaseUtils.findBaseInitialAlignment

local processBaseAIs = BaseUtils.processBaseAIs

local registerEnemyBaseStructure = ChunkUtils.registerEnemyBaseStructure

local queueGeneratedChunk = MapUtils.queueGeneratedChunk
local isRampantSetting = Utils.isRampantSetting

local processPendingUpgrades = Processor.processPendingUpgrades
local canMigrate = BaseUtils.canMigrate

local squadDispatch = Squad.squadDispatch

local cleanUpMapTables = Processor.cleanUpMapTables

local positionToChunkXY = MapUtils.positionToChunkXY

local processVengence = Processor.processVengence
local processAttackWaves = Processor.processAttackWaves

local disperseVictoryScent = MapUtils.disperseVictoryScent

local getChunkByPosition = MapUtils.getChunkByPosition

local removeChunkFromMap = MapUtils.removeChunkFromMap
local getChunkByXY = MapUtils.getChunkByXY

local entityForPassScan = ChunkUtils.entityForPassScan

local processPendingChunks = Processor.processPendingChunks
local processScanChunks = Processor.processScanChunks

local processMap = Processor.processMap
local processPlayers = Processor.processPlayers
local scanEnemyMap = Processor.scanEnemyMap
local scanPlayerMap = Processor.scanPlayerMap
local scanResourceMap = Processor.scanResourceMap

local processNests = Processor.processNests
local processHives = Processor.processHives

local rallyUnits = Squad.rallyUnits

local recycleBases = BaseUtils.recycleBases

local deathScent = MapUtils.deathScent
local victoryScent = MapUtils.victoryScent

local createSquad = Squad.createSquad

local createBase = BaseUtils.createBase
local findNearbyBaseByPosition = ChunkPropertyUtils.findNearbyBaseByPosition
local findNearbyBase = ChunkPropertyUtils.findNearbyBase

local removeDrainPylons = ChunkPropertyUtils.removeDrainPylons
local getDrainPylonPair = ChunkPropertyUtils.getDrainPylonPair

local createDrainPylon = UnitUtils.createDrainPylon

local isDrained = ChunkPropertyUtils.isDrained
local setDrainedTick = ChunkPropertyUtils.setDrainedTick
local getCombinedDeathGeneratorRating = ChunkPropertyUtils.getCombinedDeathGeneratorRating

local retreatUnits = Squad.retreatUnits

local accountPlayerEntity = ChunkUtils.accountPlayerEntity
local unregisterEnemyBaseStructure = ChunkUtils.unregisterEnemyBaseStructure
local makeImmortalEntity = ChunkUtils.makeImmortalEntity

local registerResource = ChunkUtils.registerResource
local unregisterResource = ChunkUtils.unregisterResource

local cleanSquads = Squad.cleanSquads

local queueUpgrade = BaseUtils.queueUpgrade

local modifyBaseUnitPoints = BaseUtils.modifyBaseUnitPoints

local tRemove = table.remove

local sFind = string.find

local mRandom = math.random

-- local references to global

local Universe -- manages the chunks that make up the game Universe

-- hook functions

local function onIonCannonFired(event)
    --[[
        event.force, event.surface, event.player_index, event.position, event.radius
    --]]
    local map = Universe.maps[event.surface.index]
    if not map then
        return
    end

    local chunk = getChunkByPosition(map, event.position)
    if (chunk ~= -1) then
        local base = findNearbyBase(chunk)
        if base then
            base.ionCannonBlasts = base.ionCannonBlasts + 1
            rallyUnits(chunk, event.tick)
            modifyBaseUnitPoints(base, 4000, "Ion Cannon")
        end
    end
end

local function onAbandonedRuins(event)
    local entity = event.entity
    if entity.valid and (entity.force.name ~= "enemy") then
        local map = Universe.maps[entity.surface.index]
        if not map then
            return
        end
        accountPlayerEntity(entity, map, true)
    end
end

local function hookEvents()
    if settings.startup["ion-cannon-radius"] ~= nil then
        script.on_event(remote.call("orbital_ion_cannon", "on_ion_cannon_fired"),
                        onIonCannonFired)
    end
    if settings.global["AbandonedRuins-set"] ~= nil then
        script.on_event(remote.call("AbandonedRuins", "get_on_entity_force_changed_event"),
                        onAbandonedRuins)
    end
end

local function initializeLibraries()
    BaseUtils.init(Universe)
    Squad.init(Universe)
    BaseUtils.init(Universe)
    Processor.init(Universe)
    MapUtils.init(Universe)
    ChunkPropertyUtils.init(Universe, MapUtils)
    ChunkUtils.init(Universe)
end

local function onLoad()
    Universe = global.universe

    hookEvents()
end

local function onChunkGenerated(event)
    -- queue generated chunk for delayed processing, queuing is required because
    -- some mods (RSO) mess with chunk as they are generated, which messes up the
    -- scoring.
    queueGeneratedChunk(event)
end

local function onModSettingsChange(event)

    if not isRampantSetting(event.setting) then
        return
    end

    Universe.safeEntities["curved-rail"] = settings.global["rampant--safeBuildings-curvedRail"].value
    Universe.safeEntities["straight-rail"] = settings.global["rampant--safeBuildings-straightRail"].value
    Universe.safeEntities["rail-signal"] = settings.global["rampant--safeBuildings-railSignals"].value
    Universe.safeEntities["rail-chain-signal"] = settings.global["rampant--safeBuildings-railChainSignals"].value
    Universe.safeEntities["train-stop"] = settings.global["rampant--safeBuildings-trainStops"].value
    Universe.safeEntities["lamp"] = settings.global["rampant--safeBuildings-lamps"].value

    Universe.safeEntities["big-electric-pole"] = settings.global["rampant--safeBuildings-bigElectricPole"].value

    Universe.safeEntities["big-electric-pole"] = Universe.safeEntities["big-electric-pole"]
    Universe.safeEntities["big-electric-pole-2"] = Universe.safeEntities["big-electric-pole"]
    Universe.safeEntities["big-electric-pole-3"] = Universe.safeEntities["big-electric-pole"]
    Universe.safeEntities["big-electric-pole-4"] = Universe.safeEntities["big-electric-pole"]
    Universe.safeEntities["lighted-big-electric-pole-4"] = Universe.safeEntities["big-electric-pole"]
    Universe.safeEntities["lighted-big-electric-pole-3"] = Universe.safeEntities["big-electric-pole"]
    Universe.safeEntities["lighted-big-electric-pole-2"] = Universe.safeEntities["big-electric-pole"]
    Universe.safeEntities["lighted-big-electric-pole"] = Universe.safeEntities["big-electric-pole"]

    Universe["temperamentRateModifier"] = settings.global["rampant--temperamentRateModifier"].value
    Universe["baseDistanceModifier"] = settings.global["rampant--baseDistanceModifier"].value
    Universe["printBaseAdaptation"] = settings.global["rampant--printBaseAdaptation"].value
    Universe["adaptationModifier"] = settings.global["rampant--adaptationModifier"].value

    Universe["raidAIToggle"] = settings.global["rampant--raidAIToggle"].value
    Universe["siegeAIToggle"] = settings.global["rampant--siegeAIToggle"].value
    Universe["attackPlayerThreshold"] = settings.global["rampant--attackPlayerThreshold"].value
    Universe["attackUsePlayer"] = settings.global["rampant--attackWaveGenerationUsePlayerProximity"].value

    Universe["attackWaveMaxSize"] = settings.global["rampant--attackWaveMaxSize"].value
    Universe["aiNocturnalMode"] = settings.global["rampant--permanentNocturnal"].value
    Universe["aiPointsScaler"] = settings.global["rampant--aiPointsScaler"].value

    Universe["aiPointsPrintGainsToChat"] = settings.global["rampant--aiPointsPrintGainsToChat"].value
    Universe["aiPointsPrintSpendingToChat"] = settings.global["rampant--aiPointsPrintSpendingToChat"].value
    Universe["printBaseUpgrades"] = settings.global["rampant--printBaseUpgrades"].value
    Universe["PRINT_BASE_SETTLING"] = settings.global["rampant--printBaseSettling"].value

    Universe["enabledMigration"] = Universe.expansion and settings.global["rampant--enableMigration"].value
    Universe["peacefulAIToggle"] = settings.global["rampant--peacefulAIToggle"].value
    Universe["printAIStateChanges"] = settings.global["rampant--printAIStateChanges"].value
    Universe["debugTemperament"] = settings.global["rampant--debugTemperament"].value
    Universe["legacyChunkScanning"] = settings.global["rampant--legacyChunkScanning"].value

    Universe["enabledPurpleSettlerCloud"] = settings.global["rampant--enabledPurpleSettlerCloud"].value

    Universe["AI_MAX_SQUAD_COUNT"] = settings.global["rampant--maxNumberOfSquads"].value
    Universe["AI_MAX_BUILDER_COUNT"] = settings.global["rampant--maxNumberOfBuilders"].value
    Universe["AI_MAX_VANILLA_SQUAD_COUNT"] = Universe["AI_MAX_SQUAD_COUNT"] * 0.65
    Universe["AI_MAX_VANILLA_BUILDER_COUNT"] = Universe["AI_MAX_BUILDER_COUNT"] * 0.65
    Universe["MAX_BASE_MUTATIONS"] = settings.global["rampant--max-base-mutations"].value

    Universe["MAX_BASE_ALIGNMENT_HISTORY"] = settings.global["rampant--maxBaseAlignmentHistory"].value

    Universe["initialPeaceTime"] = settings.global["rampant--initialPeaceTime"].value * TICKS_A_MINUTE
    Universe["printAwakenMessage"] = settings.global["rampant--printAwakenMessage"].value

    Universe["minimumAdaptationEvolution"] = settings.global["rampant--minimumAdaptationEvolution"].value

    return true
end

local function onConfigChanged()
    Upgrade.init(Universe)
    game.print("Rampant - Version 3.2.0")
    Upgrade.addUniverseProperties()
    initializeLibraries()
    Upgrade.attempt()

    onModSettingsChange({setting="rampant--"})

    Universe["NEW_ENEMIES"] = settings.startup["rampant--newEnemies"].value

    -- not a completed implementation needs if checks to use all forces
    -- both in the data stage, commands, and if then logic
    local enemyForces = {
        ["enemy"]=true
    }

    local npcForces = {
        ["neutral"]=true
    }

    if game.active_mods["AbandonedRuins"] then
        npcForces["AbandonedRuins:enemy"] = true
    end

    Upgrade.setCommandForces(npcForces, enemyForces)

    for _,surface in pairs(game.surfaces) do
        if not Universe.maps[surface.index] then
            prepMap(surface)
        end
    end

    if not Universe.ranIncompatibleMessage and Universe.newEnemies and
        (game.active_mods["bobenemies"] or game.active_mods["Natural_Evolution_Enemies"]) then
        Universe.ranIncompatibleMessage = true
        game.print({"description.rampant-bobs-nee-newEnemies"})
    end
end

local function onEnemyBaseBuild(entity, tick)
    local map = Universe.maps[entity.surface.index]
    if not map then
        return
    end
    local chunk = getChunkByPosition(map, entity.position)
    if (chunk ~= -1) then
        local base = findNearbyBase(chunk)
        if not base then
            base = createBase(map,
                              chunk,
                              tick)
        end

        if Universe.NEW_ENEMIES then
            if VANILLA_ENTITY_TYPE_LOOKUP[entity.name] then
                queueUpgrade(entity,
                             base,
                             nil,
                             true)
            else
                registerEnemyBaseStructure(entity, base, tick, true)
            end
        else
            registerEnemyBaseStructure(entity, base, tick)
        end
    else
        local x,y = positionToChunkXY(entity.position)
        onChunkGenerated({
                surface = entity.surface,
                tick = tick,
                area = {
                    left_top = {
                        x = x,
                        y = y
                    }
                }
        })
    end
end

local function onBuild(event)
    local entity = event.created_entity or event.entity
    if entity.valid then
        local entityForceName = entity.force.name
        if entityForceName == "enemy" then
            if BUILDING_HIVE_TYPE_LOOKUP[entity.name] then
                onEnemyBaseBuild(entity, event.tick)
            end
        else
            local map = Universe.maps[entity.surface.index]
            if not map then
                return
            end
            if entity.type == "resource" then
                registerResource(entity, map)
            else
                accountPlayerEntity(entity, map, true)
                if Universe.safeEntities[entity.type] or Universe.safeEntities[entity.name] then
                    entity.destructible = false
                end
            end
        end
    end
end

local function onMine(event)
    local entity = event.entity
    if entity.valid then
        local map = Universe.maps[entity.surface.index]
        if not map then
            return
        end
        accountPlayerEntity(entity, map, false)
    end
end

local function onDeath(event)
    local entity = event.entity
    if not entity.valid then
        return
    end
    local surface = entity.surface
    local map = Universe.maps[surface.index]
    if not map then
        return
    end
    local entityForceName = entity.force.name
    if (entityForceName == "neutral") then
        if (entity.name == "cliff") then
            entityForPassScan(map, entity)
        end
        return
    end
    local cause = event.cause
    local causedByEnemyForce = event.force and (event.force.name == "enemy")
    local tick = event.tick
    local entityType = entity.type
    local entityPosition = entity.position
    local damageTypeName = event.damage_type and event.damage_type.name
    local chunk = getChunkByPosition(map, entityPosition)
    local base

    if entityForceName == "enemy" then
        if entityType ~= "unit" then
            if getDrainPylonPair(map, entity.unit_number) then
                removeDrainPylons(map, entity.unit_number)
            else
                unregisterEnemyBaseStructure(map, entity, damageTypeName)
            end
        else
            local group = entity.unit_group
            if group then
                local squad = Universe.groupNumberToSquad[group.group_number]
                if damageTypeName and squad then
                    base = squad.base
                end
            end
        end

        if (chunk ~= -1) then
            if not base then
                base = findNearbyBase(chunk)
            end

            local artilleryBlast = (cause and ((cause.type == "artillery-wagon") or (cause.type == "artillery-turret")))
            if (entityType == "unit") and not ENTITY_SKIP_COUNT_LOOKUP[entity.name] then
                deathScent(chunk)
                if base then
                    base.lostEnemyUnits = base.lostEnemyUnits + 1
                    if damageTypeName then
                        base.damagedBy[damageTypeName] = (base.damagedBy[damageTypeName] or 0) + 0.01
                        base.deathEvents = base.deathEvents + 1
                    end
                    if base.lostEnemyUnits % 20 == 0 then
                        modifyBaseUnitPoints(base, -(20*UNIT_DEATH_POINT_COST), "20 Units Lost")
                    end
                    if (Universe.random() < Universe.rallyThreshold) and not surface.peaceful_mode then
                        rallyUnits(chunk, tick)
                    end
                    if artilleryBlast then
                        base.artilleryBlasts = base.artilleryBlasts + 1
                    end
                end

                if (getCombinedDeathGeneratorRating(chunk) < Universe.retreatThreshold) and cause and cause.valid then
                    retreatUnits(chunk,
                                 cause,
                                 map,
                                 tick,
                                 (artilleryBlast and RETREAT_SPAWNER_GRAB_RADIUS) or RETREAT_GRAB_RADIUS)
                end
            elseif BUILDING_HIVE_TYPE_LOOKUP[entity.name] or
                (entityType == "unit-spawner") or
                (entityType == "turret")
            then
                deathScent(chunk, true)
                if base then
                    if (entityType == "unit-spawner") then
                        modifyBaseUnitPoints(base, RECOVER_NEST_COST, "Nest Lost")
                    elseif (entityType == "turret") then
                        modifyBaseUnitPoints(base, RECOVER_WORM_COST, "Worm Lost")
                    end
                    rallyUnits(chunk, tick)
                    if artilleryBlast then
                        base.artilleryBlasts = base.artilleryBlasts + 1
                    end
                end

                if cause and cause.valid then
                    retreatUnits(chunk,
                                 cause,
                                 map,
                                 tick,
                                 RETREAT_SPAWNER_GRAB_RADIUS)
                end
            end
        end
    else
        local creditNatives = false
        if causedByEnemyForce then
            creditNatives = true
            local drained = false
            if chunk ~= -1 then
                victoryScent(chunk, entityType)
                drained = (entityType == "electric-turret") and isDrained(chunk, tick)
                if cause and cause.type == "unit" then
                    local group = cause.unit_group
                    if group and group.valid then
                        local squad = Universe.groupNumberToSquad[group.group_number]
                        if squad then
                            base = squad.base
                        end
                    end
                end
                if not base then
                    base = findNearbyBase(chunk)
                end
            end

            if cause or drained then
                createDrainPylon(map, cause, entity, entityType)
            end
        end
        if creditNatives and (Universe.safeEntities[entityType] or Universe.safeEntities[entity.name])
        then
            makeImmortalEntity(surface, entity)
        else
            accountPlayerEntity(entity, map, false, base)
        end
    end
end

local function processSurfaceTile(map, position, chunks, tick)
    local chunk = getChunkByPosition(map, position)

    if (chunk ~= -1) then
        Universe.chunkToPassScan[chunk.id] = chunk
    else
        local x,y = positionToChunkXY(position)
        local addMe = true
        if chunks then
            for ci=1,#chunks do
                local c = chunks[ci]
                if (c.x == x) and (c.y == y) then
                    addMe = false
                    break
                end
            end
        end
        if addMe then
            local chunkXY = {x=x,y=y}
            if chunks then
                chunks[#chunks+1] = chunkXY
            end
            onChunkGenerated({area = { left_top = chunkXY },
                              tick = tick,
                              surface = map.surface})
        end
    end
end

local function onSurfaceTileChange(event)
    local surfaceIndex = event.surface_index or (event.robot and event.robot.surface and event.robot.surface.index)
    local map = Universe.maps[surfaceIndex]
    if not map then
        return
    end
    local chunks = {}
    local tiles = event.tiles
    if event.tile then
        if ((event.tile.name == "landfill") or sFind(event.tile.name, "water")) then
            for i=1,#tiles do
                processSurfaceTile(map, tiles[i].position, chunks, event.tick)
            end
        end
    else
        for i=1,#tiles do
            local tile = tiles[i]
            if (tile.name == "landfill") or sFind(tile.name, "water") then
                processSurfaceTile(map, tiles[i].position, chunks, event.tick)
            end
        end
    end
end

local function onResourceDepleted(event)
    local entity = event.entity
    if entity.valid then
        local map = Universe.maps[entity.surface.index]
        if not map then
            return
        end
        unregisterResource(entity, map)
    end
end

local function onRobotCliff(event)
    local entity = event.robot
    if entity.valid then
        local map = Universe.maps[entity.surface.index]
        if not map then
            return
        end
        if (event.item.name == "cliff-explosives") then
            entityForPassScan(map, event.cliff)
        end
    end
end

local function onUsedCapsule(event)
    local surface = game.players[event.player_index].surface
    local map = Universe.maps[surface.index]
    if not map then
        return
    end
    if (event.item.name == "cliff-explosives") then
        setPointAreaInQuery(Universe.oucCliffQuery, event.position, 0.75)
        local cliffs = surface.find_entities_filtered(Universe.oucCliffQuery)
        for i=1,#cliffs do
            entityForPassScan(map, cliffs[i])
        end
    end
end

local function onRocketLaunch(event)
    local entity = event.rocket_silo or event.rocket
    if entity.valid then
        local map = Universe.maps[entity.surface.index]
        if not map then
            return
        end
        local chunk = getChunkByPosition(map, entity.position)
        if (chunk ~= -1) then
            local base = findNearbyBase(chunk)
            if base then
                base.rocketLaunched = base.rocketLaunched + 1
                modifyBaseUnitPoints(base, 5000, "Rocket Launch")
            end
        end
    end
end

local function onTriggerEntityCreated(event)
    if (event.effect_id == "rampant-drain-trigger") then
        local entity = event.target_entity
        if (entity and entity.valid) then
            local map = Universe.maps[event.surface_index]
            if not map then
                return
            end
            local chunk = getChunkByPosition(map, entity.position)
            if (chunk ~= -1) then
                setDrainedTick(chunk, event.tick)
            end
        elseif (event.target_position) then
            local map = Universe.maps[event.surface_index]
            if not map then
                return
            end
            local chunk = getChunkByPosition(map, event.target_position)
            if (chunk ~= -1) then
                setDrainedTick(chunk, event.tick)
            end
        end
    elseif (event.effect_id == "deathLandfillParticle--rampant") then
        local map = Universe.maps[event.surface_index]
        if not map then
            return
        end
        processSurfaceTile(
            map,
            event.target_position,
            nil,
            event.tick
        )
    end
end

local function onInit()
    global.universe = {}

    Universe = global.universe

    hookEvents()
    onConfigChanged()
end

local function onUnitGroupCreated(event)
    local group = event.group
    if (group.force.name ~= "enemy") then
        return
    end
    local surface = group.surface
    local squad
    if group.is_script_driven then
        return
    end
    local map = Universe.maps[surface.index]
    if not map then
        return
    end
    activateMap(map)
    local position = group.position
    local chunk = getChunkByPosition(map, position)
    local base
    if (chunk == -1) then
        base = findNearbyBaseByPosition(map, position.x, position.y)
    else
        base = findNearbyBase(chunk)
    end
    if not base then
        group.destroy()
        return
    end
    if not Universe.aiNocturnalMode then
        local settler = canMigrate(base) and
            (Universe.builderCount < Universe.AI_MAX_VANILLA_BUILDER_COUNT) and
            (Universe.random() < 0.25)

        if not settler and ( (Universe.squadCount >= Universe.AI_MAX_VANILLA_SQUAD_COUNT) or (chunk == -1) ) then
            group.destroy()
            return
        end

        if not settler and (chunk[BASE_PHEROMONE] < 0.0001) and (chunk[PLAYER_PHEROMONE] < 0.0001) then
            group.destroy()
            return
        end

        squad = createSquad(nil, map, group, settler, base)
        Universe.groupNumberToSquad[group.group_number] = squad

        if settler then
            Universe.builderCount = Universe.builderCount + 1
        else
            Universe.squadCount = Universe.squadCount + 1
        end
    else
        if not (surface.darkness > 0.65) then
            group.destroy()
            return
        end

        local settler = canMigrate(base) and
            (Universe.builderCount < Universe.AI_MAX_VANILLA_BUILDER_COUNT) and
            (Universe.random() < 0.25)

        if not settler and ( (Universe.squadCount >= Universe.AI_MAX_VANILLA_SQUAD_COUNT) or (chunk == -1) ) then
            group.destroy()
            return
        end

        if not settler and chunk[BASE_PHEROMONE] < 0.0001 and chunk[PLAYER_PHEROMONE] < 0.0001 then
            group.destroy()
            return
        end

        squad = createSquad(nil, map, group, settler, base)
        Universe.groupNumberToSquad[group.group_number] = squad

        if settler then
            Universe.builderCount = Universe.builderCount + 1
        else
            Universe.squadCount = Universe.squadCount + 1
        end
    end
end

local function onGroupFinishedGathering(event)
    local group = event.group
    if not group.valid or (group.force.name ~= "enemy") then
        return
    end
    local map = Universe.maps[group.surface.index]
    if not map then
        return
    end
    activateMap(map)
    local squad = Universe.groupNumberToSquad[group.group_number]
    if squad then
        if squad.settler then
            if (Universe.builderCount <= Universe.AI_MAX_BUILDER_COUNT) then
                squadDispatch(map, squad, event.tick)
            else
                group.destroy()
            end
        else
            local chunk = getChunkByPosition(map, squad.group.position)
            if (chunk ~= -1) and (chunk[BASE_PHEROMONE] < 0.0001) and (chunk[PLAYER_PHEROMONE] < 0.0001) then
                group.destroy()
                return
            end

            if (Universe.squadCount <= Universe.AI_MAX_SQUAD_COUNT) then
                squadDispatch(map, squad, event.tick)
            else
                group.destroy()
            end
        end
    else
        local position = group.position
        local chunk = getChunkByPosition(map, position)
        local base
        if chunk == -1 then
            base = findNearbyBaseByPosition(map, position.x, position.y)
        else
            base = findNearbyBase(chunk)
        end
        if not base then
            group.destroy()
            return
        end
        local settler = canMigrate(base) and
            (Universe.builderCount < Universe.AI_MAX_VANILLA_BUILDER_COUNT) and
            (Universe.random() < 0.25)

        if not settler and (Universe.squadCount >= Universe.AI_MAX_VANILLA_SQUAD_COUNT) then
            group.destroy()
            return
        end

        if not settler and (chunk[BASE_PHEROMONE] < 0.0001) and (chunk[PLAYER_PHEROMONE] < 0.0001) then
            group.destroy()
            return
        end

        squad = createSquad(nil, map, group, settler, base)
        Universe.groupNumberToSquad[group.group_number] = squad
        if settler then
            Universe.builderCount = Universe.builderCount + 1
        else
            Universe.squadCount = Universe.squadCount + 1
        end
        squadDispatch(map, squad, event.tick)
    end
end

local function onForceCreated(event)
    if Universe.playerForces then
        Universe.playerForces[#Universe.playerForces+1] = event.force.name
    end
end

local function onForceMerged(event)
    for i=#Universe.playerForces,1,-1 do
        if (Universe.playerForces[i] == event.source_name) then
            tRemove(Universe.playerForces, i)
            break
        end
    end
end

local function onSurfaceCreated(event)
    prepMap(game.surfaces[event.surface_index])
end

local function onSurfaceDeleted(event)
    local surfaceIndex = event.surface_index
    if (Universe.mapIterator == surfaceIndex) then
        Universe.mapIterator, Universe.currentMap = next(
            Universe.activeMaps,
            Universe.mapIterator
        )
    end
    if (Universe.processMapAIIterator == surfaceIndex) then
        Universe.processMapAIIterator = nil
    end
    Universe.maps[surfaceIndex] = nil
end

local function onSurfaceCleared(event)
    onSurfaceDeleted(event)
    onSurfaceCreated(event)
end

local function onChunkDeleted(event)
    local surfaceIndex = event.surface_index
    local map = Universe.maps[surfaceIndex]
    if map then
        local positions = event.positions
        for i=1,#positions do
            local position = positions[i]
            local x = position.x * 32
            local y = position.y * 32
            local chunk = getChunkByXY(map, x, y)
            if chunk ~= -1 then
                removeChunkFromMap(map, chunk)
            end
        end
    end
end

local function onBuilderArrived(event)
    local builder = event.group
    local usingUnit = false
    if not (builder and builder.valid) then
        builder = event.unit
        if not (builder and builder.valid and builder.force.name == "enemy") then
            return
        end
        usingUnit = true
    elseif (builder.force.name ~= "enemy") then
        return
    end

    local map = Universe.maps[builder.surface.index]
    if not map then
        return
    end
    activateMap(map)
    if not usingUnit then
        local squad = Universe.groupNumberToSquad[builder.group_number]
        squad.commandTick = event.tick + COMMAND_TIMEOUT * 10
    end
    if Universe.PRINT_BASE_SETTLING then
        game.print(map.surface.name.." Settled: [gps=" .. builder.position.x .. "," .. builder.position.y .."]")
    end
    if Universe.enabledPurpleSettlerCloud then
        Universe.eventId = Universe.eventId + 1
        Universe.settlePurpleCloud[Universe.eventId] = {
            map = map,
            group = builder,
            tick = event.tick + SETTLE_CLOUD_WARMUP
        }
    end
end

-- hooks

script.on_event(defines.events.on_tick,
                function ()
                    local gameRef = game
                    local tick = gameRef.tick
                    local range = (Universe.legacyChunkScanning and 4) or 3
                    local pick = tick % range
                    -- local profiler = game.create_profiler()

                    local map = nextMap()

                    if (pick == 0) then
                        processPendingChunks(tick)
                        if map then
                            recycleBases()
                        end
                        cleanUpMapTables(tick)
                        planning(gameRef.forces.enemy.evolution_factor)
                        processHives(tick)
                    elseif (pick == 1) then
                        processPlayers(gameRef.connected_players, tick)
                    elseif (pick == 2) then
                        processPendingUpgrades(tick)
                        processVengence()
                        disperseVictoryScent()
                        processAttackWaves()
                        processClouds(tick)
                        processScanChunks()
                    elseif (pick == 3) then
                        if map then
                            scanPlayerMap(map)
                            scanResourceMap(map)
                            scanEnemyMap(map, tick)
                        end
                    end

                    if map then
                        processMap(map, tick)
                    end

                    -- game.print({"", "--dispatch3 ", profiler, " , ", pick," , ",math.random()})

                    processBaseAIs(tick)
                    processNests(tick)
                    cleanSquads(tick)

                    -- game.print({"", "--dispatch4 ", profiler, " , ", pick," , ",math.random()})
end)

script.on_event(defines.events.on_chunk_deleted, onChunkDeleted)
script.on_event(defines.events.on_surface_deleted, onSurfaceDeleted)
script.on_event(defines.events.on_surface_cleared, onSurfaceCleared)
script.on_event(defines.events.on_surface_created, onSurfaceCreated)

script.on_init(onInit)
script.on_load(onLoad)
script.on_event(defines.events.on_runtime_mod_setting_changed, onModSettingsChange)
script.on_configuration_changed(onConfigChanged)

script.on_event(defines.events.on_resource_depleted, onResourceDepleted)
script.on_event({defines.events.on_player_built_tile,
                 defines.events.on_robot_built_tile,
                 defines.events.script_raised_set_tiles}, onSurfaceTileChange)

script.on_event(defines.events.on_player_used_capsule, onUsedCapsule)

script.on_event(defines.events.on_script_trigger_effect, onTriggerEntityCreated)

script.on_event(defines.events.on_pre_robot_exploded_cliff, onRobotCliff)

script.on_event({defines.events.on_player_mined_entity,
                 defines.events.on_robot_mined_entity}, onMine)

script.on_event(
    {
        defines.events.on_built_entity,
        defines.events.on_robot_built_entity,
        defines.events.script_raised_built,
        defines.events.script_raised_revive,
        defines.events.on_biter_base_built
    },
    onBuild)

script.on_event(defines.events.on_rocket_launched, onRocketLaunch)
script.on_event({defines.events.on_entity_died,
                 defines.events.script_raised_destroy}, onDeath)
script.on_event(defines.events.on_chunk_generated, onChunkGenerated)
script.on_event(defines.events.on_unit_group_created, onUnitGroupCreated)
script.on_event(defines.events.on_force_created, onForceCreated)
script.on_event(defines.events.on_forces_merged, onForceMerged)
script.on_event(defines.events.on_unit_group_finished_gathering, onGroupFinishedGathering)

script.on_event(defines.events.on_build_base_arrived, onBuilderArrived)

-- testing

remote.add_interface("rampantTests",
                     {
                         dumpEnvironment = tests.dumpEnvironment,
                         -- fillableDirtTest = tests.fillableDirtTest,
                         -- tunnelTest = tests.tunnelTest,
                         reveal = tests.reveal,
                         -- showMovementGrid = tests.showMovementGrid,
                         -- showBaseGrid = tests.showBaseGrid,
                         -- colorResourcePoints = tests.colorResourcePoints,
                         exportAiState = tests.exportAiState(nil),
                         scanEnemy = tests.scanEnemy,
                         chunkCount = tests.chunkCount
                     }
)

local function removeNewEnemies()
    game.print({"description.rampant--removeNewEnemies"})
    Universe.NEW_ENEMIES = false
    for _,map in pairs(Universe.maps) do
        local surface = map.surface
        if surface.valid then
            local entities = surface.find_entities_filtered({
                    force=Universe.enemyForces,
                    type={
                        "turret",
                        "unit-spawner"
                    }
            })
            for entityIndex = 1,#entities do
                local entity = entities[entityIndex]
                if entity.valid and not VANILLA_ENTITY_TYPE_LOOKUP[entity.name] then
                    local position = entity.position
                    local newEntityName
                    local entityType = entity.type
                    unregisterEnemyBaseStructure(map, entity, nil, true)
                    entity.destroy()
                    if entityType == "unit-spawner" then
                        if mRandom() < 0.5 then
                            newEntityName = "biter-spawner"
                        else
                            newEntityName = "spitter-spawner"
                        end
                    elseif entityType == "turret" then
                        if Universe.evolutionLevel >= 0.9 then
                            newEntityName = "behemoth-worm-turret"
                        elseif Universe.evolutionLevel >= 0.5 then
                            newEntityName = "big-worm-turret"
                        elseif Universe.evolutionLevel >= 0.3 then
                            newEntityName = "medium-worm-turret"
                        else
                            newEntityName = "small-worm-turret"
                        end
                    end
                    local newEntity = surface.create_entity({
                            name=newEntityName,
                            position=position
                    })
                    local chunk = getChunkByPosition(map, position)
                    if chunk ~= -1 then
                        local tick = game.tick
                        local base = findNearbyBase(chunk)
                        if not base then
                            base = createBase(map,
                                              chunk,
                                              tick)
                        end
                        registerEnemyBaseStructure(newEntity, base, tick)
                    end
                end
            end
        end
    end
end

local function removeFaction(cmd)
    local factionNames = cmd.parameter
    if not factionNames then
        game.print({"description.rampant--removeFactionNames"})
        return
    end
    factionNames = split(factionNames)
    for _,factionName in pairs(factionNames) do
        local found = false
        if (factionName ~= "neutral") then
            for _,faction in pairs(FACTION_SET) do
                if (faction.type == factionName) then
                    found = true
                    break
                end
            end
        end
        if not found then
            game.print({"description.rampant--removeFactionNames"})
            return
        end
    end
    local localizedFactionNames = ""
    for _,name in pairs(factionNames) do
        if localizedFactionNames == "" then
            localizedFactionNames = name
        else
            localizedFactionNames = localizedFactionNames .. "," .. name
        end
    end
    game.print({"description.rampant--removeFaction", localizedFactionNames})

    for _,base in pairs(Universe.bases) do
        if isMember(base.alignment[1], factionNames) then
            base.alignment = findBaseInitialAlignment(Universe.evolutionLevel, factionNames)
        elseif isMember(base.alignment[2], factionNames) then
            base.alignment = findBaseInitialAlignment(Universe.evolutionLevel, factionNames)
        end
    end

    for _,map in pairs(Universe.maps) do
        local surface = map.surface
        if surface.valid then
            local entities = surface.find_entities_filtered({
                    force=Universe.enemyForces,
                    type={
                        "turret",
                        "unit-spawner"
                    }
            })
            for entityIndex = 1,#entities do
                local entity = entities[entityIndex]
                if entity.valid and isMember(ENEMY_ALIGNMENT_LOOKUP[entity.name], factionNames) then
                    local position = entity.position
                    local newEntityName
                    local entityType = entity.type
                    unregisterEnemyBaseStructure(map, entity, nil, true)
                    entity.destroy()
                    if entityType == "unit-spawner" then
                        if mRandom() < 0.5 then
                            newEntityName = "biter-spawner"
                        else
                            newEntityName = "spitter-spawner"
                        end
                    elseif entityType == "turret" then
                        if Universe.evolutionLevel >= 0.9 then
                            newEntityName = "behemoth-worm-turret"
                        elseif Universe.evolutionLevel >= 0.5 then
                            newEntityName = "big-worm-turret"
                        elseif Universe.evolutionLevel >= 0.3 then
                            newEntityName = "medium-worm-turret"
                        else
                            newEntityName = "small-worm-turret"
                        end
                    end
                    local newEntity = surface.create_entity({
                            name=newEntityName,
                            position=position
                    })
                    local chunk = getChunkByPosition(map, position)
                    if chunk ~= -1 then
                        local tick = game.tick
                        local base = findNearbyBase(chunk)
                        if not base then
                            base = createBase(map,
                                              chunk,
                                              tick)
                        end
                        registerEnemyBaseStructure(newEntity, base, tick)
                    end
                end
            end
        end
    end
end

remote.add_interface("Rampant", {
                         addExcludeSurface = addExcludeSurface,
                         removeExcludeSurface = removeExcludeSurface
})

commands.add_command('rampantRemoveNewEnemies', "", removeNewEnemies)
commands.add_command('rampantRemoveFaction', "", removeFaction)

local function rampantSetAIState(event)
    if event.parameter then
        local target
        local baseId
        local i = 0

        for m in string.gmatch(event.parameter, "%d+") do
            if i == 0 then
                i = i + 1
                target = tonumber(m)
            else
                baseId = tonumber(m)
            end
        end

        if not target or not Constants.STATE_ENGLISH[target] then
            game.print(target .. " is not a valid state. /rampantSetAIState <stateId> <baseId>")
        else
            if not baseId then
                game.print("Invalid baseId. /rampantSetAIState <stateId> <baseId>")
                return
            end
            local base = Universe.bases[baseId]
            if not base then
                game.print(baseId .. " is not a valid base. /rampantSetAIState <stateId> <baseId>")
                return
            end
            local previousState = base.stateAI
            base.stateAI = target
            local surface = base.map.surface
            if not surface.valid then
                game.print("Base is invalid because surface is invalid")
                return
            end
            game.print("id:" .. baseId .. " on surface:" .. surface.name
                       .. " was in " .. Constants.STATE_ENGLISH[previousState]
                       .. " is now in " .. Constants.STATE_ENGLISH[base.stateAI])
        end
    else
        game.print("Missing parameters: /rampantSetAIState <stateId> <baseId>")
    end
end

commands.add_command('rampantSetAIState', "", rampantSetAIState)
