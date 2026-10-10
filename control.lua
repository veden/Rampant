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
local Processor = require("libs/Processor")
local Constants = require("libs/Constants")
local MapUtils = require("libs/MapUtils")
local Squad = require("libs/Squad")
local tests = require("tests")
local ChunkUtils = require("libs/ChunkUtils")
local Upgrade = require("libs/Upgrade")
local Utils = require("libs/Utils")
local Observability = require("libs/Observability")
local Executor = require("libs/runtime/Executor")
local Metrics = require("libs/Metrics")
local DeathLogic = require("libs/death_logic")
local SquadCreation = require("libs/squad_creation")
local BuildingLogic = require("libs/building")
local SourceVersionedFieldRuntime = require("libs/source_versioned_field_runtime")

-- Constants

local FACTION_SET = Constants.FACTION_SET

local ENEMY_ALIGNMENT_LOOKUP = Constants.ENEMY_ALIGNMENT_LOOKUP
local VANILLA_ENTITY_TYPE_LOOKUP = Constants.VANILLA_ENTITY_TYPE_LOOKUP
local ENTITY_SKIP_COUNT_LOOKUP = Constants.ENTITY_SKIP_COUNT_LOOKUP
local BUILDING_HIVE_TYPE_LOOKUP = Constants.BUILDING_HIVE_TYPE_LOOKUP

local COMMAND_TIMEOUT = Constants.COMMAND_TIMEOUT

local SETTLE_CLOUD_WARMUP = Constants.SETTLE_CLOUD_WARMUP

-- imported functions

local isMember = Utils.isMember
local split = Utils.split

local planning = BaseUtils.planning

local addExcludeSurface = MapUtils.addExcludedSurface
local removeExcludeSurface = MapUtils.removeExcludedSurface

local setPointAreaInQuery = Utils.setPointAreaInQuery

local processClouds = Processor.processClouds

local prepMap = MapUtils.prepMap

local findBaseInitialAlignment = BaseUtils.findBaseInitialAlignment

local processBaseAIs = BaseUtils.processBaseAIs

local registerEnemyBaseStructure = ChunkUtils.registerEnemyBaseStructure

local queueGeneratedChunk = MapUtils.queueGeneratedChunk
local markChunkDirty = MapUtils.markChunkDirty
local isRampantSetting = Utils.isRampantSetting

local processPendingUpgrades = Processor.processPendingUpgrades

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

local processPlayers = Processor.processPlayers
local processDirtyChunks = Processor.processDirtyChunks

local processNests = Processor.processNests
local processHives = Processor.processHives
local cleanHivesData = Processor.cleanHivesData

local rallyUnits = Squad.rallyUnits

local recycleBases = BaseUtils.recycleBases

local deathScent = MapUtils.deathScent
local victoryScent = MapUtils.victoryScent

local createBase = BaseUtils.createBase
local findNearbyBaseByPosition = ChunkPropertyUtils.findNearbyBaseByPosition
local findNearbyBase = ChunkPropertyUtils.findNearbyBase

local removeDrainPylons = ChunkPropertyUtils.removeDrainPylons
local getDrainPylonPair = ChunkPropertyUtils.getDrainPylonPair

local createDrainPylon = UnitUtils.createDrainPylon

local initialScan = ChunkUtils.initialScan
local isDrained = ChunkPropertyUtils.isDrained
local setDrainedTick = ChunkPropertyUtils.setDrainedTick
local getCombinedDeathGeneratorRating = ChunkPropertyUtils.getCombinedDeathGeneratorRating

local accountPlayerEntity = ChunkUtils.accountPlayerEntity
local unregisterEnemyBaseStructure = ChunkUtils.unregisterEnemyBaseStructure
local makeImmortalEntity = ChunkUtils.makeImmortalEntity

local registerResource = ChunkUtils.registerResource
local unregisterResource = ChunkUtils.unregisterResource

local cleanSquads = Squad.cleanSquads

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

local function initializeLibraries(addProperties)
    Upgrade.init(Universe)
    if addProperties then
        Upgrade.addUniverseProperties()
    end
    BaseUtils.init(Universe)
    Squad.init(Universe)
    BaseUtils.init(Universe)
    Processor.init(Universe)
    MapUtils.init(Universe)
    ChunkPropertyUtils.init(Universe, MapUtils)
    ChunkUtils.init(Universe)
    Observability.init(Universe)
    Executor.init(Universe)
    Metrics.init(Universe)
    SourceVersionedFieldRuntime.init(Universe, addProperties)
    ChunkUtils.setSourceVersionedFieldRuntime(SourceVersionedFieldRuntime)
end

local function rescanAllChunks(map, tick)
    if not map or not map.surface or not map.surface.valid then return end
    for x, xChunks in pairs(map) do
        if type(xChunks) == "table" and xChunks[1] then
            for y, chunk in pairs(xChunks) do
                if type(chunk) == "table" and chunk.id then
                    -- purgeUnits=false: the rescan re-scans LIVING
                    -- territory (evolution crossed 0.3); the initial
                    -- scan's unit purge would destroy every unit near
                    -- every nest.
                    initialScan(chunk, map, tick, false)
                    markChunkDirty(chunk.id, 4, tick)
                end
            end
        end
    end
end

-- Diagnostic: the transient working set is the set of chunks with a live
-- player/death/kamikaze generator, and it is what the dirty queue's steady-state
-- depth measures. Sampled (not per tick) because it scans the chunk index.
local function transientWorkingSet()
    local death, player, kami, any = 0, 0, 0, 0
    local KAMIKAZE = Constants.KAMIKAZE_PHEROMONE
    for _, chunk in pairs(Universe.chunkById) do
        local d = (chunk.deathGenerator or 0) ~= 0
        local p = (chunk.playerGenerator or 0) ~= 0
        local k = (chunk[KAMIKAZE] or 0) ~= 0
        if d then death = death + 1 end
        if p then player = player + 1 end
        if k then kami = kami + 1 end
        if d or p or k then any = any + 1 end
    end
    return death, player, kami, any
end

local function onLoad()
    Universe = storage.universe

    -- NOTE: no storage writes allowed here (CRC check fails the load).
    -- DEBUG_ENTITY_EVENTS persists from the save.

    initializeLibraries()

    -- NOTE: `game` is nil during on_load (Factorio restriction); no game
    -- object access is allowed here. Structure registration persists in
    -- storage (event-driven), so no re-scan is needed on load.

    hookEvents()
end

local function onChunkGenerated(event)
    Metrics.inc("event.onChunkGenerated")
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

    Universe["squadCompressionThreshold"] = settings.global["rampant--squadCompressionThreshold"].value

    Universe["AI_MAX_SQUAD_COUNT"] = settings.global["rampant--maxNumberOfSquads"].value
    Universe["AI_MAX_BUILDER_COUNT"] = settings.global["rampant--maxNumberOfBuilders"].value
    Universe["AI_MAX_VANILLA_SQUAD_COUNT"] = Universe["AI_MAX_SQUAD_COUNT"] * 0.65
    Universe["AI_MAX_VANILLA_BUILDER_COUNT"] = Universe["AI_MAX_BUILDER_COUNT"] * 0.65
    Universe["MAX_BASE_MUTATIONS"] = settings.global["rampant--max-base-mutations"].value

    Universe["MAX_BASE_ALIGNMENT_HISTORY"] = settings.global["rampant--maxBaseAlignmentHistory"].value

    Universe["initialPeaceTime"] =
        (settings.global["rampant--initialPeaceTime"].value * Constants.TICKS_A_SECOND) + (Universe.modAddedTick or game.tick)

    Universe["printAwakenMessage"] = settings.global["rampant--printAwakenMessage"].value

    Universe["minimumAdaptationEvolution"] = settings.global["rampant--minimumAdaptationEvolution"].value

    return true
end

local function onConfigChanged()
    -- onConfigChanged handles migrations/Upgrades after mod updates
    -- Full initialization is handled in onInit()
    
    if not Universe or not Universe.awake then
        log("Rampant: onConfigChanged - Universe not initialized, skipping")
        return
    end

    -- initializeLibraries must run before Upgrade.attempt() so Upgrade's
    -- local Universe reference is set via Upgrade.init()
    initializeLibraries(true)

    -- Upgrade runs after for migrations
    Upgrade.attempt()

    game.print("Rampant - Version 3.3.3")
    
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

    if script.active_mods["AbandonedRuins"] then
        npcForces["AbandonedRuins:enemy"] = true
    end

    Upgrade.setCommandForces(npcForces, enemyForces)

    for _,surface in pairs(game.surfaces) do
        if not Universe.maps[surface.index] then
            prepMap(surface)
        end
    end

    if not Universe.ranIncompatibleMessage and Universe.newEnemies and
        (script.active_mods["bobenemies"] or script.active_mods["Natural_Evolution_Enemies"]) then
        Universe.ranIncompatibleMessage = true
        game.print({"description.rampant-bobs-nee-newEnemies"})
    end
end

local function dbgEntity(msg)
    if Universe and Universe.DEBUG_ENTITY_EVENTS then
        game.print("[RAMPANT] " .. msg)
    end
end

local function onEnemyBaseBuild(entity, tick)
    dbgEntity("onEnemyBaseBuild: " .. entity.name .. " @(" .. entity.position.x .. "," .. entity.position.y .. ") tick=" .. tick)
    local map = Universe.maps[entity.surface.index]
    if not map then
        dbgEntity("  -> NO MAP for surface=" .. entity.surface.index)
        return
    end
    local chunk = getChunkByPosition(map, entity.position)
    local base
    local isNewBase = false
    if chunk ~= -1 then
        base = findNearbyBase(chunk)
        if not base then
            dbgEntity("  -> creating new base at chunk(" .. chunk.x .. "," .. chunk.y .. ")")
            isNewBase = true
        end
    end

    -- Gather data for pure evaluation
    -- NOTE: chunk is the -1 sentinel when outside the grid; -1 is truthy in
    -- Lua, so guard with `chunk ~= -1` (not `chunk and ...`)
    local buildingData = {
        entityName = entity.name,
        entityType = entity.type,
        hasMap = true,
        hasChunk = chunk ~= -1,
        chunkId = (chunk ~= -1) and chunk.id or nil,
        chunkX = (chunk ~= -1) and chunk.x or (function() local x,y = positionToChunkXY(entity.position) return x end)(),
        chunkY = (chunk ~= -1) and chunk.y or (function() local x,y = positionToChunkXY(entity.position) return y end)(),
        baseId = base and base.id or nil,
        isNewBase = isNewBase,
        newEnemies = Universe.NEW_ENEMIES,
        isVanillaEntity = VANILLA_ENTITY_TYPE_LOOKUP[entity.name],
    }

    local commands = BuildingLogic.evaluateEnemy(buildingData)

    -- Attach Factorio references for executor to use
    for _, cmd in ipairs(commands) do
        cmd.entityRef = entity
        cmd.mapRef = map
        cmd.tick = tick
    end

    -- Defer: this runs inside an entity-creation event (see Executor.queue).
    Executor.queue(commands)
end

local function onBuild(event)
    Metrics.inc("event.onBuild")
    local entity = event.created_entity or event.entity
    if entity.valid then
        dbgEntity("onBuild: event=" .. tostring(event.event) .. " entity=" .. entity.name .. " force=" .. entity.force.name .. " type=" .. entity.type .. " created_entity=" .. tostring(event.created_entity ~= nil) .. " has_entity=" .. tostring(event.entity ~= nil))
        local entityForceName = entity.force.name
        if entityForceName == "enemy" then
            onEnemyBaseBuild(entity, event.tick)
        else
            local map = Universe.maps[entity.surface.index]
            if not map then
                return
            end
            if entity.type == "resource" then
                registerResource(entity, map)
                local c = getChunkByPosition(map, entity.position)
                if c ~= -1 then markChunkDirty(c.id, 1, event.tick) end
            else
                accountPlayerEntity(entity, map, true)
                local c = getChunkByPosition(map, entity.position)
                if c ~= -1 then markChunkDirty(c.id, 1, event.tick) end
                if Universe.safeEntities[entity.type] or Universe.safeEntities[entity.name] then
                    entity.destructible = false
                end
            end
        end
    end
end

local function onMine(event)
    Metrics.inc("event.onMine")
    local entity = event.entity
    if entity.valid then
        local map = Universe.maps[entity.surface.index]
        if not map then
            return
        end
        accountPlayerEntity(entity, map, false)
        local c = getChunkByPosition(map, entity.position)
        if c ~= -1 then markChunkDirty(c.id, 1, event.tick) end
    end
end

local function onDeath(event)
    Metrics.inc("event.onDeath")
    local entity = event.entity
    if not entity.valid then
        return
    end
    dbgEntity("onDeath: " .. entity.name .. " type=" .. entity.type .. " force=" .. (entity.force and entity.force.name or "nil") .. " @(" .. entity.position.x .. "," .. entity.position.y .. ") cause=" .. (event.cause and event.cause.name or "nil"))
    local surface = entity.surface
    local map = Universe.maps[surface.index]
    if not map then
        Metrics.inc("death.mapMissing")
        return
    end
    local entityForceName = entity.force.name
    Metrics.inc("death.force." .. entityForceName)
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
    local squad

    if entityForceName == "enemy" then
        local isDrainPylon = false
        if entityType ~= "unit" then
            if getDrainPylonPair(map, entity.unit_number) then
                removeDrainPylons(map, entity.unit_number)
                isDrainPylon = true
            end
        end

        if not isDrainPylon then
            -- Gather data from Factorio entities for pure evaluation
            local group
            if entityType == "unit" then
                group = entity.commandable and entity.commandable.parent_group
                if group and group.valid then
                    squad = Universe.groupNumberToSquad[group.unique_id]
                    if squad and damageTypeName then
                        base = squad.base
                    end
                end
            end

            if not base then
                base = findNearbyBase(chunk)
            end

            local artilleryBlast = (cause and ((cause.type == "artillery-wagon") or (cause.type == "artillery-turret")))

            if chunk ~= -1 then
                Metrics.sample("death.rating",
                    getCombinedDeathGeneratorRating(chunk))
                Metrics.sample("death.threshold",
                    Universe.retreatThreshold or 0)
                Metrics.inc("death.causeValid",
                    (cause and cause.valid and (cause.get_health_ratio() ~= nil)) and 1 or 0)
            end
            local deathData = {
                entityType = entityType,
                entityName = entity.name,
                chunkId = chunk ~= -1 and chunk.id or -1,
                baseId = base and base.id or nil,
                base = base,
                squadId = squad and squad.id or nil,
                damageType = damageTypeName,
                isArtilleryCause = artilleryBlast,
                deathGeneratorRating = chunk ~= -1 and getCombinedDeathGeneratorRating(chunk) or 0,
                retreatThreshold = Universe.retreatThreshold or 0,
                causeValid = cause and cause.valid and (cause.get_health_ratio() ~= nil),
                rallyThreshold = Universe.rallyThreshold or 0,
                randomRoll = Universe.random(),
                lostEnemyUnits = base and base.lostEnemyUnits or 0,
                isDrainPylon = false,
            }

            if chunk == -1 then
                Metrics.inc("death.chunkMissing")
            end
            local commands = DeathLogic.evaluateEnemy(deathData)
            Metrics.inc("death.commandsEmitted", #commands)
            if #commands > 0 then
                for _, cmd in ipairs(commands) do
                    cmd.entityRef = entity
                    cmd.mapRef = map
                    cmd.tick = tick
                    -- Retreat commands need the aggressor ENTITY for the flee
                    -- command (defines.command.flee.from is a LuaEntity).
                    -- The pure death logic only carries a reason label.
                    if cmd.type == "RETREAT_UNITS" then
                        cmd.causeRef = cause
                    end
                end
                -- Defer: runs inside on_destroyed (see Executor.queue).
                Executor.queue(commands)
            end

            -- Scent application stays here (impure: modifies chunk pheromones directly)
            if chunk ~= -1 then
                if entityType == "unit" and not ENTITY_SKIP_COUNT_LOOKUP[entity.name] then
                    deathScent(chunk)
                elseif BUILDING_HIVE_TYPE_LOOKUP[entity.name] or
                       entityType == "unit-spawner" or entityType == "turret" then
                    deathScent(chunk, true)
                end
            end
        end
    else
        local creditNatives = false
        if causedByEnemyForce then
            creditNatives = true
            Metrics.inc("death.enemyCaused")
            local drained = false
            if chunk ~= -1 then
                victoryScent(chunk, entityType)
                Metrics.inc("death.victoryScentApplied")
                drained = (entityType == "electric-turret") and isDrained(chunk, tick)
                if cause and cause.valid and cause.type == "unit" then
                    local group = cause.commandable and cause.commandable.parent_group
                    if group and group.valid then
                        squad = Universe.groupNumberToSquad[group.unique_id]
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


-- Per-subsystem benchmark timing. On sample ticks only, time one call and log
-- it; otherwise call straight through. `SUBSYSTEM_TELEMETRY` is the
-- benchmark-only exact mode: it logs every timed call so the parser can bucket
-- min/avg/max per subsystem per range. It is high-overhead and off by default.
local function timed(tick, rangeSample, name, fn, a, b)
    if not rangeSample and not Constants.SUBSYSTEM_TELEMETRY then
        return fn(a, b)
    end
    local profiler = helpers.create_profiler()
    fn(a, b)
    log({"", "[SUB] tick=", tick, " name=", name, " ", profiler})
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
    Metrics.inc("event.onResourceDepleted")
    local entity = event.entity
    if entity.valid then
        local map = Universe.maps[entity.surface.index]
        if not map then
            return
        end
        unregisterResource(entity, map)
        local c = getChunkByPosition(map, entity.position)
        if c ~= -1 then markChunkDirty(c.id, 4, event.tick) end
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
    Metrics.inc("event.onRocketLaunch")
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
    if not Universe or not Universe.awake then
        return
    end
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
    -- Ensure storage exists
    if not storage then
        storage = {}
    end

    -- Always initialize universe in storage
    storage.universe = storage.universe or {}
    Universe = storage.universe

    -- Initialize minimal Universe structure
    Universe.modAddedTick = game.tick
    Universe.safeEntities = {}

    -- Set awake flag before calling handlers that check it
    Universe.awake = true

    -- Entity event debug logging (always on)
    Universe.DEBUG_ENTITY_EVENTS = false

    log("Rampant: Full initialization starting")

    hookEvents()
    onConfigChanged()

    -- Signal that initialization is complete and AI is ready
    storage._rampantReady = true
    print("READY_FOR_COMMANDS")
    print("Rampant Initialized: " .. tostring(Universe and Universe.awake))
end

local function onUnitGroupCreated(event)
    Metrics.inc("event.onUnitGroupCreated")
    local group = event.group
    Metrics.inc("group.created." .. group.force.name ..
        (group.is_script_driven and ".script" or ".vanilla"))
    dbgEntity("onUnitGroupCreated: force=" .. group.force.name .. " is_script=" .. tostring(group.is_script_driven) .. " @(" .. group.position.x .. "," .. group.position.y .. ")")

    local surface = group.surface
    local map = Universe.maps[surface.index]

    local position = group.position
    local chunk = getChunkByPosition(map, position)
    local base
    if chunk == -1 then
        base = findNearbyBaseByPosition(map, position.x, position.y)
    else
        base = findNearbyBase(chunk)
    end

    -- Gather data for pure evaluation
    local creationData = {
        forceName = group.force.name,
        isScriptDriven = group.is_script_driven,
        universeAwake = Universe and Universe.awake,
        hasMap = map ~= nil,
        baseId = base and base.id or nil,
        migrateParams = base and {
            stateAI = base.stateAI,
            expansion = Universe.expansion,
            peacefulMode = surface.peaceful_mode,
            aiNocturnalMode = Universe.aiNocturnalMode,
            darkness = surface.darkness,
        } or {},
        builderCount = Universe.builderCount or 0,
        maxBuilderCount = Universe.AI_MAX_VANILLA_BUILDER_COUNT,
        squadCount = Universe.squadCount or 0,
        maxSquadCount = Universe.AI_MAX_VANILLA_SQUAD_COUNT,
        hasChunk = chunk ~= -1,
        basePheromone = (chunk ~= -1) and Squad.getBasePheromone(chunk) or 0,
        playerPheromone = (chunk ~= -1) and Squad.getPlayerPheromone(chunk) or 0,
        aiNocturnalMode = Universe.aiNocturnalMode,
        darkness = surface.darkness,
        randomRoll = Universe.random(),
    }

    local commands = SquadCreation.evaluate(creationData)
    if #commands == 0 then return end

    -- Attach Factorio reference for executor to use
    for _, cmd in ipairs(commands) do
        cmd.groupRef = group
    end

    Executor.run(commands)
end

local function onGroupFinishedGathering(event)
    Metrics.inc("event.onGroupFinishedGathering")
    local group = event.group
    if not group.valid or (group.force.name ~= "enemy") then
        return
    end
    if not Universe.awake then
        group.destroy()
        return
    end
    local map = Universe.maps[group.surface.index]
    if not map then
        return
    end
    local squad = Universe.groupNumberToSquad[group.unique_id]
    if squad then
        if squad.settler then
            if (Universe.builderCount <= Universe.AI_MAX_BUILDER_COUNT) then
                squadDispatch(squad, event.tick)
            else
                group.destroy()
            end
        else
            local chunk = getChunkByPosition(map, squad.group.position)
            if (chunk ~= -1) and (Squad.getBasePheromone(chunk) < 0.0001) and (Squad.getPlayerPheromone(chunk) < 0.0001) then
                group.destroy()
                return
            end

            if (Universe.squadCount <= Universe.AI_MAX_SQUAD_COUNT) then
                squadDispatch(squad, event.tick)
            else
                group.destroy()
            end
        end
    else
        -- Group was not adopted at creation (script-driven, non-enemy, or rejected).
        -- Adoption decisions live in SquadCreation.evaluate; leave unadopted groups alone.
        return
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
    -- Purge the chunkById reverse index for chunks of the deleted surface.
    -- IMPORTANT: the map table holds BOTH the chunk grid AND registries
    -- (map.bases, map.drainPylons). Base objects have .id too, and base ids
    -- collide with chunk ids (both counters start at 0), so guard on
    -- chunk.valid to only touch real chunks.
    local map = Universe.maps[surfaceIndex]
    if map and Universe.chunkById then
        for _, xChunks in pairs(map) do
            if type(xChunks) == "table" then
                for _, chunk in pairs(xChunks) do
                    if chunk.valid and chunk.id then
                        Universe.chunkById[chunk.id] = nil
                    end
                end
            end
        end
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
    dbgEntity("onBuilderArrived: has_group=" .. tostring(event.group ~= nil and event.group.valid) .. " has_unit=" .. tostring(event.unit ~= nil and event.unit.valid))
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
    -- builder is either a unit group (LuaCommandable) or a unit (LuaEntity);
    -- LuaCommandable has no `name` key and the strict metatable ERRORS on
    -- access (it does not return nil), so only read it for units
    local builderName = "group"
    if usingUnit then
        builderName = builder.name
    end
    dbgEntity("  -> builder=" .. builderName .. " @(" .. builder.position.x .. "," .. builder.position.y .. ") usingUnit=" .. tostring(usingUnit))

    local map = Universe.maps[builder.surface.index]
    if not map then
        return
    end
    if not usingUnit then
        local squad = Universe.groupNumberToSquad[builder.unique_id]
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
                    if not Universe or not Universe.awake or not Universe.random then
                        return
                    end
                    -- Poll for incoming UDP packets (triggers on_udp_packet_received)
                    -- Poll all available sockets: server + connected players
                    helpers.recv_udp()
                    if game.connected_players[1] then
                        helpers.recv_udp(1)
                    end

                    local gameRef = game
                    local tick = gameRef.tick

                    Metrics.sampleDirtyQueue(Universe.dirtyCount)

                    local pick = tick % 3  -- Spread work across ticks (0, 1, 2)
                    -- Per-range benchmark sample: every RANGE_TICKS ticks, log one
                    -- tick's script time (2.1: game.create_profiler moved to
                    -- helpers.create_profiler). Lets a completed non-verbose
                    -- --benchmark run yield per-range numbers, since
                    -- --benchmark-verbose crashes at teardown for long runs.
                    local RANGE_TICKS = 500
                    local rangeSample = Metrics.isEnabled() and (tick % RANGE_TICKS == 0)
                    local tPick = rangeSample and helpers.create_profiler() or nil

                    if (pick == 0) then
                        timed(tick, rangeSample, "pendingChunks", processPendingChunks, tick)
                        if next(Universe.maps) ~= nil then
                            timed(tick, rangeSample, "recycleBases", recycleBases)
                        end
                        timed(tick, rangeSample, "cleanUpMap", cleanUpMapTables, tick)
                        timed(tick, rangeSample, "planning", planning, gameRef.forces.enemy.get_evolution_factor(game.surfaces[1]))
                        -- One-time rescan when evolution rises: registers enemy structures that spawned after chunk creation
                        if not Universe.rescannedForEvolution and Universe.evolutionLevel > 0.3 then
                            for _, map in pairs(Universe.maps) do
                                rescanAllChunks(map, tick)
                            end
                            Universe.rescannedForEvolution = true
                        end
                        timed(tick, rangeSample, "hives", processHives, tick)
                        timed(tick, rangeSample, "cleanHives", cleanHivesData)
                    elseif (pick == 1) then
                        timed(tick, rangeSample, "players", processPlayers, gameRef.connected_players, tick)
                    elseif (pick == 2) then
                        timed(tick, rangeSample, "pendingUpgrades", processPendingUpgrades, tick)
                        timed(tick, rangeSample, "vengence", processVengence)
                        timed(tick, rangeSample, "victoryScent", disperseVictoryScent, Universe)
                        timed(tick, rangeSample, "attackWaves", processAttackWaves)
                        timed(tick, rangeSample, "clouds", processClouds, tick)
                        timed(tick, rangeSample, "scanChunks", processScanChunks)
                    end
                    if rangeSample then
                        log({"", "[RANGE] tick=", tick, " phase=pick pick=", pick, " ", tPick})
                    end

                    -- Event-driven pheromone processing (replaces wave-based processMap)
                    local tDirty = rangeSample and helpers.create_profiler() or nil
                    timed(tick, rangeSample, "dirty", processDirtyChunks, tick, Universe)
                    Executor.drain()
                    -- Persisted solved state: a no-op once created.
                    SourceVersionedFieldRuntime.ensureStorage()
                    -- Per-field cadence: each field drains on its own budget so the
                    -- resource field's slow cadence cannot starve base/enemy.
                    timed(tick, rangeSample, "sourceField.resource", SourceVersionedFieldRuntime.processScheduled, Constants.RESOURCE_PHEROMONE)
                    timed(tick, rangeSample, "sourceField.base", SourceVersionedFieldRuntime.processScheduled, Constants.BASE_PHEROMONE)
                    timed(tick, rangeSample, "sourceField.enemy", SourceVersionedFieldRuntime.processScheduled, Constants.ENEMY_PHEROMONE)
                    timed(tick, rangeSample, "sourceField.player", SourceVersionedFieldRuntime.processScheduled, Constants.PLAYER_PHEROMONE)
                    if rangeSample then
                        local m = Metrics.get()
                        local c = m.counters or {}
                        local pending = Universe.dirtyCount or 0
                        local wsDeath, wsPlayer, wsKami, wsAny = transientWorkingSet()
                        log({"", "[RANGE] tick=", tick, " phase=dirty ", tDirty,
                             " prop=" .. (c['dirty.propagationMarks'] or 0) ..
                             " chunks=" .. (c['dirty.chunksProcessed'] or 0) ..
                             " skip=" .. (c['dirty.epsilonSkips'] or 0) ..
                             " lookups=" .. (c['index.lookups'] or 0) ..
                             " pending=" .. pending ..
                             " ws=" .. wsAny ..
                             " wsDeath=" .. wsDeath ..
                             " wsPlayer=" .. wsPlayer ..
                             " wsKami=" .. wsKami})
                    end

                    -- game.print({"", "--dispatch3 ", profiler, " , ", pick," , ",math.random()})

                    local tAI = rangeSample and helpers.create_profiler() or nil
                    timed(tick, rangeSample, "baseAIs", processBaseAIs, tick)
                    timed(tick, rangeSample, "nests", processNests, tick)
                    timed(tick, rangeSample, "cleanSquads", cleanSquads, tick)
                    if rangeSample then
                        log({"", "[RANGE] tick=", tick, " phase=ai ", tAI})
                    end
end)

script.on_event(defines.events.on_chunk_deleted, onChunkDeleted)
script.on_event(defines.events.on_surface_deleted, onSurfaceDeleted)
script.on_event(defines.events.on_surface_cleared, onSurfaceCleared)
script.on_event(defines.events.on_surface_created, onSurfaceCreated)

-- UDP server for external tooling (requires --enable-lua-udp PORT)
script.on_event(defines.events.on_udp_packet_received, Observability.onUdpPacket)

script.on_init(onInit)
script.on_load(onLoad)
script.on_event(defines.events.on_runtime_mod_setting_changed, onModSettingsChange)
script.on_configuration_changed(function(event)
    -- Full initialization happens in onInit()
    -- This handler just calls onConfigChanged if Universe is ready
    onConfigChanged(event)
end)

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
    game.forces.enemy.kill_all_units()
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
    game.forces.enemy.kill_all_units()
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
    removeExcludeSurface = removeExcludeSurface,
    queryPheromones = function(pos, surface)
        return Observability.queryPheromones(pos, surface)
    end,
    queryChunk = function(pos, surface)
        return Observability.queryChunk(pos, surface)
    end,
    queryBaseAt = function(pos, surface)
        return Observability.queryBaseAt(pos, surface)
    end,
    getBasePoints = function(baseId)
        return Observability.getBasePoints(baseId)
    end,
    getMetrics = function()
        return Metrics.get()
    end,
    getMetricsSummary = function()
        return Metrics.getSummary()
    end,
    resetMetrics = function()
        Metrics.reset()
        return true
    end,
    setTelemetry = function(enabled)
        Metrics.setEnabled(enabled)
        return Metrics.isEnabled()
    end,
    -- Test hooks (gated on the dev telemetry flag): CONSTRUCT state for
    -- deterministic behavioral verification. These bypass the state machine
    -- and economy so scenarios test a specific behavior instead of waiting
    -- for random rolls.
    setBaseState = function(baseId, state)
        if not Universe.metricsEnabled then return false end
        local base = Universe.bases[baseId]
        if not base then return false end
        base.stateAI = state
        -- Freeze the state machine: push the next roll far out so the
        -- constructed state persists (otherwise the 10-25 min re-roll
        -- overrides the construction mid-test)
        base.stateAITick = game.tick + 24 * 60 * 60 * 60
        return true
    end,
    grantUnitPoints = function(baseId, amount)
        if not Universe.metricsEnabled then return false end
        local base = Universe.bases[baseId]
        if not base then return false end
        BaseUtils.modifyBaseUnitPoints(base, amount, "test-hook")
        return true
    end,
    setEvolution = function(value)
        if not Universe.metricsEnabled then return false end
        -- 2.x: evolution is per-surface (the evolution_factor field was
        -- removed; writing it errors "LuaForce doesn't contain key").
        game.forces.enemy.set_evolution_factor(value, game.surfaces[1])
        return true
    end,
    -- Nudge: set the state WITHOUT freezing the state machine (the next
    -- roll proceeds normally). For long-run sessions that need periodic
    -- expansion windows while staying otherwise natural.
    nudgeBaseState = function(baseId, state)
        if not Universe.metricsEnabled then return false end
        local base = Universe.bases[baseId]
        if not base then return false end
        base.stateAI = state
        -- Release any freeze (setBaseState pins stateAITick far out):
        -- schedule the next natural roll on the normal cadence
        -- (BASE_AI_MIN/MAX_STATE_DURATION, in game-minutes).
        local minDur = Constants.BASE_AI_MIN_STATE_DURATION
        local maxDur = Constants.BASE_AI_MAX_STATE_DURATION
        local duration = minDur + Universe.random() * (maxDur - minDur)
        base.stateAITick = game.tick + math.floor(duration * 60 * 60)
        return true
    end,
    getEnemyGroups = function(surface)
        return Observability.getEnemyGroups(surface)
    end,
    getBaseCount = function(surface)
        return Observability.getBaseCount(surface)
    end,
    getActiveStructures = function(surface)
        return Observability.getActiveStructures(surface)
    end,
    isInitialized = function()
        return Universe and Universe.awake
    end,
    getGameState = function()
        return Observability.getGameState()
    end,
    setGameSpeed = function(speed)
        return Observability.setGameSpeed(speed)
    end,
    spawnEntity = function(name, position, forceName)
        return Observability.spawnEntity(name, position, forceName)
    end,
    countEntitiesByForce = function(forceName)
        return Observability.countEntitiesByForce(forceName)
    end,
})

-- Remote interface for integration testing (no Lua globals)
local _rampantTestMode = false
local _rampantTestResults = {}

if not remote.interfaces["rampantTests"] then
    remote.add_interface("rampantTests", {
        enableTestMode = function()
            _rampantTestMode = true
            log("Rampant: Test mode enabled via remote interface")
        end,
        isTestMode = function()
            return _rampantTestMode == true
        end,
        getTestResults = function()
            return _rampantTestResults
        end,
        setTestResult = function(name, passed, errorMsg)
            if not _rampantTestResults then _rampantTestResults = {} end
            _rampantTestResults[name] = {passed = passed, error = errorMsg}
        end,
        clearTestResults = function()
            _rampantTestResults = {}
        end
    })
end

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
