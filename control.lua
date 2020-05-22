-- imports

local chunkPropertyUtils = require("libs/ChunkPropertyUtils")
local unitUtils = require("libs/UnitUtils")
local baseUtils = require("libs/BaseUtils")
local mapUtils = require("libs/MapUtils")
local movementUtils = require("libs/MovementUtils")
local mathUtils = require("libs/MathUtils")
local unitGroupUtils = require("libs/UnitGroupUtils")
local chunkProcessor = require("libs/ChunkProcessor")
local mapProcessor = require("libs/MapProcessor")
local constants = require("libs/Constants")
local pheromoneUtils = require("libs/PheromoneUtils")
local squadDefense = require("libs/SquadDefense")
local squadAttack = require("libs/SquadAttack")
local aiAttackWave = require("libs/AIAttackWave")
local aiPlanning = require("libs/AIPlanning")
local interop = require("libs/Interop")
local tests = require("tests")
local chunkUtils = require("libs/ChunkUtils")
local upgrade = require("Upgrade")
local config = require("config")
local aiPredicates = require("libs/AIPredicates")

-- constants

local DIVISOR_DEATH_TRAIL_TABLE = constants.DIVISOR_DEATH_TRAIL_TABLE
local TRIPLE_CHUNK_SIZE = constants.TRIPLE_CHUNK_SIZE
local INTERVAL_LOGIC = constants.INTERVAL_LOGIC
local INTERVAL_PLAYER_PROCESS = constants.INTERVAL_PLAYER_PROCESS
local INTERVAL_MAP_PROCESS = constants.INTERVAL_MAP_PROCESS
local INTERVAL_ENEMY_SCAN = constants.INTERVAL_ENEMY_SCAN
local INTERVAL_SCAN = constants.INTERVAL_SCAN
local INTERVAL_PLAYER_SCAN = constants.INTERVAL_PLAYER_SCAN
local INTERVAL_SQUAD = constants.INTERVAL_SQUAD
local INTERVAL_RESQUAD = constants.INTERVAL_RESQUAD
local INTERVAL_TEMPERAMENT = constants.INTERVAL_TEMPERAMENT
local INTERVAL_SPAWNER = constants.INTERVAL_SPAWNER
local INTERVAL_CHUNK_PROCESS = constants.INTERVAL_CHUNK_PROCESS
local INTERVAL_PASS_SCAN = constants.INTERVAL_PASS_SCAN
local INTERVAL_NEST = constants.INTERVAL_NEST
local INTERVAL_CLEANUP = constants.INTERVAL_CLEANUP
local INTERVAL_MAP_STATIC_PROCESS = constants.INTERVAL_MAP_STATIC_PROCESS

local HIVE_BUILDINGS = constants.HIVE_BUILDINGS

local AI_MAX_BUILDER_COUNT = constants.AI_MAX_BUILDER_COUNT

local RECOVER_NEST_COST = constants.RECOVER_NEST_COST
local RECOVER_WORM_COST = constants.RECOVER_WORM_COST

local DOUBLE_CHUNK_SIZE = constants.DOUBLE_CHUNK_SIZE

local AI_MAX_BITER_GROUP_SIZE = constants.AI_MAX_BITER_GROUP_SIZE

local PROCESS_QUEUE_SIZE = constants.PROCESS_QUEUE_SIZE

local WATER_TILE_NAMES = constants.WATER_TILE_NAMES

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE

local RETREAT_GRAB_RADIUS = constants.RETREAT_GRAB_RADIUS

local RETREAT_SPAWNER_GRAB_RADIUS = constants.RETREAT_SPAWNER_GRAB_RADIUS

local DEFINES_BEHAVIOR_RESULT_FAIL = defines.behavior_result.fail
local DEFINES_COMMAND_GROUP = defines.command.group
local DEFINES_COMMAND_WANDER = defines.command.wander
local DEFINES_COMMAND_BUILD_BASE = defines.command.build_base
local DEFINES_COMMAND_ATTACK_AREA = defines.command.attack_area
local DEFINES_COMMAND_GO_TO_LOCATION = defines.command.go_to_location
local DEFINES_COMMMAD_COMPOUND = defines.command.compound
local DEFINES_COMMAND_FLEE = defines.command.flee
local DEFINES_COMMAND_STOP = defines.command.stop

local DEFINES_COMPOUND_COMMAND_RETURN_LAST = defines.compound_command.return_last
local DEFINES_COMPOUND_COMMAND_AND = defines.compound_command.logical_and
local DEFINES_COMPOUND_COMMAND_OR = defines.compound_command.logical_or

local CHUNK_SIZE = constants.CHUNK_SIZE

local DEFINES_DISTRACTION_NONE = defines.distraction.none
local DEFINES_DISTRACTION_BY_ENEMY = defines.distraction.by_enemy
local DEFINES_DISTRACTION_BY_ANYTHING = defines.distraction.by_anything

local DEFINES_WIRE_TYPE_RED = defines.wire_type.red
local DEFINES_WIRE_TYPE_GREEN = defines.wire_type.green

local ENERGY_THIEF_CONVERSION_TABLE = constants.ENERGY_THIEF_CONVERSION_TABLE
local ENERGY_THIEF_LOOKUP = constants.ENERGY_THIEF_LOOKUP

-- local POISON_LOOKUP = constants.POISON_LOOKUP

-- imported functions

local canMigrate = aiPredicates.canMigrate

local convertTypeToDrainCrystal = unitUtils.convertTypeToDrainCrystal

local squadDispatch = squadAttack.squadDispatch

local cleanUpMapTables = mapProcessor.cleanUpMapTables

local positionToChunkXY = mapUtils.positionToChunkXY

local temperamentPlanner = aiPlanning.temperamentPlanner

local processVengence = mapProcessor.processVengence
local processSpawners = mapProcessor.processSpawners

local processStaticMap = mapProcessor.processStaticMap

local getPlayerBaseGenerator = chunkPropertyUtils.getPlayerBaseGenerator

local getChunkByPosition = mapUtils.getChunkByPosition

local entityForPassScan = chunkUtils.entityForPassScan

local processPendingChunks = chunkProcessor.processPendingChunks
local processScanChunks = chunkProcessor.processScanChunks

local processMap = mapProcessor.processMap
local processPlayers = mapProcessor.processPlayers
local scanEnemyMap = mapProcessor.scanEnemyMap
local scanPlayerMap = mapProcessor.scanPlayerMap
local scanResourceMap = mapProcessor.scanResourceMap

local processNests = mapProcessor.processNests

local planning = aiPlanning.planning

local rallyUnits = aiAttackWave.rallyUnits

local recycleBases = baseUtils.recycleBases

local deathScent = pheromoneUtils.deathScent
local victoryScent = pheromoneUtils.victoryScent

local regroupSquads = unitGroupUtils.regroupSquads

local createSquad = unitGroupUtils.createSquad

local createBase = baseUtils.createBase
local findNearbyBase = baseUtils.findNearbyBase

local processActiveNests = mapProcessor.processActiveNests

local removeSquadFromChunk = chunkPropertyUtils.removeSquadFromChunk
local addDeathGenerator = chunkPropertyUtils.addDeathGenerator
local getDeathGenerator = chunkPropertyUtils.getDeathGenerator

local retreatUnits = squadDefense.retreatUnits

local accountPlayerEntity = chunkUtils.accountPlayerEntity
local unregisterEnemyBaseStructure = chunkUtils.unregisterEnemyBaseStructure
local registerEnemyBaseStructure = chunkUtils.registerEnemyBaseStructure
local makeImmortalEntity = chunkUtils.makeImmortalEntity

local registerResource = chunkUtils.registerResource
local unregisterResource = chunkUtils.unregisterResource

local cleanSquads = unitGroupUtils.cleanSquads

local upgradeEntity = baseUtils.upgradeEntity
local rebuildNativeTables = baseUtils.rebuildNativeTables

local mMin = math.min
local mRandom = math.random

local tRemove = table.remove

local sFind = string.find

-- local references to global

local gameSurfaces -- used for manage which surfaces have been visited
local map -- manages the chunks that make up the game world
local natives -- manages the enemy units, structures, and ai

-- hook functions

local function onIonCannonFired(event)
    --[[
        event.force, event.surface, event.player_index, event.position, event.radius
    --]]
    local surface = event.surface
    if (surface.name == natives.activeSurface) then
        natives.ionCannonBlasts = natives.ionCannonBlasts + 1
        natives.points = natives.points + 3000
        local chunk = getChunkByPosition(map, event.position)
        if (chunk ~= -1) then
            rallyUnits(chunk, map, surface, event.tick)
        end
    end
end

local function hookEvents()
    if config.ionCannonPresent then
        script.on_event(remote.call("orbital_ion_cannon", "on_ion_cannon_fired"),
                        onIonCannonFired)
    end
end

local function onLoad()
    map = global.map
    natives = global.natives
    gameSurfaces = global.gameSurfaces

    hookEvents()
end

local function onChunkGenerated(event)
    -- queue generated chunk for delayed processing, queuing is required because some mods (RSO) mess with chunk as they
    -- are generated, which messes up the scoring.
    if (event.surface.name == natives.activeSurface) then
        map.pendingChunks[event] = true
    end
end

local function rebuildMap()
    game.get_surface(natives.activeSurface).print("Rampant - Reindexing chunks, please wait.")
    -- clear old map processing Queue
    -- prevents queue adding duplicate chunks
    -- chunks are by key, so should overwrite old

    global.map = {}
    map = global.map
    map.processQueue = {}
    map.processIndex = 1
    map.cleanupIndex = 1
    map.scanPlayerIndex = 1
    map.scanResourceIndex = 1
    map.scanEnemyIndex = 1
    map.processStaticIndex = 1
    map.outgoingScanWave = true
    map.outgoingStaticScanWave = true

    map.pendingChunks = {}
    map.chunkToBase = {}
    map.chunkToNests = {}
    map.chunkToTurrets = {}
    map.chunkToTraps = {}
    map.chunkToUtilities = {}
    map.chunkToHives = {}
    map.chunkToPlayerBase = {}
    map.chunkToResource = {}
    map.chunkToPlayerCount = {}
    map.playerToChunk = {}

    map.chunkToPassScan = {}
    map.chunkToSquad = {}

    map.chunkToRetreats = {}
    map.chunkToRallys = {}

    map.chunkToPassable = {}
    map.chunkToPathRating = {}
    map.chunkToDeathGenerator = {}
    map.chunkToDrained = {}
    map.chunkToActiveNest = {}
    map.chunkToActiveRaidNest = {}

    map.nextChunkSort = 0
    map.nextChunkSortTick = 0

    map.squadIterator = nil
    map.regroupIterator = nil
    map.deployVengenceIterator = nil
    map.recycleBaseIterator = nil
    map.processActiveSpawnerIterator = nil
    map.processActiveRaidSpawnerIterator = nil
    map.processMigrationIterator = nil
    map.processNestIterator = nil

    -- preallocating memory to be used in code, making it fast by reducing garbage generated.
    map.neighbors = {
            -1,
            -1,
            -1,
            -1,
            -1,
            -1,
            -1,
            -1
    }
    map.cardinalNeighbors = {
            -1,
            -1,
            -1,
            -1
    }
    map.position = {
        x=0,
        y=0
    }
    map.position2 = {
        x=0,
        y=0
    }
    map.position3 = {
        x=0,
        y=0
    }

    map.scentStaging = {}

    for x=1,PROCESS_QUEUE_SIZE+1 do
        map.scentStaging[x] = {0,0,0,0}
    end

    map.chunkScanCounts = {}

    map.chunkOverlapArray = {
            -1,
            -1,
            -1,
            -1
    }

    map.enemiesToSquad = {}
    map.enemiesToSquad.len = 0
    map.chunkRemovals = {}
    map.processActiveNest = {}
    map.tickActiveNest = {}

    map.emptySquadsOnChunk = {}
    map.position2Top = {0, 0}
    map.position2Bottom = {0, 0}
    --this is shared between two different queries
    map.area = {{0, 0}, {0, 0}}
    map.testArea = {{0, 0}, {0, 0}}
    map.area2 = {map.position2Top, map.position2Bottom}
    map.buildPositionTop = {0, 0}
    map.buildPositionBottom = {0, 0}
    map.buildArea = {map.buildPositionTop, map.buildPositionBottom}
    map.countResourcesQuery = { area=map.area, type="resource" }
    map.filteredEntitiesUnitQuery = { area=map.area, force="enemy",type="unit" }
    map.filteredEntitiesClearBuildingQuery = { area=map.buildArea, force="neutral",collision_mask="player-layer" }
    map.filteredEntitiesEnemyUnitQuery = { area=map.area, force="enemy", type="unit", limit=301 }
    map.hasPlayerStructuresQuery = { area=map.area, force={"enemy","neutral"}, invert=true, limit=1 }
    map.filteredEntitiesEnemyStructureQuery = { area=map.area, force="enemy", type={"turret","unit-spawner"} }
    map.filteredEntitiesPointQueryLimited = {
        position = map.position,
        radius = 10,
        limit = 1,
        force = "enemy",
        type = {
            "unit-spawner",
            "turret"
        }
    }
    map.createBuildCloudQuery = {
        name = "build-clear-cloud-rampant",
        position = map.position
    }

    map.activePlayerForces = {"player"}

    for _,force in pairs(game.forces) do
        local add = true

        if (force.name ~= "neutral") and (force.name ~= "enemy") then
            for i=1,#map.activePlayerForces do
                if (map.activePlayerForces[i] == force.name) then
                    add = false
                    break
                end
            end

            if add then
                map.activePlayerForces[#map.activePlayerForces+1] = force.name
            end
        end
    end

    map.filteredEntitiesPlayerQueryLowest = { area=map.area, force=map.activePlayerForces, type={"wall",
                                                                                                 "transport-belt"}}

    map.filteredEntitiesPlayerQueryLow = { area=map.area, force=map.activePlayerForces, type={"splitter",
                                                                                              "pump",
                                                                                              "offshore-pump",
                                                                                              "lamp",
                                                                                              "solar-panel",
                                                                                              "programmable-speaker",
                                                                                              "accumulator",
                                                                                              "assembling-machine",
                                                                                              "turret",
                                                                                              "ammo-turret"}}

    map.filteredEntitiesPlayerQueryHigh = { area=map.area, force=map.activePlayerForces, type={"furnace",
                                                                                               "lab",
                                                                                               "roboport",
                                                                                               "beacon",
                                                                                               "radar",
                                                                                               "electric-turret",
                                                                                               "boiler",
                                                                                               "generator",
                                                                                               "fluid-turret",
                                                                                               "mining-drill"}}

    map.filteredEntitiesPlayerQueryHighest = { area=map.area, force=map.activePlayerForces, type={"artillery-turret",
                                                                                                  "reactor",
                                                                                                  "rocket-silo"}}

    local sharedArea = {{0,0},{0,0}}
    map.filteredEntitiesCliffQuery = { area=sharedArea, type="cliff", limit = 1 }
    map.filteredTilesPathQuery = { area=sharedArea, collision_mask="water-tile", limit = 1 }
    map.cliffQuery = {
        area=map.area2,
        type="cliff"
    }
    map.canPlaceQuery = { name="", position={0,0} }
    map.filteredTilesQuery = { collision_mask="water-tile", area=map.area }

    map.upgradeEntityQuery = {
        name = "",
        position = nil
    }

    map.attackCommand = {
        type = DEFINES_COMMAND_ATTACK_AREA,
        destination = map.position,
        radius = CHUNK_SIZE * 1.5,
        distraction = DEFINES_DISTRACTION_BY_ANYTHING
    }

    map.moveCommand = {
        type = DEFINES_COMMAND_GO_TO_LOCATION,
        destination = map.position,
        radius = 2,
        pathfind_flags = { prefer_straight_paths = true, cache = true },
        distraction = DEFINES_DISTRACTION_BY_ENEMY
    }

    map.settleCommand = {
        type = DEFINES_COMMAND_BUILD_BASE,
        destination = map.position,
        distraction = DEFINES_DISTRACTION_BY_ENEMY,
        ignore_planner = true
    }

    map.wonderCommand = {
        type = DEFINES_COMMAND_WANDER,
        wander_in_group = false,
        radius = TRIPLE_CHUNK_SIZE,
        ticks_to_wait = 3600
    }

    map.wonder2Command = {
        type = DEFINES_COMMAND_WANDER,
        wander_in_group = false,
        radius = TRIPLE_CHUNK_SIZE,
        ticks_to_wait = 360
    }

    map.stopCommand = {
        type = DEFINES_COMMAND_STOP
    }

    map.compoundSettleCommand = {
        type = DEFINES_COMMMAD_COMPOUND,
        structure_type = DEFINES_COMPOUND_COMMAND_RETURN_LAST,
        commands = {
            map.wonder2Command,
            map.settleCommand
        }
    }

    map.retreatCommand = {
        type = DEFINES_COMMAND_GROUP,
        group = nil,
        distraction = DEFINES_DISTRACTION_NONE,
        use_group_distraction = false
    }

    map.mergeGroupCommand = {
        type = DEFINES_COMMAND_GROUP,
        group = nil,
        distraction = DEFINES_DISTRACTION_NONE,
        use_group_distraction = false
    }

    map.fleeCommand = {
        type = DEFINES_COMMAND_FLEE,
        from = nil,
        distraction = DEFINES_DISTRACTION_NONE
    }

    map.compoundRetreatCommand = {
        type = DEFINES_COMMMAD_COMPOUND,
        structure_type = DEFINES_COMPOUND_COMMAND_RETURN_LAST,
        commands = {
            map.fleeCommand,
            map.retreatCommand
        }
    }

    map.formGroupCommand = { type = DEFINES_COMMAND_GROUP,
                             group = nil,
                             distraction = DEFINES_DISTRACTION_ANYTHING,
                             use_group_distraction = false
    }

    map.formLocalGroupCommand = { type = DEFINES_COMMAND_GROUP,
                                  group = nil,
                                  distraction = DEFINES_DISTRACTION_ANYTHING,
                                  use_group_distraction = false
    }

    map.formCommand = { command = map.formGroupCommand,
                        unit_count = 0,
                        unit_search_distance = TRIPLE_CHUNK_SIZE }

    map.formLocalCommand = { command = map.formLocalGroupCommand,
                             unit_count = AI_MAX_BITER_GROUP_SIZE,
                             unit_search_distance = CHUNK_SIZE }
end


local function onModSettingsChange(event)

    if event and ((string.sub(event.setting, 1, 7) ~= "rampant") or
            (string.sub(event.setting, 1, 15) == "rampant-arsenal") or
            (string.sub(event.setting, 1, 17) == "rampant-resources") or
            (string.sub(event.setting, 1, 17) == "rampant-evolution"))
    then
        return false
    end

    upgrade.compareTable(natives, "safeBuildings", settings.global["rampant-safeBuildings"].value)

    upgrade.compareTable(natives.safeEntities, "curved-rail", settings.global["rampant-safeBuildings-curvedRail"].value)
    upgrade.compareTable(natives.safeEntities, "straight-rail", settings.global["rampant-safeBuildings-straightRail"].value)
    upgrade.compareTable(natives.safeEntities, "rail-signal", settings.global["rampant-safeBuildings-railSignals"].value)
    upgrade.compareTable(natives.safeEntities, "rail-chain-signal", settings.global["rampant-safeBuildings-railChainSignals"].value)
    upgrade.compareTable(natives.safeEntities, "train-stop", settings.global["rampant-safeBuildings-trainStops"].value)
    upgrade.compareTable(natives.safeEntities, "lamp", settings.global["rampant-safeBuildings-lamps"].value)

    local changed, newValue = upgrade.compareTable(natives.safeEntities,
                                                   "big-electric-pole",
                                                   settings.global["rampant-safeBuildings-bigElectricPole"].value)
    if changed then
        natives.safeEntities["big-electric-pole"] = newValue
        natives.safeEntities["big-electric-pole-2"] = newValue
        natives.safeEntities["big-electric-pole-3"] = newValue
        natives.safeEntities["big-electric-pole-4"] = newValue
        natives.safeEntities["lighted-big-electric-pole-4"] = newValue
        natives.safeEntities["lighted-big-electric-pole-3"] = newValue
        natives.safeEntities["lighted-big-electric-pole-2"] = newValue
        natives.safeEntities["lighted-big-electric-pole"] = newValue
    end

    upgrade.compareTable(natives, "deadZoneFrequency", settings.global["rampant-deadZoneFrequency"].value)
    upgrade.compareTable(natives, "raidAIToggle", settings.global["rampant-raidAIToggle"].value)

    upgrade.compareTable(natives, "attackPlayerThreshold", settings.global["rampant-attackPlayerThreshold"].value)
    upgrade.compareTable(natives, "attackUsePlayer", settings.global["rampant-attackWaveGenerationUsePlayerProximity"].value)

    upgrade.compareTable(natives, "attackWaveMaxSize", settings.global["rampant-attackWaveMaxSize"].value)
    upgrade.compareTable(natives, "aiNocturnalMode", settings.global["rampant-permanentNocturnal"].value)
    upgrade.compareTable(natives, "aiPointsScaler", settings.global["rampant-aiPointsScaler"].value)

    upgrade.compareTable(natives, "newEnemies", settings.startup["rampant-newEnemies"].value)
    upgrade.compareTable(natives, "enemySeed", settings.startup["rampant-enemySeed"].value)

    natives.enabledMigration = natives.expansion and settings.global["rampant-enableMigration"].value

    upgrade.compareTable(natives, "ENEMY_VARIATIONS", settings.startup["rampant-newEnemyVariations"].value)

    return true
end

local function prepWorld(rebuild, surfaceName)
    local upgraded

    if surfaceName then
        natives.activeSurface = surfaceName
        if rebuild then
            global.version = nil
        end
    end

    upgraded, natives = upgrade.attempt(natives, surfaceName, gameSurfaces)
    gameSurfaces = global.gameSurfaces
    onModSettingsChange(nil)
    if natives.newEnemies then
        rebuildNativeTables(natives, game.get_surface(natives.activeSurface), game.create_random_generator(natives.enemySeed))
    else
        natives.buildingHiveTypeLookup = {}
        natives.buildingHiveTypeLookup["biter-spawner"] = "biter-spawner"
        natives.buildingHiveTypeLookup["spitter-spawner"] = "spitter-spawner"
        natives.buildingHiveTypeLookup["small-worm-turret"] = "turret"
        natives.buildingHiveTypeLookup["medium-worm-turret"] = "turret"
        natives.buildingHiveTypeLookup["big-worm-turret"] = "turret"
        natives.buildingHiveTypeLookup["behemoth-worm-turret"] = "turret"
    end
    if upgraded then
        rebuildMap()

        map.natives = natives
        natives.map = map

        -- queue all current chunks that wont be generated during play
        local surface = game.get_surface(natives.activeSurface)
        local tick = game.tick
        local position = {0,0}
        natives.nextChunkSort = 0
        for chunk in surface.get_chunks() do
            local x = chunk.x
            local y = chunk.y
            position[1] = x
            position[2] = y
            if surface.is_chunk_generated(position) then
                onChunkGenerated({ surface = surface,
                                   area = { left_top = { x = x * 32,
                                                         y = y * 32}}})
            end
        end

        processPendingChunks(map, surface, tick, rebuild, true)
    end
end

local function onConfigChanged()
    prepWorld(true, natives.activeSurface)
end

local function onBuild(event)
    local entity = event.created_entity or event.entity
    if (entity.surface.name == natives.activeSurface) then
        if (entity.type == "resource") and (entity.force.name == "neutral") then
            registerResource(entity, map)
        else
            accountPlayerEntity(entity, natives, true, false)
            if natives.safeBuildings then
                if natives.safeEntities[entity.type] or natives.safeEntities[entity.name] then
                    entity.destructible = false
                end
            end
        end
    end
end

local function onMine(event)
    local entity = event.entity
    local surface = entity.surface
    if (surface.name == natives.activeSurface) then
        if (entity.type == "resource") and (entity.force.name == "neutral") then
            if (entity.amount == 0) then
                unregisterResource(entity, map)
            end
        else
            accountPlayerEntity(entity, natives, false, false)
        end
    end
end

local function onDeath(event)
    local entity = event.entity
    if entity.valid then
        local surface = entity.surface
        if (surface.name == natives.activeSurface) then
            local entityPosition = entity.position
            local chunk = getChunkByPosition(map, entityPosition)
            local cause = event.cause
            local tick = event.tick
            local entityType = entity.type
            if (entity.force.name == "enemy") then

                local artilleryBlast = (cause and ((cause.type == "artillery-wagon") or (cause.type == "artillery-turret")))

                if artilleryBlast then
                    natives.artilleryBlasts = natives.artilleryBlasts + 1
                end

                if (entityType == "unit") then
                    if (chunk ~= -1) then
                        -- drop death pheromone where unit died
                        deathScent(map, chunk)

                        if event.force and (event.force.name ~= "enemy") and (chunk[MOVEMENT_PHEROMONE] < -natives.retreatThreshold) then

                            natives.lostEnemyUnits = natives.lostEnemyUnits + 1

                            -- retreatUnits(chunk,
                            --              entityPosition,
                            --              entity.unit_group,
                            --              map,
                            --              surface,
                            --              tick,
                            --              (artilleryBlast and RETREAT_SPAWNER_GRAB_RADIUS) or RETREAT_GRAB_RADIUS-- ,
                            --              -- artilleryBlast
                            -- )

                            if (mRandom() < natives.rallyThreshold) and not surface.peaceful_mode then
                                rallyUnits(chunk, map, surface, tick)
                            end
                        end
                    end

                elseif event.force and (event.force.name ~= "enemy") and ((entityType == "unit-spawner") or (entityType == "turret")) then

                    natives.points = natives.points + (((entityType == "unit-spawner") and RECOVER_NEST_COST) or RECOVER_WORM_COST)

                    unregisterEnemyBaseStructure(map, entity)

                    if (chunk ~= -1) then
                        rallyUnits(chunk, map, surface, tick)

                        -- retreatUnits(chunk,
                        --              entityPosition,
                        --              nil,
                        --              map,
                        --              surface,
                        --              tick,
                        --              RETREAT_SPAWNER_GRAB_RADIUS-- ,
                        --              -- (cause and ((cause.type == "artillery-wagon") or (cause.type == "artillery-turret")))
                        -- )
                    end
                end

                local pair = natives.drainPylons[entity.unit_number]
                if pair then
                    local target = pair[1]
                    local pole = pair[2]
                    if target == entity then
                        natives.drainPylons[entity.unit_number] = nil
                        if pole.valid then
                            natives.drainPylons[pole.unit_number] = nil
                            pole.die()
                        end
                    elseif (pole == entity) then
                        natives.drainPylons[entity.unit_number] = nil
                        if target.valid then
                            natives.drainPylons[target.unit_number] = nil
                            target.destroy()
                        end
                    end
                end
            elseif (entity.force.name ~= "enemy") then
                local creditNatives = false
                if (event.force ~= nil) and (event.force.name == "enemy") then
                    creditNatives = true
                    if (chunk ~= -1) then
                        victoryScent(map, chunk, entityType)
                    end

                    local drained = (entityType == "electric-turret") and map.chunkToDrained[chunk]
                    if (cause ~= nil) or (drained and (drained - tick) > 0) then
                        if ((cause and ENERGY_THIEF_LOOKUP[cause.name]) or (not cause)) then
                            local conversion = ENERGY_THIEF_CONVERSION_TABLE[entityType]
                            if conversion then
                                local newEntity = surface.create_entity({position=entity.position,
                                                                         name=convertTypeToDrainCrystal(entity.force.evolution_factor, conversion),
                                                                         direction=entity.direction})
                                if (conversion == "pole") then
                                    local targetEntity = surface.create_entity({position=entity.position,
                                                                                name="pylon-target-rampant",
                                                                                direction=entity.direction})
                                    targetEntity.backer_name = ""
                                    local pair = {targetEntity, newEntity}
                                    natives.drainPylons[targetEntity.unit_number] = pair
                                    natives.drainPylons[newEntity.unit_number] = pair
                                    local wires = entity.neighbours
                                    if wires then
                                        for _,v in pairs(wires.copper) do
                                            if (v.valid) then
                                                newEntity.connect_neighbour(v);
                                            end
                                        end
                                        for _,v in pairs(wires.red) do
                                            if (v.valid) then
                                                newEntity.connect_neighbour({wire = DEFINES_WIRE_TYPE_RED, target_entity = v});
                                            end
                                        end
                                        for _,v in pairs(wires.green) do
                                            if (v.valid) then
                                                newEntity.connect_neighbour({wire = DEFINES_WIRE_TYPE_GREEN, target_entity = v});
                                            end
                                        end
                                    end
                                elseif newEntity.backer_name then
                                    newEntity.backer_name = ""
                                end
                            end
                        end
                    end
                elseif (entity.type == "resource") and (entity.force.name == "neutral") then
                    if (entity.amount == 0) then
                        unregisterResource(entity, map)
                    end
                end
                if creditNatives and natives.safeBuildings and (natives.safeEntities[entityType] or natives.safeEntities[entity.name]) then
                    makeImmortalEntity(surface, entity)
                else
                    accountPlayerEntity(entity, natives, false, creditNatives)
                end
            end
        end
    end
end

local function onEnemyBaseBuild(event)
    local entity = event.entity
    if entity.valid then
        local surface = entity.surface

        if (surface.name == natives.activeSurface) then
            local chunk = getChunkByPosition(map, entity.position)
            if (chunk ~= -1) then
                local base
                if natives.newEnemies then
                    base = findNearbyBase(map, chunk)
                    if not base then
                        base = createBase(natives,
                                          chunk,
                                          event.tick)
                    end
                    entity = upgradeEntity(entity,
                                           surface,
                                           base.alignment,
                                           natives,
                                           nil,
                                           true)
                end
                if entity and entity.valid then
                    event.entity = registerEnemyBaseStructure(map, entity, base, surface)
                end
            end
        end
    end
end

local function onSurfaceTileChange(event)
    local surfaceIndex = event.surface_index or (event.robot and event.robot.surface and event.robot.surface.index)
    local surface = game.get_surface(natives.activeSurface)
    if (surface.index == surfaceIndex) then
        local chunks = {}
        local tiles = event.tiles
        if (event.tile.name == "landfill") or sFind(event.tile.name, "water") then
            for i=1,#tiles do
                local position = tiles[i].position
                local chunk = getChunkByPosition(map, position)

                if (chunk ~= -1) then
                    map.chunkToPassScan[chunk] = true
                else
                    local x,y = positionToChunkXY(position)
                    local addMe = true
                    for ci=1,#chunks do
                        local c = chunks[ci]
                        if (c.x == x) and (c.y == y) then
                            addMe = false
                            break
                        end
                    end
                    if addMe then
                        local chunkXY = {x=x,y=y}
                        chunks[#chunks+1] = chunkXY
                        onChunkGenerated({area = { left_top = chunkXY },
                                          surface = surface})
                    end
                end
            end
        end
    end
end

local function onResourceDepleted(event)
    local entity = event.entity
    if (entity.surface.name == natives.activeSurface) then
        unregisterResource(entity, map)
    end
end

local function onRobotCliff(event)
    local surface = event.robot.surface
    if (surface.name == natives.activeSurface) and (event.item.name == "cliff-explosives") then
        entityForPassScan(map, event.cliff)
    end
end

local function onUsedCapsule(event)
    local surface = game.players[event.player_index].surface
    if (surface.name == natives.activeSurface) and (event.item.name == "cliff-explosives") then
        map.position2Top.x = event.position.x-0.75
        map.position2Top.y = event.position.y-0.75
        map.position2Bottom.x = event.position.x+0.75
        map.position2Bottom.y = event.position.y+0.75
        local cliffs = surface.find_entities_filtered(map.cliffQuery)
        for i=1,#cliffs do
            entityForPassScan(map, cliffs[i])
        end
    end
end

local function onRocketLaunch(event)
    local entity = event.rocket_silo or event.rocket
    if entity and entity.valid and (entity.surface.name == natives.activeSurface) then
        natives.rocketLaunched = natives.rocketLaunched + 1
        natives.points = natives.points + 5000
    end
end

local function onTriggerEntityCreated(event)
    local entity = event.entity
    if entity.valid and (entity.surface.name == natives.activeSurface) and (entity.name  == "drain-trigger-rampant") then
        local chunk = getChunkByPosition(map, entity.position)
        if (chunk ~= -1) then
            map.chunkToDrained[chunk] = event.tick + 60
        end
        entity.destroy()
    end
end

local function onInit()
    global.map = {}
    global.natives = {}
    global.gameSurfaces = {}

    map = global.map
    natives = global.natives
    gameSurfaces = global.gameSurfaces

    prepWorld(false, "nauvis")
    hookEvents()
end

local function onEntitySpawned(event)
    local entity = event.entity
    if natives.newEnemies and entity.valid then
        local surface = entity.surface

        if (surface.name == natives.activeSurface) and (entity.type ~= "unit") then
            if natives.buildingHiveTypeLookup[entity.name] then
                local spawner = event.spawner

                local disPos = mathUtils.distortPosition(entity.position, 8)
                local canPlaceQuery = map.canPlaceQuery

                local chunk = getChunkByPosition(map, disPos)
                if (chunk ~= -1) then
                    local base = findNearbyBase(map, chunk)
                    if not base then
                        base = createBase(natives,
                                          chunk,
                                          event.tick)
                    end

                    entity = upgradeEntity(entity,
                                           surface,
                                           base.alignment,
                                           natives,
                                           disPos)
                    if entity and entity.valid then
                        event.entity = registerEnemyBaseStructure(map, entity, base, surface)
                    end
                else
                    entity.destroy()
                end
            else
                entity.destroy()
            end
        end
    end
end

local function onUnitGroupCreated(event)
    local profiler = game.create_profiler()
    local group = event.group
    local surface = group.surface
    if (surface.name == natives.activeSurface) and (group.force.name == "enemy") then
        if not group.is_script_driven then
            if not natives.aiNocturnalMode then
                local settler = mRandom() < 0.25 and
                    canMigrate(natives, group.surface) and
                    (natives.builderCount < AI_MAX_BUILDER_COUNT)

                squad = createSquad(nil, nil, group, settler)
                natives.groupNumberToSquad[group.group_number] = squad
                if settler then
                    natives.builderCount = natives.builderCount + 1
                end
            elseif not (surface.darkness > 0.65) then
                group.destroy()
            else
                local settler = mRandom() < 0.25 and
                    canMigrate(natives, group.surface) and
                    (natives.builderCount < AI_MAX_BUILDER_COUNT)

                squad = createSquad(nil, nil, group, settler)
                natives.groupNumberToSquad[group.group_number] = squad
                if settler then
                    natives.builderCount = natives.builderCount + 1
                end
            end
        end
    end
    -- game.print({"", "squadCreated", profiler, event.tick})
end

local function onCommandComplete(event)

    local unitNumber = event.unit_number
    local squad = natives.groupNumberToSquad[unitNumber]
    if squad then
        local profiler = game.create_profiler()
        -- local result = event.result
        -- local msg
        -- if (result == defines.behavior_result.in_progress) then
        --     msg = "progress"
        -- elseif (result == defines.behavior_result.fail) then
        --     msg = "fail"
        -- elseif (result == defines.behavior_result.success) then
        --     msg = "success"
        -- elseif (result == defines.behavior_result.deleted) then
        --     msg = "deleted"
        -- end
        -- print(msg)
        local group = squad.group
        if group and group.valid and (group.surface.name == natives.activeSurface) then

            if (event.result == DEFINES_BEHAVIOR_RESULT_FAIL) then
                print("count", #group.members)
                if (#group.members == 0) then
                    print("dropping")
                    local deathGen = getDeathGenerator(map, squad.chunk)
                    local penalties = squad.penalties
                    for xc=1,mMin(#squad.penalties,5) do
                        addDeathGenerator(map,
                                          penalties[xc].c,
                                          deathGen * DIVISOR_DEATH_TRAIL_TABLE[xc])
                    end
                    removeSquadFromChunk(map, squad)
                    group.destroy()
                else
                    squadDispatch(map, group.surface, squad, unitNumber)
                end
            else
                squadDispatch(map, group.surface, squad, unitNumber)
            end
        end
        -- game.print({"", "aiCommand", profiler, event.tick})
    end
end

local function onGroupFinishedGathering(event)
    local profiler = game.create_profiler()
    local group = event.group
    if group.valid then
        local unitNumber = group.group_number
        local squad = natives.groupNumberToSquad[unitNumber]
        if squad and (group.surface.name == natives.activeSurface) then
            squadDispatch(map, group.surface, squad, unitNumber)
        end
    end
    -- game.print({"", "finishedGather", profiler, event.tick})
end

local function onForceCreated(event)
    map.activePlayerForces[#map.activePlayerForces+1] = event.force.name
end

local function onForceMerged(event)
    for i=#map.activePlayerForces,1,-1 do
        if (map.activePlayerForces[i] == event.source_name) then
            tRemove(map.activePlayerForces, i)
            break
        end
    end
end

local function onSurfaceRenamed(event)
    if event.old_name == natives.activeSurface then
        natives.activeSurface = event.new_name
    end
    if (gameSurfaces[event.old_name]) then
        gameSurfaces[event.new_name] = gameSurfaces[event.old_name]
        gameSurfaces[event.old_name] = nil
    end
end

local function onSurfaceCleared(event)
    local surface = game.get_surface(event.surface_index)
    if surface and surface.valid and (surface.name == natives.activeSurface) then
        prepWorld(true, natives.activeSurface)
    end
end

local function onPlayerChangedSurface(event)
    local player = game.players[event.player_index]
    local surface
    if player and player.valid and not settings.get_player_settings(player)["rampant-suppress-surface-change-warnings"].value
    then
        surface = player.surface
        if (natives.activeSurface ~= surface.name) then
            local playerName = player.name
            local surfaceName = surface.name
            local playerSurfaces = gameSurfaces[surfaceName]
            if not playerSurfaces then
                playerSurfaces = {}
                gameSurfaces[surfaceName] = playerSurfaces
                player.print({"description.rampant-change-surface", surfaceName, natives.activeSurface})
                playerSurfaces[playerName] = true
            elseif not playerSurfaces[playerName] then
                player.print({"description.rampant-change-surface", surfaceName, natives.activeSurface})
                playerSurfaces[playerName] = true
            end
        end
    end
end

local function onSurfaceDeleted(event)
    local surface = game.get_surface(event.surface_index)
    if surface and surface.valid then
        if (surface.name == natives.activeSurface) then
            prepWorld(true, "nauvis")
        end
        if (gameSurfaces[surface.name]) then
            gameSurfaces[surface.name] = nil
        end
    end
end

-- hooks

script.on_nth_tick(INTERVAL_PLAYER_PROCESS,
                   function (event)
                       local profiler = game.create_profiler()
                       local gameRef = game

                       processPlayers(gameRef.connected_players,
                                      map,
                                      gameRef.get_surface(natives.activeSurface),
                                      event.tick)
                       -- game.print({"", "player", profiler, tick})
end)

script.on_nth_tick(INTERVAL_CHUNK_PROCESS,
                   function (event)
                       local profiler = game.create_profiler()
                       processPendingChunks(map,
                                            game.get_surface(natives.activeSurface),
                                            event.tick)
                       -- game.print({"", "chunk process", profiler, event.tick})
end)

script.on_nth_tick(INTERVAL_MAP_PROCESS,
                   function (event)
                       local profiler = game.create_profiler()
                       local tick = event.tick

                       if ((tick % INTERVAL_PLAYER_PROCESS) ~= 0) then
                           processMap(map,
                                      game.get_surface(natives.activeSurface),
                                      tick)
                       else
                           print("skipping MP", tick)
                       end
                       -- game.print({"", "map", profiler, event.tick})
end)

script.on_nth_tick(INTERVAL_MAP_STATIC_PROCESS,
                   function (event)
                       local profiler = game.create_profiler()
                       local tick = event.tick

                       if ((tick % INTERVAL_PLAYER_PROCESS) ~= 0) and ((tick % INTERVAL_MAP_PROCESS) ~= 0) then
                           processStaticMap(map,
                                            game.get_surface(natives.activeSurface),
                                            tick)
                       else
                           print("skipping MSP", tick)
                       end
                       -- game.print({"", "mapStatic", profiler, event.tick})
end)


script.on_nth_tick(INTERVAL_SCAN,
                   function (event)
                       local profiler = game.create_profiler()
                       local tick = event.tick
                       if ((tick % INTERVAL_PLAYER_PROCESS) ~= 0) and ((tick % INTERVAL_MAP_PROCESS) ~= 0) and
                           ((tick % INTERVAL_MAP_STATIC_PROCESS) ~= 0)
                       then
                           local picker = tick % 3
                           if (picker == 0) then
                               scanResourceMap(map,
                                               game.get_surface(natives.activeSurface),
                                               tick)
                           elseif (picker == 1) then
                               scanEnemyMap(map,
                                            game.get_surface(natives.activeSurface),
                                            tick)
                           elseif (picker == 2) then
                               scanPlayerMap(map,
                                             game.get_surface(natives.activeSurface),
                                             tick)
                           end
                       else
                           print("skipping scan", event.tick)
                       end
                       -- game.print({"", "scan", profiler, event.tick})
end)

script.on_nth_tick(INTERVAL_PASS_SCAN,
                   function (event)
                       local profiler = game.create_profiler()
                       processScanChunks(map,
                                         game.get_surface(natives.activeSurface))
                       -- game.print({"", "passscan", profiler, event.tick})
end)

script.on_nth_tick(INTERVAL_LOGIC,
                   function (event)
                       local profiler = game.create_profiler()
                       local tick = event.tick
                       planning(natives,
                                game.forces.enemy.evolution_factor,
                                tick)

                       cleanSquads(natives, map.squadIterator)

                       if natives.newEnemies then
                           recycleBases(natives, tick)
                       end
                       -- game.print({"", "logic", profiler, event.tick})
end)

script.on_nth_tick(INTERVAL_NEST,
                   function (event)
                       local profiler = game.create_profiler()
                       processNests(map,
                                    game.get_surface(natives.activeSurface),
                                    event.tick)
                       -- game.print({"", "nest", profiler, event.tick})
end)


script.on_nth_tick(INTERVAL_CLEANUP,
                   function (event)
                       local profiler = game.create_profiler()
                       cleanUpMapTables(map,
                                        event.tick)
                       -- game.print({"", "cleanup", profiler, event.tick})
end)

script.on_nth_tick(INTERVAL_SPAWNER,
                   function (event)
                       local profiler = game.create_profiler()
                       processSpawners(map,
                                       game.get_surface(natives.activeSurface),
                                       event.tick)
                       -- game.print({"", "spawners", profiler, event.tick})
end)

script.on_nth_tick(INTERVAL_SQUAD,
                   function (event)
                       local profiler = game.create_profiler()
                       processVengence(map,
                                       game.get_surface(natives.activeSurface),
                                       event.tick)
                       -- game.print({"", "vengence", profiler, event.tick})
end)

script.on_nth_tick(INTERVAL_TEMPERAMENT,
                   function (event)
                       local profiler = game.create_profiler()
                       temperamentPlanner(natives)
                       -- game.print({"", "temperament", profiler, event.tick})
end)

script.on_nth_tick(INTERVAL_RESQUAD,
                   function (event)
                       local profiler = game.create_profiler()
                       regroupSquads(natives,
                                     game.get_surface(natives.activeSurface),
                                     map.regroupIterator)
                       -- game.print({"", "regroup", profiler, event.tick})
end)

script.on_event(defines.events.on_tick,
                function (event)
                    -- if (event.tick > 36678750) then
                    --     local allGroups = {}
                    --     for _, unit in pairs(game.surfaces["nauvis"].find_entities_filtered{type = "unit", force = "enemy"}) do
                    --         if unit and unit.unit_group then
                    --             allGroups[unit.unit_group.group_number] = unit.unit_group
                    --         end
                    --     end
                    --     for group_number, group in pairs(allGroups) do
                    --         if group.position.x < -1000000 or group.position.x > 1000000 or group.position.y < -1000000 or group.position.y > 1000000 then
                    --             game.print(tostring(group_number..": "..serpent.dump(group.position)))
                    --         end
                    --     end
                    -- end

                    -- local profiler = game.create_profiler()
                    processActiveNests(map,
                                       game.get_surface(natives.activeSurface),
                                       event.tick)
                    -- game.print({"", "processActiveNests", profiler, event.tick})
end)

script.on_event(defines.events.on_surface_cleared, onSurfaceCleared)
script.on_event(defines.events.on_surface_renamed, onSurfaceRenamed)
script.on_event(defines.events.on_player_changed_surface, onPlayerChangedSurface)

script.on_init(onInit)
script.on_load(onLoad)
script.on_event(defines.events.on_runtime_mod_setting_changed, onModSettingsChange)
script.on_configuration_changed(onConfigChanged)

script.on_event(defines.events.on_resource_depleted, onResourceDepleted)
script.on_event({defines.events.on_player_built_tile,
                 defines.events.on_robot_built_tile}, onSurfaceTileChange)

script.on_event(defines.events.on_player_used_capsule, onUsedCapsule)

script.on_event(defines.events.on_trigger_created_entity, onTriggerEntityCreated)

script.on_event(defines.events.on_pre_robot_exploded_cliff, onRobotCliff)

script.on_event(defines.events.on_biter_base_built, onEnemyBaseBuild)
script.on_event({defines.events.on_player_mined_entity,
                 defines.events.on_robot_mined_entity}, onMine)

script.on_event({defines.events.on_built_entity,
                 defines.events.on_robot_built_entity,
                 defines.events.script_raised_built,
                 defines.events.script_raised_revive}, onBuild)

script.on_event(defines.events.on_ai_command_completed, onCommandComplete)
script.on_event(defines.events.on_entity_spawned, onEntitySpawned)

script.on_event(defines.events.on_rocket_launched, onRocketLaunch)
script.on_event({defines.events.on_entity_died,
                 defines.events.script_raised_destroy}, onDeath)
script.on_event(defines.events.on_chunk_generated, onChunkGenerated)
script.on_event(defines.events.on_unit_group_created, onUnitGroupCreated)
script.on_event(defines.events.on_force_created, onForceCreated)
script.on_event(defines.events.on_forces_merged, onForceMerged)
script.on_event(defines.events.on_unit_group_finished_gathering, onGroupFinishedGathering)

remote.add_interface("rampantTests",
                     {
                         pheromoneLevels = tests.pheromoneLevels,
                         activeSquads = tests.activeSquads,
                         entitiesOnPlayerChunk = tests.entitiesOnPlayerChunk,
                         findNearestPlayerEnemy = tests.findNearestPlayerEnemy,
                         morePoints = tests.morePoints,
                         aiStats = tests.aiStats,
                         dumpEnvironment = tests.dumpEnvironment,
                         fillableDirtTest = tests.fillableDirtTest,
                         tunnelTest = tests.tunnelTest,
                         dumpNatives = tests.dumpatives,
                         createEnemy = tests.createEnemy,
                         attackOrigin = tests.attackOrigin,
                         cheatMode = tests.cheatMode,
                         gaussianRandomTest = tests.gaussianRandomTest,
                         reveal = tests.reveal,
                         showMovementGrid = tests.showMovementGrid,
                         showBaseGrid = tests.showBaseGrid,
                         baseStats = tests.baseStats,
                         mergeBases = tests.mergeBases,
                         clearBases = tests.clearBases,
                         getOffsetChunk = tests.getOffsetChunk,
                         registeredNest = tests.registeredNest,
                         colorResourcePoints = tests.colorResourcePoints,
                         entityStats = tests.entityStats,
                         stepAdvanceTendrils = tests.stepAdvanceTendrils,
                         unitGroupBuild = tests.unitGroupBuild,
                         exportAiState = tests.exportAiState(nil),
                         createEnergyTest = tests.createEnergyTest,
                         killActiveSquads = tests.killActiveSquads,
                         scanChunkPaths = tests.scanChunkPaths
                     }
)

interop.setActiveSurface = function (surfaceName)
    prepWorld(true, surfaceName)
end
commands.add_command("GetRampantAISurface",
                     {"description.rampant-get-surface"},
                     function (event)
                         for _,player in pairs(game.connected_players) do
                             if (player.valid) then
                                 player.print({"description.rampant-get-surface",
                                               player.surface.name,
                                               natives.activeSurface})
                             end
                         end
end)
commands.add_command("SetRampantAISurface",
                     {"description.rampant-set-surface"},
                     function (event)
                         local surfaceName = event.parameter
                         if surfaceName then
                             if (surfaceName ~= natives.activeSurface) then
                                 if not game.get_surface(surfaceName) then
                                     game.print({"description.rampant-invalid-set-surface", surfaceName})
                                 else
                                     local surface = game.get_surface(natives.activeSurface)
                                     if surface and surface.valid then
                                         for _,entity in pairs(natives.drainPylons) do
                                             if entity and entity.valid then
                                                 entity.destroy()
                                             end
                                         end
                                     end
                                     prepWorld(true, surfaceName)
                                     game.print({"description.rampant-set-surface", surfaceName})
                                 end
                             else
                                 game.print({"description.rampant-already-set-surface", surfaceName})
                             end
                         else
                             game.print({"description.rampant-error-set-surface"})
                         end
end)
remote.add_interface("rampant", interop)
