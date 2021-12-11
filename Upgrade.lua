local upgrade = {}

-- imports

local constants = require("libs/Constants")
local chunkProcessor = require("libs/ChunkProcessor")
local mapUtils = require("libs/MapUtils")

-- constants

local DEFINES_COMMAND_GROUP = defines.command.group
local DEFINES_COMMAND_WANDER = defines.command.wander
local DEFINES_COMMAND_BUILD_BASE = defines.command.build_base
local DEFINES_COMMAND_ATTACK_AREA = defines.command.attack_area
local DEFINES_COMMAND_GO_TO_LOCATION = defines.command.go_to_location
local DEFINES_COMMMAD_COMPOUND = defines.command.compound
local DEFINES_COMMAND_FLEE = defines.command.flee
local DEFINES_COMMAND_STOP = defines.command.stop

local DEFINES_COMPOUND_COMMAND_RETURN_LAST = defines.compound_command.return_last

local DEFINES_DISTRACTION_NONE = defines.distraction.none
local DEFINES_DISTRACTION_BY_ENEMY = defines.distraction.by_enemy
local DEFINES_DISTRACTION_BY_ANYTHING = defines.distraction.by_anything

local CHUNK_SIZE = constants.CHUNK_SIZE
local TRIPLE_CHUNK_SIZE = constants.TRIPLE_CHUNK_SIZE

-- imported functions

local sFind = string.find
local queueGeneratedChunk = mapUtils.queueGeneratedChunk
local processPendingChunks = chunkProcessor.processPendingChunks

-- module code

local function addCommandSet(queriesAndCommands)
    -- preallocating memory to be used in code, making it fast by reducing garbage generated.
    queriesAndCommands.neighbors = {
            -1,
            -1,
            -1,
            -1,
            -1,
            -1,
            -1,
            -1
    }
    queriesAndCommands.cardinalNeighbors = {
            -1,
            -1,
            -1,
            -1
    }

    queriesAndCommands.chunkOverlapArray = {
            -1,
            -1,
            -1,
            -1
    }

    queriesAndCommands.playerForces = {}
    queriesAndCommands.enemyForces = {}
    queriesAndCommands.npcForces = {}
    queriesAndCommands.nonPlayerForces = {}

    -- pb
    queriesAndCommands.pbFilteredEntitiesPointQueryLimited = {
        position = {0, 0},
        radius = 10,
        limit = 1,
        force = queriesAndCommands.enemyForces,
        type = {
            "unit-spawner",
            "turret"
        }
    }

    -- msec
    queriesAndCommands.msecFilteredEntitiesEnemyStructureQuery = {
        area={
            {0,0},
            {0,0}
        },
        force=queriesAndCommands.enemyForces,
        type={
            "turret",
            "unit-spawner"
        }
    }

    -- oba
    queriesAndCommands.obaCreateBuildCloudQuery = {
        name = "build-clear-cloud-rampant",
        position = {0,0}
    }

    -- sp
    local spbSharedChunkArea = {
        {0,0},
        {0,0}
    }
    queriesAndCommands.spbHasPlayerStructuresQuery = {
        area=spbSharedChunkArea,
        force=queriesAndCommands.nonPlayerForces,
        invert=true,
        limit=1
    }
    queriesAndCommands.spbFilteredEntitiesPlayerQueryLowest = {
        area=spbSharedChunkArea,
        force=queriesAndCommands.playerForces,
        collision_mask = "player-layer",
        type={
            "wall",
            "transport-belt"
        }
    }
    queriesAndCommands.spbFilteredEntitiesPlayerQueryLow = {
        area=spbSharedChunkArea,
        force=queriesAndCommands.playerForces,
        collision_mask = "player-layer",
        type={
            "splitter",
            "pump",
            "offshore-pump",
            "lamp",
            "solar-panel",
            "programmable-speaker",
            "accumulator",
            "assembling-machine",
            "turret",
            "ammo-turret"
        }
    }
    queriesAndCommands.spbFilteredEntitiesPlayerQueryHigh = {
        area=spbSharedChunkArea,
        force=queriesAndCommands.playerForces,
        collision_mask = "player-layer",
        type={
            "furnace",
            "lab",
            "roboport",
            "beacon",
            "radar",
            "electric-turret",
            "boiler",
            "generator",
            "fluid-turret",
            "mining-drill"
        }
    }
    queriesAndCommands.spbFilteredEntitiesPlayerQueryHighest = {
        area=spbSharedChunkArea,
        force=queriesAndCommands.playerForces,
        collision_mask = "player-layer",
        type={
            "artillery-turret",
            "reactor",
            "rocket-silo"
        }
    }

    -- is
    local isSharedChunkArea = {
        {0,0},
        {0,0}
    }
    queriesAndCommands.isFilteredTilesQuery = {
        collision_mask="water-tile",
        area=isSharedChunkArea
    }
    queriesAndCommands.isFilteredEntitiesChunkNeutral = {
        area=isSharedChunkArea,
        collision_mask = "player-layer",
        type={
            "tree",
            "simple-entity"
        }
    }
    queriesAndCommands.isFilteredEntitiesEnemyStructureQuery = {
        area=isSharedChunkArea,
        force=queriesAndCommands.enemyForces,
        type={
            "turret",
            "unit-spawner"
        }
    }
    queriesAndCommands.isCountResourcesQuery = {
        area=isSharedChunkArea,
        type="resource"
    }
    queriesAndCommands.isFilteredEntitiesUnitQuery = {
        area=isSharedChunkArea,
        force=queriesAndCommands.enemyForces,
        type="unit"
    }

    -- cps
    local cpsSharedChunkArea = {
        {0,0},
        {0,0}
    }
    queriesAndCommands.cpsFilteredTilesQuery = {
        collision_mask="water-tile",
        area=cpsSharedChunkArea
    }
    queriesAndCommands.cpsFilteredEntitiesChunkNeutral = {
        area=cpsSharedChunkArea,
        collision_mask = "player-layer",
        type={
            "tree",
            "simple-entity"
        }
    }

    -- msrc
    local msrcSharedChunkArea = {
        {0,0},
        {0,0}
    }
    queriesAndCommands.msrcFilteredTilesQuery = {
        collision_mask="water-tile",
        area=msrcSharedChunkArea
    }
    queriesAndCommands.msrcFilteredEntitiesChunkNeutral = {
        area=msrcSharedChunkArea,
        collision_mask = "player-layer",
        type={
            "tree",
            "simple-entity"
        }
    }
    queriesAndCommands.msrcCountResourcesQuery = {
        area=msrcSharedChunkArea,
        type="resource"
    }

    -- sp
    local spSharedAreaChunk = {
        {0,0},
        {0,0}
    }
    queriesAndCommands.spFilteredEntitiesCliffQuery = {
        area=spSharedAreaChunk,
        type="cliff",
        limit = 1
    }
    queriesAndCommands.spFilteredTilesPathQuery = {
        area=spSharedAreaChunk,
        collision_mask="water-tile",
        limit = 1
    }

    -- ouc
    queriesAndCommands.oucCliffQuery = {
        area={
            {0,0},
            {0,0}
        },
        type="cliff"
    }

    -- ppu
    queriesAndCommands.ppuUpgradeEntityQuery = {
        name = "",
        position = {0,0}
    }

    queriesAndCommands.attackCommand = {
        type = DEFINES_COMMAND_ATTACK_AREA,
        destination = {0,0},
        radius = CHUNK_SIZE * 1.5,
        distraction = DEFINES_DISTRACTION_BY_ANYTHING
    }

    queriesAndCommands.moveCommand = {
        type = DEFINES_COMMAND_GO_TO_LOCATION,
        destination = {0,0},
        pathfind_flags = { cache = true },
        distraction = DEFINES_DISTRACTION_BY_ENEMY
    }

    queriesAndCommands.settleCommand = {
        type = DEFINES_COMMAND_BUILD_BASE,
        destination = {0,0},
        distraction = DEFINES_DISTRACTION_BY_ENEMY,
        ignore_planner = true
    }

    queriesAndCommands.wanderCommand = {
        type = DEFINES_COMMAND_WANDER,
        wander_in_group = false,
        radius = TRIPLE_CHUNK_SIZE*2,
        ticks_to_wait = 20 * 60
    }

    queriesAndCommands.wander2Command = {
        type = DEFINES_COMMAND_WANDER,
        wander_in_group = true,
        radius = TRIPLE_CHUNK_SIZE*2,
        ticks_to_wait = 2 * 60
    }

    queriesAndCommands.stopCommand = {
        type = DEFINES_COMMAND_STOP
    }

    queriesAndCommands.compoundSettleCommand = {
        type = DEFINES_COMMMAD_COMPOUND,
        structure_type = DEFINES_COMPOUND_COMMAND_RETURN_LAST,
        commands = {
            queriesAndCommands.wonder2Command,
            queriesAndCommands.settleCommand
        }
    }

    queriesAndCommands.retreatCommand = {
        type = DEFINES_COMMAND_GROUP,
        group = nil,
        distraction = DEFINES_DISTRACTION_BY_ANYTHING,
        use_group_distraction = true
    }

    queriesAndCommands.fleeCommand = {
        type = DEFINES_COMMAND_FLEE,
        from = nil,
        distraction = DEFINES_DISTRACTION_NONE
    }

    queriesAndCommands.compoundRetreatGroupCommand = {
        type = DEFINES_COMMMAD_COMPOUND,
        structure_type = DEFINES_COMPOUND_COMMAND_RETURN_LAST,
        commands = {
            queriesAndCommands.stopCommand,
            queriesAndCommands.fleeCommand,
            queriesAndCommands.retreatCommand
        }
    }

    queriesAndCommands.formGroupCommand = {
        type = DEFINES_COMMAND_GROUP,
        group = nil,
        distraction = DEFINES_DISTRACTION_BY_ANYTHING,
        use_group_distraction = false
    }

    queriesAndCommands.formCommand = {
        command = queriesAndCommands.formGroupCommand,
        unit_count = 0,
        unit_search_distance = TRIPLE_CHUNK_SIZE
    }

    queriesAndCommands.formRetreatCommand = {
        command = queriesAndCommands.compoundRetreatGroupCommand,
        unit_count = 1,
        unit_search_distance = CHUNK_SIZE
    }

    queriesAndCommands.setCommandForces = function (npcForces, enemyForces)
        for force in pairs(queriesAndCommands.playerForces) do
            queriesAndCommands.playerForces[force] = nil
        end
        for force in pairs(queriesAndCommands.npcForces) do
            queriesAndCommands.npcForces[force] = nil
        end
        for force in pairs(queriesAndCommands.enemyForces) do
            queriesAndCommands.enemyForces[force] = nil
        end
        for force in pairs(queriesAndCommands.nonPlayerForces) do
            queriesAndCommands.nonPlayerForces[force] = nil
        end
        for _,force in pairs(game.forces) do
            if not npcForces[force.name] and not enemyForces[force.name] then
                queriesAndCommands.playerForces[#queriesAndCommands.playerForces+1] = force.name
            end
        end
        for force in pairs(enemyForces) do
            queriesAndCommands.enemyForces[#queriesAndCommands.enemyForces+1] = force
            queriesAndCommands.nonPlayerForces[#queriesAndCommands.nonPlayerForces+1] = force
        end
        for force in pairs(npcForces) do
            queriesAndCommands.npcForces[#queriesAndCommands.npcForces+1] = force
            queriesAndCommands.nonPlayerForces[#queriesAndCommands.nonPlayerForces+1] = force
        end
    end
end

function upgrade.attempt(universe)
    local starting = global.version
    if not global.version or global.version < 114 then
        global.version = 114

        if not universe then
            universe = {}
            global.universe = universe
        end
        game.forces.enemy.kill_all_units()

        universe.safeEntities = {}

        universe.aiPointsScaler = settings.global["rampant--aiPointsScaler"].value

        universe.aiPointsPrintGainsToChat = settings.global["rampant--aiPointsPrintGainsToChat"].value
        universe.aiPointsPrintSpendingToChat = settings.global["rampant--aiPointsPrintSpendingToChat"].value

        universe.aiNocturnalMode = settings.global["rampant--permanentNocturnal"].value

        universe.mapIterator = nil
        universe.retreatThreshold = 0
        universe.rallyThreshold = 0
        universe.formSquadThreshold = 0
        universe.attackWaveSize = 0
        universe.attackWaveDeviation = 0
        universe.attackWaveUpperBound = 0
        universe.unitRefundAmount = 0
        universe.regroupIndex = 1

        game.map_settings.path_finder.min_steps_to_check_path_find_termination =
            constants.PATH_FINDER_MIN_STEPS_TO_CHECK_PATH

        universe.evolutionTableAlignment = {}

        universe.kamikazeThreshold = 0
        universe.attackWaveLowerBound = 1

        universe.expansion = game.map_settings.enemy_expansion.enabled
        universe.expansionMaxDistance = game.map_settings.enemy_expansion.max_expansion_distance * CHUNK_SIZE
        universe.expansionMaxDistanceDerivation = universe.expansionMaxDistance * 0.33
        universe.expansionMinTime = game.map_settings.enemy_expansion.min_expansion_cooldown
        universe.expansionMaxTime = game.map_settings.enemy_expansion.max_expansion_cooldown
        universe.expansionMinSize = game.map_settings.enemy_expansion.settler_group_min_size
        universe.expansionMaxSize = game.map_settings.enemy_expansion.settler_group_max_size

        universe.settlerCooldown = 0
        universe.settlerWaveDeviation = 0
        universe.settlerWaveSize = 0

        universe.enabledMigration = universe.expansion and settings.global["rampant--enableMigration"].value
        universe.peacefulAIToggle = settings.global["rampant--peacefulAIToggle"].value
        universe.printAIStateChanges = settings.global["rampant--printAIStateChanges"].value
        universe.debugTemperament = settings.global["rampant--debugTemperament"].value

        universe.enemyAlignmentLookup = {}

        game.map_settings.unit_group.min_group_radius = constants.UNIT_GROUP_MAX_RADIUS * 0.5
        game.map_settings.unit_group.max_group_radius = constants.UNIT_GROUP_MAX_RADIUS

        game.map_settings.unit_group.max_member_speedup_when_behind = constants.UNIT_GROUP_MAX_SPEED_UP
        game.map_settings.unit_group.max_member_slowdown_when_ahead = constants.UNIT_GROUP_MAX_SLOWDOWN
        game.map_settings.unit_group.max_group_slowdown_factor = constants.UNIT_GROUP_SLOWDOWN_FACTOR

        game.map_settings.max_failed_behavior_count = 3
        game.map_settings.unit_group.member_disown_distance = 10
        game.map_settings.unit_group.tick_tolerance_when_member_arrives = 60
        game.forces.enemy.ai_controllable = true

        universe.evolutionLevel = game.forces.enemy.evolution_factor
        global.pendingChunks = nil
        global.natives = nil
        global.map = nil
    end
    if global.version < 116 then
        global.version = 116

        universe.maxPoints = 0
    end
    if global.version < 203 then
        global.version = 203

        addCommandSet(universe)
        universe.eventId = 0
        universe.chunkId = 0
        universe.randomGenerator = nil
        universe.random = game.create_random_generator(settings.startup["rampant--enemySeed"].value+game.default_map_gen_settings.seed)
        game.forces.enemy.kill_all_units()
        universe.maps = {}
        universe.chunkIdToChunk = {}
        universe.groupNumberToSquad = {}
        universe.pendingUpgrades = {}
        universe.chunkToVictory = {}
        universe.pendingChunks = {}
        universe.processActiveNest = {}
        universe.processActiveNestIterator = nil
        universe.deployVengenceIterator = nil
        universe.pendingUpgradeIterator = nil
        universe.victoryScentIterator = nil
        universe.squadIterator = nil
        universe.processMapAIIterator = nil
        universe.processNestIterator = nil
        universe.vengenceQueue = {}
        universe.activeMap = nil
        universe.mapIterator = nil
        universe.builderCount = 0
        universe.squadCount = 0
        universe.chunkToNests = {}
        universe.processActiveSpawnerIterator = nil
        universe.processActiveRaidSpawnerIterator = nil
        universe.processMigrationIterator = nil
        universe.chunkToDrainedIterator = nil
        universe.chunkToActiveNest = {}
        universe.chunkToActiveRaidNest = {}
        universe.chunkToDrained = {}
        universe.chunkToRetreatIterator = nil
        universe.chunkToRetreats = {}
        universe.chunkToRallyIterator = nil
        universe.chunkToRallys = {}
        universe.chunkToPassScan = {}
        universe.chunkToPassScanIterator = nil

        game.print("Rampant - Version 2.0.0")
    end

    return (starting ~= global.version) and global.version
end

function upgrade.prepMap(universe, surface)
    local surfaceName = surface.name
    if sFind(surfaceName, "Factory floor") or
        sFind(surfaceName, " Orbit") or
        sFind(surfaceName, "clonespace") or
        sFind(surfaceName, "BPL_TheLabplayer") or
        sFind(surfaceName, "starmap-") or
        (surfaceName == "aai-signals") or
        sFind(surfaceName, "NiceFill") or
        sFind(surfaceName, "Asteroid Belt") or
        sFind(surfaceName, "Vault ")
    then
        return
    end

    game.print("Rampant - Indexing surface:" .. surface.name .. ", index:" .. tostring(surface.index) .. ", please wait.")

    local surfaceIndex = surface.index

    if not universe.maps then
        universe.maps = {}
    end

    local map = {}
    universe.maps[surfaceIndex] = map

    map.activatedMap = false

    map.maxAggressiveGroups = 1
    map.sentAggressiveGroups = 0
    map.processedChunks = 0
    map.processQueue = {}
    map.processIndex = 1
    map.scanPlayerIndex = 1
    map.scanResourceIndex = 1
    map.scanEnemyIndex = 1
    map.processStaticIndex = 1
    map.outgoingScanWave = true
    map.outgoingStaticScanWave = true

    map.chunkToBase = {}
    map.chunkToTurrets = {}
    map.chunkToTraps = {}
    map.chunkToUtilities = {}
    map.chunkToHives = {}
    map.chunkToNestIds = {}
    map.chunkToHiveIds = {}
    map.chunkToTrapIds = {}
    map.chunkToTurretIds = {}
    map.chunkToUtilityIds = {}

    map.chunkToPlayerBase = {}
    map.chunkToResource = {}
    map.chunkToPlayerCount = {}
    map.playerToChunk = {}

    map.chunkToSquad = {}

    map.chunkToPassable = {}
    map.chunkToPathRating = {}
    map.chunkToDeathGenerator = {}

    map.recycleBaseIterator = nil

    map.chunkScanCounts = {}

    map.chunkRemovals = {}

    map.emptySquadsOnChunk = {}

    map.surface = surface
    map.universe = universe

    map.bases = {}
    map.baseIndex = 1
    map.baseIncrement = 0
    map.points = 0
    map.state = constants.AI_STATE_AGGRESSIVE
    map.baseId = 0
    map.squads = nil
    map.pendingAttack = nil
    map.building = nil

    map.evolutionLevel = game.forces.enemy.evolution_factor
    map.canAttackTick = 0
    map.drainPylons = {}
    map.activeRaidNests = 0
    map.activeNests = 0
    map.destroyPlayerBuildings = 0
    map.lostEnemyUnits = 0
    map.lostEnemyBuilding = 0
    map.rocketLaunched = 0
    map.builtEnemyBuilding = 0
    map.ionCannonBlasts = 0
    map.artilleryBlasts = 0

    map.temperament = 0.5
    map.temperamentScore = 0
    map.stateTick = 0

    map.random = universe.random

    -- queue all current chunks that wont be generated during play
    local tick = game.tick
    for chunk in surface.get_chunks() do
        if surface.is_chunk_generated(chunk) then
            queueGeneratedChunk(universe,
                                {
                                    surface = surface,
                                    tick = tick,
                                    area = {
                                        left_top = {
                                            x = chunk.x * 32,
                                            y = chunk.y * 32
                                        }
                                    }
                                }
            )
        end
    end

    processPendingChunks(universe, tick, true)
end

function upgrade.compareTable(entities, option, new)
    local changed = false
    if (entities[option] ~= new) then
        entities[option] = new
        changed = true
    end
    return changed, new
end

return upgrade
