local upgrade = {}

-- imports

local constants = require("libs/Constants")

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
    queriesAndCommands.position = {
        x=0,
        y=0
    }
    queriesAndCommands.position2 = {
        x=0,
        y=0
    }
    queriesAndCommands.position3 = {
        x=0,
        y=0
    }

    queriesAndCommands.chunkOverlapArray = {
            -1,
            -1,
            -1,
            -1
    }

    queriesAndCommands.position2Top = {0, 0}
    queriesAndCommands.position2Bottom = {0, 0}
    --this is shared between two different queries
    queriesAndCommands.area = {
        {0, 0},
        {0, 0}
    }
    queriesAndCommands.area2 = {
        queriesAndCommands.position2Top,
        queriesAndCommands.position2Bottom
    }
    queriesAndCommands.buildPositionTop = {0, 0}
    queriesAndCommands.buildPositionBottom = {0, 0}
    queriesAndCommands.buildArea = {
        queriesAndCommands.buildPositionTop,
        queriesAndCommands.buildPositionBottom
    }
    queriesAndCommands.countResourcesQuery = {
        area=queriesAndCommands.area,
        type="resource"
    }
    queriesAndCommands.filteredEntitiesUnitQuery = {
        area=queriesAndCommands.area,
        force="enemy",
        type="unit"
    }
    queriesAndCommands.hasPlayerStructuresQuery = {
        area=queriesAndCommands.area,
        force={"enemy","neutral"},
        invert=true,
        limit=1
    }
    queriesAndCommands.filteredEntitiesEnemyStructureQuery = {
        area=queriesAndCommands.area,
        force="enemy",
        type={
            "turret",
            "unit-spawner"
        }
    }
    queriesAndCommands.filteredEntitiesPointQueryLimited = {
        position = queriesAndCommands.position,
        radius = 10,
        limit = 1,
        force = "enemy",
        type = {
            "unit-spawner",
            "turret"
        }
    }
    queriesAndCommands.createBuildCloudQuery = {
        name = "build-clear-cloud-rampant",
        position = queriesAndCommands.position
    }

    queriesAndCommands.activePlayerForces = {"player"}

    for _,force in pairs(game.forces) do
        local add = true

        if (force.name ~= "neutral") and (force.name ~= "enemy") then
            for i=1,#queriesAndCommands.activePlayerForces do
                if (queriesAndCommands.activePlayerForces[i] == force.name) then
                    add = false
                    break
                end
            end

            if add then
                queriesAndCommands.activePlayerForces[#queriesAndCommands.activePlayerForces+1] = force.name
            end
        end
    end

    queriesAndCommands.filteredEntitiesPlayerQueryLowest = {
        area=queriesAndCommands.area,
        force=queriesAndCommands.activePlayerForces,
        collision_mask = "player-layer",
        type={
            "wall",
            "transport-belt"
        }
    }

    queriesAndCommands.filteredEntitiesPlayerQueryLow = {
        area=queriesAndCommands.area,
        force=queriesAndCommands.activePlayerForces,
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

    queriesAndCommands.filteredEntitiesPlayerQueryHigh = {
        area=queriesAndCommands.area,
        force=queriesAndCommands.activePlayerForces,
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

    queriesAndCommands.filteredEntitiesPlayerQueryHighest = {
        area=queriesAndCommands.area,
        force=queriesAndCommands.activePlayerForces,
        collision_mask = "player-layer",
        type={
            "artillery-turret",
            "reactor",
            "rocket-silo"
        }
    }

    queriesAndCommands.filteredEntitiesChunkNeutral = {
        area=queriesAndCommands.area,
        collision_mask = "player-layer",
        type={
            "tree",
            "simple-entity"
        }
    }

    local sharedArea = {
        {0,0},
        {0,0}
    }
    queriesAndCommands.filteredEntitiesCliffQuery = {
        area=sharedArea,
        type="cliff",
        limit = 1
    }
    queriesAndCommands.filteredTilesPathQuery = {
        area=sharedArea,
        collision_mask="water-tile",
        limit = 1
    }
    queriesAndCommands.cliffQuery = {
        area=queriesAndCommands.area2,
        type="cliff"
    }
    queriesAndCommands.canPlaceQuery = {
        name="",
        position={0,0}
    }
    queriesAndCommands.filteredTilesQuery = {
        collision_mask="water-tile",
        area=queriesAndCommands.area
    }

    queriesAndCommands.upgradeEntityQuery = {
        name = "",
        position = nil
    }

    queriesAndCommands.attackCommand = {
        type = DEFINES_COMMAND_ATTACK_AREA,
        destination = queriesAndCommands.position,
        radius = CHUNK_SIZE * 1.5,
        distraction = DEFINES_DISTRACTION_BY_ANYTHING
    }

    queriesAndCommands.moveCommand = {
        type = DEFINES_COMMAND_GO_TO_LOCATION,
        destination = queriesAndCommands.position,
        pathfind_flags = { cache = true },
        distraction = DEFINES_DISTRACTION_BY_ENEMY
    }

    queriesAndCommands.settleCommand = {
        type = DEFINES_COMMAND_BUILD_BASE,
        destination = queriesAndCommands.position,
        distraction = DEFINES_DISTRACTION_BY_ENEMY,
        ignore_planner = true
    }

    queriesAndCommands.wonderCommand = {
        type = DEFINES_COMMAND_WANDER,
        wander_in_group = false,
        radius = TRIPLE_CHUNK_SIZE*2,
        ticks_to_wait = 36000
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
end

function upgrade.attempt(natives, queriesAndCommands)
    local starting = global.version
    if not global.version or global.version < 106 then
        global.version = 106
        game.forces.enemy.kill_all_units()
        natives.points = 0
        natives.state = constants.AI_STATE_AGGRESSIVE

        natives.safeEntities = {}
        natives.vengenceQueue = {}

        natives.aiPointsScaler = settings.global["rampant-aiPointsScaler"].value
        natives.aiNocturnalMode = settings.global["rampant-permanentNocturnal"].value

        -- needs to be on inner logic tick loop interval
        -- natives.stateTick = roundToNearest(game.tick + INTERVAL_LOGIC, INTERVAL_LOGIC)
        -- natives.temperamentTick = roundToNearest(game.tick + INTERVAL_LOGIC, INTERVAL_LOGIC)

        -- used to precompute some values per logic cycle
        natives.retreatThreshold = 0
        natives.rallyThreshold = 0
        natives.formSquadThreshold = 0
        natives.attackWaveSize = 0
        natives.attackWaveDeviation = 0
        natives.attackWaveUpperBound = 0
        natives.unitRefundAmount = 0
        natives.regroupIndex = 1
        natives.randomGenerator = game.create_random_generator(settings.startup["rampant-enemySeed"].value+1024)

        game.map_settings.path_finder.min_steps_to_check_path_find_termination =
            constants.PATH_FINDER_MIN_STEPS_TO_CHECK_PATH

        natives.evolutionTableAlignment = {}
        natives.bases = {}
        natives.baseIndex = 1
        natives.baseIncrement = 0

        natives.kamikazeThreshold = 0
        natives.attackWaveLowerBound = 1

        natives.expansion = game.map_settings.enemy_expansion.enabled
        natives.expansionMaxDistance = game.map_settings.enemy_expansion.max_expansion_distance * CHUNK_SIZE
        natives.expansionMaxDistanceDerivation = natives.expansionMaxDistance * 0.33
        natives.expansionMinTime = game.map_settings.enemy_expansion.min_expansion_cooldown
        natives.expansionMaxTime = game.map_settings.enemy_expansion.max_expansion_cooldown
        natives.expansionMinSize = game.map_settings.enemy_expansion.settler_group_min_size
        natives.expansionMaxSize = game.map_settings.enemy_expansion.settler_group_max_size

        natives.settlerCooldown = 0
        natives.settlerWaveDeviation = 0
        natives.settlerWaveSize = 0

        natives.drainPylons = {}

        natives.groupNumberToSquad = {}

        natives.enabledMigration = natives.expansion and settings.global["rampant-enableMigration"].value

        natives.enemyAlignmentLookup = {}

        natives.canAttackTick = 0

        game.map_settings.unit_group.min_group_radius = constants.UNIT_GROUP_MAX_RADIUS * 0.5
        game.map_settings.unit_group.max_group_radius = constants.UNIT_GROUP_MAX_RADIUS

        game.map_settings.unit_group.max_member_speedup_when_behind = constants.UNIT_GROUP_MAX_SPEED_UP
        game.map_settings.unit_group.max_member_slowdown_when_ahead = constants.UNIT_GROUP_MAX_SLOWDOWN
        game.map_settings.unit_group.max_group_slowdown_factor = constants.UNIT_GROUP_SLOWDOWN_FACTOR

        game.map_settings.max_failed_behavior_count = 3

        natives.ENEMY_VARIATIONS = settings.startup["rampant-newEnemyVariations"].value

        natives.evolutionLevel = game.forces.enemy.evolution_factor

        natives.activeRaidNests = 0
        natives.activeNests = 0
        natives.destroyPlayerBuildings = 0
        natives.lostEnemyUnits = 0
        natives.lostEnemyBuilding = 0
        natives.rocketLaunched = 0
        natives.builtEnemyBuilding = 0
        natives.ionCannonBlasts = 0
        natives.artilleryBlasts = 0

        natives.temperament = 0
        natives.temperamentScore = 0
        natives.stateTick = 0
    end
    if (global.version < 111) then
        global.version = 111

        natives.groupNumberToSquad = {}
        game.forces.enemy.kill_all_units()
        natives.squads = nil
        natives.pendingAttack = nil
        natives.building = nil
    end
    if (global.version < 113) then
        global.version = 113

        natives.baseId = 0

        local newBases = {}
        for _=1,#natives.bases do
            local base = natives.bases
            base.id = natives.baseId
            newBases[base.id] = base
            natives.baseId = natives.baseId + 1
        end
        natives.bases = newBases
        global.pendingChunks = nil

        game.map_settings.unit_group.member_disown_distance = 10
        game.map_settings.unit_group.tick_tolerance_when_member_arrives = 60

        natives.vengenceQueue = {}
        natives.builderCount = 0
        natives.squadCount = 0

        game.forces.enemy.ai_controllable = true

        addCommandSet(queriesAndCommands)

        game.print("Rampant - Version 1.0.3")
    end

    return starting ~= global.version
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
