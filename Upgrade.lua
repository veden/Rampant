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
        universe.randomGenerator = game.create_random_generator(settings.startup["rampant--enemySeed"].value+1024)

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

        universe.builderCount = 0
        universe.squadCount = 0

        addCommandSet(universe)
    end
    if global.version < 116 then
        global.version = 116

        universe.maxPoints = 0

        -- if (universe.maps) then
        --     for _,map in pairs(universe.maps) do
        --         for _,base in pairs(map.bases) do
        --             base.damagedBy = {}
        --             base.deathEvents = 0
        --         end
        --     end
        -- end

        game.print("Rampant - Version 1.1.0")
    end

    return (starting ~= global.version) and global.version
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
