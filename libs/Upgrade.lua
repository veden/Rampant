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


local Upgrade = {}

-- imports

local Constants = require("libs/Constants")

--

local Universe

-- Constants

local BASE_PHEROMONE = Constants.BASE_PHEROMONE
local PLAYER_PHEROMONE = Constants.PLAYER_PHEROMONE
local RESOURCE_PHEROMONE = Constants.RESOURCE_PHEROMONE
local ENEMY_PHEROMONE = Constants.ENEMY_PHEROMONE
local KAMIKAZE_PHEROMONE = Constants.KAMIKAZE_PHEROMONE
local CHUNK_TICK = Constants.CHUNK_TICK

local MINIMUM_EXPANSION_DISTANCE = Constants.MINIMUM_EXPANSION_DISTANCE
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

local CHUNK_SIZE = Constants.CHUNK_SIZE
local TRIPLE_CHUNK_SIZE = Constants.TRIPLE_CHUNK_SIZE

local TICKS_A_MINUTE = Constants.TICKS_A_MINUTE

-- imported functions

-- module code

local function addCommandSet()
    -- preallocating memory to be used in code, making it fast by reducing garbage generated.
    Universe.playerForces = {}
    Universe.enemyForces = {}
    Universe.npcForces = {}
    Universe.nonPlayerForces = {}

    Universe.mapUtilsQueries = {}
    Universe.mapUtilsQueries.neighbors = {
            -1,
            -1,
            -1,
            -1,
            -1,
            -1,
            -1,
            -1
    }
    Universe.mapUtilsQueries.position = {0,0}

    Universe.chunkOverlapArray = {
            -1,
            -1,
            -1,
            -1
    }

    Universe.chunkPropertyUtilsQueries = {}
    Universe.chunkPropertyUtilsQueries.position = {0,0}

    -- pb
    Universe.pbFilteredEntitiesPointQueryLimited = {
        position = {0, 0},
        radius = 10,
        limit = 1,
        force = Universe.enemyForces,
        type = {
            "unit-spawner",
            "turret"
        }
    }

    -- msec
    Universe.msecFilteredEntitiesEnemyStructureQuery = {
        area={
            {0,0},
            {0,0}
        },
        force=Universe.enemyForces,
        type={
            "turret",
            "unit-spawner"
        }
    }

    -- oba
    Universe.obaCreateBuildCloudQuery = {
        name = "build-clear-cloud-rampant",
        position = {0,0}
    }

    -- sp
    local spbSharedChunkArea = {
        {0,0},
        {0,0}
    }
    Universe.spbHasPlayerStructuresQuery = {
        area=spbSharedChunkArea,
        force=Universe.nonPlayerForces,
        invert=true,
        limit=1
    }
    Universe.spbFilteredEntitiesPlayerQueryLowest = {
        area=spbSharedChunkArea,
        force=Universe.playerForces,
        collision_mask = "player-layer",
        type={
            "wall",
            "transport-belt"
        }
    }
    Universe.spbFilteredEntitiesPlayerQueryLow = {
        area=spbSharedChunkArea,
        force=Universe.playerForces,
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
    Universe.spbFilteredEntitiesPlayerQueryHigh = {
        area=spbSharedChunkArea,
        force=Universe.playerForces,
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
    Universe.spbFilteredEntitiesPlayerQueryHighest = {
        area=spbSharedChunkArea,
        force=Universe.playerForces,
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
    Universe.isFilteredTilesQuery = {
        collision_mask="water-tile",
        area=isSharedChunkArea
    }
    Universe.isFilteredEntitiesChunkNeutral = {
        area=isSharedChunkArea,
        collision_mask = "player-layer",
        type={
            "tree",
            "simple-entity"
        }
    }
    Universe.isFilteredEntitiesEnemyStructureQuery = {
        area=isSharedChunkArea,
        force=Universe.enemyForces,
        type={
            "turret",
            "unit-spawner"
        }
    }
    Universe.isCountResourcesQuery = {
        area=isSharedChunkArea,
        type="resource"
    }
    Universe.isFilteredEntitiesUnitQuery = {
        area=isSharedChunkArea,
        force=Universe.enemyForces,
        type="unit"
    }

    -- cps
    local cpsSharedChunkArea = {
        {0,0},
        {0,0}
    }
    Universe.cpsFilteredTilesQuery = {
        collision_mask="water-tile",
        area=cpsSharedChunkArea
    }
    Universe.cpsFilteredEntitiesChunkNeutral = {
        area=cpsSharedChunkArea,
        collision_mask = "player-layer",
        type={
            "tree",
            "simple-entity"
        }
    }
    Universe.cpsFilteredEnemyAnyFound = {
        area=cpsSharedChunkArea,
        force=Universe.enemyForces,
        type={
            "turret",
            "unit-spawner"
        },
        limit = 1
    }

    -- msrc
    local msrcSharedChunkArea = {
        {0,0},
        {0,0}
    }
    Universe.msrcFilteredTilesQuery = {
        collision_mask="water-tile",
        area=msrcSharedChunkArea
    }
    Universe.msrcFilteredEntitiesChunkNeutral = {
        area=msrcSharedChunkArea,
        collision_mask = "player-layer",
        type={
            "tree",
            "simple-entity"
        }
    }
    Universe.msrcCountResourcesQuery = {
        area=msrcSharedChunkArea,
        type="resource"
    }

    -- sp
    local spSharedAreaChunk = {
        {0,0},
        {0,0}
    }
    Universe.spFilteredEntitiesCliffQuery = {
        area=spSharedAreaChunk,
        type="cliff",
        limit = 1
    }
    Universe.spFilteredTilesPathQuery = {
        area=spSharedAreaChunk,
        collision_mask="water-tile",
        limit = 1
    }

    -- ouc
    Universe.oucCliffQuery = {
        area={
            {0,0},
            {0,0}
        },
        type="cliff"
    }

    -- ppu
    Universe.processorQueries = {}
    Universe.processorQueries.createEntityQuery = {
        name = "",
        position = {0,0},
        raise_built = true
    }
    Universe.processorQueries.targetPosition = {0,0}

    Universe.squadQueries = {}
    Universe.squadQueries.targetPosition = {0,0}
    Universe.squadQueries.createEntityQuery = {
        name = "",
        position = {0,0}
    }
    Universe.squadQueries.attackCommand = {
        type = DEFINES_COMMAND_ATTACK_AREA,
        destination = {0,0},
        radius = CHUNK_SIZE * 1.5,
        distraction = DEFINES_DISTRACTION_BY_ANYTHING
    }
    Universe.squadQueries.moveCommand = {
        type = DEFINES_COMMAND_GO_TO_LOCATION,
        destination = {0,0},
        pathfind_flags = { cache = true, prefer_straight_paths = true },
        distraction = DEFINES_DISTRACTION_BY_ENEMY,
        radius = 5
    }
    Universe.squadQueries.settleCommand = {
        type = DEFINES_COMMAND_BUILD_BASE,
        destination = {0,0},
        distraction = DEFINES_DISTRACTION_BY_ENEMY,
        ignore_planner = true
    }
    Universe.squadQueries.wanderCommand = {
        type = DEFINES_COMMAND_WANDER,
        wander_in_group = false,
        radius = TRIPLE_CHUNK_SIZE*2,
        ticks_to_wait = 20 * 60
    }
    Universe.squadQueries.wander2Command = {
        type = DEFINES_COMMAND_WANDER,
        wander_in_group = true,
        radius = TRIPLE_CHUNK_SIZE*2,
        ticks_to_wait = 2 * 60
    }
    Universe.squadQueries.compoundSettleCommand = {
        type = DEFINES_COMMMAD_COMPOUND,
        structure_type = DEFINES_COMPOUND_COMMAND_RETURN_LAST,
        commands = {
            Universe.squadQueries.settleCommand
        }
    }
    Universe.squadQueries.retreatCommand = {
        type = DEFINES_COMMAND_GROUP,
        group = nil,
        distraction = DEFINES_DISTRACTION_BY_ANYTHING,
        use_group_distraction = true
    }
    Universe.squadQueries.fleeCommand = {
        type = DEFINES_COMMAND_FLEE,
        from = nil,
        distraction = DEFINES_DISTRACTION_NONE
    }
    Universe.squadQueries.compoundRetreatGroupCommand = {
        type = DEFINES_COMMMAD_COMPOUND,
        structure_type = DEFINES_COMPOUND_COMMAND_RETURN_LAST,
        commands = {
            Universe.squadQueries.fleeCommand,
            Universe.squadQueries.retreatCommand
        }
    }
    Universe.squadQueries.formGroupCommand = {
        type = DEFINES_COMMAND_GROUP,
        group = nil,
        distraction = DEFINES_DISTRACTION_BY_ANYTHING,
        use_group_distraction = true
    }
    Universe.squadQueries.formCommand = {
        command = Universe.squadQueries.formGroupCommand,
        unit_count = 0,
        unit_search_distance = TRIPLE_CHUNK_SIZE
    }
    Universe.squadQueries.formRetreatCommand = {
        command = Universe.squadQueries.compoundRetreatGroupCommand,
        unit_count = 1,
        unit_search_distance = CHUNK_SIZE
    }
    Universe.squadQueries.searchPath = {
        {chunk=-1, direction=-1},
        {chunk=-1, direction=-1},
        {chunk=-1, direction=-1},
        {chunk=-1, direction=-1}
    }
    Universe.squadQueries.renderText = {
        target = nil,
        text = "",
        target_offset = {0, -10},
        color = {r=1,g=1,b=1,a=0.5},
        surface = nil,
        scale = 5
    }
    Universe.squadQueries.createUnitGroup = {
        position = {0,0}
    }
end

function Upgrade.setCommandForces(npcForces, enemyForces)
    for force in pairs(Universe.playerForces) do
        Universe.playerForces[force] = nil
    end
    for force in pairs(Universe.npcForces) do
        Universe.npcForces[force] = nil
    end
    for force in pairs(Universe.enemyForces) do
        Universe.enemyForces[force] = nil
    end
    for force in pairs(Universe.nonPlayerForces) do
        Universe.nonPlayerForces[force] = nil
    end
    for _,force in pairs(game.forces) do
        if not npcForces[force.name] and not enemyForces[force.name] then
            Universe.playerForces[#Universe.playerForces+1] = force.name
        end
    end
    for force in pairs(enemyForces) do
        Universe.enemyForces[#Universe.enemyForces+1] = force
        Universe.nonPlayerForces[#Universe.nonPlayerForces+1] = force
    end
    for force in pairs(npcForces) do
        Universe.npcForces[#Universe.npcForces+1] = force
        Universe.nonPlayerForces[#Universe.nonPlayerForces+1] = force
    end
end

function Upgrade.addUniverseProperties()
    if not global.universePropertyVersion or global.version then
        for key in pairs(global) do
            if key ~= "universe" then
                global[key] = nil
            end
        end

        for key in pairs(Universe) do
            Universe[key] = nil
        end

        global.universePropertyVersion = 0
    end

    if global.universePropertyVersion < 1 then
        global.universePropertyVersion = 1

        Universe.safeEntities = {}
        Universe.flushPendingChunks = false

        Universe.aiPointsScaler = settings.global["rampant--aiPointsScaler"].value

        Universe.aiPointsPrintGainsToChat = settings.global["rampant--aiPointsPrintGainsToChat"].value
        Universe.aiPointsPrintSpendingToChat = settings.global["rampant--aiPointsPrintSpendingToChat"].value

        Universe.aiNocturnalMode = settings.global["rampant--permanentNocturnal"].value

        Universe.retreatThreshold = 0
        Universe.rallyThreshold = 0
        Universe.formSquadThreshold = 0
        Universe.attackWaveSize = 0
        Universe.attackWaveDeviation = 0
        Universe.attackWaveUpperBound = 0
        Universe.unitRefundAmount = 0

        Universe.kamikazeThreshold = 0
        Universe.attackWaveLowerBound = 1

        Universe.settlerWaveDeviation = 0
        Universe.settlerWaveSize = 0

        Universe.enabledMigration = Universe.expansion and settings.global["rampant--enableMigration"].value
        Universe.peacefulAIToggle = settings.global["rampant--peacefulAIToggle"].value
        Universe.printAIStateChanges = settings.global["rampant--printAIStateChanges"].value
        Universe.debugTemperament = settings.global["rampant--debugTemperament"].value

        Universe.upgradeId = 0
        Universe.eventId = 0
        Universe.chunkId = 0
        Universe.maps = {}
        Universe.activeMaps = {}
        Universe.groupNumberToSquad = {}
        Universe.pendingChunks = {}
        Universe.squadIterator = nil
        Universe.processMapAIIterator = nil
        Universe.processNestIterator = nil
        Universe.vengenceQueue = {}
        Universe.currentMap = nil
        Universe.mapIterator = nil
        Universe.builderCount = 0
        Universe.squadCount = 0
        Universe.processActiveSpawnerIterator = nil
        Universe.processActiveRaidSpawnerIterator = nil
        Universe.processMigrationIterator = nil

        Universe.chunkToNests = {}
        Universe.chunkToHives = {}
        Universe.chunkToUtilities = {}
        Universe.chunkToVictory = {}
        Universe.chunkToActiveNest = {}
        Universe.chunkToActiveRaidNest = {}
        Universe.chunkToDrained = {}
        Universe.chunkToRetreats = {}
        Universe.chunkToRallys = {}
        Universe.chunkToPassScan = {}

        Universe.hiveData = {}
        Universe.hives = {}
        Universe.activeHives = {}
        Universe.hiveIterator = nil

        Universe.baseId = 0
        Universe.awake = false

        Universe.recycleBaseIterator = nil

        Universe.maxPoints = 0
        Universe.maxOverflowPoints = 0

        Universe.bases = {}

        Universe.processBaseAIIterator = nil

        Universe.excludedSurfaces = {}

        Universe.pendingUpgrades = {}
        Universe.pendingUpgradesLength = 0
        Universe.settlePurpleCloud = {}
    end
    if global.universePropertyVersion < 2 then
        global.universePropertyVersion = 2
        Universe.hiveDataIterator = nil
    end
    if global.universePropertyVersion < 3 then
        global.universePropertyVersion = 3
        addCommandSet()
    end
end

function Upgrade.attempt()
    if not global.gameVersion then
        global.gameVersion = 1

        Universe.evolutionLevel = game.forces.enemy.evolution_factor

        Universe.random = game.create_random_generator(Constants.ENEMY_SEED)

        local minDiffuse = game.map_settings.pollution.min_to_diffuse
        Universe.pollutionDiffuseMinimum = minDiffuse * 0.75

        Universe.expansion = game.map_settings.enemy_expansion.enabled
        Universe.expansionMaxDistance = game.map_settings.enemy_expansion.max_expansion_distance * CHUNK_SIZE
        Universe.expansionMinTime = game.map_settings.enemy_expansion.min_expansion_cooldown / TICKS_A_MINUTE
        Universe.expansionMaxTime = game.map_settings.enemy_expansion.max_expansion_cooldown / TICKS_A_MINUTE
        Universe.expansionMinSize = game.map_settings.enemy_expansion.settler_group_min_size
        Universe.expansionMaxSize = game.map_settings.enemy_expansion.settler_group_max_size

        Universe.expansionLowTargetDistance = (Universe.expansionMaxDistance + MINIMUM_EXPANSION_DISTANCE) * 0.33
        Universe.expansionMediumTargetDistance = (Universe.expansionMaxDistance + MINIMUM_EXPANSION_DISTANCE) * 0.50
        Universe.expansionHighTargetDistance = (Universe.expansionMaxDistance + MINIMUM_EXPANSION_DISTANCE) * 0.75
        Universe.expansionDistanceDeviation = Universe.expansionMediumTargetDistance * 0.33

        Universe.modAddedTick = game.tick
    end
    if global.gameVersion < 2 then
        global.gameVersion = 2
        game.map_settings.max_failed_behavior_count = 999
        for _, squad in pairs(Universe.groupNumberToSquad) do
            squad.canBeCompressed = 0
            squad.retreats = 0
        end

        game.forces.enemy.kill_all_units()

        for _, map in pairs(Universe.maps) do
            for _, chunk in pairs(map.processQueue) do
                chunk[CHUNK_TICK] = 0
                chunk[BASE_PHEROMONE] = 0
                chunk[PLAYER_PHEROMONE] = 0
                chunk[RESOURCE_PHEROMONE] = 0
                chunk[ENEMY_PHEROMONE] = 0
                chunk[KAMIKAZE_PHEROMONE] = 0
            end
        end
    end
    if global.gameVersion < 3 then
        global.gameVersion = 3

        local lookup = {}
        for _, map in pairs(Universe.maps) do
            local entities = map.surface.find_entities_filtered({
                    type = {"unit-spawner", "turret"},
                    force = "enemy"
            })
            for i = 1, #entities do
                local entity = entities[i]
                lookup[entity.unit_number] = entity
            end
        end
        for entityId, hiveData in pairs(Universe.activeHives) do
            if not lookup[entityId] then
                Universe.activeHives[entityId] = nil
            else
                Universe.hives[entityId] = hiveData
            end
        end
        for entityId, hiveData in pairs(Universe.hives) do
            if not lookup[entityId] then
                Universe.hives[entityId] = nil
            else
                hiveData.e = lookup[entityId]
                hiveData.position = hiveData.e.position
            end
        end
        for entityId, hiveData in pairs(Universe.hiveData) do
            if not lookup[entityId] then
                Universe.hiveData[entityId] = nil
            else
                if not Universe.hives[hiveData.hiveId] then
                    Universe.hiveData[entityId] = nil
                elseif not hiveData.e then
                    Universe.hiveData[entityId] = nil
                end
            end
        end

	Universe.hiveIterator = nil
	Universe.hiveDataIterator = nil
    end
end

function Upgrade.init(universe)
    Universe = universe
end

return Upgrade
