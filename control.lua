-- imports

local unitUtils = require("libs/UnitUtils")
local baseUtils = require("libs/BaseUtils")
local mapUtils = require("libs/MapUtils")
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

-- constants

local TRIPLE_CHUNK_SIZE = constants.TRIPLE_CHUNK_SIZE
local INTERVAL_LOGIC = constants.INTERVAL_LOGIC
local INTERVAL_PLAYER_PROCESS = constants.INTERVAL_PLAYER_PROCESS
local INTERVAL_MAP_PROCESS = constants.INTERVAL_MAP_PROCESS
local INTERVAL_SCAN = constants.INTERVAL_SCAN
local INTERVAL_SQUAD = constants.INTERVAL_SQUAD

local RECOVER_NEST_COST = constants.RECOVER_NEST_COST
local RECOVER_WORM_COST = constants.RECOVER_WORM_COST

local DOUBLE_CHUNK_SIZE = constants.DOUBLE_CHUNK_SIZE

local PROCESS_QUEUE_SIZE = constants.PROCESS_QUEUE_SIZE

local WATER_TILE_NAMES = constants.WATER_TILE_NAMES

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

local AI_MAX_OVERFLOW_POINTS = constants.AI_MAX_OVERFLOW_POINTS

local RETREAT_GRAB_RADIUS = constants.RETREAT_GRAB_RADIUS

local RETREAT_SPAWNER_GRAB_RADIUS = constants.RETREAT_SPAWNER_GRAB_RADIUS

local DEFINES_COMMAND_GROUP = defines.command.group
local DEFINES_COMMAND_WANDER = defines.command.wander
local DEFINES_COMMAND_BUILD_BASE = defines.command.build_base
local DEFINES_COMMAND_ATTACK_AREA = defines.command.attack_area
local DEFINES_COMMAND_GO_TO_LOCATION = defines.command.go_to_location
local DEFINES_COMMMAD_COMPOUND = defines.command.compound
local DEFINES_COMMAND_FLEE = defines.command.flee

local DEFINES_COMPOUND_COMMAND_RETURN_LAST = defines.compound_command.return_last

local CHUNK_SIZE = constants.CHUNK_SIZE

local DEFINES_DISTRACTION_NONE = defines.distraction.none
local DEFINES_DISTRACTION_BY_ENEMY = defines.distraction.by_enemy
local DEFINES_DISTRACTION_BY_ANYTHING = defines.distraction.by_anything

local DEFINES_WIRE_TYPE_RED = defines.wire_type.red
local DEFINES_WIRE_TYPE_GREEN = defines.wire_type.green

local ENERGY_THIEF_CONVERSION_TABLE = constants.ENERGY_THIEF_CONVERSION_TABLE
local ENERGY_THIEF_LOOKUP = constants.ENERGY_THIEF_LOOKUP

local POISON_LOOKUP = constants.POISON_LOOKUP

-- imported functions

local convertTypeToDrainCrystal = unitUtils.convertTypeToDrainCrystal

local squadsDispatch = squadAttack.squadsDispatch

local positionToChunkXY = mapUtils.positionToChunkXY


local getChunkByPosition = mapUtils.getChunkByPosition

local entityForPassScan = chunkUtils.entityForPassScan

local processPendingChunks = chunkProcessor.processPendingChunks
local processScanChunks = chunkProcessor.processScanChunks

local processMap = mapProcessor.processMap
local processPlayers = mapProcessor.processPlayers
local scanMap = mapProcessor.scanMap

local planning = aiPlanning.planning

local rallyUnits = aiAttackWave.rallyUnits

local recycleBases = baseUtils.recycleBases

local deathScent = pheromoneUtils.deathScent
local victoryScent = pheromoneUtils.victoryScent

local cleanBuilders = unitGroupUtils.cleanBuilders
local regroupSquads = unitGroupUtils.regroupSquads
local convertUnitGroupToSquad = unitGroupUtils.convertUnitGroupToSquad

local createBase = baseUtils.createBase
local findNearbyBase = baseUtils.findNearbyBase

local squadsBeginAttack = squadAttack.squadsBeginAttack

local retreatUnits = squadDefense.retreatUnits

local accountPlayerEntity = chunkUtils.accountPlayerEntity
local unregisterEnemyBaseStructure = chunkUtils.unregisterEnemyBaseStructure
local registerEnemyBaseStructure = chunkUtils.registerEnemyBaseStructure
local makeImmortalEntity = chunkUtils.makeImmortalEntity

local upgradeEntity = baseUtils.upgradeEntity
local rebuildNativeTables = baseUtils.rebuildNativeTables

local mRandom = math.random

-- local references to global

local map -- manages the chunks that make up the game world
local natives -- manages the enemy units, structures, and ai
local pendingChunks -- chunks that have yet to be processed by the mod

-- hook functions

local function onIonCannonFired(event)
    --[[
	event.force, event.surface, event.player_index, event.position, event.radius
    --]]
    local surface = event.surface
    if (surface.index == natives.activeSurface) then
	natives.points = natives.points + 3000
	if (natives.points > AI_MAX_OVERFLOW_POINTS) then
	    natives.points = AI_MAX_OVERFLOW_POINTS
	end
	local chunk = getChunkByPosition(map, event.position)
	if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then
	    rallyUnits(chunk, map, surface, natives, event.tick)
	end
    end
end

local function hookEvents()
    if config.ionCannonPresent then
	script.on_event(remote.call("orbital_ion_cannon", "on_ion_cannon_fired"),
			onIonCannonFired)
	-- script.on_event(remote.call("orbital_ion_cannon", "on_ion_cannon_targeted"),
	-- 		    onIonCannonTargeted)
    end
end

local function onLoad()
    map = global.map
    natives = global.natives
    pendingChunks = global.pendingChunks

    --    print(serpent.dump(global))
    -- print(serpent.dump(map.chunkToSquad))
    -- print(serpent.dump(natives))
    -- print(serpent.dump(natives.squads))
    -- print(serpent.dump(natives.bases))

    hookEvents()
end

local function onChunkGenerated(event)
    -- queue generated chunk for delayed processing, queuing is required because some mods (RSO) mess with chunk as they
    -- are generated, which messes up the scoring.
    if (event.surface.index == natives.activeSurface) then
        pendingChunks[#pendingChunks+1] = event
    end
end

local function rebuildMap()
    game.surfaces[natives.activeSurface].print("Rampant - Reindexing chunks, please wait.")
    -- clear old map processing Queue
    -- prevents queue adding duplicate chunks
    -- chunks are by key, so should overwrite old

    -- game.forces.enemy.kill_all_units()

    global.map = {}
    map = global.map
    map.processQueue = {}
    map.processIndex = 1
    map.scanIndex = 1

    map.chunkToBase = {}
    map.chunkToNests = {}
    map.chunkToWorms = {}
    map.chunkToPlayerBase = {}
    map.chunkToResource = {}

    map.chunkToPassScan = {}
    map.chunkToSquad = {}

    map.chunkToRetreats = {}
    map.chunkToRallys = {}
    map.chunkToSpawner = {}
    map.chunkToSettler = {}

    map.chunkToPassable = {}
    map.chunkToPathRating = {}
    map.chunkToDeathGenerator = {}
    map.chunkToDrained = {}
    map.chunkToActiveNest = {}
    map.chunkToActiveRaidNest = {}

    -- map.queueSpawners = {}

    -- preallocating memory to be used in code, making it fast by reducing garbage generated.
    map.neighbors = {
        SENTINEL_IMPASSABLE_CHUNK,
        SENTINEL_IMPASSABLE_CHUNK,
        SENTINEL_IMPASSABLE_CHUNK,
        SENTINEL_IMPASSABLE_CHUNK,
        SENTINEL_IMPASSABLE_CHUNK,
        SENTINEL_IMPASSABLE_CHUNK,
        SENTINEL_IMPASSABLE_CHUNK,
        SENTINEL_IMPASSABLE_CHUNK
    }
    map.cardinalNeighbors = {
        SENTINEL_IMPASSABLE_CHUNK,
        SENTINEL_IMPASSABLE_CHUNK,
        SENTINEL_IMPASSABLE_CHUNK,
        SENTINEL_IMPASSABLE_CHUNK
    }
    map.position = {
        x=0,
        y=0
    }

    map.scentStaging = {}

    for x=1,PROCESS_QUEUE_SIZE+1 do
	map.scentStaging[x] = {0,0,0,0}
    end

    map.chunkOverlapArray = {
        SENTINEL_IMPASSABLE_CHUNK,
        SENTINEL_IMPASSABLE_CHUNK,
        SENTINEL_IMPASSABLE_CHUNK,
        SENTINEL_IMPASSABLE_CHUNK
    }

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
    map.filteredEntitiesUnitSpawnereQuery = { area=map.area, force="enemy", type="unit-spawner" }
    map.filteredEntitiesWormQuery = { area=map.area, force="enemy", type="turret" }
    map.filteredEntitiesSpawnerQueryLimited = { area=map.area2, force="enemy", type="unit-spawner" }
    map.filteredEntitiesWormQueryLimited = { area=map.area2, force="enemy", type="turret" }
    map.filteredEntitiesPlayerQuery = { area=map.area, force={"enemy", "neutral"}, invert = true }
    local sharedArea = {{0,0},{0,0}}
    map.filteredEntitiesCliffQuery = { area=sharedArea, type="cliff", limit = 1 }
    map.filteredTilesPathQuery = { area=sharedArea, collision_mask="water-tile", limit = 1 }
    map.canPlaceQuery = { name="", position={0,0} }
    map.filteredTilesQuery = { collision_mask="water-tile", area=map.area }

    map.attackCommand = {
	type = DEFINES_COMMAND_ATTACK_AREA,
	destination = map.position,
	radius = CHUNK_SIZE,
	distraction = DEFINES_DISTRACTION_BY_ANYTHING
    }

    map.moveCommand = {
	type = DEFINES_COMMAND_GO_TO_LOCATION,
	destination = map.position,
	radius = 2,
        pathfind_flags = { prefer_straight_paths = true },
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
        radius = DOUBLE_CHUNK_SIZE,
        ticks_to_wait = 360
    }

    map.compoundSettleCommand = {
        type = DEFINES_COMMMAD_COMPOUND,
        structure_type = DEFINES_COMPOUND_COMMAND_RETURN_LAST,
        commands = {
            map.wonderCommand,
            map.settleCommand
        }
    }

    map.retreatCommand = {
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
                             distraction = DEFINES_DISTRACTION_NONE }

    map.formCommand = { command = map.formGroupCommand,
                        unit_count = 0,
                        unit_search_distance = TRIPLE_CHUNK_SIZE }
end


local function onModSettingsChange(event)

    if event and ((string.sub(event.setting, 1, 7) ~= "rampant") or (string.sub(event.setting, 1, 18) == "rampant-arsenal")) then
	return false
    end

    upgrade.compareTable(natives, "safeBuildings", settings.global["rampant-safeBuildings"].value)

    upgrade.compareTable(natives.safeEntities, "curved-rail", settings.global["rampant-safeBuildings-curvedRail"].value)
    upgrade.compareTable(natives.safeEntities, "straight-rail", settings.global["rampant-safeBuildings-straightRail"].value)
    upgrade.compareTable(natives.safeEntities, "rail-signal", settings.global["rampant-safeBuildings-railSignals"].value)
    upgrade.compareTable(natives.safeEntities, "rail-chain-signal", settings.global["rampant-safeBuildings-railChainSignals"].value)
    upgrade.compareTable(natives.safeEntities, "train-stop", settings.global["rampant-safeBuildings-trainStops"].value)
    upgrade.compareTable(natives.safeEntities, "lamp", settings.global["rampant-safeBuildings-lamps"].value)

    local changed, newValue = upgrade.compareTable(natives.safeEntityName,
						   "big-electric-pole",
						   settings.global["rampant-safeBuildings-bigElectricPole"].value)
    if changed then
	natives.safeEntityName["big-electric-pole"] = newValue
	natives.safeEntityName["big-electric-pole-2"] = newValue
	natives.safeEntityName["big-electric-pole-3"] = newValue
	natives.safeEntityName["big-electric-pole-4"] = newValue
        natives.safeEntityName["lighted-big-electric-pole-4"] = newValue
        natives.safeEntityName["lighted-big-electric-pole-3"] = newValue
        natives.safeEntityName["lighted-big-electric-pole-2"] = newValue
        natives.safeEntityName["lighted-big-electric-pole"] = newValue
    end

    upgrade.compareTable(natives, "attackUsePlayer", settings.global["rampant-attackWaveGenerationUsePlayerProximity"].value)

    upgrade.compareTable(natives, "deadZoneFrequency", settings.global["rampant-deadZoneFrequency"].value)
    upgrade.compareTable(natives, "raidAIToggle", settings.global["rampant-raidAIToggle"].value)
    upgrade.compareTable(natives, "siegeAIToggle", settings.global["rampant-siegeAIToggle"].value)
    upgrade.compareTable(natives, "onslaughtAIToggle", settings.global["rampant-onslaughtAIToggle"].value)

    upgrade.compareTable(natives, "attackWaveMaxSize", settings.global["rampant-attackWaveMaxSize"].value)
    upgrade.compareTable(natives, "attackPlayerThreshold", settings.global["rampant-attackPlayerThreshold"].value)
    upgrade.compareTable(natives, "aiNocturnalMode", settings.global["rampant-permanentNocturnal"].value)
    upgrade.compareTable(natives, "aiPointsScaler", settings.global["rampant-aiPointsScaler"].value)

    upgrade.compareTable(natives, "aiAggressiveness", settings.global["rampant-aiAggressiveness"].value)

    upgrade.compareTable(natives, "newEnemies", settings.startup["rampant-newEnemies"].value)
    upgrade.compareTable(natives, "enemySeed", settings.startup["rampant-enemySeed"].value)

    upgrade.compareTable(natives, "disableVanillaAI", settings.global["rampant-disableVanillaAI"].value)

    natives.enabledMigration = natives.expansion and settings.global["rampant-enableMigration"].value

    game.forces.enemy.ai_controllable = not natives.disableVanillaAI

    return true
end

local function prepWorld(rebuild)
    local upgraded

    if (game.surfaces["battle_surface_2"] ~= nil) then
	natives.activeSurface = game.surfaces["battle_surface_2"].index
    elseif (game.surfaces["battle_surface_1"] ~= nil) then
	natives.activeSurface = game.surfaces["battle_surface_1"].index
    else
	natives.activeSurface = game.surfaces["nauvis"].index
    end

    upgraded, natives = upgrade.attempt(natives)
    onModSettingsChange(nil)
    if natives.newEnemies then
	rebuildNativeTables(natives, game.surfaces[natives.activeSurface], game.create_random_generator(natives.enemySeed))
    end
    if upgraded then
	rebuildMap()

	-- clear pending chunks, will be added when loop runs below
	global.pendingChunks = {}
	pendingChunks = global.pendingChunks

	-- queue all current chunks that wont be generated during play
	local surface = game.surfaces[natives.activeSurface]
	local tick = game.tick
        natives.nextChunkSort = 0
	for chunk in surface.get_chunks() do
	    onChunkGenerated({ tick = tick,
			       surface = surface,
			       area = { left_top = { x = chunk.x * 32,
						     y = chunk.y * 32 }}})
	end

        if natives.newEnemies and rebuild then
            game.forces.enemy.kill_all_units()
        end

	processPendingChunks(natives, map, surface, pendingChunks, tick, game.forces.enemy.evolution_factor, rebuild)
    end
end

local function onConfigChanged()
    prepWorld(true)
end

script.on_nth_tick(INTERVAL_PLAYER_PROCESS,
		   function (event)

		       local gameRef = game
		       local surface = gameRef.surfaces[natives.activeSurface]

		       processPlayers(gameRef.players,
                                      map,
                                      surface,
                                      natives,
                                      event.tick)
end)

script.on_nth_tick(INTERVAL_MAP_PROCESS,
		   function (event)

		       local gameRef = game
		       local surface = gameRef.surfaces[natives.activeSurface]

		       processMap(map,
                                  surface,
                                  natives,
                                  event.tick,
                                  gameRef.forces.enemy.evolution_factor)
end)


script.on_nth_tick(INTERVAL_SCAN,
		   function (event)
		       local tick = event.tick
		       local gameRef = game
		       local surface = gameRef.surfaces[natives.activeSurface]

		       processPendingChunks(natives, map, surface, pendingChunks, tick, gameRef.forces.enemy.evolution_factor)

		       scanMap(map, surface, natives, tick)

		       map.chunkToPassScan = processScanChunks(map, surface)
end)

script.on_nth_tick(INTERVAL_LOGIC,
		   function (event)
		       local tick = event.tick

		       planning(natives,
				game.forces.enemy.evolution_factor,
				tick)

		       if natives.newEnemies then
			   recycleBases(natives, tick)
		       end
end)

script.on_nth_tick(INTERVAL_SQUAD,
		   function ()
                       local surface = game.surfaces[natives.activeSurface]
		       squadsBeginAttack(natives)
		       squadsDispatch(map, surface, natives)

                       regroupSquads(natives, map)

                       cleanBuilders(map, natives, surface)
end)

local function onBuild(event)
    local entity = event.created_entity
    if (entity.surface.index == natives.activeSurface) then
	accountPlayerEntity(map, entity, natives, true, false)
	if natives.safeBuildings then
	    if natives.safeEntities[entity.type] or natives.safeEntityName[entity.name] then
		entity.destructible = false
	    end
	end
    end
end

local function onMine(event)
    local entity = event.entity
    local surface = entity.surface
    if (surface.index == natives.activeSurface) then
	accountPlayerEntity(map, entity, natives, false, false)
    end
end

local function onDeath(event)
    local entity = event.entity
    local surface = entity.surface
    if (surface.index == natives.activeSurface) then
	local entityPosition = entity.position
	local chunk = getChunkByPosition(map, entityPosition)
	local cause = event.cause
        local tick = event.tick
        if (entity.force.name == "enemy") then

            local artilleryBlast = (cause and ((cause.type == "artillery-wagon") or (cause.type == "artillery-turret")))

            if (entity.type == "unit") then
		if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then
		    -- drop death pheromone where unit died
		    deathScent(map, chunk)

		    if event.force and (event.force.name ~= "enemy") and (chunk[MOVEMENT_PHEROMONE] < -natives.retreatThreshold) then

			retreatUnits(chunk,
				     entityPosition,
				     convertUnitGroupToSquad(natives, entity.unit_group),
				     map,
				     surface,
				     natives,
				     tick,
				     (artilleryBlast and RETREAT_SPAWNER_GRAB_RADIUS) or RETREAT_GRAB_RADIUS,
				     artilleryBlast)

			if (mRandom() < natives.rallyThreshold) and not surface.peaceful_mode then
			    rallyUnits(chunk, map, surface, natives, tick)
			end
		    end
                end

                local cloudName = POISON_LOOKUP[entity.name]

                if cloudName then
                    surface.create_entity({position=entity.position,
                                           name=cloudName})
                end

            elseif event.force and (event.force.name ~= "enemy") and ((entity.type == "unit-spawner") or (entity.type == "turret")) then

                natives.points = natives.points + (((entity.type == "unit-spawner") and RECOVER_NEST_COST) or RECOVER_WORM_COST)
                if (natives.points > AI_MAX_OVERFLOW_POINTS) then
                    natives.points = AI_MAX_OVERFLOW_POINTS
                end

		if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then
		    unregisterEnemyBaseStructure(map, entity)

		    rallyUnits(chunk, map, surface, natives, tick)

		    retreatUnits(chunk,
				 entityPosition,
				 nil,
				 map,
				 surface,
				 natives,
				 tick,
				 RETREAT_SPAWNER_GRAB_RADIUS,
				 (cause and ((cause.type == "artillery-wagon") or (cause.type == "artillery-turret"))))
		end
            end

            -- if (cause and not artilleryBlast) then
            --     local causeChunk = getChunkByPosition(map, cause.position)
            --     if (causeChunk ~= SENTINEL_IMPASSABLE_CHUNK) and
            --         (causeChunk ~= chunk)
            --     then
            --         deathScent(map, causeChunk)
            --     end
            -- end

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
		if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then
		    victoryScent(map, chunk, entity.type)
		end

                local drained = (entity.type == "electric-turret") and map.chunkToDrained[chunk]
                if (cause ~= nil) or (drained and (drained - tick) > 0) then
                    if ((cause and ENERGY_THIEF_LOOKUP[cause.name]) or (not cause)) then
			local conversion = ENERGY_THIEF_CONVERSION_TABLE[entity.type]
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

	    end
	    if creditNatives and natives.safeBuildings and (natives.safeEntities[entity.type] or natives.safeEntityName[entity.name]) then
		makeImmortalEntity(surface, entity)
	    else
		accountPlayerEntity(map, entity, natives, false, creditNatives)
	    end
        end
    end
end

local function onEnemyBaseBuild(event)
    local entity = event.entity
    local surface = entity.surface

    if entity.valid and (surface.index == natives.activeSurface) then

	local chunk = getChunkByPosition(map, entity.position)
	if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then
            -- print(entity.name)
            local evolutionFactor = entity.force.evolution_factor
            local base
            if natives.newEnemies then
                base = findNearbyBase(map, chunk, natives)
                if not base then
                    base = createBase(map,
                                      natives,
                                      evolutionFactor,
                                      chunk,
                                      event.tick)
                end
                entity = upgradeEntity(entity,
                                       surface,
                                       base.alignment[mRandom(#base.alignment)],
                                       natives,
                                       evolutionFactor)
            end
            if entity and entity.valid then
                event.entity = registerEnemyBaseStructure(map, entity, base)
            end
        end
    end
end

local function onSurfaceTileChange(event)
    local surfaceIndex = event.surface_index or (event.robot and event.robot.surface and event.robot.surface.index)
    if event.item and
        ((event.item.name == "landfill") or (event.item.name == "waterfill")) and
        (surfaceIndex == natives.activeSurface)
    then
	local surface = game.surfaces[natives.activeSurface]
	local chunks = {}
	local tiles = event.tiles
	for i=1,#tiles do
	    local position = tiles[i].position
	    local chunk = getChunkByPosition(map, position)

	    if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then
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

local function onResourceDepleted(event)
    local entity = event.entity
    if (entity.surface.index == natives.activeSurface) then
	chunkUtils.unregisterResource(entity, map)
    end
end

local function onRobotCliff(event)

    local surface = event.robot.surface
    if (event.item.name == "cliff-explosives") and (surface.index == natives.activeSurface) then
        entityForPassScan(map, event.cliff)
    end
end

local function onUsedCapsule(event)
    local surface = game.players[event.player_index].surface
    if (event.item.name == "cliff-explosives") and (surface.index == natives.activeSurface) then
	local cliffs = surface.find_entities_filtered({area={{event.position.x-0.75,event.position.y-0.75},
							   {event.position.x+0.75,event.position.y+0.75}},
						       type="cliff"})
	for i=1,#cliffs do
	    entityForPassScan(map, cliffs[i])
	end
    end
end

local function onRocketLaunch(event)
    local entity = event.rocket_silo or event.rocket
    if entity and (entity.surface.index == natives.activeSurface) then
	natives.points = natives.points + 2000
	if (natives.points > AI_MAX_OVERFLOW_POINTS) then
	    natives.points = AI_MAX_OVERFLOW_POINTS
	end
    end
end

local function onTriggerEntityCreated(event)
    local entity = event.entity
    if entity.valid and (entity.name  == "drain-trigger-rampant") then
        local chunk = getChunkByPosition(map, entity.position)
        if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then
            map.chunkToDrained[chunk] = event.tick + 60
        end
        entity.destroy()
    end

end

local function onInit()
    global.map = {}
    global.pendingChunks = {}
    global.natives = {}

    map = global.map
    natives = global.natives
    pendingChunks = global.pendingChunks

    prepWorld(false)
    hookEvents()
end

-- hooks

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
                 defines.events.on_robot_built_entity}, onBuild)

script.on_event(defines.events.on_rocket_launched, onRocketLaunch)
script.on_event(defines.events.on_entity_died, onDeath)
script.on_event(defines.events.on_chunk_generated, onChunkGenerated)

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

remote.add_interface("rampant", interop)
