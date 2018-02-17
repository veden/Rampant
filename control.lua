-- imports

local baseUtils = require("libs/BaseUtils")
local chunkPropertyUtils = require("ChunkPropertyUtils")
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
local mathUtils = require("libs/MathUtils")
local config = require("config")
local stringUtils = require("StringUtils")

-- constants

local INTERVAL_LOGIC = constants.INTERVAL_LOGIC
local INTERVAL_PROCESS = constants.INTERVAL_PROCESS
local INTERVAL_CHUNK = constants.INTERVAL_CHUNK
local INTERVAL_SCAN = constants.INTERVAL_SCAN
local INTERVAL_SQUAD = constants.INTERVAL_SQUAD
local INTERVAL_SPAWNER = constants.INTERVAL_SPAWNER

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE

local SENTINEL_IMPASSABLE_CHUNK = constants.SENTINEL_IMPASSABLE_CHUNK

local AI_MAX_OVERFLOW_POINTS = constants.AI_MAX_OVERFLOW_POINTS

local RETREAT_GRAB_RADIUS = constants.RETREAT_GRAB_RADIUS

local RETREAT_SPAWNER_GRAB_RADIUS = constants.RETREAT_SPAWNER_GRAB_RADIUS

local DEFINES_COMMAND_GROUP = defines.command.group
local DEFINES_COMMAND_ATTACK_AREA = defines.command.attack_area

local CHUNK_SIZE = constants.CHUNK_SIZE

local DEFINES_DISTRACTION_NONE = defines.distraction.none
local DEFINES_DISTRACTION_BY_ENEMY = defines.distraction.by_enemy

-- imported functions

local positionToChunkXY = mapUtils.positionToChunkXY

local roundToNearest = mathUtils.roundToNearest

local getChunkByPosition = mapUtils.getChunkByPosition

local entityForPassScan = chunkUtils.entityForPassScan

local processPendingChunks = chunkProcessor.processPendingChunks
local processScanChunks = chunkProcessor.processScanChunks
local processSpawnerChunks = chunkProcessor.processSpawnerChunks

local processMap = mapProcessor.processMap
local processPlayers = mapProcessor.processPlayers
local scanMap = mapProcessor.scanMap

local planning = aiPlanning.planning

local rallyUnits = aiAttackWave.rallyUnits

local recycleBases = baseUtils.recycleBases

local deathScent = pheromoneUtils.deathScent
local victoryScent = pheromoneUtils.victoryScent

local cleanSquads = unitGroupUtils.cleanSquads
local regroupSquads = unitGroupUtils.regroupSquads
local convertUnitGroupToSquad = unitGroupUtils.convertUnitGroupToSquad

local createBase = baseUtils.createBase

local squadsAttack = squadAttack.squadsAttack
local squadsBeginAttack = squadAttack.squadsBeginAttack

local retreatUnits = squadDefense.retreatUnits

local getChunkBase = chunkPropertyUtils.getChunkBase

local isSpawner = stringUtils.isSpawner

local addRemovePlayerEntity = chunkUtils.addRemovePlayerEntity
local unregisterEnemyBaseStructure = chunkUtils.unregisterEnemyBaseStructure
local registerEnemyBaseStructure = chunkUtils.registerEnemyBaseStructure
local makeImmortalEntity = chunkUtils.makeImmortalEntity

local getChunkSpawnerEggTick = chunkPropertyUtils.getChunkSpawnerEggTick
local setChunkSpawnerEggTick = chunkPropertyUtils.setChunkSpawnerEggTick

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
    if (surface.index == 1) then
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
    if (event.surface.index == 1) then
        pendingChunks[#pendingChunks+1] = event
    end
end

local function rebuildMap()
    game.surfaces[1].print("Rampant - Reindexing chunks, please wait.")
    -- clear old map processing Queue
    -- prevents queue adding duplicate chunks
    -- chunks are by key, so should overwrite old

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

    map.queueSpawners = {}
    
    -- preallocating memory to be used in code, making it fast by reducing garbage generated.
    map.neighbors = { SENTINEL_IMPASSABLE_CHUNK,
		      SENTINEL_IMPASSABLE_CHUNK,
		      SENTINEL_IMPASSABLE_CHUNK,
		      SENTINEL_IMPASSABLE_CHUNK,
		      SENTINEL_IMPASSABLE_CHUNK,
		      SENTINEL_IMPASSABLE_CHUNK,
		      SENTINEL_IMPASSABLE_CHUNK,
		      SENTINEL_IMPASSABLE_CHUNK }
    map.cardinalNeighbors = { SENTINEL_IMPASSABLE_CHUNK,
			      SENTINEL_IMPASSABLE_CHUNK,
			      SENTINEL_IMPASSABLE_CHUNK,
			      SENTINEL_IMPASSABLE_CHUNK }
    map.position = {x=0,
		    y=0}

    map.position2Top = {0, 0}
    map.position2Bottom = {0, 0}
    --this is shared between two different queries
    map.area = {{0, 0}, {0, 0}}
    map.area2 = {map.position2Top, map.position2Bottom}
    map.countResourcesQuery = { area=map.area, type="resource" }
    map.filteredEntitiesEnemyQuery = { area=map.area, force="enemy" }
    map.filteredEntitiesEnemyUnitQuery = { area=map.area, force="enemy", type="unit", limit=301 }
    map.filteredEntitiesUnitSpawnereQuery = { area=map.area, force="enemy", type="unit-spawner" }
    map.filteredEntitiesWormQuery = { area=map.area, force="enemy", type="turret" }
    map.filteredEntitiesSpawnerQueryLimited = { area=map.area2, force="enemy", type="unit-spawner" }
    map.filteredEntitiesWormQueryLimited = { area=map.area2, force="enemy", type="turret" }
    map.filteredEntitiesPlayerQuery = { area=map.area, force="player" }
    map.canPlaceQuery = { name="", position={0,0} }
    map.filteredTilesQuery = { name="", area=map.area }

    map.attackAreaCommand = {
	type = DEFINES_COMMAND_ATTACK_AREA,
	destination = map.position,
	radius = CHUNK_SIZE,
	distraction = DEFINES_DISTRACTION_BY_ENEMY
    }

    map.retreatCommand = { type = DEFINES_COMMAND_GROUP,
			   group = nil,
			   distraction = DEFINES_DISTRACTION_NONE }
    
    -- switched over to tick event
    map.logicTick = roundToNearest(game.tick + INTERVAL_LOGIC, INTERVAL_LOGIC)
    map.scanTick = roundToNearest(game.tick + INTERVAL_SCAN, INTERVAL_SCAN)
    map.processTick = roundToNearest(game.tick + INTERVAL_PROCESS, INTERVAL_PROCESS)
    map.chunkTick = roundToNearest(game.tick + INTERVAL_CHUNK, INTERVAL_CHUNK)
    map.squadTick = roundToNearest(game.tick + INTERVAL_SQUAD, INTERVAL_SQUAD)
    
end    


local function onModSettingsChange(event)
    
    if event and (string.sub(event.setting, 1, 7) ~= "rampant") then
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
    end
    
    upgrade.compareTable(natives, "attackUsePlayer", settings.global["rampant-attackWaveGenerationUsePlayerProximity"].value)
    upgrade.compareTable(natives, "attackUsePollution", settings.global["rampant-attackWaveGenerationUsePollution"].value)

    upgrade.compareTable(natives, "deadZoneFrequency", settings.global["rampant-deadZoneFrequency"].value)
    
    upgrade.compareTable(natives, "attackThresholdMin", settings.global["rampant-attackWaveGenerationThresholdMin"].value)
    upgrade.compareTable(natives, "attackThresholdMax", settings.global["rampant-attackWaveGenerationThresholdMax"].value)
    upgrade.compareTable(natives, "attackThresholdRange", natives.attackThresholdMax - natives.attackThresholdMin)
    upgrade.compareTable(natives, "attackWaveMaxSize", settings.global["rampant-attackWaveMaxSize"].value)
    upgrade.compareTable(natives, "attackPlayerThreshold", settings.global["rampant-attackPlayerThreshold"].value)
    upgrade.compareTable(natives, "aiNocturnalMode", settings.global["rampant-permanentNocturnal"].value)
    upgrade.compareTable(natives, "aiPointsScaler", settings.global["rampant-aiPointsScaler"].value)

    upgrade.compareTable(natives, "newEnemies", settings.startup["rampant-newEnemies"].value)    
    upgrade.compareTable(natives, "enemySeed", settings.startup["rampant-enemySeed"].value)

    -- RE-ENABLE WHEN COMPLETE
    natives.useCustomAI = constants.DEV_CUSTOM_AI
    -- changed, newValue = upgrade.compareTable(natives, "useCustomAI", settings.startup["rampant-useCustomAI"].value)
    -- if natives.useCustomAI then
    -- 	game.forces.enemy.ai_controllable = false
    -- else
    -- 	game.forces.enemy.ai_controllable = true
    -- end
    -- if changed and newValue then
    -- 	rebuildMap()
    -- 	return false
    -- end
    return true
end

local function onConfigChanged()
    local upgraded
    upgraded, natives = upgrade.attempt(natives)
    if upgraded and onModSettingsChange(nil) then
	rebuildMap()
	if natives.newEnemies then
	    rebuildNativeTables(natives, game.surfaces[1], game.create_random_generator(natives.enemySeed))
	end

	-- clear pending chunks, will be added when loop runs below
	global.pendingChunks = {}
	pendingChunks = global.pendingChunks

	-- queue all current chunks that wont be generated during play
	local surface = game.surfaces[1]
	local tick = game.tick
	for chunk in surface.get_chunks() do
	    onChunkGenerated({ tick = tick,
			       surface = surface, 
			       area = { left_top = { x = chunk.x * 32,
						     y = chunk.y * 32 }}})
	end

	processPendingChunks(natives, map, surface, pendingChunks, tick, game.forces.enemy.evolution_factor, true)
    end
end

script.on_nth_tick(INTERVAL_PROCESS,
		   function (event)

		       local tick = event.tick
		       local gameRef = game
		       local surface = gameRef.surfaces[1]
		       
		       processPlayers(gameRef.players, map, surface, natives, tick)
		       
		       processMap(map, surface, natives, tick, gameRef.forces.enemy.evolution_factor)
end)

script.on_nth_tick(INTERVAL_SCAN,
		   function (event)
		       local tick = event.tick
		       local gameRef = game
		       local surface = gameRef.surfaces[1]

		       processPendingChunks(natives, map, surface, pendingChunks, tick, gameRef.forces.enemy.evolution_factor)

		       scanMap(map, surface, natives, tick)

		       map.queueSpawners = processSpawnerChunks(map, surface, natives, tick)
		       
		       map.chunkToPassScan = processScanChunks(map, surface)

end)

script.on_nth_tick(INTERVAL_LOGIC,
		   function (event)
		       local tick = event.tick
		       local gameRef = game
		       local surface = gameRef.surfaces[1]
		       
		       planning(natives,
				gameRef.forces.enemy.evolution_factor,
				tick,
				surface,
				gameRef.connected_players)

		       if natives.newEnemies then
			   recycleBases(natives, tick)
		       end
end)

script.on_nth_tick(INTERVAL_SQUAD,
		   function (event)
		       local gameRef = game
		       
		       cleanSquads(natives, map)
		       regroupSquads(natives, map)
		       
		       squadsBeginAttack(natives, gameRef.players)
		       squadsDispatch(map, gameRef.surfaces[1], natives)

end)

local function onBuild(event)
    local entity = event.created_entity
    if (entity.surface.index == 1) then
	addRemovePlayerEntity(map, entity, natives, true, false)
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
    if (surface.index == 1) then	
	addRemovePlayerEntity(map, entity, natives, false, false)
    end
end

local function onDeath(event)
    local entity = event.entity
    local surface = entity.surface
    if (surface.index == 1) then
	local entityPosition = entity.position
	local chunk = getChunkByPosition(map, entityPosition)
	local cause = event.cause
        if (entity.force.name == "enemy") then
            if (entity.type == "unit") then
		
		if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then
		    -- drop death pheromone where unit died
		    deathScent(chunk)
		    
		    if event.force and (event.force.name == "player") and (chunk[MOVEMENT_PHEROMONE] < natives.retreatThreshold) then
			local tick = event.tick
			
			local artilleryBlast = (cause and ((cause.type == "artillery-wagon") or (cause.type == "artillery-turret")))
			
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
                
            elseif event.force and (event.force.name == "player") and (entity.type == "unit-spawner") or (entity.type == "turret") then
		local tick = event.tick

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
        elseif (entity.force.name == "player") then
	    local creditNatives = false
	    if (event.force ~= nil) and (event.force.name == "enemy") then
		creditNatives = true
		if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then
		    victoryScent(chunk, entity.type)
		end
	    end
	    if creditNatives and natives.safeBuildings and (natives.safeEntities[entity.type] or natives.safeEntityName[entity.name]) then
		makeImmortalEntity(surface, entity)
	    else
		addRemovePlayerEntity(map, entity, natives, false, creditNatives)
	    end
        end
    end
end

local function onEnemyBaseBuild(event)
    local entity = event.entity
    local surface = entity.surface
    if entity.valid and (surface.index == 1) then
	local chunk = getChunkByPosition(map, entity.position)
	if (chunk ~= SENTINEL_IMPASSABLE_CHUNK) then
	    local evolutionFactor = entity.force.evolution_factor
	    if natives.newEnemies then
		local base = getChunkBase(map, chunk)
		if not base then
		    base = createBase(map, natives, evolutionFactor, chunk, surface, event.tick)
		end
		entity = upgradeEntity(entity, surface, base.alignment, natives, evolutionFactor)
	    end
	    if entity and entity.valid then
		event.entity = registerEnemyBaseStructure(map, entity, natives, evolutionFactor, surface, event.tick)
	    end
	end
    end
end

local function onSurfaceTileChange(event)
    local surfaceIndex = event.surface_index or (event.robot and event.robot.surface.index)
    if event.item and (event.item.name == "landfill") and (surfaceIndex == 1) then
	local surface = game.surfaces[1]
	local chunks = {}
	local tiles = event.tiles
	for i=1,#tiles do
	    local position = tiles[i].position	    
	    local chunk = getChunkByPosition(map, position, true)

	    -- weird bug with table pointer equality using name instead of pointer comparison
	    if not chunk.name then
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
    if (entity.surface.index == 1) then
	chunkUtils.unregisterResource(entity, map)
    end
end

local function onTriggerEntityCreated(event)
    local entity = event.entity
    if entity and entity.valid then
	local name = event.entity.name
	if isSpawner(name) then
	    local tick = event.tick
	    local chunk = getChunkByPosition(map, entity.position)
	    if chunk and ((tick - getChunkSpawnerEggTick(map, chunk)) > INTERVAL_SPAWNER) then
		setChunkSpawnerEggTick(map, chunk, tick)
		map.queueSpawners[#map.queueSpawners+1] = {
		    tick,
		    chunk,
		    entity.position
		}
	    end
	end
    end
end

local function onUsedCapsule(event)
    local surface = game.players[event.player_index].surface
    if (event.item.name == "cliff-explosives") and (surface.index == 1) then
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
    if entity and (entity.surface.index == 1) then
	natives.points = natives.points + 2000
	if (natives.points > AI_MAX_OVERFLOW_POINTS) then
	    natives.points = AI_MAX_OVERFLOW_POINTS
	end
    end    
end

local function onInit()
    global.map = {}
    global.pendingChunks = {}
    global.natives = {}
    
    map = global.map
    natives = global.natives
    pendingChunks = global.pendingChunks
    
    onConfigChanged()
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

script.on_event(defines.events.on_biter_base_built, onEnemyBaseBuild)
script.on_event({defines.events.on_player_mined_entity,
                 defines.events.on_robot_mined_entity}, onMine)
script.on_event({defines.events.on_built_entity,
                 defines.events.on_robot_built_entity}, onBuild)

script.on_event(defines.events.on_trigger_created_entity, onTriggerEntityCreated)

script.on_event(defines.events.on_rocket_launched, onRocketLaunch)

script.on_event(defines.events.on_entity_died, onDeath)
-- script.on_event(defines.events.on_tick, onTick)
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
			 dumpNatives = tests.dumpNatives,
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
			 exportAiState = tests.exportAiState(onTick)
		     }
)

remote.add_interface("rampant", interop)
