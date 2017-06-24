-- imports

local entityUtils = require("libs/EntityUtils")
local mapUtils = require("libs/MapUtils")
local unitGroupUtils = require("libs/UnitGroupUtils")
local chunkProcessor = require("libs/ChunkProcessor")
local baseProcessor = require("libs/BaseProcessor")
local mapProcessor = require("libs/MapProcessor")
local constants = require("libs/Constants")
local pheromoneUtils = require("libs/PheromoneUtils")
local squadDefense = require("libs/SquadDefense")
local squadAttack = require("libs/SquadAttack")
local aiAttackWave = require("libs/AIAttackWave")
local aiPlanning = require("libs/AIPlanning")
local interop = require("libs/Interop")
local tests = require("tests")
local upgrade = require("Upgrade")
local baseRegisterUtils = require("libs/BaseRegisterUtils")
local mathUtils = require("libs/MathUtils")
local config = require("config")

-- constants

local INTERVAL_LOGIC = constants.INTERVAL_LOGIC
local INTERVAL_PROCESS = constants.INTERVAL_PROCESS

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE

local AI_MAX_OVERFLOW_POINTS = constants.AI_MAX_OVERFLOW_POINTS

-- imported functions

local roundToNearest = mathUtils.roundToNearest

-- imported functions

local getChunkByPosition = mapUtils.getChunkByPosition

local processPendingChunks = chunkProcessor.processPendingChunks

local processMap = mapProcessor.processMap
local processPlayers = mapProcessor.processPlayers
local scanMap = mapProcessor.scanMap

local planning = aiPlanning.planning

local rallyUnits = aiAttackWave.rallyUnits

local deathScent = pheromoneUtils.deathScent
local victoryScent = pheromoneUtils.victoryScent

local cleanSquads = unitGroupUtils.cleanSquads
local regroupSquads = unitGroupUtils.regroupSquads
local convertUnitGroupToSquad = unitGroupUtils.convertUnitGroupToSquad

local squadsAttack = squadAttack.squadsAttack
local squadsBeginAttack = squadAttack.squadsBeginAttack

local retreatUnits = squadDefense.retreatUnits

local addRemovePlayerEntity = entityUtils.addRemovePlayerEntity
local unregisterEnemyBaseStructure = baseRegisterUtils.unregisterEnemyBaseStructure
local registerEnemyBaseStructure = baseRegisterUtils.registerEnemyBaseStructure
local makeImmortalEntity = entityUtils.makeImmortalEntity

local processBases = baseProcessor.processBases

-- local references to global

local regionMap
local natives
local pendingChunks

-- hook functions

local function onLoad()
    regionMap = global.regionMap
    natives = global.natives
    pendingChunks = global.pendingChunks
end

local function onChunkGenerated(event)
    -- queue generated chunk for delayed processing, queuing is required because some mods (RSO) mess with chunk as they
    -- are generated, which messes up the scoring.
    if (event.surface.index == 1) then
        pendingChunks[#pendingChunks+1] = event
    end
end

local function rebuildRegionMap()
    game.surfaces[1].print("Rampant - Reindexing chunks, please wait.")
    -- clear old regionMap processing Queue
    -- prevents queue adding duplicate chunks
    -- chunks are by key, so should overwrite old

    global.regionMap = {}
    regionMap = global.regionMap
    regionMap.processQueue = {}
    regionMap.processPointer = 1
    regionMap.scanPointer = 1
    -- preallocating memory to be used in code, making it fast by reducing garbage generated.
    regionMap.neighbors = { nil, nil, nil, nil, nil, nil, nil, nil }
    regionMap.cardinalNeighbors = { nil, nil, nil, nil }
    regionMap.chunkTiles = {}
    regionMap.position = {x=0,
			  y=0}
    for i=1,1024 do
	regionMap.chunkTiles[i] = nil
    end

    -- switched over to tick event
    regionMap.logicTick = roundToNearest(game.tick + INTERVAL_LOGIC, INTERVAL_LOGIC)
    regionMap.processTick = roundToNearest(game.tick + INTERVAL_PROCESS, INTERVAL_PROCESS)
    
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

    processPendingChunks(natives, regionMap, surface, pendingChunks, tick)
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
    
    upgrade.compareTable(natives, "attackThresholdMin", settings.global["rampant-attackWaveGenerationThresholdMin"].value)
    upgrade.compareTable(natives, "attackThresholdMax", settings.global["rampant-attackWaveGenerationThresholdMax"].value)
    upgrade.compareTable(natives, "attackThresholdRange", natives.attackThresholdMax - natives.attackThresholdMin)
    upgrade.compareTable(natives, "attackWaveMaxSize", settings.global["rampant-attackWaveMaxSize"].value)
    upgrade.compareTable(natives, "attackPlayerThreshold", settings.global["rampant-attackPlayerThreshold"].value)
    upgrade.compareTable(natives, "aiNocturnalMode", settings.global["rampant-permanentNocturnal"].value)
    upgrade.compareTable(natives, "aiPointsScaler", settings.global["rampant-aiPointsScaler"].value)

    -- RE-ENABLE WHEN COMPLETE
    natives.useCustomAI = constants.DEV_CUSTOM_AI
    -- changed, newValue = upgrade.compareTable(natives, "useCustomAI", settings.startup["rampant-useCustomAI"].value)
    -- if natives.useCustomAI then
    -- 	game.forces.enemy.ai_controllable = false
    -- else
    -- 	game.forces.enemy.ai_controllable = true
    -- end
    -- if changed and newValue then
    -- 	rebuildRegionMap()
    -- 	return false
    -- end
    return true
end

local function onConfigChanged()
    if upgrade.attempt(natives, regionMap) and onModSettingsChange(nil) then
	rebuildRegionMap()
    end
end

local function onTick(event)
    local tick = event.tick
    if (tick == regionMap.processTick) then
	regionMap.processTick = regionMap.processTick + INTERVAL_PROCESS
	local surface = game.surfaces[1]

	processPendingChunks(natives, regionMap, surface, pendingChunks, tick)
	scanMap(regionMap, surface, natives)

	if (tick == regionMap.logicTick) then
	    regionMap.logicTick = regionMap.logicTick + INTERVAL_LOGIC

	    local players = game.players

	    planning(natives,
	    	     game.forces.enemy.evolution_factor,
	    	     tick,
	    	     surface)
	    
	    cleanSquads(natives)
	    regroupSquads(natives)
	    
	    processPlayers(players, regionMap, surface, natives, tick)

	    if natives.useCustomAI then
	    	processBases(regionMap, surface, natives, tick)
	    end
	    
	    squadsBeginAttack(natives, players)
	    squadsAttack(regionMap, surface, natives)
	end

	processMap(regionMap, surface, natives, tick)
    end
end

local function onBuild(event)
    local entity = event.created_entity
    addRemovePlayerEntity(regionMap, entity, natives, true, false)
    if natives.safeBuildings then
	if natives.safeEntities[entity.type] or natives.safeEntityName[entity.name] then
	    entity.destructible = false
	end
    end
end

local function onPickUp(event)
    addRemovePlayerEntity(regionMap, event.entity, natives, false, false)
end

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
	local chunk = getChunkByPosition(regionMap, event.position.x, event.position.y)
	if chunk then
	    rallyUnits(chunk,
		       regionMap,
		       surface,
		       natives,
		       event.tick)
	end
    end    
end

local function onDeath(event)
    local entity = event.entity
    local surface = entity.surface
    if (surface.index == 1) then
        if (entity.force.name == "enemy") then
            if (entity.type == "unit") then
		local entityPosition = entity.position
		local deathChunk = getChunkByPosition(regionMap, entityPosition.x, entityPosition.y)
		
		if deathChunk then
		    -- drop death pheromone where unit died
		    deathScent(deathChunk)
		    
		    if event.force and (event.force.name == "player") and (deathChunk[MOVEMENT_PHEROMONE] < natives.retreatThreshold) then
			local tick = event.tick
			
			retreatUnits(deathChunk,
				     entityPosition,
				     convertUnitGroupToSquad(natives, 
							     entity.unit_group),
				     regionMap, 
				     surface, 
				     natives,
				     tick)
			if (math.random() < natives.rallyThreshold) and not surface.peaceful_mode then
			    rallyUnits(deathChunk,
				       regionMap,
				       surface,
				       natives,
				       tick)
			end
		    end
                end
                
            elseif (entity.type == "unit-spawner") or (entity.type == "turret") then
                unregisterEnemyBaseStructure(regionMap, entity)
            end
        elseif (entity.force.name == "player") then
	    local creditNatives = false
	    local entityPosition = entity.position
	    if (event.force ~= nil) and (event.force.name == "enemy") then
		creditNatives = true
		local victoryChunk = getChunkByPosition(regionMap, entityPosition.x, entityPosition.y)
		if victoryChunk then
		    victoryScent(victoryChunk, entity.type)
		end
	    end
	    if creditNatives and natives.safeBuildings and (natives.safeEntities[entity.type] or natives.safeEntityName[entity.name]) then
		makeImmortalEntity(surface, entity)
	    else
		addRemovePlayerEntity(regionMap, entity, natives, false, creditNatives)
	    end
        end
    end
end

local function onEnemyBaseBuild(event)
    local entity = event.entity
    registerEnemyBaseStructure(regionMap, entity, nil)
end

local function onSurfaceTileChange(event)
    -- local player = game.players[event.player_index]
    -- if (player.surface.index == 1) then
    -- 	aiAttackWave.fillTunnel(regionMap, player.surface, natives, event.positions)
    -- end
end

local function onInit()
    global.regionMap = {}
    global.pendingChunks = {}
    global.natives = {}
    
    regionMap = global.regionMap
    natives = global.natives
    pendingChunks = global.pendingChunks
    
    onConfigChanged()
end

-- hooks

script.on_init(onInit)
script.on_load(onLoad)
script.on_event(defines.events.on_runtime_mod_setting_changed,
		onModSettingsChange)
script.on_configuration_changed(onConfigChanged)

if config.ionCannonPresent then
    script.on_event(remote.call("orbital_ion_cannon", "on_ion_cannon_fired"),
    		    onIonCannonFired)
    -- script.on_event(remote.call("orbital_ion_cannon", "on_ion_cannon_targeted"),
    -- 		    onIonCannonTargeted)
end

script.on_event(defines.events.on_player_built_tile, onSurfaceTileChange)

script.on_event(defines.events.on_biter_base_built,
		onEnemyBaseBuild)
script.on_event({defines.events.on_preplayer_mined_item,
                 defines.events.on_robot_pre_mined}, 
    onPickUp)
script.on_event({defines.events.on_built_entity,
                 defines.events.on_robot_built_entity}, 
    onBuild)

script.on_event(defines.events.on_entity_died, onDeath)
script.on_event(defines.events.on_tick, onTick)
script.on_event(defines.events.on_chunk_generated, onChunkGenerated)

remote.add_interface("rampantTests",
		     {
			 pheromoneLevels = tests.pheromoneLevels,
			 activeSquads = tests.activeSquads,
			 entitiesOnPlayerChunk = tests.entitiesOnPlayerChunk,
			 findNearestPlayerEnemy = tests.findNearestPlayerEnemy,
			 aiStats = tests.aiStats,
			 fillableDirtTest = tests.fillableDirtTest,
			 tunnelTest = tests.tunnelTest,
			 createEnemy = tests.createEnemy,
			 attackOrigin = tests.attackOrigin,
			 cheatMode = tests.cheatMode,
			 gaussianRandomTest = tests.gaussianRandomTest,
			 reveal = tests.reveal,
			 showMovementGrid = tests.showMovementGrid,
			 baseStats = tests.baseStats,
			 baseTiles = tests.baseTiles,
			 mergeBases = tests.mergeBases,
			 clearBases = tests.clearBases,
			 getOffsetChunk = tests.getOffsetChunk,
			 registeredNest = tests.registeredNest,
			 colorResourcePoints = tests.colorResourcePoints,
			 stepAdvanceTendrils = tests.stepAdvanceTendrils
		     }
)

remote.add_interface("rampant", interop)
