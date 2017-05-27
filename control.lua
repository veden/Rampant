-- imports

local entityUtils = require("libs/EntityUtils")
local mapUtils = require("libs/MapUtils")
local unitGroupUtils = require("libs/UnitGroupUtils")
local chunkProcessor = require("libs/ChunkProcessor")
local baseProcessor = require("libs/BaseProcessor")
local mapProcessor = require("libs/MapProcessor")
local constants = require("libs/Constants")
local pheromoneUtils = require("libs/PheromoneUtils")
local aiDefense = require("libs/AIDefense")
local aiAttack = require("libs/AIAttack")
local aiBuilding = require("libs/AIBuilding")
local aiPlanning = require("libs/AIPlanning")
local interop = require("libs/Interop")
local tests = require("tests")
local upgrade = require("Upgrade")

-- constants

local INTERVAL_LOGIC = constants.INTERVAL_LOGIC
local INTERVAL_PROCESS = constants.INTERVAL_PROCESS

local MOVEMENT_PHEROMONE = constants.MOVEMENT_PHEROMONE

local BASE_RALLY_CHANCE = constants.BASE_RALLY_CHANCE
local BONUS_RALLY_CHANCE = constants.BONUS_RALLY_CHANCE

local RETREAT_MOVEMENT_PHEROMONE_LEVEL = constants.RETREAT_MOVEMENT_PHEROMONE_LEVEL

-- imported functions

local getChunkByPosition = mapUtils.getChunkByPosition

local processPendingChunks = chunkProcessor.processPendingChunks

local processMap = mapProcessor.processMap
local processPlayers = mapProcessor.processPlayers
local scanMap = mapProcessor.scanMap

local planning = aiPlanning.planning

local rallyUnits = aiBuilding.rallyUnits

local deathScent = pheromoneUtils.deathScent
local victoryScent = pheromoneUtils.victoryScent

local cleanSquads = unitGroupUtils.cleanSquads
local regroupSquads = unitGroupUtils.regroupSquads
local convertUnitGroupToSquad = unitGroupUtils.convertUnitGroupToSquad

local squadAttack = aiAttack.squadAttack
local squadBeginAttack = aiAttack.squadBeginAttack

local retreatUnits = aiDefense.retreatUnits

local addRemovePlayerEntity = entityUtils.addRemovePlayerEntity
local removeEnemyBase = entityUtils.removeEnemyBase
--local makeImmortalEntity = entityUtils.makeImmortalEntity

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

local function onModSettingsChange(event)
    
    if event and (string.sub(event.setting, 1, 7) ~= "rampant") then
	return
    end
    
    natives.safeBuildings = settings.global["rampant-safeBuildings"].value   
    
    natives.safeEntities["curved-rail"] = settings.global["rampant-safeBuildings-curvedRail"].value
    natives.safeEntities["straight-rail"] = settings.global["rampant-safeBuildings-straightRail"].value    
    natives.safeEntities["rail-signal"] = settings.global["rampant-safeBuildings-railSignals"].value
    natives.safeEntities["rail-chain-signal"] = settings.global["rampant-safeBuildings-railChainSignals"].value
    natives.safeEntities["train-stop"] = settings.global["rampant-safeBuildings-trainStops"].value

    local poles = settings.global["rampant-safeBuildings-bigElectricPole"].value
    natives.safeEntityName["big-electric-pole"] = poles
    natives.safeEntityName["big-electric-pole-2"] = poles
    natives.safeEntityName["big-electric-pole-3"] = poles
    natives.safeEntityName["big-electric-pole-4"] = poles
    
    natives.attackUsePlayer = settings.global["rampant-attackWaveGenerationUsePlayerProximity"].value
    natives.attackUsePollution = settings.global["rampant-attackWaveGenerationUsePollution"].value
    
    natives.attackThresholdMin = settings.global["rampant-attackWaveGenerationThresholdMin"].value
    natives.attackThresholdMax = settings.global["rampant-attackWaveGenerationThresholdMax"].value
    natives.attackThresholdRange = natives.attackThresholdMax - natives.attackThresholdMin
    natives.attackWaveMaxSize = settings.global["rampant-attackWaveMaxSize"].value
    natives.attackPlayerThreshold = settings.global["rampant-attackPlayerThreshold"].value
    natives.aiNocturnalMode = settings.global["rampant-permanentNocturnal"].value
    natives.aiPointsScaler = settings.global["rampant-aiPointsScaler"].value
end

local function onConfigChanged()

    if upgrade.attempt(natives, regionMap) then
	onModSettingsChange(nil)

	game.surfaces[1].print("Rampant - Reindexing chunks, please wait")
	-- clear old regionMap processing Queue
	-- prevents queue adding duplicate chunks
	-- chunks are by key, so should overwrite old
	regionMap.processQueue = {}
	regionMap.processPointer = 1
	regionMap.scanPointer = 1
	-- clear pending chunks, will be added when loop runs below
	pendingChunks = {}

	-- queue all current chunks that wont be generated during play
	local surface = game.surfaces[1]
	for chunk in surface.get_chunks() do
	    onChunkGenerated({ tick = game.tick,
			       surface = surface, 
			       area = { left_top = { x = chunk.x * 32,
						     y = chunk.y * 32 }}})
	end
    end
end

local function onTick(event)
    local tick = event.tick
    if (tick == regionMap.processTick) then
	regionMap.processTick = regionMap.processTick + INTERVAL_PROCESS
	local surface = game.surfaces[1]
	local evolutionFactor = game.forces.enemy.evolution_factor
	local players = game.players

	processPendingChunks(natives, regionMap, surface, pendingChunks, tick)
	scanMap(regionMap, surface, natives, evolutionFactor)

	if (tick == regionMap.logicTick) then
	    regionMap.logicTick = regionMap.logicTick + INTERVAL_LOGIC

	    planning(natives, evolutionFactor, tick, surface)
	    
	    cleanSquads(natives, evolutionFactor)
	    regroupSquads(natives, evolutionFactor)
	    
	    processPlayers(players, regionMap, surface, natives, evolutionFactor, tick)

	    processBases(regionMap, surface, natives, tick)
	    
	    squadBeginAttack(natives, players, evolutionFactor)
	    squadAttack(regionMap, surface, natives)
	end

	processMap(regionMap, surface, natives, evolutionFactor)
    end
end

local function onBuild(event)
    local entity = event.created_entity
    addRemoveEntity(regionMap, entity, natives, true, false)
    if natives.safeBuildings then
	if natives.safeEntities[entity.type] or natives.safeEntityName[entity.name] then
	    entity.destructible = false
	end
    end
end

local function onPickUp(event)
    addRemovePlayerEntity(regionMap, event.entity, natives, false, false)
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
		    
		    if ((event.force ~= nil) and (event.force.name == "player")) then
			local evolutionFactor = entity.force.evolution_factor
			local tick = event.tick

			if (deathChunk[MOVEMENT_PHEROMONE] < -(evolutionFactor * RETREAT_MOVEMENT_PHEROMONE_LEVEL)) then
			    retreatUnits(deathChunk, 
					 convertUnitGroupToSquad(natives, 
								 entity.unit_group),
					 regionMap, 
					 surface, 
					 natives,
					 tick)
			    local rallyThreshold = BASE_RALLY_CHANCE + (evolutionFactor * BONUS_RALLY_CHANCE)
			    if (math.random() < rallyThreshold) then
				rallyUnits(deathChunk,
					   regionMap,
					   surface,
					   natives,
					   evolutionFactor,
					   tick)
			    end
			end
		    end
                end
                
            elseif (entity.type == "unit-spawner") or (entity.type == "turret") then
                removeEnemyBase(regionMap, entity)
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

local function onSurfaceTileChange(event)
    local player = game.players[event.player_index]
    if (player.surface.index==1) then
	aiBuilding.fillTunnel(regionMap, player.surface, natives, event.positions)
    end
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

script.on_event(defines.events.on_player_built_tile, onSurfaceTileChange)

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
			 clearBases = tests.clearBases
		     }
)

remote.add_interface("rampant", interop)
