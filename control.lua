-- imports

local entityUtils = require("libs/EntityUtils")
local mapUtils = require("libs/MapUtils")
local unitGroupUtils = require("libs/UnitGroupUtils")
local chunkProcessor = require("libs/ChunkProcessor")
local mapProcessor = require("libs/MapProcessor")
local constants = require("libs/Constants")
local pheromoneUtils = require("libs/PheromoneUtils")
local aiDefense = require("libs/AIDefense")
local aiAttack = require("libs/AIAttack")
local aiBuilding = require("libs/AIBuilding")
local aiPlanning = require("libs/AIPlanning")
local mathUtils = require("libs/MathUtils")
local tests = require("tests")

-- constants

local INTERVAL_LOGIC = constants.INTERVAL_LOGIC
local INTERVAL_PROCESS = constants.INTERVAL_PROCESS

local MAX_RALLY_CRIES = constants.MAX_RALLY_CRIES
local MAX_RETREATS = constants.MAX_RETREATS

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
-- local removeScout = aiBuilding.removeScout
-- local scouting = aiBuilding.scouting

local rallyUnits = aiBuilding.rallyUnits

local deathScent = pheromoneUtils.deathScent

local regroupSquads = unitGroupUtils.regroupSquads
local convertUnitGroupToSquad = unitGroupUtils.convertUnitGroupToSquad

local squadAttack = aiAttack.squadAttack
local squadBeginAttack = aiAttack.squadBeginAttack

local retreatUnits = aiDefense.retreatUnits

local addRemoveEntity = entityUtils.addRemoveEntity

local roundToNearest = mathUtils.roundToNearest

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

local function onConfigChanged()
    if (global.version == nil) then

        -- removed in version 9
        -- regionMap.pQ = {{}} -- processing queue
        -- regionMap.pI = 1 -- insertion location for chunk processing
        -- regionMap.pP = 1 -- index for the chunk set to process
        -- regionMap.pR = -1 -- current processing roll
        natives.squads = {}
        natives.scouts = {}
        natives.tunnels = {}
        natives.points = 0
        pendingChunks = {}
        
        global.version = constants.VERSION_5
    end
    if (global.version < constants.VERSION_9) then
	
        -- remove version 5 references
        regionMap.pQ = nil
        regionMap.pI = nil
        regionMap.pP = nil
        regionMap.pR = nil
	
	global.version = constants.VERSION_9
    end
    if (global.version < constants.VERSION_10) then
	for _,squad in pairs(natives.squads) do
	    squad.frenzy = false
	    squad.frenzyPosition = {x=0,y=0}
	    squad.rabid = false
	end
	
	global.version = constants.VERSION_10
    end
    if (global.version < constants.VERSION_11) then
	natives.state = constants.AI_STATE_AGGRESSIVE
	natives.temperament = 0
	-- needs to be on inner logic tick loop interval
	natives.stateTick = roundToNearest(game.tick + INTERVAL_LOGIC, INTERVAL_LOGIC)
	natives.temperamentTick = roundToNearest(game.tick + INTERVAL_LOGIC, INTERVAL_LOGIC)
	
	global.version = constants.VERSION_11
    end
    if (global.version < constants.VERSION_12) then
	for _,squad in pairs(natives.squads) do
	    squad.status = constants.SQUAD_GUARDING
	    squad.kamikaze = false
	end
	
	-- reset ai build points due to error in earning points
	natives.points = 0
	
	global.version = constants.VERSION_12
    end
    if (global.version < constants.VERSION_13) then
	-- switched over to tick event
	regionMap.logicTick = roundToNearest(game.tick + INTERVAL_LOGIC, INTERVAL_LOGIC)
	regionMap.processTick = roundToNearest(game.tick + INTERVAL_PROCESS, INTERVAL_PROCESS)

	-- used to rate limit the number of rally cries during a period of time
	natives.rallyCries = MAX_RALLY_CRIES
	natives.retreats = MAX_RETREATS

	global.version = constants.VERSION_13
    end
    if (global.version < constants.VERSION_14) then
	-- clear old regionMap processing Queue
	-- prevents queue adding duplicate chunks
	-- chunks are by key, so should overwrite old
	regionMap.processQueue = {}
	regionMap.processPointer = 1
	regionMap.scanPointer = 1
	-- clear pending chunks, will be added when loop runs below
	pendingChunks = {}

	game.map_settings.unit_group.member_disown_distance = 5
	game.map_settings.unit_group.max_member_speedup_when_behind = 1.1
	game.map_settings.unit_group.max_member_slowdown_when_ahead = 1.0
	game.map_settings.unit_group.max_group_slowdown_factor = 0.9

	-- queue all current chunks that wont be generated during play
	local surface = game.surfaces[1]
	for chunk in surface.get_chunks() do
	    onChunkGenerated({ surface = surface, 
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
	local evolutionFactor = game.evolution_factor
	local players = game.players
	
	processPendingChunks(regionMap, surface, pendingChunks)
	scanMap(regionMap, surface)

	if (tick == regionMap.logicTick) then
	    regionMap.logicTick = regionMap.logicTick + INTERVAL_LOGIC

	    natives.rallyCries = MAX_RALLY_CRIES

	    natives.retreats = MAX_RETREATS
	    
	    planning(natives, evolutionFactor, tick)
	    
	    regroupSquads(natives, evolutionFactor)
	    
	    processPlayers(players, regionMap, surface, natives, evolutionFactor, tick)
	    
	    -- scouting(regionMap, natives)
	    
	    squadBeginAttack(natives, players, evolutionFactor)
	    squadAttack(regionMap, surface, natives)
	end

	processMap(regionMap, surface, natives, evolutionFactor) 
    end
end

local function onBuild(event)
    addRemoveEntity(regionMap, event.created_entity, natives, true, false)
end

local function onPickUp(event)
    addRemoveEntity(regionMap, event.entity, natives, false, false)
end

local function onDeath(event)
    local entity = event.entity
    local surface = entity.surface
    if (surface.index == 1) then
        if (entity.force.name == "enemy") then
            if (entity.type == "unit") then
                local entityPosition = entity.position
		local deathChunk = getChunkByPosition(regionMap, entityPosition.x, entityPosition.y)

		if (deathChunk ~= nil) then
		    -- drop death pheromone where unit died
		    deathScent(deathChunk)
		    
		    if ((event.force ~= nil) and (event.force.name == "player")) then
			local evolutionFactor = game.evolution_factor

			if (deathChunk[MOVEMENT_PHEROMONE] < -(evolutionFactor * RETREAT_MOVEMENT_PHEROMONE_LEVEL)) then
			    if (natives.retreats >= 0) then
				natives.retreats = natives.retreats - 1
				retreatUnits(deathChunk, 
					     convertUnitGroupToSquad(natives, 
								     entity.unit_group),
					     regionMap, 
					     surface, 
					     natives)
			    end
			    local rallyThreshold = BASE_RALLY_CHANCE + (evolutionFactor * BONUS_RALLY_CHANCE)
			    if (natives.rallyCries >= 0) and (math.random() < rallyThreshold) then
				natives.rallyCries = natives.rallyCries - 1
				rallyUnits(deathChunk,
					   regionMap,
					   surface,
					   natives,
					   evolutionFactor)
			    end
			end
		    end
                end
                
                -- removeScout(entity, natives)
            elseif (entity.type == "unit-spawner") or (entity.type == "turret") then
                addRemoveEntity(regionMap, entity, natives, false, false)
            end
        elseif (entity.force.name == "player") then
	    local creditNatives = false
	    if (event.force ~= nil) and (event.force.name == "enemy") then
		creditNatives = true
	    end
	    addRemoveEntity(regionMap, entity, natives, false, creditNatives)
        end
    end
end

local function onSurfaceTileChange(event)
    -- local player = game.players[event.player_index]
    -- if (player.surface.index==1) then
    -- aiBuilding.fillTunnel(global.regionMap, player.surface, global.natives, event.positions)
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

remote.add_interface("rampant", {
			 test1 = tests.test1,
			 test2 = tests.test2,
			 test3 = tests.test3,
			 test4 = tests.test4,
			 test5 = tests.test5,
			 test6 = tests.test6,
			 test7 = tests.test7,
			 test8 = tests.test8,
			 test9 = tests.test9,
			 test10 = tests.test10,
			 test11 = tests.test11
})
