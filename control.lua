-- imports

local setupUtils = require("setupUtils")
local entityUtils = require("libs/EntityUtils")
local unitGroupUtils = require("libs/UnitGroupUtils")
local chunkProcessor = require("libs/ChunkProcessor")
local mapProcessor = require("libs/MapProcessor")
local constants = require("libs/Constants")
local pheromoneUtils = require("libs/PheromoneUtils")
local aiDefense = require("libs/AIDefense")
local aiAttack = require("libs/AIAttack")
local aiBuilding = require("libs/AIBuilding")
local tests = require("tests")

-- imported functions

local processPendingChunks = chunkProcessor.processPendingChunks

local processMap = mapProcessor.processMap
local scanMap = mapProcessor.scanMap

local accumulatePoints = aiBuilding.accumulatePoints
local removeScout = aiBuilding.removeScout
-- local scouting = aiBuilding.scouting

local playerScent = pheromoneUtils.playerScent
local deathScent = pheromoneUtils.deathScent

local regroupSquads = unitGroupUtils.regroupSquads
local convertUnitGroupToSquad = unitGroupUtils.convertUnitGroupToSquad

local squadAttack = aiAttack.squadAttack
local squadBeginAttack = aiAttack.squadBeginAttack

local retreatUnits = aiDefense.retreatUnits

local addRemoveEntity = entityUtils.addRemoveEntity

-- local references to global

local regionMap
local natives
local pheromoneTotals
local pendingChunks
local temps

-- hook functions

local function onLoad()
    -- print("load")
    regionMap = global.regionMap
    natives = global.natives
    pendingChunks = global.pendingChunks
    pheromoneTotals = global.pheromoneTotals
    temps = global.temps
end

local function onChunkGenerated(event)
    -- queue generated chunk for delayed processing, queuing is required because some mods (RSO) mess with chunk as they
    -- are generated, which messes up the scoring.
    if (event.surface.index == 1) then
        pendingChunks[#pendingChunks+1] = event
    end
end

local function onConfigChanged()
    -- print("reprocess")
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
        
        regionMap.processQueue = {}
        regionMap.processPointer = 1
        regionMap.scanPointer = 1
        regionMap.processRoll = -1
        
        setupUtils.initTemps(temps)
        
        pheromoneTotals[constants.DEATH_PHEROMONE] = 0
        pheromoneTotals[constants.ENEMY_BASE_PHEROMONE] = 0
        pheromoneTotals[constants.PLAYER_PHEROMONE] = 0
        pheromoneTotals[constants.PLAYER_BASE_PHEROMONE] = 0
        pheromoneTotals[constants.PLAYER_DEFENSE_PHEROMONE] = 0
        pheromoneTotals[constants.MOVEMENT_PHEROMONE] = 0
        
        -- queue all current chunks that wont be generated during play
        local surface = game.surfaces[1]
        for chunk in surface.get_chunks() do
            onChunkGenerated({ surface = surface, 
                               area = { left_top = { x = chunk.x * 32,
                                                     y = chunk.y * 32 }}})
        end
        global.version = constants.VERSION_9
    end
end

local function onTick(event)   
    if (event.tick % 20 == 0) then
	local surface = game.surfaces[1]
        
        processPendingChunks(regionMap, surface, pendingChunks)
        scanMap(regionMap, surface)

        if (event.tick % 40 == 0) then
            
            accumulatePoints(natives)
            
            -- put down player pheromone for player hunters
            playerScent(regionMap, game.players)
            
            regroupSquads(natives, temps)
            
            -- scouting(regionMap, natives)
                        
            squadBeginAttack(natives, game.players, game.evolution_factor)
            squadAttack(regionMap, surface, natives, temps)
        end

	processMap(regionMap, surface, natives, game.evolution_factor, temps) 
    end
end

local function onBuild(event)
    addRemoveEntity(regionMap, event.created_entity, natives, true)
end

local function onPickUp(event)
    addRemoveEntity(regionMap, event.entity, natives, false)
end

local function onDeath(event)
    local entity = event.entity
    local surface = entity.surface
    if (surface.index == 1) then
        if (entity.force.name == "enemy") then
            if (entity.type == "unit") then
                local entityPosition = entity.position
                
                -- drop death pheromone where unit died
                deathScent(regionMap, entityPosition, pheromoneTotals)
                
                if (event.force ~= nil) and (event.force.name == "player") then 
                    retreatUnits(entityPosition, 
                                 convertUnitGroupToSquad(natives, 
                                                         entity.unit_group),
                                 regionMap, 
                                 surface, 
                                 natives,
                                 temps)
                end
                
                removeScout(entity, global.natives)
            elseif (entity.type == "unit-spawner") then
                addRemoveEntity(regionMap, entity, natives, false)
            end
        elseif (entity.force.name == "player") then
            addRemoveEntity(regionMap, entity, natives, false)
        end
    end
end

local function onPutItem(event)
    -- local player = game.players[event.player_index]
    -- if (player.surface.index==1) then
        -- aiBuilding.fillTunnel(global.regionMap, player.surface, global.natives, event.positions)
    -- end
end

local function onInit()
    -- print("init")
    global.regionMap = {}
    global.pendingChunks = {}
    global.natives = {}
    global.pheromoneTotals = {}
    global.temps = {}
    
    regionMap = global.regionMap
    natives = global.natives
    pendingChunks = global.pendingChunks
    pheromoneTotals = global.pheromoneTotals
    temps = global.temps
    
    onConfigChanged()
end


-- hooks

script.on_init(onInit)
script.on_load(onLoad)
script.on_configuration_changed(onConfigChanged)

script.on_event(defines.events.on_player_built_tile, onPutItem)

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
