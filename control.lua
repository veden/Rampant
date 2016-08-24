-- imports

local chunkUtils = require("libs/ChunkUtils")
local mapUtils = require("libs/MapUtils")
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

local accumulatePoints = aiBuilding.accumulatePoints
local removeScout = aiBuilding.removeScout
local scouting = aiBuilding.scouting

local playerScent = pheromoneUtils.playerScent
local deathScent = pheromoneUtils.deathScent

local regroupSquads = unitGroupUtils.regroupSquads
local convertUnitGroupToSquad = unitGroupUtils.convertUnitGroupToSquad

local squadAttackPlayer = aiAttack.squadAttackPlayer
local squadAttackLocation = aiAttack.squadAttackLocation
local squadBeginAttack = aiAttack.squadBeginAttack

local retreatUnits = aiDefense.retreatUnits

local addRemoveObject = mapUtils.addRemoveObject

-- hook functions

function onInit()
    -- print("init")
    global.regionMap = {}
    global.pendingChunks = {}
    global.natives = {}
    
    global.regionMap.pQ = {{}} -- processing queue
    global.regionMap.pI = 1 -- insertion location for chunk processing
    global.regionMap.pP = 1 -- index for the chunk set to process
    global.regionMap.pR = -1 -- current processing roll
    global.natives.squads = {}
    global.natives.scouts = {}
    global.natives.tunnels = {}
    global.natives.points = 0
    
    -- game.forces.player.research_all_technologies()
    
    -- queue all current chunks that wont be generated during play
    local surface = game.surfaces[1]
    for chunk in surface.get_chunks() do
        onChunkGenerated({ surface = surface, 
                           area = { left_top = { x = chunk.x * 32,
                                                 y = chunk.y * 32 }}})
    end
end

function onLoad()
    -- print("load")
    -- regionMap = global.regionMap
    -- pendingChunks = global.pendingChunks
    -- natives = global.natives
end

function onChunkGenerated(event)
    -- queue generated chunk for delayed processing, queuing is required because some mods (RSO) mess with chunk as they
    -- are generated, which messes up the scoring.
    if (event.surface.index == 1) then
        global.pendingChunks[#global.pendingChunks+1] = event
    end
end

function onTick(event)
    if (event.tick % 20 == 0) then
        local surface = game.surfaces[1]
        
        if (event.tick % 40 == 0) then
            -- if not game.players[1].cheat_mode then
                -- game.players[1].cheat_mode = true
            -- end
            
            accumulatePoints(global.natives)
            
            -- put down player pheromone for player hunters
            playerScent(global.regionMap, game.players)
            
            regroupSquads(global.natives)
                    
            -- scouting(regionMap, surface, natives)
            
            squadAttackPlayer(global.natives, game.players)
            
            squadBeginAttack(global.natives)
            squadAttackLocation(global.regionMap, surface, global.natives)
        end
        
        processPendingChunks(global.regionMap, surface, global.natives, global.pendingChunks)
        
        processMap(global.regionMap, surface, global.natives, game.evolution_factor)
    end
end

function onBuild(event)
    addRemoveObject(global.regionMap, event.created_entity, global.natives, true)
end

function onPickUp(event)
    addRemoveObject(global.regionMap, event.entity, global.natives, false)
end

function onDeath(event)
    local entity = event.entity
    if (entity.surface.index == 1) then
        if (entity.force.name == "enemy") then
            if (entity.type == "unit") then
                local entityPosition = entity.position
                
                -- drop death pheromone where unit died
                deathScent(global.regionMap, entityPosition.x, entityPosition.y)
                
                if (event.force ~= nil) and (event.force.name == "player") then
                    local squad = convertUnitGroupToSquad(global.natives, entity.unit_group)
                    retreatUnits(entityPosition, squad, global.regionMap, game.surfaces[1], global.natives)
                end
                
                -- removeScout(entity, global.natives)
            elseif (entity.type == "unit-spawner") then
                addRemoveObject(global.regionMap, entity, global.natives, false)
            end
        elseif (entity.force.name == "player") then
            addRemoveObject(global.regionMap, entity, global.natives, false)
        end
    end
end

function onPutItem(event)
    -- local player = game.players[event.player_index]
    -- if (player.surface.index==1) then
        -- aiBuilding.fillTunnel(global.regionMap, player.surface, global.natives, event.positions)
    -- end
end

-- hooks

script.on_init(onInit)
script.on_load(onLoad)

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
                                    test8 = tests.test8
                                })
