-- imports

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

local addRemoveEntity = entityUtils.addRemoveEntity

-- local references to global

local regionMap
local natives
local pendingChunks

-- hook functions

function onInit()
    -- print("init")
    global.regionMap = {}
    global.pendingChunks = {}
    global.natives = {}
    
    regionMap = global.regionMap
    natives = global.natives
    pendingChunks = global.pendingChunks
        
    onConfigChanged()
end

function onLoad()
    -- print("load")
    regionMap = global.regionMap
    natives = global.natives
    pendingChunks = global.pendingChunks
end

function onConfigChanged()
    -- print("reprocess")
    if (global.version == nil) then
        regionMap.pQ = {{}} -- processing queue
        regionMap.pI = 1 -- insertion location for chunk processing
        regionMap.pP = 1 -- index for the chunk set to process
        regionMap.pR = -1 -- current processing roll
        natives.squads = {}
        natives.scouts = {}
        natives.tunnels = {}
        natives.points = 0
        pendingChunks = {}
        
        global.version = constants.VERSION_5
    end
    if (global.version < constants.VERSION_8) then
    
        -- queue all current chunks that wont be generated during play
        local surface = game.surfaces[1]
        for chunk in surface.get_chunks() do
            onChunkGenerated({ surface = surface, 
                               area = { left_top = { x = chunk.x * 32,
                                                     y = chunk.y * 32 }}})
        end
        global.version = constants.VERSION_8
    end
end

function onChunkGenerated(event)
    -- queue generated chunk for delayed processing, queuing is required because some mods (RSO) mess with chunk as they
    -- are generated, which messes up the scoring.
    if (event.surface.index == 1) then
        pendingChunks[#pendingChunks+1] = event
    end
end

function onTick(event)
    if (event.tick % 20 == 0) then
        local surface = game.surfaces[1]
        
        if (event.tick % 40 == 0) then
            
            accumulatePoints(natives)
            
            -- put down player pheromone for player hunters
            playerScent(regionMap, game.players)
            
            regroupSquads(natives)
            
            -- scouting(regionMap, natives)
                        
            squadBeginAttack(natives, game.players, game.evolution_factor)
            squadAttackLocation(regionMap, surface, natives)
            squadAttackPlayer(regionMap, surface, natives)
        end
        
        processPendingChunks(regionMap, surface, natives, pendingChunks)
        
        processMap(regionMap, surface, natives, game.evolution_factor)
    end
end

function onBuild(event)
    addRemoveEntity(regionMap, event.created_entity, natives, true)
end

function onPickUp(event)
    addRemoveEntity(regionMap, event.entity, natives, false)
end

function onDeath(event)
    local entity = event.entity
    local surface = entity.surface
    if (surface.index == 1) then
        if (entity.force.name == "enemy") then
            if (entity.type == "unit") then
                local entityPosition = entity.position
                
                -- drop death pheromone where unit died
                deathScent(regionMap, entityPosition)
                
                if (event.force ~= nil) and (event.force.name == "player") then 
                    retreatUnits(entityPosition, 
                                 convertUnitGroupToSquad(natives, 
                                                         entity.unit_group),
                                 regionMap, 
                                 surface, 
                                 natives)
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

function onPutItem(event)
    -- local player = game.players[event.player_index]
    -- if (player.surface.index==1) then
        -- aiBuilding.fillTunnel(global.regionMap, player.surface, global.natives, event.positions)
    -- end
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
                                    test10 = tests.test10
                                })
