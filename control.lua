local chunkUtils = require("libs/ChunkUtils")
local mapUtils = require("libs/MapUtils")
local unitGroupUtils = require("libs/UnitGroupUtils")
local chunkProcessor = require("libs/ChunkProcessor")
local mapProcessor = require("libs/MapProcessor")
local constants = require("libs/Constants")
local pheromoneUtils = require("libs/PheromoneUtils")
local ai = require("libs/AI")
local tests = require("Tests")

local mapRoutine --coroutine holding state of in progress processing
local chunkRoutine

-- local players 
local regionMap -- chunk based map
local pendingChunks -- pending chunks to be processed
local natives -- units that are being commanded
local surface -- main game surface


-- hook functions

function onInit()
    print("init")
    global.regionMap = {}
    global.pendingChunks = {}
    global.natives = {}
    
    regionMap = global.regionMap
    pendingChunks = global.pendingChunks
    natives = global.natives
    natives.squads = {}
    natives.bases = {}
    
    -- game.map_settings.enemy_expansion.enabled = false
    
    -- turn off enemy ai
    -- game.surfaces[1].peaceful_mode = true
    -- remove enemies that aren't off
    -- game.forces.enemy.kill_all_units()
    
    -- queue all current chunks that wont be generated during play
    surface = game.surfaces[1]
    for chunk in surface.get_chunks() do
        onChunkGenerated({surface=surface, 
                          area={left_top={x=chunk.x * 32,
                                          y=chunk.y * 32}}})
    end
end

function onLoad()
    print("load")
    regionMap = global.regionMap
    pendingChunks = global.pendingChunks
    natives = global.natives
end

function onChunkGenerated(event)
    -- queue generated chunk for delayed processing, queuing is required because some mods (RSO) mess with chunk as they
    -- are generated, which messes up the scoring.
    if (event.surface.index == 1) then
        pendingChunks[#pendingChunks+1] = event
    end
end

function onTick(event)
    if (event.tick % 40 == 0) then       
        -- using coroutines to keep the cpu load time managable will still being able to work large maps
        local working, errorMsg = true, nil
        if (chunkRoutine ~= nil) and (coroutine.status(chunkRoutine) ~= "dead") then
            working, errorMsg = coroutine.resume(chunkRoutine)
        elseif (#pendingChunks > 0) then
            -- coroutines start suspended, so you have to resume them after creation
            chunkRoutine = coroutine.create(chunkProcessor.processPendingChunks)
            working, errorMsg = coroutine.resume(chunkRoutine, regionMap, surface, natives, pendingChunks)
        end
        if not working then
            error(errorMsg)
        end
        
        -- put down player pheromone for player hunters
        pheromoneUtils.playerScent(regionMap, game.players)
        
        unitGroupUtils.regroupSquads(natives)
        
        ai.squadAttackPlayer(regionMap, surface, natives, game.players)
        
        if (mapRoutine ~= nil) and (coroutine.status(mapRoutine) ~= "dead") then
            working, errorMsg = coroutine.resume(mapRoutine)
        elseif (mapRoutine == nil) or (coroutine.status(mapRoutine) == "dead") then
            mapRoutine = coroutine.create(mapProcessor.processMap)
            working, errorMsg = coroutine.resume(mapRoutine, regionMap, surface, natives)
        end
        if not working then
            error(errorMsg)
        end
    end
end

function onDeath(event)
    local entity = event.entity
    if (entity.force.name == "enemy") and (entity.type == "unit") then
        local entityPosition = entity.position
        -- drop death pheromone where unit died
        pheromoneUtils.deathScent(regionMap, 
                                  entityPosition.x,
                                  entityPosition.y,
                                  200)
        
        local squad = unitGroupUtils.convertUnitGroupToSquad(natives, entity.unit_group)
        
        ai.retreatUnits(entityPosition, squad, regionMap, surface, natives)
    end
end

-- setup variables in the various modules
function onInitialTick(event)
    -- players = game.players
    if (surface == nil) then
        surface = game.surfaces[1]
    end
    
    game.forces.player.research_all_technologies()
    game.players[1].cheat_mode = true
    
        -- turn off enemy ai
    -- game.surfaces[1].peaceful_mode = true
    -- game.surfaces[1].peaceful_mode = false
    -- remove enemies that aren't off
    -- game.forces.enemy.kill_all_units()
    
    -- turn off base expansion
    -- game.forces.enemy.ai_controllable = false
    game.map_settings.enemy_expansion.enabled = false
    
    -- add processing handler into generated chunk event loop
    chunkProcessor.install(chunkUtils.checkChunkPassability)
    chunkProcessor.install(chunkUtils.scoreChunk)
    
    -- add processing handler into chunk map processing
    mapProcessor.install(pheromoneUtils.enemyBaseScent)
    -- mapProcessor.install(ai.sendScouts)
    mapProcessor.install(pheromoneUtils.processPheromone)
    
    -- used for debugging
    tests.initTester()
    
    -- swap to real on tick function
    script.on_event(defines.events.on_tick, onTick)
end

-- hooks

script.on_init(onInit)
script.on_load(onLoad)

script.on_event(defines.events.on_entity_died, onDeath)
script.on_event(defines.events.on_tick, onInitialTick)
script.on_event(defines.events.on_chunk_generated, onChunkGenerated)

remote.add_interface("rampant", {
                                    test1 = tests.test1,
                                    test2 = tests.test2
                                })
