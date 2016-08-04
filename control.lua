local chunkUtils = require("libs/ChunkUtils")
local regionUtils = require("libs/RegionUtils")
local constants = require("libs/Constants")
local ai = require("libs/AI")
local tests = require("Tests")

local pheromoneRoutine --coroutine holding state of in progress processing
local chunkRoutine

local regionMaps -- chunk based map
local chunkProcessingQueue -- pending chunks to be processed
local units -- units that are being commanded


-- hook functions

function onInit()
    print("init")
    global.regionMaps = {}
    global.chunkProcessingQueue = {}
    global.units = {}
    
    regionMaps = global.regionMaps
    chunkProcessingQueue = global.chunkProcessingQueue
    units = global.units
    
    -- turn off enemy ai
    game.surfaces[1].peaceful_mode = true
    -- remove enemies that aren't off
    game.forces.enemy.kill_all_units()
    
    -- queue all current chunks that wont be generated during play
    local surface = game.surfaces[1]
    for chunk in surface.get_chunks() do
        onChunkGenerated({surface=surface, 
                          area={left_top={x=chunk.x * 32,
                                          y=chunk.y * 32}}})
    end
end

function onLoad()
    print("load")
    regionMaps = global.regionMaps
    chunkProcessingQueue = global.chunkProcessingQueue
    units = global.units
end

function onChunkGenerated(event)
    -- queue generated chunk for delayed processing, queuing is required because some mods (RSO) mess with chunk as they
    -- are generated, which messes up the scoring.
    chunkProcessingQueue[#chunkProcessingQueue+1] = event
end

function onTick(event)
    if (event.tick % 45 == 0) then
        -- using coroutines to keep the cpu load time managable will still being able to work large maps
        
        if (#chunkProcessingQueue > 0) and ((chunkRoutine == nil) or (coroutine.status(chunkRoutine)=="dead")) then
            -- coroutines start suspended, so you have to resume them after creation
            chunkRoutine = coroutine.create(chunkUtils.chunkProcess)
        end
        if (chunkRoutine ~= nil) then
            coroutine.resume(chunkRoutine)
        end
        
        if (pheromoneRoutine == nil) or (coroutine.status(pheromoneRoutine)=="dead") then
            pheromoneRoutine = coroutine.create(regionUtils.pheromoneProcess)
        end
        if (pheromoneRoutine ~= nil) then
            coroutine.resume(pheromoneRoutine)
        end
        
        
    end
end

-- setup variables in the various modules
function onInitialTick(event)
    ai.init(regionMaps, units)
    regionUtils.init(regionMaps)
    chunkUtils.init(regionMaps, chunkProcessingQueue)
    
    -- used for debugging
    tests.initTester()
    
    -- swap to real on tick function
    script.on_event(defines.events.on_tick, onTick)
end

-- hooks

script.on_init(onInit)
script.on_load(onLoad)

script.on_event(defines.events.on_tick, onInitialTick)
script.on_event(defines.events.on_chunk_generated, onChunkGenerated)

remote.add_interface("rampant", {
                                    test1 = tests.test1
                                })
