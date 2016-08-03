local chunkUtils = require("libs/ChunkUtils")
local regionUtils = require("libs/RegionUtils")
local constants = require("libs/Constants")

local pheromoneRoutine
local chunkRoutine

local regionMaps
local chunkProcessingQueue


-- hook functions

function onInit()
    global.regionMaps = {}
    global.chunkProcessingQueue = {}
    
    regionMaps = global.regionMaps
    chunkProcessingQueue = global.chunkProcessingQueue
    
    game.surfaces[1].peaceful_mode = true
end

function onLoad()
    regionMaps = global.regionMaps
    chunkProcessingQueue = global.chunkProcessingQueue
end

function onChunkGenerated(event)
    chunkProcessingQueue[#chunkProcessingQueue+1] = event
end

function chunkProcess()
    chunkUtils.processChunks(regionMaps, chunkProcessingQueue)
end

function pheromoneProcess()
    local player = game.players[1]
    regionUtils.placePheromone(regionMaps[player.surface.index], player.position.x, player.position.y, constants.PLAYER_PHEROMONE, 100)
    regionUtils.processPheromone(regionMaps[player.surface.index], player.surface, 2)
end

function onTick(event)
    if (event.tick % 45 == 0) then
        if (#chunkProcessingQueue > 0) and ((chunkRoutine == nil) or (coroutine.status(chunkRoutine)=="dead")) then
            chunkRoutine = coroutine.create(chunkProcess)
        end
        coroutine.resume(chunkRoutine)
        
        if (pheromoneRoutine == nil) or (coroutine.status(pheromoneRoutine)=="dead") then
            pheromoneRoutine = coroutine.create(pheromoneProcess)
        end
        coroutine.resume(pheromoneRoutine)
    end
end

-- function pheromones(event)
    -- local player = event.player
    
-- end

-- hooks

function onInitialTick(event)
    -- initTester()
    --tester()
    script.on_event(defines.events.on_tick, onTick)
end

script.on_init(onInit)
script.on_load(onLoad)

script.on_event(defines.events.on_tick, onInitialTick)
script.on_event(defines.events.on_chunk_generated, onChunkGenerated)

remote.add_interface("rampant", {
                                    test1 = function () 
                                                local player = game.players[1]
                                                local regionMap = regionMaps[player.surface.index]
                                                local playerChunkX = math.floor(player.position.x / 32)
                                                local playerChunkY = math.floor(player.position.y / 32)
                                                print("------")
                                                print(playerChunkX .. ", " .. playerChunkY)
                                                print("--")
                                                for x=playerChunkX-3, playerChunkX+3 do
                                                    for y=playerChunkY-3, playerChunkY+3 do
                                                        if (regionMap[x] ~= nil) then
                                                            local chunk = regionMap[x][y]
                                                            if (chunk ~= nil) then
                                                                print(serpent.dump(chunk))
                                                            end
                                                        end
                                                    end
                                                end
                                            end,
                                    test2 = test2,
                                    test3 = test3,
                                })

-- aux

-- function printDebug(o)
    -- if (type(o) == "table") then
        -- print(serpent.dump(o))
    -- else
        -- print(o)
    -- end
-- end