require "libs/chunkUtils"

local pheromoneMaps
local regionMaps

-- hook functions

function onInit()
    global.regionMaps = {}
    regionMaps = global.regionMaps
    local mapSettings = game.surfaces[1].map_gen_settings
    local surfaceSettings = {
                                
                            }
    surfaceSettings.autoplace_controls = {}
    game.create_surface("death", surfaceSettings)
end

function onLoad()
    regionMaps = global.regionMaps
end

function onChunkGenerated(event)
    local surface = event.surface
    
    if (regionMaps[surface.index] == nil) then
        regionMaps[surface.index] = {}
    end
    
    addChunkToRegionMap(regionMaps[surface.index],
                        event.area.left_top.x, 
                        event.area.left_top.y, 
                        surface)
end

-- function onTick(event)
    -- if (event.tick % 360 == 0) then
        -- print("here")
        -- local playerPosition = game.players[1].position
        -- local entites = game.surfaces[1].find_entities_filtered({type="unit-spawner",
                                                                 -- area={{x = playerPosition.x - 208, y = playerPosition.y - 208},
                                                                       -- {x = playerPosition.x + 208, y = playerPosition.y + 208}},
                                                                 -- force="enemy"})
        -- print("done")
    -- end
-- end

-- hooks

function onTick()
    --tester()
    script.on_event(defines.events.on_tick, nil)
end

script.on_init(onInit)
script.on_load(onLoad)

script.on_event(defines.events.on_tick, onTick)
--remote.add_interface("rampant", {showGrid=showStandardGrid})
script.on_event(defines.events.on_chunk_generated, onChunkGenerated)

-- aux

-- function printDebug(o)
    -- if (type(o) == "table") then
        -- print(serpent.dump(o))
    -- else
        -- print(o)
    -- end
-- end