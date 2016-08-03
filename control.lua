require "libs/RegionMap"
require "libs/ChunkUtils"
require "tests"

local pheromoneMaps
local regionMaps
local DEATH_PHEROMONE = 1
local BASE_PHEROMONE = 2
local PLAYER_PHEROMONE = 3

-- hook functions

function onInit()
    global.regionMaps = {}
    regionMaps = global.regionMaps
    game.surfaces[1].peaceful_mode = true
    -- game.map_settings.enemy_expansion.enabled = false
    -- game.forces.enemy.ai_controllable = false
    -- local mapSettings = game.surfaces[1].map_gen_settings
    -- local surfaceSettings = {
                                -- terrain_segmentation = mapSettings.terrain_segmentation,
                                -- water = mapSettings.water,
                                -- autoplace_controls = {},
                                -- seed = mapSettings.seed,
                                -- shift = mapSettings.shift,
                                -- width = mapSettings.width,
                                -- height = mapSettings.height,
                                -- starting_area = mapSettings.starting_area,
                                -- peaceful_mode = true
                            -- }
    -- game.create_surface("death", game.surfaces[1].map_gen_settings)
    -- game.create_surface("death1", game.surfaces[1].map_gen_settings)
    -- game.create_surface("death2", game.surfaces[1].map_gen_settings)
end

function onLoad()
    regionMaps = global.regionMaps
end

function onChunkGenerated(event)
    local surface = event.surface
    if (surface.index == 1) then
        if (regionMaps[surface.index] == nil) then
            regionMaps[surface.index] = {}
        end
        
        addChunkToRegionMap(regionMaps[surface.index],
                            event.area.left_top.x, 
                            event.area.left_top.y, 
                            surface)
    end
end

function onTick(event)
    if (event.tick % 120 == 0) then
        local player = game.players[1]
        placePheromone(regionMaps[player.surface.index], player.position.x, player.position.y, PLAYER_PHEROMONE, 100)
        processPheromone(regionMaps[player.surface.index], player.surface, 2)
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
                                    test1 = test1,
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