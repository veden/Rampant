local tests = {}

local regionMap

function tests.initTester() 
    regionMap = global.regionMap
end

function tests.test1() 
    local player = game.players[1]
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
end

function tests.test2()
    local position = game.players[1].position
    
    local spawners = game.surfaces[1].find_entities_filtered({type="unit-spawner"})
    for i=1, #spawners do
        local spawner = spawners[i]
        if (spawner ~= nil) and spawner.valid then
            spawner.destroy()
        end
    end
    
    game.forces.enemy.kill_all_units()
    
    position.x = position.x + 10
    position.y = position.y - 40
    
    for i=position.x, position.x+30, 5 do
        game.surfaces[1].create_entity({name="biter-spawner",
                                        position={i, position.y}})
    end
    -- local playerPosition = game.players[1].position
    -- playerPosition.x = playerPosition.x + 10
    -- local turret = game.surfaces[1].create_entity({name="small-worm-turret", position=playerPosition})
    -- turret
end

-- function test3()
    -- local playerPosition = game.players[1].position
    -- decayPheromone(regionMaps[1], playerPosition.x, playerPosition.y, 3)
-- end

return tests