local tests = {}

local regionMaps

function tests.initTester() 
    regionMaps = global.regionMaps
end

function tests.test1() 
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
end

-- function test2()
    -- local playerPosition = game.players[1].position
    -- spreadPheromone(regionMaps[1], playerPosition.x, playerPosition.y)
-- end

-- function test3()
    -- local playerPosition = game.players[1].position
    -- decayPheromone(regionMaps[1], playerPosition.x, playerPosition.y, 3)
-- end

return tests