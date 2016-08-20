local tests = {}

local constants = require("libs/Constants")

function tests.test1() 
    local player = game.players[1]
    local playerChunkX = math.floor(player.position.x / 32)
    local playerChunkY = math.floor(player.position.y / 32)
    print("------")
    print(playerChunkX .. ", " .. playerChunkY)
    print("--")
    for x=playerChunkX-3, playerChunkX+3 do
        for y=playerChunkY-3, playerChunkY+3 do
            if (global.regionMap[x] ~= nil) then
                local chunk = global.regionMap[x][y]
                if (chunk ~= nil) then
                    print(serpent.dump(chunk))
                end
            end
        end
    end
end

function tests.test2()
    print("--")
    for i=1, #global.natives.squads do
        local squad = global.natives.squads[i]
        if squad.group.valid then
            print(math.floor(squad.group.position.x * 0.03125), math.floor(squad.group.position.y * 0.03125), squad.status, squad.group.state)
            print(serpent.dump(squad))
        end
    end
end

function tests.test3()
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    local entities = game.surfaces[1].find_entities_filtered({area={{chunkX, chunkY},
                                                                    {chunkX + constants.CHUNK_SIZE, chunkY + constants.CHUNK_SIZE}},
                                                              force="player"})
    for i=1, #entities do
        print(entities[i].name)
    end
    print("--")
end

function tests.test4()
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    local entity = game.surfaces[1].find_nearest_enemy({position={chunkX, chunkY},
                                                        max_distance=constants.CHUNK_SIZE,
                                                        force = "enemy"})
    if (entity ~= nil) then
        print(entity.name)
    end
    print("--")
end

function tests.test5()
    print(global.natives.points)
end

return tests