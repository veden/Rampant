local tests = {}

local constants = require("libs/Constants")

function tests.test1() 
    local player = game.player.character
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
                    local str = ""
                    for i=1,#chunk do
                        str = str .. " " .. tostring(i) .. "/" .. tostring(chunk[i])
                    end
                    if (chunk.cX == playerChunkX) and (chunk.cY == playerChunkY) then
                        print("*", chunk.cX, chunk.cY, str)
                    else
                        print(chunk.cX, chunk.cY, str)
                    end
                    -- print(str)
                    print("-")
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
    print(global.natives.points, game.tick)
end

function tests.test6()
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    game.surfaces[1].set_tiles({{name="fillableDirt", position={chunkX-1, chunkY-1}},
                                {name="fillableDirt", position={chunkX, chunkY-1}},
                                {name="fillableDirt", position={chunkX-1, chunkY}},
                                {name="fillableDirt", position={chunkX, chunkY}}}, 
                               false)
end

function tests.test7()
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    game.surfaces[1].create_entity({name="tunnel-entrance", position={chunkX, chunkY}})
end

function tests.test8(x)
    local playerPosition = game.players[1].position
    local chunkX = math.floor(playerPosition.x * 0.03125) * 32
    local chunkY = math.floor(playerPosition.y * 0.03125) * 32
    game.surfaces[1].create_entity({name=x, force="enemy", position={chunkX, chunkY}})
end

function tests.test9()
    local enemy = game.surfaces[1].find_nearest_enemy({position={0,0},
                                                       max_distance = 1000})
    if (enemy ~= nil) and enemy.valid then
        print(enemy, enemy.unit_number)
        enemy.set_command({type=defines.command.attack_area,
                           destination={0,0},
                           radius=32})
    end
end

function tests.test10()
    game.players[1].cheat_mode = true
    game.forces.player.research_all_technologies()
end

return tests