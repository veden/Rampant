local aiBuilding = {}


--[[ not used due to being unable to stop unit group formation by the computer ai
function ai.removeScout(regionMap, surface, entity, natives)
    for i=#natives.scouts, 1, -1 do
        local scout = natives.scouts[i]
        if (scout == entity) then
            table.remove(natives.scouts, i)
        end
    end
end

function ai.sendScouts(regionMap, surface, natives, chunk, neighbors, validNeighbors)
    if (#natives.scouts < 5) then -- TODO scaled with evolution factor
        local enemy = surface.find_nearest_enemy({position={x=chunk.pX + constants.HALF_CHUNK_SIZE,
                                                            y=chunk.pY + constants.HALF_CHUNK_SIZE},
                                                  max_distance=16})
        if (enemy ~= nil) and (enemy.type == "unit") then
            natives.scouts[#natives.scouts+1] = enemy
            enemy.set_command({type=defines.command.attack,
                               target=game.players[1].character})
            -- print("scounting")
        end
    end
    return validNeighbors
end
]]--

return aiBuilding