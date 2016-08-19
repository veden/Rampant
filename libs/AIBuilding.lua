local aiBuilding = {}

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")

local squadNeighbors = {1,2,3,4,5,6,7,8}
local squadPosition = {1,2}

local mRandom = math.random

function aiBuilding.accumulatePoints(natives)
    natives.points = natives.points + constants.AI_POINT_GENERATOR_AMOUNT
end

function aiBuilding.removeScout(entity, natives)
    for i=1, #natives.scouts do
        local scout = natives.scouts[i]
        if (scout == entity) then
            table.remove(natives.scouts, i)
        end
    end
end

function aiBuilding.sendScouts(regionMap, surface, natives, chunk, neighbors)
    if (#natives.scouts < 5) and (mRandom() < 0.05) and (natives.points > constants.AI_SCOUT_COST) then -- TODO scaled with evolution factor
        local enemy = surface.find_nearest_enemy({position={x = chunk.pX + constants.HALF_CHUNK_SIZE,
                                                            y = chunk.pY + constants.HALF_CHUNK_SIZE},
                                                  max_distance = constants.HALF_CHUNK_SIZE})
        if (enemy ~= nil) and (enemy.type == "unit") then
            print("making scouts", natives.points)
            natives.points = natives.points - constants.AI_SCOUT_COST
            natives.scouts[#natives.scouts+1] = enemy
        end
    end
end

function aiBuilding.scouting(regionMap, surface, natives)
    print("scouting")
    -- enemy.set_command({type=defines.command.attack,
                               -- target=game.players[1].character})
                               
    -- if ()
end


function aiBuilding.formSquads(regionMap, surface, natives, chunk, neighbors)
    local score = chunk[constants.PLAYER_BASE_PHEROMONE] + chunk[constants.PLAYER_PHEROMONE] + chunk[constants.PLAYER_DEFENSE_PHEROMONE]
    if (#natives.squads < constants.AI_MAX_SQUAD_COUNT * game.evolution_factor) and (mRandom() < 0.03) and (chunk[constants.ENEMY_BASE_GENERATOR] ~= 0) and (score > 30) and (natives.points > constants.AI_SQUAD_COST) then
        mapUtils.getNeighborChunks(regionMap, 
                                   chunk.cX, 
                                   chunk.cY, 
                                   squadNeighbors)
        local squadPath
        local squadScore = -constants.MAGIC_MAXIMUM_NUMBER
        local squadDirection
        for i=1, #squadNeighbors do
            local neighborChunk = squadNeighbors[i]
            if (neighborChunk ~= nil) then
                squadPosition.x = neighborChunk.pX
                squadPosition.y = neighborChunk.pY
                
                local attackScore = surface.get_pollution(squadPosition) + neighborChunk[constants.PLAYER_PHEROMONE] + neighborChunk[constants.PLAYER_DEFENSE_PHEROMONE] + neighborChunk[constants.ENEMY_BASE_PHEROMONE] - (neighborChunk[constants.ENEMY_BASE_GENERATOR] * 5) - neighborChunk[constants.DEATH_PHEROMONE]
                if (squadScore < attackScore) then
                    squadScore = attackScore
                    squadPath = neighborChunk
                    squadDirection = i
                end
            end
        end
        
        if (squadPath ~= nil) then
            squadPosition.x = squadPath.pX
            squadPosition.y = squadPath.pY
            
            local squad = unitGroupUtils.createSquad(squadPosition, surface, natives)
            
            local foundUnits = surface.set_multi_command({command={type=defines.command.group,
                                                                   group=squad.group,
                                                                   distraction=defines.distraction.by_damage},
                                                          unit_count=constants.AI_MAX_SQUAD_SIZE * game.evolution_factor,
                                                          unit_search_distance=(constants.CHUNK_SIZE*2)})
            if (foundUnits > 0) then
                print("making squad", natives.points, foundUnits, squadPath.cX, squadPath.cY)
                natives.points = natives.points - constants.AI_SQUAD_COST
            end
        end
    end
end

return aiBuilding