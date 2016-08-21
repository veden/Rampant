local aiBuilding = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")

-- constants

local PLAYER_BASE_PHEROMONE = constants.PLAYER_BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local PLAYER_DEFENSE_PHEROMONE = constants.PLAYER_DEFENSE_PHEROMONE
local ENEMY_BASE_PHEROMONE = constants.ENEMY_BASE_PHEROMONE
local DEATH_PHEROMONE = constants.DEATH_PHEROMONE

local AI_POINT_GENERATOR_AMOUNT = constants.AI_POINT_GENERATOR_AMOUNT

local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR

local AI_SCOUT_COST = constants.AI_SCOUT_COST
local AI_SQUAD_COST = constants.AI_SQUAD_COST

local AI_MAX_SQUAD_COUNT = constants.AI_MAX_SQUAD_COUNT
local AI_MAX_SQUAD_SIZE = constants.AI_MAX_SQUAD_SIZE

local HALF_CHUNK_SIZE = constants.HALF_CHUNK_SIZE
local CHUNK_SIZE = constants.CHUNK_SIZE

local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER

local NORTH_SOUTH_PASSABLE = constants.NORTH_SOUTH_PASSABLE
local EAST_WEST_PASSABLE = constants.EAST_WEST_PASSABLE

local COMMAND_GROUP = defines.command.group
local DISTRACTION_BY_DAMAGE = defines.distraction.by_damage

-- imported functions

local getNeighborChunks = mapUtils.getNeighborChunks
local createSquad = unitGroupUtils.createSquad

local tableRemove = table.remove
local mRandom = math.random
                          
-- module code

function aiBuilding.accumulatePoints(natives)
    natives.points = natives.points + AI_POINT_GENERATOR_AMOUNT
end

function aiBuilding.removeScout(entity, natives)
    local scouts = natives.scouts
    for i=1, #scouts do
        local scout = scouts[i]
        if (scout == entity) then
            tableRemove(scouts, i)
            return
        end
    end
end

function aiBuilding.sendScouts(regionMap, surface, natives, chunk, neighbors, evolution_factor)
    if (natives.points > AI_SCOUT_COST) then
        local scouts = natives.scouts
        if (#scouts < 5) and (mRandom() < 0.05)  then -- TODO scaled with evolution factor
            local enemy = surface.find_nearest_enemy({ position = {x = chunk.pX + HALF_CHUNK_SIZE,
                                                                   y = chunk.pY + HALF_CHUNK_SIZE },
                                                       max_distance = HALF_CHUNK_SIZE})
            
            if (enemy ~= nil) and (enemy.type == "unit") then
                -- print("making scouts", natives.points)
                natives.points = natives.points - AI_SCOUT_COST
                scouts[#scouts+1] = enemy
            end
        end
    end
end

function aiBuilding.scouting(regionMap, surface, natives)
    -- print("scouting")
    -- enemy.set_command({type=defines.command.attack,
                               -- target=game.players[1].character})
                               
    -- if ()
end

function aiBuilding.digTunnel(regionMap, surface, natives)

end

function aiBuilding.formSquads(regionMap, surface, natives, chunk, neighbors, evolution_factor)
    if (natives.points > AI_SQUAD_COST) then
        local score = chunk[PLAYER_BASE_PHEROMONE] + chunk[PLAYER_PHEROMONE] + chunk[PLAYER_DEFENSE_PHEROMONE] + surface.get_pollution({chunk.pX, chunk.pY})
        if (score > 20) and (chunk[ENEMY_BASE_GENERATOR] ~= 0) and (#natives.squads < AI_MAX_SQUAD_COUNT * evolution_factor) and (mRandom() < 0.03) then
            local squadNeighbors = getNeighborChunks(regionMap, 
                                                     chunk.cX, 
                                                     chunk.cY)
            local squadPath
            local squadScore = -MAGIC_MAXIMUM_NUMBER
            local squadDirection
            local squadPosition = {x = 0, y = 0}
            for i=1, #squadNeighbors do
                local neighborChunk = squadNeighbors[i]
                if (neighborChunk ~= nil) and (neighborChunk[ENEMY_BASE_GENERATOR] == 0) then
                    squadPosition.x = neighborChunk.pX
                    squadPosition.y = neighborChunk.pY
                    if (neighborChunk[NORTH_SOUTH_PASSABLE] and neighborChunk[EAST_WEST_PASSABLE]) then
                        local attackScore = surface.get_pollution(squadPosition) + neighborChunk[PLAYER_PHEROMONE] + neighborChunk[PLAYER_DEFENSE_PHEROMONE]
                        local avoidScore = neighborChunk[DEATH_PHEROMONE] -- + neighborChunk[ENEMY_BASE_PHEROMONE]
                        local score = attackScore - avoidScore
                        if (squadScore < attackScore) then
                            squadScore = attackScore
                            squadPath = neighborChunk
                            squadDirection = i
                        end
                    end
                end
            end
            
            if (squadPath ~= nil) and (squadScore > 0) then
                squadPosition.x = squadPath.pX + HALF_CHUNK_SIZE
                squadPosition.y = squadPath.pY + HALF_CHUNK_SIZE
                
                local squad = createSquad(squadPosition, surface, natives)
                
                local foundUnits = surface.set_multi_command({command = { type = COMMAND_GROUP,
                                                                          group = squad.group,
                                                                          distraction = DISTRACTION_BY_DAMAGE },
                                                              unit_count = AI_MAX_SQUAD_SIZE * evolution_factor,
                                                              unit_search_distance = (CHUNK_SIZE*2) })
                if (foundUnits > 0) then
                    -- print("making squad", natives.points, foundUnits, squadPath.cX, squadPath.cY)
                    natives.points = natives.points - AI_SQUAD_COST
                end
            end
        end
    end
end

return aiBuilding