local aiBuilding = {}

-- imports

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")
local neighborUtils = require("NeighborUtils")

-- constants

local SQUAD_GUARDING = constants.SQUAD_GUARDING
local SQUAD_BURROWING = constants.SQUAD_BURROWING

local PLAYER_BASE_PHEROMONE = constants.PLAYER_BASE_PHEROMONE
local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
local PLAYER_DEFENSE_PHEROMONE = constants.PLAYER_DEFENSE_PHEROMONE
local ENEMY_BASE_PHEROMONE = constants.ENEMY_BASE_PHEROMONE
local DEATH_PHEROMONE = constants.DEATH_PHEROMONE

local AI_POINT_GENERATOR_AMOUNT = constants.AI_POINT_GENERATOR_AMOUNT

local ENEMY_BASE_GENERATOR = constants.ENEMY_BASE_GENERATOR

local AI_SCOUT_COST = constants.AI_SCOUT_COST
local AI_SQUAD_COST = constants.AI_SQUAD_COST
local AI_TUNNEL_COST = constants.AI_TUNNEL_COST

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
local scoreNeighbors = neighborUtils.scoreNeighbors
local createSquad = unitGroupUtils.createSquad

local tableRemove = table.remove
                          
-- module code

local function scoreUnitGroupLocation(position, squad, neighborChunk, surface)
    local attackScore = surface.get_pollution(position) + neighborChunk[PLAYER_PHEROMONE] + neighborChunk[PLAYER_DEFENSE_PHEROMONE]  
    local avoidScore = neighborChunk[DEATH_PHEROMONE] 
    return attackScore - avoidScore
end

local function validUnitGroupLocation(x, chunk, neighborChunk)
    return neighborChunk[NORTH_SOUTH_PASSABLE] and neighborChunk[EAST_WEST_PASSABLE]
end

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

function aiBuilding.makeScouts(surface, natives, chunk, evolution_factor)
    if (natives.points > AI_SCOUT_COST) then
        if (#global.natives.scouts < 5) and (math.random() < 0.05)  then -- TODO scaled with evolution factor
            local enemy = surface.find_nearest_enemy({ position = { x = chunk.pX + HALF_CHUNK_SIZE,
                                                                    y = chunk.pY + HALF_CHUNK_SIZE },
                                                       max_distance = 100})
            
            if (enemy ~= nil) and enemy.valid and (enemy.type == "unit") then
                natives.points = natives.points - AI_SCOUT_COST
                global.natives.scouts[#global.natives.scouts+1] = enemy
                -- print(enemy, enemy.unit_number)
            end
        end
    end
end

function aiBuilding.scouting(regionMap, natives)
    -- print("scouting")
    local scouts = natives.scouts
    for i=1,#scouts do 
        local scout = scouts[i]
        if scout.valid then
            -- print("scout", i, game.tick, scout, scout.has_command())
            scout.set_command({type=defines.command.attack_area,
                               destination={0,0},
                               radius=32,
                               distraction=defines.distraction.none})
        end
    end
end

function aiBuilding.formSquads(regionMap, surface, natives, chunk, evolution_factor)
    if (natives.points > AI_SQUAD_COST) then
        local score = chunk[PLAYER_BASE_PHEROMONE] + chunk[PLAYER_PHEROMONE] + chunk[PLAYER_DEFENSE_PHEROMONE] + surface.get_pollution({chunk.pX, chunk.pY})
        if (score > 20) and (chunk[ENEMY_BASE_GENERATOR] ~= 0) and (#natives.squads < AI_MAX_SQUAD_COUNT * evolution_factor) and (math.random() < 0.03) then
            local squadPath, squadScore = scoreNeighbors(chunk,
                                                         getNeighborChunks(regionMap, chunk.cX, chunk.cY),
                                                         validUnitGroupLocation,
                                                         scoreUnitGroupLocation,
                                                         nil,
                                                         surface)
            if (squadPath ~= nil) and (squadScore > 0) then
                local squadPosition = {}
                squadPosition.x = squadPath.pX + HALF_CHUNK_SIZE
                squadPosition.y = squadPath.pY + HALF_CHUNK_SIZE
                
                local squad = createSquad(squadPosition, surface, natives)
                
                local foundUnits = surface.set_multi_command({command = { type = COMMAND_GROUP,
                                                                          group = squad.group,
                                                                          distraction = DISTRACTION_BY_DAMAGE },
                                                              unit_count = AI_MAX_SQUAD_SIZE * evolution_factor,
                                                              unit_search_distance = (CHUNK_SIZE*2) })
                if (foundUnits > 0) then
                    natives.points = natives.points - AI_SQUAD_COST
                end
            end
        end
    end
end

return aiBuilding