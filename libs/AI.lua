local ai = {}

local retreatNeighbors = {1,2,3,4,5,6,7,8} -- used to minimize garbage generation
local retreatPosition = {x=0, y=0} -- used to minimize garbage generation

local constants = require("Constants")
local mapUtils = require("MapUtils")
local utils = require("Utils")
local unitGroupUtils = require("UnitGroupUtils")

function ai.squadAttackPlayer(regionMap, surface, natives, players)
    local SQUAD_RETREATING = constants.SQUAD_RETREATING
    local SQUAD_ATTACKING = constants.SQUAD_ATTACKING
    local SQUAD_GUARDING = constants.SQUAD_GUARDING
    local SQUAD_SUICIDE = constants.SQUAD_SUICIDE
    local MAGIC_MAXIMUM_NUMBER = constants.MAGIC_MAXIMUM_NUMBER
    local findDistance = utils.euclideanDistanceNamed
    
    local squads = natives.squads
    
    for si=1, #squads do
        local squad = squads[si]
        if (squad.group.valid) and (squad.status == SQUAD_GUARDING) then
            local closestPlayer
            local closestDistance = MAGIC_MAXIMUM_NUMBER
            for pi=1, #players do
                local playerCharacer = players[pi].character
                local distance = findDistance(playerCharacer.position, squad.group.position)
                if (distance < closestDistance) then
                    closestPlayer = playerCharacer
                    closestDistance = distance
                end
            end
            if (closestDistance < 60) then
                local squadType = SQUAD_ATTACKING
                if (math.random() < 0.10) then -- TODO add sliding scale based on number of members and evolution
                    squadType = SQUAD_SUICIDE
                end
                unitGroupUtils.setSquadCommand(squad,
                                               {type=defines.command.attack,
                                                target=closestPlayer},
                                               squadType,
                                               0)
            end
        end
    end
end

function ai.retreatUnits(position, squad, regionMap, surface, natives)
    local DEATH_PHEROMONE = constants.DEATH_PHEROMONE
    
    local chunk = mapUtils.getChunkByPosition(regionMap, position.x, position.y)
    if (chunk ~= nil) and (chunk[DEATH_PHEROMONE] > 1500) then -- TODO sliding scale of death based on evolution
        local PLAYER_PHEROMONE = constants.PLAYER_PHEROMONE
        local ENEMY_BASE_PHEROMONE = constants.ENEMY_BASE_PHEROMONE
    
        local performRetreat = false
        local enemiesToSquad
        
        if (squad == nil) then
            enemiesToSquad = surface.find_enemy_units(position, 15)
            if (#enemiesToSquad > 0) then
                performRetreat = true
            end
        elseif (squad ~= nil) and squad.group.valid and (squad.status ~= constants.SQUAD_RETREATING) and (squad.status ~= constants.SQUAD_SUICIDE) then
            if (#squad.group.members ~= 0) then
                performRetreat = true
            end
        end
                
        if performRetreat then
            mapUtils.getNeighborChunks(regionMap, 
                                       chunk.cX, 
                                       chunk.cY, 
                                       retreatNeighbors)
            local exitPath
            local exitScore = constants.MAGIC_MAXIMUM_NUMBER
            local exitDirection
            for i=1, 8 do
                local neighborChunk = retreatNeighbors[i]
                if (neighborChunk ~= nil) then
                    retreatPosition.x = neighborChunk.pX
                    retreatPosition.y = neighborChunk.pY
                    
                    local dangerScore = neighborChunk[DEATH_PHEROMONE] + surface.get_pollution(retreatPosition) + neighborChunk[PLAYER_PHEROMONE] + 
                                        -- putting a unit group in a nest causing pathing and disbanding issues
                                        (neighborChunk.bG * 200) - neighborChunk[ENEMY_BASE_PHEROMONE]
                    if (dangerScore < exitScore) then
                        exitScore = dangerScore
                        exitPath = neighborChunk
                        exitDirection = i
                    end
                end
            end
            
            -- center position in chunk for retreat
            retreatPosition.x = exitPath.pX + constants.HALF_CHUNK_SIZE
            retreatPosition.y = exitPath.pY + constants.HALF_CHUNK_SIZE
            
            -- in order for units in a group attacking to retreat, we have to create a new group and give the command to join
            -- to each unit
            local filter = {}
            filter[constants.SQUAD_RETREATING] = true
            local newSquad = unitGroupUtils.findNearBySquad(natives, 
                                                            retreatPosition,
                                                            18,
                                                            filter)
            
            if (newSquad == nil) then
                newSquad = unitGroupUtils.createSquad(retreatPosition, surface, natives)
                newSquad.status = constants.SQUAD_RETREATING
                newSquad.cycles = 2
            end
            if (enemiesToSquad ~= nil) then
                unitGroupUtils.membersToSquad(newSquad, enemiesToSquad, false)
            else
                unitGroupUtils.membersToSquad(newSquad, squad.group.members, true)
            end
        end
    end
end

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
return ai