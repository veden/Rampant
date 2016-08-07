local ai = {}

local retreatNeighbors = {1,2,3,4,5,6,7,8} -- used to minimize garbage generation
local retreatPosition = {x=0, y=0} -- used to minimize garbage generation

local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")

function ai.attackPlayerNearNest(regionMap, surface, natives, players)
    local ENEMY_BASE_PHEROMONE = constants.ENEMY_BASE_PHEROMONE
    
    for i=1,#players do
        local player = players[i]
        local chunk = mapUtils.getChunkByPosition(regionMap, player.position.x, player.position.y)
        if (chunk ~= nil) and (chunk[ENEMY_BASE_PHEROMONE] > 125) then -- TODO scaled base
            local enemies = surface.find_enemy_units(player.position, 
                                                     30)
            if (#enemies > 0) then
                local unitGroup
                local enemyIndex = 1
                -- while (enemyIndex <= #enemies) and (unitGroup == nil) do
                    -- local enemy = enemies[enemyIndex]
                    -- if (enemy.unit_group ~= nil) thena
                        -- unitGroup = enemy.unit_group
                    -- end
                    -- enemyIndex = enemyIndex + 1
                -- end
                if (unitGroup == nil) then
                    unitGroup = surface.create_unit_group({position=enemies[1].position})
                    natives.squads[#natives.squads+1] = unitGroup
                end
                
                for x=1,#enemies do
                    local enemy = enemies[x]
                    if (enemy.unit_group == nil) then
                        unitGroup.add_member(enemy)
                        -- enemy.set_command({type=defines.command.group,
                                           -- group=unitGroup,
                                           -- distraction=defines.distraction.none})
                    end
                end
                
                if (unitGroup.state == defines.group_state.gathering) then
                    unitGroup.set_command({type=defines.command.attack,
                                           target=player.character})
                end
                unitGroup.start_moving()
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
            enemiesToSquad = surface.find_enemy_units(position, 20)
            if (#enemiesToSquad > 0) then
                performRetreat = true
            end
        elseif (squad ~= nil) and squad.group.valid and (squad.status ~= constants.SQUAD_RETREATING) and (squad.status ~= constants.SQUAD_SUICIDE) then
            if (#squad.group.members == 0) then
                squad.group.destroy()
            else
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
                    
                    local dangerScore = neighborChunk[DEATH_PHEROMONE] + surface.get_pollution(retreatPosition) + neighborChunk[PLAYER_PHEROMONE] - neighborChunk[ENEMY_BASE_PHEROMONE]
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
            local newSquad = unitGroupUtils.findNearBySquad(natives, 
                                                            retreatPosition, 
                                                            18)
            if (newSquad == nil) then
                newSquad = unitGroupUtils.createSquad(retreatPosition, surface, natives)
            end
            if (enemiesToSquad ~= nil) then
                unitGroupUtils.membersToSquad(newSquad, enemiesToSquad, false)
            else
                unitGroupUtils.membersToSquad(newSquad, squad.group.members, true)
            end
            newSquad.status = constants.SQUAD_RETREATING
            newSquad.cycles = 5
            -- unitGroupUtils.setSquadCommand(newSquad,
                                           -- {type=defines.command.go_to_location,
                                            -- destination=retreatPosition,
                                            -- distraction=defines.distraction.by_enemy},
                                           -- constants.SQUAD_RETREATING)
        end
    end
end

-- function ai.sendScouts(regionMap, surface, natives, chunk, neighbors, validNeighbors)
    
    -- return validNeighbors
-- end

return ai