local ai = {}

local retreatNeighbors = {1,2,3,4,5,6,7,8}
local constants = require("Constants")
local mapUtils = require("MapUtils")
local unitGroupUtils = require("UnitGroupUtils")

--[[
function ai.attackPlayerNearNest(regionMap, surface, natives, players)
    for i=1,#players do
        local player = players[i]
        local chunk = mapUtils.getChunkByPosition(regionMap, player.position.x, player.position.y)
        if (chunk ~= nil) and (chunk[constants.BASE_PHEROMONE] > 125) and (chunk[constants.DEATH_PHEROMONE] < 800) then
            local enemies = surface.find_enemy_units(player.position, 
                                                     30)
            if (#enemies > 0) then
                local unitGroup
                local enemyIndex = 1
                while (enemyIndex <= #enemies) and (unitGroup == nil) do
                    local enemy = enemies[enemyIndex]
                    if (enemy.unit_group ~= nil) then
                        unitGroup = enemy.unit_group
                    end
                    enemyIndex = enemyIndex + 1
                end
                if (unitGroup == nil) then
                    unitGroup = surface.create_unit_group({position=enemies[1].position})
                    -- natives.squads[#natives.squads+1] = unitGroup
                end
                -- print("before " .. tostring(unitGroup.state))
                for x=1,#enemies do
                    local enemy = enemies[x]
                    if (enemy.unit_group == nil) then
                        -- unitGroup.add_member(enemy)
                        enemy.set_command({type=defines.command.group,
                                           group=unitGroup,
                                           distraction=defines.distraction.none})
                    end
                end
                -- print("after " .. tostring(unitGroup.state))
                if (unitGroup.state == defines.group_state.gathering) then
                    unitGroup.set_command({type=defines.command.attack,
                                           target=player.character})
                    unitGroup.start_moving()
                end
            end
        end
    end
end
]]--

function ai.retreatUnitGroup(position, unitGroup, regionMap, surface, natives)
    if (unitGroup ~= nil) then
        local memberCount = #unitGroup.members
        if (memberCount > 0) then
            local chunk = mapUtils.getChunkByPosition(regionMap, position.x, position.y)
            if (chunk ~= nil) then
                if (chunk[constants.DEATH_PHEROMONE] > 1500) and (memberCount > 5) then
                    mapUtils.getNeighborChunks(regionMap, 
                                               chunk.cX, 
                                               chunk.cY, 
                                               retreatNeighbors)
                    local exitPath
                    local exitScore = constants.MAX_PHEROMONE + 1
                    local exitDirection
                    for i=1, 8 do
                        local neighborChunk = retreatNeighbors[i]
                        if (neighborChunk ~= nil) then
                            if (neighborChunk[constants.DEATH_PHEROMONE] < exitScore) then
                                exitScore = neighborChunk[constants.DEATH_PHEROMONE]
                                exitPath = neighborChunk
                                exitDirection = i
                            end
                        end
                    end
                    if (exitPath ~= nil) then
                        local exitPosition = {x=exitPath.pX,
                                              y=exitPath.pY}
                        local nearGroup = unitGroupUtils.findNearByUnitGroup(natives, exitPosition, 50)
                        
                        if (nearGroup == nil) then
                            nearGroup = surface.create_unit_group({position=exitPosition})
                            natives.squads[#natives.squads+1] = nearGroup
                        end
                        for i=1, memberCount do
                            local member = unitGroup.members[i]
                            if (member ~= nil) and member.valid then
                                member.set_command({type=defines.command.group,
                                                    group=nearGroup,
                                                    distraction=defines.distraction.none})
                            end
                        end
                        -- unitGroup.destroy()
                    end
                end
            end
        elseif (memberCount == 0) then
            unitGroup.destroy()
        end
    end
end

function ai.addAutonomousUnitGroup(unitGroup, natives)
    if (unitGroup ~= nil) then
        local squads = natives.squads
        local addUnitGroup = true
        local i = 1
        while (i <= #squads) and addUnitGroup do
            local squad = squads[i]
            if (squad == unitGroup) then
                addUnitGroup = false
            end
            i = i + 1
        end
        if addUnitGroup then
            squads[#squads+1] = unitGroup
        end
    end
end

-- function ai.purgeUnitGroups(natives)
    -- for i=1, #natives.squads do
        -- local squad = natives.squads[i]
        -- if (squad == nil) then
            -- table.remove(natives.squads, i)
        -- elseif (not squad.valid) then
            -- table.remove(natives.squads, i)
        -- elseif (#squad.members == 0) then
            -- squad.destroy()
            -- table.remove(natives.squads, i)
        -- end
    -- end
-- end

-- function ai.sendScouts(regionMap, surface, natives, chunk, neighbors, validNeighbors)
    
    -- return validNeighbors
-- end

return ai