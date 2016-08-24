local unitGroupUtils = {}

-- imports

local mapUtils = require("MapUtils")
local constants = require("Constants")

-- constants

local MOVEMENT_PENALTY_PHEROMONE_GENERATOR_AMOUNT = constants.MOVEMENT_PENALTY_PHEROMONE_GENERATOR_AMOUNT

local GROUP_STATE_FINISHED = defines.group_state.finished

local SQUAD_RETREATING = constants.SQUAD_RETREATING
local SQUAD_GUARDING = constants.SQUAD_GUARDING

-- imported functions

local tableRemove = table.remove
local tableInsert = table.insert
local euclideanDistanceNamed = mapUtils.euclideanDistanceNamed

-- module code
                          
function unitGroupUtils.findNearBySquad(natives, position, distance, filter)
    local squads = natives.squads
    for i=1, #squads do
        local squad = squads[i]
        local unitGroup = squad.group
        if (unitGroup ~= nil) and unitGroup.valid and ((filter == nil) or (filter ~= nil and filter[squad.status])) then
            if (euclideanDistanceNamed(unitGroup.position, position) <= distance) then
                return squad
            end
        end
    end
end

function unitGroupUtils.createSquad(position, surface, natives)
    local unitGroup = surface.create_unit_group({position=position})
    
    local squad = { group = unitGroup, 
                    status = SQUAD_GUARDING,
                    penalties = {},
                    cycles = 0 }
    natives.squads[#natives.squads+1] = squad
    return squad
end

function unitGroupUtils.membersToSquad(squad, members, overwriteGroup, distraction)
    if (members ~= nil) then
        local group = squad.group
        for i=1,#members do
            local member = members[i]
            if (member ~= nil) and member.valid and (overwriteGroup or (not overwriteGroup and (member.unit_group == nil))) then
                member.set_command({ type = defines.command.group,
                                     group = group,
                                     distraction = distraction })
            end
        end
    end
end

function unitGroupUtils.convertUnitGroupToSquad(natives, unitGroup)
    local returnSquad
    if (unitGroup ~= nil) then
        local squads = natives.squads
        for i=1, #squads do
            local squad = squads[i]
            if (squad.group == unitGroup) then  
                return squad
            end
        end
        returnSquad = { group = unitGroup,
                        status = SQUAD_GUARDING,
                        penalties = {},
                        cycles = 0 }
        squads[#squads+1] = returnSquad
    end
    
    return returnSquad
end

function unitGroupUtils.addSquadMovementPenalty(squad, chunkX, chunkY)   
    local penalties = squad.penalties
    for penaltyIndex=1, #penalties do
        local penalty = squad.penalties[penaltyIndex]
        if (penalty.x == chunkX) and (penalty.y == chunkY) then
            penalty.v = penalty.v + MOVEMENT_PENALTY_PHEROMONE_GENERATOR_AMOUNT
            return
        end
    end
    if (#penalties == 10) then
        tableRemove(penalties, 10)
    end
    tableInsert(penalties, 1, { v = MOVEMENT_PENALTY_PHEROMONE_GENERATOR_AMOUNT,
                                x = chunkX,
                                y = chunkY })
end

function unitGroupUtils.lookupSquadMovementPenalty(squad, chunkX, chunkY)
    local penalties = squad.penalties
    for penaltyIndex=1, #penalties do
        local penalty = penalties[penaltyIndex]
        if (penalty.x == chunkX) and (penalty.y == chunkY) then
            return penalty.v
        end
    end
    return 0
end

function unitGroupUtils.regroupSquads(natives)

    local squads = natives.squads
    for i=1,#squads do
        local squad = squads[i]
        if squad.group.valid then
            local squadPosition = squad.group.position
            for x=i+1, #squads do
                local mergeSquad = squads[x]
                local mergeGroup = mergeSquad.group
                if mergeGroup.valid and (mergeSquad.status == squad.status) and (euclideanDistanceNamed(squadPosition, mergeGroup.position) < 16) then
                    unitGroupUtils.membersToSquad(squad, mergeGroup.members, true)
                    mergeGroup.destroy()
                end
            end
        end
    end
    
    for i=#squads,1,-1 do
        local squad = squads[i]
        if (squad.group == nil) then
            tableRemove(squads, i)
        elseif not squad.group.valid then
            tableRemove(squads, i)
        elseif (#squad.group.members == 0) then
            squad.group.destroy()
            tableRemove(squads, i)
        else            
            if (squad.status == SQUAD_RETREATING) and ((squad.cycles == 0) or (squad.group.state == GROUP_STATE_FINISHED)) then
                squad.status = SQUAD_GUARDING
                squad.cycles = 0
            elseif (squad.cycles > 0) then
                squad.cycles = squad.cycles - 1
            end
        end
    end
end

return unitGroupUtils