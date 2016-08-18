local unitGroupUtils = {}

local utils = require("Utils")
local constants = require("Constants")

local groupingCommand = {type=defines.command.group,
                         group=1,
                         distraction=0}

function unitGroupUtils.findNearBySquad(natives, position, distance, filter)
    local getDistance = utils.euclideanDistanceNamed
    local squads = natives.squads
    local i = 1
    while (i <= #squads) do
        local squad = squads[i]
        local unitGroup = squad.group
        if (unitGroup ~= nil) and unitGroup.valid and ((filter == nil) or (filter ~= nil and filter[squad.status])) then
            if (getDistance(unitGroup.position, position) <= distance) then
                return squad
            end
        end
        i = i + 1
    end
end

function unitGroupUtils.createSquad(position, surface, natives)
    local unitGroup = surface.create_unit_group({position=position})
    
    local squad = { group = unitGroup, 
                    status = constants.SQUAD_GUARDING,
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
                groupingCommand.group = group
                groupingCommand.distraction = distraction
                member.set_command(groupingCommand)
            end
        end
    end
end

function unitGroupUtils.convertUnitGroupToSquad(natives, unitGroup)
    local returnSquad
    if (unitGroup ~= nil) then
        local squads = natives.squads
        local addUnitGroup = true
        local i = 1
        while (i <= #squads) and addUnitGroup do
            local squad = squads[i]
            if (squad.group == unitGroup) then  
                addUnitGroup = false
                returnSquad = squad
            end
            i = i + 1
        end
        if addUnitGroup then
            returnSquad = { group = unitGroup,
                            status = constants.SQUAD_GUARDING,
                            cycles = 0 }
            squads[#squads+1] = returnSquad
        end
    end
    
    return returnSquad
end

-- function unitGroupUtils.setSquadCommand(squad, command, state, cycles)
    -- local group = squad.group
    -- if (group ~= nil) and group.valid then
        -- squad.status = state
        -- squad.cycles = cycles
        -- group.set_command(command)
        -- group.start_moving()
    -- end
-- end

function unitGroupUtils.regroupSquads(natives)
    local SQUAD_RETREATING = constants.SQUAD_RETREATING
    local SQUAD_GUARDING = constants.SQUAD_GUARDING
    local findDistance = utils.euclideanDistanceNamed
    local mergeSquadMembers = unitGroupUtils.membersToSquad

    local squads = natives.squads
    for i=1,#squads do
        local squad = squads[i]
        if squad.group.valid then
            local squadPosition = squad.group.position
            for x=i+1, #squads do
                local mergeSquad = squads[x]
                local mergeGroup = mergeSquad.group
                if mergeGroup.valid and (mergeSquad.status == squad.status) and (findDistance(squadPosition, mergeGroup.position) < constants.HALF_CHUNK_SIZE) then
                    mergeSquadMembers(squad, mergeGroup.members, true)
                    mergeGroup.destroy()
                end
            end
        end
    end
    
    local squads = natives.squads
    for i=#squads,1,-1 do
        local squad = squads[i]
        if (squad.group == nil) then
            table.remove(squads, i)
        elseif not squad.group.valid then
            table.remove(squads, i)
        elseif (#squad.group.members == 0) then
            squad.group.destroy()
            table.remove(squads, i)
        else            
            if (squad.status == SQUAD_RETREATING) and (squad.cycles == 0) then
                squad.status = SQUAD_GUARDING
            elseif (squad.cycles > 0) then
                squad.cycles = squad.cycles - 1
            end
        end
    end
end

return unitGroupUtils