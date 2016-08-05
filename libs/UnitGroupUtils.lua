local unitGroupUtils = {}

local utils = require("Utils")

function unitGroupUtils.findNearByUnitGroup(natives, position, distance)
    local squads = natives.squads
    local i = 1
    while (i <= #squads) do
        local squad = squads[i]
        if (squad ~= nil) and squad.valid then
            if (utils.euclideanDistanceNamed(squad.position, position) <= distance) then
                return squad
            end
        end
        i = i + 1
    end
end

function unitGroupUtils.mergeNearByUnitGroup(natives, unitGroup)
        
end

return unitGroupUtils