local setupUtils = {}

-- imports

local constants = require("libs/Constants")

-- module code

function setupUtils.initTemps(temps)
    -- minimize garbage by preallocating all tables needed for operation
    if (temps[constants.ATTACK_COMMAND] == nil) then
        local attackPosition = {x=0, y=0}
        temps[constants.ATTACK_POSITION] = attackPosition
        temps[constants.ATTACK_COMMAND] = { type = defines.command.attack_area,
                                            destination = attackPosition,
                                            radius = 32,
                                            distraction = defines.distraction.by_anything }
        temps[constants.ATTACK_DIRECTION] = {{d=1}, {d=2}, {d=3}, {d=4}}
        
        
        temps[constants.GROUP_COMMAND] = { type = defines.command.group,
                                           group = 1,
                                           distraction = defines.distraction.none }
    end
end

return setupUtils