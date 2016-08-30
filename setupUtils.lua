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
        
        
        local groupCmd = { type = defines.command.group,
                           group = 1,
                           distraction = defines.distraction.none }
        temps[constants.GROUP_COMMAND] = groupCmd
                                           
        temps[constants.SQUAD_POSITION] = {x=0, y=0}
	
	local retreatPosition = {x=0, y=0}
        temps[constants.RETREAT_POSITION] = retreatPosition
        temps[constants.RETREAT_NEIGHBORS_WITH_DIRECTION] = {{d=1},{d=2},{d=3},{d=4},{d=5},{d=6},{d=7},{d=8}}
	-- temps[constants.RETREAT_COMMAND] = { type = defines.command.go_to_location,
	-- 				     destination = retreatPosition,
	-- 				     distraction = defines.distraction.none }
        
        temps[constants.MULTI_GROUP_COMMAND] = {command = groupCmd,
                                                unit_count = 1,
                                                unit_search_distance = (constants.CHUNK_SIZE*2) }		  

	
    end
end

return setupUtils
