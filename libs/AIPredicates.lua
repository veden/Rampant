local aiPredicates = {}

-- imports

local constants = require("Constants")

-- constants

local AI_STATE_RAIDING = constants.AI_STATE_RAIDING
local AI_STATE_AGGRESSIVE = constants.AI_STATE_AGGRESSIVE
local AI_STATE_MIGRATING = constants.AI_STATE_MIGRATING
local AI_STATE_SIEGE = constants.AI_STATE_SIEGE
local AI_STATE_NOCTURNAL = constants.AI_STATE_NOCTURNAL

-- imported functions

-- module code

function aiPredicates.canAttack(natives, surface)
    return ((natives.state == AI_STATE_AGGRESSIVE) or
	    aiPredicates.canAttackDark(natives, surface) or
	    (natives.state == AI_STATE_RAIDING)) and not surface.peaceful_mode
end

function aiPredicates.canMigrate(natives, surface) 
    return ((natives.state == AI_STATE_MIGRATING) or
	    (natives.state == AI_STATE_SIEGE) or
	    aiPredicates.canAttackDark(natives, surface)) and natives.expansion
end

function aiPredicates.isDark(surface)
    return surface.darkness > 0.65
end

function aiPredicates.canAttackDark(natives, surface)
    return aiPredicates.isDark(surface) and natives.state == AI_STATE_NOCTURNAL
end


return aiPredicates
