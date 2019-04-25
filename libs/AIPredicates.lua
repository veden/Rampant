if (aiPredicatesG) then
    return aiPredicatesG
end
local aiPredicates = {}

-- imports

local constants = require("Constants")

-- constants

local AI_STATE_RAIDING = constants.AI_STATE_RAIDING
local AI_STATE_AGGRESSIVE = constants.AI_STATE_AGGRESSIVE
local AI_STATE_MIGRATING = constants.AI_STATE_MIGRATING
local AI_STATE_SIEGE = constants.AI_STATE_SIEGE
local AI_STATE_ONSLAUGHT = constants.AI_STATE_ONSLAUGHT

-- imported functions

-- module code

function aiPredicates.canAttack(natives, surface)
    return ((natives.state == AI_STATE_AGGRESSIVE) or (natives.state == AI_STATE_RAIDING) or (natives.state == AI_STATE_ONSLAUGHT))
        and not surface.peaceful_mode
        and ((not natives.aiNocturnalMode) or
                (natives.aiNocturnalMode and surface.darkness > 0.65))
end

function aiPredicates.canMigrate(natives, surface)
    return ((natives.state == AI_STATE_MIGRATING) or
	    (natives.state == AI_STATE_SIEGE))
        and natives.expansion
        and not surface.peaceful_mode
        and ((not natives.aiNocturnalMode) or
                (natives.aiNocturnalMode and surface.darkness > 0.65))
end

aiPredicatesG = aiPredicates
return aiPredicates
