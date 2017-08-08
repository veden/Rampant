local aiPredicates = {}

-- imports

local constants = require("Constants")
local nocturnalUtils = require("NocturnalUtils")

-- constants

local AI_STATE_AGGRESSIVE = constants.AI_STATE_AGGRESSIVE

-- imported functions

local canAttackNocturnal = nocturnalUtils.canAttack

-- module code

function aiPredicates.canAttack(natives, surface)
    return (((natives.state == AI_STATE_AGGRESSIVE) or canAttackNocturnal(natives, surface))
	    and not surface.peaceful_mode and (#natives.squads < natives.maxSquads))
end

return aiPredicates
