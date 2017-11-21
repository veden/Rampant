local aiPredicates = {}

-- imports

local constants = require("Constants")

-- constants

local AI_STATE_AGGRESSIVE = constants.AI_STATE_AGGRESSIVE
local AI_STATE_NOCTURNAL = constants.AI_STATE_NOCTURNAL

-- imported functions

-- module code

function aiPredicates.canAttack(natives, surface)
    return (((natives.state == AI_STATE_AGGRESSIVE) or aiPredicates.canAttackDark(natives, surface))
	    and not surface.peaceful_mode and (#natives.squads < natives.maxSquads))
end

function aiPredicates.isDark(surface)
    return surface.darkness > 0.65
end

function aiPredicates.canAttackDark(natives, surface)
    return aiPredicates.isDark(surface) and natives.state == AI_STATE_NOCTURNAL
end


return aiPredicates
