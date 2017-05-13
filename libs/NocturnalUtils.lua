local nocturnalUtils = {}

-- imports

local constants = require("Constants")

-- constants

local AI_STATE_NOCTURNAL = constants.AI_STATE_NOCTURNAL

-- module code

function nocturnalUtils.isDark(surface)
    return surface.darkness > 0.55
end

function nocturnalUtils.canAttack(natives, surface)
    return nocturnalUtils.isDark(surface) and natives.state == AI_STATE_NOCTURNAL
end

return nocturnalUtils
