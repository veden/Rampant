local baseProcessor = {}

-- imports

local nestUtils = require("NestUtils")
local tendrilUtils = require("TendrilUtils")
local constants = require("Constants")

-- constants

local BASE_QUEUE_SIZE = constants.BASE_QUEUE_SIZE

-- imported functions

local mMin = math.min

local buildOrder = nestUtils.buildOrder
local advanceTendrils = tendrilUtils.advanceTendrils

-- module code

function baseProcessor.processBases(map, surface, natives, tick)
    local baseIndex = natives.baseIndex
    local bases = natives.bases

    local endIndex = mMin(baseIndex+BASE_QUEUE_SIZE, #bases)
    for index = baseIndex, endIndex do
        local base = bases[index]

	
	-- buildOrder(map, natives, base, surface, tick)
	-- advanceTendrils(map, base, surface, tick, natives)
    end

    if (endIndex == #bases) then
	natives.baseIndex = 1
    else
	natives.baseIndex = endIndex + 1
    end
end


return baseProcessor
