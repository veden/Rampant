local baseProcessor = {}

-- imports

local baseUtils = require("BaseUtils")
local constants = require("Constants")

-- constants

local BASE_QUEUE_SIZE = constants.BASE_QUEUE_SIZE

-- imported functions

local mMin = math.min

local buildOrder = baseUtils.buildOrder

-- module code

function baseProcessor.processBases(regionMap, surface, natives, tick)
    local baseIndex = natives.baseIndex
    local bases = natives.bases

    local endIndex = mMin(baseIndex+BASE_QUEUE_SIZE, #bases)
    for index = baseIndex, endIndex do
        local base = bases[index]

	buildOrder(regionMap, natives, base, surface, tick)
    end

    if (endIndex == #bases) then
	natives.baseIndex = 1
    else
	natives.baseIndex = endIndex + 1
    end
end


return baseProcessor
