local baseProcessor = {}

-- imports

local BASE_QUEUE_SIZE = constants.BASE_QUEUE_SIZE

-- imported functions

local mMin = math.min

-- module code

function baseProcessor.processBases(regionMap, surface, pendingStack, natives, tick)
    local baseIndex = natives.baseIndex
    local bases = natives.bases

    local endIndex = mMin(baseIndex+BASE_QUEUE_SIZE, #bases)
    for index = baseIndex, endIndex do
        local base = bases[index]
	
    end

    if (endIndex == #bases) then
	natives.baseIndex = 1
    else
	natives.baseIndex = endIndex + 1
    end
end


return baseProcessor
