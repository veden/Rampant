local worldProcessor = {}

-- imports

local constants = require("Constants")

-- constants

local ITEM_COLLECTOR_DISTANCE = constants.ITEM_COLLECTOR_DISTANCE

local ITEM_COLLECTOR_QUEUE_SIZE = constants.ITEM_COLLECTOR_QUEUE_SIZE

-- imported functions

local mMin = math.min

-- module code

function worldProcessor.processWorld(surface, world, tick)
    local collectorIndex = world.itemCollectorIndex
    local collectors = world.itemCollectors

    local topLeftPosition = {x = 0, y = 0}
    local bottomRightPosition = {x = 0, y = 0}
    local boundingArea = {topLeftPosition,
			  bottomRightPosition}

    print ("here")
    
    local endIndex = mMin(collectorIndex+ITEM_COLLECTOR_QUEUE_SIZE, #collectors)
    for index = collectorIndex, endIndex do
        local itemCollector = collectors[index]

	print ("Found Collector")
	
	if itemCollector.valid then
	    local collectorPosition = itemCollector.position
	    
	    topLeftPosition.x = collectorPosition.x - ITEM_COLLECTOR_DISTANCE
	    topLeftPosition.y = collectorPosition.y - ITEM_COLLECTOR_DISTANCE

	    bottomRightPosition.x = collectorPosition.x + ITEM_COLLECTOR_DISTANCE
	    bottomRightPosition.y = collectorPosition.y + ITEM_COLLECTOR_DISTANCE
	    
	    local items = surface.find_entities_filtered({area = boundingArea,
							  name = "item-on-ground"})
	    
	    if (#items > 0) then
		for x=1,#items do
		    local item = items[x]
		    item.destroy()
		end
	    end
	end
    end

    if (endIndex == #collectors) then
	world.itemCollectorIndex = 1
    else
	world.itemCollectorIndex = endIndex + 1
    end
end


return worldProcessor
