local worldProcessor = {}

-- imports

local constants = require("Constants")

-- constants

local ITEM_COLLECTOR_DISTANCE = constants.ITEM_COLLECTOR_DISTANCE

local ITEM_COLLECTOR_QUEUE_SIZE = constants.ITEM_COLLECTOR_QUEUE_SIZE

-- imported functions

-- module code

function worldProcessor.processWorld(surface, world, tick)
    local collectorLookup = world.itemCollectorLookup
    local collectors = world.itemCollectorEvents

    local topLeftPosition = {x = 0, y = 0}
    local bottomRightPosition = {x = 0, y = 0}
    local boundingArea = {topLeftPosition,
			  bottomRightPosition}

    local count = 0
    for index = #collectors, 1, -1 do
	count = count + 1
        local itemCollectorPair = collectorLookup[collectors[index]]
	collectors[index] = nil
	local chest = itemCollectorPair[1]
	local dish = itemCollectorPair[2]
	
	if chest.valid and dish.valid then

	    local collectorPosition = dish.position
	    
	    topLeftPosition.x = collectorPosition.x - ITEM_COLLECTOR_DISTANCE
	    topLeftPosition.y = collectorPosition.y - ITEM_COLLECTOR_DISTANCE

	    bottomRightPosition.x = collectorPosition.x + ITEM_COLLECTOR_DISTANCE
	    bottomRightPosition.y = collectorPosition.y + ITEM_COLLECTOR_DISTANCE
	    
	    local items = surface.find_entities_filtered({area = boundingArea,
							  name = "item-on-ground"})
	    
	    local counts = {}
	    if (#items > 0) then
		for x=1,#items do
		    local item = items[x]
		    if not counts[item.stack.name] then
			counts[item.stack.name] = 1
		    else
			counts[item.stack.name] = counts[item.stack.name] + 1
		    end
		    item.destroy()
		end
		for k,c in pairs(counts) do
		    chest.insert({name=k, count=c})
		end
		-- dish.surface.create_entity({name="item-collector-base-particle-rampant",
		-- 			    position=dish.position})
	    end
	end
	if (count >= ITEM_COLLECTOR_QUEUE_SIZE) then
	    return
	end
    end
end


return worldProcessor
