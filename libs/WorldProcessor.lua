local worldProcessor = {}

-- imports

local constants = require("Constants")

-- constants

local ITEM_COLLECTOR_DISTANCE = constants.ITEM_COLLECTOR_DISTANCE

local ITEM_COLLECTOR_QUEUE_SIZE = constants.ITEM_COLLECTOR_QUEUE_SIZE

-- imported functions

-- module code

function worldProcessor.processWorld(surface, world, tick)
    local collectors = world.itemCollectorEvents
    if (#collectors > 0) then
	local collectorLookup = world.itemCollectorLookup

	local inserter = {name="", count=0}
	local topLeftPosition = {x = 0, y = 0}
	local bottomRightPosition = {x = 0, y = 0}
	local boundingArea = {topLeftPosition,
			      bottomRightPosition}

	local count = 0
	for index = #collectors, 1, -1 do
	    count = count + 1
	    local itemCollectorPair = collectorLookup[collectors[index]]
	    collectors[index] = nil

	    if itemCollectorPair then
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
			    local itemName = item.stack.name
			    if not counts[itemName] then
				counts[itemName] = {item}
			    else
				counts[itemName][#counts[itemName]+1] = item
			    end
			end
			for k,a in pairs(counts) do
			    inserter.name = k
			    inserter.count = #a
			    local stored = chest.insert(inserter)
			    for i=1,stored do
				a[i].destroy()
			    end
			end
			-- dish.surface.create_entity({name="item-collector-base-particle-rampant",
			-- 			    position=dish.position})
		    end
		end
	    end
	    if (count >= ITEM_COLLECTOR_QUEUE_SIZE) then
		return
	    end
	end
    end
end


return worldProcessor
