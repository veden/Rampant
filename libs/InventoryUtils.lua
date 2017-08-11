local inventoryUtils = {}

-- imported functions

local mMin = math.min

-- modules code

function inventoryUtils.swapItemInventory(inventory, src, dst)
    if inventory and inventory.valid then
	local itemCount = inventory.get_item_count(src)
	if (itemCount > 0) then
	    inventory.remove({name = src, count = itemCount})
	    inventory.insert({name = dst, count = itemCount})
	end
    end
end

function inventoryUtils.topOffHand(inventory, handStack, src, dst)
    if inventory and inventory.valid then
	local itemCount = inventory.get_item_count(src)
	if (itemCount > 0) then
	    if handStack and handStack.valid and handStack.valid_for_read and (handStack.prototype.name == dst) then
		local remaining = mMin(itemCount, handStack.prototype.stack_size - handStack.count)
		if (remaining > 0) then
		    local stack = { name = src, count = remaining + handStack.count }
		    if (handStack.can_set_stack(stack)) and handStack.set_stack(stack) then
			inventory.remove({name = src, count = remaining})
		    end
		end
	    end
	end
    end   
end

function inventoryUtils.swapItemStack(stack, src, dst)
    if stack and stack.valid_for_read and (stack.name == src) then
    	local item = { name = dst, count = stack.count }
    	if stack.can_set_stack(item) then
    	    stack.set_stack(item)
    	end
    end
end


return inventoryUtils
