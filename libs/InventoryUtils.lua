local inventoryUtils = {}

-- modules code

function inventoryUtils.swapItemInventory(inventory, src, dst)
    if inventory and inventory.valid then
	local itemCount = inventory.get_item_count(src)
	if (itemCount > 0) then
	    inventory.insert({name = dst, count = itemCount})
	    inventory.remove({name = src, count = itemCount})
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
