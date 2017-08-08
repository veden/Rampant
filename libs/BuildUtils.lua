local buildUtils = {}

-- module code

function buildUtils.buildComplexEntity(entity, world)
    if (entity.name == "item-collector-base-overlay-rampant") or (entity.name == "item-collector-base-rampant") then
	local surface = entity.surface
	local x = entity.position.x
	local y = entity.position.y
	local chest
	if (entity.name == "item-collector-base-overlay-rampant") then
	    local position = entity.position
	    local force = entity.force
	    entity.destroy()
	    chest = surface.create_entity({name = "item-collector-chest-rampant",
					   position = position,
					   force = force})
	    entity = surface.create_entity({name="item-collector-base-rampant",
					    position = position,
					    force = force})
	else	    
	    local ghosts = surface.find_entities_filtered({name="entity-ghost",
							   area={{x=x-1,
								  y=y-1},
							       {x=x+1,
								y=y+1}},
							   limit=1})
	    if (#ghosts > 0) then
		local ghost = ghosts[1]
		if (ghost.ghost_name == "item-collector-chest-rampant") then
		    local conflicts
		    conflicts, chest = ghost.revive()
		end
	    end
	end
	if chest and chest.valid then
	    local pair = { chest, entity }
	    world.itemCollectorLookup[chest.unit_number] = pair
	    world.itemCollectorLookup[entity.unit_number] = pair
	    entity.backer_name = ""
	end
    end
    return entity
end

function buildUtils.mineComplexEntity(entity, world, destroyed)
    if (entity.name == "item-collector-chest-rampant") or (entity.name == "item-collector-base-rampant") then
	local pair = world.itemCollectorLookup[entity.unit_number]
	if pair then
	    local chest = pair[1]
	    local dish = pair[2]

	    if chest and chest.valid then
		world.itemCollectorLookup[chest.unit_number] = nil
		if destroyed and (entity == chest) then
		    chest.destroy()
		elseif (entity ~= chest) then
		    chest.destroy()
		end
	    end
	    if dish and dish.valid then
		world.itemCollectorLookup[dish.unit_number] = nil
		if destroyed and (entity ~= dish) then
		    dish.die()
		elseif (entity ~= dish) then
		    dish.destroy()
		end
	    end
	end
    end
    return entity
end

return buildUtils
