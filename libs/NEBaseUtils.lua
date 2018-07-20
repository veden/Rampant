local ne = {}

-- imports

local constants = require("Constants")

-- imported constants

local EVOLUTION_INCREMENTS = constants.EVOLUTION_INCREMENTS
local BASE_ALIGNMENT_NE = constants.BASE_ALIGNMENT_NE
local ENABLED_BOBS_UNITS = constants.ENABLED_BOBS_UNITS

-- imported functions

local mFloor = math.floor

-- module code

local function fileEntity(baseAlignment, entity, evolutionTable, evo)
    local evoRequirement = mFloor((evo or entity.prototype.build_base_evolution_requirement)/EVOLUTION_INCREMENTS) * EVOLUTION_INCREMENTS
    local eTable = evolutionTable[baseAlignment]
    if not eTable then
	eTable = {}
	evolutionTable[baseAlignment] = eTable
    end
    local aTable = eTable[evoRequirement]
    if not aTable then
	aTable = {}
	eTable[evoRequirement] = aTable
    end
    aTable[#aTable+1] = entity.name
end

function ne.processNEUnitClass(natives, surface)	
    local position = { x = 0, y = 0 }

    if settings.startup["NE_Blue_Spawners"].value then    
	local entity = surface.create_entity({
		name = "ne-spawner-blue",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE_BLUE, entity, natives.evolutionTableUnitSpawner, 0.0)
	entity.destroy()
    end

    if settings.startup["NE_Red_Spawners"].value then
		local entity = surface.create_entity({
		name = "ne-spawner-red",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE_RED, entity, natives.evolutionTableUnitSpawner, 0.0)
	entity.destroy()
    end


    if ENABLED_BOBS_UNITS then
	entity = surface.create_entity({
		name = "bob-biter-spawner",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE, entity, natives.evolutionTableUnitSpawner, 0.0)
	entity.destroy()

	entity = surface.create_entity({
		name = "bob-spitter-spawner",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE, entity, natives.evolutionTableUnitSpawner, 0.0)
	entity.destroy()

	entity = surface.create_entity({
		name = "bob-big-fire-worm-turret",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE, entity, natives.evolutionTableWorm)
	entity.destroy()

	entity = surface.create_entity({
		name = "bob-big-poison-worm-turret",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE, entity, natives.evolutionTableWorm)
	entity.destroy()

	entity = surface.create_entity({
		name = "bob-big-piercing-worm-turret",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE, entity, natives.evolutionTableWorm)
	entity.destroy()

	entity = surface.create_entity({
		name = "bob-big-electric-worm-turret",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE, entity, natives.evolutionTableWorm)
	entity.destroy()

	entity = surface.create_entity({
		name = "bob-giant-worm-turret",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE, entity, natives.evolutionTableWorm)
	entity.destroy()

	entity = surface.create_entity({
		name = "bob-behemoth-worm-turret",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE, entity, natives.evolutionTableWorm)
	entity.destroy()

	entity = surface.create_entity({
		name = "bob-big-explosive-worm-turret",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE, entity, natives.evolutionTableWorm)
	entity.destroy()   

    end

    entity = surface.create_entity({
	    name = "small-worm-turret",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_NE, entity, natives.evolutionTableWorm)
    entity.destroy()

    entity = surface.create_entity({
	    name = "medium-worm-turret",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_NE, entity, natives.evolutionTableWorm)
    entity.destroy()

    entity = surface.create_entity({
	    name = "big-worm-turret",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_NE, entity, natives.evolutionTableWorm)
    entity.destroy()    
end

return ne
