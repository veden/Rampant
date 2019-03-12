if neBaseUtilsG then
    return neBaseUtilsG
end
local ne = {}

-- imports

local constants = require("Constants")

-- imported constants

local BASE_ALIGNMENT_NE = constants.BASE_ALIGNMENT_NE
local ENABLED_BOBS_UNITS = constants.ENABLED_BOBS_UNITS

local BASE_ALIGNMENT_NE_BLUE = constants.BASE_ALIGNMENT_NE_BLUE
local BASE_ALIGNMENT_NE_RED = constants.BASE_ALIGNMENT_NE_RED
local BASE_ALIGNMENT_NE_PINK = constants.BASE_ALIGNMENT_NE_PINK
local BASE_ALIGNMENT_NE_GREEN = constants.BASE_ALIGNMENT_NE_GREEN
local BASE_ALIGNMENT_NE_YELLOW = constants.BASE_ALIGNMENT_NE_YELLOW

-- imported functions

local mMin = math.min

-- module code

local function fileEntity(baseAlignment, entity, evolutionTable, natives, evo)
    local evoRequirement = mMin(evo or entity.prototype.build_base_evolution_requirement, 1)
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

    -- natives.enemyAlignmentLookup[entity.name] = baseAlignment
end

function ne.processNEUnitClass(natives, surface)
    local position = { x = 0, y = 0 }

    local factionSet = {}

    local entity = surface.create_entity({
	    name = "biter-spawner",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_NE, entity, natives.evolutionTableUnitSpawner, natives, 0.0)
    entity.destroy()

    entity = surface.create_entity({
	    name = "spitter-spawner",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_NE, entity, natives.evolutionTableUnitSpawner, natives, 0.0)
    entity.destroy()


    if settings.startup["NE_Blue_Spawners"].value then
	entity = surface.create_entity({
		name = "ne-spawner-blue",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE_BLUE, entity, natives.evolutionTableUnitSpawner, natives, 0.0)
	factionSet[#factionSet+1] = BASE_ALIGNMENT_NE_BLUE
	entity.destroy()
    end

    if settings.startup["NE_Red_Spawners"].value then
	entity = surface.create_entity({
		name = "ne-spawner-red",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE_RED, entity, natives.evolutionTableUnitSpawner, natives, 0.0)
	factionSet[#factionSet+1] = BASE_ALIGNMENT_NE_RED
	entity.destroy()
    end

    if settings.startup["NE_Green_Spawners"].value then
	entity = surface.create_entity({
		name = "ne-spawner-green",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE_GREEN, entity, natives.evolutionTableUnitSpawner, natives, 0.0)
	factionSet[#factionSet+1] = BASE_ALIGNMENT_NE_GREEN
	entity.destroy()
    end


    if settings.startup["NE_Yellow_Spawners"].value then
	entity = surface.create_entity({
		name = "ne-spawner-yellow",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE_YELLOW, entity, natives.evolutionTableUnitSpawner, natives, 0.0)
	factionSet[#factionSet+1] = BASE_ALIGNMENT_NE_YELLOW
	entity.destroy()
    end

    if settings.startup["NE_Pink_Spawners"].value then
	entity = surface.create_entity({
		name = "ne-spawner-pink",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE_PINK, entity, natives.evolutionTableUnitSpawner, natives, 0.0)
	factionSet[#factionSet+1] = BASE_ALIGNMENT_NE_PINK
	entity.destroy()
    end

    factionSet[#factionSet+1] = BASE_ALIGNMENT_NE

    if ENABLED_BOBS_UNITS then
	entity = surface.create_entity({
		name = "bob-biter-spawner",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE, entity, natives.evolutionTableUnitSpawner, natives, 0.0)
	entity.destroy()

	entity = surface.create_entity({
		name = "bob-spitter-spawner",
		position = position
	})
	fileEntity(BASE_ALIGNMENT_NE, entity, natives.evolutionTableUnitSpawner, natives, 0.0)
	entity.destroy()

	for _,alignment in pairs(factionSet) do
	    entity = surface.create_entity({
		    name = "bob-big-fire-worm-turret",
		    position = position
	    })
	    fileEntity(alignment, entity, natives.evolutionTableWorm, natives)
	    entity.destroy()

	    entity = surface.create_entity({
		    name = "bob-big-poison-worm-turret",
		    position = position
	    })
	    fileEntity(alignment, entity, natives.evolutionTableWorm, natives)
	    entity.destroy()

	    entity = surface.create_entity({
		    name = "bob-big-piercing-worm-turret",
		    position = position
	    })
	    fileEntity(alignment, entity, natives.evolutionTableWorm, natives)
	    entity.destroy()

	    entity = surface.create_entity({
		    name = "bob-big-electric-worm-turret",
		    position = position
	    })
	    fileEntity(alignment, entity, natives.evolutionTableWorm, natives)
	    entity.destroy()

	    entity = surface.create_entity({
		    name = "bob-giant-worm-turret",
		    position = position
	    })
	    fileEntity(alignment, entity, natives.evolutionTableWorm, natives)
	    entity.destroy()

	    entity = surface.create_entity({
		    name = "behemoth-worm-turret",
		    position = position
	    })
	    fileEntity(alignment, entity, natives.evolutionTableWorm, natives)
	    entity.destroy()

	    entity = surface.create_entity({
		    name = "bob-big-explosive-worm-turret",
		    position = position
	    })
	    fileEntity(alignment, entity, natives.evolutionTableWorm, natives)
	    entity.destroy()
	end
    end

    for _,alignment in pairs(factionSet) do
	entity = surface.create_entity({
		name = "small-worm-turret",
		position = position
	})
	fileEntity(alignment, entity, natives.evolutionTableWorm, natives)
	entity.destroy()

	entity = surface.create_entity({
		name = "medium-worm-turret",
		position = position
	})
	fileEntity(alignment, entity, natives.evolutionTableWorm, natives)
	entity.destroy()

	entity = surface.create_entity({
		name = "big-worm-turret",
		position = position
	})
	fileEntity(alignment, entity, natives.evolutionTableWorm, natives)
	entity.destroy()
    end
end

neBaseUtilsG = ne
return ne
