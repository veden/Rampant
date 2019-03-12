if bobsBaseUnits then
    return bobsBaseUnits
end
local bobs = {}

-- imports

local constants = require("Constants")

-- imported constants

local BASE_ALIGNMENT_BOBS = constants.BASE_ALIGNMENT_BOBS

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

function bobs.processBobsUnitClass(natives, surface)
    local position = { x = 0, y = 0 }

    local entity = surface.create_entity({
	    name = "bob-biter-spawner",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_BOBS, entity, natives.evolutionTableUnitSpawner, natives, 0.0)
    entity.destroy()

    entity = surface.create_entity({
	    name = "bob-spitter-spawner",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_BOBS, entity, natives.evolutionTableUnitSpawner, natives, 0.0)
    entity.destroy()

    entity = surface.create_entity({
	    name = "small-worm-turret",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_BOBS, entity, natives.evolutionTableWorm, natives)
    entity.destroy()

    entity = surface.create_entity({
	    name = "medium-worm-turret",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_BOBS, entity, natives.evolutionTableWorm, natives)
    entity.destroy()

    entity = surface.create_entity({
	    name = "big-worm-turret",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_BOBS, entity, natives.evolutionTableWorm, natives)
    entity.destroy()

    entity = surface.create_entity({
	    name = "bob-big-explosive-worm-turret",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_BOBS, entity, natives.evolutionTableWorm, natives)
    entity.destroy()

    entity = surface.create_entity({
	    name = "bob-big-fire-worm-turret",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_BOBS, entity, natives.evolutionTableWorm, natives)
    entity.destroy()

    entity = surface.create_entity({
	    name = "bob-big-poison-worm-turret",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_BOBS, entity, natives.evolutionTableWorm, natives)
    entity.destroy()

    entity = surface.create_entity({
	    name = "bob-big-piercing-worm-turret",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_BOBS, entity, natives.evolutionTableWorm, natives)
    entity.destroy()

    entity = surface.create_entity({
	    name = "bob-big-electric-worm-turret",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_BOBS, entity, natives.evolutionTableWorm, natives)
    entity.destroy()

    entity = surface.create_entity({
	    name = "bob-giant-worm-turret",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_BOBS, entity, natives.evolutionTableWorm, natives)
    entity.destroy()

    entity = surface.create_entity({
	    name = "behemoth-worm-turret",
	    position = position
    })
    fileEntity(BASE_ALIGNMENT_BOBS, entity, natives.evolutionTableWorm, natives)
    entity.destroy()

end

bobsBaseUnits = bobs
return bobs
