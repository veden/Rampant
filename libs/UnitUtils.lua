if (unitUtilsG) then
    return unitUtilsG
end
local unitUtils = {}

-- imports

local constants = require("Constants")
local chunkPropertyUtils = require("ChunkPropertyUtils")

-- constants

local DEFINES_WIRE_TYPE_RED = defines.wire_type.red
local DEFINES_WIRE_TYPE_GREEN = defines.wire_type.green

local ENERGY_THIEF_CONVERSION_TABLE = constants.ENERGY_THIEF_CONVERSION_TABLE
local ENERGY_THIEF_LOOKUP = constants.ENERGY_THIEF_LOOKUP

local ENERGY_THIEF_DRAIN_CRYSTALS = constants.ENERGY_THIEF_DRAIN_CRYSTALS

-- imported functions

local setDrainPylons = chunkPropertyUtils.setDrainPylons

-- module code

local function convertTypeToDrainCrystal(evolutionFactor, entity)
    if (entity == "pole") then
        return "crystal-drain-pole-rampant"
    else
        if (entity == "smallUnit") then
            if (evolutionFactor < 0.25) then
                return ENERGY_THIEF_DRAIN_CRYSTALS[1]
            elseif (evolutionFactor < 0.50) then
                return ENERGY_THIEF_DRAIN_CRYSTALS[2]
            elseif (evolutionFactor < 0.75) then
                return ENERGY_THIEF_DRAIN_CRYSTALS[3]
            else
                return ENERGY_THIEF_DRAIN_CRYSTALS[4]
            end
        elseif (entity == "unit") then
            if (evolutionFactor < 0.25) then
                return ENERGY_THIEF_DRAIN_CRYSTALS[4]
            elseif (evolutionFactor < 0.50) then
                return ENERGY_THIEF_DRAIN_CRYSTALS[5]
            elseif (evolutionFactor < 0.75) then
                return ENERGY_THIEF_DRAIN_CRYSTALS[6]
            else
                return ENERGY_THIEF_DRAIN_CRYSTALS[7]
            end
        else
            if (evolutionFactor < 0.25) then
                return ENERGY_THIEF_DRAIN_CRYSTALS[7]
            elseif (evolutionFactor < 0.50) then
                return ENERGY_THIEF_DRAIN_CRYSTALS[8]
            elseif (evolutionFactor < 0.75) then
                return ENERGY_THIEF_DRAIN_CRYSTALS[9]
            else
                return ENERGY_THIEF_DRAIN_CRYSTALS[10]
            end
        end
    end
end

function unitUtils.createDrainPylon(map, cause, entity, entityType)
    if ((cause and ENERGY_THIEF_LOOKUP[cause.name]) or (not cause)) then
        local conversion = ENERGY_THIEF_CONVERSION_TABLE[entityType]
        if conversion then
            local newEntity = map.surface.create_entity({
                    position=entity.position,
                    name=convertTypeToDrainCrystal(entity.force.evolution_factor, conversion),
                    direction=entity.direction
            })
            if (conversion == "pole") then
                local targetEntity = map.surface.create_entity({
                        position=entity.position,
                        name="pylon-target-rampant",
                        direction=entity.direction
                })
                targetEntity.backer_name = ""
                local wires = entity.neighbours
                if wires then
                    for _,v in pairs(wires.copper) do
                        if (v.valid) then
                            newEntity.connect_neighbour(v);
                        end
                    end
                    for _,v in pairs(wires.red) do
                        if (v.valid) then
                            newEntity.connect_neighbour({
                                    wire = DEFINES_WIRE_TYPE_RED,
                                    target_entity = v
                            });
                        end
                    end
                    for _,v in pairs(wires.green) do
                        if (v.valid) then
                            newEntity.connect_neighbour({
                                    wire = DEFINES_WIRE_TYPE_GREEN,
                                    target_entity = v
                            });
                        end
                    end
                end
                setDrainPylons(map, targetEntity, newEntity)
            elseif newEntity.backer_name then
                newEntity.backer_name = ""
            end
        end
    end
end

unitUtilsG = unitUtils
return unitUtils
