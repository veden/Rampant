if (unitUtilsG) then
    return unitUtilsG
end
local unitUtils = {}

-- imports

local constants = require("Constants")

-- constants

local ENERGY_THIEF_DRAIN_CRYSTALS = constants.ENERGY_THIEF_DRAIN_CRYSTALS

-- imported functions

-- module code

function unitUtils.convertTypeToDrainCrystal(evolutionFactor, entity)
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

unitUtilsG = unitUtils
return unitUtils
