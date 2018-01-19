local stringUtils = require("libs/StringUtils")
local vanillaBuildings = require("prototypes/buildings/UpdatesVanilla")

local isRampant = stringUtils.isRampant

if settings.startup["rampant-newEnemies"].value then
    for k,v in pairs(data.raw["unit-spawner"]) do
	if not isRampant(k) then
	    v.autoplace = nil
	end
    end
    for k,v in pairs(data.raw["turret"]) do
	if not isRampant(k) then
	    v.autoplace = nil
	end
    end
end

if settings.startup["rampant-enableSwarm"] then    
    for k, unit in pairs(data.raw["unit"]) do
	if (string.find(k, "biter") or string.find(k, "spitter")) and unit.collision_box then
	    unit.collision_box = {
		{unit.collision_box[1][1] * 0.40, unit.collision_box[1][2] * 0.40},
		{unit.collision_box[2][1] * 0.40, unit.collision_box[2][2] * 0.40}
	    }
	end
    end
end

if settings.startup["rampant-addWallResistanceAcid"].value then
    vanillaBuildings.addWallAcidResistance()
end
