local vanillaBuildings = require("prototypes/buildings/UpdatesVanilla")

if settings.startup["rampant-newEnemies"].value then
    for k, unitSpawner in pairs(data.raw["unit-spawner"]) do
	if (unitSpawner.name ~= "biter-spawner") then
	   unitSpawner.autoplace = nil 
	end
    end
    for k, unitSpawner in pairs(data.raw["turret"]) do
	if (unitSpawner.name ~= "small-worm-turret") then
	   unitSpawner.autoplace = nil 
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
