local vanillaBuildings = require("prototypes/buildings/UpdatesVanilla")

if settings.startup["rampant-newEnemies"].value then
    require("prototypes/Neutral")
    require("prototypes/Acid")
    require("prototypes/Physical")
    require("prototypes/Suicide")
    require("prototypes/Fire")
    require("prototypes/Electric")
    require("prototypes/Nuclear")
    require("prototypes/Inferno")
    require("prototypes/Fast")
    require("prototypes/Troll")
    -- require("prototypes/Decaying")
    -- require("prototypes/Poison")
    -- require("prototypes/Undying")
    require("prototypes/Spawner")
    require("prototypes/Wasp")
    require("prototypes/Laser")
    
    for _, unitSpawner in pairs(data.raw["unit-spawner"]) do
	if (unitSpawner.name ~= "biter-spawner") then
	    unitSpawner.autoplace = nil 
	end
    end
    for _, unitSpawner in pairs(data.raw["turret"]) do
	if (unitSpawner.name ~= "small-worm-turret") then
	    unitSpawner.autoplace = nil 
	end
    end
end

-- for k, wall in pairs(data.raw["wall"]) do
--     if not wall.collision_mask then
-- 	wall.collision_mask = {"player-layer", "item-layer", "object-layer" }
--     end
--     wall.collision_mask[#wall.collision_mask+1] = "layer-11"
--     print(serpent.dump(wall.collision_mask))
-- end

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
