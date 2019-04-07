local vanillaBuildings = require("prototypes/buildings/UpdatesVanilla")

local neutral = require("prototypes/Neutral")
local acid = require("prototypes/Acid")
local physical = require("prototypes/Physical")
local suicide = require("prototypes/Suicide")
local fire = require("prototypes/Fire")
local electric = require("prototypes/Electric")
local nuclear = require("prototypes/Nuclear")
local inferno = require("prototypes/Inferno")
local fast = require("prototypes/Fast")
local troll = require("prototypes/Troll")
local poison = require("prototypes/Poison")
local spawner = require("prototypes/Spawner")
local wasp = require("prototypes/Wasp")
local laser = require("prototypes/Laser")
local energyThief = require("prototypes/EnergyThief")
-- require("prototypes/Decaying")
-- require("prototypes/Undying")

if settings.startup["rampant-newEnemies"].value then

    neutral.addFaction()
    if settings.startup["rampant-acidEnemy"].value then
        acid.addFaction()
    end
    if settings.startup["rampant-physicalEnemy"].value then
        physical.addFaction()
    end
    if settings.startup["rampant-suicideEnemy"].value then
        suicide.addFaction()
    end
    if settings.startup["rampant-fireEnemy"].value then
        fire.addFaction()
    end
    if settings.startup["rampant-electricEnemy"].value then
        electric.addFaction()
    end
    if settings.startup["rampant-nuclearEnemy"].value then
        nuclear.addFaction()
    end
    if settings.startup["rampant-infernoEnemy"].value then
        inferno.addFaction()
    end
    if settings.startup["rampant-fastEnemy"].value then
        fast.addFaction()
    end
    if settings.startup["rampant-trollEnemy"].value then
        troll.addFaction()
    end
    if settings.startup["rampant-spawnerEnemy"].value then
        spawner.addFaction()
    end
    if settings.startup["rampant-waspEnemy"].value then
        wasp.addFaction()
    end
    if settings.startup["rampant-laserEnemy"].value then
        laser.addFaction()
    end
    if settings.startup["rampant-energyThiefEnemy"].value then
        energyThief.addFaction()
    end
    if settings.startup["rampant-poisonEnemy"].value then
        poison.addFaction()
    end
    -- require("prototypes/Decaying")
    -- require("prototypes/Undying")

end

for _, unitSpawner in pairs(data.raw["unit-spawner"]) do
    if settings.startup["rampant-unitSpawnerBreath"].value then
        if not unitSpawner.flags then
            unitSpawner.flags = {}
        end
        unitSpawner.flags[#unitSpawner.flags+1] = "breaths-air"
    end
end

if settings.startup["rampant-enableSwarm"].value then
    for k, unit in pairs(data.raw["unit"]) do
	if (string.find(k, "biter") or string.find(k, "spitter")) and unit.collision_box then
	    unit.collision_box = {
		{unit.collision_box[1][1] * 0.40, unit.collision_box[1][2] * 0.40},
		{unit.collision_box[2][1] * 0.40, unit.collision_box[2][2] * 0.40}
	    }
	end
    end
end

if settings.startup["rampant-enableShrinkNestsAndWorms"].value then    
    for k, unit in pairs(data.raw["unit-spawner"]) do
	if (string.find(k, "biter") or string.find(k, "spitter")) and unit.collision_box then
	    unit.collision_box = {
		{unit.collision_box[1][1] * 0.50, unit.collision_box[1][2] * 0.50},
		{unit.collision_box[2][1] * 0.50, unit.collision_box[2][2] * 0.50}
	    }
	end
    end

    for k, unit in pairs(data.raw["turret"]) do
	if string.find(k, "worm") and unit.collision_box then
	    unit.collision_box = {
		{unit.collision_box[1][1] * 0.50, unit.collision_box[1][2] * 0.50},
		{unit.collision_box[2][1] * 0.50, unit.collision_box[2][2] * 0.50}
	    }
	end
    end    
end


if settings.startup["rampant-addWallResistanceAcid"].value then
    vanillaBuildings.addWallAcidResistance()
end
