local vanillaBuildings = require("prototypes/buildings/UpdatesVanilla")

local biterFunctions = require("prototypes/utils/BiterUtils")

local swarmUtils = require("prototypes/SwarmUtils")

swarmUtils.processFactions()

if settings.startup["rampant-newEnemies"].value then
    
    -- require("prototypes/Decaying")
    -- require("prototypes/Undying")

end

if settings.startup["rampant-removeBloodParticles"].value then
    local explosions = data.raw["explosion"]

    for k,v in pairs(explosions) do
        if string.find(k, "blood") then
            v["created_effect"] = nil
        end
    end
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
                {unit.collision_box[1][1] * 0.20, unit.collision_box[1][2] * 0.20},
                {unit.collision_box[2][1] * 0.20, unit.collision_box[2][2] * 0.20}
            }

            if not settings.startup["rampant-newEnemies"].value then
                unit.affected_by_tiles = true

                unit.ai_settings = { destroy_when_commands_fail = false, allow_try_return_to_spawner = true, path_resolution_modifier = -5, do_seperation = true }
            end            
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
