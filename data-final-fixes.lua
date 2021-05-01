local vanillaBuildings = require("prototypes/buildings/UpdatesVanilla")

local swarmUtils = require("prototypes/SwarmUtils")


if settings.startup["rampant--newEnemies"].value then
    swarmUtils.processFactions()
    swarmUtils.generateSpawnerProxy(data.raw["unit-spawner"]["neutral-biter-spawner-v1-t10-rampant"].result_units)
else
    swarmUtils.generateSpawnerProxy(data.raw["unit-spawner"]["biter-spawner"].result_units)
end

if settings.startup["rampant--removeBloodParticles"].value then
    local explosions = data.raw["explosion"]

    for k,v in pairs(explosions) do
        if string.find(k, "blood") then
            v["created_effect"] = nil
        end
    end
end

if settings.startup["rampant--unitSpawnerBreath"].value then
    for _, unitSpawner in pairs(data.raw["unit-spawner"]) do
        if (string.find(unitSpawner.name, "hive") or string.find(unitSpawner.name, "biter") or
            string.find(unitSpawner.name, "spitter")) then
            if not unitSpawner.flags then
                unitSpawner.flags = {}
            end
            unitSpawner.flags[#unitSpawner.flags+1] = "breaths-air"
        end
    end
end


for k, unit in pairs(data.raw["unit"]) do
    if (string.find(k, "biter") or string.find(k, "spitter")) and unit.collision_box then
        if settings.startup["rampant--enableSwarm"].value then
            unit.collision_box = {
                {unit.collision_box[1][1] * 0.20, unit.collision_box[1][2] * 0.20},
                {unit.collision_box[2][1] * 0.20, unit.collision_box[2][2] * 0.20}
            }
        end

        unit.affected_by_tiles = settings.startup["rampant--unitsAffectedByTiles"].value

        unit.ai_settings = {
            destroy_when_commands_fail = false,
            allow_try_return_to_spawner = true
        }
    end
end

if settings.startup["rampant--enableShrinkNestsAndWorms"].value then
    for k, unit in pairs(data.raw["unit-spawner"]) do
        if (string.find(k, "biter") or string.find(k, "spitter") or string.find(k, "hive")) and unit.collision_box then
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

if settings.startup["rampant--enableFadeTime"].value then
    for k, corpse in pairs(data.raw["corpse"]) do
        if (string.find(k, "biter") or string.find(k, "spitter") or string.find(k, "hive") or
            string.find(k, "worm") or string.find(k, "spawner")) then
            corpse.time_before_removed = settings.startup["rampant--unitAndSpawnerFadeTime"].value * 60
        end
    end
end

if settings.startup["rampant--addWallResistanceAcid"].value then
    vanillaBuildings.addWallAcidResistance()
end
