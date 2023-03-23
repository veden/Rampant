-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

local vanillaBuildings = require("prototypes/buildings/UpdatesVanilla")
local constants = require("libs/Constants")

if settings.startup["rampant--newEnemies"].value and mods["SchallAlienLoot"] then
    local SizeLootRampant = {1, 2, 3, 3, 4, 4, 4, 5, 5, 6}
    for _,faction in pairs(constants.FACTION_SET) do
        for v=1,settings.startup["rampant--newEnemyVariations"].value do
            for factionSize = 1, constants.TIERS do
                local effectiveLevel = constants.TIER_UPGRADE_SET[factionSize]
                SchallAlienLoot_add_spawner(faction.type.."-hive-v"..v.."-t"..factionSize.."-rampant")
                SchallAlienLoot_add_spawner(faction.type.."-spitter-spawner-v"..v.."-t"..factionSize.."-rampant")
                SchallAlienLoot_add_spawner(faction.type.."-biter-spawner-v"..v.."-t"..factionSize.."-rampant")
                SchallAlienLoot_add_worm(faction.type.."-worm-v"..v.."-t"..factionSize.."-rampant", factionSize)
                SchallAlienLoot_add_mover(faction.type.."-spitter-v"..v.."-t"..factionSize.."-rampant", SizeLootRampant[effectiveLevel])
                SchallAlienLoot_add_mover(faction.type.."-biter-v"..v.."-t"..factionSize.."-rampant", SizeLootRampant[effectiveLevel])
            end
        end
    end
end

for _, projectile in pairs(data.raw["projectile"]) do
    if not projectile.hit_collision_mask then
        projectile.hit_collision_mask = {
            "player-layer",
            "train-layer",
            RampantGlobalVariables.projectileCollisionLayer
        }
    else
        projectile.hit_collision_mask[#projectile.hit_collision_mask+1] = RampantGlobalVariables.projectileCollisionLayer
    end
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
                {unit.collision_box[1][1] * 0.70, unit.collision_box[1][2] * 0.70},
                {unit.collision_box[2][1] * 0.70, unit.collision_box[2][2] * 0.70}
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
                {unit.collision_box[1][1] * 0.75, unit.collision_box[1][2] * 0.75},
                {unit.collision_box[2][1] * 0.75, unit.collision_box[2][2] * 0.75}
            }
        end
    end

    for k, unit in pairs(data.raw["turret"]) do
        if string.find(k, "worm") and unit.collision_box then
            unit.collision_box = {
                {unit.collision_box[1][1] * 0.75, unit.collision_box[1][2] * 0.75},
                {unit.collision_box[2][1] * 0.75, unit.collision_box[2][2] * 0.75}
            }
        end
    end
end

if settings.startup["rampant--enableLandfillOnDeath"].value then
    local particles = {
        "guts-entrails-particle-small-medium",
        "guts-entrails-particle-big"
    }

    for _,particleName in pairs(particles) do
        data.raw["optimized-particle"][particleName].ended_in_water_trigger_effect = {
            {
                type = "set-tile",
                tile_name = "landfill",
                radius = 0.5,
            },
            {
                type = "script",
                effect_id = "deathLandfillParticle--rampant"
            }
        }
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
