-- 2.0 has no hr_version sprites (one set, always high-res) and no "compressed" flag.
local function reduceSprites(t, seen)
    if type(t) ~= "table" or seen[t] then return end
    seen[t] = true
    if type(t.hr_version) == "table" then
        local hr = t.hr_version
        t.hr_version = nil
        for k, v in pairs(hr) do t[k] = v end
    end
    if type(t.flags) == "table" then
        for i = #t.flags, 1, -1 do
            if t.flags[i] == "compressed" then table.remove(t.flags, i) end
        end
    end
    for _, v in pairs(t) do
        if type(v) == "table" then reduceSprites(v, seen) end
    end
end

do
    local seen = {}
    for _, group in pairs(data.raw) do
        if type(group) == "table" then
            for _, proto in pairs(group) do
                if type(proto) == "table" then reduceSprites(proto, seen) end
            end
        end
    end
end

-- 2.0 collision masks are dictionaries ({ layers = { name = true } }) and the layer names
-- changed: "player-layer" -> "player", "water-tile" -> "water_tile". Convert every mask the
-- 1.1 prototypes still carry, nested ones included (action target_effects hold tile masks).
local function entityLayerName(name)
    if type(name) ~= "string" then return name end
    if name:sub(-6) == "-layer" then name = name:sub(1, -7) end
    return (name:gsub("%-", "_"))
end

local function tileLayerName(name)
    if type(name) ~= "string" then return name end
    if name:sub(-5) == "-tile" then return (name:gsub("%-", "_")) end
    return name
end

local function toLayers(mask, isTile)
    if type(mask) ~= "table" then return mask end
    local map = isTile and tileLayerName or entityLayerName
    if mask.layers then
        local mapped = {}
        for name, value in pairs(mask.layers) do mapped[map(name)] = value end
        mask.layers = mapped
        return mask
    end
    local layers = {}
    for _, entry in pairs(mask) do
        if type(entry) == "string" then
            layers[map(entry)] = true
        elseif type(entry) == "table" and entry.name then
            layers[map(entry.name)] = true
        end
    end
    if next(layers) then return { layers = layers } end
    return mask
end

local function convertMasks(t, seen)
    if type(t) ~= "table" or seen[t] then return end
    seen[t] = true
    if t.collision_mask then t.collision_mask = toLayers(t.collision_mask) end
    if t.hit_collision_mask then t.hit_collision_mask = toLayers(t.hit_collision_mask) end
    if t.tile_collision_mask then t.tile_collision_mask = toLayers(t.tile_collision_mask, true) end
    for _, v in pairs(t) do
        if type(v) == "table" then convertMasks(v, seen) end
    end
end

do
    -- fire used collision_mask as a tile mask in 1.1
    for _, fire in pairs(data.raw["fire"] or {}) do
        if fire.collision_mask and not fire.tile_collision_mask then
            fire.tile_collision_mask = toLayers(fire.collision_mask, true)
            fire.collision_mask = nil
        end
    end
    local seen = {}
    for _, group in pairs(data.raw) do
        if type(group) == "table" then
            for _, proto in pairs(group) do
                if type(proto) == "table" then convertMasks(proto, seen) end
            end
        end
    end
end

-- 2.0 collision layers are named prototypes; a mod declares the one it uses.
data:extend({ { type = "collision-layer", name = "rampant_projectile" } })

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
