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


local biterFunctions = require("prototypes/utils/BiterUtils")
local constants = require("libs/Constants")
local smokeUtils = require("prototypes/utils/SmokeUtils")

data:extend({
        biterFunctions.makeBiter({
                name = "chunk-scanner-squad",
                scale=15,
                movement=1,
                effectiveLevel=1,
                resistances = {},
                hitSprayName = "blood-fountain-hit-spray",
                attack = biterFunctions.createMeleeAttack({
                        radius=1,
                        damage=1,
                        scale=15,
                        effectiveLevel=1
                })
        }),

        biterFunctions.makeBiter({
                name = "chunk-scanner-squad-movement",
                scale=2.5,
                movement=1,
                effectiveLevel=1,
                resistances = {},
                hitSprayName = "blood-fountain-hit-spray",
                attack = biterFunctions.createMeleeAttack({
                        radius=1,
                        damage=1,
                        scale=15,
                        effectiveLevel=1
                })
        })
})

local function generateCollisionBox(scale, entityType)
    if entityType == "turret" then
        return {
            {-1.1 * scale, -1.0 * scale},
            {1.1 * scale, 1.0 * scale}
        }
    elseif (entityType == "biter-spawner") or (entityType == "spitter-spawner") then
        return {
            {-3 * scale, -2 * scale},
            {2 * scale, 2 * scale}
        }
    elseif entityType == "hive" then
        return {
            {-3 * scale, -2 * scale},
            {2 * scale, 2 * scale}
        }
    end
end

local scales = {
    -- ["trap"] = {},
    -- ["utility"] = {},
    ["spitter-spawner"] = {
        [1] = 0.70, [2] = 0.83, [3] = 0.96, [4] = 1.09, [5] = 1.22,
        [6] = 1.35, [7] = 1.48, [8] = 1.61, [9] = 1.74, [10] = 1.87
    },
    -- ["biter-spawner"] = {
    --     [1] = 0.70, [2] = 0.83, [3] = 0.96, [4] = 1.09, [5] = 1.22,
    --     [6] = 1.35, [7] = 1.48, [8] = 1.61, [9] = 1.74, [10] = 1.87,
    --     [11] = 2.0
    -- },
    ["biter-spawner"] = {
        [1] = 0.83, [2] = 0.96, [3] = 1.09, [4] = 1.22, [5] = 1.35,
        [6] = 1.48, [7] = 1.61, [8] = 1.74, [9] = 1.87, [10] = 2.0
    },
    ["hive"] = {
        [1] = 1.35, [2] = 1.48, [3] = 1.61, [4] = 1.74, [5] = 1.87,
        [6] = 2.0, [7] = 2.13, [8] = 2.26, [9] = 2.39, [10] = 2.52
    },
    ["turret"] = {
        [1] = 0.635, [2] = 0.765, [3] = 0.895, [4] = 1.025, [5] = 1.155,
        [6] = 1.285, [7] = 1.415, [8] = 1.545, [9] = 1.675, [10] = 1.805
    }
}

local subTypes = constants.HIVE_BUILDINGS_TYPES

for si=1,#subTypes do
    local st = subTypes[si]
    if scales[st] then
        for t=1,10 do
            -- local scale = scales[st][t]
            local scale = scales["biter-spawner"][t]

            data:extend(
                {
                    {
                        type = "land-mine",
                        name = "entity-proxy-" .. st .. "-t" .. t .. "-rampant",
                        icon = "__base__/graphics/icons/steel-chest.png",
                        icon_size = 32,
                        flags = {},
                        build_base_evolution_requirement = 0.08 * (t-1),
                        order = "s-e-w-f",
                        collision_mask = {"player-layer", "object-layer", "water-tile", "train-layer"},
                        minable = nil,
                        max_health = 100,
                        corpse = nil,
                        timeout = 1,
                        trigger_radius = 0,
                        -- collision_box = generateCollisionBox(scale, st),
                        collision_box = generateCollisionBox(scale, "biter-spawner"),
                        selection_box = nil,

                        picture_safe =
                            {
                                filename = "__core__/graphics/empty.png",
                                priority = "extra-high",
                                width = 1,
                                height = 1
                            },
                        picture_set =
                            {
                                filename = "__core__/graphics/empty.png",
                                priority = "extra-high",
                                width = 1,
                                height = 1
                            }
                    }
                }
            )
        end
    end
end


smokeUtils.makeNewCloud(
    {
        name = "build-clear",
        wind = false,
        scale = 9,
        duration = 540,
        cooldown = 10,
        tint = { r=0.7, g=0.2, b=0.7 }
    },
    {
        type = "area",
        radius = 17,
        force = "not-same",
        action_delivery =
            {
                type = "instant",
                target_effects =
                    {
                        {
                            type = "damage",
                            damage = { amount = 1.1, type = "poison"}
                        },
                        {
                            type = "damage",
                            damage = { amount = 1.1, type = "acid"}
                        },
                        {
                            type = "damage",
                            damage = { amount = 1.1, type = "fire"}
                        }
                    }
            }
    }
)
