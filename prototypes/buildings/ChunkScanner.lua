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

local scales = {
    [1] = 0.9,
    [2] = 1.1,
    [3] = 1.2,
    [4] = 1.3,
    [5] = 1.4,
    [6] = 1.5,
    [7] = 1.6,
    [8] = 1.8,
    [9] = 2.0,
    [10] = 2.1,
    [11] = 2.2
}

local subTypes = constants.HIVE_BUILDINGS_TYPES

for t=1,11 do
    local scale = scales[t] * 1.2

    for si=1,#subTypes do
        local st = subTypes[si]

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
                    collision_box = {{-3 * scale, -2 * scale}, {2 * scale, 2 * scale}},
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
