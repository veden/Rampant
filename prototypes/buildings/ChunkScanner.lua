local biterFunctions = require("prototypes/utils/BiterUtils")
local constants = require("libs/Constants")

data:extend({
        biterFunctions.makeBiter("chunk-scanner-squad",
                                 {
                                     scale=15,
                                     movement=1,
                                     effectiveLevel=1
                                 },
                                 biterFunctions.createMeleeAttack({
                                         radius=1,
                                         damage=1,
                                         scale=15,
                                         effectiveLevel=1
                                 }),
                                 {}),

        biterFunctions.makeBiter("chunk-scanner-squad-movement",
                                 {
                                     scale=2.5,
                                     movement=1,
                                     effectiveLevel=1
                                 },
                                 biterFunctions.createMeleeAttack({
                                         radius=1,
                                         damage=1,
                                         scale=1,
                                         effectiveLevel=1
                                 }),
                                 {})
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

for t=1,11 do
    local scale = scales[t] * 1.2
    data:extend(
        {
            {
                type = "simple-entity-with-force",
                name = "chunk-scanner-" .. t .. "-nest-rampant",
                icon = "__base__/graphics/icons/steel-chest.png",
                icon_size = 32,
                flags = {},
                order = "s-e-w-f",
                collision_mask = {"player-layer", "object-layer", "water-tile"},
                minable = nil,
                max_health = 100,
                corpse = nil,
                collision_box = {{-3 * scale, -2 * scale}, {2 * scale, 2 * scale}},
                selection_box = {{-3 * scale, -2 * scale}, {2 * scale, 2 * scale}},
                picture =
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

-- local types = constants.FACTION_TYPES

local subTypes = constants.HIVE_BUILDINGS_TYPES

-- for x=1,#types do
--     local name = types[x]

for t=1,11 do
    local scale = scales[t] * 1.2

    for si=1,#subTypes do
        local st = subTypes[si]

        data:extend(
            {
                {
                    type = "simple-entity-with-force",
                    name = "entity-proxy-" .. st .. "-t" .. t .. "-rampant",
                    icon = "__base__/graphics/icons/steel-chest.png",
                    icon_size = 32,
                    flags = {},
                    order = "s-e-w-f",
                    collision_mask = {"player-layer", "object-layer", "water-tile"},
                    minable = nil,
                    max_health = 100,
                    corpse = nil,
                    collision_box = {{-3 * scale, -2 * scale}, {2 * scale, 2 * scale}},
                    selection_box = {{-3 * scale, -2 * scale}, {2 * scale, 2 * scale}},
                    picture =
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
-- end
