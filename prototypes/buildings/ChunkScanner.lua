local biterFunctions = require("prototypes/utils/BiterUtils")


data:extend({
        biterFunctions.makeBiter("chunk-scanner-squad",
                                 {
                                     scale=15,
                                     movement=1
                                 },
                                 biterFunctions.createMeleeAttack({
                                         radius=1,
                                         damage=1,
                                         scale=15
                                 }),
                                 {}),

        biterFunctions.makeBiter("chunk-scanner-squad-movement",
                                 {
                                     scale=2.5,
                                     movement=1
                                 },
                                 biterFunctions.createMeleeAttack({
                                         radius=1,
                                         damage=1,
                                         scale=1
                                 }),
                                 {})                       
})

local scales = {
    [1] = 0.7,
    [2] = 0.8,
    [3] = 0.9,
    [4] = 1,
    [5] = 1.1,
    [6] = 1.2,
    [7] = 1.3,
    [8] = 1.4,
    [9] = 1.5,
    [10] = 1.6,
    [11] = 1.7
}

for t=1,11 do
    local scale = scales[t] * 1.65
    data:extend(
        {
            {
                type = "simple-entity-with-force",
                name = "chunk-scanner-" .. t .. "-nest-rampant",
                -- render_layer = "object",
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
            -- {
            --     type = "container",
            --     name = "chunk-scanner-" .. t .. "-nest-rampant",
            --     -- icon = "__base__/graphics/icons/wooden-chest.png",
            --     -- icon_size = 32,
            --     flags = {},
            --     collision_mask = {"player-layer", "object-layer", "water-tile"},
            --     collision_box = {{-3 * scale, -2 * scale}, {2 * scale, 2 * scale}},
            --     selection_box = {{-3 * scale, -2 * scale}, {2 * scale, 2 * scale}},
            --     minable = nil,
            --     max_health = 100,
            --     corpse = nil,
            --     fast_replaceable_group = "container",
            --     inventory_size = 16,
            --     open_sound = nil,
            --     close_sound = nil,
            --     vehicle_impact_sound =  nil,
            --     picture =
            --         {
            --             filename = "__core__/graphics/empty.png",
            --             priority = "extra-high",
            --             width = 1,
            --             height = 1,
            --             shift = util.by_pixel(0.5, -2)
            --         },
            --     circuit_wire_connection_point = circuit_connector_definitions["chest"].points,
            --     circuit_connector_sprites = circuit_connector_definitions["chest"].sprites,
            --     circuit_wire_max_distance = default_circuit_wire_max_distance
            -- }
        }
    )
end

local types = {
    "neutral",
    "acid",
    "physical",
    "electric",
    "suicide",
    "nuclear",
    "fire",
    "inferno",
    "troll",
    "fast",
    "laser",
    "wasp",
    "spawner",
    "energy-thief",
    "poison"
}

local subTypes = {
    "trap",
    "turret",
    "utility",
    "spawner",
    "hive",
    
}

for x=1,#types do
    local name = types[x]
    
    for t=1,11 do        
        local scale = scales[t] * 1.65

        for si=1,#subTypes do
            local st = subTypes[si]

            data:extend(
                {
                    --     {
                    --         type = "container",
                    --         name = "chunk-scanner-" .. t .. "-" .. name .. "-nest-rampant",
                    --         -- icon = "__base__/graphics/icons/wooden-chest.png",
                    --         -- icon_size = 32,
                    --         flags = {},
                    --         collision_mask = {"player-layer", "object-layer", "water-tile"},
                    --         collision_box = {{-3 * scale, -2 * scale}, {2 * scale, 2 * scale}},
                    --         selection_box = {{-3 * scale, -2 * scale}, {2 * scale, 2 * scale}},
                    --         minable = nil,
                    --         max_health = 1,
                    --         corpse = nil,
                    --         fast_replaceable_group = "container",
                    --         inventory_size = 1,
                    --         open_sound = nil,
                    --         close_sound = nil,
                    --         vehicle_impact_sound = nil,
                    --         picture =
                    --             {
                    --                 filename = "__core__/graphics/empty.png",
                    --                 priority = "extra-high",
                    --                 width = 1,
                    --                 height = 1
                    --             },
                    --         circuit_wire_connection_point = circuit_connector_definitions["chest"].points,
                    --         circuit_connector_sprites = circuit_connector_definitions["chest"].sprites,
                    --         circuit_wire_max_distance = default_circuit_wire_max_distance
                    --     }
                    {
                        type = "simple-entity-with-force",
                        name = "entity-proxy-" .. t .. "-" .. name .. "-" .. st .. "-rampant",
                        -- render_layer = "object",
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
end
