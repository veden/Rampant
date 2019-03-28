


data:extend({
	{
	    type = "container",
	    name = "chunk-scanner-squad-rampant",
	    icon = "__base__/graphics/icons/wooden-chest.png",
	    icon_size = 32,
	    flags = {},
	    collision_mask = {"player-layer", "object-layer", "water-tile"},
	    collision_box = {{-6, -6}, {6, 6}},
	    selection_box = {{-6, -6}, {6, 6}},
	    minable = {mining_time = 1, result = "wooden-chest"},
	    max_health = 100,
	    corpse = "small-remnants",
	    fast_replaceable_group = "container",
	    inventory_size = 16,
	    open_sound = { filename = "__base__/sound/wooden-chest-open.ogg" },
	    close_sound = { filename = "__base__/sound/wooden-chest-close.ogg" },
	    vehicle_impact_sound =  { filename = "__base__/sound/car-wood-impact.ogg", volume = 1.0 },
	    picture =
		{
                    filename = "__base__/graphics/entity/wooden-chest/wooden-chest.png",
                    priority = "extra-high",
                    width = 32,
                    height = 36,
                    shift = util.by_pixel(0.5, -2),
                    hr_version =
                        {
                            filename = "__base__/graphics/entity/wooden-chest/hr-wooden-chest.png",
                            priority = "extra-high",
                            width = 62,
                            height = 72,
                            shift = util.by_pixel(0.5, -2),
                            scale = 0.5
                        }
                },
	    circuit_wire_connection_point = circuit_connector_definitions["chest"].points,
	    circuit_connector_sprites = circuit_connector_definitions["chest"].sprites,
	    circuit_wire_max_distance = default_circuit_wire_max_distance
        },
        {
	    type = "container",
	    name = "chunk-scanner-squad-movement-rampant",
	    icon = "__base__/graphics/icons/wooden-chest.png",
	    icon_size = 32,
	    flags = {},
	    collision_mask = {"water-tile"},
	    collision_box = {{-1, -1}, {1, 1}},
	    selection_box = {{-1, -1}, {1, 1}},
	    minable = {mining_time = 1, result = "wooden-chest"},
	    max_health = 100,
	    corpse = "small-remnants",
	    fast_replaceable_group = "container",
	    inventory_size = 16,
	    open_sound = { filename = "__base__/sound/wooden-chest-open.ogg" },
	    close_sound = { filename = "__base__/sound/wooden-chest-close.ogg" },
	    vehicle_impact_sound =  { filename = "__base__/sound/car-wood-impact.ogg", volume = 1.0 },
	    picture =
		{
                    filename = "__base__/graphics/entity/wooden-chest/wooden-chest.png",
                    priority = "extra-high",
                    width = 32,
                    height = 36,
                    shift = util.by_pixel(0.5, -2),
                    hr_version =
                        {
                            filename = "__base__/graphics/entity/wooden-chest/hr-wooden-chest.png",
                            priority = "extra-high",
                            width = 62,
                            height = 72,
                            shift = util.by_pixel(0.5, -2),
                            scale = 0.5
                        }
                },
	    circuit_wire_connection_point = circuit_connector_definitions["chest"].points,
	    circuit_connector_sprites = circuit_connector_definitions["chest"].sprites,
	    circuit_wire_max_distance = default_circuit_wire_max_distance
        }        
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
    local scale = scales[t] * 1.5
    data:extend(
        {
            {
                type = "container",
                name = "chunk-scanner-" .. t .. "-nest-rampant",
                icon = "__base__/graphics/icons/wooden-chest.png",
                icon_size = 32,
                flags = {},
                collision_mask = {"player-layer", "object-layer", "water-tile"},
                collision_box = {{-3 * scale, -2 * scale}, {2 * scale, 2 * scale}},
                selection_box = {{-6, -6}, {6, 6}},
                minable = {mining_time = 1, result = "wooden-chest"},
                max_health = 100,
                corpse = "small-remnants",
                fast_replaceable_group = "container",
                inventory_size = 16,
                open_sound = { filename = "__base__/sound/wooden-chest-open.ogg" },
                close_sound = { filename = "__base__/sound/wooden-chest-close.ogg" },
                vehicle_impact_sound =  { filename = "__base__/sound/car-wood-impact.ogg", volume = 1.0 },
                picture =
                    {
                        filename = "__base__/graphics/entity/wooden-chest/wooden-chest.png",
                        priority = "extra-high",
                        width = 32,
                        height = 36,
                        shift = util.by_pixel(0.5, -2),
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/wooden-chest/hr-wooden-chest.png",
                                priority = "extra-high",
                                width = 62,
                                height = 72,
                                shift = util.by_pixel(0.5, -2),
                                scale = 0.5
                            }
                    },
                circuit_wire_connection_point = circuit_connector_definitions["chest"].points,
                circuit_connector_sprites = circuit_connector_definitions["chest"].sprites,
                circuit_wire_max_distance = default_circuit_wire_max_distance
            }-- ,
            -- {
            --     type = "container",
            --     name = "chunk-scanner-" .. t .. "-worm-rampant",
            --     icon = "__base__/graphics/icons/wooden-chest.png",
            --     icon_size = 32,
            --     flags = {},
            --     collision_mask = {"player-layer", "object-layer", "water-tile"},
            --     collision_box = {{-1.1 * scale, -1 * scale}, {1.1 * scale, 1 * scale}},
            --     selection_box = {{-3, -3}, {3, 3}},
            --     minable = {mining_time = 1, result = "wooden-chest"},
            --     max_health = 100,
            --     corpse = "small-remnants",
            --     fast_replaceable_group = "container",
            --     inventory_size = 16,
            --     open_sound = { filename = "__base__/sound/wooden-chest-open.ogg" },
            --     close_sound = { filename = "__base__/sound/wooden-chest-close.ogg" },
            --     vehicle_impact_sound =  { filename = "__base__/sound/car-wood-impact.ogg", volume = 1.0 },
            --     picture =
            --         {
            --             filename = "__base__/graphics/entity/wooden-chest/wooden-chest.png",
            --             priority = "extra-high",
            --             width = 32,
            --             height = 36,
            --             shift = util.by_pixel(0.5, -2),
            --             hr_version =
            --                 {
            --                     filename = "__base__/graphics/entity/wooden-chest/hr-wooden-chest.png",
            --                     priority = "extra-high",
            --                     width = 62,
            --                     height = 72,
            --                     shift = util.by_pixel(0.5, -2),
            --                     scale = 0.5
            --                 }
            --         },
            --     circuit_wire_connection_point = circuit_connector_definitions["chest"].points,
            --     circuit_connector_sprites = circuit_connector_definitions["chest"].sprites,
            --     circuit_wire_max_distance = default_circuit_wire_max_distance
            -- }
        }
    )
end
