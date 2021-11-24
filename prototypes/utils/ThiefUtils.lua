local thiefUtils = {}

-- module code

function thiefUtils.makeDrainCrystal(attributes)
    local name = attributes.name .. "-drain-rampant"
    local itemName = attributes.name .. "-item-drain-rampant"

    data:extend({
            {
                type = "item",
                name = itemName,
                icon = "__Rampant__/graphics/icons/thief/crystal-drain.png",
                icon_size = 32,
                flags = {"hidden"},
                subgroup = "energy",
                order = "e[accumulator]-a[accumulator]",
                place_result = name,
                stack_size = 50
            },

            {
                type = "radar",
                name = name,
                icon = "__Rampant__/graphics/icons/thief/crystal-drain.png",
                icon_size = 32,
                flags = {"placeable-enemy"},
                minable = {hardness = 0.2, mining_time = 0.5, result = itemName},
                max_health = attributes.health or 500,
                corpse = "small-remnants",
                collision_box = {{-0.9 * attributes.scale, -0.9 * attributes.scale}, {0.9 * attributes.scale, 0.9 * attributes.scale}},
                selection_box = {{-1.1 * attributes.scale, -1.1 * attributes.scale}, {1.1 * attributes.scale, 1.1 * attributes.scale}},
                energy_per_sector = "100MJ",
                max_distance_of_sector_revealed = 0,
                max_distance_of_nearby_sector_revealed = 0,
                energy_per_nearby_scan = "750kJ",
                resistances = {
                    {
                        type = "physical",
                        percent = 25
                    },
                    {
                        type = "fire",
                        percent = 85
                    },
                    {
                        type = "electric",
                        percent = 95
                    },
                    {
                        type = "laser",
                        percent = 90
                    }
                },
                energy_source =
                    {
                        type = "electric",
                        usage_priority = "primary-input"
                    },
                energy_usage = attributes.drain or "500kW",
                pictures =
                    {
                        filename = "__Rampant__/graphics/entities/thief/crystal-drain.png",
                        priority = "low",
                        width = 128,
                        height = 128,
                        scale = attributes.scale,
                        apply_projection = false,
                        direction_count = 32,
                        animation_speed = 0.5,
                        line_length = 8,
                        shift = {0.65, 0}
                    },
                vehicle_impact_sound =  { filename = "__base__/sound/car-metal-impact.ogg", volume = 0.65 },
                working_sound =
                    {
                        sound = {
                            {
                                filename = "__base__/sound/accumulator-working.ogg"
                            }
                        },
                        apparent_volume = 2,
                    },
                radius_minimap_visualisation_color = { r = 0.059, g = 0.092, b = 0.8, a = 0.275 },
            }
    })
    return
end

return thiefUtils
