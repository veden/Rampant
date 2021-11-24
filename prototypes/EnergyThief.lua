
-- imports

local thiefUtils = require("utils/ThiefUtils")

-- constants

local energyThief = {}

-- imported functions

local makeDrainCrystal = thiefUtils.makeDrainCrystal

function energyThief.addFactionAddon()

    data:extend({
            {
                type = "simple-entity-with-force",
                name = "drain-trigger-rampant",
                render_layer = "object",
                icon = "__base__/graphics/icons/steel-chest.png",
                icon_size = 32,
                flags = {"placeable-neutral", "player-creation"},
                order = "s-e-w-f",
                minable = {mining_time = 1, result = "drain-trigger-rampant"},
                max_health = 100,
                selectable_in_game = false,
                corpse = "small-remnants",
                collision_box = {{-0.35, -0.35}, {0.35, 0.35}},
                selection_box = {{-0.5, -0.5}, {0.5, 0.5}},
                picture =
                    {
                        filename = "__core__/graphics/empty.png",
                        priority = "extra-high",
                        width = 1,
                        height = 1,
                        shift = {0, 0}
                    }
            },

            {
                type = "item",
                name = "drain-trigger-rampant",
                icon = "__Rampant__/graphics/icons/thief/crystal-drain.png",
                icon_size = 32,
                icon_mipmaps = 1,
                flags = {"hidden"},
                subgroup = "energy",
                order = "e[accumulator]-a[accumulator]",
                place_result = "drain-trigger-rampant",
                stack_size = 50
            },

            {
                type = "item",
                name = "crystal-drain-pole-rampant",
                icon = "__Rampant__/graphics/icons/thief/crystal-drain.png",
                icon_size = 32,
                icon_mipmaps = 1,
                flags = {"hidden"},
                subgroup = "energy",
                order = "e[accumulator]-a[accumulator]",
                place_result = "crystal-drain-pole-rampant",
                stack_size = 50
            },

            {
                type = "electric-pole",
                name = "crystal-drain-pole-rampant",
                icon = "__Rampant__/graphics/icons/thief/crystal-drain.png",
                icon_size = 32,
                icon_mipmaps = 1,
                flags = {"hidden"},
                selectable_in_game = false,
                minable = {hardness = 0.2, mining_time = 0.5, result = "big-electric-pole"},
                max_health = 750,
                healing_per_tick = 0.02,
                corpse = "medium-remnants",
                resistances =
                    {
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
                collision_box = {{-0.55, -0.55}, {0.55, 0.55}},
                selection_box = {{-0.55, -0.55}, {0.55, 0.55}},
                drawing_box = {{-1, -3}, {1, 0.5}},
                maximum_wire_distance = 30,
                supply_area_distance = 9,
                vehicle_impact_sound =  { filename = "__base__/sound/car-metal-impact.ogg", volume = 0.65 },
                pictures =
                    {
                        filename = "__Rampant__/graphics/entities/thief/crystal-drain-pole.png",
                        priority = "high",
                        width = 168,
                        height = 130,
                        direction_count = 4,
                        shift = {1.6, -1.4}
                    },
                connection_points =
                    {
                        {
                            shadow =
                                {
                                    copper = {2.7, 0},
                                    green = {1.8, 0},
                                    red = {3.6, 0}
                                },
                            wire =
                                {
                                    copper = {0, -2.5},
                                    green = {-0.59375, -2.5},
                                    red = {0.625, -2.5}
                                }
                        },
                        {
                            shadow =
                                {
                                    copper = {3.1, 0.2},
                                    green = {2.3, -0.3},
                                    red = {3.8, 0.6}
                                },
                            wire =
                                {
                                    copper = {-0.0625, -2.5},
                                    green = {-0.5, -3},
                                    red = {0.34375, -2}
                                }
                        },
                        {
                            shadow =
                                {
                                    copper = {2.9, 0.06},
                                    green = {3.0, -0.6},
                                    red = {3.0, 0.8}
                                },
                            wire =
                                {
                                    copper = {-0.09375, -2.5},
                                    green = {-0.09375, -3},
                                    red = {-0.09375, -2}
                                }
                        },
                        {
                            shadow =
                                {
                                    copper = {3.1, 0.2},
                                    green = {3.8, -0.3},
                                    red = {2.35, 0.6}
                                },
                            wire =
                                {
                                    copper = {-0.0625, -2.4},
                                    green = {0.375, -2.9},
                                    red = {-0.46875, -2.4}
                                }
                        }
                    },
                radius_visualisation_picture =
                    {
                        filename = "__base__/graphics/entity/small-electric-pole/electric-pole-radius-visualization.png",
                        width = 12,
                        height = 12,
                        priority = "extra-high-no-scale"
                    }
            }
    })

    local chest = util.table.deepcopy(data.raw["radar"]["radar"])
    chest.name = "pylon-target-rampant"
    chest.icon = "__Rampant__/graphics/icons/thief/crystal-drain.png"
    chest.icon_size = 32
    chest.icon_mipmaps = 1
    chest.flags = {"not-repairable", "not-on-map", "hidden"}
    chest.subgroup = "enemies"
    chest.next_upgrade = nil
    chest.backer_name = false
    chest.pictures = {
        layers={
            {
                filename = "__core__/graphics/empty.png",
                priority = "low",
                width = 1,
                height = 1,
                direction_count = 1,
                line_length = 1,
                shift = {0, 0}
            }
    }}
    chest.max_health = 750
    chest.resistances =
        {
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
        }
    chest.energy_usage = "500kW"
    -- chest.collision_mask = {}
    chest.collision_box = nil
    chest.selection_box = {{-0.55, -0.55}, {0.55, 0.55}}
    chest.minable.result = "pylon-target-rampant"
    chest.working_sound = {
        sound = {
            {
                filename = "__base__/sound/accumulator-working.ogg"
            }
        },
        apparent_volume = 2,
    }

    data:extend({
            chest,

            {
                type = "item",
                name = "pylon-target-rampant",
                icon = "__Rampant__/graphics/icons/thief/crystal-drain.png",
                icon_size = 32,
                icon_mipmaps = 1,
                flags = {"hidden"},
                subgroup = "enemies",
                order = "a[items]-h[steel-collector]",
                place_result = "pylon-target-rampant",
                stack_size = 50
            }
    })


    for i=1,10 do
        local drainCrystalAttributes = {
            name = "crystal-v" .. i,
            drain = i * 1.3 .. "MW",
            scale = (i * 0.1) + 0.5,
            health = 400 * i
        }

        makeDrainCrystal(drainCrystalAttributes)
    end

end

return energyThief
