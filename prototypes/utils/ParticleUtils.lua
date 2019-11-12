local particleUtils = {}

-- module code

local function makeBloodParticle(attributes)
    local name = attributes.name .. "-blood-particle-rampant"
    local tint = attributes.tint2
    
    data:extend({
            {
                type = "particle",
                name = name,
                flags = {"not-on-map"},
                movement_modifier_when_on_ground = 0.2,
                life_time = 240,
                pictures =
                    {
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-01.png",
                            priority = "extra-high",
                            width = 4,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = tint,
                            shift = util.by_pixel(0,1),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-01.png",
                                    width = 6,
                                    height = 6,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = tint,
                                    shift = util.by_pixel(0,0.5)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-02.png",
                            priority = "extra-high",
                            width = 6,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = tint,
                            shift = util.by_pixel(1,1),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-02.png",
                                    line_length = 6,
                                    width = 8,
                                    height = 6,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = tint,
                                    shift = util.by_pixel(0.5,0.5)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-03.png",
                            priority = "extra-high",
                            width = 4,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = tint,
                            shift = util.by_pixel(1,1),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-03.png",
                                    line_length = 6,
                                    width = 6,
                                    height = 6,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = tint,
                                    shift = util.by_pixel(0.5,0.5)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-04.png",
                            priority = "extra-high",
                            width = 4,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = tint,
                            shift = util.by_pixel(1,0),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-04.png",
                                    line_length = 6,
                                    width = 6,
                                    height = 6,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = tint,
                                    shift = util.by_pixel(0.5,-0.5)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-05.png",
                            priority = "extra-high",
                            width = 4,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = tint,
                            shift = util.by_pixel(1,1),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-05.png",
                                    line_length = 6,
                                    width = 6,
                                    height = 4,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = tint,
                                    shift = util.by_pixel(0.5,0.5)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-06.png",
                            priority = "extra-high",
                            width = 4,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = tint,
                            shift = util.by_pixel(1,1),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-06.png",
                                    line_length = 6,
                                    width = 6,
                                    height = 6,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = tint,
                                    shift = util.by_pixel(0.5,0.5)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-07.png",
                            priority = "extra-high",
                            width = 4,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = tint,
                            shift = util.by_pixel(1,1),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-07.png",
                                    line_length = 6,
                                    width = 6,
                                    height = 6,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = tint,
                                    shift = util.by_pixel(0.5,0.5)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-08.png",
                            priority = "extra-high",
                            width = 6,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = tint,
                            shift = util.by_pixel(2,0),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-08.png",
                                    line_length = 6,
                                    width = 10,
                                    height = 8,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = tint,
                                    shift = util.by_pixel(1.5,0)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-09.png",
                            priority = "extra-high",
                            width = 6,
                            height = 6,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = tint,
                            shift = util.by_pixel(2,0),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-09.png",
                                    width = 10,
                                    height = 8,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = tint,
                                    shift = util.by_pixel(1.5,-0.5)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-10.png",
                            priority = "extra-high",
                            width = 6,
                            height = 6,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = tint,
                            shift = util.by_pixel(1,0),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-10.png",
                                    width = 10,
                                    height = 10,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = tint,
                                    shift = util.by_pixel(0.5,0)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-11.png",
                            priority = "extra-high",
                            width = 4,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = tint,
                            shift = util.by_pixel(1,0),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-11.png",
                                    width = 8,
                                    height = 8,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = tint,
                                    shift = util.by_pixel(1,0)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-12.png",
                            priority = "extra-high",
                            width = 4,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = tint,
                            shift = util.by_pixel(0,0),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-12.png",
                                    line_length = 6,
                                    width = 8,
                                    height = 8,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = tint,
                                    shift = util.by_pixel(0,0)
                                }
                        }
                    },
                shadows =
                    {
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-01.png",
                            priority = "extra-high",
                            width = 4,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                            shift = util.by_pixel(0,1),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-01.png",
                                    width = 6,
                                    height = 6,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                                    shift = util.by_pixel(0,0.5)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-02.png",
                            priority = "extra-high",
                            width = 6,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                            shift = util.by_pixel(1,1),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-02.png",
                                    line_length = 6,
                                    width = 8,
                                    height = 6,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                                    shift = util.by_pixel(0.5,0.5)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-03.png",
                            priority = "extra-high",
                            width = 4,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                            shift = util.by_pixel(1,1),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-03.png",
                                    line_length = 6,
                                    width = 6,
                                    height = 6,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                                    shift = util.by_pixel(0.5,0.5)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-04.png",
                            priority = "extra-high",
                            width = 4,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                            shift = util.by_pixel(1,0),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-04.png",
                                    line_length = 6,
                                    width = 6,
                                    height = 6,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                                    shift = util.by_pixel(0.5,-0.5)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-05.png",
                            priority = "extra-high",
                            width = 4,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                            shift = util.by_pixel(1,1),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-05.png",
                                    line_length = 6,
                                    width = 6,
                                    height = 4,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                                    shift = util.by_pixel(0.5,0.5)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-06.png",
                            priority = "extra-high",
                            width = 4,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                            shift = util.by_pixel(1,1),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-06.png",
                                    width = 6,
                                    height = 6,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                                    shift = util.by_pixel(0.5,0.5)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-07.png",
                            priority = "extra-high",
                            width = 4,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                            shift = util.by_pixel(1,1),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-07.png",
                                    width = 6,
                                    height = 6,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                                    shift = util.by_pixel(0.5,0.5)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-08.png",
                            priority = "extra-high",
                            width = 6,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                            shift = util.by_pixel(2,0),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-08.png",
                                    line_length = 6,
                                    width = 10,
                                    height = 8,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                                    shift = util.by_pixel(1.5,0)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-09.png",
                            priority = "extra-high",
                            width = 6,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                            shift = util.by_pixel(2,0),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-09.png",
                                    width = 10,
                                    height = 8,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                                    shift = util.by_pixel(1.5,-0.5)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-10.png",
                            priority = "extra-high",
                            width = 6,
                            height = 6,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                            shift = util.by_pixel(1,0),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-10.png",
                                    width = 10,
                                    height = 10,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                                    shift = util.by_pixel(0.5,0)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-11.png",
                            priority = "extra-high",
                            width = 4,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,
                            tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},                            
                            shift = util.by_pixel(1,0),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-11.png",
                                    width = 8,
                                    height = 8,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                                    shift = util.by_pixel(1,0)
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/blood-particle/blood-particle-12.png",
                            priority = "extra-high",
                            width = 4,
                            height = 4,
                            line_length = 6,
                            frame_count = 12,
                            scale = 1,                            
                            tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                            shift = util.by_pixel(0,0),
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/blood-particle/hr-blood-particle-12.png",
                                    line_length = 6,
                                    width = 8,
                                    height = 8,
                                    line_length = 6,
                                    frame_count = 12,
                                    scale = 1,
                                    tint = {r=tint.r*0.1, b=tint.b*0.1, g=tint.g*0.1, a=tint.a*0.1},
                                    shift = util.by_pixel(0,0)
                                }
                        }
                    },
                ended_in_water_trigger_effect =
                    {
                        type = "create-entity",
                        entity_name = "water-splash"
                    }

            }
    })
    
    return name
end


function particleUtils.makeBloodFountains(attributes)

    local bloodParticle = makeBloodParticle(attributes)
    
    data:extend({
            {
                type = "particle-source",
                name = attributes.name .. "-blood-fountain-rampant",
                particle = bloodParticle,
                time_to_live = 10,
                time_to_live_deviation = 5,
                time_before_start = 0,
                time_before_start_deviation = 3,
                height = 0.4,
                height_deviation = 0.1,
                vertical_speed = 0.05,
                vertical_speed_deviation = 0.03,
                horizontal_speed = 0.025,
                horizontal_speed_deviation = 0.025
            },
            {
                type = "particle-source",
                name = attributes.name .. "-blood-fountain-big-rampant",
                particle = bloodParticle,
                time_to_live = 30,
                time_to_live_deviation = 5,
                time_before_start = 0,
                time_before_start_deviation = 10,
                height = 0.4,
                height_deviation = 0.1,
                vertical_speed = 0.15,
                vertical_speed_deviation = 0.05,
                horizontal_speed = 0.04,
                horizontal_speed_deviation = 0.04
            },
            {
                type = "explosion",
                name = attributes.name .. "-blood-explosion-small-rampant",
                flags = {"not-on-map"},
                animations =
                    {
                        {
                            filename = "__core__/graphics/empty.png",
                            priority = "high",
                            width = 1,
                            height = 1,
                            frame_count = 1
                        }
                    },
                created_effect =
                    {
                        type = "direct",
                        action_delivery =
                            {
                                type = "instant",
                                target_effects =
                                    {
                                        type = "create-entity",
                                        entity_name = attributes.name .. "-blood-fountain-rampant",
                                        repeat_count = 20,
                                        offset_deviation = {{-0.4, -0.4}, {0.4, 0.4}}
                                    }
                            }
                    }
            },
            {
                type = "explosion",
                name = attributes.name .. "-blood-explosion-big-rampant",
                flags = {"not-on-map"},
                animations =
                    {
                        {
                            filename = "__core__/graphics/empty.png",
                            priority = "high",
                            width = 1,
                            height = 1,
                            frame_count = 1
                        }
                    },
                created_effect =
                    {
                        type = "direct",
                        action_delivery =
                            {
                                type = "instant",
                                target_effects =
                                    {
                                        {
                                            type = "create-particle",
                                            repeat_count = 150,
                                            entity_name = bloodParticle,
                                            initial_height = 0.5,
                                            speed_from_center = 0.08,
                                            speed_from_center_deviation = 0.05,
                                            initial_vertical_speed = -0.01,
                                            initial_vertical_speed_deviation = 0.02,
                                            offset_deviation = {{-0.4, -0.4}, {0.4, 0.4}}
                                        },
                                        {
                                            type = "create-entity",
                                            entity_name = attributes.name .. "-blood-fountain-rampant",
                                            repeat_count = 35,
                                            offset_deviation = {{-0.4, -0.4}, {0.4, 0.4}}
                                        }
                                    }
                            }
                    }
            },

            {
                type = "explosion",
                name = attributes.name .. "-blood-explosion-huge-rampant",
                flags = {"not-on-map"},
                animations =
                    {
                        {
                            filename = "__core__/graphics/empty.png",
                            priority = "high",
                            width = 1,
                            height = 1,
                            frame_count = 1
                        }
                    },
                created_effect =
                    {
                        type = "direct",
                        action_delivery =
                            {
                                type = "instant",
                                target_effects =
                                    {
                                        {
                                            type = "create-particle",
                                            repeat_count = 150,
                                            entity_name = bloodParticle,
                                            initial_height = 0.5,
                                            speed_from_center = 0.08,
                                            speed_from_center_deviation = 0.05,
                                            initial_vertical_speed = -0.01,
                                            initial_vertical_speed_deviation = 0.02,
                                            offset_deviation = {{-0.4, -0.4}, {0.4, 0.4}}
                                        },
                                        {
                                            type = "create-entity",
                                            entity_name = attributes.name .. "-blood-fountain-big-rampant",
                                            repeat_count = 35,
                                            offset_deviation = {{-1.6, -1.6}, {1.6, 1.6}}
                                        }
                                    }
                            }
                    }
            }
    })

end

return particleUtils
