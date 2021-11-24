
local unitSpawnerUtils = {}

function unitSpawnerUtils.spawner_integration(scale)
    return
        {
            filename = "__base__/graphics/entity/spawner/spawner-idle-integration.png",
            variation_count = 4,
            width = 258,
            height = 188,
            shift = util.by_pixel(2, -2),
            frame_count = 1,
            scale = scale * 1.01,
            line_length = 1,
            hr_version =
                {
                    filename = "__base__/graphics/entity/spawner/hr-spawner-idle-integration.png",
                    variation_count = 4,
                    width = 522,
                    height = 380,
                    shift = util.by_pixel(3, -3),
                    frame_count = 1,
                    line_length = 1,
                    scale = scale * 0.51
                }
        }
end

function unitSpawnerUtils.spawner_idle_animation(variation, tint, scale, tint2)
    return
        {
            layers =
                {
                    {
                        filename = "__base__/graphics/entity/spawner/spawner-idle.png",
                        line_length = 4,
                        width = 248,
                        height = 180,
                        frame_count = 8,
                        animation_speed = 0.18,
                        direction_count = 1,
                        scale = scale,
                        run_mode = "forward-then-backward",
                        shift = util.by_pixel(2, -4),
                        tint = tint,
                        y = variation * 180 * 2,
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/spawner/hr-spawner-idle.png",
                                line_length = 4,
                                width = 490,
                                height = 354,
                                frame_count = 8,
                                animation_speed = 0.18,
                                direction_count = 1,
                                scale = scale * 0.5,
                                tint = tint,
                                run_mode = "forward-then-backward",
                                shift = util.by_pixel(3, -2),
                                y = variation * 354 * 2,
                            }
                    },
                    {
                        filename = "__base__/graphics/entity/spawner/spawner-idle-mask.png",
                        flags = { "mask" },
                        width = 140,
                        height = 118,
                        frame_count = 8,
                        animation_speed = 0.18,
                        run_mode = "forward-then-backward",
                        shift = util.by_pixel(-1.5 + (-0.5 * scale), -11 + (-3 * scale)),
                        line_length = 4,
                        tint = tint2,
                        scale = scale,
                        y = variation * 118 * 2,
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/spawner/hr-spawner-idle-mask.png",
                                flags = { "mask" },
                                width = 276,
                                height = 234,
                                frame_count = 8,
                                animation_speed = 0.18,
                                run_mode = "forward-then-backward",
                                shift = util.by_pixel(3 + (-2 * (scale * 2.5)), -0.8 + (-8 * (scale * 1.55))),
                                line_length = 4,
                                tint = tint2,
                                y = variation * 234 * 2,
                                scale = scale * 0.5
                            }
                    }
                }
        }
end

function unitSpawnerUtils.spawner_die_animation(variation, tint, scale, tint2)
    return
        {
            layers =
                {
                    {
                        filename = "__base__/graphics/entity/spawner/spawner-die.png",
                        line_length = 8,
                        width = 248,
                        height = 178,
                        frame_count = 8,
                        direction_count = 1,
                        shift = util.by_pixel(2, -2),
                        y = variation * 178,
                        tint = tint,
                        scale = scale,
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/spawner/hr-spawner-die.png",
                                line_length = 8,
                                width = 490,
                                height = 354,
                                frame_count = 8,
                                direction_count = 1,
                                tint = tint,
                                shift = util.by_pixel(3, -2),
                                y = variation * 354,
                                scale = scale * 0.5
                            }
                    },
                    {
                        filename = "__base__/graphics/entity/spawner/spawner-die-mask.png",
                        flags = { "mask" },
                        width = 140,
                        height = 118,
                        frame_count = 8,
                        direction_count = 1,
                        shift = util.by_pixel(-2, -14),
                        scale = scale,
                        line_length = 8,
                        tint = tint2,
                        y = variation * 118,
                        he_version =
                            {
                                filename = "__base__/graphics/entity/spawner/hr-spawner-die-mask.png",
                                flags = { "mask" },
                                width = 276,
                                height = 234,
                                frame_count = 8,
                                direction_count = 1,
                                shift = util.by_pixel(-1, -14),
                                line_length = 8,
                                tint = tint2,
                                y = variation * 234,
                                scale = scale * 0.5
                            }
                    }
                }
        }
end

return unitSpawnerUtils
