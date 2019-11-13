local unitUtils = {}

function unitUtils.spitter_alternative_attacking_animation_sequence()
    return {
        warmup_frame_sequence = { 1, 2, 3, 4, 5, 6 },
        warmup2_frame_sequence = { 7, 7, 7, 7, 7, 7 },
        attacking_frame_sequence = { 7, 8, 9, 10, 11,  12, 13, 14, 13, 14,  13, 12, 11, 10, 9,  8 },
        cooldown_frame_sequence = { 7 },
        prepared_frame_sequence = { 7 },
        back_to_walk_frame_sequence = { 6, 5, 4, 3, 2, 1 },

        warmup_animation_speed = 1 / 6 * 0.4,
        attacking_animation_speed = 1 / 16 * 0.4,
        cooldown_animation_speed = 1 / 1 * 0.4 * 0.125;
        prepared_animation_speed = 1 / 1 * 0.5 * 0.4,
        back_to_walk_animation_speed = 1 / 6 * 0.4,
    }
end

local function crabRunBiter(scale, tint1, tint2)
    return
        {
            layers=
                {
                    {
                        filenames =
                            {
                                "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/biter-run-01.png",
                                "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/biter-run-02.png",
                                "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/biter-run-03.png",
                                "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/biter-run-04.png",
                            },
                        slice = 8,
                        lines_per_file = 8,
                        line_length = 8,
                        width = 280,
                        height = 200,
                        frame_count = 16,
                        direction_count = 16,
                        shift = {0,0},
                        scale = scale,
                        tint = tint1,
                        -- hr_version =
                        --     {
                        --         filenames =
                        --             {
                        --                 "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/hr-biter-run-01.png",
                        --                 "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/hr-biter-run-02.png",
                        --                 "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/hr-biter-run-03.png",
                        --                 "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/hr-biter-run-04.png",
                        --             },
                        --         slice = 8,
                        --         lines_per_file = 8,
                        --         line_length = 8,
                        --         width = 398,
                        --         height = 310,
                        --         frame_count = 16,
                        --         shift = util.mul_shift(util.by_pixel(-1, -5), scale),
                        --         direction_count = 16,
                        --         scale = 0.5 * scale,
                        --     }
                    },
                    {
                        filenames =
                            {
                                "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/biter-run-mask1-01.png",
                                "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/biter-run-mask1-02.png",
                                "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/biter-run-mask1-03.png",
                                "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/biter-run-mask1-04.png",
                            },
                        slice = 8,
                        lines_per_file = 8,
                        flags = { "mask" },
                        line_length = 8,
                        width = 280,
                        height = 200,
                        frame_count = 16,
                        direction_count = 16,
                        shift = {0,0},
                        scale = scale,
                        tint = tint2,
                        -- hr_version =
                        --     {
                        --         filenames =
                        --             {
                        --                 "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/hr-biter-run-mask1-01.png",
                        --                 "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/hr-biter-run-mask1-02.png",
                        --                 "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/hr-biter-run-mask1-03.png",
                        --                 "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/hr-biter-run-mask1-04.png",
                        --             },
                        --         slice = 8,
                        --         lines_per_file = 8,
                        --         line_length = 8,
                        --         width = 238,
                        --         height = 182,
                        --         frame_count = 16,
                        --         shift = util.mul_shift(util.by_pixel(-1, -37), scale),
                        --         direction_count = 16,
                        --         scale = 0.5 * scale,
                        --         tint = tint1,
                        --     }
                    },
                    {
                        filenames =
                            {
                                "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/biter-run-mask2-01.png",
                                "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/biter-run-mask2-02.png",
                                "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/biter-run-mask2-03.png",
                                "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/biter-run-mask2-04.png",
                            },
                        slice = 8,
                        lines_per_file = 8,
                        flags = { "mask" },
                        line_length = 8,
                        width = 280,
                        height = 200,
                        frame_count = 16,
                        direction_count = 16,
                        shift = {0,0},
                        scale = scale,
                        tint = tint2,
                        -- hr_version =
                        --     {
                        --         filenames =
                        --             {
                        --                 "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/hr-biter-run-mask2-01.png",
                        --                 "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/hr-biter-run-mask2-02.png",
                        --                 "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/hr-biter-run-mask2-03.png",
                        --                 "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/hr-biter-run-mask2-04.png",
                        --             },
                        --         slice = 8,
                        --         lines_per_file = 8,
                        --         line_length = 8,
                        --         width = 232,
                        --         height = 184,
                        --         frame_count = 16,
                        --         shift = util.mul_shift(util.by_pixel(0, -38), scale),
                        --         direction_count = 16,
                        --         scale = 0.5 * scale,
                        --         tint = tint2,
                        --     }
                    },
                    {
                        filenames =
                            {
                                "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/biter-run-shadow-01.png",
                                "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/biter-run-shadow-02.png",
                                "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/biter-run-shadow-03.png",
                                "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/biter-run-shadow-04.png",
                            },
                        slice = 8,
                        lines_per_file = 8,
                        line_length = 8,
                        width = 280,
                        height = 200,
                        frame_count = 16,
                        shift = {0,0},
                        direction_count = 16,
                        scale = scale,
                        draw_as_shadow = true,
                        -- hr_version =
                        --     {
                        --         filenames =
                        --             {
                        --                 "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/hr-biter-run-shadow-01.png",
                        --                 "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/hr-biter-run-shadow-02.png",
                        --                 "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/hr-biter-run-shadow-03.png",
                        --                 "__Rampant__/graphics/entities/CybranM_Biter_Run_Anim/hr-biter-run-shadow-04.png",
                        --             },
                        --         slice = 8,
                        --         lines_per_file = 8,
                        --         line_length = 8,
                        --         width = 432,
                        --         height = 292,
                        --         frame_count = 16,
                        --         shift = util.mul_shift(util.by_pixel(8, -1), scale),
                        --         direction_count = 16,
                        --         scale = 0.5 * scale,
                        --         draw_as_shadow = true,
                        --     }
                    }
                }
        }
end

local function vanillaRunBiter(scale, tint1, tint2)
    return
        {
            layers=
                {
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/biter/biter-run-01.png",
                                "__base__/graphics/entity/biter/biter-run-02.png",
                                "__base__/graphics/entity/biter/biter-run-03.png",
                                "__base__/graphics/entity/biter/biter-run-04.png",
                            },
                        slice = 8,
                        lines_per_file = 8,
                        line_length = 8,
                        width = 202,
                        height = 158,
                        frame_count = 16,
                        direction_count = 16,
                        shift = util.mul_shift(util.by_pixel(-2, -6), scale),
                        scale = scale,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__base__/graphics/entity/biter/hr-biter-run-01.png",
                                        "__base__/graphics/entity/biter/hr-biter-run-02.png",
                                        "__base__/graphics/entity/biter/hr-biter-run-03.png",
                                        "__base__/graphics/entity/biter/hr-biter-run-04.png",
                                    },
                                slice = 8,
                                lines_per_file = 8,
                                line_length = 8,
                                width = 398,
                                height = 310,
                                frame_count = 16,
                                shift = util.mul_shift(util.by_pixel(-1, -5), scale),
                                direction_count = 16,
                                scale = 0.5 * scale,
                            }
                    },
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/biter/biter-run-mask1-01.png",
                                "__base__/graphics/entity/biter/biter-run-mask1-02.png",
                                "__base__/graphics/entity/biter/biter-run-mask1-03.png",
                                "__base__/graphics/entity/biter/biter-run-mask1-04.png",
                            },
                        slice = 8,
                        lines_per_file = 8,
                        flags = { "mask" },
                        line_length = 8,
                        width = 118,
                        height = 94,
                        frame_count = 16,
                        direction_count = 16,
                        shift = util.mul_shift(util.by_pixel(0, -38), scale),
                        scale = scale,
                        tint = tint1,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__base__/graphics/entity/biter/hr-biter-run-mask1-01.png",
                                        "__base__/graphics/entity/biter/hr-biter-run-mask1-02.png",
                                        "__base__/graphics/entity/biter/hr-biter-run-mask1-03.png",
                                        "__base__/graphics/entity/biter/hr-biter-run-mask1-04.png",
                                    },
                                slice = 8,
                                lines_per_file = 8,
                                line_length = 8,
                                width = 238,
                                height = 182,
                                frame_count = 16,
                                shift = util.mul_shift(util.by_pixel(-1, -37), scale),
                                direction_count = 16,
                                scale = 0.5 * scale,
                                tint = tint1,
                            }
                    },
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/biter/biter-run-mask2-01.png",
                                "__base__/graphics/entity/biter/biter-run-mask2-02.png",
                                "__base__/graphics/entity/biter/biter-run-mask2-03.png",
                                "__base__/graphics/entity/biter/biter-run-mask2-04.png",
                            },
                        slice = 8,
                        lines_per_file = 8,
                        flags = { "mask" },
                        line_length = 8,
                        width = 120,
                        height = 92,
                        frame_count = 16,
                        direction_count = 16,
                        shift = util.mul_shift(util.by_pixel(-2, -38), scale),
                        scale = scale,
                        tint = tint2,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__base__/graphics/entity/biter/hr-biter-run-mask2-01.png",
                                        "__base__/graphics/entity/biter/hr-biter-run-mask2-02.png",
                                        "__base__/graphics/entity/biter/hr-biter-run-mask2-03.png",
                                        "__base__/graphics/entity/biter/hr-biter-run-mask2-04.png",
                                    },
                                slice = 8,
                                lines_per_file = 8,
                                line_length = 8,
                                width = 232,
                                height = 184,
                                frame_count = 16,
                                shift = util.mul_shift(util.by_pixel(0, -38), scale),
                                direction_count = 16,
                                scale = 0.5 * scale,
                                tint = tint2,
                            }
                    },
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/biter/biter-run-shadow-01.png",
                                "__base__/graphics/entity/biter/biter-run-shadow-02.png",
                                "__base__/graphics/entity/biter/biter-run-shadow-03.png",
                                "__base__/graphics/entity/biter/biter-run-shadow-04.png",
                            },
                        slice = 8,
                        lines_per_file = 8,
                        line_length = 8,
                        width = 216,
                        height = 144,
                        frame_count = 16,
                        shift = util.mul_shift(util.by_pixel(8, 0), scale),
                        direction_count = 16,
                        scale = scale,
                        draw_as_shadow = true,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__base__/graphics/entity/biter/hr-biter-run-shadow-01.png",
                                        "__base__/graphics/entity/biter/hr-biter-run-shadow-02.png",
                                        "__base__/graphics/entity/biter/hr-biter-run-shadow-03.png",
                                        "__base__/graphics/entity/biter/hr-biter-run-shadow-04.png",
                                    },
                                slice = 8,
                                lines_per_file = 8,
                                line_length = 8,
                                width = 432,
                                height = 292,
                                frame_count = 16,
                                shift = util.mul_shift(util.by_pixel(8, -1), scale),
                                direction_count = 16,
                                scale = 0.5 * scale,
                                draw_as_shadow = true,
                            }
                    }
                }
        }
end

function unitUtils.biterrunanimation(scale, tint1, tint2, altBiter)
    if altBiter then
        return crabRunBiter(scale, tint1, tint2)
    else
        return vanillaRunBiter(scale, tint1, tint2)
    end
end


return unitUtils
