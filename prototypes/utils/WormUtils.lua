local wormUtils = {}

function wormUtils.wormFoldedAnimation(scale, tint, tint2)
    return
        {
            layers=
                {
                    {
                        filename = "__base__/graphics/entity/worm/worm-folded.png",
                        run_mode = "forward-then-backward",
                        line_length = 9,
                        width = 66,
                        height = 60,
                        frame_count = 9,
                        shift = util.mul_shift(util.by_pixel(0, 4), scale),
                        direction_count = 1,
                        tint = tint,
                        scale = scale,
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/worm/hr-worm-folded.png",
                                run_mode = "forward-then-backward",
                                line_length = 9,
                                width = 130,
                                height = 120,
                                frame_count = 9,
                                shift = util.mul_shift(util.by_pixel(0, 4), scale),
                                direction_count = 1,
                                tint = tint,
                                scale = scale * 0.5,
                            }
                    },
                    {
                        filename = "__base__/graphics/entity/worm/worm-folded-mask.png",
                        flags = { "mask" },
                        run_mode = "forward-then-backward",
                        line_length = 9,
                        width = 66,
                        height = 56,
                        frame_count = 9,
                        shift = util.mul_shift(util.by_pixel(0, 6), scale),
                        scale = scale,
                        direction_count = 1,
                        tint = tint2,
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/worm/hr-worm-folded-mask.png",
                                flags = { "mask" },
                                run_mode = "forward-then-backward",
                                line_length = 9,
                                width = 130,
                                height = 108,
                                frame_count = 9,
                                shift = util.mul_shift(util.by_pixel(0, 7), scale),
                                scale = scale * 0.5,
                                direction_count = 1,
                                tint = tint2
                            }
                    },
                    {
                        filename = "__base__/graphics/entity/worm/worm-folded-shadow.png",
                        run_mode = "forward-then-backward",
                        line_length = 9,
                        width = 60,
                        height = 34,
                        frame_count = 9,
                        shift = util.mul_shift(util.by_pixel(4, -4), scale),
                        direction_count = 1,
                        scale = scale,
                        draw_as_shadow = true,
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/worm/hr-worm-folded-shadow.png",
                                run_mode = "forward-then-backward",
                                line_length = 9,
                                width = 116,
                                height = 68,
                                frame_count = 9,
                                shift = util.mul_shift(util.by_pixel(5, -4), scale),
                                direction_count = 1,
                                scale = scale * 0.5,
                                draw_as_shadow = true,
                            }
                    },
                }
        }
end

function wormUtils.wormPreparingAnimation(scale, tint, run_mode, tint2)
    return
        {
            layers=
                {
                    {
                        filename = "__base__/graphics/entity/worm/worm-preparing.png",
                        width = 94,
                        height = 152,
                        line_length = 6,
                        frame_count = 18,
                        shift = util.mul_shift(util.by_pixel(0, -42), scale),
                        run_mode = run_mode,
                        scale = scale,
                        tint = tint,
                        direction_count = 1,
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/worm/hr-worm-preparing.png",
                                width = 188,
                                height = 304,
                                line_length = 6,
                                frame_count = 18,
                                shift = util.mul_shift(util.by_pixel(0, -42), scale),
                                run_mode = run_mode,
                                scale = 0.5 * scale,
                                tint = tint,
                                direction_count = 1,
                            }
                    },
                    {
                        filename = "__base__/graphics/entity/worm/worm-preparing-mask.png",
                        flags = { "mask" },
                        line_length = 6,
                        width = 94,
                        height = 124,
                        frame_count = 18,
                        shift = util.mul_shift(util.by_pixel(0, -28), scale),
                        run_mode = run_mode,
                        scale = scale,
                        direction_count = 1,
                        tint = tint2,
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/worm/hr-worm-preparing-mask.png",
                                flags = { "mask" },
                                line_length = 6,
                                width = 188,
                                height = 248,
                                frame_count = 18,
                                shift = util.mul_shift(util.by_pixel(0, -28), scale),
                                run_mode = run_mode,
                                scale = 0.5 * scale,
                                direction_count = 1,
                                tint = tint2,
                            }
                    },
                    {
                        filename = "__base__/graphics/entity/worm/worm-preparing-shadow.png",
                        width = 208,
                        height = 66,
                        line_length = 6,
                        frame_count = 18,
                        shift = util.mul_shift(util.by_pixel(54, -6), scale),
                        run_mode = run_mode,
                        scale = scale,
                        direction_count = 1,
                        draw_as_shadow = true,
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/worm/hr-worm-preparing-shadow.png",
                                width = 410,
                                height = 124,
                                line_length = 6,
                                frame_count = 18,
                                shift = util.mul_shift(util.by_pixel(55, -4), scale),
                                run_mode = run_mode,
                                scale = 0.5 * scale,
                                direction_count = 1,
                                draw_as_shadow = true,
                            }
                    },
                }
        }
end

function wormUtils.wormPreparedAnimation(scale, tint, tint2)
    return
        {
            layers=
                {
                    {
                        filename = "__base__/graphics/entity/worm/worm-prepared.png",
                        run_mode = "forward-then-backward",
                        line_length = 9,
                        width = 98,
                        height = 152,
                        frame_count = 9,
                        scale = scale,
                        direction_count = 1,
                        shift = util.mul_shift(util.by_pixel(-4, -42), scale),
                        tint = tint,
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/worm/hr-worm-prepared.png",
                                run_mode = "forward-then-backward",
                                line_length = 9,
                                width = 190,
                                height = 300,
                                frame_count = 9,
                                scale = 0.5 * scale,
                                direction_count = 1,
                                tint = tint,
                                shift = util.mul_shift(util.by_pixel(-3, -41), scale),
                            }
                    },
                    {
                        filename = "__base__/graphics/entity/worm/worm-prepared-mask.png",
                        flags = { "mask" },
                        run_mode = "forward-then-backward",
                        line_length = 9,
                        width = 98,
                        height = 132,
                        frame_count = 9,
                        shift = util.mul_shift(util.by_pixel(-4, -32), scale),
                        scale = scale,
                        direction_count = 1,
                        tint = tint2,
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/worm/hr-worm-prepared-mask.png",
                                flags = { "mask" },
                                run_mode = "forward-then-backward",
                                line_length = 9,
                                width = 190,
                                height = 268,
                                frame_count = 9,
                                shift = util.mul_shift(util.by_pixel(-3, -33), scale),
                                scale = 0.5 * scale,
                                direction_count = 1,
                                tint = tint2,
                            }
                    },
                    {
                        filename = "__base__/graphics/entity/worm/worm-prepared-shadow.png",
                        run_mode = "forward-then-backward",
                        line_length = 9,
                        width = 204,
                        height = 62,
                        frame_count = 9,
                        scale = scale,
                        direction_count = 1,
                        shift = util.mul_shift(util.by_pixel(52, -6), scale),
                        draw_as_shadow = true,
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/worm/hr-worm-prepared-shadow.png",
                                run_mode = "forward-then-backward",
                                line_length = 9,
                                width = 408,
                                height = 122,
                                frame_count = 9,
                                scale = 0.5 * scale,
                                direction_count = 1,
                                shift = util.mul_shift(util.by_pixel(52, -6), scale),
                                draw_as_shadow = true,
                            }
                    },
                }
        }
end


function wormUtils.wormPreparedAlternativeAnimation(scale, tint, tint2)
    return
        {
            layers=
                {
                    {
                        filename = "__base__/graphics/entity/worm/worm-prepared-alternative.png",
                        line_length = 6,
                        width = 92,
                        height = 164,
                        frame_count = 17,
                        frame_sequence = alternate_frame_sequence,
                        scale = scale,
                        direction_count = 1,
                        tint = tint,
                        shift = util.mul_shift(util.by_pixel(-2, -48), scale),
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/worm/hr-worm-prepared-alternative.png",
                                line_length = 6,
                                width = 182,
                                height = 324,
                                frame_count = 17,
                                frame_sequence = alternate_frame_sequence,
                                scale = 0.5 * scale,
                                tint = tint,
                                direction_count = 1,
                                shift = util.mul_shift(util.by_pixel(-2, -47), scale),
                            }
                    },
                    {
                        filename = "__base__/graphics/entity/worm/worm-prepared-alternative-mask.png",
                        flags = { "mask" },
                        line_length = 6,
                        width = 92,
                        height = 144,
                        frame_count = 17,
                        frame_sequence = alternate_frame_sequence,
                        shift = util.mul_shift(util.by_pixel(-2, -38), scale),
                        scale = scale,
                        direction_count = 1,
                        tint = tint2,
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/worm/hr-worm-prepared-alternative-mask.png",
                                flags = { "mask" },
                                line_length = 6,
                                width = 182,
                                height = 288,
                                frame_count = 17,
                                frame_sequence = alternate_frame_sequence,
                                shift = util.mul_shift(util.by_pixel(-2, -38), scale),
                                scale = 0.5 * scale,
                                direction_count = 1,
                                tint = tint2,
                            }
                    },
                    {
                        filename = "__base__/graphics/entity/worm/worm-prepared-alternative-shadow.png",
                        line_length = 6,
                        width = 214,
                        height = 60,
                        frame_count = 17,
                        frame_sequence = alternate_frame_sequence,
                        scale = scale,
                        direction_count = 1,
                        shift = util.mul_shift(util.by_pixel(56, -6), scale),
                        draw_as_shadow = true,
                        hr_version =
                            {
                                filename = "__base__/graphics/entity/worm/hr-worm-prepared-alternative-shadow.png",
                                line_length = 6,
                                width = 424,
                                height = 120,
                                frame_count = 17,
                                frame_sequence = alternate_frame_sequence,
                                scale = 0.5 * scale,
                                direction_count = 1,
                                shift = util.mul_shift(util.by_pixel(57, -6), scale),
                                draw_as_shadow = true,
                            }
                    },
                }
        }
end


function wormUtils.wormStartAttackAnimation(scale, tint, tint2)
    return
        {
            layers=
                {
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/worm/worm-attack-01.png",
                                "__base__/graphics/entity/worm/worm-attack-02.png",
                                "__base__/graphics/entity/worm/worm-attack-03.png",
                                "__base__/graphics/entity/worm/worm-attack-04.png",
                                "__base__/graphics/entity/worm/worm-attack-05.png",
                                "__base__/graphics/entity/worm/worm-attack-06.png",
                                "__base__/graphics/entity/worm/worm-attack-07.png",
                                "__base__/graphics/entity/worm/worm-attack-08.png",
                                "__base__/graphics/entity/worm/worm-attack-09.png",
                                "__base__/graphics/entity/worm/worm-attack-10.png"
                            },
                        slice = 4,
                        lines_per_file = 4,
                        line_length = 4,
                        width = 240,
                        height = 222,
                        frame_count = 10,
                        frame_sequence = start_attack_frame_sequence,
                        direction_count = 16,
                        tint = tint,
                        shift = util.mul_shift(util.by_pixel(-8, -30), scale),
                        scale = scale,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__base__/graphics/entity/worm/hr-worm-attack-01.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-02.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-03.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-04.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-05.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-06.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-07.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-08.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-09.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-10.png"
                                    },
                                slice = 4,
                                lines_per_file = 4,
                                line_length = 4,
                                width = 480,
                                height = 440,
                                tint = tint,
                                frame_count = 10,
                                frame_sequence = start_attack_frame_sequence,
                                direction_count = 16,
                                shift = util.mul_shift(util.by_pixel(-8, -29), scale),
                                scale = 0.5 * scale,
                            }
                    },
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/worm/worm-attack-mask-01.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-02.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-03.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-04.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-05.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-06.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-07.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-08.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-09.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-10.png"
                            },
                        slice = 4,
                        lines_per_file = 4,
                        line_length = 4,
                        width = 196,
                        height = 186,
                        frame_count = 10,
                        frame_sequence = start_attack_frame_sequence,
                        direction_count = 16,
                        shift = util.mul_shift(util.by_pixel(-8, -28), scale),
                        tint = tint2,
                        scale = scale,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-01.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-02.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-03.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-04.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-05.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-06.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-07.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-08.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-09.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-10.png"
                                    },
                                slice = 4,
                                lines_per_file = 4,
                                line_length = 4,
                                width = 388,
                                height = 366,
                                frame_count = 10,
                                frame_sequence = start_attack_frame_sequence,
                                direction_count = 16,
                                shift = util.mul_shift(util.by_pixel(-7, -27), scale),
                                tint = tint2,
                                scale = 0.5 * scale,
                            }
                    },
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/worm/worm-attack-shadow-01.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-02.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-03.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-04.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-05.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-06.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-07.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-08.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-09.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-10.png"
                            },
                        slice = 4,
                        lines_per_file = 4,
                        line_length = 4,
                        width = 310,
                        height = 176,
                        frame_count = 10,
                        frame_sequence = start_attack_frame_sequence,
                        direction_count = 16,
                        shift = util.mul_shift(util.by_pixel(38, -2), scale),
                        scale = scale,
                        draw_as_shadow = true,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-01.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-02.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-03.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-04.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-05.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-06.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-07.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-08.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-09.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-10.png"
                                    },
                                slice = 4,
                                lines_per_file = 4,
                                line_length = 4,
                                width = 618,
                                height = 350,
                                frame_count = 10,
                                frame_sequence = start_attack_frame_sequence,
                                direction_count = 16,
                                shift = util.mul_shift(util.by_pixel(38, -2), scale),
                                scale = 0.5 * scale,
                                draw_as_shadow = true,
                            }
                    },
                }
        }
end

function wormUtils.wormEndAttackAnimation(scale, tint, tint2)
    return
        {
            layers=
                {
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/worm/worm-attack-01.png",
                                "__base__/graphics/entity/worm/worm-attack-02.png",
                                "__base__/graphics/entity/worm/worm-attack-03.png",
                                "__base__/graphics/entity/worm/worm-attack-04.png",
                                "__base__/graphics/entity/worm/worm-attack-05.png",
                                "__base__/graphics/entity/worm/worm-attack-06.png",
                                "__base__/graphics/entity/worm/worm-attack-07.png",
                                "__base__/graphics/entity/worm/worm-attack-08.png",
                                "__base__/graphics/entity/worm/worm-attack-09.png",
                                "__base__/graphics/entity/worm/worm-attack-10.png"
                            },
                        slice = 4,
                        lines_per_file = 4,
                        line_length = 4,
                        width = 240,
                        height = 222,
                        tint = tint,
                        frame_count = 10,
                        frame_sequence = end_attack_frame_sequence,
                        direction_count = 16,
                        shift = util.mul_shift(util.by_pixel(-8, -30), scale),
                        scale = scale,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__base__/graphics/entity/worm/hr-worm-attack-01.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-02.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-03.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-04.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-05.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-06.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-07.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-08.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-09.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-10.png"
                                    },
                                slice = 4,
                                lines_per_file = 4,
                                line_length = 4,
                                width = 480,
                                height = 440,
                                tint = tint,
                                frame_count = 10,
                                frame_sequence = end_attack_frame_sequence,
                                direction_count = 16,
                                shift = util.mul_shift(util.by_pixel(-8, -29), scale),
                                scale = 0.5 * scale,
                            }
                    },
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/worm/worm-attack-mask-01.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-02.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-03.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-04.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-05.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-06.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-07.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-08.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-09.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-10.png"
                            },
                        slice = 4,
                        lines_per_file = 4,
                        line_length = 4,
                        width = 196,
                        height = 186,
                        frame_count = 10,
                        frame_sequence = end_attack_frame_sequence,
                        direction_count = 16,
                        shift = util.mul_shift(util.by_pixel(-8, -28), scale),
                        tint = tint2,
                        scale = scale,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-01.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-02.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-03.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-04.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-05.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-06.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-07.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-08.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-09.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-mask-10.png"
                                    },
                                slice = 4,
                                lines_per_file = 4,
                                line_length = 4,
                                width = 388,
                                height = 366,
                                frame_count = 10,
                                frame_sequence = end_attack_frame_sequence,
                                direction_count = 16,
                                shift = util.mul_shift(util.by_pixel(-7, -27), scale),
                                tint = tint2,
                                scale = 0.5 * scale,
                            }
                    },
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/worm/worm-attack-shadow-01.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-02.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-03.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-04.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-05.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-06.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-07.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-08.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-09.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-10.png"
                            },
                        slice = 4,
                        lines_per_file = 4,
                        line_length = 4,
                        width = 310,
                        height = 176,
                        frame_count = 10,
                        frame_sequence = end_attack_frame_sequence,
                        direction_count = 16,
                        shift = util.mul_shift(util.by_pixel(38, -2), scale),
                        scale = scale,
                        draw_as_shadow = true,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-01.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-02.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-03.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-04.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-05.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-06.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-07.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-08.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-09.png",
                                        "__base__/graphics/entity/worm/hr-worm-attack-shadow-10.png"
                                    },
                                slice = 4,
                                lines_per_file = 4,
                                line_length = 4,
                                width = 618,
                                height = 350,
                                frame_count = 10,
                                frame_sequence = end_attack_frame_sequence,
                                direction_count = 16,
                                shift = util.mul_shift(util.by_pixel(38, -2), scale),
                                scale = 0.5 * scale,
                                draw_as_shadow = true,
                            }
                    },
                }
        }
end


function wormUtils.wormDieAnimation(scale, tint, tint2)
    return
        {
            {
                layers=
                    {
                        {
                            filename = "__base__/graphics/entity/worm/worm-die-01.png",
                            line_length = 6,
                            width = 134,
                            height = 186,
                            frame_count = 24,
                            shift = util.mul_shift(util.by_pixel(18, -26), scale),
                            direction_count = 1,
                            scale = scale,
                            tint = tint,
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/worm/hr-worm-die-01.png",
                                    line_length = 6,
                                    width = 262,
                                    height = 372,
                                    frame_count = 24,
                                    shift = util.mul_shift(util.by_pixel(19, -26), scale),
                                    direction_count = 1,
                                    tint = tint,
                                    scale = 0.5 * scale,
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/worm/worm-die-01-mask.png",
                            flags = { "mask" },
                            line_length = 6,
                            width = 124,
                            height = 160,
                            frame_count = 24,
                            shift = util.mul_shift(util.by_pixel(15, -19), scale),
                            direction_count = 1,
                            scale = scale,
                            tint = tint2,
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/worm/hr-worm-die-01-mask.png",
                                    flags = { "mask" },
                                    line_length = 6,
                                    width = 250,
                                    height = 318,
                                    frame_count = 24,
                                    shift = util.mul_shift(util.by_pixel(15, -19), scale),
                                    direction_count = 1,
                                    scale = 0.5 * scale,
                                    tint = tint2,
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/worm/worm-die-01-shadow.png",
                            line_length = 6,
                            width = 202,
                            height = 106,
                            frame_count = 24,
                            shift = util.mul_shift(util.by_pixel(54, 16), scale),
                            direction_count = 1,
                            scale = scale,
                            draw_as_shadow = true,
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/worm/hr-worm-die-01-shadow.png",
                                    line_length = 6,
                                    width = 404,
                                    height = 210,
                                    frame_count = 24,
                                    shift = util.mul_shift(util.by_pixel(54, 16), scale),
                                    direction_count = 1,
                                    scale = 0.5 * scale,
                                    draw_as_shadow = true,
                                }
                        },
                    },
            },
            {
                layers=
                    {
                        {
                            filename = "__base__/graphics/entity/worm/worm-die-02.png",
                            line_length = 6,
                            width = 184,
                            height = 164,
                            frame_count = 24,
                            shift = util.mul_shift(util.by_pixel(-48, -42), scale),
                            direction_count = 1,
                            scale = scale,
                            tint = tint,
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/worm/hr-worm-die-02.png",
                                    line_length = 6,
                                    width = 362,
                                    height = 322,
                                    frame_count = 24,
                                    tint = tint,
                                    shift = util.mul_shift(util.by_pixel(-47, -41), scale),
                                    direction_count = 1,
                                    scale = 0.5 * scale,
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/worm/worm-die-02-mask.png",
                            flags = { "mask" },
                            line_length = 6,
                            width = 156,
                            height = 144,
                            frame_count = 24,
                            shift = util.mul_shift(util.by_pixel(-33, -31), scale),
                            direction_count = 1,
                            scale = scale,
                            tint = tint2,
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/worm/hr-worm-die-02-mask.png",
                                    flags = { "mask" },
                                    line_length = 6,
                                    width = 306,
                                    height = 282,
                                    frame_count = 24,
                                    shift = util.mul_shift(util.by_pixel(-33, -31), scale),
                                    direction_count = 1,
                                    scale = 0.5 * scale,
                                    tint = tint2,
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/worm/worm-die-02-shadow.png",
                            line_length = 6,
                            width = 296,
                            height = 82,
                            frame_count = 24,
                            shift = util.mul_shift(util.by_pixel(16, 4), scale),
                            direction_count = 1,
                            scale = scale,
                            draw_as_shadow = true,
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/worm/hr-worm-die-02-shadow.png",
                                    line_length = 6,
                                    width = 588,
                                    height = 160,
                                    frame_count = 24,
                                    shift = util.mul_shift(util.by_pixel(17, 5), scale),
                                    direction_count = 1,
                                    scale = 0.5 * scale,
                                    draw_as_shadow = true,
                                }
                        },
                    },
            },
            {
                layers=
                    {
                        {
                            filename = "__base__/graphics/entity/worm/worm-die-03.png",
                            line_length = 6,
                            width = 130,
                            height = 164,
                            frame_count = 24,
                            shift = util.mul_shift(util.by_pixel(16, -48), scale),
                            direction_count = 1,
                            scale = scale,
                            tint = tint,
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/worm/hr-worm-die-03.png",
                                    line_length = 6,
                                    width = 260,
                                    height = 328,
                                    frame_count = 24,
                                    shift = util.mul_shift(util.by_pixel(16, -48), scale),
                                    direction_count = 1,
                                    tint = tint,
                                    scale = 0.5 * scale,
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/worm/worm-die-03-mask.png",
                            flags = { "mask" },
                            line_length = 6,
                            width = 124,
                            height = 136,
                            frame_count = 24,
                            shift = util.mul_shift(util.by_pixel(14, -34), scale),
                            direction_count = 1,
                            scale = scale,
                            tint = tint2,
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/worm/hr-worm-die-03-mask.png",
                                    flags = { "mask" },
                                    line_length = 6,
                                    width = 246,
                                    height = 272,
                                    frame_count = 24,
                                    shift = util.mul_shift(util.by_pixel(14, -34), scale),
                                    direction_count = 1,
                                    scale = 0.5 * scale,
                                    tint = tint2,
                                }
                        },
                        {
                            filename = "__base__/graphics/entity/worm/worm-die-03-shadow.png",
                            line_length = 6,
                            width = 204,
                            height = 116,
                            frame_count = 24,
                            shift = util.mul_shift(util.by_pixel(52, -34), scale),
                            direction_count = 1,
                            scale = scale,
                            draw_as_shadow = true,
                            hr_version =
                                {
                                    filename = "__base__/graphics/entity/worm/hr-worm-die-03-shadow.png",
                                    line_length = 6,
                                    width = 404,
                                    height = 224,
                                    frame_count = 24,
                                    shift = util.mul_shift(util.by_pixel(53, -32), scale),
                                    direction_count = 1,
                                    scale = 0.5 * scale,
                                    draw_as_shadow = true,
                                }
                        },
                    }
            }
        }
end


return wormUtils
