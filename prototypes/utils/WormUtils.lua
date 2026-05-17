-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
                        scale = scale
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
                        tint = tint2
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
                        draw_as_shadow = true
                    }
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
                        direction_count = 1
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
                        tint = tint2
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
                        draw_as_shadow = true
                    }
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
                        tint = tint
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
                        tint = tint2
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
                        draw_as_shadow = true
                    }
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
                        shift = util.mul_shift(util.by_pixel(-2, -48), scale)
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
                        tint = tint2
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
                        draw_as_shadow = true
                    }
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
                                "__base__/graphics/entity/worm/worm-attack-1.png",
                                "__base__/graphics/entity/worm/worm-attack-2.png",
                                "__base__/graphics/entity/worm/worm-attack-3.png",
                                "__base__/graphics/entity/worm/worm-attack-4.png",
                                "__base__/graphics/entity/worm/worm-attack-5.png",
                                "__base__/graphics/entity/worm/worm-attack-6.png",
                                "__base__/graphics/entity/worm/worm-attack-7.png",
                                "__base__/graphics/entity/worm/worm-attack-8.png",
                                "__base__/graphics/entity/worm/worm-attack-9.png",
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
                        scale = scale
                    },
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/worm/worm-attack-mask-1.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-2.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-3.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-4.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-5.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-6.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-7.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-8.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-9.png",
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
                        scale = scale
                    },
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/worm/worm-attack-shadow-1.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-2.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-3.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-4.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-5.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-6.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-7.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-8.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-9.png",
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
                        draw_as_shadow = true
                    }
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
                                "__base__/graphics/entity/worm/worm-attack-1.png",
                                "__base__/graphics/entity/worm/worm-attack-2.png",
                                "__base__/graphics/entity/worm/worm-attack-3.png",
                                "__base__/graphics/entity/worm/worm-attack-4.png",
                                "__base__/graphics/entity/worm/worm-attack-5.png",
                                "__base__/graphics/entity/worm/worm-attack-6.png",
                                "__base__/graphics/entity/worm/worm-attack-7.png",
                                "__base__/graphics/entity/worm/worm-attack-8.png",
                                "__base__/graphics/entity/worm/worm-attack-9.png",
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
                        scale = scale
                    },
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/worm/worm-attack-mask-1.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-2.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-3.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-4.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-5.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-6.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-7.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-8.png",
                                "__base__/graphics/entity/worm/worm-attack-mask-9.png",
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
                        scale = scale
                    },
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/worm/worm-attack-shadow-1.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-2.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-3.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-4.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-5.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-6.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-7.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-8.png",
                                "__base__/graphics/entity/worm/worm-attack-shadow-9.png",
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
                        draw_as_shadow = true
                    }
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
                            tint = tint
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
                            tint = tint2
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
                            draw_as_shadow = true
                        }
                    }
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
                            tint = tint
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
                            tint = tint2
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
                            draw_as_shadow = true
                        }
                    }
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
                            tint = tint
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
                            tint = tint2
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
                            draw_as_shadow = true
                        }
                    }
            }
        }
end

return wormUtils
