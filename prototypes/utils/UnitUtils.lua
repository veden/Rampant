local unitUtils = {}

function unitUtils.spitter_water_reflection(scale)
  return
  {
    pictures =
    {
      filename = "__base__/graphics/entity/spitter/spitter-reflection.png",
      priority = "extra-high",
      width = 20,
      height = 32,
      shift = util.by_pixel(5, 15),
      scale = 5 * scale,
      variation_count = 1,
    },
    rotate = true,
    orientation_to_variation = false
  }
end

function unitUtils.biter_water_reflection(scale)
  return
  {
    pictures =
    {
      filename = "__base__/graphics/entity/biter/biter-reflection.png",
      priority = "extra-high",
      width = 20,
      height = 28,
      shift = util.by_pixel(5, 15),
      scale = 5 * scale,
      variation_count = 1,
    },
    rotate = true,
    orientation_to_variation = false
  }
end


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

local function vanillaDieBiter(scale, tint1, tint2)
    return {
        layers=
            {
                {
                    filenames =
                        {
                            "__base__/graphics/entity/biter/biter-die-01.png",
                            "__base__/graphics/entity/biter/biter-die-02.png",
                            "__base__/graphics/entity/biter/biter-die-03.png",
                            "__base__/graphics/entity/biter/biter-die-04.png",
                            "__base__/graphics/entity/biter/biter-die-05.png",
                            "__base__/graphics/entity/biter/biter-die-06.png",
                            "__base__/graphics/entity/biter/biter-die-07.png",
                            "__base__/graphics/entity/biter/biter-die-08.png",
                            "__base__/graphics/entity/biter/biter-die-09.png",
                            "__base__/graphics/entity/biter/biter-die-10.png",
                            "__base__/graphics/entity/biter/biter-die-11.png",
                            "__base__/graphics/entity/biter/biter-die-12.png",
                            "__base__/graphics/entity/biter/biter-die-13.png",
                            "__base__/graphics/entity/biter/biter-die-14.png",
                            "__base__/graphics/entity/biter/biter-die-15.png",
                            "__base__/graphics/entity/biter/biter-die-16.png",
                            "__base__/graphics/entity/biter/biter-die-17.png",
                        },
                    slice = 4,
                    lines_per_file = 4,
                    line_length = 4,
                    width = 276,
                    height = 202,
                    frame_count = 17,
                    direction_count = 16,
                    shift= util.mul_shift(util.by_pixel(-2, -4), scale),
                    scale = scale,
                    hr_version =
                        {
                            filenames =
                                {
                                    "__base__/graphics/entity/biter/hr-biter-die-01.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-02.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-03.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-04.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-05.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-06.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-07.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-08.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-09.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-10.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-11.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-12.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-13.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-14.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-15.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-16.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-17.png",
                                },
                            slice = 4,
                            lines_per_file = 4,
                            line_length = 4,
                            width = 544,
                            height = 402,
                            frame_count = 17,
                            shift = util.mul_shift(util.by_pixel(0, -4), scale),
                            direction_count = 16,
                            scale = 0.5 * scale,
                        }
                },
                {
                    filenames =
                        {
                            "__base__/graphics/entity/biter/biter-die-mask1-01.png",
                            "__base__/graphics/entity/biter/biter-die-mask1-02.png",
                            "__base__/graphics/entity/biter/biter-die-mask1-03.png",
                            "__base__/graphics/entity/biter/biter-die-mask1-04.png",
                            "__base__/graphics/entity/biter/biter-die-mask1-05.png",
                            "__base__/graphics/entity/biter/biter-die-mask1-06.png",
                            "__base__/graphics/entity/biter/biter-die-mask1-07.png",
                            "__base__/graphics/entity/biter/biter-die-mask1-08.png",
                            "__base__/graphics/entity/biter/biter-die-mask1-09.png",
                            "__base__/graphics/entity/biter/biter-die-mask1-10.png",
                            "__base__/graphics/entity/biter/biter-die-mask1-11.png",
                            "__base__/graphics/entity/biter/biter-die-mask1-12.png",
                            "__base__/graphics/entity/biter/biter-die-mask1-13.png",
                            "__base__/graphics/entity/biter/biter-die-mask1-14.png",
                            "__base__/graphics/entity/biter/biter-die-mask1-15.png",
                            "__base__/graphics/entity/biter/biter-die-mask1-16.png",
                            "__base__/graphics/entity/biter/biter-die-mask1-17.png",
                        },
                    slice = 4,
                    lines_per_file = 4,
                    flags = { "mask" },
                    line_length = 4,
                    width = 198,
                    height = 166,
                    frame_count = 17,
                    direction_count = 16,
                    shift = util.mul_shift(util.by_pixel(0, -22), scale),
                    scale = scale,
                    tint = tint1,
                    hr_version =
                        {
                            filenames =
                                {
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-01.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-02.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-03.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-04.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-05.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-06.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-07.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-08.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-09.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-10.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-11.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-12.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-13.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-14.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-15.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-16.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask1-17.png",
                                },
                            slice = 4,
                            lines_per_file = 4,
                            line_length = 4,
                            width = 398,
                            height = 328,
                            frame_count = 17,
                            shift = util.mul_shift(util.by_pixel(-1, -21), scale),
                            direction_count = 16,
                            scale = 0.5 * scale,
                            tint = tint1,
                        }
                },
                {
                    filenames =
                        {
                            "__base__/graphics/entity/biter/biter-die-mask2-01.png",
                            "__base__/graphics/entity/biter/biter-die-mask2-02.png",
                            "__base__/graphics/entity/biter/biter-die-mask2-03.png",
                            "__base__/graphics/entity/biter/biter-die-mask2-04.png",
                            "__base__/graphics/entity/biter/biter-die-mask2-05.png",
                            "__base__/graphics/entity/biter/biter-die-mask2-06.png",
                            "__base__/graphics/entity/biter/biter-die-mask2-07.png",
                            "__base__/graphics/entity/biter/biter-die-mask2-08.png",
                            "__base__/graphics/entity/biter/biter-die-mask2-09.png",
                            "__base__/graphics/entity/biter/biter-die-mask2-10.png",
                            "__base__/graphics/entity/biter/biter-die-mask2-11.png",
                            "__base__/graphics/entity/biter/biter-die-mask2-12.png",
                            "__base__/graphics/entity/biter/biter-die-mask2-13.png",
                            "__base__/graphics/entity/biter/biter-die-mask2-14.png",
                            "__base__/graphics/entity/biter/biter-die-mask2-15.png",
                            "__base__/graphics/entity/biter/biter-die-mask2-16.png",
                            "__base__/graphics/entity/biter/biter-die-mask2-17.png",
                        },
                    slice = 4,
                    lines_per_file = 4,
                    flags = { "mask" },
                    line_length = 4,
                    width = 200,
                    height = 166,
                    frame_count = 17,
                    direction_count = 16,
                    shift = util.mul_shift(util.by_pixel(-2, -22), scale),
                    scale = scale,
                    tint = tint2,
                    hr_version =
                        {
                            filenames =
                                {
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-01.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-02.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-03.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-04.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-05.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-06.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-07.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-08.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-09.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-10.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-11.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-12.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-13.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-14.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-15.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-16.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-mask2-17.png",
                                },
                            slice = 4,
                            lines_per_file = 4,
                            line_length = 4,
                            width = 396,
                            height = 330,
                            frame_count = 17,
                            shift = util.mul_shift(util.by_pixel(-1, -22), scale),
                            direction_count = 16,
                            scale = 0.5 * scale,
                            tint = tint2,
                        }
                },
                {
                    filenames =
                        {
                            "__base__/graphics/entity/biter/biter-die-shadow-01.png",
                            "__base__/graphics/entity/biter/biter-die-shadow-02.png",
                            "__base__/graphics/entity/biter/biter-die-shadow-03.png",
                            "__base__/graphics/entity/biter/biter-die-shadow-04.png",
                            "__base__/graphics/entity/biter/biter-die-shadow-05.png",
                            "__base__/graphics/entity/biter/biter-die-shadow-06.png",
                            "__base__/graphics/entity/biter/biter-die-shadow-07.png",
                            "__base__/graphics/entity/biter/biter-die-shadow-08.png",
                            "__base__/graphics/entity/biter/biter-die-shadow-09.png",
                            "__base__/graphics/entity/biter/biter-die-shadow-10.png",
                            "__base__/graphics/entity/biter/biter-die-shadow-11.png",
                            "__base__/graphics/entity/biter/biter-die-shadow-12.png",
                            "__base__/graphics/entity/biter/biter-die-shadow-13.png",
                            "__base__/graphics/entity/biter/biter-die-shadow-14.png",
                            "__base__/graphics/entity/biter/biter-die-shadow-15.png",
                            "__base__/graphics/entity/biter/biter-die-shadow-16.png",
                            "__base__/graphics/entity/biter/biter-die-shadow-17.png",
                        },
                    slice = 4,
                    lines_per_file = 4,
                    line_length = 4,
                    width = 282,
                    height = 192,
                    frame_count = 17,
                    shift = util.mul_shift(util.by_pixel(4, 0), scale),
                    direction_count = 16,
                    scale = scale,
                    draw_as_shadow = true,
                    hr_version =
                        {
                            filenames =
                                {
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-01.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-02.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-03.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-04.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-05.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-06.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-07.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-08.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-09.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-10.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-11.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-12.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-13.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-14.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-15.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-16.png",
                                    "__base__/graphics/entity/biter/hr-biter-die-shadow-17.png",
                                },
                            slice = 4,
                            lines_per_file = 4,
                            line_length = 4,
                            width = 564,
                            height = 384,
                            frame_count = 17,
                            shift = util.mul_shift(util.by_pixel(4, 0), scale),
                            direction_count = 16,
                            scale = 0.5 * scale,
                            draw_as_shadow = true,
                        }
                },
            }
    }
end

local function armoredDieBiter(scale, tint1, tint2)
    return {
        layers=
            {
                {
                    filenames =
                        {
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-01.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-02.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-03.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-04.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-05.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-06.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-07.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-08.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-09.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-10.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-11.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-12.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-13.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-14.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-15.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-16.png"
                        },
                    slice = 4,
                    lines_per_file = 4,
                    line_length = 4,
                    width = 238,
                    height = 170,
                    frame_count = 16,
                    direction_count = 16,
                    shift= {0,0},
                    tint=tint1,
                    scale = scale,
                    hr_version =
                        {
                            filenames =
                                {
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-01.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-02.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-03.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-04.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-05.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-06.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-07.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-08.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-09.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-10.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-11.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-12.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-13.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-14.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-15.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-16.png"
                                },
                            slice = 4,
                            lines_per_file = 4,
                            line_length = 4,
                            width = 476,
                            height = 340,
                            frame_count = 16,
                            shift = {0,0},
                            direction_count = 16,
                            scale = 0.5 * scale,
                        }
                },
                {
                    filenames =
                        {
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1-01.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1-02.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1-03.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1-04.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1-05.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1-06.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1-07.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1-08.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1-09.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1-10.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1-11.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1-12.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1-13.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1-14.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1-15.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask1-16.png"
                        },
                    slice = 4,
                    lines_per_file = 4,
                    flags = { "mask" },
                    line_length = 4,
                    width = 238,
                    height = 170,
                    frame_count = 16,
                    direction_count = 16,
                    shift = {0,0},
                    scale = scale,
                    tint = tint2,
                    hr_version =
                        {
                            filenames =
                                {
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask1-01.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask1-02.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask1-03.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask1-04.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask1-05.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask1-06.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask1-07.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask1-08.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask1-09.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask1-10.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask1-11.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask1-12.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask1-13.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask1-14.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask1-15.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask1-16.png"
                                },
                            slice = 4,
                            lines_per_file = 4,
                            line_length = 4,
                            width = 476,
                            height = 340,
                            frame_count = 16,
                            shift = {0,0},
                            direction_count = 16,
                            scale = 0.5 * scale,
                            tint = tint1,
                        }
                },
                {
                    filenames =
                        {
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2-01.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2-02.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2-03.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2-04.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2-05.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2-06.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2-07.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2-08.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2-09.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2-10.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2-11.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2-12.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2-13.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2-14.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2-15.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-mask2-16.png"
                        },
                    slice = 4,
                    lines_per_file = 4,
                    flags = { "mask" },
                    line_length = 4,
                    width = 238,
                    height = 170,
                    frame_count = 16,
                    direction_count = 16,
                    shift = {0,0},
                    scale = scale,
                    tint = tint2,
                    hr_version =
                        {
                            filenames =
                                {
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask2-01.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask2-02.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask2-03.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask2-04.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask2-05.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask2-06.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask2-07.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask2-08.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask2-09.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask2-10.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask2-11.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask2-12.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask2-13.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask2-14.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask2-15.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-mask2-16.png"
                                },
                            slice = 4,
                            lines_per_file = 4,
                            line_length = 4,
                            width = 476,
                            height = 340,
                            frame_count = 16,
                            shift = {0,0},
                            direction_count = 16,
                            scale = 0.5 * scale,
                            tint = tint2,
                        }
                },
                {
                    filenames =
                        {
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow-01.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow-02.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow-03.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow-04.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow-05.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow-06.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow-07.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow-08.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow-09.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow-10.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow-11.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow-12.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow-13.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow-14.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow-15.png",
                            "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-die-shadow-16.png"
                        },
                    slice = 4,
                    lines_per_file = 4,
                    line_length = 4,
                    width = 238,
                    height = 170,
                    frame_count = 16,
                    shift = {0,0},
                    direction_count = 16,
                    scale = scale,
                    draw_as_shadow = true,
                    hr_version =
                        {
                            filenames =
                                {
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-shadow-01.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-shadow-02.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-shadow-03.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-shadow-04.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-shadow-05.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-shadow-06.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-shadow-07.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-shadow-08.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-shadow-09.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-shadow-10.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-shadow-11.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-shadow-12.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-shadow-13.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-shadow-14.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-shadow-15.png",
                                    "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-die-shadow-16.png"
                                },
                            slice = 4,
                            lines_per_file = 4,
                            line_length = 4,
                            width = 476,
                            height = 340,
                            frame_count = 16,
                            shift = {0,0},
                            direction_count = 16,
                            scale = 0.5 * scale,
                            draw_as_shadow = true,
                        }
                }
            }
    }
end

local function vanillaAttackBiter(scale, tint1, tint2)
    return
        {
            layers=
                {
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/biter/biter-attack-01.png",
                                "__base__/graphics/entity/biter/biter-attack-02.png",
                                "__base__/graphics/entity/biter/biter-attack-03.png",
                                "__base__/graphics/entity/biter/biter-attack-04.png",
                            },
                        slice = 11,
                        lines_per_file = 4,
                        line_length = 16,
                        width = 182,
                        height = 176,
                        frame_count = 11,
                        direction_count = 16,
                        animation_speed = 0.4,
                        shift = util.mul_shift(util.by_pixel(-2, -26), scale),
                        scale = scale,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__base__/graphics/entity/biter/hr-biter-attack-01.png",
                                        "__base__/graphics/entity/biter/hr-biter-attack-02.png",
                                        "__base__/graphics/entity/biter/hr-biter-attack-03.png",
                                        "__base__/graphics/entity/biter/hr-biter-attack-04.png",
                                    },
                                slice = 11,
                                lines_per_file = 4,
                                line_length = 16,
                                width = 356,
                                height = 348,
                                frame_count = 11,
                                shift = util.mul_shift(util.by_pixel(0, -25), scale),
                                direction_count = 16,
                                animation_speed = 0.4,
                                scale = 0.5 * scale,
                            }
                    },
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/biter/biter-attack-mask1-01.png",
                                "__base__/graphics/entity/biter/biter-attack-mask1-02.png",
                                "__base__/graphics/entity/biter/biter-attack-mask1-03.png",
                                "__base__/graphics/entity/biter/biter-attack-mask1-04.png",
                            },
                        slice = 11,
                        lines_per_file = 4,
                        flags = { "mask" },
                        line_length = 16,
                        width = 178,
                        height = 144,
                        frame_count = 11,
                        direction_count = 16,
                        animation_speed = 0.4,
                        shift = util.mul_shift(util.by_pixel(0, -42), scale),
                        scale = scale,
                        tint = tint1,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__base__/graphics/entity/biter/hr-biter-attack-mask1-01.png",
                                        "__base__/graphics/entity/biter/hr-biter-attack-mask1-02.png",
                                        "__base__/graphics/entity/biter/hr-biter-attack-mask1-03.png",
                                        "__base__/graphics/entity/biter/hr-biter-attack-mask1-04.png",
                                    },
                                slice = 11,
                                lines_per_file = 4,
                                line_length = 16,
                                width = 360,
                                height = 282,
                                frame_count = 11,
                                shift = util.mul_shift(util.by_pixel(-1, -41), scale),
                                direction_count = 16,
                                animation_speed = 0.4,
                                scale = 0.5 * scale,
                                tint = tint1,
                            }
                    },
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/biter/biter-attack-mask2-01.png",
                                "__base__/graphics/entity/biter/biter-attack-mask2-02.png",
                                "__base__/graphics/entity/biter/biter-attack-mask2-03.png",
                                "__base__/graphics/entity/biter/biter-attack-mask2-04.png",
                            },
                        slice = 11,
                        lines_per_file = 4,
                        flags = { "mask" },
                        line_length = 16,
                        width = 182,
                        height = 144,
                        frame_count = 11,
                        direction_count = 16,
                        animation_speed = 0.4,
                        shift = util.mul_shift(util.by_pixel(-2, -42), scale),
                        scale = scale,
                        tint = tint2,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__base__/graphics/entity/biter/hr-biter-attack-mask2-01.png",
                                        "__base__/graphics/entity/biter/hr-biter-attack-mask2-02.png",
                                        "__base__/graphics/entity/biter/hr-biter-attack-mask2-03.png",
                                        "__base__/graphics/entity/biter/hr-biter-attack-mask2-04.png",
                                    },
                                slice = 11,
                                lines_per_file = 4,
                                line_length = 16,
                                width = 358,
                                height = 282,
                                frame_count = 11,
                                shift = util.mul_shift(util.by_pixel(-1, -41), scale),
                                direction_count = 16,
                                animation_speed = 0.4,
                                scale = 0.5 * scale,
                                tint = tint2,
                            }
                    },
                    {
                        filenames =
                            {
                                "__base__/graphics/entity/biter/biter-attack-shadow-01.png",
                                "__base__/graphics/entity/biter/biter-attack-shadow-02.png",
                                "__base__/graphics/entity/biter/biter-attack-shadow-03.png",
                                "__base__/graphics/entity/biter/biter-attack-shadow-04.png",
                            },
                        slice = 11,
                        lines_per_file = 4,
                        line_length = 16,
                        width = 240,
                        height = 128,
                        frame_count = 11,
                        shift = util.mul_shift(util.by_pixel(30, 0), scale),
                        direction_count = 16,
                        animation_speed = 0.4,
                        scale = scale,
                        draw_as_shadow = true,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__base__/graphics/entity/biter/hr-biter-attack-shadow-01.png",
                                        "__base__/graphics/entity/biter/hr-biter-attack-shadow-02.png",
                                        "__base__/graphics/entity/biter/hr-biter-attack-shadow-03.png",
                                        "__base__/graphics/entity/biter/hr-biter-attack-shadow-04.png",
                                    },
                                slice = 11,
                                lines_per_file = 4,
                                line_length = 16,
                                width = 476,
                                height = 258,
                                frame_count = 11,
                                shift = util.mul_shift(util.by_pixel(31, -1), scale),
                                direction_count = 16,
                                animation_speed = 0.4,
                                scale = 0.5 * scale,
                                draw_as_shadow = true,
                            }
                    },
                }
        }
end

local function armoredAttackBiter(scale, tint1, tint2)
    return
        {
            layers=
                {
                    {
                        filenames =
                            {
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-01.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-02.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-03.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-04.png",
                            },
                        slice = 11,
                        lines_per_file = 4,
                        line_length = 16,
                        width = 238,
                        height = 170,
                        frame_count = 11,
                        direction_count = 16,
                        animation_speed = 0.4,
                        shift = {0,0},
                        tint=tint1,
                        scale = scale,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-attack-01.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-attack-02.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-attack-03.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-attack-04.png",
                                    },
                                slice = 11,
                                lines_per_file = 4,
                                line_length = 16,
                                width = 476,
                                height = 340,
                                frame_count = 11,
                                shift = {0,0},
                                direction_count = 16,
                                animation_speed = 0.4,
                                scale = 0.5 * scale,
                            }
                    },
                    {
                        filenames =
                            {
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-mask1-01.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-mask1-02.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-mask1-03.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-mask1-04.png",
                            },
                        slice = 11,
                        lines_per_file = 4,
                        flags = { "mask" },
                        line_length = 16,
                        width = 238,
                        height = 170,
                        frame_count = 11,
                        direction_count = 16,
                        animation_speed = 0.4,
                        shift = {0,0},
                        scale = scale,
                        tint = tint2,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-attack-mask1-01.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-attack-mask1-02.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-attack-mask1-03.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-attack-mask1-04.png",
                                    },
                                slice = 11,
                                lines_per_file = 4,
                                line_length = 16,
                                width = 476,
                                height = 340,
                                frame_count = 11,
                                shift = {0,0},
                                direction_count = 16,
                                animation_speed = 0.4,
                                scale = 0.5 * scale,
                                tint = tint1,
                            }
                    },
                    {
                        filenames =
                            {
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-mask2-01.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-mask2-02.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-mask2-03.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-mask2-04.png",
                            },
                        slice = 11,
                        lines_per_file = 4,
                        flags = { "mask" },
                        line_length = 16,
                        width = 238,
                        height = 170,
                        frame_count = 11,
                        direction_count = 16,
                        animation_speed = 0.4,
                        shift = {0,0},
                        scale = scale,
                        tint = tint2,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-attack-mask2-01.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-attack-mask2-02.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-attack-mask2-03.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-attack-mask2-04.png",
                                    },
                                slice = 11,
                                lines_per_file = 4,
                                line_length = 16,
                                width = 476,
                                height = 340,
                                frame_count = 11,
                                shift = {0,0},
                                direction_count = 16,
                                animation_speed = 0.4,
                                scale = 0.5 * scale,
                                tint = tint2,
                            }
                    },
                    {
                        filenames =
                            {
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-shadow-01.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-shadow-02.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-shadow-03.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-attack-shadow-04.png",
                            },
                        slice = 11,
                        lines_per_file = 4,
                        line_length = 16,
                        width = 238,
                        height = 170,
                        frame_count = 11,
                        shift = {0,0},
                        direction_count = 16,
                        animation_speed = 0.4,
                        scale = scale,
                        draw_as_shadow = true,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-attack-shadow-01.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-attack-shadow-02.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-attack-shadow-03.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-attack-shadow-04.png",
                                    },
                                slice = 11,
                                lines_per_file = 4,
                                line_length = 16,
                                width = 476,
                                height = 340,
                                frame_count = 11,
                                shift = {0,0},
                                direction_count = 16,
                                animation_speed = 0.4,
                                scale = 0.5 * scale,
                                draw_as_shadow = true,
                            }
                    },
                }
        }
end

local function armoredRunBiter(scale, tint1, tint2)
    return
        {
            layers=
                {
                    {
                        filenames =
                            {
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-01.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-02.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-03.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-04.png",
                            },
                        slice = 8,
                        lines_per_file = 8,
                        line_length = 8,
                        width = 238,
                        height = 170,
                        frame_count = 16,
                        direction_count = 16,
                        shift = {0,0},
                        scale = scale,
                        tint = tint1,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-run-01.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-run-02.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-run-03.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-run-04.png",
                                    },
                                slice = 8,
                                lines_per_file = 8,
                                line_length = 8,
                                width = 476,
                                height = 340,
                                frame_count = 16,
                                shift = {0,0},
                                direction_count = 16,
                                scale = 0.5 * scale,
                            }
                    },
                    {
                        filenames =
                            {
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-mask1-01.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-mask1-02.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-mask1-03.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-mask1-04.png",
                            },
                        slice = 8,
                        lines_per_file = 8,
                        flags = { "mask" },
                        line_length = 8,
                        width = 238,
                        height = 170,
                        frame_count = 16,
                        direction_count = 16,
                        shift = {0,0},
                        scale = scale,
                        tint = tint2,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-run-mask1-01.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-run-mask1-02.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-run-mask1-03.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-run-mask1-04.png",
                                    },
                                slice = 8,
                                lines_per_file = 8,
                                line_length = 8,
                                width = 476,
                                height = 340,
                                frame_count = 16,
                                shift = {0,0},
                                direction_count = 16,
                                scale = 0.5 * scale,
                                tint = tint1,
                            }
                    },
                    {
                        filenames =
                            {
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-mask2-01.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-mask2-02.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-mask2-03.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-mask2-04.png",
                            },
                        slice = 8,
                        lines_per_file = 8,
                        flags = { "mask" },
                        line_length = 8,
                        width = 238,
                        height = 170,
                        frame_count = 16,
                        direction_count = 16,
                        shift = {0,0},
                        scale = scale,
                        tint = tint2,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-run-mask2-01.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-run-mask2-02.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-run-mask2-03.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-run-mask2-04.png",
                                    },
                                slice = 8,
                                lines_per_file = 8,
                                line_length = 8,
                                width = 476,
                                height = 340,
                                frame_count = 16,
                                shift = {0,0},
                                direction_count = 16,
                                scale = 0.5 * scale,
                                tint = tint2,
                            }
                    },
                    {
                        filenames =
                            {
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-shadow-01.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-shadow-02.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-shadow-03.png",
                                "__ArmouredBiters__/graphics/armoured-biter/LowRes/armoured-biter-run-shadow-04.png",
                            },
                        slice = 8,
                        lines_per_file = 8,
                        line_length = 8,
                        width = 238,
                        height = 170,
                        frame_count = 16,
                        shift = {0,0},
                        direction_count = 16,
                        scale = scale,
                        draw_as_shadow = true,
                        hr_version =
                            {
                                filenames =
                                    {
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-run-shadow-01.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-run-shadow-02.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-run-shadow-03.png",
                                        "__ArmouredBiters__/graphics/armoured-biter/HighRes/hr-armoured-biter-run-shadow-04.png",
                                    },
                                slice = 8,
                                lines_per_file = 8,
                                line_length = 8,
                                width = 476,
                                height = 340,
                                frame_count = 16,
                                shift = {0,0},
                                direction_count = 16,
                                scale = 0.5 * scale,
                                draw_as_shadow = true,
                            }
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

function unitUtils.biterdieanimation(scale, tint1, tint2, altBiter)
    if (altBiter == "armored") and mods["ArmouredBiters"] then
        return armoredDieBiter(scale, tint1, tint2)
    else
        return vanillaDieBiter(scale, tint1, tint2)
    end
end

function unitUtils.biterattackanimation(scale, tint1, tint2, altBiter)
    if (altBiter == "armored") and mods["ArmouredBiters"] then
        return armoredAttackBiter(scale, tint1, tint2)
    else
        return vanillaAttackBiter(scale, tint1, tint2)
    end
end


function unitUtils.biterrunanimation(scale, tint1, tint2, altBiter)
    if (altBiter == "armored") and mods["ArmouredBiters"] then
        return armoredRunBiter(scale, tint1, tint2)
    else
        return vanillaRunBiter(scale, tint1, tint2)
    end
end


return unitUtils
