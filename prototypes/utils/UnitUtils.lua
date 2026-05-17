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
-- along with this program.  If not, see <https://www.gnu.org/licenses/>


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

-- Factorio 2.0 sprite sheet dimensions (from base game .png files)
-- biter-die: 3920x3456 per file, 4 files, slice=8 (8 dirs/row, 2 rows/file)
-- biter-die-shadow: 3228x3460 per file, 5 files, slice=6
-- biter-run: 3184x2480 per file, 4 files, slice=8
-- biter-attack: varies, 4 files, slice=11

local dieWidth = 3920
local dieHeight = 3456
local dieShadowWidth = 3228
local dieShadowHeight = 3460
local runWidth = 3184
local runHeight = 2480
local attackWidth = 182 * 16  -- estimated from old dimensions
local attackHeight = 176 * 4

local function vanillaDieBiter(scale, tint1, tint2)
    return {
        layers = {
            -- Base die layer
            {
                sheet = {
                    filename = "__base__/graphics/entity/biter/biter-die",
                    width = dieWidth,
                    height = dieHeight,
                    frame_count = 17,
                    line_length = 8,
                    variation_count = 2,
                    scale = scale * 0.5,
                    shift = util.mul_shift(util.by_pixel(-2, -4), scale),
                },
            },
            -- Mask1 layer
            {
                sheet = {
                    filename = "__base__/graphics/entity/biter/biter-die-mask1",
                    width = dieWidth,
                    height = dieHeight,
                    frame_count = 17,
                    line_length = 8,
                    variation_count = 2,
                    scale = scale * 0.5,
                    tint = tint1,
                    shift = util.mul_shift(util.by_pixel(0, -22), scale),
                },
            },
            -- Mask2 layer
            {
                sheet = {
                    filename = "__base__/graphics/entity/biter/biter-die-mask2",
                    width = dieWidth,
                    height = dieHeight,
                    frame_count = 17,
                    line_length = 8,
                    variation_count = 2,
                    scale = scale * 0.5,
                    tint = tint2,
                    shift = util.mul_shift(util.by_pixel(-2, -22), scale),
                },
            },
            -- Shadow layer
            {
                sheet = {
                    filename = "__base__/graphics/entity/biter/biter-die-shadow",
                    width = dieShadowWidth,
                    height = dieShadowHeight,
                    frame_count = 17,
                    line_length = 6,
                    variation_count = 3,
                    scale = scale,
                    shift = util.mul_shift(util.by_pixel(4, 0), scale),
                    draw_as_shadow = true,
                },
            },
        },
    }
end

local function armoredDieBiter(scale, tint1, tint2)
    -- Fallback to vanilla if ArmouredBiters not available
    return vanillaDieBiter(scale, tint1, tint2)
end

local function vanillaAttackBiter(scale, tint1, tint2)
    return {
        layers = {
            {
                sheet = {
                    filename = "__base__/graphics/entity/biter/biter-attack",
                    width = attackWidth,
                    height = attackHeight,
                    frame_count = 11,
                    line_length = 16,
                    variation_count = 1,
                    scale = scale * 0.5,
                    animation_speed = 0.4,
                    shift = util.mul_shift(util.by_pixel(-2, -26), scale),
                },
            },
            {
                sheet = {
                    filename = "__base__/graphics/entity/biter/biter-attack-mask1",
                    width = attackWidth,
                    height = attackHeight,
                    frame_count = 11,
                    line_length = 16,
                    variation_count = 1,
                    scale = scale * 0.5,
                    tint = tint1,
                    animation_speed = 0.4,
                    shift = util.mul_shift(util.by_pixel(0, -42), scale),
                },
            },
            {
                sheet = {
                    filename = "__base__/graphics/entity/biter/biter-attack-mask2",
                    width = attackWidth,
                    height = attackHeight,
                    frame_count = 11,
                    line_length = 16,
                    variation_count = 1,
                    scale = scale * 0.5,
                    tint = tint2,
                    animation_speed = 0.4,
                    shift = util.mul_shift(util.by_pixel(-2, -42), scale),
                },
            },
            {
                sheet = {
                    filename = "__base__/graphics/entity/biter/biter-attack-shadow",
                    width = attackWidth,
                    height = attackHeight,
                    frame_count = 11,
                    line_length = 16,
                    variation_count = 1,
                    scale = scale,
                    animation_speed = 0.4,
                    draw_as_shadow = true,
                    shift = util.mul_shift(util.by_pixel(30, 0), scale),
                },
            },
        },
    }
end

local function armoredAttackBiter(scale, tint1, tint2)
    return vanillaAttackBiter(scale, tint1, tint2)
end

local function armoredRunBiter(scale, tint1, tint2)
    return vanillaRunBiter(scale, tint1, tint2)
end

local function vanillaRunBiter(scale, tint1, tint2)
    return {
        layers = {
            {
                sheet = {
                    filename = "__base__/graphics/entity/biter/biter-run",
                    width = runWidth,
                    height = runHeight,
                    frame_count = 16,
                    line_length = 16,
                    variation_count = 1,
                    scale = scale * 0.5,
                    shift = util.mul_shift(util.by_pixel(-2, -6), scale),
                },
            },
            {
                sheet = {
                    filename = "__base__/graphics/entity/biter/biter-run-mask1",
                    width = runWidth,
                    height = runHeight,
                    frame_count = 16,
                    line_length = 16,
                    variation_count = 1,
                    scale = scale * 0.5,
                    tint = tint1,
                    shift = util.mul_shift(util.by_pixel(0, -38), scale),
                },
            },
            {
                sheet = {
                    filename = "__base__/graphics/entity/biter/biter-run-mask2",
                    width = runWidth,
                    height = runHeight,
                    frame_count = 16,
                    line_length = 16,
                    variation_count = 1,
                    scale = scale * 0.5,
                    tint = tint2,
                    shift = util.mul_shift(util.by_pixel(-2, -38), scale),
                },
            },
            {
                sheet = {
                    filename = "__base__/graphics/entity/biter/biter-run-shadow",
                    width = runWidth,
                    height = runHeight,
                    frame_count = 16,
                    line_length = 16,
                    variation_count = 1,
                    scale = scale,
                    draw_as_shadow = true,
                    shift = util.mul_shift(util.by_pixel(8, 0), scale),
                },
            },
        },
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
