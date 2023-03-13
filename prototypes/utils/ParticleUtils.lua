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


local particleUtils = {}

-- module code

local particleSet = {}
-- local particleCount = 0

function particleUtils.makeDamagedParticle(attributes)
    local name = "particle-" .. ((attributes.effectiveLevel and (attributes.effectiveLevel .. "-")) or "")
        .. (attributes.faction or attributes.name) .. "-damaged-particle-rampant"

    local o = {
        type = "create-entity",
        entity_name = name,
        offset_deviation = {{-0.5, -0.5}, {0.5, 0.5}},
        offsets = {{0,0}},
        damage_type_filters = {"fire", "acid"}
    }

    if not particleSet[name] then
        particleSet[name] = true
        -- particleCount = particleCount + 1
        -- print(particleCount)
    else
        return o
    end

    data:extend({
            {
                type = "explosion",
                name = name,
                flags = {"not-on-map"},
                subgroup = "hit-effects",
                icon = "__core__/graphics/icons/mip/trash.png",
                height = 0.3,
                icon_size = 32,
                animations =
                    {
                        util.empty_sprite()
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
                                            type = "create-entity",
                                            entity_name = attributes.hitSprayName,
                                            repeat_count = 1,
                                        }
                                    }
                            }
                    }
            }
    })

    return o
end


local function makeBloodParticle(attributes)
    local name = "particle-" .. ((attributes.effectiveLevel and (attributes.effectiveLevel .. "-")) or "")
        .. (attributes.faction or attributes.name) .. "-blood-particle-rampant"
    local tint = attributes.tint2

    if not particleSet[name] then
        particleSet[name] = true
        -- particleCount = particleCount + 1
        -- print(particleCount)
    else
        return name
    end

    data:extend({
            {
                type = "optimized-particle",
                name = name,
                flags = {"not-on-map"},
                render_layer = "higher-object-under",
                render_layer_when_on_ground = "corpse",
                movement_modifier_when_on_ground = 0.2,
                life_time = 240,
                pictures =
                    {
                        {
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-01.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-01.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-02.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-02.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-03.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-03.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-04.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-04.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-05.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-05.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-06.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-06.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-07.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-07.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-08.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-08.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-09.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-09.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-10.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-10.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-11.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-11.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-12.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-12.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-01.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-01.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-02.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-02.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-03.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-03.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-04.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-04.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-05.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-05.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-06.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-06.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-07.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-07.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-08.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-08.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-09.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-09.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-10.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-10.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-11.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-11.png",
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
                            filename = "__base__/graphics/particle/blood-particle/blood-particle-12.png",
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
                                    filename = "__base__/graphics/particle/blood-particle/hr-blood-particle-12.png",
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
                    -- {
                    --     type = "set-tile",
                    --     tile_name = "landfill",
                    --     radius = 1,
                    -- }
                    -- {
                    --     type = "create-entity",
                    --     entity_name = "water-splash"
                    -- }
                    nil

            }
    })

    return name
end


function particleUtils.makeBloodFountains(attributes)

    local particleScaler = settings.startup["rampant--scaleParticleCount"].value

    local bloodParticle = makeBloodParticle(attributes)

    data:extend({
            {
                type = "particle-source",
                name = attributes.name .. "-damaged-fountain-rampant",
                particle = bloodParticle,
                time_to_live = 10,
                time_to_live_deviation = 5,
                time_before_start = 0,
                time_before_start_deviation = 3,
                height = 0.3,
                height_deviation = 0.1,
                vertical_speed = 0.02,
                vertical_speed_deviation = 0.08,
                horizontal_speed = 0.07,
                horizontal_speed_deviation = 0.04
            },
            {
                type = "particle-source",
                name = attributes.name .. "-blood-fountain-rampant",
                particle = bloodParticle,
                time_to_live = 15,
                time_to_live_deviation = 5,
                time_before_start = 0,
                time_before_start_deviation = 3,
                height = 0.5,
                height_deviation = 0.1,
                vertical_speed = 0.05,
                vertical_speed_deviation = 0.08,
                horizontal_speed = 0.05,
                horizontal_speed_deviation = 0.04
            },
            {
                type = "particle-source",
                name = attributes.name .. "-blood-fountain-big-rampant",
                particle = bloodParticle,
                time_to_live = 30,
                time_to_live_deviation = 5,
                time_before_start = 0,
                time_before_start_deviation = 5,
                height = 1,
                height_deviation = 0.1,
                vertical_speed = 0.05,
                vertical_speed_deviation = 0.08,
                horizontal_speed = 0.05,
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
                                        {
                                            type = "create-entity",
                                            entity_name = attributes.name .. "-blood-fountain-rampant",
                                            repeat_count = math.max(10 * particleScaler, 1),
                                            repeat_count_deviation = math.max(3 * particleScaler, 1),
                                            offset_deviation = {{-0.4, -0.4}, {0.4, 0.4}}
                                        },
                                        {
                                            type = "create-particle",
                                            repeat_count = 1,
                                            repeat_count_deviation = 2,
                                            probability = 1,
                                            affects_target = false,
                                            show_in_tooltip = false,
                                            particle_name = "guts-entrails-particle-small-medium",
                                            offsets = {
                                                { 0.03906, -0.02344 }
                                            },
                                            offset_deviation = { { -1, -0.6992 }, { 1, 0.6992 } },
                                            tile_collision_mask = nil,
                                            initial_height = 0.4,
                                            initial_height_deviation = 0.4,
                                            initial_vertical_speed = 0.04,
                                            initial_vertical_speed_deviation = 0.05,
                                            speed_from_center = 0.04,
                                            speed_from_center_deviation = 0.05,
                                            frame_speed = 1,
                                            frame_speed_deviation = 0.955,
                                            tail_length = 3,
                                            tail_length_deviation = 0,
                                            tail_width = 1
                                        }
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
                                            repeat_count = math.max(65 * particleScaler, 1),
                                            repeat_count_deviation = math.max(20 * particleScaler, 1),
                                            particle_name = bloodParticle,
                                            initial_height = 0.5,
                                            speed_from_center = 0.08,
                                            speed_from_center_deviation = 0.05,
                                            initial_vertical_speed = -0.01,
                                            initial_vertical_speed_deviation = 0.02,
                                            offset_deviation = {{-0.4, -0.4}, {0.4, 0.4}}
                                        },
                                        {
                                            type = "create-particle",
                                            repeat_count = 1,
                                            repeat_count_deviation = 2,
                                            probability = 1,
                                            affects_target = false,
                                            show_in_tooltip = false,
                                            particle_name = "guts-entrails-particle-small-medium",
                                            offsets = {
                                                { 0.03906, -0.02344 }
                                            },
                                            offset_deviation = { { -1, -0.6992 }, { 1, 0.6992 } },
                                            tile_collision_mask = nil,
                                            initial_height = 0.4,
                                            initial_height_deviation = 0.4,
                                            initial_vertical_speed = 0.04,
                                            initial_vertical_speed_deviation = 0.05,
                                            speed_from_center = 0.04,
                                            speed_from_center_deviation = 0.05,
                                            frame_speed = 1,
                                            frame_speed_deviation = 0.955,
                                            tail_length = 3,
                                            tail_length_deviation = 0,
                                            tail_width = 1
                                        },
                                        {
                                            type = "create-particle",
                                            repeat_count = 2,
                                            repeat_count_deviation = 2,
                                            probability = 1,
                                            affects_target = false,
                                            show_in_tooltip = false,
                                            particle_name = "guts-entrails-particle-big",
                                            offsets = {
                                                { 0, 0 }
                                            },
                                            offset_deviation = { { -0.5, -0.5 }, { 0.5, 0.5 } },
                                            tile_collision_mask = nil,
                                            initial_height = 0.02,
                                            initial_height_deviation = 0.5,
                                            initial_vertical_speed = 0.125,
                                            initial_vertical_speed_deviation = 0.05,
                                            speed_from_center = 0.035,
                                            speed_from_center_deviation = 0.05,
                                            frame_speed = 1,
                                            frame_speed_deviation = 0,
                                            tail_length = 2,
                                            tail_length_deviation = 0,
                                            tail_width = 1
                                        },
                                        {
                                            type = "create-entity",
                                            entity_name = attributes.name .. "-blood-fountain-rampant",
                                            repeat_count = math.max(20 * particleScaler, 1),
                                            repeat_count_deviation = math.max(5 * particleScaler, 1),
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
                                            repeat_count = math.max(100 * particleScaler, 1),
                                            repeat_count_deviation = math.max(20 * particleScaler, 1),
                                            particle_name = bloodParticle,
                                            initial_height = 0.5,
                                            speed_from_center = 0.08,
                                            speed_from_center_deviation = 0.05,
                                            initial_vertical_speed = -0.01,
                                            initial_vertical_speed_deviation = 0.02,
                                            offset_deviation = {{-0.4, -0.4}, {0.4, 0.4}}
                                        },
                                        {
                                            type = "create-particle",
                                            repeat_count = 4,
                                            repeat_count_deviation = 2,
                                            probability = 1,
                                            affects_target = false,
                                            show_in_tooltip = false,
                                            particle_name = "guts-entrails-particle-small-medium",
                                            offsets = {
                                                { 0.03906, -0.02344 }
                                            },
                                            offset_deviation = { { -1, -0.6992 }, { 1, 0.6992 } },
                                            tile_collision_mask = nil,
                                            initial_height = 0.4,
                                            initial_height_deviation = 0.4,
                                            initial_vertical_speed = 0.04,
                                            initial_vertical_speed_deviation = 0.05,
                                            speed_from_center = 0.04,
                                            speed_from_center_deviation = 0.05,
                                            frame_speed = 1,
                                            frame_speed_deviation = 0.955,
                                            tail_length = 3,
                                            tail_length_deviation = 0,
                                            tail_width = 1
                                        },
                                        {
                                            type = "create-particle",
                                            repeat_count = 4,
                                            repeat_count_deviation = 1,
                                            probability = 1,
                                            affects_target = false,
                                            show_in_tooltip = false,
                                            particle_name = "guts-entrails-particle-big",
                                            offsets = {
                                                { 0, 0 }
                                            },
                                            offset_deviation = { { -0.5, -0.5 }, { 0.5, 0.5 } },
                                            tile_collision_mask = nil,
                                            initial_height = 0.02,
                                            initial_height_deviation = 0.5,
                                            initial_vertical_speed = 0.125,
                                            initial_vertical_speed_deviation = 0.05,
                                            speed_from_center = 0.035,
                                            speed_from_center_deviation = 0.05,
                                            frame_speed = 1,
                                            frame_speed_deviation = 0,
                                            tail_length = 2,
                                            tail_length_deviation = 0,
                                            tail_width = 1
                                        },
                                        {
                                            type = "create-entity",
                                            entity_name = attributes.name .. "-blood-fountain-big-rampant",
                                            repeat_count = math.max(30 * particleScaler, 1),
                                            repeat_count_deviation = math.max(5 * particleScaler, 1),
                                            offset_deviation = {{-1.6, -1.6}, {1.6, 1.6}}
                                        }
                                    }
                            }
                    }
            }
    })
end

return particleUtils
