local imageUtils = {}

-- module code

local function foreach(table_, fun_)
    for _, tab in pairs(table_) do fun_(tab) end
    return table_
end

function imageUtils.create_burnt_patch_pictures(attributes)
    local base = {
        filename = "__base__/graphics/entity/fire-flame/burnt-patch.png",
        line_length = 3,
        width = 115,
        height = 56,
        frame_count = 9,
        axially_symmetrical = false,
        tint = attributes.tint2,
        direction_count = 1,
        shift = {-0.09375, 0.125},
    }

    local variations = {}

    for y=1,(base.frame_count / base.line_length) do
        for x=1,base.line_length do
            table.insert(variations,
                         {
                             filename = base.filename,
                             width = base.width,
                             height = base.height,
                             tint = base.tint,
                             shift = base.shift,
                             x = (x-1) * base.width,
                             y = (y-1) * base.height,
            })
        end
    end

    return variations
end


function imageUtils.create_small_tree_flame_animations(opts)
    local fire_blend_mode = opts.blend_mode or "additive"
    local fire_animation_speed = opts.animation_speed or 0.5
    local fire_scale =  opts.scale or 1
    local fire_tint = opts.tint2
    local fire_flags = { "compressed" }
    local retval = {
        {
            filename = "__base__/graphics/entity/fire-flame/tree-fire-flame-01-a.png",
            line_length = 8,
            width = 38,
            height = 110,
            frame_count = 32,
            axially_symmetrical = false,
            direction_count = 1,
            shift = {-0.03125, -1.5},
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags
        },
        {
            filename = "__base__/graphics/entity/fire-flame/tree-fire-flame-01-b.png",
            line_length = 8,
            width = 39,
            height = 111,
            frame_count = 32,
            axially_symmetrical = false,
            direction_count = 1,
            shift = {-0.078125, -1.51562},
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags
        },
        {
            filename = "__base__/graphics/entity/fire-flame/tree-fire-flame-01-c.png",
            line_length = 8,
            width = 44,
            height = 108,
            frame_count = 32,
            axially_symmetrical = false,
            direction_count = 1,
            shift = {-0.15625, -1.5},
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags
        },
        {
            filename = "__base__/graphics/entity/fire-flame/tree-fire-flame-03-a.png",
            line_length = 8,
            width = 38,
            height = 110,
            frame_count = 23,
            axially_symmetrical = false,
            direction_count = 1,
            shift = {-0.03125, -1.5},
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags
        },
        {
            filename = "__base__/graphics/entity/fire-flame/tree-fire-flame-03-b.png",
            line_length = 8,
            width = 34,
            height = 98,
            frame_count = 23,
            axially_symmetrical = false,
            direction_count = 1,
            shift = {-0.03125, -1.34375},
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags
        },
        {
            filename = "__base__/graphics/entity/fire-flame/tree-fire-flame-03-c.png",
            line_length = 8,
            width = 39,
            height = 111,
            frame_count = 23,
            axially_symmetrical = false,
            direction_count = 1,
            shift = {-0.078125, -1.51562},
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags
        }
    }

    return foreach(retval, function(tab)
                       if tab.shift and tab.scale then tab.shift = { tab.shift[1] * tab.scale, tab.shift[2] * tab.scale } end
    end)
end

function imageUtils.create_fire_pictures(opts)
    local fire_blend_mode = opts.blend_mode or "additive"
    local fire_animation_speed = opts.animation_speed or 0.5
    local fire_scale =  opts.scale or 1
    local fire_tint = opts.tint2
    local fire_flags = { "compressed" }
    local retval = {
        {
            filename = "__base__/graphics/entity/fire-flame/fire-flame-13.png",
            line_length = 8,
            width = 60,
            height = 118,
            frame_count = 25,
            axially_symmetrical = false,
            direction_count = 1,
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags,
            shift = { -0.0390625, -0.90625 }
        },
        {
            filename = "__base__/graphics/entity/fire-flame/fire-flame-12.png",
            line_length = 8,
            width = 63,
            height = 116,
            frame_count = 25,
            axially_symmetrical = false,
            direction_count = 1,
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags,
            shift = { -0.015625, -0.914065 }
        },
        {
            filename = "__base__/graphics/entity/fire-flame/fire-flame-11.png",
            line_length = 8,
            width = 61,
            height = 122,
            frame_count = 25,
            axially_symmetrical = false,
            direction_count = 1,
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags,
            shift = { -0.0078125, -0.90625 }
        },
        {
            filename = "__base__/graphics/entity/fire-flame/fire-flame-10.png",
            line_length = 8,
            width = 65,
            height = 108,
            frame_count = 25,
            axially_symmetrical = false,
            direction_count = 1,
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags,
            shift = { -0.0625, -0.64844 }
        },
        {
            filename = "__base__/graphics/entity/fire-flame/fire-flame-09.png",
            line_length = 8,
            width = 64,
            height = 101,
            frame_count = 25,
            axially_symmetrical = false,
            direction_count = 1,
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags,
            shift = { -0.03125, -0.695315 }
        },
        {
            filename = "__base__/graphics/entity/fire-flame/fire-flame-08.png",
            line_length = 8,
            width = 50,
            height = 98,
            frame_count = 32,
            axially_symmetrical = false,
            direction_count = 1,
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags,
            shift = { -0.0546875, -0.77344 }
        },
        {
            filename = "__base__/graphics/entity/fire-flame/fire-flame-07.png",
            line_length = 8,
            width = 54,
            height = 84,
            frame_count = 32,
            axially_symmetrical = false,
            direction_count = 1,
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags,
            shift = { 0.015625, -0.640625 }
        },
        {
            filename = "__base__/graphics/entity/fire-flame/fire-flame-06.png",
            line_length = 8,
            width = 65,
            height = 92,
            frame_count = 32,
            axially_symmetrical = false,
            direction_count = 1,
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags,
            shift = { 0, -0.83594 }
        },
        {
            filename = "__base__/graphics/entity/fire-flame/fire-flame-05.png",
            line_length = 8,
            width = 59,
            height = 103,
            frame_count = 32,
            axially_symmetrical = false,
            direction_count = 1,
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags,
            shift = { 0.03125, -0.882815 }
        },
        {
            filename = "__base__/graphics/entity/fire-flame/fire-flame-04.png",
            line_length = 8,
            width = 67,
            height = 130,
            frame_count = 32,
            axially_symmetrical = false,
            direction_count = 1,
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags,
            shift = { 0.015625, -1.109375 }
        },
        {
            filename = "__base__/graphics/entity/fire-flame/fire-flame-03.png",
            line_length = 8,
            width = 74,
            height = 117,
            frame_count = 32,
            axially_symmetrical = false,
            direction_count = 1,
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags,
            shift = { 0.046875, -0.984375 }
        },
        {
            filename = "__base__/graphics/entity/fire-flame/fire-flame-02.png",
            line_length = 8,
            width = 74,
            height = 114,
            frame_count = 32,
            axially_symmetrical = false,
            direction_count = 1,
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags,
            shift = { 0.0078125, -0.96875 }
        },
        {
            filename = "__base__/graphics/entity/fire-flame/fire-flame-01.png",
            line_length = 8,
            width = 66,
            height = 119,
            frame_count = 32,
            axially_symmetrical = false,
            direction_count = 1,
            blend_mode = fire_blend_mode,
            animation_speed = fire_animation_speed,
            scale = fire_scale,
            tint = fire_tint,
            flags = fire_flags,
            shift = { -0.0703125, -1.039065 }
        },
    }
    return foreach(retval, function(tab)
                       if tab.shift and tab.scale then tab.shift = { tab.shift[1] * tab.scale, tab.shift[2] * tab.scale } end
    end)
end

return imageUtils
