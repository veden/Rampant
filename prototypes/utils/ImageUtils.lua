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
    -- TODO: Factorio 2.0 renamed tree-fire-flame-* to tree-flame-*. Stub out for now.
    return nil
end

function imageUtils.create_fire_pictures(opts)
    -- TODO: Factorio 2.0 removed fire-flame-05 through fire-flame-13 sprites.
    -- Return nil to use Factorio's default fire rendering. Custom tinted flames
    -- can be restored when compatible sprite sheets are available.
    return nil
end

return imageUtils
