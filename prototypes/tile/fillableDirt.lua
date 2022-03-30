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


data:extend({
    {
        type = "tile",
        name = "fillableDirt",
        needs_correction = false,
        collision_mask =
        {
          "water-tile",
        },
        layer = 40,
        variants =
        {
          main =
          {
            {
              picture = "__Rampant__/graphics/tiles/fillableDirt/dirt1.png",
              count = 8,
              size = 1
            }
            ,
            {
              picture = "__Rampant__/graphics/tiles/fillableDirt/dirt2.png",
              count = 8,
              size = 2
            },
            {
              picture = "__Rampant__/graphics/tiles/fillableDirt/dirt4.png",
              count = 6,
              size = 4
            }
          },
          inner_corner =
          {
            picture = "__Rampant__/graphics/tiles/fillableDirt/dirt-inner-corner.png",
            count = 6
          },
          outer_corner =
          {
            picture = "__Rampant__/graphics/tiles/fillableDirt/dirt-outer-corner.png",
            count = 6
          },
          side =
          {
            picture = "__Rampant__/graphics/tiles/fillableDirt/dirt-side.png",
            count = 8
          }
        },
        map_color={r=0.4196, g=0.3294, b=0.2196},
        ageing=0
    }
})
