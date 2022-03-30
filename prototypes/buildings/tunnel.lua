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
    type = "simple-entity",
    name = "tunnel-entrance-rampant",
    flags = {"placeable-neutral", "placeable-off-grid", "not-on-map"},
    icon = "__base__/graphics/icons/small-scorchmark.png",
    icon_size = 32,
    subgroup = "grass",
    order = "b[decorative]-k[tunnel-entrance]-a[big]",
    collision_box = {{-1.3, -1.3}, {1.3, 1.3}},
    selection_box = {{-1.5, -1.5}, {1.5, 1.5}},
    render_layer = "remnants",
    destructible = "false",
    max_health = 1,
    pictures =
    {
      {
        filename = "__Rampant__/graphics/entities/tunnel/tunnelEntrance.png",
        width = 142,
        height = 104,
        shift = {0, 0}
      }
    }
  }
})
