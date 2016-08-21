data:extend({
{
    type = "simple-entity",
    name = "tunnel-entrance",
    flags = {"placeable-neutral", "placeable-off-grid", "not-on-map"},
    icon = "__base__/graphics/icons/small-scorchmark.png",
    subgroup = "grass",
    order = "b[decorative]-k[tunnel-entrance]-a[big]",
    collision_box = {{-1.3, -1.3}, {1.3, 1.3}},
    selection_box = {{-1.5, -1.5}, {1.5, 1.5}},
    -- minable =
    -- {
      -- mining_particle = "stone-particle",
      -- mining_time = 8,
      -- result = "stone",
      -- count = 20
    -- },
    -- loot =
    -- {
      -- {item = "stone", probability = 1, count_min = 5, count_max = 10}
    -- },
    -- mined_sound = { filename = "__base__/sound/deconstruct-bricks.ogg" },
    render_layer = "remnants",
    max_health = 0,
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