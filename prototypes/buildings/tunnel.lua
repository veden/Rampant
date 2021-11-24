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
