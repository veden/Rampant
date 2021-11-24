local bombUtils = {}

function bombUtils.makeAtomicBlast(attributes)
    local name = attributes.name .. "-atomic-blast-rampant"
    data:extend({{
                type = "projectile",
                name = name,
                flags = {"not-on-map"},
                acceleration = 0,
                action =
                    {
                        {
                            type = "direct",
                            action_delivery =
                                {
                                    type = "instant",
                                    target_effects =
                                        {
                                            {
                                                type = "create-entity",
                                                entity_name = "explosion"
                                            }
                                        }
                                }
                        },
                        {
                            type = "area",
                            radius = 3,
                            action_delivery =
                                {
                                    type = "instant",
                                    target_effects =
                                        {
                                            type = "damage",
                                            damage = {amount = (attributes.damage * 2) or 400,
                                                      type = attributes.damageType or "explosion"}
                                        }
                                }
                        }
                    },
                animation =
                    {
                        filename = "__core__/graphics/empty.png",
                        frame_count = 1,
                        width = 1,
                        height = 1,
                        priority = "high"
                    },
                shadow =
                    {
                        filename = "__core__/graphics/empty.png",
                        frame_count = 1,
                        width = 1,
                        height = 1,
                        priority = "high"
                    }
    }})
    return name
end

return bombUtils
