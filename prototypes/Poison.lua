-- imports

local smokeUtils = require("utils/SmokeUtils")

-- constants

local poison = {}

-- imported functions

local makeCloud = smokeUtils.makeCloud

function poison.addFactionAddon()

    for i=1,10 do
        makeCloud(
            {
                name = "poison-cloud-v" .. i,
                scale = 0.80 + (i * 0.15),
                wind = true,
                slowdown = -1.3,
                duration = 10 * (i * 5),
                cooldown = 5
            },
            {
                type = "direct",
                action_delivery =
                    {
                        type = "instant",
                        target_effects =
                            {
                                type = "nested-result",
                                action =
                                    {
                                        {
                                            type = "area",
                                            radius = 2 + (i * 0.5),
                                            force = "ally",
                                            entity_flags = {"placeable-enemy"},
                                            action_delivery =
                                                {
                                                    type = "instant",
                                                    target_effects =
                                                        {
                                                            type = "damage",
                                                            damage = { amount = -2 * i, type = "healing"}
                                                        }
                                                }
                                        },
                                        {
                                            type = "area",
                                            radius = 2 + (i * 0.5),
                                            force = "enemy",
                                            action_delivery =
                                                {
                                                    type = "instant",
                                                    target_effects =
                                                        {
                                                            type = "damage",
                                                            damage = { amount = 0.9 * i, type = "poison"}
                                                        }
                                                }
                                        }
                                    }
                            }
                    }
            }
        )
    end

end

return poison
