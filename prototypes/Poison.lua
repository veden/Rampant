-- imports

local physicalBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local smokeUtils = require("utils/SmokeUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")

-- constants

local poison = {}

local POISON_UNIT_TIERS = constants.POISON_UNIT_TIERS
local POISON_UNIT_VARIATIONS = constants.POISON_UNIT_VARIATIONS

local POISON_NEST_TIERS = constants.POISON_NEST_TIERS
local POISON_NEST_VARIATIONS = constants.POISON_NEST_VARIATIONS

local POISON_WORM_TIERS = constants.POISON_WORM_TIERS
local POISON_WORM_VARIATIONS = constants.POISON_WORM_VARIATIONS

-- imported functions

local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createAttackBall = physicalBall.createAttackBall
local createRangedAttack = biterUtils.createRangedAttack
local createMeleeAttack = biterUtils.createMeleeAttack

local makeCloud = smokeUtils.makeCloud

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

function poison.addFaction()

    local biterLoot = makeUnitAlienLootTable("green")
    local spawnerLoot = makeSpawnerAlienLootTable("green")
    local wormLoot = makeWormAlienLootTable("green")

    data:extend({
            {
                type = "damage-type",
                name = "healing"
            }
    })

    for i=1,10 do
        makeCloud(
            {
                name = "poison-cloud-v" .. i,
                scale = 0.80 + (i * 0.15),
                wind = true,
                slowdown = -1.3,
                duration = 60 * (i * 5)
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
                                                            damage = { amount = -8 * i, type = "healing"}
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
                                                            damage = { amount = 14 * i, type = "poison"}
                                                        }
                                                }
                                        }
                                    }
                            }
                    }
            }
        )
    end

    -- poison biters
    buildUnitSpawner(
        {
            unit = {
                name = "poison-biter",

                loot = biterLoot,
                attributes = {
                    explosion = "blood-explosion-small"
                },
                attack = {
                    damageType = "poison"
                },
                resistances = {},

                type = "biter",
                tint = {r=0.1, g=0.8, b=0.1, a=1}
            },

            unitSpawner = {
                name = "poison-biter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},
                tint = {r=0.1, g=0.8, b=0.1, a=1}
            }
        },

        {
            unit = {

                {
                    type = "resistance",
                    name = "fire",
                    decrease = {
                        [1] = 1,
                        [2] = 1,
                        [3] = 1,
                        [4] = 1,
                        [5] = 2,
                        [6] = 2,
                        [7] = 3,
                        [8] = 3,
                        [9] = 3,
                        [10] = 4
                    },
                    percent = {
                        [1] = 30,
                        [2] = 30,
                        [3] = 30,
                        [4] = 40,
                        [5] = 40,
                        [6] = 40,
                        [7] = 45,
                        [8] = 45,
                        [9] = 45,
                        [10] = 50
                    }
                },

                {
                    type = "resistance",
                    name = "poison",
                    decrease = {
                        [1] = 5,
                        [2] = 5,
                        [3] = 7,
                        [4] = 10,
                        [5] = 13,
                        [6] = 13,
                        [7] = 16,
                        [8] = 16,
                        [9] = 19,
                        [10] = 23
                    },
                    percent = {
                        [1] = 65,
                        [2] = 65,
                        [3] = 70,
                        [4] = 75,
                        [5] = 75,
                        [6] = 80,
                        [7] = 85,
                        [8] = 85,
                        [9] = 90,
                        [10] = 90
                    }
                },

                {
                    type = "resistance",
                    name = "electric",
                    decrease = {
                        [1] = -3,
                        [2] = -3,
                        [3] = -7,
                        [4] = -7,
                        [5] = -10,
                        [6] = -10,
                        [7] = -13,
                        [8] = -13,
                        [9] = -16,
                        [10] = -18
                    },
                    percent = {
                        [1] = -35,
                        [2] = -35,
                        [3] = -40,
                        [4] = -40,
                        [5] = -45,
                        [6] = -45,
                        [7] = -50,
                        [8] = -55,
                        [9] = -55,
                        [10] = -60
                    }
                },

                {
                    type = "resistance",
                    name = "explosion",
                    decrease = {
                        [1] = -3,
                        [2] = -3,
                        [3] = -7,
                        [4] = -7,
                        [5] = -10,
                        [6] = -10,
                        [7] = -13,
                        [8] = -13,
                        [9] = -16,
                        [10] = -18
                    },
                    percent = {
                        [1] = -35,
                        [2] = -35,
                        [3] = -40,
                        [4] = -40,
                        [5] = -45,
                        [6] = -45,
                        [7] = -50,
                        [8] = -55,
                        [9] = -55,
                        [10] = -60
                    }
                },

                {
                    type = "resistance",
                    name = "laser",
                    decrease = {
                        [1] = -3,
                        [2] = -3,
                        [3] = -7,
                        [4] = -7,
                        [5] = -10,
                        [6] = -10,
                        [7] = -13,
                        [8] = -13,
                        [9] = -16,
                        [10] = -18
                    },
                    percent = {
                        [1] = -35,
                        [2] = -35,
                        [3] = -40,
                        [4] = -40,
                        [5] = -45,
                        [6] = -45,
                        [7] = -50,
                        [8] = -55,
                        [9] = -55,
                        [10] = -60
                    }
                }

            },

            unitSpawner = {

                {
                    type = "attribute",
                    name = "evolutionRequirement",
                    [1] = 0,
                    [2] = 0.15,
                    [3] = 0.25,
                    [4] = 0.35,
                    [5] = 0.45,
                    [6] = 0.55,
                    [7] = 0.65,
                    [8] = 0.70,
                    [9] = 0.75,
                    [10] = 0.95
                },
                
                {
                    type = "resistance",
                    name = "fire",
                    decrease = {
                        [1] = 1,
                        [2] = 1,
                        [3] = 1,
                        [4] = 1,
                        [5] = 2,
                        [6] = 2,
                        [7] = 3,
                        [8] = 3,
                        [9] = 3,
                        [10] = 4
                    },
                    percent = {
                        [1] = 30,
                        [2] = 30,
                        [3] = 30,
                        [4] = 40,
                        [5] = 40,
                        [6] = 40,
                        [7] = 45,
                        [8] = 45,
                        [9] = 45,
                        [10] = 50
                    }
                },

                {
                    type = "resistance",
                    name = "poison",
                    decrease = {
                        [1] = 5,
                        [2] = 5,
                        [3] = 7,
                        [4] = 10,
                        [5] = 13,
                        [6] = 13,
                        [7] = 16,
                        [8] = 16,
                        [9] = 19,
                        [10] = 23
                    },
                    percent = {
                        [1] = 65,
                        [2] = 65,
                        [3] = 70,
                        [4] = 75,
                        [5] = 75,
                        [6] = 80,
                        [7] = 85,
                        [8] = 85,
                        [9] = 90,
                        [10] = 90
                    }
                },

                {
                    type = "resistance",
                    name = "electric",
                    decrease = {
                        [1] = -3,
                        [2] = -3,
                        [3] = -7,
                        [4] = -7,
                        [5] = -10,
                        [6] = -10,
                        [7] = -13,
                        [8] = -13,
                        [9] = -16,
                        [10] = -18
                    },
                    percent = {
                        [1] = -35,
                        [2] = -35,
                        [3] = -40,
                        [4] = -40,
                        [5] = -45,
                        [6] = -45,
                        [7] = -50,
                        [8] = -55,
                        [9] = -55,
                        [10] = -60
                    }
                },

                {
                    type = "resistance",
                    name = "explosion",
                    decrease = {
                        [1] = -3,
                        [2] = -3,
                        [3] = -7,
                        [4] = -7,
                        [5] = -10,
                        [6] = -10,
                        [7] = -13,
                        [8] = -13,
                        [9] = -16,
                        [10] = -18
                    },
                    percent = {
                        [1] = -35,
                        [2] = -35,
                        [3] = -40,
                        [4] = -40,
                        [5] = -45,
                        [6] = -45,
                        [7] = -50,
                        [8] = -55,
                        [9] = -55,
                        [10] = -60
                    }
                },

                {
                    type = "resistance",
                    name = "laser",
                    decrease = {
                        [1] = -3,
                        [2] = -3,
                        [3] = -7,
                        [4] = -7,
                        [5] = -10,
                        [6] = -10,
                        [7] = -13,
                        [8] = -13,
                        [9] = -16,
                        [10] = -18
                    },
                    percent = {
                        [1] = -35,
                        [2] = -35,
                        [3] = -40,
                        [4] = -40,
                        [5] = -45,
                        [6] = -45,
                        [7] = -50,
                        [8] = -55,
                        [9] = -55,
                        [10] = -60
                    }
                }

            }
        },

        createMeleeAttack,

        {
            unit = POISON_UNIT_VARIATIONS,
            unitSpawner = POISON_NEST_VARIATIONS
        },

        {
            unit = POISON_UNIT_TIERS,
            unitSpawner = POISON_NEST_TIERS
        }
    )

    -- poison worms
    buildWorm(
        {
            name = "poison-worm",

            loot = wormLoot,
            attributes = {
            },
            attack = {
                type = "projectile",
                damageType = "poison",
                pointEffects = function(attributes)
		    return
			{
                            {
                                type="create-entity",
                                entity_name = attributes.cloud
                            }
                        }
                end
            },
            resistances = {},

            attackName = "poison-worm",
            tint = {r=0.15, g=0.85, b=0.15, a=0.65}
        },

        {
            {
                type = "attack",
                mapping = "cloud",
                [1] = "poison-cloud-v1-cloud-rampant",
                [2] = "poison-cloud-v2-cloud-rampant",
                [3] = "poison-cloud-v3-cloud-rampant",
                [4] = "poison-cloud-v4-cloud-rampant",
                [5] = "poison-cloud-v5-cloud-rampant",
                [6] = "poison-cloud-v6-cloud-rampant",
                [7] = "poison-cloud-v7-cloud-rampant",
                [8] = "poison-cloud-v8-cloud-rampant",
                [9] = "poison-cloud-v9-cloud-rampant",
                [10] = "poison-cloud-v10-cloud-rampant"
            },

            {
                type = "attribute",
                name = "evolutionRequirement",
                [1] = 0,
                [2] = 0.15,
                [3] = 0.25,
                [4] = 0.35,
                [5] = 0.45,
                [6] = 0.55,
                [7] = 0.65,
                [8] = 0.70,
                [9] = 0.75,
                [10] = 0.95
            },
            
            {
                type = "resistance",
                name = "fire",
                decrease = {
                    [1] = 1,
                    [2] = 1,
                    [3] = 1,
                    [4] = 1,
                    [5] = 2,
                    [6] = 2,
                    [7] = 3,
                    [8] = 3,
                    [9] = 3,
                    [10] = 4
                },
                percent = {
                    [1] = 30,
                    [2] = 30,
                    [3] = 30,
                    [4] = 40,
                    [5] = 40,
                    [6] = 40,
                    [7] = 45,
                    [8] = 45,
                    [9] = 45,
                    [10] = 50
                }
            },

            {
                type = "resistance",
                name = "poison",
                decrease = {
                    [1] = 5,
                    [2] = 5,
                    [3] = 7,
                    [4] = 10,
                    [5] = 13,
                    [6] = 13,
                    [7] = 16,
                    [8] = 16,
                    [9] = 19,
                    [10] = 23
                },
                percent = {
                    [1] = 65,
                    [2] = 65,
                    [3] = 70,
                    [4] = 75,
                    [5] = 75,
                    [6] = 80,
                    [7] = 85,
                    [8] = 85,
                    [9] = 90,
                    [10] = 90
                }
            },

            {
                type = "resistance",
                name = "electric",
                decrease = {
                    [1] = -3,
                    [2] = -3,
                    [3] = -7,
                    [4] = -7,
                    [5] = -10,
                    [6] = -10,
                    [7] = -13,
                    [8] = -13,
                    [9] = -16,
                    [10] = -18
                },
                percent = {
                    [1] = -35,
                    [2] = -35,
                    [3] = -40,
                    [4] = -40,
                    [5] = -45,
                    [6] = -45,
                    [7] = -50,
                    [8] = -55,
                    [9] = -55,
                    [10] = -60
                }
            },

            {
                type = "resistance",
                name = "explosion",
                decrease = {
                    [1] = -3,
                    [2] = -3,
                    [3] = -7,
                    [4] = -7,
                    [5] = -10,
                    [6] = -10,
                    [7] = -13,
                    [8] = -13,
                    [9] = -16,
                    [10] = -18
                },
                percent = {
                    [1] = -35,
                    [2] = -35,
                    [3] = -40,
                    [4] = -40,
                    [5] = -45,
                    [6] = -45,
                    [7] = -50,
                    [8] = -55,
                    [9] = -55,
                    [10] = -60
                }
            },

            {
                type = "resistance",
                name = "laser",
                decrease = {
                    [1] = -3,
                    [2] = -3,
                    [3] = -7,
                    [4] = -7,
                    [5] = -10,
                    [6] = -10,
                    [7] = -13,
                    [8] = -13,
                    [9] = -16,
                    [10] = -18
                },
                percent = {
                    [1] = -35,
                    [2] = -35,
                    [3] = -40,
                    [4] = -40,
                    [5] = -45,
                    [6] = -45,
                    [7] = -50,
                    [8] = -55,
                    [9] = -55,
                    [10] = -60
                }
            }

        },

        function (attack, attributes, tier)
            return createRangedAttack(attack,
                                      createAttackBall(attack))
        end,

        POISON_WORM_VARIATIONS,
        POISON_WORM_TIERS
    )
end

return poison
