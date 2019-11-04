-- imports

local physicalBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local smokeUtils = require("utils/SmokeUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")
local particleUtils = require("utils/ParticleUtils")

-- constants

local poison = {}

-- imported functions

local makeBloodFountains = particleUtils.makeBloodFountains
local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createAttackBall = physicalBall.createAttackBall
local createRangedAttack = biterUtils.createRangedAttack
local createMeleeAttack = biterUtils.createMeleeAttack

local makeCloud = smokeUtils.makeCloud

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

local function evolutionFunction(tier)
    if (tier == 0) then
        return 0
    else
        return 0.15 + ((tier - 2) * 0.10)
    end
end

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

    local bloodFountains = {
        type = "attribute",
        mapping = "explosion",
        [1] = "poison-blood-explosion-small-rampant",
        [2] = "poison-blood-explosion-small-rampant",
        [3] = "poison-blood-explosion-small-rampant",
        [4] = "poison-blood-explosion-small-rampant",
        [5] = "poison-blood-explosion-big-rampant",
        [6] = "poison-blood-explosion-big-rampant",
        [7] = "poison-blood-explosion-big-rampant",
        [8] = "poison-blood-explosion-huge-rampant",
        [9] = "poison-blood-explosion-huge-rampant",
        [10] = "poison-blood-explosion-huge-rampant",
    }

    makeBloodFountains({
            name = "poison",
            tint = {r=0, g=0.7, b=0, a=1}
    })


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
                                                            damage = { amount = 1.2 * i, type = "poison"}
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
                },
                attack = {
                    damageType = "poison"
                },
                resistances = {},

                type = "biter",
                tint = {r=0.4, g=0.6, b=0.5, a=1},
                tint2 = {r=0, g=0.7, b=0, a=1}
            },

            unitSpawner = {
                name = "poison-biter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},
                tint = {r=0.4, g=0.6, b=0.5, a=1},
                tint2 = {r=0, g=0.7, b=0, a=1}
            }
        },

        {
            unit = {

                bloodFountains,

                {
                    type = "majorResistances",
                    entries = {"poison"}
                },

                {
                    type = "minorResistances",
                    entries = {"fire"}
                },

                {
                    type = "minorWeaknesses",
                    entries = {"electric", "explosion", "laser"}
                }
            },

            unitSpawner = {

                bloodFountains,

                {
                    type = "attribute",
                    name = "evolutionRequirement",
                    formula = evolutionFunction
                },

                {
                    type = "majorResistances",
                    entries = {"poison"}
                },

                {
                    type = "minorResistances",
                    entries = {"fire"}
                },

                {
                    type = "minorWeaknesses",
                    entries = {"electric", "explosion", "laser"}
                }

            }
        },

        createMeleeAttack
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
            tint = {r=0.4, g=0.6, b=0.5, a=1},
            tint2 = {r=0, g=0.7, b=0, a=1}
        },

        {
            bloodFountains,

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
                formula = evolutionFunction
            },

            {
                type = "majorResistances",
                entries = {"poison"}
            },

            {
                type = "minorResistances",
                entries = {"fire"}
            },

            {
                type = "minorWeaknesses",
                entries = {"electric", "explosion", "laser"}
            }

        },

        function (attack, attributes, tier)
            return createRangedAttack(attack,
                                      createAttackBall(attack))
        end
    )
end

return poison
