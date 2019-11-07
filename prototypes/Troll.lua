-- imports

local acidBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")
local particleUtils = require("utils/ParticleUtils")

-- constants

local troll = {}

-- imported functions

local makeBloodFountains = particleUtils.makeBloodFountains
local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createAttackBall = acidBall.createAttackBall
local createRangedAttack = biterUtils.createRangedAttack
local createMeleeAttack = biterUtils.createMeleeAttack

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

local function evolutionFunction(tier)
    if (tier == 0) then
        return 0
    else
        return 0.17 + ((tier - 2) * 0.10)
    end
end

function troll.addFaction()

    local biterLoot = makeUnitAlienLootTable("green")
    local spawnerLoot = makeSpawnerAlienLootTable("green")
    local wormLoot = makeWormAlienLootTable("green")

    local bloodFountains = {
        type = "attribute",
        mapping = "explosion",
        [1] = "troll-blood-explosion-small-rampant",
        [2] = "troll-blood-explosion-small-rampant",
        [3] = "troll-blood-explosion-small-rampant",
        [4] = "troll-blood-explosion-small-rampant",
        [5] = "troll-blood-explosion-big-rampant",
        [6] = "troll-blood-explosion-big-rampant",
        [7] = "troll-blood-explosion-big-rampant",
        [8] = "troll-blood-explosion-huge-rampant",
        [9] = "troll-blood-explosion-huge-rampant",
        [10] = "troll-blood-explosion-huge-rampant",
    }

    makeBloodFountains({
            name = "troll",
            tint = {r=0.8, g=0.8, b=0.8, a=1}
    })


    -- troll biters
    buildUnitSpawner(
        {
            unit = {
                name = "troll-biter",

                loot = biterLoot,
                attributes = {
                },
                attack = {},
                resistances = {},

                type = "biter",
                scales = {
                    [1] = 0.7,
                    [2] = 0.8,
                    [3] = 0.9,
                    [4] = 1,
                    [5] = 1.1,
                    [6] = 1.3,
                    [7] = 1.5,
                    [8] = 1.7,
                    [9] = 1.9,
                    [10] = 2.1
                },
                tint = {r=0.4, g=0.4, b=0.4, a=1},
                tint2 = {r=0.8, g=0.8, b=0.8, a=1}
            },

            unitSpawner = {
                name = "troll-biter-spawner",
                
                loot = spawnerLoot,
                attributes = {},
                resistances = {},
                scales = {
                    [1] = 0.7,
                    [2] = 0.8,
                    [3] = 0.9,
                    [4] = 1,
                    [5] = 1.1,
                    [6] = 1.2,
                    [7] = 1.4,
                    [8] = 1.6,
                    [9] = 1.8,
                    [10] = 2.0
                },
                tint = {r=0.4, g=0.4, b=0.4, a=1},
                tint2 = {r=0.8, g=0.8, b=0.8, a=1}
            }
        },

        {
            unit = {
                bloodFountains,

                {
                    type = "attribute",
                    name = "health",
                    [1] = 30,
                    [2] = 150,
                    [3] = 300,
                    [4] = 500,
                    [5] = 1500,
                    [6] = 3000,
                    [7] = 5000,
                    [8] = 12000,
                    [9] = 20000,
                    [10] = 40000
                },

                {
                    type = "attribute",
                    name = "healing",
                    [1] = 0.3,
                    [2] = 0.3,
                    [3] = 0.35,
                    [4] = 0.4,
                    [5] = 0.8,
                    [6] = 1.2,
                    [7] = 1.8,
                    [8] = 2.5,
                    [9] = 2.5,
                    [10] = 3
                },

                {
                    type = "minorResistances",
                    entries = {"explosion", "physical"}
                },

                {
                    type = "majorWeaknesses",
                    entries = {"fire"}
                },

                {
                    type = "attribute",
                    name = "movement",
                    [1] = 0.16,
                    [2] = 0.16,
                    [3] = 0.155,
                    [4] = 0.15,
                    [5] = 0.155,
                    [6] = 0.15,
                    [7] = 0.15,
                    [8] = 0.15,
                    [9] = 0.15,
                    [10] = 0.15
                }

            },

            unitSpawner = {

                bloodFountains,

                {
                    type = "attribute",
                    name = "health",
                    [1] = 700,
                    [2] = 1000,
                    [3] = 1500,
                    [4] = 3000,
                    [5] = 7000,
                    [6] = 15000,
                    [7] = 22000,
                    [8] = 40000,
                    [9] = 60000,
                    [10] = 70000
                },

                {
                    type = "attribute",
                    name = "healing",
                    [1] = 0.6,
                    [2] = 0.6,
                    [3] = 0.65,
                    [4] = 0.8,
                    [5] = 1.6,
                    [6] = 3.2,
                    [7] = 4.4,
                    [8] = 5.2,
                    [9] = 6,
                    [10] = 7
                },

                {
                    type = "attribute",
                    name = "evolutionRequirement",
                    formula = evolutionFunction
                },

                {
                    type = "minorResistances",
                    entries = {"explosion", "physical"}
                },

                {
                    type = "majorWeaknesses",
                    entries = {"fire"}
                },
            }
        },

        createMeleeAttack
    )

    -- troll spitters
    buildUnitSpawner(
        {
            unit = {
                name = "troll-spitter",

                loot = biterLoot,
                attributes = {
                },
                attack = {
                    type = "projectile",
                    directionOnly = true
                },
                resistances = {},

                type = "spitter",
                scales = {
                    [1] = 0.7,
                    [2] = 0.8,
                    [3] = 0.9,
                    [4] = 1,
                    [5] = 1.1,
                    [6] = 1.2,
                    [7] = 1.3,
                    [8] = 1.4,
                    [9] = 1.5,
                    [10] = 1.6
                },
                attackName = "troll-spitter",
                tint = {r=0.4, g=0.4, b=0.4, a=1},
                tint2 = {r=0.8, g=0.8, b=0.8, a=1}
            },

            unitSpawner = {
                name = "troll-spitter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},

                scales = {
                    [1] = 0.7,
                    [2] = 0.8,
                    [3] = 0.9,
                    [4] = 1,
                    [5] = 1.1,
                    [6] = 1.2,
                    [7] = 1.4,
                    [8] = 1.6,
                    [9] = 1.8,
                    [10] = 2.0
                },
                tint = {r=0.4, g=0.4, b=0.4, a=1},
                tint2 = {r=0.8, g=0.8, b=0.8, a=1}
            }
        },

        {
            unit = {
                bloodFountains,

                {
                    type = "attribute",
                    name = "health",
                    [1] = 700,
                    [2] = 1000,
                    [3] = 1500,
                    [4] = 3000,
                    [5] = 7000,
                    [6] = 15000,
                    [7] = 22000,
                    [8] = 40000,
                    [9] = 60000,
                    [10] = 70000
                },

                {
                    type = "minorResistances",
                    entries = {"explosion", "physical"}
                },

                {
                    type = "majorWeaknesses",
                    entries = {"fire"}
                },

                {
                    type = "attribute",
                    name = "healing",
                    [1] = 0.3,
                    [2] = 0.3,
                    [3] = 0.35,
                    [4] = 0.4,
                    [5] = 0.8,
                    [6] = 1.6,
                    [7] = 2.2,
                    [8] = 3,
                    [9] = 3.5,
                    [10] = 4.5
                },

                {
                    type = "attribute",
                    name = "movement",
                    [1] = 0.165,
                    [2] = 0.16,
                    [3] = 0.16,
                    [4] = 0.15,
                    [5] = 0.15,
                    [6] = 0.14,
                    [7] = 0.14,
                    [8] = 0.13,
                    [9] = 0.13,
                    [10] = 0.12
                }

            },

            unitSpawner = {
                bloodFountains,

                {
                    type = "attribute",
                    name = "health",
                    [1] = 700,
                    [2] = 1000,
                    [3] = 1500,
                    [4] = 3000,
                    [5] = 5000,
                    [6] = 7000,
                    [7] = 10000,
                    [8] = 14000,
                    [9] = 20000,
                    [10] = 30000
                },

                {
                    type = "attribute",
                    name = "healing",
                    [1] = 0.6,
                    [2] = 0.6,
                    [3] = 0.65,
                    [4] = 0.8,
                    [5] = 1.6,
                    [6] = 3.2,
                    [7] = 4.4,
                    [8] = 5.2,
                    [9] = 6,
                    [10] = 7
                },

                {
                    type = "minorResistances",
                    entries = {"explosion", "physical"}
                },

                {
                    type = "majorWeaknesses",
                    entries = {"fire"}
                }
            }
        },

        function (attributes)
            return createRangedAttack(attributes,
                                      createAttackBall(attributes),
                                      spitterattackanimation(attributes.scale,
                                                             attributes.tint,
                                                             attributes.tint2))
        end
    )

    -- troll worms
    buildWorm(
        {
            name = "troll-worm",

            loot = wormLoot,
            attributes = {},
            attack = {
                type = "projectile",
                damageType = "physical",
                pointEffects = function (attributes)
                    return {
                        {
                            type= "create-entity",
                            entity_name = "small-scorchmark"
                        },
                        {
                            type= "create-entity",
                            entity_name = attributes.explosion
                        }
                    }
                end
            },
            resistances = {},

            scales = {
                [1] = 0.7,
                [2] = 0.8,
                [3] = 0.9,
                [4] = 1,
                [5] = 1.1,
                [6] = 1.2,
                [7] = 1.4,
                [8] = 1.6,
                [9] = 1.8,
                [10] = 2.0
            },
            attackName = "troll-worm",
            tint = {r=0.4, g=0.4, b=0.4, a=1},
            tint2 = {r=0.2, g=0.2, b=0.2, a=1}
        },

        {
            bloodFountains,

            {
                type = "attack",
                mapping = "explosion",
                [1] = "explosion",
                [2] = "explosion",
                [3] = "big-explosion",
                [4] = "big-explosion",
                [5] = "big-explosion",
                [6] = "big-explosion",
                [7] = "massive-explosion",
                [8] = "massive-explosion",
                [9] = "massive-explosion",
                [10] = "massive-explosion"
            },

            {
                type = "attribute",
                name = "health",
                [1] = 400,
                [2] = 700,
                [3] = 1000,
                [4] = 1500,
                [5] = 4000,
                [6] = 7000,
                [7] = 15000,
                [8] = 24000,
                [9] = 40000,
                [10] = 50000
            },

            {
                type = "attribute",
                name = "healing",
                [1] = 0.6,
                [2] = 0.6,
                [3] = 0.65,
                [4] = 0.8,
                [5] = 1.6,
                [6] = 3.2,
                [7] = 4.4,
                [8] = 5.2,
                [9] = 6,
                [10] = 7
            },

            {
                type = "attribute",
                name = "evolutionRequirement",
                formula = evolutionFunction
            },

            {
                type = "minorResistances",
                entries = {"explosion", "physical"}
            },

            {
                type = "majorWeaknesses",
                entries = {"fire"}
            },
        },

        function (attributes)
            return createRangedAttack(attributes, createAttackBall(attributes))
        end
    )
end

return troll
