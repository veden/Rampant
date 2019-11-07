-- imports

local acidBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local particleUtils = require("utils/ParticleUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")

-- constants

local acid = {}

-- imported functions

local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm

local createAttackBall = acidBall.createAttackBall
local createRangedAttack = biterUtils.createRangedAttack
local createMeleeAttack = biterUtils.createMeleeAttack

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

local makeBloodFountains = particleUtils.makeBloodFountains

local biterLoot = makeUnitAlienLootTable("green")
local spawnerLoot = makeSpawnerAlienLootTable("green")
local wormLoot = makeWormAlienLootTable("green")

local function evolutionFunction(tier)
    if (tier == 0) then
        return 0
    else
        return 0.12 + ((tier - 2) * 0.10)
    end
end

function acid.addFaction()

    local bloodFountains = {
        type = "attribute",
        mapping = "explosion",
        [1] = "acid-blood-explosion-small-rampant",
        [2] = "acid-blood-explosion-small-rampant",
        [3] = "acid-blood-explosion-small-rampant",
        [4] = "acid-blood-explosion-small-rampant",
        [5] = "acid-blood-explosion-big-rampant",
        [6] = "acid-blood-explosion-big-rampant",
        [7] = "acid-blood-explosion-big-rampant",
        [8] = "acid-blood-explosion-huge-rampant",
        [9] = "acid-blood-explosion-huge-rampant",
        [10] = "acid-blood-explosion-huge-rampant",
    }

    makeBloodFountains({
            name = "acid",
            tint = {r=0, g=0.9, b=0, a=1}
    })

    -- acid biters
    buildUnitSpawner(
        {
            unit = {
                name = "acid-biter",

                attributes = {
                },
                attack = {
                    damageType = "acid"
                },
                resistances = {},

                type = "biter",
                loot = biterLoot,
                tint = {r=1, g=1, b=1, a=1},
                tint2 = {r=0, g=0.9, b=0, a=1}
            },

            unitSpawner = {
                name = "acid-biter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},
                tint = {r=1, g=1, b=1, a=1},
                tint2 = {r=0, g=0.9, b=0, a=1}
            }
        },

        {
            unit = {
                bloodFountains,

                {
                    type = "majorResistances",
                    entries = {"acid"}
                },

                {
                    type = "minorResistances",
                    entries = {"poison"}
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
                    entries = {"acid"}
                },

                {
                    type = "minorResistances",
                    entries = {"poison"}
                }

            }
        },

        createMeleeAttack
    )

    -- acid spitters
    buildUnitSpawner(
        {
            unit = {
                name = "acid-spitter",

                loot = biterLoot,
                attributes = {
                },
                attack = {
                    type = "projectile",
                    directionOnly = true
                },
                resistances = {},

                type = "spitter",
                attackName = "acid-spitter",
                tint = {r=1, g=1, b=1, a=1},
                tint2 = {r=0, g=0.9, b=0, a=1}
            },

            unitSpawner = {
                name = "acid-spitter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},

                tint = {r=1, g=1, b=1, a=1},
                tint2 = {r=0, g=0.9, b=0, a=1}
            }
        },

        {
            unit = {

                bloodFountains,

                {
                    type = "attack",
                    name = "damagePerTick",
                    [1] = 0.1,
                    [2] = 0.2,
                    [3] = 0.6,
                    [4] = 1.2,
                    [5] = 1.2,
                    [6] = 1.3,
                    [7] = 1.3,
                    [8] = 1.3,
                    [9] = 1.4,
                    [10] = 1.4
                },

                {
                    type = "majorResistances",
                    entries = {"acid"}
                },

                {
                    type = "minorResistances",
                    entries = {"poison"}
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
                    entries = {"acid"}
                },

                {
                    type = "minorResistances",
                    entries = {"poison"}
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

    -- acid worms
    buildWorm(
        {
            name = "acid-worm",

            loot = wormLoot,
            attributes = {},
            attack = {
                type = "projectile"
            },
            resistances = {},

            attackName = "acid-worm",
            tint = {r=1, g=1, b=1, a=1},
            tint2 = {r=0, g=0.9, b=0, a=1}
        },

        {
            bloodFountains,

            {
                type = "majorResistances",
                entries = {"acid"}
            },

            {
                type = "minorResistances",
                entries = {"poison"}
            }
        },

        function (attributes)
            return createRangedAttack(attributes,
                                      createAttackBall(attributes))
        end
    )


    swarmUtils.buildEntitySpawner(
        {
            name = "acid-hive",

            loot = spawnerLoot,
            attributes = {},
            resistances = {},

            scales = {
                [1] = 1.2,
                [2] = 1.3,
                [3] = 1.4,
                [4] = 1.5,
                [5] = 1.6,
                [6] = 1.7,
                [7] = 1.8,
                [8] = 1.9,
                [9] = 2.0,
                [10] = 2.1
            },

            tint = {r=1, g=1, b=1, a=1},
            tint2 = {r=0, g=0.9, b=0, a=1}
        },
        {
            [1] = {
                {"entity-proxy-turret", 0.8},
                {"entity-proxy-spitter-spawner", 0.1},
                {"entity-proxy-biter-spawner", 0.1}
            },
            [2] = {
                {"entity-proxy-turret", 0.7},
                {"entity-proxy-spitter-spawner", 0.15},
                {"entity-proxy-biter-spawner", 0.15}
            },
            [3] = {
                {"entity-proxy-turret", 0.595},
                {"entity-proxy-spitter-spawner", 0.2},
                {"entity-proxy-biter-spawner", 0.2},
                {"entity-proxy-hive", 0.005}
            },
            [4] = {
                {"entity-proxy-turret", 0.59},
                {"entity-proxy-spitter-spawner", 0.2},
                {"entity-proxy-biter-spawner", 0.2},
                {"entity-proxy-hive", 0.01}
            },
            [5] = {
                {"entity-proxy-turret", 0.59},
                {"entity-proxy-spitter-spawner", 0.2},
                {"entity-proxy-biter-spawner", 0.2},
                {"entity-proxy-hive", 0.01}
            },
            [6] = {
                {"entity-proxy-turret", 0.58},
                {"entity-proxy-spitter-spawner", 0.2},
                {"entity-proxy-biter-spawner", 0.2},
                {"entity-proxy-hive", 0.02}
            },
            [7] = {
                {"entity-proxy-turret", 0.58},
                {"entity-proxy-spitter-spawner", 0.2},
                {"entity-proxy-biter-spawner", 0.2},
                {"entity-proxy-hive", 0.02}
            },
            [8] = {
                {"entity-proxy-turret", 0.57},
                {"entity-proxy-spitter-spawner", 0.2},
                {"entity-proxy-biter-spawner", 0.2},
                {"entity-proxy-hive", 0.03}
            },
            [9] = {
                {"entity-proxy-turret", 0.57},
                {"entity-proxy-spitter-spawner", 0.2},
                {"entity-proxy-biter-spawner", 0.2},
                {"entity-proxy-hive", 0.03}
            },
            [10] = {
                {"entity-proxy-turret", 0.57},
                {"entity-proxy-spitter-spawner", 0.2},
                {"entity-proxy-biter-spawner", 0.2},
                {"entity-proxy-hive", 0.03}
            }
        },
        {
            bloodFountains,

            {
                type = "attribute",
                name = "evolutionRequirement",
                formula = evolutionFunction
            },

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
                [10] = 90000
            },

            {
                type = "majorResistances",
                entries = {"acid"}
            },

            {
                type = "minorResistances",
                entries = {"poison"}
            },

            {
                type = "attribute",
                name = "spawningRadius",
                [1] = 10,
                [2] = 13,
                [3] = 15,
                [4] = 17,
                [5] = 20,
                [6] = 23,
                [7] = 26,
                [8] = 29,
                [9] = 32,
                [10] = 35
            },

            {
                type = "attribute",
                name = "spawningSpacing",
                [1] = 5,
                [2] = 5,
                [3] = 5,
                [4] = 6,
                [5] = 6,
                [6] = 6,
                [7] = 7,
                [8] = 7,
                [9] = 7,
                [10] = 8
            },

            -- {
            --     type = "attribute",
            --     mapping = "spawningCooldown",
            --     [1] = {2840,1020},
            --     [2] = {2800,1015},
            --     [3] = {2760,1010},
            --     [4] = {2720,1005},
            --     [5] = {2680,1000},
            --     [6] = {2640,995},
            --     [7] = {2600,990},
            --     [8] = {2560,985},
            --     [9] = {2520,980},
            --     [10] = {2480,975}
            -- },

            {
                type = "attribute",
                mapping = "spawningCooldown",
                [1] = {60,60},
                [2] = {60,60},
                [3] = {60,60},
                [4] = {60,60},
                [5] = {60,60},
                [6] = {60,60},
                [7] = {60,60},
                [8] = {60,60},
                [9] = {60,60},
                [10] = {60,60}
            },

            {
                type = "attribute",
                name = "unitsToSpawn",
                [1] = 2000,
                [2] = 2000,
                [3] = 2000,
                [4] = 2000,
                [5] = 2000,
                [6] = 2000,
                [7] = 2000,
                [8] = 2000,
                [9] = 2000,
                [10] = 2000
            }

        }
    )

end

return acid
