-- imports

local acidBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")
local particleUtils = require("utils/ParticleUtils")

-- constants

local fast = {}

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

local biterLoot = makeUnitAlienLootTable("purple")
local spawnerLoot = makeSpawnerAlienLootTable("purple")
local wormLoot = makeWormAlienLootTable("purple")

local function evolutionFunction(tier)
    if (tier == 0) then
        return 0
    else
        return 0.12 + ((tier - 2) * 0.10)
    end
end

function fast.addFaction()

    local bloodFountains = {
        type = "attribute",
        mapping = "explosion",
        [1] = "fast-blood-explosion-small-rampant",
        [2] = "fast-blood-explosion-small-rampant",
        [3] = "fast-blood-explosion-small-rampant",
        [4] = "fast-blood-explosion-small-rampant",
        [5] = "fast-blood-explosion-big-rampant",
        [6] = "fast-blood-explosion-big-rampant",
        [7] = "fast-blood-explosion-big-rampant",
        [8] = "fast-blood-explosion-huge-rampant",
        [9] = "fast-blood-explosion-huge-rampant",
        [10] = "fast-blood-explosion-huge-rampant",
    }

    makeBloodFountains({
            name = "fast",
            tint = {r=0, g=0.85, b=0.80, a=1}
    })


    -- fast biters
    buildUnitSpawner(
        {
            unit = {
                name = "fast-biter",

                attributes = {
                },
                attack = {},
                resistances = {},

                loot = biterLoot,
                type = "biter",
                tint = {r=0.26, g=0.66, b=0.62, a=1},
                tint2 = {r=0, g=0.85, b=0.80, a=1}
            },

            unitSpawner = {
                name = "fast-biter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},
                tint = {r=0.26, g=0.66, b=0.62, a=1},
                tint2 = {r=0, g=0.85, b=0.80, a=1}
            }
        },

        {
            unit = {

                bloodFountains,

                {
                    type = "attack",
                    name = "cooldown",
                    [1] = 35,
                    [2] = 36,
                    [3] = 38,
                    [4] = 38,
                    [5] = 40,
                    [6] = 42,
                    [7] = 42,
                    [8] = 43,
                    [9] = 43,
                    [10] = 45
                },

                {
                    type = "minorResistances",
                    entries = {"explosion"}
                },

                {
                    type = "attribute",
                    name = "movement",
                    [1] = 0.22,
                    [2] = 0.22,
                    [3] = 0.23,
                    [4] = 0.23,
                    [5] = 0.23,
                    [6] = 0.23,
                    [7] = 0.24,
                    [8] = 0.24,
                    [9] = 0.24,
                    [10] = 0.25
                },

                {
                    type = "attribute",
                    name = "distancePerFrame",
                    [1] = 0.12,
                    [2] = 0.145,
                    [3] = 0.17,
                    [4] = 0.21,
                    [5] = 0.215,
                    [6] = 0.22,
                    [7] = 0.22,
                    [8] = 0.24,
                    [9] = 0.24,
                    [10] = 0.26
                }

            },

            unitSpawner = {

                bloodFountains,

                {
                    type = "attribute",
                    name = "spawingCooldownStart",
                    [1] = 330,
                    [2] = 330,
                    [3] = 325,
                    [4] = 325,
                    [5] = 320,
                    [6] = 320,
                    [7] = 315,
                    [8] = 315,
                    [9] = 310,
                    [10] = 310
                },

                {
                    type = "attribute",
                    name = "spawingCooldownEnd",
                    [1] = 120,
                    [2] = 120,
                    [3] = 125,
                    [4] = 125,
                    [5] = 120,
                    [6] = 120,
                    [7] = 115,
                    [8] = 115,
                    [9] = 110,
                    [10] = 110
                },

                {
                    type = "minorResistances",
                    entries = {"explosion"}
                },

                {
                    type = "attribute",
                    name = "evolutionRequirement",
                    formula = evolutionFunction
                }
            }
        },

        createMeleeAttack
    )

    -- fast spitters
    buildUnitSpawner(
        {
            unit = {
                name = "fast-spitter",

                loot = biterLoot,
                attributes = {
                    explosion = "blood-explosion-small"
                },
                attack = {
                    type = "projectile",
                    directionOnly = true
                },
                resistances = {},

                type = "spitter",
                attackName = "fast-ball",
                tint = {r=0.26, g=0.66, b=0.62, a=1},
                tint2 = {r=0, g=0.85, b=0.80, a=1}
            },

            unitSpawner = {
                name = "fast-spitter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},

                tint = {r=0.26, g=0.66, b=0.62, a=1},
                tint2 = {r=0, g=0.85, b=0.80, a=1}
            }
        },

        {
            unit = {

                bloodFountains,

                {
                    type = "attack",
                    name = "cooldown",
                    [1] = 80,
                    [2] = 80,
                    [3] = 77,
                    [4] = 77,
                    [5] = 75,
                    [6] = 75,
                    [7] = 73,
                    [8] = 73,
                    [9] = 70,
                    [10] = 70
                },

                {
                    type = "attribute",
                    name = "movement",
                    [1] = 0.205,
                    [2] = 0.20,
                    [3] = 0.20,
                    [4] = 0.21,
                    [5] = 0.21,
                    [6] = 0.22,
                    [7] = 0.22,
                    [8] = 0.23,
                    [9] = 0.23,
                    [10] = 0.24
                },

                {
                    type = "minorResistances",
                    entries = {"explosion"}
                },

                {
                    type = "attribute",
                    name = "distancePerFrame",
                    [1] = 0.06,
                    [2] = 0.065,
                    [3] = 0.070,
                    [4] = 0.075,
                    [5] = 0.080,
                    [6] = 0.085,
                    [7] = 0.090,
                    [8] = 0.095,
                    [9] = 0.10,
                    [10] = 0.104
                }

            },

            unitSpawner = {

                bloodFountains,

                {

                    type = "attribute",
                    name = "spawingCooldownStart",
                    [1] = 330,
                    [2] = 330,
                    [3] = 325,
                    [4] = 325,
                    [5] = 320,
                    [6] = 320,
                    [7] = 315,
                    [8] = 315,
                    [9] = 310,
                    [10] = 310
                },

                {
                    type = "attribute",
                    name = "spawingCooldownEnd",
                    [1] = 120,
                    [2] = 120,
                    [3] = 115,
                    [4] = 115,
                    [5] = 110,
                    [6] = 110,
                    [7] = 105,
                    [8] = 105,
                    [9] = 100,
                    [10] = 100
                },

                {
                    type = "minorResistances",
                    entries = {"explosion"}
                },

                {
                    type = "attribute",
                    name = "evolutionRequirement",
                    formula = evolutionFunction
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

    -- fast worms
    buildWorm(
        {
            name = "fast-worm",

            loot = wormLoot,
            attributes = {},
            attack = {
                type = "projectile"
            },
            resistances = {},

            attackName = "worm-fast",
            tint = {r=0.26, g=0.66, b=0.62, a=1},
            tint2 = {r=0.26, g=0.85, b=0.80, a=1}
        },

        {
            bloodFountains,

            {
                type = "attack",
                name = "cooldown",
                [1] = 50,
                [2] = 50,
                [3] = 45,
                [4] = 45,
                [5] = 40,
                [6] = 40,
                [7] = 35,
                [8] = 35,
                [9] = 30,
                [10] = 30
            },

            {
                type = "attribute",
                name = "foldingSpeed",
                [1] = 0.17,
                [2] = 0.17,
                [3] = 0.18,
                [4] = 0.18,
                [5] = 0.18,
                [6] = 0.19,
                [7] = 0.19,
                [8] = 0.20,
                [9] = 0.20,
                [10] = 0.21
            },

            {
                type = "attribute",
                name = "preparingSpeed",
                [1] = 0.027,
                [2] = 0.027,
                [3] = 0.028,
                [4] = 0.028,
                [5] = 0.029,
                [6] = 0.029,
                [7] = 0.030,
                [8] = 0.030,
                [9] = 0.031,
                [10] = 0.031
            },

            {
                type = "minorResistances",
                entries = {"explosion"}
            },

            {
                type = "attribute",
                name = "evolutionRequirement",
                formula = evolutionFunction
            },

            {
                type = "attack",
                name = "particleHoizontalSpeed",
                [1] = 0.8,
                [2] = 0.8,
                [3] = 0.9,
                [4] = 0.9,
                [5] = 1,
                [6] = 1,
                [7] = 1.1,
                [8] = 1.1,
                [9] = 1.2,
                [10] = 1.2
            }
        },

        function (attributes)
            return createRangedAttack(attributes,
                                      createAttackBall(attributes))
        end
    )
end

return fast
