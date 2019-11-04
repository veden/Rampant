-- imports

local acidBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")
local particleUtils = require("utils/ParticleUtils")

-- constants

local neutral = {}

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

function neutral.addFaction()

    local biterLoot = makeUnitAlienLootTable(nil)
    local spawnerLoot = makeSpawnerAlienLootTable(nil)
    local wormLoot = makeWormAlienLootTable(nil)

    local bloodFountains = {
        type = "attribute",
        mapping = "explosion",
        [1] = "neutral-blood-explosion-small-rampant",
        [2] = "neutral-blood-explosion-small-rampant",
        [3] = "neutral-blood-explosion-small-rampant",
        [4] = "neutral-blood-explosion-small-rampant",
        [5] = "neutral-blood-explosion-big-rampant",
        [6] = "neutral-blood-explosion-big-rampant",
        [7] = "neutral-blood-explosion-big-rampant",
        [8] = "neutral-blood-explosion-huge-rampant",
        [9] = "neutral-blood-explosion-huge-rampant",
        [10] = "neutral-blood-explosion-huge-rampant",
    }

    makeBloodFountains({
            name = "neutral",
            tint = {r=1, g=1, b=1, a=1}
    })

    -- neutral biters
    buildUnitSpawner(
        {
            unit = {
                name = "neutral-biter",

                loot = biterLoot,
                attributes = {
                },
                attack = {},
                resistances = {},

                type = "biter",
                tint = {r=0.9, g=0.9, b=0.9, a=1},
                tint2 = {r=1, g=1, b=1, a=1}
            },

            unitSpawner = {
                name = "neutral-biter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},
                tint = {r=0.9, g=0.9, b=0.9, a=1},
                tint2 = {r=1, g=1, b=1, a=1}
            }
        },

        {
            unit = {
                bloodFountains
            },

            unitSpawner = {
                bloodFountains
            }
        },

        createMeleeAttack
    )

    -- neutral spitters
    buildUnitSpawner(
        {
            unit = {
                name = "neutral-spitter",

                loot = biterLoot,
                attributes = {
                },
                attack = {
                    type = "projectile",
                    directionOnly = true
                },
                resistances = {},

                type = "spitter",
                attackName = "neutral-spitter",
                tint = {r=0.9, g=0.9, b=0.9, a=1},
                tint2 = {r=1, g=1, b=1, a=1}
            },

            unitSpawner = {
                name = "neutral-spitter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},

                tint = {r=0.9, g=0.9, b=0.9, a=1},
                tint2 = {r=1, g=1, b=1, a=1}
            }
        },

        {
            unit = {
                bloodFountains
            },

            unitSpawner = {
                bloodFountains
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

    -- neutral worms
    buildWorm(
        {
            name = "neutral-worm",

            loot = wormLoot,
            attributes = {},
            attack = {
                type = "projectile"
            },
            resistances = {},

            attackName = "neutral-worm",
            tint = {r=0.9, g=0.9, b=0.9, a=1},
            tint2 = {r=1, g=1, b=1, a=1}
        },

        {
            bloodFountains
        },

        function (attributes)
            return createRangedAttack(attributes,
                                      createAttackBall(attributes))
        end
    )
end


return neutral
