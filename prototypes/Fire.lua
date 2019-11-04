-- imports

local acidBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")
local particleUtils = require("utils/ParticleUtils")

-- constants

local fire = {}

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

local biterLoot = makeUnitAlienLootTable("red")
local spawnerLoot = makeSpawnerAlienLootTable("red")
local wormLoot = makeWormAlienLootTable("red")

local function evolutionFunction(tier)
    if (tier == 0) then
        return 0
    else
        return 0.12 + ((tier - 2) * 0.10)
    end
end

function fire.addFaction()

    local bloodFountains = {
        type = "attribute",
        mapping = "explosion",
        [1] = "fire-blood-explosion-small-rampant",
        [2] = "fire-blood-explosion-small-rampant",
        [3] = "fire-blood-explosion-small-rampant",
        [4] = "fire-blood-explosion-small-rampant",
        [5] = "fire-blood-explosion-big-rampant",
        [6] = "fire-blood-explosion-big-rampant",
        [7] = "fire-blood-explosion-big-rampant",
        [8] = "fire-blood-explosion-huge-rampant",
        [9] = "fire-blood-explosion-huge-rampant",
        [10] = "fire-blood-explosion-huge-rampant",
    }

    makeBloodFountains({
            name = "fire",
            tint = {r=0.9, g=0, b=0, a=1}
    })


    -- fire biters
    buildUnitSpawner(
        {
            unit = {
                name = "fire-biter",

                loot = biterLoot,
                attributes = {
                },
                attack = {
                    damageType = "acid"
                },
                resistances = {},

                type = "biter",
                tint = {r=1, g=1, b=1, a=1},
                tint2 = {r=0.9, g=0, b=0, a=1}
            },

            unitSpawner = {
                name = "fire-biter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},
                tint = {r=1, g=1, b=1, a=1},
                tint2 = {r=0.9, g=0, b=0, a=1}
            }
        },

        {
            unit = {
                bloodFountains,

                {
                    type = "majorResistances",
                    entries = {"acid","fire"}
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
                    entries = {"acid","fire"}
                }
            }
        },

        createMeleeAttack
    )

    -- fire spitters
    buildUnitSpawner(
        {
            unit = {
                name = "fire-spitter",

                attributes = {
                    explosion = "blood-explosion-small"
                },
                loot = biterLoot,
                attack = {
                    type = "projectile",
                    damageType = "acid",
                    directionOnly = true
                },
                resistances = {},

                type = "spitter",
                attackName = "fire-spitter",
                tint = {r=1, g=1, b=1, a=1},
                tint2 = {r=0.9, g=0, b=0, a=1}
            },

            unitSpawner = {
                name = "fire-spitter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},

                tint = {r=1, g=1, b=1, a=1},
                tint2 = {r=0.9, g=0, b=0, a=1}
            }
        },

        {
            unit = {

                bloodFountains,

                {
                    type = "majorResistances",
                    entries = {"acid","fire"}
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
                    entries = {"acid","fire"}
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

    -- fire worms
    buildWorm(
        {
            name = "fire-worm",

            attributes = {},
            loot = wormLoot,
            attack = {
                type = "projectile",
                damageType = "acid"
            },
            resistances = {},

            attackName = "fire-worm",
            tint = {r=1, g=1, b=1, a=1},
            tint2 = {r=0.9, g=0, b=0, a=1}
        },

        {

            bloodFountains,

            {
                type = "attribute",
                name = "evolutionRequirement",
                formula = evolutionFunction
            },

            {
                type = "majorResistances",
                entries = {"acid","fire"}
            }
        },

        function (attributes)
            return createRangedAttack(attributes,
                                      createAttackBall(attributes))
        end
    )
end


return fire
