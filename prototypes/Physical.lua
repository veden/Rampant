-- imports

local physicalBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")
local particleUtils = require("utils/ParticleUtils")

-- constants

local physical = {}

-- imported functions

local makeBloodFountains = particleUtils.makeBloodFountains
local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createAttackBall = physicalBall.createAttackBall
local createRangedAttack = biterUtils.createRangedAttack
local createMeleeAttack = biterUtils.createMeleeAttack

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

local function evolutionFunction(tier)
    if (tier == 0) then
        return 0
    else
        return 0.12 + ((tier - 2) * 0.10)
    end
end

function physical.addFaction()

    local biterLoot = makeUnitAlienLootTable("red")
    local spawnerLoot = makeSpawnerAlienLootTable("red")
    local wormLoot = makeWormAlienLootTable("red")


    local bloodFountains = {
        type = "attribute",
        mapping = "explosion",
        [1] = "physical-blood-explosion-small-rampant",
        [2] = "physical-blood-explosion-small-rampant",
        [3] = "physical-blood-explosion-small-rampant",
        [4] = "physical-blood-explosion-small-rampant",
        [5] = "physical-blood-explosion-big-rampant",
        [6] = "physical-blood-explosion-big-rampant",
        [7] = "physical-blood-explosion-big-rampant",
        [8] = "physical-blood-explosion-huge-rampant",
        [9] = "physical-blood-explosion-huge-rampant",
        [10] = "physical-blood-explosion-huge-rampant",
    }

    makeBloodFountains({
            name = "physical",
            tint = {r=0, g=0, b=0, a=1}
    })

    -- physical biters
    buildUnitSpawner(
        {
            unit = {
                name = "physical-biter",

                loot = biterLoot,
                attributes = {
                },
                attack = {

                },
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
                tint = {r=0.9, g=0.9, b=0.9, a=1},
                tint2 = {r=0, g=0, b=0, a=1},
            },

            unitSpawner = {
                name = "physical-biter-spawner",

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
                tint = {r=0.9, g=0.9, b=0.9, a=1},
                tint2 = {r=0, g=0, b=0, a=1},
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
                    name = "movement",
                    [1] = 0.17,
                    [2] = 0.17,
                    [3] = 0.1675,
                    [4] = 0.1675,
                    [5] = 0.165,
                    [6] = 0.165,
                    [7] = 0.16,
                    [8] = 0.16,
                    [9] = 0.1575,
                    [10] = 0.1575
                },

                {
                    type = "attribute",
                    name = "distancePerFrame",
                    [1] = 0.075,
                    [2] = 0.075,
                    [3] = 0.08,
                    [4] = 0.08,
                    [5] = 0.085,
                    [6] = 0.085,
                    [7] = 0.09,
                    [8] = 0.09,
                    [9] = 0.1,
                    [10] = 0.1
                },

                {
                    type = "majorResistances",
                    entries = {"physical", "explosion"}
                },

                {
                    type = "minorWeaknesses",
                    entries = {"laser", "electric"}
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
                    name = "evolutionRequirement",
                    formula = evolutionFunction
                },

                {
                    type = "majorResistances",
                    entries = {"physical", "explosion"}
                },

                {
                    type = "minorWeaknesses",
                    entries = {"laser", "electric"}
                }

            }
        },

        createMeleeAttack
    )

    -- physical worms
    buildWorm(
        {
            name = "physical-worm",

            loot = wormLoot,
            attributes = {
            },
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
                            entity_name = attributes.attackExplosion
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
            attackName = "physical-worm",
            tint = {r=0.9, g=0.9, b=0.9, a=1},
            tint2 = {r=0, g=0, b=0, a=1},
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
                type = "majorResistances",
                entries = {"physical", "explosion"}
            },

            {
                type = "minorWeaknesses",
                entries = {"laser", "electric"}
            },

            {
                type = "attribute",
                name = "evolutionRequirement",
                formula = evolutionFunction
            }

        },

        function (attributes)
            return createRangedAttack(attributes,
                                      createAttackBall(attributes))
        end
    )
end

return physical
