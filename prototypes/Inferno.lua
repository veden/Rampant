-- imports

local attackFlame = require("utils/AttackFlame")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")

local constants = require("__Rampant__/libs/Constants")
local math3d = require("math3d")
local particleUtils = require("utils/ParticleUtils")

-- constants

local inferno = {}

-- imported functions

local makeBloodFountains = particleUtils.makeBloodFountains
local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createAttackFlame = attackFlame.createAttackFlame
local createStreamAttack = biterUtils.createStreamAttack

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

local biterLoot = makeUnitAlienLootTable("orange")
local spawnerLoot = makeSpawnerAlienLootTable("orange")
local wormLoot = makeWormAlienLootTable("orange")

local function evolutionFunction(tier)
    if (tier == 0) then
        return 0
    else
        return 0.15 + ((tier - 2) * 0.10)
    end
end

function inferno.addFaction()

    local bloodFountains = {
        type = "attribute",
        mapping = "explosion",
        [1] = "inferno-blood-explosion-small-rampant",
        [2] = "inferno-blood-explosion-small-rampant",
        [3] = "inferno-blood-explosion-small-rampant",
        [4] = "inferno-blood-explosion-small-rampant",
        [5] = "inferno-blood-explosion-big-rampant",
        [6] = "inferno-blood-explosion-big-rampant",
        [7] = "inferno-blood-explosion-big-rampant",
        [8] = "inferno-blood-explosion-huge-rampant",
        [9] = "inferno-blood-explosion-huge-rampant",
        [10] = "inferno-blood-explosion-huge-rampant",
    }

    makeBloodFountains({
            name = "inferno",
            tint = {r=0.6, g=0, b=0, a=1}
    })


    -- inferno spitters
    buildUnitSpawner(
        {
            unit = {
                name = "inferno-spitter",

                loot = biterLoot,
                attributes = {
                },
                attack = {
                    damageType = "acid",
                    fireDamagePerTickType = "acid",
                    stickerDamagePerTickType = "acid"
                },
                resistances = {},

                type = "spitter",
                attackName = "spitter-inferno",
                tint = {r=0.7, g=0.45, b=0.5, a=1},
                tint2 = {r=0.9, g=0, b=0, a=1}
            },

            unitSpawner = {
                name = "inferno-spitter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},

                tint = {r=0.7, g=0.45, b=0.5, a=1},
                tint2 = {r=0.9, g=0, b=0, a=1}
            }
        },

        {
            unit = {

                bloodFountains,

                {
                    type = "attack",
                    name = "stickerDamagePerTick",
                    [1] = 0.6,
                    [2] = 0.6,
                    [3] = 0.8,
                    [4] = 0.8,
                    [5] = 0.8,
                    [6] = 0.9,
                    [7] = 1,
                    [8] = 1,
                    [9] = 1.3,
                    [10] = 1.5
                },

                {
                    type = "attack",
                    name = "particleTimeout",
                    [1] = 3,
                    [2] = 3,
                    [3] = 4,
                    [4] = 4,
                    [5] = 5,
                    [6] = 5,
                    [7] = 6,
                    [8] = 6,
                    [9] = 7,
                    [10] = 7
                },

                {
                    type = "attack",
                    name = "fireSpreadRadius",
                    [1] = 0.75,
                    [2] = 0.75,
                    [3] = 0.77,
                    [4] = 0.77,
                    [5] = 0.79,
                    [6] = 0.79,
                    [7] = 0.83,
                    [8] = 0.83,
                    [9] = 0.85,
                    [10] = 0.85
                },

                {
                    type = "attack",
                    name = "damageMaxMultipler",
                    [1] = 6,
                    [2] = 6,
                    [3] = 7,
                    [4] = 7,
                    [5] = 7,
                    [6] = 7,
                    [7] = 8,
                    [8] = 8,
                    [9] = 8,
                    [10] = 9
                },

                {
                    type = "attack",
                    name = "stickerMovementModifier",
                    [1] = 1.1,
                    [2] = 1.1,
                    [3] = 1.1,
                    [4] = 1.1,
                    [5] = 1.1,
                    [6] = 1.1,
                    [7] = 1.1,
                    [8] = 1.1,
                    [9] = 1.1,
                    [10] = 1.1
                },

                {
                    type = "attack",
                    name = "fireSpreadCooldown",
                    [1] = 30,
                    [2] = 30,
                    [3] = 29,
                    [4] = 29,
                    [5] = 28,
                    [6] = 28,
                    [7] = 27,
                    [8] = 27,
                    [9] = 25,
                    [10] = 25
                },

                {
                    type = "attack",
                    name = "stickerDuration",
                    [1] = 800,
                    [2] = 800,
                    [3] = 900,
                    [4] = 900,
                    [5] = 1000,
                    [6] = 1000,
                    [7] = 1100,
                    [8] = 1100,
                    [9] = 1200,
                    [10] = 1200
                },

                {
                    type = "attack",
                    name = "damage",
                    [1] = 4,
                    [2] = 4,
                    [3] = 5,
                    [4] = 5,
                    [5] = 6,
                    [6] = 6,
                    [7] = 6,
                    [8] = 6,
                    [9] = 6,
                    [10] = 7
                },

                {
                    type = "majorResistances",
                    entries = {"acid", "fire"}
                },

                {
                    type = "minorWeaknesses",
                    entries = {"poison"}
                }

            },

            unitSpawner = {

                bloodFountains,

                {
                    type = "majorResistances",
                    entries = {"acid", "fire"}
                },

                {
                    type = "minorWeaknesses",
                    entries = {"poison"}
                },

                {
                    type = "attribute",
                    name = "evolutionRequirement",
                    formula = evolutionFunction
                }
            }
        },

        function (attributes)
            return createStreamAttack(attributes,
                                      createAttackFlame(attributes),
                                      spitterattackanimation(attributes.scale,
                                                             attributes.tint,
                                                             attributes.tint2))
        end
    )

    -- inferno worms
    buildWorm(
        {
            name = "inferno-worm",

            loot = wormLoot,
            attributes = {},
            attack = {
                damageType = "acid",
                fireDamagePerTickType = "acid",
                stickerDamagePerTickType = "acid"
            },
            resistances = {},

            attackName = "worm-inferno",
            tint = {r=0.7, g=0.45, b=0.5, a=1},
            tint2 = {r=0.9, g=0, b=0, a=1}

        },

        {
            bloodFountains,

            {
                type = "attack",
                name = "stickerDamagePerTick",
                [1] = 0.6,
                [2] = 0.6,
                [3] = 0.8,
                [4] = 0.8,
                [5] = 0.8,
                [6] = 0.9,
                [7] = 1,
                [8] = 1,
                [9] = 1.3,
                [10] = 1.5
            },

            {
                type = "attribute",
                name = "evolutionRequirement",
                formula = evolutionFunction
            },

            {
                type = "attack",
                name = "particleTimeout",
                [1] = 3,
                [2] = 3,
                [3] = 4,
                [4] = 4,
                [5] = 5,
                [6] = 5,
                [7] = 6,
                [8] = 6,
                [9] = 7,
                [10] = 7
            },

            {
                type = "attack",
                name = "fireSpreadRadius",
                [1] = 0.75,
                [2] = 0.75,
                [3] = 0.77,
                [4] = 0.77,
                [5] = 0.79,
                [6] = 0.79,
                [7] = 0.83,
                [8] = 0.83,
                [9] = 0.85,
                [10] = 0.85
            },

            {
                type = "attack",
                name = "damageMaxMultipler",
                [1] = 6,
                [2] = 6,
                [3] = 7,
                [4] = 7,
                [5] = 7,
                [6] = 7,
                [7] = 8,
                [8] = 8,
                [9] = 8,
                [10] = 9
            },

            {
                type = "attack",
                name = "stickerMovementModifier",
                [1] = 1.1,
                [2] = 1.1,
                [3] = 1.1,
                [4] = 1.1,
                [5] = 1.1,
                [6] = 1.1,
                [7] = 1.1,
                [8] = 1.1,
                [9] = 1.1,
                [10] = 1.1
            },

            {
                type = "attack",
                name = "fireSpreadCooldown",
                [1] = 30,
                [2] = 30,
                [3] = 29,
                [4] = 29,
                [5] = 28,
                [6] = 28,
                [7] = 27,
                [8] = 27,
                [9] = 25,
                [10] = 25
            },

            {
                type = "attack",
                name = "stickerDuration",
                [1] = 800,
                [2] = 800,
                [3] = 900,
                [4] = 900,
                [5] = 1000,
                [6] = 1000,
                [7] = 1100,
                [8] = 1100,
                [9] = 1200,
                [10] = 1200
            },

            {
                type = "attack",
                name = "damage",
                [1] = 4,
                [2] = 4,
                [3] = 5,
                [4] = 5,
                [5] = 6,
                [6] = 6,
                [7] = 6,
                [8] = 6,
                [9] = 6,
                [10] = 7
            },

            {
                type = "majorResistances",
                entries = {"acid", "fire"}
            },

            {
                type = "minorWeaknesses",
                entries = {"poison"}
            }
        },

        function (attributes)
            return createStreamAttack(attributes,
                                      createAttackFlame(attributes))
        end
    )
end

return inferno
