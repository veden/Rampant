-- imports

local acidBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local stickerUtils = require("utils/StickerUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")
local particleUtils = require("utils/ParticleUtils")

-- constants

local suicide = {}

-- imported functions

local makeBloodFountains = particleUtils.makeBloodFountains
local makeSticker = stickerUtils.makeSticker
local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createAttackBall = acidBall.createAttackBall
local createRangedAttack = biterUtils.createRangedAttack
local createSuicideAttack = biterUtils.createSuicideAttack

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

function suicide.addFaction()

    local biterLoot = makeUnitAlienLootTable("yellow")
    local spawnerLoot = makeSpawnerAlienLootTable("yellow")
    local wormLoot = makeWormAlienLootTable("yellow")

    local bloodFountains = {
        type = "attribute",
        mapping = "explosion",
        [1] = "suicide-blood-explosion-small-rampant",
        [2] = "suicide-blood-explosion-small-rampant",
        [3] = "suicide-blood-explosion-small-rampant",
        [4] = "suicide-blood-explosion-small-rampant",
        [5] = "suicide-blood-explosion-big-rampant",
        [6] = "suicide-blood-explosion-big-rampant",
        [7] = "suicide-blood-explosion-big-rampant",
        [8] = "suicide-blood-explosion-huge-rampant",
        [9] = "suicide-blood-explosion-huge-rampant",
        [10] = "suicide-blood-explosion-huge-rampant",
    }

    makeBloodFountains({
            name = "suicide",
            tint = {r=0.85, g=0.85, b=0, a=1}
    })


    -- suicide biters
    buildUnitSpawner(
        {
            unit = {
                name = "suicide-biter",

                loot = biterLoot,
                attributes = {

                },
                attack = {
                    scorchmark = "small-scorchmark"
                },
                resistances = {},

                type = "biter",
                tint = {r=1, g=1, b=1, a=1},
                tint = {r=0.85, g=0.85, b=0, a=1}
            },

            unitSpawner = {
                name = "suicide-biter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},
                tint = {r=1, g=1, b=1, a=1},
                tint = {r=0.85, g=0.85, b=0, a=1}
            }
        },

        {
            unit = {
                bloodFountains,

                {

                    type = "attribute",
                    name = "health",
                    [1] = 7,
                    [2] = 42,
                    [3] = 150,
                    [4] = 250,
                    [5] = 1000,
                    [6] = 3000,
                    [7] = 5000,
                    [8] = 7000,
                    [9] = 9000,
                    [10] = 17500
                },

                {
                    type = "attribute",
                    name = "spawningTimeModifer",
                    [1] = 1,
                    [2] = 1,
                    [3] = 1,
                    [4] = 2,
                    [5] = 3,
                    [6] = 5,
                    [7] = 6,
                    [8] = 6,
                    [9] = 8,
                    [10] = 8
                },

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
                    type = "attack",
                    name = "radius",
                    [1] = 3.5,
                    [2] = 3.5,
                    [3] = 4,
                    [4] = 5,
                    [5] = 6,
                    [6] = 6,
                    [7] = 7,
                    [8] = 7,
                    [9] = 7.5,
                    [10] = 8
                },

                {
                    type = "attack",
                    name = "explosionDistance",
                    [1] = 2,
                    [2] = 2,
                    [3] = 2,
                    [4] = 2,
                    [5] = 2,
                    [6] = 2.5,
                    [7] = 2.5,
                    [8] = 2.5,
                    [9] = 3,
                    [10] = 3
                },

                {
                    type = "attack",
                    name = "explosionCount",
                    min = 2,
                    [1] = 2,
                    [2] = 3,
                    [3] = 4,
                    [4] = 5,
                    [5] = 6,
                    [6] = 8,
                    [7] = 10,
                    [8] = 12,
                    [9] = 13,
                    [10] = 14
                },

                {
                    type = "attack",
                    name = "damage",
                    [1] = 100,
                    [2] = 200,
                    [3] = 300,
                    [4] = 400,
                    [5] = 600,
                    [6] = 800,
                    [7] = 1000,
                    [8] = 1200,
                    [9] = 1500,
                    [10] = 2000
                },

                {
                    type = "attribute",
                    name = "movement",
                    [1] = 0.23,
                    [2] = 0.23,
                    [3] = 0.22,
                    [4] = 0.22,
                    [5] = 0.21,
                    [6] = 0.21,
                    [7] = 0.2,
                    [8] = 0.2,
                    [9] = 0.19,
                    [10] = 0.19
                },
                {
                    type = "attribute",
                    name = "distancePerFrame",
                    [1] = 0.12,
                    [2] = 0.145,
                    [3] = 0.17,
                    [4] = 0.21,
                    [5] = 0.21,
                    [6] = 0.22,
                    [7] = 0.22,
                    [8] = 0.23,
                    [9] = 0.23,
                    [10] = 0.24
                },

                {
                    type = "majorWeaknesses",
                    entries = {"explosion"}
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
                    type = "attribute",
                    name = "evolutionRequirement",
                    formula = evolutionFunction
                },

                {
                    type = "attribute",
                    name = "unitsOwned",
                    [1] = 6,
                    [2] = 6,
                    [3] = 7,
                    [4] = 7,
                    [5] = 8,
                    [6] = 8,
                    [7] = 9,
                    [8] = 9,
                    [9] = 10,
                    [10] = 10
                },

                {
                    type = "attribute",
                    name = "unitsToSpawn",
                    [1] = 3,
                    [2] = 3,
                    [3] = 4,
                    [4] = 5,
                    [5] = 5,
                    [6] = 6,
                    [7] = 6,
                    [8] = 7,
                    [9] = 7,
                    [10] = 8
                },

                {
                    type = "majorResistances",
                    entries = {"explosion"}
                },

                {
                    type = "minorResistances",
                    entries = {"fire", "poison"}
                }
            }
        },

        createSuicideAttack
    )

    -- suicide worms
    buildWorm(
        {
            name = "suicide-worm",

            loot = wormLoot,
            attributes = {},
            attack = {
                type = "projectile",
                force = "enemy",
                stickerAnimation = {
                    filename = "__base__/graphics/entity/slowdown-sticker/slowdown-sticker.png",
                    priority = "extra-high",
                    width = 11,
                    height = 11,
                    frame_count = 13,
                    animation_speed = 0.4
                },
                areaEffects = function (attributes)
                    return {
                        {
                            type = "damage",
                            damage = { amount = attributes.damage, type = "acid" }
                        },
                        {
                            type = "create-sticker",
                            sticker = attributes.name .. "-sticker-rampant"
                        }
                    }
                end
            },
            resistances = {},

            attackName = "suicide-worm",
            tint = {r=1, g=1, b=1, a=1},
            tint = {r=0.85, g=0.85, b=0, a=1}
        },

        {
            bloodFountains,

            {
                type = "attack",
                name = "stickerMovementModifier",
                [1] = 0.8,
                [2] = 0.8,
                [3] = 0.7,
                [4] = 0.7,
                [5] = 0.6,
                [6] = 0.6,
                [7] = 0.5,
                [8] = 0.5,
                [9] = 0.4,
                [10] = 0.4
            },

            {
                type = "attack",
                name = "stickerDuration",
                [1] = 1800,
                [2] = 1800,
                [3] = 1900,
                [4] = 1900,
                [5] = 2000,
                [6] = 2000,
                [7] = 2100,
                [8] = 2100,
                [9] = 2200,
                [10] = 2200
            },

            {
                type = "attribute",
                name = "evolutionRequirement",
                formula = evolutionFunction
            },

            {
                type = "majorResistances",
                entries = {"explosion"}
            },

            {
                type = "minorResistances",
                entries = {"fire", "poison"}
            }
        },

        function (attributes)
            makeSticker(attributes)
            return createRangedAttack(attributes, createAttackBall(attributes))
        end
    )
end

return suicide
