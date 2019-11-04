-- imports

local acidBall = require("utils/AttackBall")
local biterUtils = require("utils/BiterUtils")
local stickerUtils = require("utils/StickerUtils")
local bombUtils = require("utils/BombUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")
local particleUtils = require("utils/ParticleUtils")

-- constants

local nuclear = {}

-- imported functions

local makeBloodFountains = particleUtils.makeBloodFountains
local makeSticker = stickerUtils.makeSticker
local makeAtomicBlast = bombUtils.makeAtomicBlast
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
        return 0.17 + ((tier - 2) * 0.10)
    end
end

function nuclear.addFaction()

    local bloodFountains = {
        type = "attribute",
        mapping = "explosion",
        [1] = "nuclear-blood-explosion-small-rampant",
        [2] = "nuclear-blood-explosion-small-rampant",
        [3] = "nuclear-blood-explosion-small-rampant",
        [4] = "nuclear-blood-explosion-small-rampant",
        [5] = "nuclear-blood-explosion-big-rampant",
        [6] = "nuclear-blood-explosion-big-rampant",
        [7] = "nuclear-blood-explosion-big-rampant",
        [8] = "nuclear-blood-explosion-huge-rampant",
        [9] = "nuclear-blood-explosion-huge-rampant",
        [10] = "nuclear-blood-explosion-huge-rampant",
    }

    makeBloodFountains({
            name = "nuclear",
            tint = {r=0.95, g=0.95, b=0, a=1}
    })

    local biterLoot = makeUnitAlienLootTable("yellow")
    local spawnerLoot = makeSpawnerAlienLootTable("yellow")
    local wormLoot = makeWormAlienLootTable("yellow")

    -- nuclear biters
    buildUnitSpawner(
        {
            unit = {
                name = "nuclear-biter",

                loot = biterLoot,
                attributes = {
                },
                attack = {
                    nuclear = true,
                    scorchmark = "small-scorchmark"
                },
                resistances = {},

                attackName = "nuclear-biter",
                type = "biter",
                tint = {r=0.6, g=0.6, b=0.4, a=1},
                tint2 = {r=0.95, g=0.95, b=0, a=1}
            },

            unitSpawner = {
                name = "nuclear-biter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},
                tint = {r=0.6, g=0.6, b=0.4, a=1},
                tint2 = {r=0.95, g=0.95, b=0, a=1}
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
                    [3] = 75,
                    [4] = 125,
                    [5] = 200,
                    [6] = 350,
                    [7] = 750,
                    [8] = 1500,
                    [9] = 5000,
                    [10] = 10000
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
                    [1] = 5,
                    [2] = 10,
                    [3] = 10,
                    [4] = 12,
                    [5] = 14,
                    [6] = 16,
                    [7] = 16,
                    [8] = 18,
                    [9] = 18,
                    [10] = 20
                },

                {
                    type = "attack",
                    name = "repeatCount",
                    [1] = 150,
                    [2] = 175,
                    [3] = 250,
                    [4] = 300,
                    [5] = 350,
                    [6] = 400,
                    [7] = 450,
                    [8] = 500,
                    [9] = 550,
                    [10] = 600
                },

                {
                    type = "attack",
                    name = "damage",
                    [1] = 50,
                    [2] = 60,
                    [3] = 80,
                    [4] = 100,
                    [5] = 120,
                    [6] = 130,
                    [7] = 140,
                    [8] = 150,
                    [9] = 180,
                    [10] = 200
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
                    [1] = 1,
                    [2] = 2,
                    [3] = 3,
                    [4] = 4,
                    [5] = 4,
                    [6] = 5,
                    [7] = 5,
                    [8] = 6,
                    [9] = 6,
                    [10] = 6
                },

                {
                    type = "attribute",
                    name = "unitsToSpawn",
                    [1] = 1,
                    [2] = 2,
                    [3] = 3,
                    [4] = 3,
                    [5] = 4,
                    [6] = 4,
                    [7] = 5,
                    [8] = 5,
                    [9] = 5,
                    [10] = 5
                },

                {
                    type = "majorResistances",
                    entries = {"explosion"}
                },

                {
                    type = "minorResistances",
                    entries = {"fire"}
                }
            }
        },

        function (attributes)
            return createSuicideAttack(attributes,
                                       makeAtomicBlast(attributes))
        end
    )

    -- nuclear worms
    buildWorm(
        {
            name = "nuclear-worm",

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

            attackName = "nuclear-worm",
            tint = {r=0.6, g=0.6, b=0.4, a=1},
            tint2 = {r=0.95, g=0.95, b=0, a=1}
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
                type = "attribute",
                name = "evolutionRequirement",
                formula = evolutionFunction
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
                type = "majorResistances",
                entries = {"explosion"}
            },

            {
                type = "minorResistances",
                entries = {"fire"}
            }

        },

        function (attributes)
            makeSticker(attributes)
            return createRangedAttack(attributes,
                                      createAttackBall(attributes))
        end
    )
end

return nuclear
