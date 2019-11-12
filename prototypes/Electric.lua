-- imports

local biterUtils = require("utils/BiterUtils")
local beamUtils = require("utils/BeamUtils")
local attackBall = require("utils/AttackBall")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")
local particleUtils = require("utils/ParticleUtils")

-- constants

local electric = {}

-- imported functions

local makeBloodFountains = particleUtils.makeBloodFountains
local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createElectricAttack = biterUtils.createElectricAttack
local createAttackBall = attackBall.createAttackBall
local makeLaser = beamUtils.makeLaser
local createRangedAttack = biterUtils.createRangedAttack
local makeBeam = beamUtils.makeBeam
local makeBubble = beamUtils.makeBubble

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

function electric.addFaction()

    local biterLoot = makeUnitAlienLootTable("blue")
    local spawnerLoot = makeSpawnerAlienLootTable("blue")
    local wormLoot = makeWormAlienLootTable("blue")

    local electricBubble = makeBubble({
            name = "electric-worm",
            tint = {r=0, g=0.1, b=1, a=1}
    })

    local bloodFountains = {
        type = "attribute",
        mapping = "explosion",
        [1] = "electric-blood-explosion-small-rampant",
        [2] = "electric-blood-explosion-small-rampant",
        [3] = "electric-blood-explosion-small-rampant",
        [4] = "electric-blood-explosion-small-rampant",
        [5] = "electric-blood-explosion-big-rampant",
        [6] = "electric-blood-explosion-big-rampant",
        [7] = "electric-blood-explosion-big-rampant",
        [8] = "electric-blood-explosion-huge-rampant",
        [9] = "electric-blood-explosion-huge-rampant",
        [10] = "electric-blood-explosion-huge-rampant",
    }

    makeBloodFountains({
            name = "electric",
            tint = {r=0, g=0, b=1, a=1}
    })

    -- electric biters
    buildUnitSpawner(
        {
            unit = {
                name = "electric-biter",

                attributes = {
                },
                loot = biterLoot,
                attack = {
                    damageType = "electric"
                },
                resistances = {},

                type = "biter",
                attackName = "biter-electric",
                tint = {r=0.7, g=0.7, b=1.0, a=1.0},
                tint2 = {r=0, g=0, b=1, a=1}
            },

            unitSpawner = {
                name = "electric-biter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},
                tint = {r=0.7, g=0.7, b=1.0, a=1.0},
                tint2 = {r=0, g=0, b=1, a=1}
            }
        },

        {
            unit = {
                bloodFountains,

                {
                    type = "attribute",
                    name = "health",
                    [1] = 10,
                    [2] = 50,
                    [3] = 200,
                    [4] = 350,
                    [5] = 1250,
                    [6] = 2250,
                    [7] = 3250,
                    [8] = 6500,
                    [9] = 12500,
                    [10] = 25000
                },

                {
                    type = "attack",
                    name = "width",
                    [1] = 1.5,
                    [2] = 1.5,
                    [3] = 1.6,
                    [4] = 1.6,
                    [5] = 1.7,
                    [6] = 1.7,
                    [7] = 1.8,
                    [8] = 1.8,
                    [9] = 1.9,
                    [10] = 1.9
                },

                {
                    type = "attack",
                    name = "damageInterval",
                    [1] = 20,
                    [2] = 20,
                    [3] = 21,
                    [4] = 21,
                    [5] = 22,
                    [6] = 22,
                    [7] = 23,
                    [8] = 23,
                    [9] = 24,
                    [10] = 24
                },

                {
                    type = "attack",
                    name = "duration",
                    [1] = 20,
                    [2] = 20,
                    [3] = 21,
                    [4] = 21,
                    [5] = 22,
                    [6] = 22,
                    [7] = 23,
                    [8] = 23,
                    [9] = 24,
                    [10] = 24
                },

                {
                    type = "attack",
                    name = "damage",
                    [1] = 6,
                    [2] = 10,
                    [3] = 15,
                    [4] = 20,
                    [5] = 30,
                    [6] = 45,
                    [7] = 60,
                    [8] = 75,
                    [9] = 90,
                    [10] = 150
                },

                {
                    type = "majorResistances",
                    entries = {"electric"}
                },

                {
                    type = "minorResistances",
                    entries = {"laser"}
                },

                {
                    type = "attack",
                    name = "range",
                    [1] = 11,
                    [2] = 11,
                    [3] = 12,
                    [4] = 12,
                    [5] = 13,
                    [6] = 13,
                    [7] = 14,
                    [8] = 14,
                    [9] = 15,
                    [10] = 15
                }
            },

            unitSpawner = {

                {
                    type = "majorResistances",
                    entries = {"electric"}
                },

                {
                    type = "minorResistances",
                    entries = {"laser"}
                },

                {
                    type = "attribute",
                    name = "evolutionRequirement",
                    formula = evolutionFunction
                }

            }
        },

        function (attributes)
            return createElectricAttack(attributes,
                                        makeBeam(attributes),
                                        biterattackanimation(attributes.scale, attributes.tint, attributes.tint2))
        end
    )

    -- electric worms
    buildWorm(
        {
            name = "electric-worm",

            loot = wormLoot,
            attributes = {},
            attack = {
                type = "projectile",
                bubble = electricBubble,
                damageType = "electric",
                pointEffects = function(attributes)
                    return
                        {
                            {
                                type="nested-result",
                                action = {
                                    {
                                        type = "cluster",
                                        cluster_count = attributes.clusters,
                                        distance = attributes.clusterDistance,
                                        distance_deviation = 3,
                                        action_delivery =
                                            {
                                                type = "projectile",
                                                projectile = makeLaser(attributes),
                                                duration = 20,
                                                direction_deviation = 0.6,
                                                starting_speed = attributes.startingSpeed,
                                                starting_speed_deviation = 0.3
                                            }
                                    }
                                },
                            }
                        }
                end
            },
            resistances = {},

            attackName = "worm-electric",
            tint = {r=0.7, g=0.7, b=1.0, a=1.0},
            tint2 = {r=0, g=0, b=1, a=1}
        },

        {
            bloodFountains,

            {
                type = "attack",
                name = "startingSpeed",
                [1] = 0.25,
                [2] = 0.25,
                [3] = 0.27,
                [4] = 0.27,
                [5] = 0.29,
                [6] = 0.29,
                [7] = 0.31,
                [8] = 0.31,
                [9] = 0.33,
                [10] = 0.33
            },

            {
                type = "attribute",
                name = "evolutionRequirement",
                formula = evolutionFunction
            },

            {
                type = "attack",
                name = "clusterDistance",
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
                name = "clusters",
                min = 2,
                [1] = 5,
                [2] = 5,
                [3] = 6,
                [4] = 6,
                [5] = 7,
                [6] = 7,
                [7] = 8,
                [8] = 8,
                [9] = 9,
                [10] = 9
            },

            {
                type = "majorResistances",
                entries = {"electric"}
            },

            {
                type = "minorResistances",
                entries = {"laser"}
            }
        },

        function (attributes)            
            return createRangedAttack(attributes,
                                      createAttackBall(attributes))
        end
    )
end

return electric
