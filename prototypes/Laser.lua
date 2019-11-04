-- imports

local acidBall = require("utils/AttackBall")
local beamUtils = require("utils/BeamUtils")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")
local particleUtils = require("utils/ParticleUtils")

-- constants

-- imported functions

local laser = {}

local makeBloodFountains = particleUtils.makeBloodFountains
local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local createAttackBall = acidBall.createAttackBall
local makeLaser = beamUtils.makeLaser
local createRangedAttack = biterUtils.createRangedAttack
local makeBubble = beamUtils.makeBubble

local createMeleeAttack = biterUtils.createMeleeAttack

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

local biterLoot = makeUnitAlienLootTable("blue")
local spawnerLoot = makeSpawnerAlienLootTable("blue")
local wormLoot = makeWormAlienLootTable("blue")

local function evolutionFunction(tier)
    if (tier == 0) then
        return 0
    else
        return 0.12 + ((tier - 2) * 0.10)
    end
end

function laser.addFaction()

    local laserBubble = makeBubble({
            name = "laser-worm",
            tint = {r=0, g=0, b=0.42, a=0.65}
    })


    local bloodFountains = {
        type = "attribute",
        mapping = "explosion",
        [1] = "laser-blood-explosion-small-rampant",
        [2] = "laser-blood-explosion-small-rampant",
        [3] = "laser-blood-explosion-small-rampant",
        [4] = "laser-blood-explosion-small-rampant",
        [5] = "laser-blood-explosion-big-rampant",
        [6] = "laser-blood-explosion-big-rampant",
        [7] = "laser-blood-explosion-big-rampant",
        [8] = "laser-blood-explosion-huge-rampant",
        [9] = "laser-blood-explosion-huge-rampant",
        [10] = "laser-blood-explosion-huge-rampant",
    }

    makeBloodFountains({
            name = "laser",
            tint = {r=0, g=0, b=0.42, a=1}
    })


    -- laser biters
    buildUnitSpawner(
        {
            unit = {
                name = "laser-biter",

                attributes = {
                    damageType = "laser",
                    explosion = "blood-explosion-small"
                },
                attack = {},
                resistances = {},

                loot = biterLoot,
                type = "biter",
                tint = {r=0.3, g=0.3, b=0.42, a=1},
                tint2 = {r=0, g=0.6, b=0.8, a=1}
            },

            unitSpawner = {
                name = "laser-biter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},
                tint = {r=0.3, g=0.3, b=0.42, a=1},
                tint2 = {r=0, g=0.6, b=0.8, a=1}
            }
        },

        {
            unit = {

                bloodFountains,

                {
                    type = "majorResistances",
                    entries = {"laser"}
                },

                {
                    type = "minorResistances",
                    entries = {"electric"}
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
                    entries = {"laser"}
                },

                {
                    type = "minorResistances",
                    entries = {"electric"}
                }
            }
        },

        createMeleeAttack
    )

    -- laser spitters
    buildUnitSpawner(
        {
            unit = {
                name = "laser-spitter",

                loot = biterLoot,
                attributes = {
                    damageType = "laser"
                },
                attack = {
                    type = "projectile",
                    bubble = laserBubble,
                    damageType = "laser",
                    directionOnly = true,
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
                                                    projectile = attributes.laserName,
                                                    duration = 20,
                                                    direction_deviation = 0.6,
                                                    starting_speed = attributes.startingSpeed,
                                                    starting_speed_deviation = 0.3
                                                },
                                            repeat_count = 2
                                        }
                                    }
                                }
                            }
                    end
                },
                resistances = {},

                type = "spitter",
                attackName = "laser-spitter",
                tint = {r=0.3, g=0.3, b=0.42, a=1},
                tint2 = {r=0, g=0.6, b=0.8, a=1}
            },

            unitSpawner = {
                name = "laser-spitter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},

                tint = {r=0.3, g=0.3, b=0.42, a=1},
                tint2 = {r=0, g=0.6, b=0.8, a=1}
            }
        },

        {
            unit = {

                bloodFountains,

                {
                    type = "majorResistances",
                    entries = {"laser"}
                },

                {
                    type = "minorResistances",
                    entries = {"electric"}
                },

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
                    [1] = 2,
                    [2] = 3,
                    [3] = 3,
                    [4] = 4,
                    [5] = 4,
                    [6] = 5,
                    [7] = 5,
                    [8] = 5,
                    [9] = 6,
                    [10] = 6
                }

            },

            unitSpawner = {

                bloodFountains,

                {
                    type = "majorResistances",
                    entries = {"laser"}
                },

                {
                    type = "minorResistances",
                    entries = {"electric"}
                }
            }
        },

        function (attributes)
            attributes.laserName = makeLaser(attributes)
            return createRangedAttack(attributes,
                                      createAttackBall(attributes),
                                      spitterattackanimation(attributes.scale,
                                                             attributes.tint,
                                                             attributes.tint2))
        end
    )

    -- laser worms
    buildWorm(
        {
            name = "laser-worm",

            loot = wormLoot,
            attributes = {},
            attack = {
                type = "projectile",
                bubble = laserBubble,
                damageType = "laser",
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
                                                projectile = attributes.laserName,
                                                duration = 20,
                                                direction_deviation = 0.6,
                                                starting_speed = attributes.startingSpeed,
                                                starting_speed_deviation = 0.3
                                            },
                                        repeat_count = 3
                                    }
                                }
                            }
                        }
                end
            },
            resistances = {},

            attackName = "laser-worm",
            tint = {r=0.3, g=0.3, b=0.42, a=1},
            tint2 = {r=0, g=0.6, b=0.8, a=1}
        },

        {

            bloodFountains,

            {
                type = "majorResistances",
                entries = {"laser"}
            },

            {
                type = "minorResistances",
                entries = {"electric"}
            },

            {
                type = "attribute",
                name = "evolutionRequirement",
                formula = evolutionFunction
            },

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
            }

        },

        function (attributes)
            attributes.laserName = makeLaser(attributes)
            return createRangedAttack(attributes,
                                      createAttackBall(attributes))
        end
    )
end

return laser
