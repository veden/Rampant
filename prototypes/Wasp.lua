-- imports

local attackBall = require("utils/AttackBall")
local streamUtils = require("utils/StreamUtils")
local droneUtils = require("utils/DroneUtils")
local biterUtils = require("utils/BiterUtils")
local swarmUtils = require("SwarmUtils")
local constants = require("__Rampant__/libs/Constants")

-- constants

local wasp = {}

local WASP_UNIT_TIERS = constants.WASP_UNIT_TIERS
local WASP_UNIT_VARIATIONS = constants.WASP_UNIT_VARIATIONS

local WASP_NEST_TIERS = constants.WASP_NEST_TIERS
local WASP_NEST_VARIATIONS = constants.WASP_NEST_VARIATIONS

local WASP_WORM_TIERS = constants.WASP_WORM_TIERS
local WASP_WORM_VARIATIONS = constants.WASP_WORM_VARIATIONS

-- imported functions

local buildUnitSpawner = swarmUtils.buildUnitSpawner
local buildWorm = swarmUtils.buildWorm
local buildUnits = swarmUtils.buildUnits
local createAttackBall = attackBall.createAttackBall
local makeAcidStream = streamUtils.makeAcidStream
local createProjectileAttack = biterUtils.createProjectileAttack

local biterAttackSounds = biterUtils.biterAttackSounds

local createCapsuleProjectile = droneUtils.createCapsuleProjectile

local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable
local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable

function wasp.addFaction()

    local biterLoot = makeUnitAlienLootTable("purple")
    local spawnerLoot = makeSpawnerAlienLootTable("purple")
    local wormLoot = makeWormAlienLootTable("purple")

    -- wasp
    buildUnits(
        {
            name = "wasp-drone",

            attributes = {
                followsPlayer = true
            },
            attack = {

            },
            death = function (attributes)
                return {
                    type = "direct",
                    action_delivery =
                        {
                            type = "instant",
                            target_effects =
                                {
                                    type = "create-entity",
                                    entity_name = "acid-splash-purple"
                                }
                        }
                }
            end,
            scales = {
                [1] = 0.5,
                [2] = 0.5,
                [3] = 0.6,
                [4] = 0.6,
                [5] = 0.7,
                [6] = 0.7,
                [7] = 0.8,
                [8] = 0.8,
                [9] = 0.9,
                [10] = 0.9
            },
            resistances = {},

            type = "drone",
            attackName = "wasp-drone",
            tint = {r=1, g=1, b=0, a=1}           
        },
        function (attack, attributes, tier)
            return createProjectileAttack(attack,
                                          createAttackBall(attack))
        end,
        {
            {		
                type = "attribute",
                name = "health",
                [1] = 10,
                [2] = 15,
                [3] = 20,
                [4] = 25,
                [5] = 30,
                [6] = 35,
                [7] = 40,
                [8] = 45,
                [9] = 50,
                [10] = 55
            },

            {		
                type = "attack",
                name = "cooldown",
                [1] = 60,
                [2] = 60,
                [3] = 55,
                [4] = 55,
                [5] = 50,
                [6] = 50,
                [7] = 45,
                [8] = 45,
                [9] = 40,
                [10] = 40
            },

            {		
                type = "attribute",
                name = "ttl",
                [1] = 300,
                [2] = 300,
                [3] = 350,
                [4] = 350,
                [5] = 400,
                [6] = 400,
                [7] = 450,
                [8] = 450,
                [9] = 500,
                [10] = 500
            },
            
            {		
                type = "attack",
                name = "damage",
                [1] = 2,
                [2] = 4,
                [3] = 7,
                [4] = 13,
                [5] = 15,
                [6] = 18,
                [7] = 22,
                [8] = 28,
                [9] = 35,
                [10] = 40
            },
            
            {
                type = "attribute",
                name = "movement",
                [1] = 0.03,
                [2] = 0.03,
                [3] = 0.04,
                [4] = 0.04,
                [5] = 0.05,
                [6] = 0.05,
                [7] = 0.06,
                [8] = 0.06,
                [9] = 0.07,
                [10] = 0.07
            },
            {
                type = "attribute",
                name = "distancePerFrame",
                [1] = 0.013,
                [2] = 0.013,
                [3] = 0.014,
                [4] = 0.014,
                [5] = 0.015,
                [6] = 0.015,
                [7] = 0.016,
                [8] = 0.016,
                [9] = 0.017,
                [10] = 0.017
            },

            {
                type = "attack",
                name = "rangeFromPlayer",
                [1] = 24,
                [2] = 24,
                [3] = 25,
                [4] = 25,
                [5] = 26,
                [6] = 26,
                [7] = 27,
                [8] = 27,
                [9] = 28,
                [10] = 28
            },
            
            {
                type = "attack",
                name = "range",
                [1] = 10,
                [2] = 10,
                [3] = 11,
                [4] = 11,
                [5] = 12,
                [6] = 12,
                [7] = 13,
                [8] = 13,
                [9] = 14,
                [10] = 14
            },

            {
                type = "attack",
                name = "radius",
                [1] = 1.2,
                [2] = 1.3,
                [3] = 1.4,
                [4] = 1.5,
                [5] = 1.6,
                [6] = 1.7,
                [7] = 1.8,
                [8] = 1.9,
                [9] = 2.0,
                [10] = 2.5
            }

        },
        WASP_UNIT_VARIATIONS,
        WASP_UNIT_TIERS
    )

    buildUnits(
        {
            name = "wasp-worm-drone",

            attributes = {	    
            },
            attack = {

            },
            death = function (attributes)
                return {
                    type = "direct",
                    action_delivery =
                        {
                            type = "instant",
                            target_effects =
                                {
                                    type = "create-entity",
                                    entity_name = "acid-splash-purple"
                                }
                        }
                }
            end,
            scales = {
                [1] = 0.5,
                [2] = 0.5,
                [3] = 0.6,
                [4] = 0.6,
                [5] = 0.7,
                [6] = 0.7,
                [7] = 0.8,
                [8] = 0.8,
                [9] = 0.9,
                [10] = 0.9
            },
            resistances = {},

            type = "drone",
            attackName = "wasp-worm-drone",
            tint = {r=1, g=1, b=0, a=1}
        },
        function (attack)
            return {
                type = "projectile",
                ammo_category = "biological",
                cooldown = attack.cooldown or 20,
                projectile_center = {0, 1},
                projectile_creation_distance = 0.6,
                range = attack.range or 15,
                sound = biterAttackSounds(),
                ammo_type =
                    {
                        category = "biological",
                        action =
                            {
                                type = "direct",
                                action_delivery =
                                    {
                                        type = "stream",
                                        stream = makeAcidStream(attack),
                                        duration = 160
                                    }
                            }
                    }
            }
        end,
        {
            {		
                type = "attribute",
                name = "health",
                [1] = 10,
                [2] = 15,
                [3] = 20,
                [4] = 25,
                [5] = 30,
                [6] = 35,
                [7] = 40,
                [8] = 45,
                [9] = 50,
                [10] = 55
            },
            
            {		
                type = "attack",
                name = "cooldown",
                [1] = 60,
                [2] = 60,
                [3] = 55,
                [4] = 55,
                [5] = 50,
                [6] = 50,
                [7] = 45,
                [8] = 45,
                [9] = 40,
                [10] = 40
            },

            {		
                type = "attribute",
                name = "ttl",
                [1] = 300,
                [2] = 300,
                [3] = 350,
                [4] = 350,
                [5] = 400,
                [6] = 400,
                [7] = 450,
                [8] = 450,
                [9] = 500,
                [10] = 500
            },
            
            {		
                type = "attack",
                name = "damage",
                [1] = 2,
                [2] = 4,
                [3] = 7,
                [4] = 13,
                [5] = 15,
                [6] = 18,
                [7] = 22,
                [8] = 28,
                [9] = 35,
                [10] = 40
            },

            {
                type = "attack",
                name = "rangeFromPlayer",
                [1] = 10,
                [2] = 10,
                [3] = 11,
                [4] = 11,
                [5] = 12,
                [6] = 12,
                [7] = 13,
                [8] = 13,
                [9] = 14,
                [10] = 14
            },
            
            {
                type = "attribute",
                name = "movement",
                [1] = 0.01,
                [2] = 0.01,
                [3] = 0.02,
                [4] = 0.02,
                [5] = 0.03,
                [6] = 0.03,
                [7] = 0.04,
                [8] = 0.04,
                [9] = 0.05,
                [10] = 0.05
            },
            {
                type = "attribute",
                name = "distancePerFrame",
                [1] = 0.013,
                [2] = 0.013,
                [3] = 0.014,
                [4] = 0.014,
                [5] = 0.015,
                [6] = 0.015,
                [7] = 0.016,
                [8] = 0.016,
                [9] = 0.017,
                [10] = 0.017
            },

            {
                type = "attack",
                name = "range",
                [1] = 10,
                [2] = 10,
                [3] = 11,
                [4] = 11,
                [5] = 12,
                [6] = 12,
                [7] = 13,
                [8] = 13,
                [9] = 14,
                [10] = 14
            },

            {
                type = "attack",
                name = "radius",
                [1] = 1.2,
                [2] = 1.3,
                [3] = 1.4,
                [4] = 1.5,
                [5] = 1.6,
                [6] = 1.7,
                [7] = 1.8,
                [8] = 1.9,
                [9] = 2.0,
                [10] = 2.5
            }
            
        },
        WASP_WORM_VARIATIONS,
        WASP_WORM_TIERS
    )


    -- wasp spitters
    buildUnitSpawner(
        {
            unit = {
                name = "wasp-spitter",

                loot = biterLoot,
                attributes = {
                    explosion = "blood-explosion-small"
                },
                attack = {
                    type = "projectile",
                    directionOnly = true,
                    collisionBox = {{0,0}, {0,0}}
                },
                resistances = {},

                type = "spitter",
                attackName = "wasp-drone",
                tint = {r=1, g=1, b=0, a=1}
            },

            unitSpawner = {
                name = "wasp-spitter-spawner",

                loot = spawnerLoot,
                attributes = {},
                resistances = {},

                tint = {r=1, g=1, b=0, a=1}
            }
        },

        {
            unit = {	   
                
            },

            unitSpawner = {
                {
                    type = "attribute",
                    name = "evolutionRequirement",
                    [1] = 0,
                    [2] = 0.15,
                    [3] = 0.25,
                    [4] = 0.35,
                    [5] = 0.45,
                    [6] = 0.55,
                    [7] = 0.65,
                    [8] = 0.70,
                    [9] = 0.75,
                    [10] = 0.95
                }
            }
        },

        function (attributes)
            return createProjectileAttack(attributes,
                                          createCapsuleProjectile(attributes.name,
                                                                  attributes,
                                                                  attributes.name .. "-drone-rampant"),
                                          spitterattackanimation(attributes.scale, attributes.tint, attributes.tint))
        end,

        {
            unit = WASP_UNIT_VARIATIONS,
            unitSpawner = WASP_NEST_VARIATIONS
        },

        {
            unit = WASP_UNIT_TIERS,
            unitSpawner = WASP_NEST_TIERS
        }
    )

    -- wasp worms
    buildWorm(
        {
            name = "wasp-worm",

            loot = wormLoot,	    
            attributes = {
            },
            attack = {
                type = "projectile",
                collisionBox = {{0,0}, {0,0}}
            },
            resistances = {},

            attackName = "wasp-worm-drone",
            tint = {r=1, g=1, b=0, a=1}
        },

        {	
            {
                type = "attribute",
                name = "evolutionRequirement",
                [1] = 0,
                [2] = 0.15,
                [3] = 0.25,
                [4] = 0.35,
                [5] = 0.45,
                [6] = 0.55,
                [7] = 0.65,
                [8] = 0.70,
                [9] = 0.75,
                [10] = 0.95
            }
        },

        function (attributes)
            return createProjectileAttack(attributes,
                                          createCapsuleProjectile(attributes.name,
                                                                  attributes,
                                                                  attributes.name .. "-drone-rampant"))
        end,

        WASP_WORM_VARIATIONS,
        WASP_WORM_TIERS
    )
end

return wasp
