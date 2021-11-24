local vanillaUpdates = {}

local biterUtils = require("BiterUtils")

function vanillaUpdates.useDumbProjectiles()
    local turrets = data.raw["turret"];

    local attackType = "projectile"

    turrets["small-worm-turret"]["attack_parameters"] = biterUtils.createRangedAttack(
        {
            cooldown = 60,
            range = 25,
            min_range = 5,
            turn_range = 1,
            attackType = "projectile",
            fire_penalty = 0,
            damageModifier = 0.9,
            effectiveLevel = 1,
            scale = 0.8
        },
        "acid-ball-2-" .. attackType .. "-rampant")

    turrets["medium-worm-turret"]["attack_parameters"] = biterUtils.createRangedAttack(
        {
            cooldown = 60,
            range = 30,
            min_range = 3,
            turn_range = 1,
            attackType = "projectile",
            fire_penalty = 0,
            damageModifier = 0.87,
            effectiveLevel = 3,
            scale = 1
        },
        "acid-ball-3-" .. attackType .. "-rampant")


    turrets["big-worm-turret"]["attack_parameters"] = biterUtils.createRangedAttack(
        {
            cooldown = 60,
            range = 38,
            min_range = 3,
            attackType = "projectile",
            turn_range = 1,
            fire_penalty = 0,
            effectiveLevel = 5,
            scale = 1.2
        },
        "acid-ball-4-" .. attackType .. "-rampant")

    turrets["behemoth-worm-turret"]["attack_parameters"] = biterUtils.createRangedAttack(
        {
            cooldown = 60,
            range = 48,
            min_range = 3,
            attackType = "projectile",
            turn_range = 1,
            fire_penalty = 0,
            effectiveLevel = 7,
            scale = 1.5
        },
        "acid-ball-5-" .. attackType .. "-rampant")

    local units = data.raw["unit"];

    local unit = units["small-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
        {
            cooldown = 100,
            range = 13,
            warmup = 30,
            min_range = 3,
            turn_range = 1,
            attackType = "projectile",
            effectiveLevel = 1,
            fire_penalty = 15,
            scale = biterUtils.findRunScale(unit)
        },
        "acid-ball-" .. attackType .. "-rampant",
        spitterattackanimation(biterUtils.findRunScale(unit),
                               tint_1_spitter_small,
                               tint_2_spitter_small))

    unit = units["medium-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
        {
            cooldown = 95,
            range = 14,
            min_range = 3,
            attackType = "projectile",
            warmup = 30,
            turn_range = 1,
            effectiveLevel = 3,
            fire_penalty = 15,
            scale = biterUtils.findRunScale(unit)
        },
        "acid-ball-1-" .. attackType .. "-rampant",
        spitterattackanimation(biterUtils.findRunScale(unit),
                               tint_1_spitter_medium,
                               tint_2_spitter_medium))

    unit = units["big-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
        {
            cooldown = 90,
            range = 15,
            min_range = 3,
            attackType = "projectile",
            warmup = 30,
            turn_range = 1,
            effectiveLevel = 5,
            fire_penalty = 15,
            scale = biterUtils.findRunScale(unit)
        },
        "acid-ball-2-direction-" .. attackType .. "-rampant",
        spitterattackanimation(biterUtils.findRunScale(unit),
                               tint_1_spitter_big,
                               tint_2_spitter_big))

    unit = units["behemoth-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
        {
            cooldown = 90,
            range = 16,
            min_range = 3,
            warmup = 30,
            attackType = "projectile",
            turn_range = 1,
            effectiveLevel = 7,
            fire_penalty = 15,
            scale = biterUtils.findRunScale(unit)
        },
        "acid-ball-3-direction-" .. attackType .. "-rampant",
        spitterattackanimation(biterUtils.findRunScale(unit),
                               tint_1_spitter_behemoth,
                               tint_2_spitter_behemoth))

    unit = units["small-biter"]
    unit["attack_parameters"]["ammo_type"]["action"] = {
        {
            type = "area",
            radius = 0.2,
            force = "enemy",
            ignore_collision_condition = true,
            action_delivery =
                {
                    type = "instant",
                    target_effects =
                        {
                            type = "damage",
                            damage = { amount = 7 * 0.75, type = "physical" }
                        }
                }
        },
        {
            type = "direct",
            action_delivery =
                {
                    type = "instant",
                    target_effects =
                        {
                            type = "damage",
                            damage = { amount = 7 * 0.25, type = "physical" }
                        }
                }
        }
    }

    unit = units["medium-biter"]
    unit["attack_parameters"]["ammo_type"]["action"] = {
        {
            type = "area",
            radius = 0.6,
            force = "enemy",
            ignore_collision_condition = true,
            action_delivery =
                {
                    type = "instant",
                    target_effects =
                        {
                            type = "damage",
                            damage = { amount = 15 * 0.75, type = "physical" }
                        }
                }
        },
        {
            type = "direct",
            action_delivery =
                {
                    type = "instant",
                    target_effects =
                        {
                            type = "damage",
                            damage = { amount = 15 * 0.25, type = "physical" }
                        }
                }
        }
    }

    unit = units["big-biter"]
    unit["attack_parameters"]["ammo_type"]["action"] = {
        {
            type = "area",
            radius = 0.9,
            force = "enemy",
            ignore_collision_condition = true,
            action_delivery =
                {
                    type = "instant",
                    target_effects =
                        {
                            type = "damage",
                            damage = { amount = 30 * 0.75, type = "physical" }
                        }
                }
        },
        {
            type = "direct",
            action_delivery =
                {
                    type = "instant",
                    target_effects =
                        {
                            type = "damage",
                            damage = { amount = 30 * 0.25, type = "physical" }
                        }
                }
        }
    }

    unit = units["behemoth-biter"]
    unit["attack_parameters"]["ammo_type"]["action"] = {
        {
            type = "area",
            radius = 1.2,
            force = "enemy",
            ignore_collision_condition = true,
            action_delivery =
                {
                    type = "instant",
                    target_effects =
                        {
                            type = "damage",
                            damage = { amount = 90 * 0.75, type = "physical" }
                        }
                }
        },
        {
            type = "direct",
            action_delivery =
                {
                    type = "instant",
                    target_effects =
                        {
                            type = "damage",
                            damage = { amount = 90 * 0.25, type = "physical" }
                        }
                }
        }
    }
end

return vanillaUpdates
