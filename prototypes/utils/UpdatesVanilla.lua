local vanillaUpdates = {}

local FORCE_OLD_PROJECTILES = settings.startup["rampant-forceOldProjectiles"].value

local biterUtils = require("BiterUtils")

function vanillaUpdates.useDumbProjectiles()
    local turrets = data.raw["turret"];

    local attackType = (FORCE_OLD_PROJECTILES and "stream") or "projectile"
    local unitPrefix = (FORCE_OLD_PROJECTILES and "") or "direction-"
    
    turrets["small-worm-turret"]["attack_parameters"] = biterUtils.createRangedAttack(
	{
	    cooldown = 60,
	    range = 21,
	    min_range = 5,
	    turn_range = 1,
	    type = "projectile",
	    fire_penalty = 0,
	    damageModifier = 0.9,
	    scale = 0.8
	},
	"acid-ball-2-" .. attackType .. "-rampant")

    turrets["medium-worm-turret"]["attack_parameters"] = biterUtils.createRangedAttack(
	{
	    cooldown = 60,
	    range = 25,
	    min_range = 3,
	    turn_range = 1,
	    type = "projectile",
	    fire_penalty = 0,
	    damageModifier = 0.87,
	    scale = 1
	},
	"acid-ball-3-" .. attackType .. "-rampant")


    turrets["big-worm-turret"]["attack_parameters"] = biterUtils.createRangedAttack(
	{
	    cooldown = 60,
	    range = 26,
	    min_range = 3,
	    type = "projectile",
	    turn_range = 1,
	    fire_penalty = 0,
	    scale = 1.2
	},
	"acid-ball-4-" .. attackType .. "-rampant")

    local units = data.raw["unit"];

    local unit = units["small-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
	{
	    cooldown = 100,
	    range = 13,
	    warmup = 30,
	    min_range = 3,
	    turn_range = 1,
	    type = "projectile",
	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit)
	},
	"acid-ball-" .. unitPrefix .. attackType .. "-rampant",
	spitterattackanimation(biterUtils.findRunScale(unit),
			       biterUtils.findTint(unit)))

    unit = units["medium-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
	{
	    cooldown = 95,
	    range = 14,
	    min_range = 3,
	    type = "projectile",
	    warmup = 30,
	    turn_range = 1,
	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit)
	},
	"acid-ball-1-" .. unitPrefix .. attackType .. "-rampant",
	spitterattackanimation(biterUtils.findRunScale(unit),
			       biterUtils.findTint(unit)))

    unit = units["big-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
	{
	    cooldown = 90,
	    range = 15,
	    min_range = 3,
	    type = "projectile",
	    warmup = 30,
	    turn_range = 1,
	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit)
	},
	"acid-ball-2-" .. unitPrefix .. attackType .. "-rampant",
	spitterattackanimation(biterUtils.findRunScale(unit),
			       biterUtils.findTint(unit)))

    unit = units["behemoth-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
	{
	    cooldown = 90,
	    range = 16,
	    min_range = 3,
	    warmup = 30,
	    type = "projectile",
	    turn_range = 1,
	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit)
	},
	"acid-ball-3-" .. unitPrefix .. attackType .. "-rampant",
	spitterattackanimation(biterUtils.findRunScale(unit),
			       biterUtils.findTint(unit)))
end

return vanillaUpdates
