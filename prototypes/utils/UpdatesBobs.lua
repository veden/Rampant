local bobsUpdates = {}

local FORCE_OLD_PROJECTILES = settings.startup["rampant-forceOldProjectiles"].value

local biterUtils = require("BiterUtils")

function bobsUpdates.useDumbProjectiles()
    local turrets = data.raw["turret"];

    local attackType = (FORCE_OLD_PROJECTILES and "stream") or "projectile"
    local unitPrefix = (FORCE_OLD_PROJECTILES and "") or "direction-"
    
    turrets["bob-big-explosive-worm-turret"]["attack_parameters"] = biterUtils.createRangedAttack(
    	{
    	    cooldown = 60,
    	    range = 26,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
	    type = "projectile",
    	    scale = 1.2
    	},
    	"bob-explosive-ball-" .. attackType .. "-rampant")

    turrets["bob-big-fire-worm-turret"]["attack_parameters"] = biterUtils.createRangedAttack(
    	{
    	    cooldown = 60,
    	    range = 26,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
	    type = "projectile",
    	    scale = 1.2
    	},
    	"bob-fire-ball-" .. attackType .. "-rampant")

    turrets["bob-big-poison-worm-turret"]["attack_parameters"] = biterUtils.createRangedAttack(
    	{
    	    cooldown = 60,
    	    range = 26,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
	    type = "projectile",
    	    scale = 1.2
    	},
    	"bob-poison-ball-" .. attackType .. "-rampant")

    turrets["bob-big-piercing-worm-turret"]["attack_parameters"] = biterUtils.createRangedAttack(
    	{
    	    cooldown = 60,
    	    range = 26,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
	    type = "projectile",
    	    scale = 1.2
    	},
    	"bob-piercing-ball-" .. attackType .. "-rampant")
    
    turrets["bob-big-electric-worm-turret"]["attack_parameters"] = biterUtils.createRangedAttack(
    	{
    	    cooldown = 60,
    	    range = 26,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
	    type = "projectile",
    	    scale = 1.2
    	},
    	"bob-electric-ball-" .. attackType .. "-rampant")

    turrets["bob-giant-worm-turret"]["attack_parameters"] = biterUtils.createRangedAttack(
    	{
    	    cooldown = 60,
    	    range = 28,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
	    type = "projectile",
    	    scale = 1.6
    	},
    	"acid-ball-5-" .. attackType .. "-rampant")

    local units = data.raw["unit"]

    local unit = units["behemoth-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
    	{
    	    cooldown = 90,
    	    range = 16,
    	    min_range = 3,
    	    turn_range = 1,
	    warmup = 30,
	    type = "projectile",
    	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit)
    	},
    	"acid-ball-3-" .. unitPrefix .. attackType .. "-rampant",
	spitterattackanimation(biterUtils.findRunScale(unit),
			       biterUtils.findTint(unit)))

    unit = units["bob-big-electric-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
    	{
    	    cooldown = 90,
    	    range = 15,
    	    min_range = 3,
    	    turn_range = 1,
	    damageModifier = 0.6,
	    type = "projectile",
	    warmup = 30,
    	    fire_penalty = 0,
	    scale = biterUtils.findRunScale(unit)
    	},
    	"bob-electric-ball-" .. unitPrefix .. attackType .. "-rampant",
	spitterattackanimation(biterUtils.findRunScale(unit),
			       biterUtils.findTint(unit)))

    unit = units["bob-huge-explosive-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
    	{
    	    cooldown = 90,
    	    range = 16,
    	    min_range = 3,
	    type = "projectile",
	    warmup = 30,
    	    turn_range = 1,
	    damageModifier = 0.8,
    	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit)
    	},
    	"bob-explosive-ball-" .. unitPrefix .. attackType .. "-rampant",
	spitterattackanimation(biterUtils.findRunScale(unit),
			       biterUtils.findTint(unit)))

    unit = units["bob-huge-acid-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
	{
	    cooldown = 90,
	    range = 16,
	    min_range = 3,
	    type = "projectile",
	    turn_range = 1,
	    warmup = 30,
	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit)
	},
	"wide-acid-ball-" .. unitPrefix .. attackType .. "-rampant",
	spitterattackanimation(biterUtils.findRunScale(unit),
			       biterUtils.findTint(unit)))

    unit = units["bob-giant-fire-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
	{
	    cooldown = 90,
	    range = 16,
	    type = "projectile",
	    min_range = 3,
	    turn_range = 1,
	    warmup = 30,
	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit)
	},
	"bob-fire-ball-" .. unitPrefix .. attackType .. "-rampant",
	spitterattackanimation(biterUtils.findRunScale(unit),
			       biterUtils.findTint(unit)))

    unit = units["bob-giant-poison-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
	{
	    cooldown = 90,
	    range = 16,
	    type = "projectile",
	    min_range = 3,
	    turn_range = 1,
	    warmup = 30,
	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit)
	},
	"bob-poison-ball-" .. unitPrefix .. attackType .. "-rampant",
	spitterattackanimation(biterUtils.findRunScale(unit),
			       biterUtils.findTint(unit)))

    unit = units["bob-titan-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
	{
	    cooldown = 90,
	    range = 16,
	    type = "projectile",
	    min_range = 3,
	    turn_range = 1,
	    warmup = 30,
	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit)
	},
	"bob-titan-ball-" .. unitPrefix .. attackType .. "-rampant",
	spitterattackanimation(biterUtils.findRunScale(unit),
			       biterUtils.findTint(unit)))

    unit = units["bob-behemoth-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
	{
	    cooldown = 90,
	    range = 16,
	    type = "projectile",
	    min_range = 3,
	    turn_range = 1,
	    warmup = 30,
	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit)
	},
	"bob-behemoth-ball-" .. unitPrefix .. attackType .. "-rampant",
	spitterattackanimation(biterUtils.findRunScale(unit),
			       biterUtils.findTint(unit)))


    unit = units["bob-leviathan-spitter"]
    unit["attack_parameters"] = biterUtils.createRangedAttack(
	{
	    cooldown = 90,
	    type = "projectile",
	    range = 17,
	    min_range = 3,
	    warmup = 30,
	    turn_range = 1,
	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit)
	},
	"bob-leviathan-ball-" .. unitPrefix .. attackType .. "-rampant",
	spitterattackanimation(biterUtils.findRunScale(unit),
			       biterUtils.findTint(unit)))
end

return bobsUpdates
