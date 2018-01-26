local vanillaUpdates = {}

local biterUtils = require("BiterUtils")

function vanillaUpdates.useDumbProjectiles()
    local turrets = data.raw["turret"];

    turrets["small-worm-turret"]["attack_parameters"] = biterUtils.createStreamAttack(
	{
	    cooldown = 60,
	    range = 21,
	    min_range = 5,
	    turn_range = 1,
	    fire_penalty = 0,
	    damageModifier = 0.9,
	    scale = 0.8
	},
	"acid-ball-2-stream-rampant")

    turrets["medium-worm-turret"]["attack_parameters"] = biterUtils.createStreamAttack(
	{
	    cooldown = 60,
	    range = 25,
	    min_range = 3,
	    turn_range = 1,
	    fire_penalty = 0,
	    damageModifier = 0.87,
	    scale = 1
	},
	"acid-ball-3-stream-rampant")


    turrets["big-worm-turret"]["attack_parameters"] = biterUtils.createStreamAttack(
	{
	    cooldown = 60,
	    range = 26,
	    min_range = 3,
	    turn_range = 1,
	    fire_penalty = 0,
	    scale = 1.2
	},
	"acid-ball-4-stream-rampant")

    local units = data.raw["unit"];

    local unit = units["small-spitter"]
    unit["attack_parameters"] = biterUtils.createStreamAttack(
	{
	    cooldown = 100,
	    range = 13,
	    warmup = 30,
	    min_range = 3,
	    turn_range = 1,
	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit),
	    tint = biterUtils.findTint(unit)
	},
	"acid-ball-stream-rampant")

    unit = units["medium-spitter"]
    unit["attack_parameters"] = biterUtils.createStreamAttack(
	{
	    cooldown = 95,
	    range = 14,
	    min_range = 3,
	    warmup = 30,
	    turn_range = 1,
	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit),
	    tint = biterUtils.findTint(unit)
	},
	"acid-ball-1-stream-rampant")

    unit = units["big-spitter"]
    unit["attack_parameters"] = biterUtils.createStreamAttack(
	{
	    cooldown = 90,
	    range = 15,
	    min_range = 3,
	    warmup = 30,
	    turn_range = 1,
	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit),
	    tint = biterUtils.findTint(unit)
	},
	"acid-ball-2-stream-rampant")

    unit = units["behemoth-spitter"]
    unit["attack_parameters"] = biterUtils.createStreamAttack(
	{
	    cooldown = 90,
	    range = 16,
	    min_range = 3,
	    warmup = 30,
	    turn_range = 1,
	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit),
	    tint = biterUtils.findTint(unit)
	},
	"acid-ball-3-stream-rampant")
end

return vanillaUpdates
