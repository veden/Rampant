local bobsUpdates = {}

local biterUtils = require("BiterUtils")

function bobsUpdates.useDumbProjectiles()
    local turrets = data.raw["turret"];

    turrets["bob-big-explosive-worm-turret"]["attack_parameters"] = biterUtils.createFireAttack(
    	{
    	    cooldown = 80,
    	    range = 25,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
    	    scale = 1.2
    	},
    	"bob-explosive-ball-1-stream-rampant")

    turrets["bob-big-fire-worm-turret"]["attack_parameters"] = biterUtils.createFireAttack(
    	{
    	    cooldown = 80,
    	    range = 25,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
    	    scale = 1.2
    	},
    	"bob-fire-ball-stream-rampant")

    turrets["bob-big-poison-worm-turret"]["attack_parameters"] = biterUtils.createFireAttack(
    	{
    	    cooldown = 80,
    	    range = 25,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
    	    scale = 1.2
    	},
    	"bob-poison-ball-1-stream-rampant")

    turrets["bob-big-piercing-worm-turret"]["attack_parameters"] = biterUtils.createFireAttack(
    	{
    	    cooldown = 80,
    	    range = 25,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
    	    scale = 1.2
    	},
    	"bob-piercing-ball-stream-rampant")
    
    turrets["bob-big-electric-worm-turret"]["attack_parameters"] = biterUtils.createFireAttack(
    	{
    	    cooldown = 80,
    	    range = 25,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
    	    scale = 1.2
    	},
    	"bob-electric-ball-1-stream-rampant")

    turrets["bob-giant-worm-turret"]["attack_parameters"] = biterUtils.createFireAttack(
    	{
    	    cooldown = 80,
    	    range = 28,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
    	    scale = 1.6
    	},
    	"acid-ball-4-stream-rampant")

    turrets["bob-behemoth-worm-turret"]["attack_parameters"] = biterUtils.createFireAttack(
    	{
    	    cooldown = 80,
    	    range = 30,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
    	    scale = 2
    	},
    	"acid-ball-5-stream-rampant")

    local units = data.raw["unit"]

    local unit = units["behemoth-spitter"]
    unit["attack_parameters"] = biterUtils.createFireAttack(
    	{
    	    cooldown = 80,
    	    range = 15,
    	    min_range = 3,
    	    turn_range = 1,
	    warmup = 30,
    	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit),
	    tint1 = biterUtils.findTint(unit),
	    tint2 = biterUtils.findTint(unit)
    	},
    	"acid-ball-3-stream-rampant")

    unit = units["bob-big-electric-spitter"]
    unit["attack_parameters"] = biterUtils.createFireAttack(
    	{
    	    cooldown = 80,
    	    range = 15,
    	    min_range = 3,
    	    turn_range = 1,
	    warmup = 30,
    	    fire_penalty = 0,
	    scale = biterUtils.findRunScale(unit),
	    tint1 = biterUtils.findTint(unit),
	    tint2 = biterUtils.findTint(unit)
    	},
    	"bob-electric-ball-stream-rampant")

    unit = units["bob-huge-explosive-spitter"]
    unit["attack_parameters"] = biterUtils.createFireAttack(
    	{
    	    cooldown = 80,
    	    range = 15,
    	    min_range = 3,
	    warmup = 30,
    	    turn_range = 1,
    	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit),
	    tint1 = biterUtils.findTint(unit),
	    tint2 = biterUtils.findTint(unit)
    	},
    	"bob-explosive-ball-stream-rampant")

    unit = units["bob-huge-acid-spitter"]
    unit["attack_parameters"] = biterUtils.createFireAttack(
    	{
    	    cooldown = 80,
    	    range = 15,
    	    min_range = 3,
    	    turn_range = 1,
	    warmup = 30,
    	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit),
	    tint1 = biterUtils.findTint(unit),
	    tint2 = biterUtils.findTint(unit)
    	},
    	"wide-acid-ball-stream-rampant")

    unit = units["bob-giant-fire-spitter"]
    unit["attack_parameters"] = biterUtils.createFireAttack(
    	{
    	    cooldown = 80,
    	    range = 15,
    	    min_range = 3,
    	    turn_range = 1,
	    warmup = 30,
    	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit),
	    tint1 = biterUtils.findTint(unit),
	    tint2 = biterUtils.findTint(unit)
    	},
    	"bob-fire-ball-stream-rampant")

    unit = units["bob-giant-poison-spitter"]
    unit["attack_parameters"] = biterUtils.createFireAttack(
    	{
    	    cooldown = 80,
    	    range = 15,
    	    min_range = 3,
    	    turn_range = 1,
	    warmup = 30,
    	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit),
	    tint1 = biterUtils.findTint(unit),
	    tint2 = biterUtils.findTint(unit)
    	},
    	"bob-poison-ball-stream-rampant")
    
    unit = units["bob-titan-spitter"]
    unit["attack_parameters"] = biterUtils.createFireAttack(
    	{
    	    cooldown = 80,
    	    range = 15,
    	    min_range = 3,
    	    turn_range = 1,
	    warmup = 30,
    	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit),
	    tint1 = biterUtils.findTint(unit),
	    tint2 = biterUtils.findTint(unit)
    	},
    	"bob-titan-ball-stream-rampant")

    unit = units["bob-behemoth-spitter"]
    unit["attack_parameters"] = biterUtils.createFireAttack(
    	{
    	    cooldown = 80,
    	    range = 15,
    	    min_range = 3,
    	    turn_range = 1,
	    warmup = 30,
    	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit),
	    tint1 = biterUtils.findTint(unit),
	    tint2 = biterUtils.findTint(unit)
    	},
    	"bob-behemoth-ball-stream-rampant")

    
    unit = units["bob-leviathan-spitter"]
    unit["attack_parameters"] = biterUtils.createFireAttack(
    	{
    	    cooldown = 80,
    	    range = 15,
    	    min_range = 3,
	    warmup = 30,
    	    turn_range = 1,
    	    fire_penalty = 15,
	    scale = biterUtils.findRunScale(unit),
	    tint1 = biterUtils.findTint(unit),
	    tint2 = biterUtils.findTint(unit)
    	},
    	"bob-leviathan-ball-stream-rampant")
end

return bobsUpdates
