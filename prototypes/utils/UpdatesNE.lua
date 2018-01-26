local NEUpdates = {}

local biterUtils = require("BiterUtils")

function NEUpdates.useNEUnitLaunchers ()
    local turrets = data.raw["turret"];
    
    turrets["medium-worm-turret"]["attack_parameters"] = biterUtils.createStreamAttack(
    	{
    	    cooldown = 60,
    	    range = 25,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
	    damageModifier = 2.5,
    	    scale = 1.2
    	},
    	"ne-infected-unit-ball-stream-rampant")

    turrets["big-worm-turret"]["attack_parameters"] = biterUtils.createStreamAttack(
    	{
    	    cooldown = 60,
    	    range = 30,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
	    damageModifier = 3,
    	    scale = 1.6
    	},
    	"ne-mutated-unit-ball-stream-rampant")
end

function NEUpdates.useDumbProjectiles()
    local turrets = data.raw["turret"];    

    local turret = turrets["small-worm-turret"]
    turret["attack_parameters"].range = 19

    turret = turrets["medium-worm-turret"]
    turret["attack_parameters"].range = 22
    turret["attack_parameters"] = biterUtils.createStreamAttack(
    	{
    	    cooldown = 60,
    	    range = 25,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
	    damageModifier = 4.5,
	    scale = 1.2
    	},
    	"ne-infected-ball-stream-rampant")

    turret = turrets["big-worm-turret"]
    turret["attack_parameters"].range = 27
    turret["attack_parameters"] = biterUtils.createStreamAttack(
    	{
    	    cooldown = 60,
    	    range = 30,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 0,
	    damageModifier = 5.5,
	    scale = 1.6
    	},
    	"ne-mutated-ball-stream-rampant")
    
    local units = data.raw["unit"]

    local unit = units["small-spitter-Mk2"]
    unit["attack_parameters"] = biterUtils.createStreamAttack(
    	{
    	    cooldown = 100,
    	    range = 13,
    	    min_range = 3,
    	    turn_range = 1,
    	    fire_penalty = 15,
	    warmup = 30,
    	    damageModifier = 1.1,
    	    scale = biterUtils.findRunScale(unit),
    	    tint = biterUtils.findTint(unit)
    	},
    	"ne-infected-ball-stream-rampant")

    unit = units["small-spitter-Mk3"]
    unit["attack_parameters"] = biterUtils.createStreamAttack(
    	{
    	    cooldown = 100,
    	    range = 13,
    	    min_range = 3,
    	    turn_range = 1,
	    warmup = 30,
    	    fire_penalty = 15,
    	    damageModifier = 1.2,
    	    scale = biterUtils.findRunScale(unit),
    	    tint = biterUtils.findTint(unit)
    	},
    	"ne-mutated-ball-stream-rampant")

    
    unit = units["medium-spitter-Mk2"]
    unit["attack_parameters"] = biterUtils.createStreamAttack(
    	{
    	    cooldown = 100,
    	    range = 14,
    	    min_range = 3,
    	    turn_range = 1,
	    warmup = 30,
    	    fire_penalty = 15,
    	    damageModifier = 2.3,
    	    scale = biterUtils.findRunScale(unit),
    	    tint = biterUtils.findTint(unit)
    	},
    	"ne-infected-ball-stream-rampant")

    unit = units["medium-spitter-Mk3"]
    unit["attack_parameters"] = biterUtils.createStreamAttack(
    	{
    	    cooldown = 100,
    	    range = 14,
    	    min_range = 3,
    	    turn_range = 1,
	    warmup = 30,
    	    fire_penalty = 15,
    	    damageModifier = 2.6,
    	    scale = biterUtils.findRunScale(unit),
    	    tint = biterUtils.findTint(unit)
    	},
    	"ne-mutated-ball-stream-rampant")
    
    unit = units["big-spitter-Mk2"]
    unit["attack_parameters"] = biterUtils.createStreamAttack(
    	{
    	    cooldown = 100,
    	    range = 15,
    	    min_range = 3,
    	    turn_range = 1,
	    warmup = 30,
    	    fire_penalty = 15,
    	    damageModifier = 3.3,
    	    scale = biterUtils.findRunScale(unit),
    	    tint = biterUtils.findTint(unit)
    	},
    	"ne-infected-ball-stream-rampant")

    
    unit = units["big-spitter-Mk3"]
    unit["attack_parameters"] = biterUtils.createStreamAttack(
    	{
    	    cooldown = 100,
    	    range = 15,
    	    min_range = 3,
    	    turn_range = 1,
	    warmup = 30,
    	    fire_penalty = 15,
    	    damageModifier = 3.6,
    	    scale = biterUtils.findRunScale(unit),
    	    tint = biterUtils.findTint(unit)
    	},
    	"ne-mutated-ball-stream-rampant")
    
end

return NEUpdates
