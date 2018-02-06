-- import

local attackBall = require("AttackBall")

-- module code

local softSmoke = "the-soft-smoke-rampant"
local createAttackBall = attackBall.createAttackBall


createAttackBall(
    {
	name = "ne-infected-unit-ball",
	pTint = {r=0, g=0.97, b=0.34, a=0.5},
	sTint = {r=0, g=0.1, b=1, a=1},
	softSmokeName = softSmoke,
	type = "projectile",
	pointEffects = function (attributes)
	    return {
		{
		    type = "create-entity",
		    entity_name = "unit-cluster",
		    trigger_created_entity = "true"
		},
		{
		    type = "create-sticker",
		    sticker = "slowdown-sticker",
		},
		{
		    type = "create-entity",
		    entity_name = "Infected-Poison-Cloud"
		},
		{
		    type = "damage",
		    damage = {amount = 10, type = "explosion"}
		},
		{
		    type = "damage",
		    damage = {amount = 24, type = "poison"}
		}
	    }
	end,
	radius = 1,
	areaEffects = function (attributes)
	    return 
		{
		    {
			type = "damage",
			damage = { amount = 0, type = "explosion" }
		    }
		}
	end	    
    }
)

--

createAttackBall(
    {
	name = "ne-mutated-unit-ball",
	pTint = {r=0.5, g=0.7, b=0.34, a=0.5},
	sTint = {r=0.5, g=0.97, b=0.34, a=0.5},
	softSmokeName = softSmoke,
	type = "projectile",
	pointEffects = function (attributes)
	    return {
		{
		    type = "create-entity",
		    entity_name = "unit-cluster",
		    trigger_created_entity = "true"
		},
		{
		    type = "create-sticker",
		    sticker = "slowdown-sticker",
		},
		{
		    type = "create-entity",
		    entity_name = "acid-splash-purple"
		}
	    }
	end,
	radius = 2,
	areaEffects = function (attributes)
	    return 
		{
		    {
			type = "damage",
			damage = { amount = 8, type = "explosion" }
		    },
		    {
			type = "damage",
			damage = { amount = 18, type = "acid" }
		    }
		}
	end	    
    }
)

--

createAttackBall(
    {
	name = "ne-infected-ball",
	pTint = {r=0.5, g=0.7, b=0.34, a=0.5},
	sTint = {r=0.5, g=0.97, b=0.34, a=0.5},
	softSmokeName = softSmoke,
	type = "projectile",
	pointEffects = function (attributes)
	    return {
		{
		    type = "create-entity",
		    entity_name = "Infected-Poison-Cloud"
		}
	    }
	end,
	radius = 1.5,
	areaEffects = function (attributes)
	    return 
		{
		    {
			type = "damage",
			damage = { amount = 5, type = "explosion" }
		    },
		    {
			type = "damage",
			damage = { amount = 12, type = "poison" }
		    }
		}
	end	    
    }
)

--

createAttackBall(
    {
	name = "ne-mutated-ball",
	pTint = {r=0.5, g=0.7, b=0.34, a=0.5},
	sTint = {r=0.5, g=0.97, b=0.34, a=0.5},
	softSmokeName = softSmoke,
	type = "projectile",
	pointEffects = function (attributes)
	    return {
		{
		    type = "create-entity",
		    entity_name = "acid-splash-purple"
		}
	    }
	end,
	radius = 1.5,
	areaEffects = function (attributes)
	    return 
		{
		    {
			type = "damage",
			damage = { amount = 5, type = "explosion" }
		    },
		    {
			type = "damage",
			damage = { amount = 12, type = "acid" }
		    }
		}
	end	    
    }
)
