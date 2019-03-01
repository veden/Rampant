-- import

local attacks = {}

local attackBall = require("AttackBall")

local FORCE_OLD_PROJECTILES = settings.startup["rampant-forceOldProjectiles"].value

-- module code

local softSmoke = "the-soft-smoke-rampant"
local createAttackBall = attackBall.createAttackBall

function attacks.addAttacks()

    local multipler = (FORCE_OLD_PROJECTILES and 1) or 2.7 

    -- createAttackBall(
    --     {
    --         name = "ne-infected-unit-ball",
    --         pTint = {r=0, g=0.97, b=0.34, a=0.5},
    --         sTint = {r=0, g=0.1, b=1, a=1},
    --         softSmokeName = softSmoke,
    --         type = "projectile",
    --         pointEffects = function (attributes)
    --     	return {
    --     	    {
    --     		type = "create-entity",
    --     		entity_name = "unit-cluster",
    --     		trigger_created_entity = "true"
    --     	    },
    --     	    {
    --     		type = "create-sticker",
    --     		sticker = "slowdown-sticker",
    --     	    },
    --     	    {
    --     		type = "create-entity",
    --     		entity_name = "Infected-Poison-Cloud"
    --     	    },
    --     	    {
    --     		type = "damage",
    --     		damage = {amount = 10 * multipler, type = "explosion"}
    --     	    },
    --     	    {
    --     		type = "damage",
    --     		damage = {amount = 24 * multipler, type = "poison"}
    --     	    }
    --     	}
    --         end,
    --         radius = 1,
    --         areaEffects = function (attributes)
    --     	return 
    --     	    {
    --     		{
    --     		    type = "damage",
    --     		    damage = { amount = 0, type = "explosion" }
    --     		}
    --     	    }
    --         end	    
    --     }
    -- )

    --

    -- createAttackBall(
    --     {
    --         name = "ne-mutated-unit-ball",
    --         pTint = {r=0.5, g=0.7, b=0.34, a=0.5},
    --         sTint = {r=0.5, g=0.97, b=0.34, a=0.5},
    --         softSmokeName = softSmoke,
    --         type = "projectile",
    --         pointEffects = function (attributes)
    --     	return {
    --     	    {
    --     		type = "create-entity",
    --     		entity_name = "unit-cluster",
    --     		trigger_created_entity = "true"
    --     	    },
    --     	    {
    --     		type = "create-sticker",
    --     		sticker = "slowdown-sticker",
    --     	    },
    --     	    {
    --     	        type = "create-entity",
    --                     --FIXME
    --     	        entity_name = "explosion"
    --     	    }
    --     	}
    --         end,
    --         radius = 2,
    --         areaEffects = function (attributes)
    --     	return 
    --     	    {
    --     		{
    --     		    type = "damage",
    --     		    damage = { amount = 8 * multipler, type = "explosion" }
    --     		},
    --     		{
    --     		    type = "damage",
    --     		    damage = { amount = 18 * multipler, type = "acid" }
    --     		}
    --     	    }
    --         end	    
    --     }
    -- )

    --

    -- createAttackBall(
    --     {
    --         name = "ne-infected-ball",
    --         pTint = {r=0.5, g=0.7, b=0.34, a=0.5},
    --         sTint = {r=0.5, g=0.97, b=0.34, a=0.5},
    --         softSmokeName = softSmoke,
    --         type = "projectile",
    --         pointEffects = function (attributes)
    --     	return {
    --     	    {
    --     		type = "create-entity",
    --     		entity_name = "Infected-Poison-Cloud"
    --     	    }
    --     	}
    --         end,
    --         radius = 1.5,
    --         areaEffects = function (attributes)
    --     	return 
    --     	    {
    --     		{
    --     		    type = "damage",
    --     		    damage = { amount = 5 * multipler, type = "explosion" }
    --     		},
    --     		{
    --     		    type = "damage",
    --     		    damage = { amount = 12 * multipler, type = "poison" }
    --     		}
    --     	    }
    --         end	    
    --     }
    -- )

    if not FORCE_OLD_PROJECTILES then
	-- createAttackBall(
	--     {
	-- 	name = "ne-infected-ball-direction",
	-- 	pTint = {r=0.5, g=0.7, b=0.34, a=0.5},
	-- 	sTint = {r=0.5, g=0.97, b=0.34, a=0.5},
	-- 	softSmokeName = softSmoke,
	-- 	directionOnly = true,
	-- 	type = "projectile",
	-- 	pointEffects = function (attributes)
	-- 	    return {
	-- 		{
	-- 		    type = "create-entity",
	-- 		    entity_name = "Infected-Poison-Cloud"
	-- 		}
	-- 	    }
	-- 	end,
	-- 	radius = 1.5,
	-- 	areaEffects = function (attributes)
	-- 	    return 
	-- 		{
	-- 		    {
	-- 			type = "damage",
	-- 			damage = { amount = 5 * multipler, type = "explosion" }
	-- 		    },
	-- 		    {
	-- 			type = "damage",
	-- 			damage = { amount = 12 * multipler, type = "poison" }
	-- 		    }
	-- 		}
	-- 	end	    
	--     }
	-- )
    end

    --

    -- createAttackBall(
    --     {
    --         name = "ne-mutated-ball",
    --         pTint = {r=0.5, g=0.7, b=0.34, a=0.5},
    --         sTint = {r=0.5, g=0.97, b=0.34, a=0.5},
    --         softSmokeName = softSmoke,
    --         type = "projectile",
    --         pointEffects = function (attributes)
    --     	return {
    --     	    {
    --                     --FIXME
    --     	        type = "create-entity",
    --     	        entity_name = "explosion"
    --     	    }
    --     	}
    --         end,
    --         radius = 1.5,
    --         areaEffects = function (attributes)
    --     	return 
    --     	    {
    --     		{
    --     		    type = "damage",
    --     		    damage = { amount = 5 * multipler, type = "explosion" }
    --     		},
    --     		{
    --     		    type = "damage",
    --     		    damage = { amount = 12 * multipler, type = "acid" }
    --     		}
    --     	    }
    --         end	    
    --     }
    -- )

    if not FORCE_OLD_PROJECTILES then
	-- createAttackBall(
	--     {
	-- 	name = "ne-mutated-ball-direction",
	-- 	pTint = {r=0.5, g=0.7, b=0.34, a=0.5},
	-- 	sTint = {r=0.5, g=0.97, b=0.34, a=0.5},
	-- 	softSmokeName = softSmoke,
	-- 	directionOnly = true,
	-- 	type = "projectile",
	-- 	pointEffects = function (attributes)
	-- 	    return {
	-- 		{
        --                     --FIXME
	-- 		    type = "create-entity",
	-- 		    entity_name = "explosion"
	-- 		}
	-- 	    }
	-- 	end,
	-- 	radius = 1.5,
	-- 	areaEffects = function (attributes)
	-- 	    return 
	-- 		{
	-- 		    {
	-- 			type = "damage",
	-- 			damage = { amount = 5 * multipler, type = "explosion" }
	-- 		    },
	-- 		    {
	-- 			type = "damage",
	-- 			damage = { amount = 12 * multipler, type = "acid" }
	-- 		    }
	-- 		}
	-- 	end	    
	--     }
	-- )
    end
end

return attacks
