data:extend({

	{
	    type = "bool-setting",
	    name = "rampant-useDumbProjectiles",
	    description = "rampant-useDumbProjectiles",
	    setting_type = "startup",
	    default_value = true,
	    order = "a[modifier]-a[projectiles]",
	    per_user = false
	},
	
	{
	    type = "bool-setting",
	    name = "rampant-useNEUnitLaunchers",
	    setting_type = "startup",
	    default_value = true,
	    order = "a[modifier]-b[projectiles]",
	    per_user = false
	},
	
	{
	    type = "bool-setting",
	    name = "rampant-attackWaveGenerationUsePollution",
	    setting_type = "runtime-global",
	    default_value = true,
	    order = "b[modifier]-a[trigger]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-attackWaveGenerationUsePlayerProximity",
	    setting_type = "runtime-global",
	    default_value = true,
	    order = "b[modifier]-b[trigger]",
	    per_user = false
	},

	{
	    type = "double-setting",
	    name = "rampant-attackPlayerThreshold",
	    setting_type = "runtime-global",
	    minimum_value = 0,
	    default_value = 7,
	    order = "c[modifier]-c[threshold]",
	    per_user = false
	},
	
	{
	    type = "double-setting",
	    name = "rampant-attackWaveGenerationThresholdMax",
	    setting_type = "runtime-global",
	    minimum_value = 0,
	    default_value = 20,
	    order = "c[modifier]-b[threshold]",
	    per_user = false
	},

	{
	    type = "double-setting",
	    name = "rampant-attackWaveGenerationThresholdMin",
	    setting_type = "runtime-global",
	    minimum_value = 0,
	    default_value = 0,
	    order = "c[modifier]-a[threshold]",
	    per_user = false
	},

	{
	    type = "int-setting",
	    name = "rampant-newEnemyVariations",
	    setting_type = "startup",
	    minimum_value = 1,
	    maximum_value = 20,
	    default_value = 1,
	    order = "d[modifier]-b[wave]",
	    per_user = false
	},

	{
	    type = "int-setting",
	    name = "rampant-newEnemyTiers",
	    setting_type = "startup",
	    minimum_value = 5,
	    maximum_value = 10,
	    default_value = 5,
	    order = "d[modifier]-c[wave]",
	    per_user = false
	},
	
	{
	    type = "int-setting",
	    name = "rampant-attackWaveMaxSize",
	    setting_type = "runtime-global",
	    minimum_value = 20,
	    maximum_value = 400,
	    default_value = 150,
	    order = "d[modifier]-a[wave]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-safeBuildings",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "e[modifier]-a[safe]",
	    per_user = false
	},
	
	{
	    type = "bool-setting",
	    name = "rampant-safeBuildings-curvedRail",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "e[modifier]-b[safe]",
	    per_user = false
	},

		
	{
	    type = "bool-setting",
	    name = "rampant-safeBuildings-straightRail",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "e[modifier]-c[safe]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-safeBuildings-bigElectricPole",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "e[modifier]-d[safe]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-safeBuildings-railSignals",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "e[modifier]-e[safe]",
	    per_user = false
	},
	
	{
	    type = "bool-setting",
	    name = "rampant-safeBuildings-railChainSignals",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "e[modifier]-f[safe]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-safeBuildings-trainStops",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "e[modifier]-g[safe]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-safeBuildings-lamps",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "e[modifier]-h[safe]",
	    per_user = false
	},
	
	{
	    type = "bool-setting",
	    name = "rampant-permanentNocturnal",
	    description = "rampant-permanentNocturnal",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "f[modifier]-a[ai]",
	    per_user = false
	},


	{
	    type = "double-setting",
	    name = "rampant-deadZoneFrequency",
	    description = "rampant-deadZoneFrequency",
	    setting_type = "runtime-global",
	    default_value = 0.1,
	    minimum_value = 0.0,
	    maximum_value = 1.0,
	    order = "f[modifier]-c[ai]",
	    per_user = false
	},
	
	{
	    type = "double-setting",
	    name = "rampant-aiPointsScaler",
	    description = "rampant-aiPointsScaler",
	    setting_type = "runtime-global",
	    default_value = 1.0,
	    minimum_value = 0.0,
	    maximum_value = 100.0,
	    order = "f[modifier]-b[ai]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-addWallResistanceAcid",
	    description = "rampant-addWallResistanceAcid",
	    setting_type = "startup",
	    default_value = false,
	    order = "g[modifier]-a[damage]",
	    per_user = false
	},

	
	{
	    type = "bool-setting",
	    name = "rampant-removeBloodParticles",
	    description = "rampant-reduceBloodParticles",
	    setting_type = "startup",
	    default_value = true,
	    order = "h[modifier]-a[optimize]",
	    per_user = false
	},
	
	{
	    type = "bool-setting",
	    name = "rampant-attack-warning",
	    description = "rampant-attack-warning",
	    setting_type = "runtime-per-user",
	    default_value = true,
	    order = "j[modifer]-a[message]",
	    per_user = true
	},
	
	{
	    type = "bool-setting",
	    name = "rampant-enableSwarm",
	    description = "rampant-enableSwarm",
	    setting_type = "startup",
	    default_value = true,
	    order = "k[modifier]-a[unit]",
	    per_user = false
	},

	{
	    type = "int-setting",
	    name = "rampant-enemySeed",
	    description = "rampant-enemySeed",
	    setting_type = "startup",
	    minimum_value = 0,
	    default_value = 0,
	    order = "l[modifer]-a[seed]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-newEnemies",
	    description = "rampant-newEnemies",
	    setting_type = "startup",
	    default_value = false,
	    order = "l[modifier]-b[unit]",
	    per_user = false
	}
	
		
	-- {
	--     type = "bool-setting",
	--     name = "rampant-reduceAnimations",
	--     description = "rampant-reduceAnimations",
	--     setting_type = "startup",
	--     default_value = true,
	--     order = "h[modifier]-b[optimize]",
	--     per_user = false
	-- }


	-- ,

	-- {
	--     type = "bool-setting",
	--     name = "rampant-useCustomAI",
	--     description = "rampant-useCustomAI",
	--     setting_type = 'startup',
	--     default_value = false,
	--     order = "h[total]-a[ai]",
	--     per_user = false
	-- }
	
})
