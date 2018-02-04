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
	    order = "b[modifier]-c[threshold]",
	    per_user = false
	},
	
	{
	    type = "double-setting",
	    name = "rampant-attackWaveGenerationThresholdMax",
	    setting_type = "runtime-global",
	    minimum_value = 0,
	    default_value = 20,
	    order = "b[modifier]-d[threshold]",
	    per_user = false
	},

	{
	    type = "double-setting",
	    name = "rampant-attackWaveGenerationThresholdMin",
	    setting_type = "runtime-global",
	    minimum_value = 0,
	    default_value = 0,
	    order = "b[modifier]-e[threshold]",
	    per_user = false
	},
	
	{
	    type = "int-setting",
	    name = "rampant-attackWaveMaxSize",
	    setting_type = "runtime-global",
	    minimum_value = 20,
	    maximum_value = 400,
	    default_value = 150,
	    order = "b[modifier]-f[wave]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-permanentNocturnal",
	    description = "rampant-permanentNocturnal",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "b[modifier]-g[ai]",
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
	    order = "b[modifier]-h[ai]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-enableSwarm",
	    description = "rampant-enableSwarm",
	    setting_type = "startup",
	    default_value = true,
	    order = "b[modifier]-j[unit]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-safeBuildings",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "c[modifier]-a[safe]",
	    per_user = false
	},
	
	{
	    type = "bool-setting",
	    name = "rampant-safeBuildings-curvedRail",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "c[modifier]-b[safe]",
	    per_user = false
	},

	
	{
	    type = "bool-setting",
	    name = "rampant-safeBuildings-straightRail",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "c[modifier]-c[safe]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-safeBuildings-bigElectricPole",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "c[modifier]-d[safe]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-safeBuildings-railSignals",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "c[modifier]-e[safe]",
	    per_user = false
	},
	
	{
	    type = "bool-setting",
	    name = "rampant-safeBuildings-railChainSignals",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "c[modifier]-f[safe]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-safeBuildings-trainStops",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "c[modifier]-g[safe]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-safeBuildings-lamps",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "c[modifier]-h[safe]",
	    per_user = false
	},
	
	{
	    type = "bool-setting",
	    name = "rampant-addWallResistanceAcid",
	    description = "rampant-addWallResistanceAcid",
	    setting_type = "startup",
	    default_value = false,
	    order = "c[modifier]-j[damage]",
	    per_user = false
	},

{
	    type = "bool-setting",
	    name = "rampant-disallowFriendlyFire",
	    setting_type = "startup",
	    default_value = false,
	    order = "c[modifier]-k[trigger]",
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
	    order = "d[modifier]-a[ai]",
	    per_user = false
	},
	

	

	
	
	{
	    type = "bool-setting",
	    name = "rampant-newEnemies",
	    description = "rampant-newEnemies",
	    setting_type = "startup",
	    default_value = false,
	    order = "e[modifier]-a[unit]",
	    per_user = false
	},	

	{
	    type = "int-setting",
	    name = "rampant-enemySeed",
	    description = "rampant-enemySeed",
	    setting_type = "startup",
	    minimum_value = 0,
	    default_value = 0,
	    order = "l[modifer]-b[unit]",
	    per_user = false
	},


	{
	    type = "int-setting",
	    name = "rampant-newEnemyNestTiers",
	    description = "rampant-newEnemyNestTiers",
	    setting_type = "startup",
	    default_value = 5,
	    allowed_values = { 5, 10 },
	    order = "l[modifer]-c[unit]",
	    per_user = false
	},

	{
	    type = "int-setting",
	    name = "rampant-newEnemyNestVariations",
	    description = "rampant-newEnemyNestVariations",
	    setting_type = "startup",
	    minimum_value = 1,
	    maximum_value = 20,
	    default_value = 1,
	    order = "l[modifier]-d[unit]",
	    per_user = false
	},

	{
	    type = "int-setting",
	    name = "rampant-newEnemyWormTiers",
	    description = "rampant-newEnemyWormTiers",
	    setting_type = "startup",
	    default_value = 5,
	    allowed_values = { 5, 10 },
	    order = "l[modifer]-e[unit]",
	    per_user = false
	},

	{
	    type = "int-setting",
	    name = "rampant-newEnemyWormVariations",
	    description = "rampant-newEnemyWormVariations",
	    setting_type = "startup",
	    minimum_value = 1,
	    maximum_value = 20,
	    default_value = 1,
	    order = "l[modifier]-f[unit]",
	    per_user = false
	},

	{
	    type = "int-setting",
	    name = "rampant-newEnemyUnitTiers",
	    description = "rampant-newEnemyUnitTiers",
	    setting_type = "startup",
	    default_value = 5,
	    allowed_values = { 5, 10 },
	    order = "l[modifer]-g[unit]",
	    per_user = false
	},

	{
	    type = "int-setting",
	    name = "rampant-newEnemyUnitVariations",
	    description = "rampant-newEnemyUnitVariations",
	    setting_type = "startup",
	    minimum_value = 1,
	    maximum_value = 20,
	    default_value = 1,
	    order = "l[modifier]-h[unit]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-enableBobsUnits",
	    setting_type = "startup",
	    default_value = true,
	    order = "l[modifier]-i[unit]",
	    per_user = false
	},
	
	{
	    type = "bool-setting",
	    name = "rampant-enableNEUnits",
	    setting_type = "startup",
	    default_value = true,
	    order = "l[modifier]-j[unit]",
	    per_user = false
	},

	{
	    type = "int-setting",
	    name = "rampant-tierStart",
	    setting_type = "startup",
	    default_value = 1,
	    minimum_value = 1,
	    maximum_value = 10,
	    order = "l[modifier]-l[unit]",
	    per_user = false
	},

	{
	    type = "int-setting",
	    name = "rampant-tierEnd",
	    setting_type = "startup",
	    minimum_value = 1,
	    maximum_value = 10,
	    default_value = 4,
	    order = "l[modifier]-m[unit]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-removeBloodParticles",
	    description = "rampant-reduceBloodParticles",
	    setting_type = "startup",
	    default_value = true,
	    order = "n[modifier]-a[optimize]",
	    per_user = false
	},
	
	{
	    type = "bool-setting",
	    name = "rampant-attack-warning",
	    description = "rampant-attack-warning",
	    setting_type = "runtime-per-user",
	    default_value = true,
	    order = "o[modifer]-a[message]",
	    per_user = true
	},	
	

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
