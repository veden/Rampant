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
	    name = "rampant-permanentNocturnal",
	    description = "rampant-permanentNocturnal",
	    setting_type = "runtime-global",
	    default_value = false,
	    order = "f[modifier]-a[ai]",
	    per_user = false
	},

	{
	    type = "double-setting",
	    name = "rampant-aiPointsScaler",
	    description = "rampant-aiPointsScaler",
	    setting_type = "runtime-global",
	    default_value = 1.0,
	    minimum_value = 0.0,
	    maximum_value = 5.0,
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
	}
})
