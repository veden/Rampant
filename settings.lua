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
	    setting_type = "startup",
	    default_value = true,
	    order = "b[modifier]-a[trigger]",
	    per_user = false
	},

	{
	    type = "bool-setting",
	    name = "rampant-attackWaveGenerationUsePlayerProximity",
	    setting_type = "startup",
	    default_value = true,
	    order = "b[modifier]-b[trigger]",
	    per_user = false
	},

	{
	    type = "double-setting",
	    name = "rampant-attackWaveGenerationThresholdMax",
	    setting_type = "startup",
	    minimum_value = 0,
	    default_value = 20,
	    order = "c[modifier]-b[threshold]",
	    per_user = false
	},

	{
	    type = "double-setting",
	    name = "rampant-attackWaveGenerationThresholdMin",
	    setting_type = "startup",
	    minimum_value = 0,
	    default_value = 0,
	    order = "c[modifier]-a[threshold]",
	    per_user = false
	},

	{
	    type = "int-setting",
	    name = "rampant-attackWaveMaxSize",
	    setting_type = "startup",
	    minimum_value = 20,
	    maximum_value = 250,
	    default_value = 150,
	    order = "d[modifier]-a[wave]",
	    per_user = false
	}

		
})
