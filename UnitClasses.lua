-- imports

local biterUtils = require("prototypes/enemies/BiterUtils")
local swarmUtils = require("Swarmutils")

-- imported functions



local createUnitClass = swarmUtils.createUnitClass

local createSuicideAttack = biterUtils.createSuicideAttack


-- module code

createUnitClass(
    {
	unit = 10,
	unitSpawner = 5,
	probabilityTable = 5
    },
    {
	unit = {
	    name = "rampant-suicide-biter",

	    attributes = {
		health = 30,
		movement = 0.21,
		distancePerFrame = 0.1,
		healing = 0.01,
		explosion = "blood-explosion-small",
	    },

	    attack = {
		area = 3.5,
		damage = 20,
		explosion = "explosion",
		scorchmark = "small-scorchmark",
		explosionCount = 2,
		explosionDistance = 2,
	    },

	    resistances = {
		explosion = {
		    decrease = 0,
		    percentage = -50
		},
		laser = {
		    decrease = 1,
		    percentage = 0
		},
		fire = {
		    decrease = 0,
		    percentage = -60
		}
	    },

	    type = "biter",
	    scale = 0.55,
	    tint1 = {r=0.6, g=0.0, b=0.70, a=0.8},
	    tint2 = {r=0.7, g=0.0, b=0.72, a=0.4}
	},

	unitSpawner = {
	    name = "rampant-suicide-nest",
	    attributes = {
		health = 30,
		healing = 0.01,
		unitsOwned = 7,
		unitsToSpawn = 5,
		spawingCooldownStart = 360,
		spawingCooldownStop = 150,
		
	    },
	    resistances = {
		explosion = {
		    decrease = 0,
		    percentage = -50
		},
		laser = {
		    decrease = 1,
		    percentage = 0
		},
		fire = {
		    decrease = 0,
		    percentage = -60
		}
	    },
	    scale = 0.95,
	    tint = {r=0.7, g=0.0, b=0.72, a=0.4}
	}
    },

    {
	unit = {
	    {
		cost = 1,
		bonus = {
		    {
			type = "attribute",
			name = "health",
			adjustment = 50
		    }
		}
	    }
	},

	unitSpawner = {
	    {
		cost = 1,
		bonus = {
		    {
			type = "attribute",
			name = "health",
			adjustment = 50
		    }
		}
	    }
	},

	probabilityTable = {
	    {
		cost = 1,
		index = 1,
		adjustment = 1
	    },
	    {
		cost = 1,
		index = 2,
		adjustment = 1
	    },
	    {
		cost = 1,
		index = 3,
		adjustment = 1
	    },
	    {
		cost = 1,
		index = 4,
		adjustment = 1
	    },
	    {
		cost = 1,
		index = 5,
		adjustment = 1
	    },
	    {
		cost = 1,
		index = 6,
		adjustment = 1.5
	    },
	    {
		cost = 1,
		index = 7,
		adjustment = 2
	    },
	    {
		cost = 1,
		index = 8,
		adjustment = 2
	    },
	    {
		cost = 1,
		index = 9,
		adjustment = 1.5
	    },
	    {
		cost = 1,
		index = 10,
		adjustment = 1
	    }
	}
    },

    createSuicideAttack,

    {
	unit = 5,
	unitSpawner = 5
    },

    {
	unit = 10,
	unitSpawner = 7
    }
)

