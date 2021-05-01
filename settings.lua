data:extend({

        {
            type = "bool-setting",
            name = "rampant--useDumbProjectiles",
            description = "rampant--useDumbProjectiles",
            setting_type = "startup",
            default_value = true,
            order = "a[modifier]-a[projectiles]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--disableCollidingProjectiles",
            setting_type = "startup",
            default_value = true,
            order = "b[modifier]-b[trigger]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--unitsAffectedByTiles",
            setting_type = "startup",
            default_value = true,
            order = "b[modifier]-b[trigger]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--oldRedEnemyMapColor",
            setting_type = "startup",
            default_value = false,
            order = "b[modifier]-c[trigger]",
            per_user = false
        },

        {
            type = "int-setting",
            name = "rampant--attackWaveMaxSize",
            setting_type = "runtime-global",
            minimum_value = 20,
            maximum_value = 600,
            default_value = 75,
            order = "b[modifier]-f[wave]",
            per_user = false
        },

        {
            type = "int-setting",
            name = "rampant--maxNumberOfSquads",
            setting_type = "runtime-global",
            minimum_value = 1,
            maximum_value = 300,
            default_value = 50,
            order = "b[modifier]-f[wave]",
            per_user = false
        },

        {
            type = "int-setting",
            name = "rampant--maxNumberOfBuilders",
            setting_type = "runtime-global",
            minimum_value = 1,
            maximum_value = 120,
            default_value = 35,
            order = "b[modifier]-f[wave]",
            per_user = false
        },

        {
            type = "int-setting",
            name = "rampant--unitAndSpawnerFadeTime",
            setting_type = "startup",
            minimum_value = 1,
            maximum_value = 30000000 * 60,
            default_value = 5 * 60,
            order = "b[modifier]-f[wave]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--enableFadeTime",
            setting_type = "startup",
            default_value = true,
            order = "b[modifier]-f[wave]",
            per_user = false
        },

        -- {
        --     type = "bool-setting",
        --     name = "rampant--liteMode",
        --     setting_type = "startup",
        --     default_value = false,
        --     order = "b[modifier]-g[ai]",
        --     per_user = false
        -- },

        {
            type = "bool-setting",
            name = "rampant--enableSwarm",
            description = "rampant--enableSwarm",
            setting_type = "startup",
            default_value = true,
            order = "b[modifier]-j[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--enableShrinkNestsAndWorms",
            description = "rampant--enableShrinkNestsAndWorms",
            setting_type = "startup",
            default_value = true,
            order = "b[modifier]-j[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--safeBuildings",
            setting_type = "runtime-global",
            default_value = false,
            order = "c[modifier]-a[safe]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--safeBuildings-curvedRail",
            setting_type = "runtime-global",
            default_value = false,
            order = "c[modifier]-b[safe]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--safeBuildings-straightRail",
            setting_type = "runtime-global",
            default_value = false,
            order = "c[modifier]-c[safe]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--safeBuildings-bigElectricPole",
            setting_type = "runtime-global",
            default_value = false,
            order = "c[modifier]-d[safe]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--safeBuildings-railSignals",
            setting_type = "runtime-global",
            default_value = false,
            order = "c[modifier]-e[safe]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--safeBuildings-railChainSignals",
            setting_type = "runtime-global",
            default_value = false,
            order = "c[modifier]-f[safe]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--safeBuildings-trainStops",
            setting_type = "runtime-global",
            default_value = false,
            order = "c[modifier]-g[safe]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--safeBuildings-lamps",
            setting_type = "runtime-global",
            default_value = false,
            order = "c[modifier]-h[safe]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--addWallResistanceAcid",
            description = "rampant--addWallResistanceAcid",
            setting_type = "startup",
            default_value = false,
            order = "c[modifier]-j[damage]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--disallowFriendlyFire",
            setting_type = "startup",
            default_value = false,
            order = "c[modifier]-k[trigger]",
            per_user = false
        },


        {
            type = "double-setting",
            name = "rampant--deadZoneFrequency",
            description = "rampant--deadZoneFrequency",
            setting_type = "runtime-global",
            default_value = 0.1,
            minimum_value = 0.0,
            maximum_value = 1.0,
            order = "d[modifier]-a[ai]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--newEnemies",
            description = "rampant--newEnemies",
            setting_type = "startup",
            default_value = false,
            order = "e[modifier]-a[unit]",
            per_user = false
        },

        {
            type = "int-setting",
            name = "rampant--enemySeed",
            description = "rampant--enemySeed",
            setting_type = "startup",
            minimum_value = 1,
            default_value = 1024567,
            order = "l[modifer]-b[unit]",
            per_user = false
        },

        {
            type = "int-setting",
            name = "rampant--newEnemyVariations",
            description = "rampant--newEnemyVariations",
            setting_type = "startup",
            minimum_value = 1,
            maximum_value = 20,
            default_value = 1,
            order = "l[modifier]-h[unit]",
            per_user = false
        },

        {
            type = "int-setting",
            name = "rampant--tierStart",
            setting_type = "startup",
            default_value = 1,
            minimum_value = 1,
            maximum_value = 10,
            order = "l[modifier]-l[unit]",
            per_user = false
        },

        {
            type = "int-setting",
            name = "rampant--tierEnd",
            setting_type = "startup",
            minimum_value = 1,
            maximum_value = 10,
            default_value = 4,
            order = "l[modifier]-m[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--acidEnemy",
            setting_type = "startup",
            default_value = true,
            order = "l[modifier]-n[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--physicalEnemy",
            setting_type = "startup",
            default_value = true,
            order = "l[modifier]-o[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--suicideEnemy",
            setting_type = "startup",
            default_value = true,
            order = "l[modifier]-p[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--fireEnemy",
            setting_type = "startup",
            default_value = true,
            order = "l[modifier]-q[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--electricEnemy",
            setting_type = "startup",
            default_value = true,
            order = "l[modifier]-r[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--nuclearEnemy",
            setting_type = "startup",
            default_value = true,
            order = "l[modifier]-s[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--infernoEnemy",
            setting_type = "startup",
            default_value = true,
            order = "l[modifier]-t[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--fastEnemy",
            setting_type = "startup",
            default_value = true,
            order = "l[modifier]-u[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--trollEnemy",
            setting_type = "startup",
            default_value = true,
            order = "l[modifier]-v[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--spawnerEnemy",
            setting_type = "startup",
            default_value = true,
            order = "l[modifier]-w[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--waspEnemy",
            setting_type = "startup",
            default_value = true,
            order = "l[modifier]-x[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--laserEnemy",
            setting_type = "startup",
            default_value = true,
            order = "l[modifier]-y[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--energyThiefEnemy",
            setting_type = "startup",
            default_value = true,
            order = "l[modifier]-z[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--poisonEnemy",
            setting_type = "startup",
            default_value = true,
            order = "l[modifier]-za[unit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--unitSpawnerBreath",
            setting_type = "startup",
            default_value = true,
            order = "l[modifier]-zb[unit]",
            per_user = false
        },

        -- {
        --     type = "bool-setting",
        --     name = "rampant--disableVanillaAI",
        --     description = "rampant--disableVanillaAI",
        --     setting_type = 'runtime-global',
        --     default_value = true,
        --     order = "m[total]-a[ai]",
        --     per_user = false
        -- },

        {
            type = "bool-setting",
            name = "rampant--enableMigration",
            description = "rampant--enableMigration",
            setting_type = 'runtime-global',
            default_value = true,
            order = "m[total]-c[ai]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--raidAIToggle",
            setting_type = "runtime-global",
            default_value = true,
            order = "m[total]-c[ai]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--siegeAIToggle",
            setting_type = "runtime-global",
            default_value = true,
            order = "m[total]-c[ai]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--peacefulAIToggle",
            setting_type = "runtime-global",
            default_value = true,
            order = "m[total]-c[ai]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--printAIStateChanges",
            setting_type = "runtime-global",
            default_value = false,
            order = "m[total]-c[ai]z",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--debugTemperament",
            setting_type = "runtime-global",
            default_value = false,
            order = "m[total]-c[ai]zz",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--permanentNocturnal",
            description = "rampant--permanentNocturnal",
            setting_type = "runtime-global",
            default_value = false,
            order = "m[total]-a[ai]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--aiPointsScaler",
            description = "rampant--aiPointsScaler",
            setting_type = "runtime-global",
            default_value = 1.0,
            minimum_value = 0.0,
            maximum_value = 100.0,
            order = "m[total]-b[ai]1",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--aiPointsPrintSpendingToChat",
            description = "rampant--aiPointsPrintSpendingToChat",
            setting_type = "runtime-global",
            default_value = false,
            order = "m[total]-c[ai]z",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--aiPointsPrintGainsToChat",
            description = "rampant--aiPointsPrintGainsToChat",
            setting_type = "runtime-global",
            default_value = false,
            order = "m[total]-c[ai]z",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--temperamentRateModifier",
            setting_type = "runtime-global",
            default_value = 1,
            minimum_value = 0.001,
            maximum_value = 100.0,
            order = "m[total]-b[ai]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--removeBloodParticles",
            description = "rampant--reduceBloodParticles",
            setting_type = "startup",
            default_value = false,
            order = "n[modifier]-a[optimize]",
            per_user = false
        },

        -- {
        --     type = "bool-setting",
        --     name = "rampant--enableFullMapScan",
        --     setting_type = "runtime-global",
        --     default_value = true,
        --     order = "n[modifier]-a[optimize]",
        --     per_user = false
        -- },

        {
            type = "bool-setting",
            name = "rampant--unkillableLogisticRobots",
            setting_type = "startup",
            default_value = false,
            order = "n[modifier]-b[optimize]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--unkillableConstructionRobots",
            setting_type = "startup",
            default_value = false,
            order = "n[modifier]-c[optimize]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitBiterHealthScaler",
            description = "rampant--unitBiterHealthScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-a[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitBiterHealingScaler",
            description = "rampant--unitBiterHealingScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-a[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitBiterSpeedScaler",
            description = "rampant--unitBiterSpeedScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-b[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitBiterDamageScaler",
            description = "rampant--unitBiterDamageScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-c[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitBiterRangeScaler",
            description = "rampant--unitBiterRangeScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-d[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitSpitterHealthScaler",
            description = "rampant--unitSpitterHealthScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-e[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitSpitterHealingScaler",
            description = "rampant--unitSpitterHealingScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-e[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitSpitterSpeedScaler",
            description = "rampant--unitSpitterSpeedScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-f[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitSpitterDamageScaler",
            description = "rampant--unitSpitterDamageScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-g[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitSpitterRangeScaler",
            description = "rampant--unitSpitterRangeScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-h[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitDroneHealthScaler",
            description = "rampant--unitDroneHealthScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-i[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitDroneHealingScaler",
            description = "rampant--unitDroneHealingScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-i[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitDroneSpeedScaler",
            description = "rampant--unitDroneSpeedScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-j[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitDroneDamageScaler",
            description = "rampant--unitDroneDamageScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-k[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitDroneRangeScaler",
            description = "rampant--unitDroneRangeScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-l[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitWormHealthScaler",
            description = "rampant--unitWormHealthScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-m[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitWormHealingScaler",
            description = "rampant--unitWormHealingScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-m[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitWormDamageScaler",
            description = "rampant--unitWormDamageScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-n[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitWormRangeScaler",
            description = "rampant--unitWormRangeScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-o[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitSpawnerHealthScaler",
            description = "rampant--unitSpawnerHealthScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-p[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitSpawnerHealingScaler",
            description = "rampant--unitSpawnerHealingScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-p[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitSpawnerOwnedScaler",
            description = "rampant--unitSpawnerOwnedScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-q[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitSpawnerSpawnScaler",
            description = "rampant--unitSpawnerSpawnScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-r[unit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitSpawnerRespawnScaler",
            description = "rampant--unitSpawnerRespawnScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-r[unit]",
            per_user = false
        },


        {
            type = "double-setting",
            name = "rampant--unitHiveRespawnScaler",
            description = "rampant--unitHiveRespawnScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-r[zunit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitHiveHealthScaler",
            description = "rampant--unitHiveHealthScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-r[zunit]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--unitHiveHealingScaler",
            description = "rampant--unitHiveHealingScaler",
            setting_type = "startup",
            default_value = 1.0,
            minimum_value = 0.0001,
            maximum_value = 100000.0,
            order = "p[modifier]-r[zunit]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--attackWaveGenerationUsePlayerProximity",
            setting_type = "runtime-global",
            default_value = true,
            order = "b[modifier]-b[trigger]",
            per_user = false

        },

        {
            type = "double-setting",
            name = "rampant--attackPlayerThreshold",
            setting_type = "runtime-global",
            minimum_value = 0,
            default_value = 20,
            order = "b[modifier]-c[threshold]",
            per_user = false
        }

})
