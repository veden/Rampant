-- Copyright (C) 2022  veden

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
            default_value = 30,
            order = "b[modifier]-f[wave]",
            per_user = false
        },

        {
            type = "int-setting",
            name = "rampant--maxNumberOfBuilders",
            setting_type = "runtime-global",
            minimum_value = 1,
            maximum_value = 120,
            default_value = 10,
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
            default_value = false,
            order = "b[modifier]-j[unit]",
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
            default_value = true,
            order = "c[modifier]-k[trigger]",
            per_user = false
        },

        {
            type = "int-setting",
            name = "rampant--max-evo-dist",
            setting_type = "startup",
            default_value = 300,
            minimum_value = 0,
            maximum_value = 100000,
            order = "c[max]-k[evolution]",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--adaptationModifier",
            setting_type = "runtime-global",
            default_value = 1,
            minimum_value = 0.001,
            maximum_value = 100.0,
            order = "m[total]-b[ai]",
            per_user = false
        },

        {
            type = "int-setting",
            name = "rampant--max-base-mutations",
            setting_type = "runtime-global",
            minimum_value = 0,
            maximum_value = 9999999999,
            default_value = 2,
            order = "d[modifier]-a[ai]",
            per_user = false
        },

        {
            type = "int-setting",
            name = "rampant--maxBaseAlignmentHistory",
            setting_type = "runtime-global",
            minimum_value = 0,
            maximum_value = 16,
            default_value = 16,
            order = "d[modifier]-a[ai]",
            per_user = false
        },

        {
            type = "int-setting",
            name = "rampant--initialPeaceTime",
            setting_type = "runtime-global",
            minimum_value = 0,
            default_value = 20,
            maximum_value = 9999999999,
            order = "d[modifier]-a[ai]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--printAwakenMessage",
            setting_type = "runtime-global",
            default_value = true,
            order = "d[modifier]-a[ai]",
            per_user = false
        },

        -- {
        --     type = "double-setting",
        --     name = "rampant--deadZoneFrequency",
        --     description = "rampant--deadZoneFrequency",
        --     setting_type = "runtime-global",
        --     default_value = 0.1,
        --     minimum_value = 0.0,
        --     maximum_value = 1.0,
        --     order = "d[modifier]-a[ai]",
        --     per_user = false
        -- },

        {
            type = "bool-setting",
            name = "rampant--newEnemies",
            description = "rampant--newEnemies",
            setting_type = "startup",
            default_value = true,
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
            maximum_value = 4294967295,
            order = "l[modifer]-b[unit]",
            per_user = false
        },

        {
            type = "int-setting",
            name = "rampant--newEnemyVariations",
            description = "rampant--newEnemyVariations",
            setting_type = "startup",
            minimum_value = 1,
            maximum_value = 7,
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
            default_value = 6,
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
            name = "rampant--enabledPurpleSettlerCloud",
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
            name = "rampant--printBaseAdaptation",
            setting_type = "runtime-global",
            default_value = false,
            order = "m[total]-c[ai]z",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--printBaseUpgrades",
            setting_type = "runtime-global",
            default_value = false,
            order = "m[total]-c[ai]z",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--printBaseSettling",
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
            name = "rampant--legacyChunkScanning",
            description = "rampant--legacyChunkScanning",
            setting_type = "runtime-global",
            default_value = false,
            order = "m[total]-c[ai]zz",
            per_user = false
        },

        {
            type = "double-setting",
            name = "rampant--minimumAdaptationEvolution",
            description = "rampant--minimumAdaptationEvolution",
            setting_type = "runtime-global",
            default_value = 0.2,
            minimum_value = 0.0,
            maximum_value = 1.0,
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
            type = "double-setting",
            name = "rampant--baseDistanceModifier",
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

        {
            type = "double-setting",
            name = "rampant--scaleParticleCount",
            description = "rampant--scaleParticleCount",
            setting_type = "startup",
            default_value = 1,
            minimum_value = 0.01,
            maximum_value = 5.0,
            order = "n[modifier]-a[optimize]",
            per_user = false
        },

        {
            type = "bool-setting",
            name = "rampant--enableLandfillOnDeath",
            description = "rampant--reduceBloodParticles",
            setting_type = "startup",
            default_value = true,
            order = "n[modifier]-a[optimize]",
            per_user = false
        },

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

-- Converts the RGBA struct to Hexadecimal discarding the alpha value.
-- Since the RGBA struct stores values as percent (0-1) and not actual RGB (0-255) there might be precision errors when converting.
local function rgbaToHex(rgba)
    local r = string.format("%x", rgba.r * 255)
    local g = string.format("%x", rgba.g * 255)
    local b = string.format("%x", rgba.b * 255)
    local a = string.format("%x", rgba.a * 255)
    if #r == 1 then
        r = "0"..r
    end
    if #g == 1 then
        g = "0"..g
    end
    if #b == 1 then
        b = "0"..b
    end
    if #a == 1 then
        a = "0"..a
    end
    return r..g..b..a
end

-- Sets the default value of the faction tint settings.
-- Change this function if the formating of the tint settings changes.
local function setDefaultValue(tint, tint2, tint3)
    return rgbaToHex(tint) .. " " .. rgbaToHex(tint2) .. " " .. rgbaToHex(tint3)
end

local FACTIONS = {
    {
        -- Neutral faction
        type = "neutral",
        tint =  {r = 0.90, g = 0.90, b = 0.90, a = 1.00},
        tint2 = {r = 1.00, g = 1.00, b = 1.00, a = 1.00},
        tint3 = {r = 0.50, g = 0.50, b = 0.50, a = 0.20}
    },
    {
        -- Acidic faction
        type = "acid",
        tint =  {r = 1.00, g = 1.00, b = 1.00, a = 1.00},
        tint2 = {r = 0.40, g = 0.90, b = 0.40, a = 1.00},
        tint3 = {r = 0.40, g = 0.60, b = 0.40, a = 0.20}
    },
    {
        -- Laser faction
        type = "laser",
        tint =  {r = 0.60, g = 0.60, b = 0.84, a = 1.00},
        tint2 = {r = 0.60, g = 0.60, b = 0.84, a = 1.00},
        tint3 = {r = 0.30, g = 0.30, b = 0.70, a = 0.20}
    },
    {
        -- Fire faction
        type = "fire",
        tint =  {r = 1.00, g = 1.00, b = 1.00, a = 1.00},
        tint2 = {r = 0.90, g = 0.20, b = 0.20, a = 1.00},
        tint3 = {r = 0.90, g = 0.70, b = 0.70, a = 0.20}
    },
    {
        -- Inferno faction
        type = "inferno",
        tint =  {r = 0.90, g = 0.75, b = 0.75, a = 1.00},
        tint2 = {r = 0.70, g = 0.30, b = 0.30, a = 1.00},
        tint3 = {r = 0.50, g = 0.00, b = 0.00, a = 0.20}
    },
    {
        -- Wasp faction
        type = "wasp",
        tint =  {r = 1.00, g = 1.00, b = 0.70, a = 1.00},
        tint2 = {r = 0.50, g = 0.50, b = 0.00, a = 1.00},
        tint3 = {r = 0.80, g = 0.80, b = 0.00, a = 0.20}
    },
    {
        -- Spawner faction
        type = "spawner",
        tint =  {r = 1.00, g = 0.80, b = 1.00, a = 1.00},
        tint2 = {r = 0.90, g = 0.30, b = 0.90, a = 1.00},
        tint3 = {r = 0.60, g = 0.10, b = 0.60, a = 0.20}
    },
    {
        -- Electric faction
        type = "electric",
        tint =  {r = 0.80, g = 0.80, b = 1.00, a = 1.00},
        tint2 = {r = 0.30, g = 0.30, b = 1.00, a = 1.00},
        tint3 = {r = 0.10, g = 0.10, b = 0.70, a = 0.20}
    },
    {
        -- Brutal faction
        type = "physical",
        tint =  {r = 0.90, g = 0.90, b = 0.90, a = 1.00},
        tint2 = {r = 0.80, g = 0.80, b = 0.80, a = 1.00},
        tint3 = {r = 0.20, g = 0.20, b = 0.20, a = 0.20}
    },
    {
        -- Regenerative faction
        type = "troll",
        tint =  {r = 0.55, g = 0.55, b = 0.55, a = 1.00},
        tint2 = {r = 1.00, g = 0.30, b = 0.30, a = 1.00},
        tint3 = {r = 0.30, g = 0.10, b = 0.10, a = 0.20}
    },
    {
        -- Poison faction
        type = "poison",
        tint =  {r = 0.50, g = 0.70, b = 0.60, a = 1.00},
        tint2 = {r = 0.30, g = 0.90, b = 0.30, a = 1.00},
        tint3 = {r = 0.05, g = 0.30, b = 0.05, a = 0.20}
    },
    {
        -- Suicide faction
        type = "suicide",
        tint =  {r = 0.80, g = 0.80, b = 0.80, a = 1.00},
        tint2 = {r = 1.00, g = 0.50, b = 0.00, a = 1.00},
        tint3 = {r = 0.55, g = 0.30, b = 0.15, a = 0.20}
    },
    {
        -- Nuclear faction
        type = "nuclear",
        tint =  {r = 0.10, g = 0.95, b = 0.10, a = 1.00},
        tint2 = {r = 1.00, g = 0.50, b = 0.00, a = 1.00},
        tint3 = {r = 0.50, g = 0.75, b = 0.00, a = 0.20}
    },
    {
        -- Sapper faction
        type = "energyThief",
        tint =  {r = 0.50, g = 0.50, b = 0.70, a = 1.00},
        tint2 = {r = 0.40, g = 0.40, b = 0.40, a = 1.00},
        tint3 = {r = 0.10, g = 0.10, b = 0.40, a = 0.20}
    },
    {
        -- Fast faction
        type = "fast",
        tint =  {r = 0.90, g = 0.90, b = 0.90, a = 1.00},
        tint2 = {r = 1.00, g = 1.00, b = 0.10, a = 1.00},
        tint3 = {r = 0.50, g = 0.50, b = 0.00, a = 0.20}
    }
}

for i = 1, #FACTIONS do
    local entry = FACTIONS[i]
    local setting = "rampant--" .. entry.type .. "Tints"

    data:extend({
            {
                type = "string-setting",
                name = setting,
                description = setting,
                setting_type = "startup",
                default_value = setDefaultValue(entry.tint, entry.tint2, entry.tint3),
                order = "p[modifier]-r[zzunit]",
            },
    })
end
