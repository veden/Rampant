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


local swarmUtils = {}
-- imports

local sounds = require ("__base__.prototypes.entity.sounds")

local bombUtils = require("utils/BombUtils")
local attackFlame = require("utils/AttackFlame")
local energyThiefFaction = require("EnergyThief")
local poisonFaction = require("Poison")
local beamUtils = require("utils/BeamUtils")
local acidBall = require("utils/AttackBall")
local droneUtils = require("utils/DroneUtils")
local biterUtils = require("utils/BiterUtils")
local particleUtils = require("utils/ParticleUtils")
local stickerUtils = require("utils/StickerUtils")
local unitUtils = require("utils/UnitUtils")
local fireUtils = require("utils/FireUtils")

local constants = require("__Rampant__/libs/Constants")
local mathUtils = require("__Rampant__/libs/MathUtils")

-- imported constants

local TIERS = constants.TIERS

-- imported functions

local roundToNearest = mathUtils.roundToNearest

local mMax = math.max
local mMin = math.min

local distort = mathUtils.distort

local deepcopy = util.table.deepcopy

local TIER_UPGRADE_SET = constants.TIER_UPGRADE_SET

local xorRandom = mathUtils.xorRandom(constants.ENEMY_SEED)

local biterattackanimation = unitUtils.biterattackanimation

local createProjectileAttack = biterUtils.createProjectileAttack
local createCapsuleProjectile = droneUtils.createCapsuleProjectile
local makeSticker = stickerUtils.makeSticker
local makeAtomicBlast = bombUtils.makeAtomicBlast
local makeLaser = beamUtils.makeLaser
local createAttackBall = acidBall.createAttackBall
local createSpitFire = acidBall.createSpitFire
local createRangedAttack = biterUtils.createRangedAttack
local createMeleeAttack = biterUtils.createMeleeAttack
local makeAcidSplashFire = fireUtils.makeAcidSplashFire

local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable
local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable

local createSuicideAttack = biterUtils.createSuicideAttack

local createAttackFlame = attackFlame.createAttackFlame
local createStreamAttack = biterUtils.createStreamAttack

local gaussianRandomRangeRG = mathUtils.gaussianRandomRangeRG

local makeBloodFountains = particleUtils.makeBloodFountains

local makeBubble = beamUtils.makeBubble
local makeBeam = beamUtils.makeBeam
local createElectricAttack = biterUtils.createElectricAttack
local createBeamAttack = biterUtils.createBeamAttack

local makeBiter = biterUtils.makeBiter
local makeDrone = droneUtils.makeDrone
local makeSpitter = biterUtils.makeSpitter
local makeWorm = biterUtils.makeWorm
local makeUnitSpawner = biterUtils.makeUnitSpawner

-- module code

local scorchmarkTiers = {
    "small-scorchmark",
    "small-scorchmark",
    "medium-scorchmark",
    "medium-scorchmark",
    "medium-scorchmark",
    "big-scorchmark",
    "big-scorchmark",
    "big-scorchmark",
    "huge-scorchmark",
    "huge-scorchmark"
}

local explosionTiers = {
    "explosion",
    "explosion",
    "big-explosion",
    "big-explosion",
    "big-explosion",
    "big-explosion",
    "massive-explosion",
    "massive-explosion",
    "massive-explosion",
    "massive-explosion"
}

local bloodFountains = {
    "blood-explosion-small-rampant",
    "blood-explosion-small-rampant",
    "blood-explosion-small-rampant",
    "blood-explosion-small-rampant",
    "blood-explosion-big-rampant",
    "blood-explosion-big-rampant",
    "blood-explosion-big-rampant",
    "blood-explosion-huge-rampant",
    "blood-explosion-huge-rampant",
    "blood-explosion-huge-rampant",
}

local biterDyingSounds = {
    sounds.biter_dying(0.45),
    sounds.biter_dying(0.50),
    sounds.biter_dying(0.55),
    sounds.biter_dying(0.60),
    sounds.biter_dying(0.65),
    sounds.biter_dying_big(0.40),
    sounds.biter_dying_big(0.45),
    sounds.biter_dying_big(0.50),
    sounds.biter_dying_big(0.55),
    sounds.biter_dying_big(0.60)
}

local biterWorkingSounds = {
    sounds.biter_calls(0.75),
    sounds.biter_calls(0.80),
    sounds.biter_calls(0.87),
    sounds.biter_calls_big(0.62),
    sounds.biter_calls_big(0.67),
    sounds.biter_calls_big(0.73),
    sounds.biter_calls_big(0.80),
    sounds.biter_calls_behemoth(0.90),
    sounds.biter_calls_behemoth(0.95),
    sounds.biter_calls_behemoth(1)
}

local biterWalkingSounds = {
    sounds.biter_walk(0.15),
    sounds.biter_walk(0.20),
    sounds.biter_walk(0.25),
    sounds.biter_walk(0.30),
    sounds.biter_walk(0.35),
    sounds.biter_walk_big(0.70),
    sounds.biter_walk_big(0.74),
    sounds.biter_walk_big(0.78),
    sounds.biter_walk_big(0.82),
    sounds.biter_walk_big(0.86)
}

local biterRoarSounds = {
    sounds.biter_roars(0.30),
    sounds.biter_roars(0.40),
    sounds.biter_roars_mid(0.68),
    sounds.biter_roars_mid(0.73),
    sounds.biter_roars_mid(0.78),
    sounds.biter_roars_big(0.32),
    sounds.biter_roars_big(0.37),
    sounds.biter_roars_big(0.42),
    sounds.biter_roars_behemoth(0.60),
    sounds.biter_roars_behemoth(0.70)
}

local spitterDyingSounds = {
    sounds.spitter_dying(0.40),
    sounds.spitter_dying(0.50),
    sounds.spitter_dying_mid(0.55),
    sounds.spitter_dying_mid(0.65),
    sounds.spitter_dying_mid(0.75),
    sounds.spitter_dying_big(0.67),
    sounds.spitter_dying_big(0.71),
    sounds.spitter_dying_big(0.75),
    sounds.spitter_dying_behemoth(0.60),
    sounds.spitter_dying_behemoth(0.70)
}

local spitterWorkingSounds = {
    sounds.spitter_calls(0.39),
    sounds.spitter_calls(0.44),
    sounds.spitter_calls(0.49),
    sounds.spitter_calls_med(0.48),
    sounds.spitter_calls_med(0.53),
    sounds.spitter_calls_med(0.58),
    sounds.spitter_calls_med(0.63),
    sounds.spitter_calls_big(0.50),
    sounds.spitter_calls_big(0.55),
    sounds.spitter_calls_big(0.60)
}

local spitterWalkingSounds = {
    sounds.spitter_walk(0.20),
    sounds.spitter_walk(0.30),
    sounds.spitter_walk(0.40),
    sounds.spitter_walk(0.50),
    sounds.spitter_walk(0.60),
    sounds.spitter_walk_big(0.45),
    sounds.spitter_walk_big(0.50),
    sounds.spitter_walk_big(0.55),
    sounds.spitter_walk_big(0.60),
    sounds.spitter_walk_big(0.65)
}

local wormFoldSounds = {
    sounds.worm_fold(0.74),
    sounds.worm_fold(0.77),
    sounds.worm_fold(0.80),
    sounds.worm_fold(0.83),
    sounds.worm_fold(0.86),
    sounds.worm_fold(0.89),
    sounds.worm_fold(0.92),
    sounds.worm_fold(0.95),
    sounds.worm_fold(0.98),
    sounds.worm_fold(1)
}

local wormStandupSounds = {
    sounds.worm_standup_small(0.75),
    sounds.worm_standup_small(0.82),
    sounds.worm_standup_small(0.89),
    sounds.worm_standup_small(1),
    sounds.worm_standup(0.75),
    sounds.worm_standup(0.80),
    sounds.worm_standup(0.85),
    sounds.worm_standup(0.90),
    sounds.worm_standup(0.95),
    sounds.worm_standup(1)
}

local wormBreathSounds = {
    sounds.worm_breath(0.4),
    sounds.worm_breath(0.5),
    sounds.worm_breath(0.6),
    sounds.worm_breath(0.7),
    sounds.worm_breath(0.8),
    sounds.worm_breath_big(0.80),
    sounds.worm_breath_big(0.85),
    sounds.worm_breath_big(0.90),
    sounds.worm_breath_big(0.95),
    sounds.worm_breath_big(1)
}

local wormDyingSounds = {
    sounds.worm_dying_small(0.52),
    sounds.worm_dying_small(0.57),
    sounds.worm_dying_small(0.65),
    sounds.worm_dying(0.60),
    sounds.worm_dying(0.65),
    sounds.worm_dying(0.7),
    sounds.worm_dying(0.75),
    sounds.worm_dying_big(0.67),
    sounds.worm_dying_big(0.72),
    sounds.worm_dying_big(0.77)
}

local wormRoarSounds = {
    sounds.worm_roars(0.51),
    sounds.worm_roars(0.56),
    sounds.worm_roars(0.62),
    sounds.worm_roars(0.68),
    sounds.worm_roars(0.73),
    sounds.worm_roars_big(0.62),
    sounds.worm_roars_big(0.67),
    sounds.worm_roars_big(0.72),
    sounds.worm_roars_big(0.77),
    sounds.worm_roars_big(0.82)
}

local wormAltRoarSounds = {
    sounds.worm_roar_alternative(0.54),
    sounds.worm_roar_alternative(0.59),
    sounds.worm_roar_alternative(0.64),
    sounds.worm_roar_alternative(0.68),
    sounds.worm_roar_alternative(0.73),
    sounds.worm_roar_alternative_big(0.67),
    sounds.worm_roar_alternative_big(0.72),
    sounds.worm_roar_alternative_big(0.82),
    sounds.worm_roar_alternative_big(0.87),
    sounds.worm_roar_alternative_big(0.92)
}

local nuclearAttackNumeric = {
    ["damage"] = { 15, 15, 25, 25, 35, 35, 45, 45, 55, 55 },
    ["repeatCount"] = { 150, 175, 250, 300, 350, 400, 450, 500, 550, 600 },
    ["radius"] = { 5, 10, 10, 12, 14, 16, 16, 18, 18, 20 },
    ["explosionDistance"] = { 3, 3, 4, 4, 5, 5, 6, 6, 7, 7 },
    ["explosionCount"] = { 3, 3, 3, 4, 4, 4, 5, 5, 6, 6 }
}

local bombAttackNumeric = {
    ["damage"] = { 75, 112, 150, 187, 270, 360, 600, 720, 975, 1050 },
    ["radius"] = { 1.75, 1.75, 2, 2.5, 3, 3, 3.5, 3.5, 3.75, 4 },
    ["explosionDistance"] = { 2, 2, 2, 2, 2, 2.5, 2.5, 2.5, 3, 3 },
    ["explosionCount"] = { 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }
}

local streamAttackNumeric = {
    ["stickerDamagePerTick"] = { 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65 },
    ["stickerDuration"] = { 400, 410, 420, 430, 440, 450, 440, 470, 480, 490 },
    ["particleTimeout"] = { 3, 3, 4, 4, 5, 5, 6, 6, 7, 7 },
    ["fireSpreadRadius"] = { 0.75, 0.75, 0.77, 0.77, 0.79, 0.79, 0.83, 0.83, 0.85, 0.85 },
    ["damageMaxMultipler"] = { 2, 2, 2, 3, 3, 3, 4, 4, 4, 5 },
    ["fireSpreadCooldown"] = { 30, 30, 29, 29, 28, 28, 27, 27, 25, 25 },
    ["fireDamagePerTick"] = { 1, 1, 1, 2, 2, 2, 3, 3, 3, 4 },
    ["damage"] = { 4, 7.5, 11.25, 15, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5 }
}

local beamAttackNumeric = {
    ["range"] = { 11, 11, 12, 12, 13, 13, 14, 14, 15, 15 },
    ["damage"] = { 3.6, 6, 9, 12, 18, 27, 36, 45, 54, 90 },
    ["duration"] = { 20, 20, 21, 21, 22, 22, 23, 23, 24, 24 },
    ["damageInterval"] =  { 20, 20, 21, 21, 22, 22, 23, 23, 24, 24 },
    ["width"] = { 1.5, 1.5, 1.6, 1.6, 1.7, 1.7, 1.8, 1.8, 1.9, 1.9 }
}

local clusterAttackNumeric = {
    ["clusterDistance"] = { 3, 3, 4, 4, 5, 5, 6, 6, 7, 7 },
    ["clusters"] = { 5, 5, 6, 6, 7, 7, 8, 8, 9, 9 },
    ["startingSpeed"] = { 0.25, 0.25, 0.27, 0.27, 0.29, 0.29, 0.31, 0.31, 0.33, 0.33 }
}

local liteClusterAttackNumeric = {
    ["clusterDistance"] = { 2, 2, 3, 3, 3, 4, 4, 4, 5, 5 },
    ["clusters"] = { 2, 2, 3, 3, 4, 4, 5, 5, 6, 6 },
    ["startingSpeed"] = { 0.25, 0.25, 0.27, 0.27, 0.29, 0.29, 0.31, 0.31, 0.33, 0.33 },
    ["damage"] = { 4, 7.5, 11.25, 15, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5 }
}

local biterAttributeNumeric = {
    ["range"] = { 0.5, 0.5, 0.75, 0.75, 1.0, 1.0, 1.25, 1.50, 1.75, 2.0 },
    ["radius"] = { 0.5, 0.65, 0.75, 0.85, 0.95, 1.1, 1.2, 1.3, 1.4, 1.5 },
    ["cooldown"] = { 40, 41, 42, 44, 46, 48, 50, 52, 55, 57 },
    ["damage"] = { 16, 35, 65, 85, 115, 145, 175, 205.0, 235.0, 265.0 },
    ["scale"] = { 0.25, 0.40, 0.60, 0.8, 0.9, 1, 1.2, 1.4, 1.6, 1.8 },
    ["healing"] = { 0.01, 0.01, 0.015, 0.02, 0.05, 0.075, 0.1, 0.12, 0.14, 0.16 },
    ["physicalDecrease"] = { 0, 0, 4, 5, 6, 8, 11, 13, 16, 17 },
    ["physicalPercent"] = { 0, 0, 0, 10, 12, 12, 14, 16, 18, 20 },
    ["explosionDecrease"] = { 0, 0, 0, 0, 0, 10, 12, 14, 16, 20 },
    ["explosionPercent"] = { 0, 0, 0, 10, 12, 13, 15, 16, 17, 20 },
    -- ["distancePerFrame"] = { 0.1, 0.125, 0.15, 0.19, 0.195, 0.2, 0.2, 0.2, 0.2, 0.2 },
    ["distancePerFrame"] = { 0.08, 0.10, 0.125, 0.15, 0.18, 0.195, 0.2, 0.2, 0.2, 0.2 },
    ["movement"] = { 0.2, 0.19, 0.185, 0.18, 0.175, 0.17, 0.17, 0.17, 0.17, 0.17 },
    ["health"] = { 15, 75, 150, 250, 1000, 2000, 3500, 7500, 15000, 30000 },
    ["pollutionToAttack"] = { 10, 40, 80, 120, 200, 300, 450, 550, 650, 750 },
    ["spawningTimeModifer"] = { 1, 1, 2, 2, 4, 4, 6, 6, 8, 8 }
}

local acidPuddleAttributeNumeric = {
    ["damagePerTick"] = { 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5 },
    ["stickerDamagePerTick"] = { 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5 },
    ["stickerDuration"] = { 600, 610, 620, 630, 640, 650, 660, 670, 680, 690 }
}

local spitterAttributeNumeric = {
    ["range"] = { 11, 11, 11, 12, 12, 12, 13, 13, 14, 14 },
    ["radius"] = { 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.5 },
    ["cooldown"] = { 100, 100, 97, 97, 95, 95, 93, 93, 90, 90 },
    ["warmup"] = { 30, 29, 28, 27, 26, 25, 24, 23, 22, 21 },
    ["stickerDuration"] = { 600, 610, 620, 630, 640, 650, 660, 670, 680, 690 },
    ["damagePerTick"] = { 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1 },
    ["stickerDamagePerTick"] = { 0.025, 0.05, 0.075, 0.1, 0.125, 0.15, 0.175, 0.2, 0.225, 0.25 },
    ["stickerMovementModifier"] = { 0.97, 0.96, 0.95, 0.94, 0.93, 0.92, 0.91, 0.90, 0.89, 0.88 },
    ["damage"] = { 4, 7.5, 11.25, 15, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5 },
    ["particleVerticalAcceleration"] = { 0.01, 0.01, 0.02, 0.02, 0.03, 0.03, 0.04, 0.04, 0.05, 0.05 },
    ["particleHoizontalSpeed"] = { 0.6, 0.6, 0.7, 0.7, 0.8, 0.8, 0.9, 0.9, 1, 1 },
    ["particleHoizontalSpeedDeviation"] = { 0.0025, 0.0025, 0.0024, 0.0024, 0.0023, 0.0023, 0.0022, 0.0022, 0.0021, 0.0021 },
    ["scale"] = { 0.25, 0.40, 0.60, 0.8, 0.9, 1, 1.2, 1.4, 1.6, 1.8 },
    ["healing"] = { 0.01, 0.01, 0.015, 0.02, 0.05, 0.075, 0.1, 0.12, 0.14, 0.16 },
    ["physicalDecrease"] = { 0, 0, 0, 0, 2, 4, 6, 8, 10, 12 },
    ["physicalPercent"] = { 0, 0, 0, 10, 12, 12, 14, 14, 15, 15 },
    ["explosionPercent"] = { 0, 0, 10, 10, 20, 20, 30, 30, 40, 40 },
    ["distancePerFrame"] = { 0.04, 0.045, 0.050, 0.055, 0.060, 0.065, 0.067, 0.069, 0.071, 0.073 },
    ["movement"] = { 0.185, 0.18, 0.18, 0.17, 0.17, 0.16, 0.16, 0.15, 0.15, 0.14 },
    ["health"] = { 10, 50, 200, 350, 1250, 2250, 3250, 6500, 12500, 25000 },
    ["pollutionToAttack"] = { 10, 40, 80, 120, 200, 300, 450, 550, 650, 750 },
    ["spawningTimeModifer"] = { 1, 1, 2, 2, 4, 4, 6, 6, 8, 8 }
}

local droneAttributeNumeric = {
    ["scale"] = { 0.3, 0.32, 0.34, 0.36, 0.4, 0.42, 0.44, 0.5, 0.52, 0.54 },
    ["particleVerticalAcceleration"] = { 0.01, 0.01, 0.02, 0.02, 0.03, 0.03, 0.04, 0.04, 0.05, 0.05 },
    ["particleHoizontalSpeed"] = { 0.6, 0.6, 0.7, 0.7, 0.8, 0.8, 0.9, 0.9, 1, 1 },
    ["particleHoizontalSpeedDeviation"] = { 0.0025, 0.0025, 0.0024, 0.0024, 0.0023, 0.0023, 0.0022, 0.0022, 0.0021, 0.0021 },
    ["stickerDuration"] = { 600, 610, 620, 630, 640, 650, 660, 670, 680, 690 },
    ["stickerMovementModifier"] = { 0.8, 0.8, 0.75, 0.75, 0.7, 0.7, 0.65, 0.65, 0.5, 0.5 },
    ["damagePerTick"] = { 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1 },
    ["cooldown"] = { 60, 60, 55, 55, 50, 50, 45, 45, 40, 40 },
    ["warmup"] = { 20, 19, 18, 17, 16, 15, 14, 13, 12, 11 },
    ["ttl"] = { 300, 300, 350, 350, 400, 400, 450, 450, 500, 500 },
    ["damage"] = { 2, 4, 7, 13, 15, 18, 22, 28, 35, 40 },
    ["movement"] = { 0.06, 0.06, 0.07, 0.07, 0.08, 0.08, 0.09, 0.09, 0.1, 0.1 },
    ["distancePerFrame"] = { 0.1, 0.1, 0.105, 0.105, 0.110, 0.110, 0.112, 0.112, 0.114, 0.114 },
    ["rangeFromPlayer"] = { 9, 9, 10, 10, 11, 11, 12, 12, 13, 13 },
    ["range"] = { 10, 10, 11, 11, 12, 12, 13, 13, 14, 14 },
    ["radius"] = { 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.5 },
    ["health"] = { 20, 150, 300, 450, 600, 800, 1000, 2000, 4000, 8000 },
    ["healing"] = { 0.01, 0.01, 0.015, 0.02, 0.05, 0.075, 0.1, 0.12, 0.14, 0.16 },
}

local unitSpawnerAttributeNumeric = {
    ["health"] = { 350, 500, 750, 1500, 3500, 7500, 11000, 20000, 30000, 45000 },
    ["healing"] = { 0.02, 0.02, 0.022, 0.024, 0.026, 0.028, 0.03, 0.032, 0.034, 0.036 },
    ["spawningCooldownStart"] = { 2160, 2150, 2140, 2130, 2120, 2110, 2100, 2090, 2080, 2070 },
    ["spawningCooldownEnd"] = { 700, 690, 680, 670, 660, 650, 640, 630, 620, 610 },
    ["unitsToSpawn"] = { 11, 11, 12, 12, 13, 13, 14, 14, 15, 15 },
    ["cooldown"] = { 50, 50, 45, 45, 40, 40, 35, 35, 30, 30 },
    ["unitsOwned"] = { 9, 9, 10, 10, 11, 11, 12, 12, 13, 13 },
    ["physicalDecrease"] = { 1, 2, 3, 4, 6, 6, 8, 10, 12, 14 },
    ["physicalPercent"] = { 15, 15, 17, 17, 18, 18, 19, 19, 20, 20 },
    ["explosionDecrease"] = { 5, 5, 6, 6, 7, 7, 8, 8, 9, 9 },
    ["explosionPercent"] = { 15, 15, 17, 17, 18, 18, 19, 19, 20, 20 },
    ["fireDecrease"] = { 3, 3, 4, 4, 4, 4, 4, 4, 5, 5 },
    ["firePercent"] = { 40, 40, 42, 42, 43, 43, 44, 44, 45, 45 },
    ["evolutionRequirement"] = { 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 },
    ["scale"] = { 0.80, 0.85, 0.90, 0.95, 1, 1.05, 1.1, 1.15, 1.2, 1.25 }
}

local hiveAttributeNumeric = {
    ["health"] = { 700, 1000, 1500, 3000, 7000, 15000, 22000, 40000, 60000, 90000 },
    ["healing"] = { 0.02, 0.02, 0.022, 0.024, 0.026, 0.028, 0.03, 0.032, 0.034, 0.036 },
    ["spawningCooldownStart"] = { 2485, 2465, 2445, 2425, 2405, 2390, 2370, 2350, 2330, 2310 },
    ["spawningRadius"] = { 10, 13, 15, 17, 20, 23, 26, 29, 32, 35 },
    ["spawningSpacing"] = { 5, 5, 5, 6, 6, 6, 7, 7, 7, 8 },
    ["spawningCooldownEnd"] = { 892, 890, 887, 885, 882, 880, 877, 875, 872, 870 },
    ["unitsToSpawn"] = { 3000, 3000, 300, 3000, 3000, 3000, 3000, 3000, 3000, 3000 },
    ["scale"] = { 1.0, 1.05, 1.1, 1.15, 1.20, 1.25, 1.30, 1.35, 1.4, 1.45 },
    ["unitsOwned"] = { 7, 7, 8, 8, 9, 9, 10, 10, 11, 11 },
    ["physicalDecrease"] = { 1, 2, 3, 4, 6, 6, 8, 10, 12, 14 },
    ["physicalPercent"] = { 15, 15, 17, 17, 18, 18, 19, 19, 20, 20 },
    ["explosionDecrease"] = { 5, 5, 6, 6, 7, 7, 8, 8, 9, 9 },
    ["explosionPercent"] = { 15, 15, 17, 17, 18, 18, 19, 19, 20, 20 },
    ["fireDecrease"] = { 3, 3, 4, 4, 4, 4, 4, 4, 5, 5 },
    ["firePercent"] = { 40, 40, 42, 42, 43, 43, 44, 44, 45, 45 },
    ["evolutionRequirement"] = { 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 },
    ["cooldown"] = { 50, 50, 45, 45, 40, 40, 35, 35, 30, 30 }
}

local wormAttributeNumeric = {
    ["stickerDuration"] = { 800, 810, 820, 830, 840, 850, 860, 870, 880, 890 },
    ["stickerMovementModifier"] = { 0.8, 0.8, 0.75, 0.75, 0.7, 0.7, 0.65, 0.65, 0.5, 0.5 },
    ["damagePerTick"] = { 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1 },
    ["range"] = { 25, 27, 31, 33, 35, 36, 37, 38, 39, 40 },
    ["cooldown"] = { 70, 70, 68, 66, 64, 62, 60, 58, 56, 54 },
    ["damage"] = { 12, 22.5, 33.75, 45, 67.5, 82.5, 97.5, 112.5, 127.5, 142.5 },
    ["scale"] = { 0.75, 0.85, 0.95, 1.05, 1.15, 1.25, 1.35, 1.45, 1.55, 1.65 },
    ["radius"] = { 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.2, 2.3, 2.5, 3.0 },
    ["stickerDamagePerTick"] = { 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5 },
    ["particleVerticalAcceleration"] = { 0.01, 0.01, 0.02, 0.02, 0.03, 0.03, 0.04, 0.04, 0.05, 0.05 },
    ["particleHoizontalSpeed"] = { 0.6, 0.6, 0.7, 0.7, 0.8, 0.8, 0.9, 0.9, 1, 1 },
    ["particleHoizontalSpeedDeviation"] = { 0.0025, 0.0025, 0.0024, 0.0024, 0.0023, 0.0023, 0.0022, 0.0022, 0.0021, 0.0021 },
    ["foldingSpeed"] = { 0.15, 0.15, 0.16, 0.16, 0.16, 0.17, 0.17, 0.18, 0.18, 0.19 },
    ["preparingSpeed"] = { 0.025, 0.025, 0.026, 0.026, 0.027, 0.027, 0.028, 0.028, 0.029, 0.029 },
    ["physicalDecrease"] = { 0, 0, 5, 5, 8, 8, 10, 10, 12, 12 },
    ["explosionDecrease"] = { 0, 0, 5, 5, 8, 8, 10, 10, 12, 12 },
    ["explosionPercent"] = { 0, 0, 10, 10, 20, 20, 30, 30, 40, 40 },
    ["fireDecrease"] = { 3, 3, 4, 4, 5, 5, 5, 5, 5, 6 },
    ["health"] = { 200, 350, 500, 750, 2000, 3500, 7500, 12000, 20000, 25000 },
    ["firePercent"] = { 40, 40, 42, 42, 43, 43, 44, 44, 45, 45 },
    ["healing"] = { 0.01, 0.01, 0.015, 0.02, 0.05, 0.075, 0.1, 0.12, 0.14, 0.16 },
    ["evolutionRequirement"] = { 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 }
}

local propTables = {
    {{0, 1}, {0.65, 0.0}},
    {{0.3, 0}, {0.35, 0.5}, {0.80, 0.0}},
    {{0.4, 0}, {0.45, 0.5}, {0.90, 0.0}},
    {{0.5, 0}, {0.55, 0.5}, {0.90, 0.0}},
    {{0.6, 0}, {0.65, 0.5}, {0.95, 0.0}},
    {{0.7, 0}, {0.75, 0.5}, {0.975, 0.0}},
    {{0.8, 0}, {0.825, 0.5}, {0.975, 0.0}},
    {{0.85, 0}, {0.875, 0.5}, {0.975, 0.0}},
    {{0.90, 0}, {0.925, 0.5}, {0.975, 0.0}},
    {{0.93, 0}, {1, 1.0}}
}

local function mapTierToProbability(lst, reduce)
    if reduce then
        for i, item in pairs(lst) do
            local adjustmentRatio = 1
            if (reduce > 0) then
                adjustmentRatio = 1 - (0.05 * reduce)
            elseif (reduce == 0) then
                adjustmentRatio = 0.975
            end
            lst[i] = {item[1] * adjustmentRatio, item[2]}
            if item[3] then
                lst[i][3] = item[3]
            end
        end
        return lst
    else
        return lst
    end
end

local function fillUnitTable(spawnerTier, result, unitSet, tier, probability)
    for x=1,#unitSet[tier] do
        if (tier == 1) then
            result[#result+1] = {
                unitSet[tier][x],
                probability
            }
        else
            result[#result+1] = {
                unitSet[tier][x],
                mapTierToProbability(probability, spawnerTier - tier)
            }
        end
    end
end

local function unitSetToProbabilityTable(unitSet, tier)
    local result = {}

    fillUnitTable(tier, result, unitSet, 1, {{0, 1}, {0.50, 0.0}})
    fillUnitTable(tier, result, unitSet, 2, {{0.3, 0}, {0.35, 1.0}, {0.80, 0.0}})
    fillUnitTable(tier, result, unitSet, 3, {{0.4, 0}, {0.45, 1.0}, {0.90, 0.0}})
    fillUnitTable(tier, result, unitSet, 4, {{0.5, 0}, {0.55, 1.0}, {0.90, 0.0}})
    fillUnitTable(tier, result, unitSet, 5, {{0.6, 0}, {0.65, 1.0}, {0.95, 0.0}})
    fillUnitTable(tier, result, unitSet, 6, {{0.7, 0}, {0.75, 1.0}, {0.975, 0.0}})
    fillUnitTable(tier, result, unitSet, 7, {{0.8, 0}, {0.825, 1.0}, {0.975, 0.0}})
    fillUnitTable(tier, result, unitSet, 8, {{0.85, 0}, {0.875, 1.0}, {0.975, 0.0}})
    fillUnitTable(tier, result, unitSet, 9, {{0.90, 0}, {0.925, 1.0}, {0.975, 0.0}})
    fillUnitTable(tier, result, unitSet, 10, {{0.93, 0}, {1, 1.0}})

    return result
end

local function addMajorResistance(entity, name, tier)
    local decreases = { 7, 7, 10, 10, 13, 13, 16, 16, 19, 23 }
    local percents = { 80, 80, 84, 84, 89, 89, 93, 93, 97, 97 }
    entity.resistances[name] = {
        decrease = roundToNearest(gaussianRandomRangeRG(decreases[tier], decreases[tier] * 0.1, decreases[tier] * 0.85, decreases[tier] * 1.30, xorRandom), 0.1),
        percent = roundToNearest(gaussianRandomRangeRG(percents[tier], percents[tier] * 0.1, percents[tier] * 0.85, 0.99, xorRandom), 0.1)
    }
end

local function addMinorResistance(entity, name, tier)
    local decreases = { 3, 3, 7, 7, 10, 10, 13, 13, 16, 18 }
    local percents = { 40, 40, 44, 44, 48, 48, 52, 56, 56, 60 }
    entity.resistances[name] = {
        decrease = roundToNearest(gaussianRandomRangeRG(decreases[tier], decreases[tier] * 0.1, decreases[tier] * 0.85, decreases[tier] * 1.30, xorRandom), 0.1),
        percent = roundToNearest(gaussianRandomRangeRG(percents[tier], percents[tier] * 0.1, percents[tier] * 0.85, percents[tier] * 1.30, xorRandom), 0.1)
    }
end

local function addMajorWeakness(entity, name, tier)
    local decreases = { -7, -7, -10, -10, -13, -13, -16, -16, -19, -23 }
    local percents = { -65, -65, -70, -75, -75, -80, -85, -85, -90, -90 }
    entity.resistances[name] = {
        decrease = roundToNearest(gaussianRandomRangeRG(decreases[tier], decreases[tier] * 0.1, decreases[tier] * 0.85, decreases[tier] * 1.30, xorRandom), 0.1),
        percent = roundToNearest(gaussianRandomRangeRG(percents[tier], percents[tier] * 0.1, percents[tier] * 0.85, percents[tier] * 1.30, xorRandom), 0.1)
    }
end

local function addMinorWeakness(entity, name, tier)
    local decreases = { -3, -3, -7, -7, -10, -10, -13, -13, -16, -18 }
    local percents = { -40, -40, -44, -44, -48, -48, -52, -56, -56, -60 }
    entity.resistances[name] = {
        decrease = roundToNearest(gaussianRandomRangeRG(decreases[tier], decreases[tier] * 0.1, decreases[tier] * 0.85, decreases[tier] * 1.30, xorRandom), 0.1),
        percent = roundToNearest(gaussianRandomRangeRG(percents[tier], percents[tier] * 0.1, percents[tier] * 0.85, percents[tier] * 1.30, xorRandom), 0.1)
    }
end


local function scaleAttributes (entity)
    if (entity.type == "biter") then
        entity["health"] = entity["health"] * settings.startup["rampant--unitBiterHealthScaler"].value
        entity["movement"] = entity["movement"] * settings.startup["rampant--unitBiterSpeedScaler"].value
        entity["distancePerFrame"] = entity["distancePerFrame"] * settings.startup["rampant--unitBiterSpeedScaler"].value
        entity["damage"] = entity["damage"] * settings.startup["rampant--unitBiterDamageScaler"].value
        entity["range"] = entity["range"] * settings.startup["rampant--unitBiterRangeScaler"].value
        entity["healing"] = entity["healing"] * settings.startup["rampant--unitBiterHealingScaler"].value
    elseif (entity.type == "spitter") then
        entity["health"] = entity["health"] * settings.startup["rampant--unitSpitterHealthScaler"].value
        entity["movement"] = entity["movement"] * settings.startup["rampant--unitSpitterSpeedScaler"].value
        entity["distancePerFrame"] = entity["distancePerFrame"] * settings.startup["rampant--unitSpitterSpeedScaler"].value
        entity["damage"] = entity["damage"] * settings.startup["rampant--unitSpitterDamageScaler"].value
        if entity["stickerDamagePerTick"] then
            entity["stickerDamagePerTick"] = entity["stickerDamagePerTick"] * settings.startup["rampant--unitSpitterDamageScaler"].value
        end
        entity["damagePerTick"] = entity["damagePerTick"] * settings.startup["rampant--unitSpitterDamageScaler"].value
        entity["range"] = entity["range"] * settings.startup["rampant--unitSpitterRangeScaler"].value
        entity["healing"] = entity["healing"] * settings.startup["rampant--unitSpitterHealingScaler"].value
    elseif (entity.type == "drone") then
        entity["health"] = entity["health"] * settings.startup["rampant--unitDroneHealthScaler"].value
        entity["movement"] = entity["movement"] * settings.startup["rampant--unitDroneSpeedScaler"].value
        entity["distancePerFrame"] = entity["distancePerFrame"] * settings.startup["rampant--unitDroneSpeedScaler"].value
        entity["damage"] = entity["damage"] * settings.startup["rampant--unitDroneDamageScaler"].value
        entity["damagePerTick"] = entity["damagePerTick"] * settings.startup["rampant--unitDroneDamageScaler"].value
        entity["range"] = entity["range"] * settings.startup["rampant--unitDroneRangeScaler"].value
        entity["healing"] = entity["healing"] * settings.startup["rampant--unitDroneHealingScaler"].value
    elseif (entity.type == "biter-spawner") or (entity.type == "spitter-spawner") then
        entity["health"] = entity["health"] * settings.startup["rampant--unitSpawnerHealthScaler"].value
        entity["unitsOwned"] = entity["unitsOwned"] * settings.startup["rampant--unitSpawnerOwnedScaler"].value
        entity["unitsToSpawn"] = entity["unitsToSpawn"] * settings.startup["rampant--unitSpawnerSpawnScaler"].value
        entity["spawningCooldownStart"] = entity["spawningCooldownStart"] * settings.startup["rampant--unitSpawnerRespawnScaler"].value
        entity["spawningCooldownEnd"] = entity["spawningCooldownEnd"] * settings.startup["rampant--unitSpawnerRespawnScaler"].value
        entity["healing"] = entity["healing"] * settings.startup["rampant--unitSpawnerHealingScaler"].value
    elseif (entity.type == "turret") then
        entity["health"] = entity["health"] * settings.startup["rampant--unitWormHealthScaler"].value
        entity["damage"] = entity["damage"] * settings.startup["rampant--unitWormDamageScaler"].value
        entity["damagePerTick"] = entity["damagePerTick"] * settings.startup["rampant--unitWormDamageScaler"].value
        entity["range"] = entity["range"] * settings.startup["rampant--unitWormRangeScaler"].value
        entity["prepareRange"] = entity["range"] + 2
        entity["healing"] = entity["healing"] * settings.startup["rampant--unitWormHealingScaler"].value
    elseif (entity.type == "hive") then
        entity["health"] = entity["health"] * settings.startup["rampant--unitHiveHealthScaler"].value
        entity["healing"] = entity["healing"] * settings.startup["rampant--unitHiveHealingScaler"].value
        entity["spawningCooldownStart"] = entity["spawningCooldownStart"] * settings.startup["rampant--unitHiveRespawnScaler"].value
        entity["spawningCooldownEnd"] = entity["spawningCooldownEnd"] * settings.startup["rampant--unitHiveRespawnScaler"].value
    end
end

local function fillEntityTemplate(entity)
    local tier = entity.effectiveLevel

    if (entity.type == "biter") then
        for key,value in pairs(biterAttributeNumeric) do
            if not entity[key] then
                entity[key] = distort(xorRandom, value[tier])
            else
                entity[key] = distort(xorRandom, entity[key][tier])
            end
        end
    elseif (entity.type == "spitter") then
        for key,value in pairs(spitterAttributeNumeric) do
            if not entity[key] then
                entity[key] = distort(xorRandom, value[tier])
            else
                entity[key] = distort(xorRandom, entity[key][tier])
            end
        end
    elseif (entity.type == "biter-spawner") or (entity.type == "spitter-spawner") then
        for key,value in pairs(unitSpawnerAttributeNumeric) do
            if not entity[key] then
                entity[key] = distort(xorRandom, value[tier])
            else
                entity[key] = distort(xorRandom, entity[key][tier])
            end
        end
    elseif (entity.type == "hive") then
        for key,value in pairs(hiveAttributeNumeric) do
            if not entity[key] then
                entity[key] = distort(xorRandom, value[tier])
            else
                entity[key] = distort(xorRandom, entity[key][tier])
            end
        end
    elseif (entity.type == "turret") then
        for key,value in pairs(wormAttributeNumeric) do
            if not entity[key] then
                entity[key] = distort(xorRandom, value[tier])
            else
                entity[key] = distort(xorRandom, entity[key][tier])
            end
        end
    elseif (entity.type == "drone") then
        for key,value in pairs(droneAttributeNumeric) do
            if not entity[key] then
                entity[key] = distort(xorRandom, value[tier])
            else
                entity[key] = distort(xorRandom, entity[key][tier])
            end
        end
    end

    for k,v in pairs(entity) do
        local startDecrease = string.find(k, "Decrease")
        local startPercent = string.find(k, "Percent")
        if startDecrease or startPercent then
            local damageType = string.sub(k, 1, (startDecrease or startPercent)-1)
            if not entity.resistances[damageType] then
                entity.resistances[damageType] = {}
            end

            if startDecrease then
                entity.resistances[damageType].decrease = v
            elseif startPercent then
                entity.resistances[damageType].percent = v
            end
        end
    end

    for i=1,#entity.addon do
        for k,v in pairs(entity.addon[i]) do
            entity[k] = v[tier]
        end
    end

    for key, value in pairs(entity) do
        if (key == "drops") then
            if not entity.loot then
                entity.loot = {}
            end
            for _,lootTable in pairs(entity.drops) do
                entity.loot[#entity.loot+1] = lootTable[tier]
            end
        elseif (key == "explosion") then
            local ti = tier
            if (entity.type == "drone") then
                ti = 1
            end
            entity["hitSprayName"] = entity.faction .. "-" .. "damaged-fountain-rampant"
            entity["explosion"] = entity.faction .. "-" .. bloodFountains[ti]
        elseif (key == "evolutionFunction") then
            entity["evolutionRequirement"] = distort(xorRandom, value(tier))
        elseif (key == "majorResistances") then
            for i=1,#value do
                addMajorResistance(entity, value[i], tier)
            end
        elseif (key == "minorResistances") then
            for i=1,#value do
                addMinorResistance(entity, value[i], tier)
            end
        elseif (key == "majorWeaknesses") then
            for i=1,#value do
                addMajorWeakness(entity, value[i], tier)
            end
        elseif (key == "minorWeaknesses") then
            for i=1,#value do
                addMinorWeakness(entity, value[i], tier)
            end
        elseif (key == "explosionTiers") then
            entity.attackExplosion = entity.explosionTiers[tier]
            entity.attackScorchmark = entity.scorchmarkTiers[tier]
        end
    end

    if entity.attributes then
        for i=1,#entity.attributes do
            local attribute = entity.attributes[i]
            if (attribute == "lowHealth") then
                entity["health"] = entity["health"] * 0.75
            elseif (attribute == "lowestHealth") then
                entity["health"] = entity["health"] * 0.50
            elseif (attribute == "quickCooldown") then
                entity["cooldown"] = entity["cooldown"] * 0.50
                entity["damage"] = entity["damage"] * 0.65
                if entity["damagePerTick"] then
                    entity["damagePerTick"] = entity["damagePerTick"] * 0.65
                end
            elseif (attribute == "slowMovement") then
                entity["movement"] = entity["movement"] * 0.35
                entity["distancePerFrame"] = entity["distancePerFrame"] * 0.65
            elseif (attribute == "quickMovement") then
                entity["movement"] = entity["movement"] * 1.25
                entity["distancePerFrame"] = entity["distancePerFrame"] * 1.35
            elseif (attribute == "quickSpawning") then
                if entity["spawningCooldownStart"] then
                    entity["spawningCooldownStart"] = entity["spawningCooldownStart"] * 0.85
                    entity["spawningCooldownEnd"] = entity["spawningCooldownEnd"] * 0.85
                end
                if entity["spawningTimeModifer"] then
                    entity["spawningTimeModifer"] = entity["spawningTimeModifer"] * 0.85
                end
            elseif (attribute == "altBiterArmored") then
                entity["altBiter"] = "armored"
            elseif (attribute == "highRegen") then
                entity["healing"] = entity["healing"] * 1.5
            elseif (attribute == "highestRegen") then
                entity["healing"] = entity["healing"] * 3.5
            elseif (attribute == "big") then
                entity["scale"] = entity["scale"] * 1.2
            elseif (attribute == "bigger") then
                entity["scale"] = entity["scale"] * 1.35
            elseif (attribute == "longReach") then
                entity["range"] = entity["range"] * 3
            elseif (attribute == "smallest") then
                entity["scale"] = entity["scale"] * 0.5
            elseif (attribute == "fragile") then
                entity["health"] = entity["health"] * 0.35
                entity["explosion"] = entity.faction .. "-" .. "damaged-fountain-rampant"
            elseif (attribute == "selfDamaging") then
                local divider
                if entity.health < 100 then
                    divider = 5
                else
                    divider = 7
                end
                entity.healthDamage = entity.health / divider
                entity.sourceEffect = function (attributes)
                    return
                        {
                            {
                                type = "damage",
                                affects_target = true,
                                damage = {amount = attributes.healthDamage or 5, type = attributes.damageType or "physical"}
                            }
                        }
                end
            elseif (attribute == "killsSelf") then
                entity.healthDamage = entity.health * 3
            elseif (attribute == "unstable") then
                entity["healing"] = entity["healing"] * -1
            elseif (attribute == "checkBuildability") then
                entity.checkBuildability = true
            elseif (attribute == "spawnDuringDays") then
                entity.minSpawnDarkness = 0
                entity.maxSpawnDarkness = 0.45
            elseif (attribute == "followsPlayer") then
                entity.followsPlayer = true
            elseif (attribute == "twoUnits") then
                entity.unitsOwned = 2
                entity.unitsToSpawn = 30
            elseif (attribute == "stationary") then
                entity.movement = 0
                entity.distancePerFrame = 0
            elseif (attribute == "highHealth") then
                entity["health"] = entity["health"] * 1.50
            elseif (attribute == "poisonDeathCloud") then
                entity.dyingEffect = {
                    type = "create-entity",
                    entity_name = "poison-cloud-v" .. tier .. "-cloud-rampant"
                }
            elseif (attribute == "highestHealth") then
                entity["health"] = entity["health"] * 2
            elseif (attribute == "skipKillCount") then
                if not entity["appendFlags"] then
                    entity["appendFlags"] = {}
                end
                entity["appendFlags"]["not-in-kill-statistics"] = true
            elseif type(attribute) == "table" then
                if (attribute[1] == "clusterDeath") then
                    entity.deathGenerator = function (attack)
                        return {
                            {
                                type = "cluster",
                                cluster_count = attack.clusters,
                                distance = attack.clusterDistance,
                                distance_deviation = 3,
                                action_delivery =
                                    {
                                        type = "projectile",
                                        projectile = createCapsuleProjectile(attack,
                                                                             attack.faction .. "-" .. attribute[2]
                                                                             .. "-v" .. attack.variation .. "-t"
                                                                             .. attack.effectiveLevel .. "-rampant"),
                                        direction_deviation = 0.6,
                                        starting_speed = 0.25,
                                        max_range = attack.range,
                                        starting_speed_deviation = 0.3
                                    }
                            }
                        }
                    end
                else
                    error("Unknown table attribute " .. attribute[1])
                end
            else
                error("Unknown attribute " .. attribute)
            end
        end
    end

    scaleAttributes(entity)
end

local function calculateRGBa(tint, tier, staticAlpha, invert)
    local stdDevMultipler = 0.10 + (0.005 * tier)
    local lowMultipler = 0.85 - (0.005 * tier)
    local r = gaussianRandomRangeRG(tint.r,
                                    tint.r * stdDevMultipler,
                                    mMax(tint.r * lowMultipler, 0),
                                    mMin(tint.r * 1.15, 1), xorRandom)
    local g = gaussianRandomRangeRG(tint.g,
                                    tint.g * stdDevMultipler,
                                    mMax(tint.g * lowMultipler, 0),
                                    mMin((tint.g * 1.15), 1), xorRandom)
    local b = gaussianRandomRangeRG(tint.b,
                                    tint.b * stdDevMultipler,
                                    mMax(tint.b * lowMultipler, 0),
                                    mMin(tint.b * 1.15, 1), xorRandom)
    local a = tint.a
    if not staticAlpha then
        a = gaussianRandomRangeRG(tint.a,
                                  tint.a * stdDevMultipler,
                                  mMax(tint.a * lowMultipler, 0),
                                  mMin(tint.a * 1.15, 1), xorRandom)
    end

    if invert then
        return { r=(1-r), g=(1-g), b=(1-b), a=(1-a) }
    else
        return { r=r, g=g, b=b, a=a }
    end
end

local function generateApperance(unit, invert)
    local tier = unit.effectiveLevel
    if unit.scale then
        local scaleValue = unit.scale[tier]
        local scale = gaussianRandomRangeRG(scaleValue, scaleValue * 0.03, scaleValue * 0.15, scaleValue * 1.15, xorRandom)
        unit.scale = scale
    end
    if invert then
        if unit.tint then
            unit.tint = calculateRGBa(unit.tint3, tier, true) -- calculateRGBa(unit.tint2, tier, true, true)
        end
        if unit.tint2 then
            unit.tint2 = {r=1,g=1,b=1,a=1}
        end
    else
        if unit.tint then
            unit.tint = calculateRGBa(unit.tint, tier, true)
        end
        if unit.tint2 then
            unit.tint2 = calculateRGBa(unit.tint2, tier, true)
        end
    end
end

local function buildUnits(template)
    local unitSet = {}

    local variations = constants.ENEMY_VARIATIONS

    for tier=1, TIERS do
        local effectiveLevel = TIER_UPGRADE_SET[tier]
        local result = {}

        for i=1,variations do
            local unit = deepcopy(template)
            unit.name = unit.name .. "-v" .. i .. "-t" .. tier
            unit.tier = tier
            unit.effectiveLevel = effectiveLevel
            unit.variation = i
            generateApperance(unit)
            fillEntityTemplate(unit)
            unit.roarSounds = biterRoarSounds[effectiveLevel]
            unit.attack = unit.attackGenerator(unit)
            unit.death = (unit.deathGenerator and unit.deathGenerator(unit)) or nil

            local entity
            if (unit.type == "spitter") then
                unit.dyingSounds = spitterDyingSounds[effectiveLevel]
                unit.workingSounds = spitterWorkingSounds[effectiveLevel]
                unit.walkingSounds = spitterWalkingSounds[effectiveLevel]
                entity = makeSpitter(unit)
            elseif (unit.type == "biter") then
                unit.dyingSounds = biterDyingSounds[effectiveLevel]
                unit.workingSounds = biterWorkingSounds[effectiveLevel]
                unit.walkingSounds = biterWalkingSounds[effectiveLevel]
                unit.roarSounds = biterRoarSounds[effectiveLevel]
                entity = makeBiter(unit)
            elseif (unit.type == "drone") then
                -- if not unit.death then
                --     unit.death = {
                --         type = "direct",
                --         action_delivery =
                --             {
                --                 type = "instant",
                --                 target_effects =
                --                     {
                --                         type = "create-entity",
                --                         entity_name = "massive-explosion"
                --                     }
                --             }
                --     }
                -- end
                entity = makeDrone(unit)
            end

            result[#result+1] = entity.name

            data:extend({entity})
        end

        unitSet[#unitSet+1] = result
    end

    return unitSet
end

local function buildEntitySpawner(template, tierBonus)
    local variations = constants.ENEMY_VARIATIONS

    for tier=1, TIERS do
        local effectiveLevel = TIER_UPGRADE_SET[tier]

        for i=1,variations do
            local unitSpawner = deepcopy(template)
            unitSpawner.name = unitSpawner.name .. "-v" .. i .. "-t" .. tier
            unitSpawner.tier = tier
            unitSpawner.effectiveLevel = effectiveLevel
            unitSpawner.variation = i
            generateApperance(unitSpawner, true)
            local unitTable = unitSetToProbabilityTable(
                template.unitSet,
                tier + (tierBonus or 0)
            )
            unitSpawner.unitSet = unitTable
            unitSpawner.death = (unitSpawner.deathGenerator and unitSpawner.deathGenerator(unitSpawner)) or nil
            fillEntityTemplate(unitSpawner)

            if unitSpawner.autoplace then
                unitSpawner.autoplace = unitSpawner.autoplace[effectiveLevel]
            end

            data:extend({
                    makeUnitSpawner(unitSpawner)
            })
        end
    end
end

local function buildUnitSpawner(template)
    local variations = constants.ENEMY_VARIATIONS

    for tier=1, TIERS do
        local effectiveLevel = TIER_UPGRADE_SET[tier]

        for i=1,variations do
            local unitSpawner = deepcopy(template)
            unitSpawner.name = unitSpawner.name .. "-v" .. i .. "-t" .. tier
            unitSpawner.tier = tier
            unitSpawner.effectiveLevel = effectiveLevel
            unitSpawner.variation = i
            local unitTable = unitSetToProbabilityTable(template.unitSet, tier)
            unitSpawner.unitSet = unitTable
            generateApperance(unitSpawner)
            unitSpawner.death = (unitSpawner.deathGenerator and unitSpawner.deathGenerator(unitSpawner)) or nil
            fillEntityTemplate(unitSpawner)

            if unitSpawner.autoplace then
                unitSpawner.autoplace = unitSpawner.autoplace[effectiveLevel]
            end

            data:extend({
                    makeUnitSpawner(unitSpawner)
            })
        end
    end
end

local function buildWorm(template)
    local variations = constants.ENEMY_VARIATIONS

    for tier=1, TIERS do
        local effectiveLevel = TIER_UPGRADE_SET[tier]
        for i=1,variations do
            local worm = deepcopy(template)
            worm.name = worm.name .. "-v" .. i .. "-t" .. tier
            worm.tier = tier
            worm.effectiveLevel = effectiveLevel
            worm.variation = i
            generateApperance(worm)
            worm.death = (worm.deathGenerator and worm.deathGenerator(worm)) or nil
            fillEntityTemplate(worm)

            worm.standupSounds = wormStandupSounds[effectiveLevel]
            worm.breathSounds = wormBreathSounds[effectiveLevel]
            worm.dyingSounds = wormDyingSounds[effectiveLevel]
            worm.roarSounds = wormRoarSounds[effectiveLevel]
            worm.altRoarSounds = wormAltRoarSounds[effectiveLevel]
            worm.foldSounds = wormFoldSounds[effectiveLevel]

            worm.attack = worm.attackGenerator(worm)

            if worm.autoplace then
                worm.attributes["autoplace"] = worm.autoplace[effectiveLevel]
            end
            data:extend({
                    makeWorm(worm)
            })
        end
    end
end

local function makeLootTables(template)
    local makeLootTable
    if (template.type == "biter") or (template.type == "spitter") then
        makeLootTable = makeUnitAlienLootTable
    elseif (template.type == "worm") then
        makeLootTable = makeWormAlienLootTable
    elseif (template.type == "biter-spawner") or (template.type == "spitter-spawner") then
        makeLootTable = makeSpawnerAlienLootTable
    elseif (template.type == "hive") then
        makeLootTable = makeSpawnerAlienLootTable
    else
        return nil
    end

    local newDrops = {}
    for i=1,#template.drops do
        local attribute = template.drops[i]
        if (attribute == "greenArtifact") then
            newDrops[#newDrops+1] = makeLootTable("green")
        elseif (attribute == "yellowArtifact") then
            newDrops[#newDrops+1] = makeLootTable("yellow")
        elseif (attribute == "blueArtifact") then
            newDrops[#newDrops+1] = makeLootTable("blue")
        elseif (attribute == "orangeArtifact") then
            newDrops[#newDrops+1] = makeLootTable("orange")
        elseif (attribute == "redArtifact") then
            newDrops[#newDrops+1] = makeLootTable("red")
        elseif (attribute == "purpleArtifact") then
            newDrops[#newDrops+1] = makeLootTable("purple")
        elseif (attribute == "nilArtifact") then
            newDrops[#newDrops+1] = makeLootTable(nil)
        end
    end

    return newDrops
end

local function buildAttack(faction, template)
    for i=1,#template.attackAttributes do
        local attack = template.attackAttributes[i]
        if (attack == "melee") then
            template.attackGenerator = createMeleeAttack
        elseif (attack == "acidPool") then
            template.addon[#template.addon+1] = acidPuddleAttributeNumeric
            template.meleePuddleGenerator = function (attributes)
                attributes.stickerDamagePerTickType = "acid"
                return {
                    type="create-fire",
                    entity_name = makeAcidSplashFire(attributes, attributes.stickerName or makeSticker(attributes)),
                    check_buildability = true,
                    initial_ground_flame_count = 2,
                    show_in_tooltip = true
                }
            end
        elseif (attack == "spit") then
            template.attackType = "projectile"
            -- template.attackDirectionOnly = true

            template.attackGenerator = function (attack)
                return createRangedAttack(attack,
                                          createAttackBall(attack),
                                          (template.attackAnimation and template.attackAnimation(attack.scale,
                                                                                                 attack.tint,
                                                                                                 attack.tint2)) or nil)
            end
        elseif (attack == "spitFire") then
            template.attackType = "projectile"
            -- template.attackDirectionOnly = true
            template.addon[#template.addon+1] = streamAttackNumeric

            template.attackGenerator = function (attack)
                return createRangedAttack(attack,
                                          createSpitFire(attack),
                                          (template.attackAnimation and template.attackAnimation(attack.scale,
                                                                                                 attack.tint,
                                                                                                 attack.tint2)) or nil)
            end
        elseif (attack == "touch") then
            template.attackType = "projectile"
            -- template.attackDirectionOnly = true
            template.range = { 0.1, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.3, 0.3, 0.4 }

            template.attackGenerator = function (attack)
                return createRangedAttack(attack,
                                          createAttackBall(attack),
                                          (template.attackAnimation and template.attackAnimation(attack.scale,
                                                                                                 attack.tint,
                                                                                                 attack.tint2)) or nil)
            end
        elseif (attack == "drainCrystal") then
            template.range = { 6, 6, 7, 7, 8, 8, 9, 9, 10, 10 }
            template.addon[#template.addon+1] = liteClusterAttackNumeric
            template.attackPointEffects = function(attributes)
                return
                    {
                        {
                            type = "script",
                            effect_id = "rampant-drain-trigger"
                        },
                        {
                            type = "damage",
                            damage = { type="laser", amount = attributes.damage }
                        },
                        {
                            type="nested-result",
                            action = {
                                {
                                    type = "cluster",
                                    cluster_count = attributes.clusters,
                                    distance = attributes.clusterDistance,
                                    distance_deviation = 1,
                                    action_delivery =
                                        {
                                            type = "projectile",
                                            projectile = makeLaser(attributes),
                                            duration = 20,
                                            direction_deviation = 0.6,
                                            starting_speed = attributes.startingSpeed,
                                            starting_speed_deviation = 0.3
                                        },
                                    repeat_count = 2
                                }
                            }
                        }
                    }
            end
        elseif (attack == "physical") then
            template.explosionTiers = explosionTiers
            template.scorchmarkTiers = scorchmarkTiers
            template.damageType = "physical"
            template.fireDamagePerTickType = "physical"
            template.stickerDamagePerTickType = "physical"
            template.attackPointEffects = function (attributes)
                return {
                    {
                        type= "create-entity",
                        entity_name = attributes.attackScorchmark
                    },
                    {
                        type= "create-entity",
                        entity_name = attributes.attackExplosion
                    }
                }
            end
        elseif (attack == "acid") then
            template.damageType = "acid"
            template.fireDamagePerTickType = "acid"
            template.stickerDamagePerTickType = "acid"
        elseif (attack == "laser") then
            template.damageType = "laser"
            template.fireDamagePerTickType = "laser"
            template.stickerDamagePerTickType = "laser"
        elseif (attack == "electric") then
            template.damageType = "electric"
            template.fireDamagePerTickType = "electric"
            template.stickerDamagePerTickType = "electric"
        elseif (attack == "poison") then
            template.damageType = "poison"
            template.fireDamagePerTickType = "poison"
            template.stickerDamagePerTickType = "poison"
            template.attackPointEffects = function(attributes)
                return
                    {
                        {
                            type="create-entity",
                            entity_name = "poison-cloud-v" .. attributes.effectiveLevel .. "-cloud-rampant"
                        }
                    }
            end
        elseif (attack == "stream") then
            template.addon[#template.addon+1] = streamAttackNumeric
            template.attackGenerator = function (attack)
                return createStreamAttack(attack,
                                          createAttackFlame(attack),
                                          (template.attackAnimation and template.attackAnimation(attack.scale,
                                                                                                 attack.tint,
                                                                                                 attack.tint2)) or nil)
            end
        elseif (attack == "scatterBeam") then
            template.addon[#template.addon+1] = beamAttackNumeric
            template.attackGenerator = function (attack)
                return createElectricAttack(attack,
                                            makeBeam(attack),
                                            (template.attackAnimation and template.attackAnimation(attack.scale,
                                                                                                   attack.tint,
                                                                                                   attack.tint2)) or nil)
            end
        elseif (attack == "beam") then
            template.addon[#template.addon+1] = beamAttackNumeric
            template.attackGenerator = function (attack)
                return createBeamAttack(attack,
                                        makeBeam(attack),
                                        (template.attackAnimation and template.attackAnimation(attack.scale,
                                                                                               attack.tint,
                                                                                               attack.tint2)) or nil)
            end
        elseif (attack == "cluster") then
            template.addon[#template.addon+1] = clusterAttackNumeric
            template.attackBubble = makeBubble(template)
            template.attackPointEffects = function(attributes)
                return
                    {
                        {
                            type="nested-result",
                            action = {
                                {
                                    type = "cluster",
                                    cluster_count = attributes.clusters,
                                    distance = attributes.clusterDistance,
                                    distance_deviation = 3,
                                    action_delivery =
                                        {
                                            type = "projectile",
                                            projectile = makeLaser(attributes),
                                            duration = 20,
                                            direction_deviation = 0.6,
                                            starting_speed = attributes.startingSpeed,
                                            starting_speed_deviation = 0.3
                                        },
                                    repeat_count = 2
                                }
                            }
                        }
                    }
            end
        elseif (attack == "slow") then
            template.force = "not-same"
            template.stickerAnimation = {
                filename = "__base__/graphics/entity/slowdown-sticker/slowdown-sticker.png",
                priority = "extra-high",
                line_length = 5,
                width = 22,
                height = 24,
                frame_count = 50,
                animation_speed = 0.5,
                tint = {r = 0.3500, g = 0.663, b = 0.000, a = 0.694}, -- #4a900b1
                shift = util.by_pixel (2,-1),
                hr_version =
                    {
                        filename = "__base__/graphics/entity/slowdown-sticker/hr-slowdown-sticker.png",
                        line_length = 5,
                        width = 42,
                        height = 48,
                        frame_count = 50,
                        animation_speed = 0.5,
                        tint = {r = 0.3500, g = 0.663, b = 0.000, a = 0.694}, -- #ffa900b1
                        shift = util.by_pixel(2, -0.5),
                        scale = 0.5
                    }
            }
            template.areaEffects = function (attributes)
                return {
                    {
                        type = "damage",
                        damage = { amount = attributes.damage, type = "acid" }
                    },
                    {
                        type = "create-sticker",
                        sticker = makeSticker(attributes)
                    }
                }
            end
        elseif (attack == "nuclear") then
            template.addon[#template.addon+1] = nuclearAttackNumeric
            template.explosionTiers = explosionTiers
            template.scorchmarkTiers = scorchmarkTiers
            template.nuclear = true
            template.attackGenerator = function (attack)
                return createSuicideAttack(attack,
                                           makeAtomicBlast(attack),
                                           (template.attackAnimation and template.attackAnimation(attack.scale,
                                                                                                  attack.tint,
                                                                                                  attack.tint2)) or nil)
            end
        elseif (attack == "bomb") then
            template.addon[#template.addon+1] = bombAttackNumeric
            template.explosionTiers = explosionTiers
            template.scorchmarkTiers = scorchmarkTiers
            template.attackGenerator = function (attack)
                return createSuicideAttack(attack,
                                           nil,
                                           (template.attackAnimation and template.attackAnimation(attack.scale,
                                                                                                  attack.tint,
                                                                                                  attack.tint2)) or nil)
            end
        elseif (attack == "capsule") then
            template.attackType = "projectile"
            -- template.attackDirectionOnly = true

            template.attackGenerator = function (attack)
                return createProjectileAttack(attack,
                                              createCapsuleProjectile(attack,
                                                                      attack.entityGenerator(attack)),
                                              (template.attackAnimation and template.attackAnimation(attack.scale,
                                                                                                     attack.tint,
                                                                                                     attack.tint2)) or nil)
            end
        elseif (attack == "noFriendlyFire") then
            template["force"] = "not-same"
        elseif (attack == "noAcidPuddle") then
            template.noAcidPuddle = true
        elseif (type(attack) == "table") then
            if (attack[1] == "drone") then
                template.entityGenerator = function (attributes)
                    return template.faction .. "-" .. attack[2] .. "-v" ..
                        attributes.variation .. "-t" .. attributes.effectiveLevel .. "-drone-rampant"
                end
            end
        else
            error ("unknown attack " .. attack)
        end
    end
end

local function checkForAddons(template)
    for i=1,#template.attributes do
        local attribute = template.attributes[i]
        if (type(attribute) == "table") then
            if (attribute[1] == "clusterDeath") then
                template.addon[#template.addon+1] = clusterAttackNumeric
            end
        end
    end
end

local function buildUnitTemplate(faction, unit)
    local template = deepcopy(unit)

    template.name = faction.type .. "-" .. unit.name
    template.unit_name = unit.name
    template.isRampant = true
    template.tint = faction.tint
    template.tint2 = faction.tint2
    template.tint3 = faction.tint3

    template.faction = faction.type

    template.addon = {}
    template.explosion = faction.type

    template.resistances = {}

    if (template.type == "biter") then
        template.attackAnimation = biterattackanimation
    elseif (template.type == "spitter") then
        template.attackAnimation = spitterattackanimation
    end

    checkForAddons(template)

    buildAttack(faction, template)

    if template.drops then
        template.drops = makeLootTables(template)
    end

    if not template.attackGenerator then
        error("missing attack generator " .. faction.type .. " " .. template.name)
    end

    return template
end

local function buildTurretTemplate(faction, turret)
    local template = deepcopy(turret)

    template.name = faction.type .. "-" .. turret.name
    template.unit_name = turret.name
    template.isRampant = true
    template.tint = faction.tint
    template.tint2 = faction.tint2
    template.tint3 = faction.tint3

    template.faction = faction.type

    template.evolutionFunction = function (tier)
        if (tier == 0) then
            return 0
        else
            return math.min(faction.evo + ((tier - 2) * 0.10), 0.92)
        end
    end

    template.explosion = faction.type

    template.addon = {}
    template.resistances = {}

    buildAttack(faction, template)

    checkForAddons(template)

    if template.drops then
        template.drops = makeLootTables(template)
    end

    if not template.attackGenerator then
        error("missing attack generator " .. faction.type .. " " .. template.name)
    end

    return template
end

local function buildUnitSpawnerTemplate(faction, incomingTemplate, unitSets)
    local template = deepcopy(incomingTemplate)

    template.name = faction.type .. "-" .. template.name
    template.unit_name = incomingTemplate.name
    template.isRampant = true
    template.tint = faction.tint
    template.tint2 = faction.tint2
    template.tint3 = faction.tint3

    template.faction = faction.type

    template.evolutionFunction = function (tier)
        if (tier == 0) then
            return 0
        else
            return math.min(faction.evo + ((tier - 2) * 0.10), 0.92)
        end
    end

    template.explosion = faction.type
    template.addon = {}

    checkForAddons(template)

    template.resistances = {}

    local unitSet = {}

    -- local unitVariations = constants.ENEMY_VARIATIONS

    for t=1,TIERS do
        for i=1,#template.buildSets do
            local buildSet = template.buildSets[i]
            if (buildSet.tierStart <= t) and (t <= buildSet.tierEnd) then
                local activeUnitSet = unitSets[buildSet.name][t]
                local unitSetTier = unitSet[t]
                if unitSetTier then
                    for b=1,#activeUnitSet do
                        unitSetTier[#unitSetTier+1] = activeUnitSet[b]
                    end
                else
                    unitSet[t] = deepcopy(activeUnitSet)
                end
            end
        end
    end

    template.unitSet = unitSet

    if template.drops then
        template.drops = makeLootTables(template)
    end

    return template
end

local function buildHiveTemplate(faction, incomingTemplate, unitSets)
    local template = deepcopy(incomingTemplate)

    template.name = faction.type .. "-" .. template.name
    template.unit_name = incomingTemplate.name
    template.isRampant = true
    template.tint = faction.tint
    template.tint2 = faction.tint2
    template.tint3 = faction.tint3

    template.faction = faction.type

    template.evolutionFunction = function (tier)
        if (tier == 0) then
            return 0
        else
            return math.min(faction.evo + ((tier - 2) * 0.10), 0.92)
        end
    end

    template.addon = {}
    template.explosion = faction.type

    checkForAddons(template)

    template.resistances = {}

    local unitSet = {}

    for t=1,TIERS do
        for i=1,#template.buildSets do
            local buildSet = template.buildSets[i]
            if (buildSet.tierStart <= t) and (t <= buildSet.tierEnd) then
                local activeUnitSet = unitSets[buildSet.name][t]
                local unitSetTier = unitSet[t]
                if unitSetTier then
                    for b=1,#activeUnitSet do
                        unitSetTier[#unitSetTier+1] = activeUnitSet[b]
                    end
                else
                    unitSet[t] = deepcopy(activeUnitSet)
                end
            end
        end
    end


    template.unitSet = unitSet

    if template.drops then
        template.drops = makeLootTables(template)
    end

    return template
end

function swarmUtils.processFactions()
    for i=1,#constants.FACTION_SET do
        local faction = constants.FACTION_SET[i]

        if (faction.type == "energy-thief") then
            energyThiefFaction.addFactionAddon()
        elseif (faction.type == "poison") then
            data:extend({
                    {
                        type = "damage-type",
                        name = "healing"
                    }
            })
            poisonFaction.addFactionAddon()
        end

        makeBloodFountains({
                name = faction.type,
                tint2 = faction.tint2
        })

        local unitSets = {}

        for iu=1,#faction.units do
            local unit = faction.units[iu]
            local template = buildUnitTemplate(faction, unit)

            unitSets[unit.name] = buildUnits(template)
        end

        for iu=1,#faction.buildings do
            local building = faction.buildings[iu]

            if (building.type == "spitter-spawner") then
                local template = buildUnitSpawnerTemplate(faction, building, unitSets)

                buildUnitSpawner(template)
            elseif (building.type == "biter-spawner") then
                local template = buildUnitSpawnerTemplate(faction, building, unitSets)

                buildUnitSpawner(template)
            elseif (building.type == "turret") then
                local template = buildTurretTemplate(faction, building)

                buildWorm(template)
            elseif (building.type == "hive") then
                local template = buildHiveTemplate(faction, building, unitSets)

                buildEntitySpawner(template, 5)
            elseif (building.type == "trap") then

            elseif (building.type == "utility") then

            end


        end
    end
end

return swarmUtils
