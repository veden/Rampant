local swarmUtils = {}
-- imports

local droneUtils = require("utils/DroneUtils")
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

-- imported functions

local addFactionAddon = energyThiefFaction.addFactionAddon
local addFactionAddon = poisonFaction.addFactionAddon

local roundToNearest = mathUtils.roundToNearest

local mMax = math.max
local mMin = math.min

local distort = mathUtils.distort

local mFloor = math.floor
local deepcopy = util.table.deepcopy

local TIER_UPGRADE_SET_10 = constants.TIER_UPGRADE_SET_10

local xorRandom = mathUtils.xorRandom(settings.startup["rampant-enemySeed"].value)

local biterattackanimation = unitUtils.biterattackanimation

local createProjectileAttack = biterUtils.createProjectileAttack
local createCapsuleProjectile = droneUtils.createCapsuleProjectile
local makeSticker = stickerUtils.makeSticker
local makeAtomicBlast = bombUtils.makeAtomicBlast
local makeLaser = beamUtils.makeLaser
local createAttackBall = acidBall.createAttackBall
local createRangedAttack = biterUtils.createRangedAttack
local createMeleeAttack = biterUtils.createMeleeAttack
local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTabl
local makeAcidSplashFire = fireUtils.makeAcidSplashFire

local makeWormAlienLootTable = biterUtils.makeWormAlienLootTable
local makeUnitAlienLootTable = biterUtils.makeUnitAlienLootTable
local makeSpawnerAlienLootTable = biterUtils.makeSpawnerAlienLootTable

local createSuicideAttack = biterUtils.createSuicideAttack

local createAttackFlame = attackFlame.createAttackFlame
local createStreamAttack = biterUtils.createStreamAttack

local gaussianRandomRangeRG = mathUtils.gaussianRandomRangeRG

local makeBloodFountains = particleUtils.makeBloodFountains

local makeBiterCorpse = biterUtils.makeBiterCorpse
local makeUnitSpawnerCorpse = biterUtils.makeUnitSpawnerCorpse
local makeWormCorpse = biterUtils.makeWormCorpse
local makeSpitterCorpse = biterUtils.makeSpitterCorpse

local makeBubble = beamUtils.makeBubble
local makeBeam = beamUtils.makeBeam
local createElectricAttack = biterUtils.createElectricAttack

local makeBiter = biterUtils.makeBiter
local makeDrone = droneUtils.makeDrone
local makeSpitter = biterUtils.makeSpitter
local makeWorm = biterUtils.makeWorm
local makeUnitSpawner = biterUtils.makeUnitSpawner

-- module code

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

local nuclearAttackNumeric = {
    ["damage"] = { 50, 60, 80, 100, 120, 130, 140, 150, 180, 240 },
    ["repeatCount"] = { 150, 175, 250, 300, 350, 400, 450, 500, 550, 600 },
    ["radius"] = { 5, 10, 10, 12, 14, 16, 16, 18, 18, 20 },
    ["explosionDistance"] = { 3, 3, 4, 4, 5, 5, 6, 6, 7, 7 },
    ["explosionCount"] = { 3, 3, 3, 4, 4, 4, 5, 5, 6, 6 }
}

local bombAttackNumeric = {
    ["damage"] = { 100, 200, 300, 400, 600, 800, 1000, 1200, 1500, 2000 },
    ["radius"] = { 3.5, 3.5, 4, 5, 6, 6, 7, 7, 7.5, 8 },
    ["explosionDistance"] = { 2, 2, 2, 2, 2, 2.5, 2.5, 2.5, 3, 3 },
    ["explosionCount"] = { 2, 3, 4, 5, 6, 8, 10, 12, 13, 14 }
}

local slowStickerNumeric = {
    ["stickerMovementModifier"] = { 0.8, 0.8, 0.7, 0.7, 0.6, 0.6, 0.5, 0.5, 0.4, 0.4 },
    ["stickerDuration"] = { 1800, 1800, 1900, 1900, 2000, 2000, 2100, 2100, 2200, 2200 }
}

local streamAttackNumeric = {
    ["stickerDamagePerTick"] = { 0.6, 0.6, 0.8, 0.8, 0.8, 0.9, 1, 1, 1.3, 1.5 },
    ["particleTimeout"] = { 3, 3, 4, 4, 5, 5, 6, 6, 7, 7 },
    ["fireSpreadRadius"] = { 0.75, 0.75, 0.77, 0.77, 0.79, 0.79, 0.83, 0.83, 0.85, 0.85 },
    ["damageMaxMultipler"] = { 6, 6, 7, 7, 7, 7, 8, 8, 8, 9 },
    ["stickerMovementModifier"] = { 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1, 1.1 },
    ["fireSpreadCooldown"] = { 30, 30, 29, 29, 28, 28, 27, 27, 25, 25 },
    ["stickerDuration"] = { 800, 800, 900, 900, 1000, 1000, 1100, 1100, 1200, 1200 },
    ["damage"] = { 4, 4, 5, 5, 6, 6, 6, 6, 6, 7 }
}

local beamAttackNumeric = {
    ["range"] = { 11, 11, 12, 12, 13, 13, 14, 14, 15, 15 },
    ["damage"] = { 6, 10, 15, 20, 30, 45, 60, 75, 90, 150 },
    ["duration"] = { 20, 20, 21, 21, 22, 22, 23, 23, 24, 24 },
    ["damageInterval"] =  { 20, 20, 21, 21, 22, 22, 23, 23, 24, 24 },
    ["width"] = { 1.5, 1.5, 1.6, 1.6, 1.7, 1.7, 1.8, 1.8, 1.9, 1.9 }
}

local clusterAttackNumeric = {
    ["clusterDistance"] = { 3, 3, 4, 4, 5, 5, 6, 6, 7, 7 },
    ["clusters"] = { 5, 5, 6, 6, 7, 7, 8, 8, 9, 9 },
    ["startingSpeed"] = { 0.25, 0.25, 0.27, 0.27, 0.29, 0.29, 0.31, 0.31, 0.33, 0.33 }
}

local biterAttributeNumeric = {
    ["range"] = { 0.5, 0.5, 0.75, 0.75, 1.0, 1.0, 1.25, 1.50, 1.75, 2.0 },
    ["radius"] = { 0.5, 0.65, 0.75, 0.85, 0.95, 1.1, 1.2, 1.3, 1.4, 1.5 },
    ["cooldown"] = { 40, 41, 42, 44, 46, 48, 50, 52, 55, 57 },
    ["damage"] = { 7, 15, 22.5, 35, 45, 60, 75, 90, 150, 200 },
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
    ["pollutionToAttack"] = { 200, 750, 1200, 1750, 2500, 5000, 10000, 12500, 15000, 20000 },
    ["spawningTimeModifer"] = { 1, 1, 1, 2, 3, 7, 10, 10, 12, 12 }
}

local acidPuddleAttributeNumeric = {
    ["damagePerTick"] = { 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5 },
    ["stickerDamagePerTick"] = { 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5 },
    ["stickerDuration"] = { 600, 610, 620, 630, 640, 650, 660, 670, 680, 690 }
}

local spitterAttributeNumeric = {
    ["range"] = { 13, 13, 14, 14, 15, 15, 16, 16, 17, 17 },
    ["radius"] = { 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.5 },
    ["cooldown"] = { 100, 100, 97, 97, 95, 95, 93, 93, 90, 90 },
    ["stickerDuration"] = { 600, 610, 620, 630, 640, 650, 660, 670, 680, 690 },
    ["damagePerTick"] = { 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1 },
    ["stickerDamagePerTick"] = { 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5 },
    ["stickerMovementModifier"] = { 0.8, 0.7, 0.6, 0.55, 0.50, 0.45, 0.40, 0.35, 0.30, 0.25 },
    ["damage"] = { 16, 30, 45, 60, 90, 110, 130, 150, 170, 190 },
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
    ["pollutionToAttack"] = { 200, 750, 1200, 1750, 2500, 5000, 10000, 12500, 15000, 20000 },
    ["spawningTimeModifer"] = { 1, 1, 1, 2, 2, 5, 8, 8, 10, 10 },
}

local droneAttributeNumeric = {
    ["scale"] = { 0.3, 0.32, 0.34, 0.36, 0.4, 0.42, 0.44, 0.5, 0.52, 0.54 },
    ["particleVerticalAcceleration"] = { 0.01, 0.01, 0.02, 0.02, 0.03, 0.03, 0.04, 0.04, 0.05, 0.05 },
    ["particleHoizontalSpeed"] = { 0.6, 0.6, 0.7, 0.7, 0.8, 0.8, 0.9, 0.9, 1, 1 },
    ["particleHoizontalSpeedDeviation"] = { 0.0025, 0.0025, 0.0024, 0.0024, 0.0023, 0.0023, 0.0022, 0.0022, 0.0021, 0.0021 },
    ["stickerDuration"] = { 600, 610, 620, 630, 640, 650, 660, 670, 680, 690 },
    ["stickerMovementModifier"] = { 0.8, 0.7, 0.6, 0.55, 0.50, 0.45, 0.40, 0.35, 0.30, 0.25 },
    ["damagePerTick"] = { 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1 },
    ["cooldown"] = { 60, 60, 55, 55, 50, 50, 45, 45, 40, 40 },
    ["ttl"] = { 300, 300, 350, 350, 400, 400, 450, 450, 500, 500 },
    ["damage"] = { 2, 4, 7, 13, 15, 18, 22, 28, 35, 40 },
    ["movement"] = { 0.06, 0.06, 0.07, 0.07, 0.08, 0.08, 0.09, 0.09, 0.1, 0.1 },
    ["distancePerFrame"] = { 0.1, 0.1, 0.105, 0.105, 0.110, 0.110, 0.112, 0.112, 0.114, 0.114 },
    ["rangeFromPlayer"] = { 9, 9, 10, 10, 11, 11, 12, 12, 13, 13 },
    ["range"] = { 10, 10, 11, 11, 12, 12, 13, 13, 14, 14 },
    ["radius"] = { 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.5 },
    ["health"] = { 15, 75, 100, 150, 200, 250, 275, 300, 325, 350 },
    ["healing"] = { 0.01, 0.01, 0.015, 0.02, 0.05, 0.075, 0.1, 0.12, 0.14, 0.16 },
}

local unitSpawnerAttributeNumeric = {
    ["health"] = { 350, 500, 750, 1500, 3500, 7500, 11000, 20000, 30000, 45000 },
    ["healing"] = { 0.02, 0.02, 0.022, 0.024, 0.026, 0.028, 0.03, 0.032, 0.034, 0.036 },
    ["spawningCooldownStart"] = { 1080, 1075, 1070, 1065, 1060, 1055, 1050, 1045, 1040, 1035 },
    ["spawningCooldownEnd"] = { 350, 345, 340, 335, 330, 325, 320, 315, 310, 305 },
    ["unitsToSpawn"] = { 5, 5, 6, 6, 7, 7, 8, 8, 9, 9 },
    ["scale"] = { 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.2, 1.4, 1.6, 1.8 },
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

local hiveAttributeNumeric = {
    ["health"] = { 700, 1000, 1500, 3000, 7000, 15000, 22000, 40000, 60000, 90000 },
    ["healing"] = { 0.02, 0.02, 0.022, 0.024, 0.026, 0.028, 0.03, 0.032, 0.034, 0.036 },
    ["spawningCooldownStart"] = { 2840, 2800, 2760, 2720, 2680, 2640, 2600, 2560, 2520, 2480 },
    -- ["spawningCooldownStart"] = { 60, 60, 60, 60, 60, 60, 60, 60, 60, 60 },
    ["spawningRadius"] = { 10, 13, 15, 17, 20, 23, 26, 29, 32, 35 },
    ["spawningSpacing"] = { 5, 5, 5, 6, 6, 6, 7, 7, 7, 8 },
    ["spawningCooldownEnd"] = { 1020, 1015, 1010, 1005, 1000, 995, 990, 985, 980, 975 },
    -- ["spawningCooldownEnd"] = { 60, 60, 60, 60, 60, 60, 60, 60, 60, 60 },
    ["unitsToSpawn"] = { 3000, 3000, 300, 3000, 3000, 3000, 3000, 3000, 3000, 3000 },
    ["scale"] = { 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1 },
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
    ["stickerMovementModifier"] = { 0.8, 0.7, 0.6, 0.55, 0.50, 0.45, 0.40, 0.35, 0.30, 0.25 },
    ["damagePerTick"] = { 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1 },
    ["range"] = { 25, 27, 31, 33, 35, 36, 37, 38, 39, 40 },
    ["cooldown"] = { 70, 70, 68, 66, 64, 62, 60, 58, 56, 54 },
    ["damage"] = { 36, 45, 85, 135, 155, 175, 195, 215, 235, 255 },
    ["scale"] = { 0.25, 0.40, 0.60, 0.8, 0.9, 1, 1.2, 1.4, 1.6, 1.8 },
    ["radius"] = { 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.2, 2.3, 2.5, 3.0 },
    ["stickerDamagePerTick"] = { 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5 },
    ["particleVerticalAcceleration"] = { 0.01, 0.01, 0.02, 0.02, 0.03, 0.03, 0.04, 0.04, 0.05, 0.05 },
    ["particleHoizontalSpeed"] = { 0.6, 0.6, 0.7, 0.7, 0.8, 0.8, 0.9, 0.9, 1, 1 },
    ["particleHoizontalSpeedDeviation"] = { 0.0025, 0.0025, 0.0024, 0.0024, 0.0023, 0.0023, 0.0022, 0.0022, 0.0021, 0.0021 },
    ["foldingSpeed"] = { 0.15, 0.15, 0.16, 0.16, 0.16, 0.17, 0.17, 0.18, 0.18, 0.19 },
    ["preparingSpeed"] = { 0.025, 0.025, 0.026, 0.026, 0.027, 0.027, 0.028, 0.028, 0.029, 0.029 },
    ["prepareRange"] = { 30, 30, 35, 35, 40, 40, 40, 40, 45, 45 },
    ["physicalDecrease"] = { 0, 0, 5, 5, 8, 8, 10, 10, 12, 12 },
    ["explosionDecrease"] = { 0, 0, 5, 5, 8, 8, 10, 10, 12, 12 },
    ["explosionPercent"] = { 0, 0, 10, 10, 20, 20, 30, 30, 40, 40 },
    ["fireDecrease"] = { 3, 3, 4, 4, 5, 5, 5, 5, 5, 6 },
    ["health"] = { 200, 350, 500, 750, 2000, 3500, 7500, 12000, 20000, 25000 },
    ["firePercent"] = { 40, 40, 42, 42, 43, 43, 44, 44, 45, 45 },
    ["healing"] = { 0.01, 0.01, 0.015, 0.02, 0.05, 0.075, 0.1, 0.12, 0.14, 0.16 },
    ["evolutionRequirement"] = { 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9 }
}

local propTables10 = {
    {{0, 1}, {0.35, 0.0}},
    {{0.3, 0}, {0.35, 0.5}, {0.45, 0.0}},
    {{0.4, 0}, {0.45, 0.5}, {0.55, 0.0}},
    {{0.5, 0}, {0.55, 0.5}, {0.65, 0.0}},
    {{0.6, 0}, {0.65, 0.5}, {0.75, 0.0}},
    {{0.7, 0}, {0.75, 0.5}, {0.85, 0.0}},
    {{0.8, 0}, {0.825, 0.5}, {0.875, 0.0}},
    {{0.85, 0}, {0.875, 0.5}, {0.925, 0.0}},
    {{0.90, 0}, {0.925, 0.5}, {0.975, 0.0}},
    {{0.93, 0}, {1, 1.0}}
}

local function fillUnitTable(result, unitSet, tier, probability)
    for x=1,#unitSet[tier] do
        result[#result+1] = {unitSet[tier][x], probability}
    end
end

local function unitSetToProbabilityTable(unitSet)
    local result = {}

    fillUnitTable(result, unitSet, 1, {{0, 1}, {0.35, 0.0}})
    fillUnitTable(result, unitSet, 2, {{0.3, 0}, {0.35, 0.5}, {0.45, 0.0}})
    fillUnitTable(result, unitSet, 3, {{0.4, 0}, {0.45, 0.5}, {0.55, 0.0}})
    fillUnitTable(result, unitSet, 4, {{0.5, 0}, {0.55, 0.5}, {0.65, 0.0}})
    fillUnitTable(result, unitSet, 5, {{0.6, 0}, {0.65, 0.5}, {0.75, 0.0}})
    fillUnitTable(result, unitSet, 6, {{0.7, 0}, {0.75, 0.5}, {0.85, 0.0}})
    fillUnitTable(result, unitSet, 7, {{0.8, 0}, {0.825, 0.5}, {0.875, 0.0}})
    fillUnitTable(result, unitSet, 8, {{0.85, 0}, {0.875, 0.5}, {0.925, 0.0}})
    fillUnitTable(result, unitSet, 9, {{0.90, 0}, {0.925, 0.5}, {0.975, 0.0}})
    fillUnitTable(result, unitSet, 10, {{0.93, 0}, {1, 1.0}})

    return result
end

local function addMajorResistance(entity, name, tier)
    local decreases = { 7, 7, 10, 10, 13, 13, 16, 16, 19, 23 }
    local percents = { 65, 65, 70, 75, 75, 80, 85, 85, 90, 90 }
    entity.resistances[name] = {
        decrease = roundToNearest(gaussianRandomRangeRG(decreases[tier], decreases[tier] * 0.1, decreases[tier] * 0.85, decreases[tier] * 1.30, xorRandom), 0.1),
        percent = roundToNearest(gaussianRandomRangeRG(percents[tier], percents[tier] * 0.1, percents[tier] * 0.85, percents[tier] * 1.30, xorRandom), 0.1)
    }
end

local function addMinorResistance(entity, name, tier)
    -- {
    --     type = "resistance",
    --     name = "fire",
    --     decrease = { 1, 1, 1, 1, 2, 2, 3, 3, 3, 4 },
    --     percent = { 30, 30, 30, 40, 40, 40, 45, 45, 45, 50 }
    -- }
    local decreases = { 3, 3, 7, 7, 10, 10, 13, 13, 16, 18 }
    local percents = { 35, 35, 40, 40, 45, 45, 50, 55, 55, 60 }
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
    local percents = { -35, -35, -40, -40, -45, -45, -50, -55, -55, -60 }
    entity.resistances[name] = {
        decrease = roundToNearest(gaussianRandomRangeRG(decreases[tier], decreases[tier] * 0.1, decreases[tier] * 0.85, decreases[tier] * 1.30, xorRandom), 0.1),
        percent = roundToNearest(gaussianRandomRangeRG(percents[tier], percents[tier] * 0.1, percents[tier] * 0.85, percents[tier] * 1.30, xorRandom), 0.1)
    }
end


local function scaleAttributes (entity)
    if (entity.type == "biter") then
        entity["health"] = entity["health"] * settings.startup["rampant-unitBiterHealthScaler"].value
        entity["movement"] = entity["movement"] * settings.startup["rampant-unitBiterSpeedScaler"].value
        entity["distancePerFrame"] = entity["distancePerFrame"] * settings.startup["rampant-unitBiterSpeedScaler"].value
        entity["damage"] = entity["damage"] * settings.startup["rampant-unitBiterDamageScaler"].value
        entity["range"] = entity["range"] * settings.startup["rampant-unitBiterRangeScaler"].value
        entity["healing"] = entity["healing"] * settings.startup["rampant-unitBiterHealingScaler"].value
    elseif (entity.type == "spitter") then
        entity["health"] = entity["health"] * settings.startup["rampant-unitSpitterHealthScaler"].value
        entity["movement"] = entity["movement"] * settings.startup["rampant-unitSpitterSpeedScaler"].value
        entity["distancePerFrame"] = entity["distancePerFrame"] * settings.startup["rampant-unitSpitterSpeedScaler"].value
        entity["damage"] = entity["damage"] * settings.startup["rampant-unitSpitterDamageScaler"].value
        if entity["stickerDamagePerTick"] then
            entity["stickerDamagePerTick"] = entity["stickerDamagePerTick"] * settings.startup["rampant-unitSpitterDamageScaler"].value
        end
        entity["damagePerTick"] = entity["damagePerTick"] * settings.startup["rampant-unitSpitterDamageScaler"].value
        entity["range"] = entity["range"] * settings.startup["rampant-unitSpitterRangeScaler"].value
        entity["healing"] = entity["healing"] * settings.startup["rampant-unitSpitterHealingScaler"].value
    elseif (entity.type == "drone") then
        entity["health"] = entity["health"] * settings.startup["rampant-unitDroneHealthScaler"].value
        entity["movement"] = entity["movement"] * settings.startup["rampant-unitDroneSpeedScaler"].value
        entity["distancePerFrame"] = entity["distancePerFrame"] * settings.startup["rampant-unitDroneSpeedScaler"].value
        entity["damage"] = entity["damage"] * settings.startup["rampant-unitDroneDamageScaler"].value
        entity["damagePerTick"] = entity["damagePerTick"] * settings.startup["rampant-unitDroneDamageScaler"].value
        entity["range"] = entity["range"] * settings.startup["rampant-unitDroneRangeScaler"].value
        entity["healing"] = entity["healing"] * settings.startup["rampant-unitDroneHealingScaler"].value
    elseif (entity.type == "template") then
        entity["health"] = entity["health"] * settings.startup["rampant-unitSpawnerHealthScaler"].value
        entity["unitsOwned"] = entity["unitsOwned"] * settings.startup["rampant-unitSpawnerOwnedScaler"].value
        entity["unitsToSpawn"] = entity["unitsToSpawn"] * settings.startup["rampant-unitSpawnerSpawnScaler"].value
        entity["spawningCooldownStart"] = entity["spawningCooldownStart"] * settings.startup["rampant-unitSpawnerRespawnScaler"].value
        entity["spawningCooldownEnd"] = entity["spawningCooldownEnd"] * settings.startup["rampant-unitSpawnerRespawnScaler"].value
        entity["healing"] = entity["healing"] * settings.startup["rampant-unitSpawnerHealingScaler"].value
    elseif (entity.type == "worm") then
        entity["health"] = entity["health"] * settings.startup["rampant-unitWormHealthScaler"].value
        entity["damage"] = entity["damage"] * settings.startup["rampant-unitWormDamageScaler"].value
        entity["damagePerTick"] = entity["damagePerTick"] * settings.startup["rampant-unitWormDamageScaler"].value
        entity["range"] = entity["range"] * settings.startup["rampant-unitWormRangeScaler"].value
        entity["healing"] = entity["healing"] * settings.startup["rampant-unitWormHealingScaler"].value
    end
end

-- local function distort(xorRandom, num, min, max)
--     local min = min or num * 0.85
--     local max = max or num * 1.30
--     if (num < 0) then
--         local t = min
--         min = max
--         max = t
--     end
--     return roundToNearest(gaussianRandomRangeRG(num, num * 0.15, min, max, xorRandom), 0.01)
-- end

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
            for k,lootTable in pairs(entity.drops) do
                entity.loot[#entity.loot+1] = lootTable[tier]
            end
        elseif (key == "explosion") then
            local ti = tier
            if (entity.type == "drone") then
                ti = 1
            end
            entity[key] = entity[key] .. "-" .. bloodFountains[ti]
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
        elseif (key == "attributes") then
            for i=1,#entity[key] do
                local attribute = entity[key][i]
                if (attribute == "lowHealth") then
                    entity["health"] = entity["health"] * 0.75
                elseif (attribute == "lowestHealth") then
                    entity["health"] = entity["health"] * 0.50
                elseif (attribute == "quickCooldown") then
                    entity["cooldown"] = entity["cooldown"] * 0.85
                elseif (attribute == "slowMovement") then
                    entity["movement"] = entity["movement"] * 0.35
                    entity["distancePerFrame"] = entity["distancePerFrame"] * 0.65
                elseif (attribute == "quickMovement") then
                    entity["movement"] = entity["movement"] * 1.05
                    entity["distancePerFrame"] = entity["distancePerFrame"] * 1.15
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
                    entity["health"] = entity["health"] * 0.1
                elseif (attribute == "selfDamaging") then
                    local divider
                    if entity.health < 100 then
                        divider = 2
                    else
                        divider = 2.5
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
                elseif (attribute == "unstable") then
                    entity["healing"] = entity["healing"] * -1
                elseif (attribute == "checkBuildability") then
                    entity.checkBuildability = true
                elseif (attribute == "followsPlayer") then
                    entity.followsPlayer = true
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
    end

    -- print(serpent.dump(entity))
    scaleAttributes(entity)
end

local function calculateRGBa(tint, tier, staticAlpha)
    local r = gaussianRandomRangeRG(tint.r, tint.r * 0.10 + (0.005 * tier), mMax(tint.r * 0.85 - (0.005 * tier), 0), mMin(tint.r * 1.15, 1), xorRandom)
    local g = gaussianRandomRangeRG(tint.g, tint.g * 0.10 + (0.005 * tier), mMax(tint.g * 0.85 - (0.005 * tier), 0), mMin(tint.g * 1.15, 1), xorRandom)
    local b = gaussianRandomRangeRG(tint.b, tint.b * 0.10 + (0.005 * tier), mMax(tint.b * 0.85 - (0.005 * tier), 0), mMin(tint.b * 1.15, 1), xorRandom)
    local a = tint.a
    if not staticAlpha then
        a = gaussianRandomRangeRG(tint.a, tint.a * 0.10 + (0.005 * tier), mMax(tint.a * 0.85 - (0.005 * tier), 0), mMin(tint.a * 1.15, 1), xorRandom)
    end

    return { r=r, g=g, b=b, a=a }
end

local function generateApperance(unit)
    local tier = unit.effectiveLevel
    if unit.scale then
        local scaleValue = unit.scale[tier]
        local scale = gaussianRandomRangeRG(scaleValue, scaleValue * 0.12, scaleValue * 0.60, scaleValue * 1.40, xorRandom)
        unit.scale = scale
    end
    if unit.tint then
        unit.tint = calculateRGBa(unit.tint, tier, true)
    end
    if unit.tint2 then
        unit.tint2 = calculateRGBa(unit.tint2, tier, true)
    end
end

function swarmUtils.buildUnits(template)
    local unitSet = {}

    local variations = settings.startup["rampant-newEnemyVariations"].value

    for tier=1, 10 do
        local effectiveLevel = TIER_UPGRADE_SET_10[tier]
        local result = {}

        for i=1,variations do
            local unit = deepcopy(template)
            unit.name = unit.name .. "-v" .. i .. "-t" .. tier
            -- unit.nameSuffix = "-v" .. i .. "-t" .. tier
            unit.effectiveLevel = effectiveLevel
            unit.variation = i
            generateApperance(unit)
            fillEntityTemplate(unit)
            unit.attack = unit.attackGenerator(unit)
            unit.death = (unit.deathGenerator and unit.deathGenerator(unit)) or nil

            local entity
            if (unit.type == "spitter") then
                entity = makeSpitter(unit)
            elseif (unit.type == "biter") then
                entity = makeBiter(unit)
            elseif (unit.type == "drone") then
                if not death then
                    death = {
                        type = "direct",
                        action_delivery =
                            {
                                type = "instant",
                                target_effects =
                                    {
                                        type = "create-entity",
                                        entity_name = "massive-explosion"
                                    }
                            }
                    }
                end
                entity = makeDrone(unit)
            end

            result[#result+1] = entity.name

            data:extend({entity})
        end

        unitSet[#unitSet+1] = result
    end

    return unitSet
end

local function buildEntities(entityTemplates)
    local unitSet = {}

    for tier=1, 10 do
        local effectiveLevel = TIER_UPGRADE_SET_10[tier]
        local result = {}

        local entityTemplate = entityTemplates[effectiveLevel]

        for ei=1,#entityTemplate do
            local template = entityTemplate[ei]

            local probability = deepcopy(propTables10[tier])

            for z=1,#probability do
                probability[z][2] = probability[z][2] * template[2]
            end
            unitSet[#unitSet+1] = {template[1] .. "-t" .. tier .. "-rampant", probability}
        end
    end

    return unitSet
end

function swarmUtils.buildEntitySpawner(template)
    local variations = settings.startup["rampant-newEnemyVariations"].value

    for tier=1, 10 do
        local effectiveLevel = TIER_UPGRADE_SET_10[tier]

        for i=1,variations do
            local unitSpawner = deepcopy(template)
            unitSpawner.name = unitSpawner.name .. "-v" .. i .. "-t" .. tier
            unitSpawner.effectiveLevel = effectiveLevel
            unitSpawner.variation = i
            generateApperance(unitSpawner)
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

function swarmUtils.buildUnitSpawner(template)
    local variations = settings.startup["rampant-newEnemyVariations"].value

    for tier=1, 10 do
        local effectiveLevel = TIER_UPGRADE_SET_10[tier]

        for i=1,variations do
            local unitSpawner = deepcopy(template)
            unitSpawner.name = unitSpawner.name .. "-v" .. i .. "-t" .. tier
            unitSpawner.effectiveLevel = effectiveLevel
            unitSpawner.variation = i
            local unitTable = unitSetToProbabilityTable(template.unitSet)
            unitSpawner.unitSet = unitTable
            generateApperance(unitSpawner)
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

function swarmUtils.buildWorm(template)
    local variations = settings.startup["rampant-newEnemyVariations"].value

    for tier=1, 10 do
        local effectiveLevel = TIER_UPGRADE_SET_10[tier]
        for i=1,variations do
            local worm = deepcopy(template)
            worm.name = worm.name .. "-v" .. i .. "-t" .. tier
            worm.effectiveLevel = effectiveLevel
            worm.variation = i
            generateApperance(worm)
            fillEntityTemplate(worm)

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
            template.actions = function(attributes, electricBeam)
                return
                    {
                        {
                            type = "instant",
                            target_effects =
                                {
                                    type = "create-entity",
                                    trigger_created_entity = true,
                                    entity_name = "drain-trigger-rampant"
                                }
                        },
                        {
                            type = "beam",
                            beam = electricBeam or "electric-beam",
                            duration = attributes.duration or 20
                        }
                    }
            end
        elseif (attack == "physical") then
            template.explosionTiers = explosionTiers
            template.damageType = "physical"
            template.fireDamagePerTickType = "physical"
            template.stickerDamagePerTickType = "physical"
            template.attackPointEffects = function (attributes)
                return {
                    {
                        type= "create-entity",
                        entity_name = "small-scorchmark"
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
        elseif (attack == "beam") then
            template.addon[#template.addon+1] = beamAttackNumeric
            template.attackGenerator = function (attack)
                return createElectricAttack(attack,
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
            template.force = "enemy"
            template.stickerAnimation = {
                filename = "__base__/graphics/entity/slowdown-sticker/slowdown-sticker.png",
                priority = "extra-high",
                width = 11,
                height = 11,
                frame_count = 13,
                animation_speed = 0.4
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
            template["force"] = "enemy"
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
    template.tint = faction.tint
    template.tint2 = faction.tint2

    template.faction = faction.type

    template.addon = {}
    template.explosion = faction.type

    template.resistances = {}

    local attackAnimation
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
    template.tint = faction.tint
    template.tint2 = faction.tint2

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

local function buildUnitSpawnerTemplate(faction, template, unitSets)
    local template = deepcopy(template)

    template.name = faction.type .. "-" .. template.name
    template.tint = faction.tint
    template.tint2 = faction.tint2

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

    -- local unitVariations = settings.startup["rampant-newEnemyVariations"].value

    for t=1,10 do
        for i=1,#template.buildSets do
            local buildSet = template.buildSets[i]
            if (buildSet[2] <= t) and (t <= buildSet[3]) then
                local activeUnitSet = unitSets[buildSet[1]][t]
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
        -- while (#unitSet[t] > unitVariations) do
        --     table.remove(unitSet, math.random(#unitSet[t]))
        -- end
    end

    template.unitSet = unitSet

    if template.drops then
        template.drops = makeLootTables(template)
    end

    return template
end

local function buildHiveTemplate(faction, template)
    local template = deepcopy(template)

    template.name = faction.type .. "-" .. template.name
    template.tint = faction.tint
    template.tint2 = faction.tint2

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

    local buildingVariations = settings.startup["rampant-newEnemyVariations"].value

    for t=1,10 do
        local unitSetTier = unitSet[t]
        for i=1,#template.buildSets do
            local buildSet = template.buildSets[i]
            if (buildSet[2] <= t) and (t <= buildSet[3]) then
                if not unitSetTier then
                    unitSetTier = {}
                    unitSet[t] = unitSetTier
                end

                unitSetTier[#unitSetTier+1] = {
                    "entity-proxy-" .. buildSet[1],
                    mathUtils.linearInterpolation((t-1)/9,
                        buildSet[4],
                        buildSet[5])
                }
            end
        end
        local total = 0
        for i=1,#unitSetTier do
            total = total + unitSetTier[i][2]
        end
        for i=1,#unitSetTier do
            unitSetTier[i][2] = unitSetTier[i][2] / total
        end
        -- while (#unitSet[t] > unitVariations) do
        --     table.remove(unitSet, math.random(#unitSet[t]))
        -- end
    end

    template.unitSet = buildEntities(unitSet)

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

            unitSets[unit.name] = swarmUtils.buildUnits(template)
        end

        for iu=1,#faction.buildings do
            local building = faction.buildings[iu]

            if (building.type == "spitter-spawner") then
                local template = buildUnitSpawnerTemplate(faction, building, unitSets)

                swarmUtils.buildUnitSpawner(template)
            elseif (building.type == "biter-spawner") then
                local template = buildUnitSpawnerTemplate(faction, building, unitSets)

                swarmUtils.buildUnitSpawner(template)
            elseif (building.type == "turret") then
                local template = buildTurretTemplate(faction, building)

                swarmUtils.buildWorm(template)
            elseif (building.type == "hive") then
                local template = buildHiveTemplate(faction, building)

                swarmUtils.buildEntitySpawner(template)
            elseif (building.type == "trap") then

            elseif (building.type == "utility") then

            end


        end

    end
end

return swarmUtils
