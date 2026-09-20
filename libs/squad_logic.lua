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
-- along with this program.  If not, see <https://www.gnu.org/licenses/>

--[[
    SquadLogic - Pure squad decision logic.

    Zero Factorio dependencies in the decision functions: the clock is
    ALWAYS a parameter (decay-on-read math must not read it), and
    no function here touches storage/settings/Universe. The impure
    caller (libs/Squad.lua) reads the clock once per call site and
    threads it in.

    Decision outputs are structured tables (never bare booleans):
      gateDecision     { accepted: bool, metric: string? }
      settleDecision   { settle: bool, reason: string? }
      kamikazeDecision { kamikaze: bool, threshold: number }
]]

local Command = require("command")
local Constants = require("Constants")
local ChunkPropertyUtils = require("ChunkPropertyUtils")
local MathUtils = require("MathUtils")

local SQUAD_RAIDING    = Constants.SQUAD_RAIDING
local SQUAD_SETTLING   = Constants.SQUAD_SETTLING
local SQUAD_GUARDING   = Constants.SQUAD_GUARDING
local SQUAD_RETREATING = Constants.SQUAD_RETREATING

-- Chunk pheromone indices
local PLAYER_PHEROMONE = Constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = Constants.BASE_PHEROMONE
local ENEMY_PHEROMONE = Constants.ENEMY_PHEROMONE
local ENEMY_PHEROMONE_MULTIPLER = Constants.ENEMY_PHEROMONE_MULTIPLER
local RESOURCE_PHEROMONE = Constants.RESOURCE_PHEROMONE
local KAMIKAZE_PHEROMONE = Constants.KAMIKAZE_PHEROMONE

-- Decay-on-read tables (indexed 0..depth, table[0] = 1)
local DECAY_090 = Constants.DECAY_090
local DECAY_098 = Constants.DECAY_098
local DECAY_TABLE_DEPTH = Constants.DECAY_TABLE_DEPTH

local PLAYER_PHEROMONE_MULTIPLER = Constants.PLAYER_PHEROMONE_MULTIPLER
local BASE_AI_STATE_SIEGE = Constants.BASE_AI_STATE_SIEGE
local KAMIKAZE_SIEGE_MULTIPLIER = Constants.KAMIKAZE_SIEGE_MULTIPLIER

local Squad = {}

-- Optional source-versioned field reader. The runtime adapter installs this
-- when `rampant--sourceVersionedField` is enabled. Keeping it as an injected
-- callback lets this module stay free of Factorio/storage dependencies.
local sourceVersionedFieldReader

function Squad.setSourceVersionedFieldReader(reader)
    sourceVersionedFieldReader = reader
end

local function readSourceVersionedField(fieldType, chunk)
    if Constants.USE_SOURCE_VERSIONED_FIELD and sourceVersionedFieldReader then
        return sourceVersionedFieldReader(fieldType, chunk)
    end
    return nil
end

-- Settle trigger decision reasons
local REASON_MAX_DISTANCE = "max_distance"
local REASON_EMPTY_RESOURCE_CHUNK = "empty_resource_chunk"
local REASON_PATH_TERMINATED = "path_terminated"

-- Distraction decision keys (mapped to defines.distraction.* by the caller)
local DISTRACTION_NONE = "none"
local DISTRACTION_BY_ENEMY = "by_enemy"

-- The settle march lookahead depth (scoreNeighbors builds up to this many
-- steps; pathEndIndex below it means the march was cut short)
local SETTLE_PATH_LENGTH = 4

-- Exact counter names for formation rejections (existing Metrics counters)
local FORM_REJECT_METRICS = {
    attackerPoints = "squad.formReject.attacker.points",
    attackerPheromone = "squad.formReject.attacker.pheromone",
    attackerSquadLimit = "squad.formReject.attacker.squadLimit",
    attackerCandidate = "squad.formReject.attacker.candidate",
    attackerThreshold = "squad.formReject.attacker.threshold",
    settlerPoints = "squad.formReject.settler.points",
    settlerBuilder = "squad.formReject.settler.builder",
    settlerGroups = "squad.formReject.settler.groups",
    settlerThreshold = "squad.formReject.settler.threshold",
}

--- Evaluate what a squad should do. Returns a table of commands.
-- State transitions: RETREATING/GUARDING → SETTLING or RAIDING.
-- RAIDING/SETTLING/BUILDING continue without transition.
-- @param squad table with: { id, status, settlers }
-- @return table of commands
function Squad.evaluate(squad)
    local commands = {}
    local status = squad.status

    if status == SQUAD_RETREATING then
        if squad.settlers then
            table.insert(commands, Command.dispatchSquad(
                squad.id, SQUAD_SETTLING, nil, "retreat_to_settle"))
        else
            table.insert(commands, Command.dispatchSquad(
                squad.id, SQUAD_RAIDING, nil, "retreat_end"))
        end

    elseif status == SQUAD_GUARDING then
        if squad.settlers then
            table.insert(commands, Command.dispatchSquad(
                squad.id, SQUAD_SETTLING, nil, "guard_to_settle"))
        else
            table.insert(commands, Command.dispatchSquad(
                squad.id, SQUAD_RAIDING, nil, "guard_end"))
        end
        -- SQUAD_BUILDING, SQUAD_RAIDING, SQUAD_SETTLING: no transition
    end

    return commands
end

-- Decay core (tick always a parameter)

--- Decay-on-read for a chunk value: value scaled by rate^elapsed, clamped
-- at the table depth. Port of Squad.lua getDecayed with the clock threaded
-- in as a parameter.
-- @param chunk chunkValue table
-- @param index numeric chunk field index (pheromone slot)
-- @param timestampKey chunk field holding the last-refresh tick
-- @param tick the current tick (the impure caller reads the clock)
-- @param decayTable precomputed rate^i table (i = 0..depth)
-- @param depth decay table depth
function Squad.decayedValue(chunk, index, timestampKey, tick, decayTable, depth)
    local value = chunk[index] or 0
    if value == 0 then return 0 end
    local lastRefresh = chunk[timestampKey] or 0
    local elapsed = tick - lastRefresh
    local factor = decayTable[elapsed] or decayTable[depth]
    return value * factor
end

--- The source rating the write path multiplies a generator by
-- (newBase = max(gen, neighborMax*0.9) * chunkDeathRating). A source
-- chunk's field must never read below its own generator x rating: the
-- generator is a floor, exactly as the write path applies it. This is what
-- lets source chunks remain correct without periodic renewal.
local function sourceRating(chunk)
    return ChunkPropertyUtils.getCombinedDeathGeneratorRating(chunk)
        * ChunkPropertyUtils.getPathRating(chunk)
end

function Squad.playerPheromone(chunk, tick)
    local decayed = Squad.decayedValue(chunk, PLAYER_PHEROMONE, "playerLastRefresh", tick, DECAY_098, DECAY_TABLE_DEPTH)
    local floor = (chunk.playerGenerator or 0) * sourceRating(chunk)
    if floor > decayed then return floor end
    return decayed
end

function Squad.basePheromone(chunk, tick)
    local sourceVersionedValue = readSourceVersionedField(BASE_PHEROMONE, chunk)
    if sourceVersionedValue ~= nil then
        return sourceVersionedValue
    end

    -- ACO decay-on-read: diffused base pheromone decays by time since the
    -- chunk last re-processed. But in a chunk WITH nests, the base
    -- pheromone IS the nests' presence — constant while they exist. Without
    -- this exemption the value decays to ~0 within ~90 ticks of the last
    -- dirty pass and the attack-formation gate (basePheromone < 0.0001)
    -- locks the base out of forming attack squads on quiet maps.
    if (chunk.nestCount or 0) > 0 then
        return chunk[BASE_PHEROMONE] or 0
    end
    local decayed = Squad.decayedValue(chunk, BASE_PHEROMONE, "baseLastRefresh", tick, DECAY_090, DECAY_TABLE_DEPTH)
    local floor = (chunk.playerBaseGenerator or 0) * sourceRating(chunk)
    if floor > decayed then return floor end
    return decayed
end

function Squad.enemyPheromone(chunk, tick)
    local sourceVersionedValue = readSourceVersionedField(ENEMY_PHEROMONE, chunk)
    if sourceVersionedValue ~= nil then
        return sourceVersionedValue
    end

    local decayed = Squad.decayedValue(chunk, ENEMY_PHEROMONE, "enemyLastRefresh", tick, DECAY_090, DECAY_TABLE_DEPTH)
    local structures = (chunk.nestCount or 0) + (chunk.turretCount or 0)
        + (chunk.utilityCount or 0) + (chunk.hiveCount or 0)
    local floor = structures * ENEMY_PHEROMONE_MULTIPLER * sourceRating(chunk)
    if floor > decayed then return floor end
    return decayed
end

function Squad.resourcePheromone(chunk, tick)
    local decayed = Squad.decayedValue(chunk, RESOURCE_PHEROMONE, "resourceLastRefresh", tick, DECAY_090, DECAY_TABLE_DEPTH)
    local generator = chunk.resourceGenerator or 0
    if generator > 0 then
        -- the write path stores linearInterpolation(gen, 15000, 20000) * rating * 0.9
        local floor = MathUtils.linearInterpolation(generator, 15000, 20000) * sourceRating(chunk) * 0.9
        if floor > decayed then return floor end
    end
    return decayed
end

-- Raw scorers (numbers in → number out; canonical prod formulas)

function Squad.settlerFormationRaw(resource, player)
    return resource - player
end

function Squad.siegeFormationRaw(resource, base, player)
    return (resource + base) - player
end

function Squad.unitGroupFormationRaw(player, base)
    return player + base
end

function Squad.resourceMovementRaw(resource, player, enemy)
    return resource - (player * PLAYER_PHEROMONE_MULTIPLER) - enemy
end

function Squad.siegeMovementRaw(base, resource, player, enemy)
    return base + (resource * 0.5) + (player * PLAYER_PHEROMONE_MULTIPLER) - enemy
end

function Squad.siegeKamikazeMovementRaw(kamikaze, resource, enemy)
    return kamikaze + (resource * 0.5) - enemy
end

-- Predicates (plain values in → boolean out)

function Squad.isPreferredSiegeChunk(playerBaseGenerator, playerGenerator)
    return (not playerBaseGenerator) and (not playerGenerator)
end

function Squad.isPreferredResourceChunk(playerBaseGenerator, playerGenerator, resourceGenerator, enemyStructureCount)
    if (not playerBaseGenerator) and (not playerGenerator) and resourceGenerator then
        return enemyStructureCount == 0
    end
    return false
end


--- Valid unit-group formation location: no nests on the chunk.
-- NOTE: (not nestCount) — a nestCount of 0 is truthy in Lua and therefore
-- INVALID; this mirrors the prod quirk the dispatcher relied on (the
-- nestCount counter is cleared by removal, not zeroed).
function Squad.validUnitGroupLocation(chunk)
    return not chunk.nestCount
end

-- Chunk scorers ((chunk, tick) → (score, preferred); the contract
-- scoreNeighbors consumes)

function Squad.scoreResourceMovementChunk(chunk, tick)
    local preferred = Squad.isPreferredResourceChunk(
        chunk.playerBaseGenerator,
        chunk.playerGenerator,
        chunk.resourceGenerator,
        ChunkPropertyUtils.getEnemyStructureCount(chunk)
    )
    local score = Squad.resourceMovementRaw(
        Squad.resourcePheromone(chunk, tick),
        Squad.playerPheromone(chunk, tick),
        Squad.enemyPheromone(chunk, tick)
    )
    return score, preferred
end

function Squad.scoreSiegeMovementChunk(chunk, tick)
    local preferred = Squad.isPreferredSiegeChunk(
        chunk.playerBaseGenerator,
        chunk.playerGenerator
    )
    local score = Squad.siegeMovementRaw(
        Squad.basePheromone(chunk, tick),
        Squad.resourcePheromone(chunk, tick),
        Squad.playerPheromone(chunk, tick),
        Squad.enemyPheromone(chunk, tick)
    )
    return score, preferred
end

function Squad.scoreSiegeKamikazeMovementChunk(chunk, tick)
    local preferred = Squad.isPreferredSiegeChunk(
        chunk.playerBaseGenerator,
        chunk.playerGenerator
    )
    -- Kamikaze and resource pheromones are read RAW here (no decay):
    -- the prod scorer read the chunk fields directly and only the enemy
    -- term went through decay-on-read.
    local score = Squad.siegeKamikazeMovementRaw(
        chunk[KAMIKAZE_PHEROMONE],
        chunk[RESOURCE_PHEROMONE],
        Squad.enemyPheromone(chunk, tick)
    )
    return score, preferred
end

-- Formation scorers have no preferred logic: the gather location is chosen
-- purely by score (preferred = true keeps the scoreNeighbors contract).

function Squad.scoreSettlerFormationChunk(chunk, tick)
    return Squad.settlerFormationRaw(
        Squad.resourcePheromone(chunk, tick),
        Squad.playerPheromone(chunk, tick)
    ), true
end

function Squad.scoreSiegeFormationChunk(chunk, tick)
    return Squad.siegeFormationRaw(
        Squad.resourcePheromone(chunk, tick),
        Squad.basePheromone(chunk, tick),
        Squad.playerPheromone(chunk, tick)
    ), true
end

function Squad.scoreUnitGroupFormationChunk(chunk, tick)
    return Squad.unitGroupFormationRaw(
        Squad.playerPheromone(chunk, tick),
        Squad.basePheromone(chunk, tick)
    ), true
end

-- Decision evaluators (plain values in → decision tables out)

--- Formation gate: the full deploySquad rejection chain. Params:
-- { attacker, vengence, unitPoints, cost, basePheromone, playerPheromone,
--   squadCount, squadLimit, builderCount, builderLimit,
--   sentExpansionGroups, maxExpansionGroups, hasActiveCandidate,
--   formThreshold, roll }
-- Threshold semantics: reject when roll > formThreshold (roll == threshold
-- passes — the prod `Universe.random() > Universe.formSquadThreshold`
-- comparison direction).
-- @return gateDecision { accepted: bool, metric: string? }
function Squad.evaluateFormationGate(p)
    local lackingPoints = ((p.unitPoints - p.cost) < 0)
    if p.attacker then
        if lackingPoints then
            return { accepted = false, metric = FORM_REJECT_METRICS.attackerPoints }
        end
        if (p.basePheromone < 0.0001) and (p.playerPheromone < 0.0001) then
            return { accepted = false, metric = FORM_REJECT_METRICS.attackerPheromone }
        end
        if p.squadCount > p.squadLimit then
            return { accepted = false, metric = FORM_REJECT_METRICS.attackerSquadLimit }
        end
        if (not p.vengence) and (not p.hasActiveCandidate) then
            return { accepted = false, metric = FORM_REJECT_METRICS.attackerCandidate }
        end
        if p.roll > p.formThreshold then
            return { accepted = false, metric = FORM_REJECT_METRICS.attackerThreshold }
        end
    else
        if lackingPoints then
            return { accepted = false, metric = FORM_REJECT_METRICS.settlerPoints }
        end
        if p.builderCount > p.builderLimit then
            return { accepted = false, metric = FORM_REJECT_METRICS.settlerBuilder }
        end
        if (not p.vengence) and (p.sentExpansionGroups >= p.maxExpansionGroups) then
            return { accepted = false, metric = FORM_REJECT_METRICS.settlerGroups }
        end
        if p.roll > p.formThreshold then
            return { accepted = false, metric = FORM_REJECT_METRICS.settlerThreshold }
        end
    end
    return { accepted = true }
end

--- Select the formation scorer for a squad about to be deployed.
-- attacker → unitGroupFormation; otherwise siege state → siegeFormation,
-- anything else → settlerFormation.
function Squad.selectFormationScorer(attacker, baseStateAI)
    if attacker then
        return Squad.scoreUnitGroupFormationChunk
    end
    if baseStateAI == BASE_AI_STATE_SIEGE then
        return Squad.scoreSiegeFormationChunk
    end
    return Squad.scoreSettlerFormationChunk
end

--- Select the settle-march scorer for a SETTLING squad.
-- squadType may be nil (legacy squads): nil is non-siege.
function Squad.selectSettleScorer(squadType, kamikaze)
    if squadType == BASE_AI_STATE_SIEGE then
        if kamikaze then
            return Squad.scoreSiegeKamikazeMovementChunk
        end
        return Squad.scoreSiegeMovementChunk
    end
    return Squad.scoreResourceMovementChunk
end

--- Settle-command distraction: kamikaze squads ignore distractions while
-- building, everyone else is distracted by enemies.
-- @return DISTRACTION_NONE | DISTRACTION_BY_ENEMY
function Squad.selectSettleDistraction(kamikaze)
    if kamikaze then
        return DISTRACTION_NONE
    end
    return DISTRACTION_BY_ENEMY
end

--- Move-command distraction for the settle march: rabid or kamikaze squads
-- ignore distractions, everyone else is distracted by enemies.
-- @return DISTRACTION_NONE | DISTRACTION_BY_ENEMY
function Squad.selectMoveDistraction(rabid, kamikaze)
    if rabid or kamikaze then
        return DISTRACTION_NONE
    end
    return DISTRACTION_BY_ENEMY
end

--- Settle trigger: should a SETTLING squad stop marching and build?
-- Params: { distanceToOrigin, maxDistance, nestCount, hiveCount,
--   resourceGenerator, pathEndIndex, pathLength }
-- pathEndIndex is the index at the end of the march lookahead
-- (SETTLE_PATH_LENGTH steps); nil means the path has not been evaluated
-- and the path rule is inert.
-- Reasons: "max_distance" | "empty_resource_chunk" | "path_terminated"
-- @return settleDecision { settle: bool, reason: string? }
function Squad.evaluateSettleTrigger(p)
    local pathLength = p.pathLength or SETTLE_PATH_LENGTH
    if p.pathEndIndex and (p.pathEndIndex < pathLength) then
        return { settle = true, reason = REASON_PATH_TERMINATED }
    end
    if p.distanceToOrigin >= p.maxDistance then
        return { settle = true, reason = REASON_MAX_DISTANCE }
    end
    if p.resourceGenerator and (not p.nestCount) and (not p.hiveCount) then
        return { settle = true, reason = REASON_EMPTY_RESOURCE_CHUNK }
    end
    return { settle = false }
end

--- Kamikaze roll for a freshly formed squad. Params:
-- { attacker, siege, memberCount, maxSize, evolutionLevel, roll }
-- threshold = (memberCount / maxSize) * 0.2 + evolutionLevel * 0.2; a
-- SETTLER squad from a SIEGE base gets the threshold multiplied by
-- KAMIKAZE_SIEGE_MULTIPLIER (the siege wavefront presses toward the player
-- even with small squads). Comparison: kamikaze when roll < threshold.
-- @return kamikazeDecision { kamikaze: bool, threshold: number }
function Squad.evaluateKamikazeRoll(p)
    local threshold = ((p.memberCount / p.maxSize) * 0.2) + (p.evolutionLevel * 0.2)
    if (not p.attacker) and p.siege then
        threshold = threshold * KAMIKAZE_SIEGE_MULTIPLIER
    end
    return { kamikaze = p.roll < threshold, threshold = threshold }
end

Squad.DISTRACTION_NONE = DISTRACTION_NONE
Squad.DISTRACTION_BY_ENEMY = DISTRACTION_BY_ENEMY
Squad.SETTLE_REASON_MAX_DISTANCE = REASON_MAX_DISTANCE
Squad.SETTLE_REASON_EMPTY_RESOURCE_CHUNK = REASON_EMPTY_RESOURCE_CHUNK
Squad.SETTLE_REASON_PATH_TERMINATED = REASON_PATH_TERMINATED

return Squad
