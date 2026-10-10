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

-- Module cache. This module holds injected state (the source-versioned field
-- reader), and Factorio resolves both "squad_logic" and "libs/squad_logic" to
-- this file under different package.loaded keys. Without the guard those are two
-- instances with two readers, and whichever one the runtime did not install into
-- silently reads zeros. The other libraries in this tree use the same guard.
if SquadLogicG then
    return SquadLogicG
end

local Command = require("command")
local Constants = require("Constants")
local ChunkPropertyUtils = require("ChunkPropertyUtils")

local SQUAD_RAIDING    = Constants.SQUAD_RAIDING
local SQUAD_SETTLING   = Constants.SQUAD_SETTLING
local SQUAD_GUARDING   = Constants.SQUAD_GUARDING
local SQUAD_RETREATING = Constants.SQUAD_RETREATING

-- Chunk pheromone indices
local PLAYER_PHEROMONE = Constants.PLAYER_PHEROMONE
local BASE_PHEROMONE = Constants.BASE_PHEROMONE
local ENEMY_PHEROMONE = Constants.ENEMY_PHEROMONE
local RESOURCE_PHEROMONE = Constants.RESOURCE_PHEROMONE
local KAMIKAZE_PHEROMONE = Constants.KAMIKAZE_PHEROMONE

-- Decay-on-read tables (indexed 0..depth, table[0] = 1)

local PLAYER_PHEROMONE_MULTIPLER = Constants.PLAYER_PHEROMONE_MULTIPLER
local BASE_AI_STATE_SIEGE = Constants.BASE_AI_STATE_SIEGE
local KAMIKAZE_SIEGE_MULTIPLIER = Constants.KAMIKAZE_SIEGE_MULTIPLIER

local Squad = {}

-- Source-versioned field reader. The runtime adapter installs this at init.
-- Keeping it as an injected callback lets this module stay free of
-- Factorio/storage dependencies.
local sourceVersionedFieldReader

function Squad.setSourceVersionedFieldReader(reader)
    sourceVersionedFieldReader = reader
end

-- Total: returns 0 rather than nil, so every accessor built on it yields a
-- number. The reader itself returns nil only before init or for a chunk with no
-- id, and nil arithmetic downstream would be a crash rather than a stale read.
local function readSourceVersionedField(fieldType, chunk)
    if not sourceVersionedFieldReader then
        return 0
    end
    local value = sourceVersionedFieldReader(fieldType, chunk)
    if value == nil then
        return 0
    end
    return value
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

-- Every field is source-versioned now, so there is no decay-on-read left: a
-- field fades because its source fades, and the solver re-propagates the change.

-- The player field is solved from the player generator, exactly like base,
-- enemy and resource. The generator is registered with the solver on deposit
-- and on decay (ChunkPropertyUtils), so the field follows the player and fades
-- as the deposit fades.
function Squad.playerPheromone(chunk, tick)
    return readSourceVersionedField(PLAYER_PHEROMONE, chunk)
end

-- base, enemy and resource are solved by the source-versioned field and read
-- straight from it. There is no H7 fallback left: the runtime adapter is always
-- installed at init, and readSourceVersionedField is total (0 for an unknown
-- chunk), so these accessors never return nil.
function Squad.basePheromone(chunk, tick)
    return readSourceVersionedField(BASE_PHEROMONE, chunk)
end

function Squad.enemyPheromone(chunk, tick)
    return readSourceVersionedField(ENEMY_PHEROMONE, chunk)
end

function Squad.resourcePheromone(chunk, tick)
    return readSourceVersionedField(RESOURCE_PHEROMONE, chunk)
end

-- The kamikaze field is the player field's sources at a faster decay (0.95
-- against 0.9), scaled 0.95, so it is solved rather than maintained. It is what
-- kamikaze squads press toward and what retreat scoring moves away from.
function Squad.kamikazePheromone(chunk, tick)
    return readSourceVersionedField(KAMIKAZE_PHEROMONE, chunk)
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
        Squad.kamikazePheromone(chunk, tick),
        Squad.resourcePheromone(chunk, tick),
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

SquadLogicG = Squad
return Squad
