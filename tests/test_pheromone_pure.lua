-- Pheromone field accessor tests.
--
-- Every pheromone field is solved by the source-versioned solver now: there is
-- no chunk-slot maintenance pass left to test here, so what remains is that the
-- accessors read the solver and stay total.

local TestCollection = require("tests/framework")
local Constants = require("Constants")

local my_collection = TestCollection:new()

local function withReader(map, fn)
    local SquadLogic = require("libs/squad_logic")
    SquadLogic.setSourceVersionedFieldReader(function(fieldType)
        return map[fieldType]
    end)
    fn(SquadLogic)
    SquadLogic.setSourceVersionedFieldReader(nil)
end

my_collection:add('field_accessors_read_the_solver', function()
    withReader({
        [Constants.BASE_PHEROMONE] = 11,
        [Constants.ENEMY_PHEROMONE] = 22,
        [Constants.RESOURCE_PHEROMONE] = 33,
        [Constants.PLAYER_PHEROMONE] = 44,
        [Constants.KAMIKAZE_PHEROMONE] = 55,
    }, function(SquadLogic)
        local chunk = {}
        assert(SquadLogic.basePheromone(chunk, 0) == 11, "base")
        assert(SquadLogic.enemyPheromone(chunk, 0) == 22, "enemy")
        assert(SquadLogic.resourcePheromone(chunk, 0) == 33, "resource")
        assert(SquadLogic.playerPheromone(chunk, 0) == 44, "player")
        assert(SquadLogic.kamikazePheromone(chunk, 0) == 55, "kamikaze")
    end)
end)

my_collection:add('field_accessors_are_total_without_a_reader', function()
    -- nil here would be nil arithmetic in every scorer and navigator downstream.
    withReader({}, function(SquadLogic)
        local chunk = {}
        assert(SquadLogic.basePheromone(chunk, 0) == 0, "base")
        assert(SquadLogic.enemyPheromone(chunk, 0) == 0, "enemy")
        assert(SquadLogic.resourcePheromone(chunk, 0) == 0, "resource")
        assert(SquadLogic.playerPheromone(chunk, 0) == 0, "player")
        assert(SquadLogic.kamikazePheromone(chunk, 0) == 0, "kamikaze")
    end)
end)

my_collection:add('deathGenerator_is_the_difference_of_the_two_signed_fields', function()
    -- Losses are negative, which max-plus alone cannot express: the read
    -- subtracts the loss field from the gain field (min-plus == negated max-plus).
    withReader({
        [Constants.DEATH_GAIN_PHEROMONE] = 0.5,
        [Constants.DEATH_LOSS_PHEROMONE] = 0.2,
    }, function(SquadLogic)
        local combined = SquadLogic.deathGenerator({}, 0)
        assert(math.abs(combined - 0.3) < 1e-12,
            "gain minus loss, got " .. tostring(combined))
        assert(math.abs(SquadLogic.deathGeneratorRating({}, 0) - 1.0) < 1e-12,
            "a net gain leaves the rating clamped at 1")
    end)

    withReader({
        [Constants.DEATH_GAIN_PHEROMONE] = 0,
        [Constants.DEATH_LOSS_PHEROMONE] = 0.4,
    }, function(SquadLogic)
        assert(math.abs(SquadLogic.deathGenerator({}, 0) + 0.4) < 1e-12,
            "a pure loss reads negative")
        assert(math.abs(SquadLogic.deathGeneratorRating({}, 0) - 0.6) < 1e-12,
            "the rating falls with the loss")
    end)

    withReader({
        [Constants.DEATH_GAIN_PHEROMONE] = 0,
        [Constants.DEATH_LOSS_PHEROMONE] = 5,
    }, function(SquadLogic)
        assert(math.abs(SquadLogic.deathGeneratorRating({}, 0) - 0.001) < 1e-12,
            "the rating clamps at 0.001, never negative")
    end)
end)

return my_collection
