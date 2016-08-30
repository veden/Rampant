local utils = require("BiterFunctions")

local createFireAttack = utils.createFireAttack
local makeSpitter = utils.makeSpitter

data:extend({
    makeSpitter({name = "small-fire-spitter",
                 health = 30,
                 movement = 0.25,
                 healing = 0.01,
                 scale = 0.5,
                 tint1 = {r=0.6, g=0.0, b=0.70, a=0.8},
                 tint2 = {r=0.7, g=0.0, b=0.72, a=0.4},
                 explosion = "blood-explosion-small"},
                createFireAttack({ scale = 0.5,
                                   tint1 = {r=0.6, g=0.0, b=0.70, a=0.8},
                                   tint2 = {r=0.7, g=0.0, b=0.72, a=0.4},
                                   range = 20,
                                   minRange = 1,
                                   turnRange = 1.0/3.0,
                                   firePenalty = 15}))
})