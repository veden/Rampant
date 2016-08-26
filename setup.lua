local setup = {}

local constants = require("libs/Constants")

function setup.initializeTemps(temps)
    local attackAreaPosition = {x=0, y=0}
    temps[constants.ATTACK_AREA_POSITION] = attackAreaPosition
    temps[constants.ATTACK_AREA] = { 
                                      type=defines.command.attack_area,
                                      destination=attackAreaPosition,
                                      radius=constants.HALF_CHUNK_SIZE,
                                      distraction=defines.distraction.by_anything
                                   }
                                      
    temps[constants.ATTACK_PLAYER] = {
                                        type=defines.command.attack,
                                        target=1
                                     }
                                     
    temps[constants.CARDINAL_WITH_DIRECTION] = {{d=1}, {d=2}, {d=3}, {d=4}}
end

return setup