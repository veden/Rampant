function makeBiter(biterAttributes, biterAttack, biterResistances)
    return {
                type = "unit",
                name = biterAttributes.name,
                icon = "__base__/graphics/icons/small-biter.png",
                flags = {"placeable-player", "placeable-enemy", "placeable-off-grid", "breaths-air"},
                max_health = biterAttributes.health,
                order = "b-b-a",
                subgroup="enemies",
                healing_per_tick = biterAttributes.healing,
                resistances = biterResistances,
                collision_box = {{-0.4 * biterAttributes.scale, -0.4 * biterAttributes.scale}, 
                                 {0.4 * biterAttributes.scale, 0.4 * biterAttributes.scale}},
                selection_box = {{-0.7 * biterAttributes.scale, -1.5 * biterAttributes.scale}, 
                                 {0.7 * biterAttributes.scale, 0.3 * biterAttributes.scale}},
                sticker_box = {{-0.6 * biterAttributes.scale, -0.8 * biterAttributes.scale}, 
                               {0.6 * biterAttributes.scale, 0}},
                attack_parameters = biterAttack,
                vision_distance = 30,
                movement_speed = biterAttributes.movement,
                distance_per_frame = 0.1,
                pollution_to_join_attack = 200,
                distraction_cooldown = 300,
                dying_explosion = biterAttributes.explosion,
                dying_sound =  make_biter_dying_sounds(1.0),
                working_sound =  make_biter_calls(0.7),
                run_animation = biterrunanimation(biterAttributes.scale, biterAttributes.tint1, biterAttributes.tint2)
          }
end

function createSuicideAttack(attributes)
    return { type = "projectile",
             range = 0.5,
             cooldown = 35,
             ammo_category = "melee",
             ammo_type = { 
                           category = "biological",
                           action = {  type = "direct",
                                       action_delivery = { type = "instant",
                                                           target_effects = {
                                                                                { type = "create-entity",
                                                                                  entity_name = attributes.explosion
                                                                                },
                                                                                { type = "nested-result",
                                                                                  action = { type = "area",
                                                                                             perimeter = attributes.area,
                                                                                             action_delivery = { type = "instant",
                                                                                                                 target_effects = {
                                                                                                                                     {
                                                                                                                                         type = "damage",
                                                                                                                                         damage = {amount = attributes.damage, 
                                                                                                                                                   type = "explosion"}
                                                                                                                                     },
                                                                                                                                  }
                                                                                                               }
                                                                                           }
                                                                                },
                                                                                {
                                                                                   type = "create-entity",
                                                                                   entity_name = attributes.scorchmark,
                                                                                   check_buildability = true
                                                                                }
                                                                            }
                                                         }
                                    }
                         },
             sound = make_biter_roars(0.5),
             animation = biterattackanimation(smallbiterscale, small_biter_tint1, small_biter_tint2)
           }
end

function makeResistance(name, decrease, percentage)
    local obj = {
                    type = name,
                }
    if (decrease ~= 0) then
        obj.decrease = decrease
    end
    if (percentage ~= 0) then
        obj.percentage = percentage
    end
    return obj
end

data:extend({
    makeBiter({name = "small-suicide-biter",
               health = 30,
               movement = 0.25,
               healing = 0.01,
               scale = 0.55,
               tint1 = {r=0.6, g=0.0, b=0.70, a=0.8},
               tint2 = {r=0.7, g=0.0, b=0.72, a=0.4},
               explosion = "blood-explosion-small"},
              createSuicideAttack({ area = 3.5,
                                    damage = 60,
                                    explosion = "explosion",
                                    scorchmark = "small-scorchmark"})),
    makeBiter({name = "medium-suicide-biter",
               health = 125,
               movement = 0.24,
               healing = 0.01,
               scale = 0.75,
               tint1 = {r=0.6, g=0.0, b=0.70, a=0.8},
               tint2 = {r=0.7, g=0.0, b=0.72, a=0.4},
               explosion = "blood-explosion-small"},
              createSuicideAttack({ area = 4.5,
                                    damage = 140,
                                    explosion = "medium-explosion",
                                    scorchmark = "small-scorchmark"}),
              {makeResistance("physical", 4, 0),
               makeResistance("explosion", 0, 10),
               makeResistance("fire", 0, -50)}),
    makeBiter({name = "big-suicide-biter",
               health = 425,
               movement = 0.23,
               scale = 1.05,
               tint1 = {r=0.6, g=0.0, b=0.70, a=0.8},
               tint2 = {r=0.7, g=0.0, b=0.72, a=0.4},
               explosion = "blood-explosion-big"},
              createSuicideAttack({ area = 5.5,
                                    damage = 500,
                                    explosion = "big-explosion",
                                    scorchmark = "small-scorchmark"}),
              {makeResistance("physical", 8, 0),
               makeResistance("explosion", 0, 10),
               makeResistance("fire", 0, -30)}),
    makeBiter({name = "behemoth-suicide-biter",
               health = 1200,
               movement = 0.22,
               scale = 1.25,
               tint1 = {r=0.6, g=0.0, b=0.70, a=0.8},
               tint2 = {r=0.7, g=0.0, b=0.72, a=0.4},
               explosion = "blood-explosion-big"},
              createSuicideAttack({ area = 7.5,
                                    damage = 3000,
                                    explosion = "massive-explosion",
                                    scorchmark = "small-scorchmark"}),
              {makeResistance("physical", 8, 20),
               makeResistance("explosion", 10, 20),
               makeResistance("fire", 0, -10)})
})