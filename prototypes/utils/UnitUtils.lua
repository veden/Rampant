local unitUtils = {}

function unitUtils.spitter_alternative_attacking_animation_sequence()
    return {
        warmup_frame_sequence = { 1, 2, 3, 4, 5, 6 },
        warmup2_frame_sequence = { 7, 7, 7, 7, 7, 7 },
        attacking_frame_sequence = { 7, 8, 9, 10, 11,  12, 13, 14, 13, 14,  13, 12, 11, 10, 9,  8 },
        cooldown_frame_sequence = { 7 },
        prepared_frame_sequence = { 7 },
        back_to_walk_frame_sequence = { 6, 5, 4, 3, 2, 1 },

        warmup_animation_speed = 1 / 6 * 0.4,
        attacking_animation_speed = 1 / 16 * 0.4,
        cooldown_animation_speed = 1 / 1 * 0.4 * 0.125;
        prepared_animation_speed = 1 / 1 * 0.5 * 0.4,
        back_to_walk_animation_speed = 1 / 6 * 0.4,
    }
end

return unitUtils
