globals = {
        "mods",
        "game",
        "util",
        "data",
        "remote",
        "settings",
        "commands",
        "global",
        "storage",
        "log",
        "helpers",
        "rendering",
        "table_size",
        "script",
        "defines",
        "ProcessorG",
        "ConstantsG",
        "ChunkPropertyUtilsG",
        "ChunkUtilsG",
        "MapUtilsG",
        "MetricsG",
        "MathUtilsG",
        "SquadG",
        "SquadLogicG",
        "unitUtilsG",
        "BaseUtilsG",
        "UtilsG",
        "RampantGlobalVariables",
        "IntegrationTests"
}

max_line_length = false

-- Scratch benchmark copies of the mod (one per build under test) live here.
-- They are git-ignored, so linting them only reports thousands of stale
-- duplicate warnings.
exclude_files = {
    ".bench/**",
}
