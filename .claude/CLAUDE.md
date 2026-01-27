# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Rampant is a Factorio mod that implements advanced enemy AI using pheromone-based potential fields for coordinated attacks, retreats, and player tracking.

## Commands

### Build and Deploy
```fish
./action.fish copy --dir=/mnt/gallery/factorio/mods    # Copy mod to Factorio mods folder
./action.fish zip                                      # Create mod zip for distribution
```

### Linting
```bash
luacheck .  # Uses config from .luacheckrc (24 globals defined)
```

## Architecture

### Main Entry Points
- `control.lua` - Runtime initialization and game event handlers
- `data.lua` - Data phase: loads enemy faction prototypes
- `data-updates.lua` - Data phase: updates existing prototypes
- `data-final-fixes.lua` - Final data phase modifications
- `settings.lua` - ~50 mod settings (startup, runtime-global, runtime-per-user)

### Core AI Libraries (`libs/`)
- `Processor.lua` - Main game loop, processes pheromones and attack waves
- `Squad.lua` - Unit group behavior (probing, retreats, reinforcements, commands)
- `BaseUtils.lua` - Spawner/base AI, building new enemy bases
- `ChunkPropertyUtils.lua` - Terrain analysis for AI pathfinding
- `Constants.lua` - Configuration constants (1400+ lines)

### Prototypes (`prototypes/`)
Enemy faction definitions and entity creation utilities (`prototypes/utils/` contains 30+ utility files for creating biters, worms, projectiles, particles, etc.)

## How the AI Works

1. **Pheromone System**: Enemies use potential fields (not standard pathfinding) for single-step pathfinding
2. **Squads**: Unit groups that probe defenses, retreat when outnumbered, call reinforcements, and hunt players
3. **AI States**: Raiding (pollution-based attacks), Migration (resource-seeking), Sieging (builds toward player), Onslaught (double credits)
4. **Tactical Behaviors**: Swarming, breaching, player hunting, rallying death cry, counterattacks

## Configuration

- Mod version: 3.3.4, requires Factorio >= 1.1.81
- License: GPLv3
- 24 globals defined for luacheck (game, data, script, ProcessorG, ConstantsG, SquadG, etc.)

## Working Directories

### `/mnt/backpack/code/games/factorio/luaDefinitions`
LSP directory containing comprehensive Factorio APIs, data models, and game element definitions. Use as the primary reference for Lua API documentation and type information.

### `/mnt/gallery/gameFiles/factorio/data/`
Contains the actual Lua representation of Factorio's game code. Use to understand actual game implementation details.