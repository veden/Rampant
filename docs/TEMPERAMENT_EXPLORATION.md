# Creative Temperament System Exploration

## Executive Summary

This document explores novel approaches to Rampant's temperament system that go beyond incremental improvements. The current system uses a **single scalar value** (-10000 to +10000, normalized to 0-1) that determines AI behavior states based on success/failure metrics. While functional, it creates predictable, rigid patterns that feel "punishing" rather than challenging.

**Key Problems with Current System:**
- **Rigid state transitions**: AI jumps between discrete states (PEACEFUL, AGGRESSIVE, RAIDING, etc.) based on threshold crossings
- **One-dimensional**: A single number must capture all aspects of AI "mood"
- **Predictable patterns**: Players learn to manipulate temperament through predictable tactics
- **No base personality**: All bases react identically to the same events
- **No learning**: AI doesn't adapt tactics based on what works against the player
- **Hidden mechanic**: Players can't see or manipulate temperament directly

---

## 1. Multi-Dimensional Temperament Architectures

### Current State
Single scalar: `temperamentScore` (-10000 to +10000) → `temperament` (0.0 to 1.0)

### Concept A: Trait Vector System

**Inspiration**: The Sims' needs system (Hunger, Energy, Social, Fun, Hygiene), Crusader Kings' traits

**Architecture:**
```lua
base.temperament = {
    aggression = 0.7,      -- Willingness to attack
    caution = 0.3,         -- Risk aversion
    expansion = 0.5,       -- Drive to spread
    desperation = 0.2,     -- Response to losses
    ruthlessness = 0.6,    -- Willingness to sacrifice units
    patience = 0.4,        -- Time before acting
    adaptability = 0.5     -- Rate of change
}
```

**How It Works:**
- Each trait ranges 0.0-1.0
- Traits can conflict (high aggression + high caution = internal "tension")
- State decisions use weighted combinations, not thresholds
- Example: Attack decision = `aggression * 0.4 + (1-caution) * 0.3 + desperation * 0.3`

**Trait Interactions:**
```lua
-- Internal tension creates "unstable" AI behavior
local tension = math.abs(base.temperament.aggression - base.temperament.caution)
if tension > 0.6 then
    -- AI behaves erratically, switches tactics frequently
    base.temperament.volatility = tension
end

-- Trait synergies
if base.temperament.ruthlessness > 0.7 and base.temperament.patience < 0.3 then
    -- "Feral" behavior: rapid, costly attacks
    applyFeralModifiers(base)
end
```

**Benefits:**
- Rich, nuanced behavior from simple components
- Natural variation without complexity explosion
- Conflicting traits create interesting "personality" tension
- Easier to balance (adjust individual traits)

**Challenges:**
- More complex decision logic
- Harder to predict/debug AI behavior
- Requires tuning many more parameters

**Complexity Estimate**: Medium-High
- Requires redesign of `temperamentPlanner()` function
- New trait update logic for each event type
- State transition logic becomes weighted decision trees

---

### Concept B: Emotional State Machine

**Inspiration**: GOAP (Goal-Oriented Action Planning) with emotional modifiers

**Architecture:**
```lua
base.emotionalState = {
    current = "CALM",
    history = {"CALM", "ANGER", "CALM", "FEAR"},
    intensity = 0.5,
    duration = 0,
    
    -- Emotion-specific modifiers
    anger = {threshold = 0.7, decay = 0.01, effect = "aggressive_attacks"},
    fear = {threshold = 0.3, decay = 0.02, effect = "retreat_preference"},
    frustration = {threshold = 0.5, decay = 0.005, effect = "desperate_tactics"},
    confidence = {threshold = 0.6, decay = 0.01, effect = "bold_moves"}
}
```

**How It Works:**
- Emotions accumulate based on events (damage taken = anger, unit losses = fear)
- Each emotion has threshold, decay rate, and behavioral effect
- Multiple emotions can be active simultaneously with different intensities
- Current state = dominant emotion above threshold

**Event Mapping:**
```lua
local emotionEvents = {
    unitDeath = {anger = 0.05, fear = 0.03},
    buildingDestroyed = {anger = 0.1, frustration = 0.08},
    successfulAttack = {confidence = 0.1, anger = -0.05},
    playerBuildingDestroyed = {anger = 0.15, confidence = 0.1},
    heavyLosses = {fear = 0.2, frustration = 0.15}
}

function applyEmotionEvent(base, eventType, magnitude)
    local events = emotionEvents[eventType]
    if not events then return end
    
    for emotion, delta in pairs(events) do
        base.emotionalState[emotion] = 
            math.min(1, math.max(0, base.emotionalState[emotion] + delta * magnitude))
    end
    updateDominantEmotion(base)
end
```

**Benefits:**
- More "human-like" emotional responses
- Natural emotion decay creates recovery periods
- History tracking enables "grudge" mechanics
- Intensity creates gradation within states

**Challenges:**
- Emotion interactions can be unpredictable
- Requires careful tuning of thresholds and decay rates
- More state to serialize/save

**Complexity Estimate**: Medium
- Add emotion tracking to existing base structure
- Modify temperamentPlanner to read emotions
- New event hooks for emotion updates

---

## 2. Personality & Variation Systems

### Concept A: Base Personality Profiles

**Inspiration**: Crusader Kings character personalities, XCOM enemy variety

**Architecture:**
```lua
local PERSONALITY_PROFILES = {
    ["aggressive_warlord"] = {
        aggression = 0.9, caution = 0.2, expansion = 0.6,
        patience = 0.1, ruthlessness = 0.8, adaptability = 0.5,
        preferredTactics = ["swarm", "frontal_assault"],
        reactionToLosses = "rage_attack"
    },
    ["cautious_strategist"] = {
        aggression = 0.4, caution = 0.8, expansion = 0.5,
        patience = 0.7, ruthlessness = 0.3, adaptability = 0.8,
        preferredTactics = ["flanking", "attrition"],
        reactionToLosses = "regroup"
    },
    ["desperate_survivor"] = {
        aggression = 0.7, caution = 0.3, expansion = 0.2,
        patience = 0.2, ruthlessness = 0.9, adaptability = 0.6,
        preferredTactics = ["kamikaze", "hit_and_run"],
        reactionToLosses = "desperate_charge"
    },
    ["patient_hunter"] = {
        aggression = 0.5, caution = 0.6, expansion = 0.7,
        patience = 0.9, ruthlessness = 0.4, adaptability = 0.7,
        preferredTactics = ["ambush", "surround"],
        reactionToLosses = "withdraw_and_wait"
    }
}
```

**Generation Strategy:**
```lua
-- Personality assigned at base creation based on:
-- 1. Distance from player (far = patient, near = aggressive)
-- 2. Evolution level (early = cautious, late = aggressive)
-- 3. Random seed for variety
-- 4. Environmental factors (resource-rich = confident)

function generateBasePersonality(base, evolutionLevel, distance)
    local profileKey
    local roll = math.random()
    
    -- Weight profile selection by context
    if distance < 10 then
        -- Close bases more likely to be aggressive
        profileKey = roll < 0.6 and "aggressive_warlord" or "cautious_strategist"
    elseif evolutionLevel > 0.7 then
        -- Late game more varied
        profileKey = PERSONALITY_KEYS[math.random(#PERSONALITY_KEYS)]
    else
        -- Early game more cautious
        profileKey = roll < 0.7 and "cautious_strategist" or "patient_hunter"
    end
    
    -- Clone profile and apply random variance
    local baseProfile = table.copy(PERSONALITY_PROFILES[profileKey])
    applyVariance(baseProfile, 0.15)  -- ±15% variance
    
    return baseProfile
end
```

**Benefits:**
- Each base feels unique
- Players must adapt tactics per base
- Natural difficulty progression
- Memorable "boss" bases with distinct personalities

**Challenges:**
- Requires personality-aware squad formation
- More complex debugging (which personality caused this behavior?)
- Risk of creating unfair matchups

**Complexity Estimate**: Medium
- Add personality table to base structure
- Modify squad formation to consider personality
- New personality assignment logic at base creation

---

### Concept B: Dynamic Personality Evolution

**Inspiration**: Machine learning reward systems, behavioral psychology

**Architecture:**
```lua
base.personalityEvolution = {
    -- Baseline personality
    baseline = {aggression = 0.5, caution = 0.5, ...},
    
    -- Learned adjustments based on success
    learned = {aggression = 0.0, caution = 0.0, ...},
    
    -- Success counters for each tactic
    tacticSuccess = {
        swarm = {wins = 3, losses = 1, effectiveness = 0.75},
        flanking = {wins = 1, losses = 2, effectiveness = 0.33},
        siege = {wins = 0, losses = 1, effectiveness = 0.0}
    },
    
    -- Player counter tracking
    playerCounters = {
        laserTurrets = {count = 15, effectivenessAgainstUs = 0.8},
        artillery = {count = 3, effectivenessAgainstUs = 0.9},
        walls = {count = 50, effectivenessAgainstUs = 0.4}
    }
}
```

**Learning Mechanism:**
```lua
function updatePersonalityBasedOnOutcome(base, tactic, outcome, playerComposition)
    -- Update tactic success tracking
    local tacticData = base.personalityEvolution.tacticSuccess[tactic]
    if outcome == "win" then
        tacticData.wins = tacticData.wins + 1
    else
        tacticData.losses = tacticData.losses + 1
    end
    tacticData.effectiveness = tacticData.wins / (tacticData.wins + tacticData.losses)
    
    -- Adjust learned personality based on what worked
    if outcome == "win" then
        if tactic == "swarm" then
            base.personalityEvolution.learned.aggression = 
                base.personalityEvolution.learned.aggression + 0.05
            base.personalityEvolution.learned.caution = 
                base.personalityEvolution.learned.caution - 0.03
        end
    else
        -- Losses make AI more cautious about that tactic
        if tactic == "swarm" then
            base.personalityEvolution.learned.aggression = 
                math.max(0, base.personalityEvolution.learned.aggression - 0.03)
            base.personalityEvolution.learned.caution = 
                base.personalityEvolution.learned.caution + 0.05
        end
    end
    
    -- Decay learned traits over time (forgetting mechanism)
    decayLearnedTraits(base.personalityEvolution, 0.001)
end
```

**Benefits:**
- AI adapts to player strategy
- Each playthrough feels different
- Players must vary tactics to avoid AI adaptation
- Creates "arms race" dynamic

**Challenges:**
- Risk of AI becoming too strong
- Learning must be slow enough to not frustrate players
- Requires tracking extensive battle data
- Can feel "unfair" if AI seems to specifically counter player strategy

**Complexity Estimate**: High
- New data structures for tracking
- Integration with battle outcome reporting
- Learning rate tuning critical
- Potential performance impact from tracking

---

## 3. Learning & Adaptation Systems

### Concept A: Tactical Effectiveness Memory

**Inspiration**: Reinforcement learning Q-tables, game AI that learns player patterns

**Architecture:**
```lua
base.tacticalMemory = {
    -- Matrix: [tactic][playerDefense] = effectiveness (0.0-1.0)
    effectiveness = {
        swarm = {laserTurrets = 0.3, artillery = 0.2, walls = 0.6},
        flanking = {laserTurrets = 0.7, artillery = 0.4, walls = 0.3},
        siege = {laserTurrets = 0.5, artillery = 0.6, walls = 0.8}
    },
    
    -- Recent battle history (sliding window)
    recentBattles = {
        {tactic = "swarm", playerDefense = "laserTurrets", outcome = "loss", timestamp = 12345},
        {tactic = "flanking", playerDefense = "walls", outcome = "win", timestamp = 12400}
    },
    
    -- Player behavior patterns
    playerPatterns = {
        retreatsAfterLosses = true,
        overcommitsToDefense = false,
        prefersRangedCombat = true,
        averageResponseTime = 450  -- ticks
    }
}
```

**Decision Logic:**
```lua
function selectTactic(base, playerComposition)
    local bestTactic = nil
    local bestScore = -1
    
    for tactic, defenses in pairs(base.tacticalMemory.effectiveness) do
        local score = 0
        local totalWeight = 0
        
        for defense, effectiveness in pairs(defenses) do
            if playerComposition[defense] then
                -- Weight by how much of that defense player has
                score = score + effectiveness * playerComposition[defense]
                totalWeight = totalWeight + playerComposition[defense]
            end
        end
        
        if totalWeight > 0 then
            score = score / totalWeight
        else
            -- No data, use neutral score
            score = 0.5
        end
        
        -- Add exploration factor (try less successful tactics occasionally)
        score = score + (math.random() * 0.1)
        
        if score > bestScore then
            bestScore = score
            bestTactic = tactic
        end
    end
    
    return bestTactic
end
```

**Benefits:**
- AI genuinely learns from experience
- Players rewarded for varied tactics
- Creates strategic depth
- Natural difficulty scaling

**Challenges:**
- Requires extensive battle tracking
- Early game AI may be weak (no data)
- Risk of AI "overfitting" to recent battles
- Memory management (how much history to keep?)

**Complexity Estimate**: High
- New memory structures per base
- Battle outcome reporting system
- Tactic selection logic overhaul
- Performance considerations for large maps

---

### Concept B: Counter-Adaptation System

**Inspiration**: Rock-paper-scissors game balance, real-time strategy AI

**Architecture:**
```lua
base.adaptation = {
    -- Detected player strategies
    detectedStrategies = {
        ["heavy_defense"] = {confidence = 0.8, detectedAt = 12000},
        ["rush"] = {confidence = 0.3, detectedAt = 15000},
        ["turtle"] = {confidence = 0.9, detectedAt = 10000}
    },
    
    -- Counter strategies available
    counters = {
        ["heavy_defense"] = {"siege", "flanking", "attrition"},
        ["rush"] = {"ambush", "defensive_hold"},
        ["turtle"] = {"expansion", "harassment", "siege"}
    },
    
    -- Adaptation cooldown (prevent rapid switching)
    lastAdaptation = 0,
    adaptationCooldown = 36000  -- 10 minutes
}
```

**Detection Logic:**
```lua
function detectPlayerStrategy(base, playerBuildings)
    local totalBuildings = 0
    local defenses = 0
    let production = 0
    
    for building, count in pairs(playerBuildings) do
        totalBuildings = totalBuildings + count
        if isDefensiveBuilding(building) then
            defenses = defenses + count
        elseif isProductionBuilding(building) then
            production = production + count
        end
    end
    
    local defenseRatio = defenses / math.max(1, totalBuildings)
    local timeSinceStart = game.tick - base.createdTick
    
    -- Strategy detection
    if timeSinceStart < 3600 and defenseRatio > 0.6 then
        updateStrategyConfidence(base, "rush", 0.1)
    elseif defenseRatio > 0.7 then
        updateStrategyConfidence(base, "heavy_defense", 0.05)
    elseif production > defenses * 2 then
        updateStrategyConfidence(base, "expansion", 0.03)
    end
end

function updateStrategyConfidence(base, strategy, delta)
    local data = base.adaptation.detectedStrategies[strategy]
    if data then
        data.confidence = math.min(1, math.max(0, data.confidence + delta))
    end
end
```

**Benefits:**
- AI responds intelligently to player composition
- Players must adapt their own strategies
- Creates dynamic meta-game
- Feels more "alive" and responsive

**Challenges:**
- Detection must be accurate but not perfect
- Cooldowns needed to prevent chaos
- Risk of feeling "punishing" if AI always counters perfectly
- Requires clear counter relationships

**Complexity Estimate**: Medium-High
- Player building tracking system
- Strategy detection algorithms
- Counter relationship definitions
- Adaptation timing logic

---

## 4. Context-Aware Temperament

### Concept A: Game Phase Modifiers

**Inspiration**: Campaign difficulty scaling, stage-based AI behavior

**Architecture:**
```lua
local GAME_PHASES = {
    early = {range = {0, 0.3}, modifiers = {
        aggression = 0.7,      -- More cautious early
        patience = 0.8,        -- Take time to build up
        expansion = 0.9,       -- Focus on spreading
        ruthlessness = 0.3     -- Preserve units
    }},
    mid = {range = {0.3, 0.7}, modifiers = {
        aggression = 1.0,      -- Full aggression
        patience = 0.5,        -- Balanced
        expansion = 0.7,       -- Still expanding but attacking
        ruthlessness = 0.6     -- Willing to trade
    }},
    late = {range = {0.7, 1.0}, modifiers = {
        aggression = 1.2,      -- Maximum aggression
        patience = 0.3,        -- Constant pressure
        expansion = 0.5,       -- Focus on destruction
        ruthlessness = 0.9     -- All-out war
    }}
}

function getPhaseAdjustedTemperament(base, evolutionLevel)
    local phase
    for phaseName, phaseData in pairs(GAME_PHASES) do
        if evolutionLevel >= phaseData.range[1] and 
           evolutionLevel < phaseData.range[2] then
            phase = phaseData
            break
        end
    end
    
    local baseTemperament = getBaseTemperament(base)
    local adjusted = {}
    
    for trait, value in pairs(baseTemperament) do
        local modifier = phase.modifiers[trait] or 1.0
        adjusted[trait] = math.min(1, math.max(0, value * modifier))
    end
    
    return adjusted
end
```

**Benefits:**
- Natural difficulty progression
- AI behavior matches game stage expectations
- Predictable scaling players can understand
- Easy to balance per-phase

**Challenges:**
- Phase transitions can be jarring
- May feel too predictable
- Doesn't account for player skill variation

**Complexity Estimate**: Low-Medium
- Add phase detection logic
- Modify temperament calculation to apply modifiers
- Minimal structural changes

---

### Concept B: Environmental & Temporal Modifiers

**Inspiration**: Real-world animal behavior, weather systems in games

**Architecture:**
```lua
function getContextualModifiers(base, context)
    local modifiers = {
        aggression = 1.0,
        caution = 1.0,
        patience = 1.0
    }
    
    -- Time of day
    if context.isNight then
        modifiers.aggression = modifiers.aggression * 1.3  -- More aggressive at night
        modifiers.caution = modifiers.caution * 0.7
    end
    
    -- Surface type
    if context.surfaceType == "volcanic" then
        modifiers.patience = modifiers.patience * 0.6  -- More impatient in harsh environments
    elseif context.surfaceType == "island" then
        modifiers.expansion = modifiers.expansion * 0.5  -- Limited expansion on islands
    end
    
    -- Pollution density
    if context.pollutionDensity > 0.8 then
        modifiers.aggression = modifiers.aggression * 1.2  -- More aggressive in high pollution
        modifiers.desperation = modifiers.desperation + 0.1
    end
    
    -- Player proximity
    if context.playerDistance < 10 then
        modifiers.caution = modifiers.caution * 0.5  -- Less cautious when player is close
        modifiers.aggression = modifiers.aggression * 1.4
    end
    
    -- Resource richness
    if context.resourceDensity > 0.7 then
        modifiers.patience = modifiers.patience * 1.3  -- More patient when resources are abundant
        modifiers.expansion = modifiers.expansion * 1.2
    end
    
    return modifiers
end
```

**Benefits:**
- Environmentally responsive AI
- Encourages player environmental manipulation
- Adds strategic depth (lure AI into unfavorable terrain)
- Natural variation without explicit personality

**Challenges:**
- Requires context data collection
- Can feel arbitrary to players
- May create unfair situations in certain environments

**Complexity Estimate**: Medium
- Context gathering system
- Modifier application logic
- Surface/environment type definitions

---

## 5. Narrative/Story-Driven Temperament

### Concept A: Event Memory & Grudges

**Inspiration**: Crusader Kings grudges, RPG reputation systems

**Architecture:**
```lua
base.narrativeMemory = {
    -- Significant events that shape "personality"
   重大事件 = {
        {
            type = "humiliation",
            description = "Lost 500 units to artillery barrage",
            timestamp = 12000,
            emotionalImpact = {anger = 0.3, fear = 0.1},
            lastingEffect = "artillery_phobia"
        },
        {
            type = "victory",
            description = "Destroyed player's main base",
            timestamp = 15000,
            emotionalImpact = {confidence = 0.4, aggression = 0.2},
            lastingEffect = "aggressive_tendency"
        }
    },
    
    -- Current "story arc"
    currentArc = {
        theme = "revenge",
        target = "player_base_3",
        motivation = "They destroyed our mother nest!",
        goal = "Destroy player's artillery positions",
        progress = 0.6
    },
    
    -- Relationships with specific player elements
    relationships = {
        ["artillery"] = {sentiment = -0.8, encounters = 12, lastEncounter = 14500},
        ["turrets"] = {sentiment = -0.5, encounters = 25, lastEncounter = 15200},
        ["player_character"] = {sentiment = -0.3, encounters = 8, lastEncounter = 15800}
    }
}
```

**Behavioral Impact:**
```lua
function getNarrativeModifiers(base)
    local modifiers = {}
    
    -- Current arc influences behavior
    if base.narrativeMemory.currentArc then
        local arc = base.narrativeMemory.currentArc
        if arc.theme == "revenge" then
            modifiers.aggression = modifiers.aggression * 1.5
            modifiers.patience = modifiers.patience * 0.5
            -- Prioritize targets related to revenge
            modifiers.targetPriority = arc.target
        elseif arc.theme == "expansion" then
            modifiers.expansion = modifiers.expansion * 1.4
            modifiers.caution = modifiers.caution * 0.8
        end
    end
    
    -- Grudges affect target selection
    for target, relationship in pairs(base.narrativeMemory.relationships) do
        if relationship.sentiment < -0.5 then
            -- Strong negative sentiment = priority target
            modifiers.targetPriority[target] = 1.0 - relationship.sentiment
        end
    end
    
    return modifiers
end
```

**Benefits:**
- Creates memorable stories and moments
- AI feels like it has history and motivation
- Players can track AI "personality" development
- Natural quest/objective generation

**Challenges:**
- Requires narrative event tracking
- Story elements may feel forced
- Can complicate debugging
- May not appeal to all players

**Complexity Estimate**: High
- Event tracking and significance determination
- Narrative state management
- Story arc progression logic
- Integration with behavior systems

---

### Concept B: Base-Specific Goals

**Inspiration**: Campaign missions, AI with objectives beyond "kill player"

**Architecture:**
```lua
base.goals = {
    primary = {
        type = "resource_control",
        target = "iron_ore_patch_42",
        priority = 0.9,
        deadline = 20000,
        successCriteria = "control_resource_for_1_hour"
    },
    secondary = {
        type = "territory_expansion",
        targetDirection = "northeast",
        priority = 0.6,
        deadline = nil,
        successCriteria = "establish_3_new_bases"
    },
    defensive = {
        type = "protect_mother_nest",
        target = "this_base",
        priority = 1.0,
        deadline = nil,
        successCriteria = "never_destroyed"
    }
}
```

**Goal-Driven Behavior:**
```lua
function selectActionBasedOnGoals(base, availableActions)
    local bestAction = nil
    local bestScore = -1
    
    for _, action in pairs(availableActions) do
        local score = 0
        
        -- Score action based on how well it advances goals
        for goalType, goal in pairs(base.goals) do
            local goalContribution = evaluateActionForGoal(action, goal)
            score = score + goalContribution * goal.priority
        end
        
        -- Add some randomness for variety
        score = score + (math.random() * 0.1)
        
        if score > bestScore then
            bestScore = score
            bestAction = action
        end
    end
    
    return bestAction
end

function evaluateActionForGoal(action, goal)
    -- Simple examples:
    if goal.type == "resource_control" and action.type == "defend_resource" then
        return 0.9
    elseif goal.type == "territory_expansion" and action.type == "build_base" then
        return 0.8
    elseif goal.type == "protect_mother_nest" and action.type == "retreat_to_base" then
        return 0.7
    else
        return 0.1  -- Neutral contribution
    end
end
```

**Benefits:**
- AI has clear motivations players can understand
- Creates strategic objectives beyond "survive"
- Natural variation in AI behavior
- Players can anticipate and counter AI plans

**Challenges:**
- Goal selection logic can be complex
- May conflict with existing temperament system
- Requires clear goal communication to players
- Risk of AI being too predictable

**Complexity Estimate**: Medium-High
- Goal definition system
- Goal evaluation logic
- Goal selection and prioritization
- Integration with existing AI systems

---

## 6. Player-Visible Temperament

### Concept A: Visible AI Psychology Indicators

**Inspiration**: Detective games showing suspect motives, strategy games with fog-of-war intelligence

**Implementation:**
```lua
-- Player-visible UI elements
function getTemperamentDisplay(base)
    return {
        -- Visual indicator (color-coded)
        moodColor = getMoodColor(base.temperament),
        
        -- Text description
        moodDescription = getMoodDescription(base.temperament),
        
        -- Estimated tendencies
        tendencies = {
            likelyToAttack = base.temperament.aggression > 0.6,
            likelyToRetreat = base.temperament.caution > 0.7,
            desperate = base.temperament.desperation > 0.8
        },
        
        -- Recent activity (last few actions)
        recentActivity = {
            "Attacked player base at 14:32",
            "Lost 50 units to artillery",
            "Building up forces"
        },
        
        -- Weaknesses (if discovered)
        discoveredWeaknesses = {
            "Vulnerable to flanking",
            "Avoids artillery zones"
        }
    }
end

function getMoodColor(temperament)
    if temperament.aggression > 0.8 then return {1, 0, 0}  -- Red
    elseif temperament.aggression > 0.6 then return {1, 0.5, 0}  -- Orange
    elseif temperament.aggression > 0.4 then return {1, 1, 0}  -- Yellow
    elseif temperament.aggression > 0.2 then return {0, 1, 0}  -- Green
    else return {0, 0.5, 1}  -- Blue (calm)
    end
end

function getMoodDescription(temperament)
    if temperament.aggression > 0.8 and temperament.caution < 0.3 then
        return "FURIOUS - Will attack anything"
    elseif temperament.aggression > 0.6 and temperament.desperation > 0.7 then
        return "DESPERATE - Dangerous when cornered"
    elseif temperament.caution > 0.7 then
        return "CAUTIOUS - Won't take risks"
    elseif temperament.patience > 0.8 then
        return "PATIENT - Waiting for opportunity"
    else
        return "BALANCED - Standard threat level"
    end
end
```

**Benefits:**
- Players can make informed decisions
- Reduces frustration from hidden mechanics
- Creates strategic depth (exploit visible weaknesses)
- More accessible to new players

**Challenges:**
- May reduce surprise/challenge
- UI space requirements
- Risk of information overload
- Balance between helpful and hand-holding

**Complexity Estimate**: Low-Medium
- UI components for display
- Data gathering for display
- Minimal backend changes

---

### Concept B: Player Manipulation Mechanics

**Inspiration**: Stealth games, animal taming, psychological warfare

**Mechanics:**
```lua
-- Player actions that affect AI temperament
local PLAYER_TEMPERAMENT_ACTIONS = {
    -- Decoy systems to agitate AI
    decoy = {
        cost = {iron = 10, copper = 5},
        effect = {aggression = +0.3, caution = -0.2},
        duration = 600,  -- 10 minutes
        description = "Emits signals that anger nearby AI bases"
    },
    
    -- Retreat feints to calm AI
    tacticalRetreat = {
        cost = {none},
        effect = {aggression = -0.2, caution = +0.1},
        duration = 300,
        description = "Feigning weakness to make AI more cautious"
    },
    
    -- Show of force to intimidate
    showOfForce = {
        cost = {requires_visible_army},
        effect = {caution = +0.4, aggression = -0.3},
        duration = 900,
        description = "Massing troops makes AI more hesitant"
    },
    
    -- Targeted strikes to create fear
    surgicalStrike = {
        cost = {requires_stealth_units},
        effect = {fear = +0.5, patience = -0.2},
        duration = 1200,
        description = "Destroying key structures creates hesitation"
    },
    
    -- Peace offerings (if diplomacy mod exists)
    peaceOffering = {
        cost = {resources = 1000},
        effect = {aggression = -0.5, caution = +0.2},
        duration = 1800,
        description = "Trade offers reduce aggression temporarily"
    }
}

function applyPlayerTemperamentAction(base, actionType, player)
    local action = PLAYER_TEMPERAMENT_ACTIONS[actionType]
    if not action then return false end
    
    -- Check cost
    if not player.hasResources(action.cost) then
        return false
    end
    player.spendResources(action.cost)
    
    -- Apply effect to nearby bases
    for _, nearbyBase in pairs(findNearbyBases(base.position, 20)) do
        for trait, delta in pairs(action.effect) do
            nearbyBase.temperament[trait] = 
                math.min(1, math.max(0, nearbyBase.temperament[trait] + delta))
        end
        
        -- Log the manipulation for player feedback
        game.print(string.format("Base %d temperament affected by %s", 
            nearbyBase.id, actionType))
    end
    
    -- Schedule effect expiration
    scheduleEffectExpiration(base, actionType, action.duration)
    
    return true
end
```

**Benefits:**
- Creates new strategic layer
- Players can actively manipulate AI
- Encourages experimentation
- Makes temperament a gameplay mechanic, not just background logic

**Challenges:**
- Risk of making AI too manipulatable
- Balance complexity (costs, durations, effects)
- May feel "cheap" if overpowered
- Requires clear feedback to players

**Complexity Estimate**: High
- New player action systems
- Cost and balance tuning
- Effect duration management
- UI for tracking manipulations
- Integration with existing systems

---

## Recommendations & Next Steps

### Immediate Wins (Low Complexity, High Impact)

1. **Context-Aware Modifiers** (Section 4B)
   - Add time-of-day and pollution modifiers
   - Simple multiplier system
   - Creates natural variation without major restructuring

2. **Player-Visible Temperament** (Section 6A)
   - Display current temperament state in UI
   - Color-coded indicators
   - Reduces frustration, increases strategic depth

### Medium-Term Projects

3. **Multi-Dimensional Traits** (Section 1A)
   - Replace single scalar with trait vector
   - Requires redesign of temperamentPlanner
   - Foundation for all other advanced features

4. **Game Phase Modifiers** (Section 4A)
   - Add early/mid/late game behavior scaling
   - Complements trait system well
   - Easy to tune and balance

### Long-Term Vision

5. **Learning & Adaptation** (Section 3A/B)
   - Tactical effectiveness memory
   - Counter-adaptation system
   - Requires extensive testing to avoid frustration

6. **Narrative Elements** (Section 5A/B)
   - Event memory and grudges
   - Base-specific goals
   - Adds story depth but may not appeal to all players

### Implementation Priority Matrix

| Feature | Impact | Complexity | Priority |
|---------|--------|------------|----------|
| Visible Temperament | High | Low | 1 |
| Context Modifiers | Medium | Low | 2 |
| Trait Vector System | High | Medium | 3 |
| Game Phase Modifiers | Medium | Low | 4 |
| Base Personalities | High | Medium | 5 |
| Tactical Memory | Very High | High | 6 |
| Player Manipulation | Medium | High | 7 |
| Narrative Memory | Medium | High | 8 |

### Testing Strategy

1. **Prototype each concept** in isolation first
2. **A/B testing** with player base
3. **Metrics to track:**
   - Player satisfaction scores
   - Average game duration
   - Win/loss ratios
   - Tactics variety (are players using diverse strategies?)
   - AI behavior diversity (are bases acting differently?)

### Risk Mitigation

- **Start small**: Implement visible temperament first to gauge player response
- **Gradual rollout**: Beta test major changes with dedicated community
- **Fallback options**: Keep old system as fallback if new system causes issues
- **Configurable**: Make new features toggleable via settings

---

## Conclusion

The current temperament system serves its purpose but limits Rampant's potential for creating dynamic, engaging AI behavior. By moving to a multi-dimensional, context-aware, and player-visible system, Rampant can create more memorable, strategic, and varied gameplay experiences.

The recommended approach is **incremental innovation**: start with low-risk, high-impact features (visible temperament, context modifiers) and gradually introduce more complex systems (trait vectors, learning AI) as the foundation is built.

This exploration provides a roadmap for transforming Rampant's AI from "challenging but predictable" to "dynamic, adaptive, and memorable."

---

*Document created: 2026-02-28*
*Author: Exploration Agent*
*Status: Ready for review and implementation planning*