# Rampant Temperament System: Player Experience Analysis

## Executive Summary

This analysis examines the Rampant temperament system from a **player's perspective**, translating technical implementation into actual gameplay experience. Based on code analysis, documentation, and changelog history, this document describes what players actually experience when playing with Rampant's AI system.

---

## 1. What the Player Actually Experiences

### The Core Gameplay Loop

**Player builds base** → **AI responds based on temperament** → **Player defends/counterattacks** → **AI adapts behavior**

The temperament system creates the following player-facing experience:

#### Early Game (0-30% Evolution)
- **What players see**: Relatively calm AI with occasional probing attacks
- **What's happening**: AI bases are mostly in MIGRATING or PEACEFUL states (temperament 0.0-0.4)
- **Player feeling**: "The planet is watching me, but not fully committed yet"
- **Attack patterns**: Small, infrequent raids; AI is "feeling out" defenses
- **Strategic implication**: Players have time to build defenses before major threats

#### Mid Game (30-70% Evolution)
- **What players see**: More aggressive attacks, varied tactics
- **What's happening**: AI enters AGGRESSIVE and RAIDING states (temperament 0.4-0.6)
- **Player feeling**: "The pressure is ramping up, I need better defenses"
- **Attack patterns**: Larger waves, more coordinated attacks, night assaults
- **Strategic implication**: Players must balance expansion with defense

#### Late Game (70%+ Evolution)
- **What players see**: Constant pressure, overwhelming attacks
- **What's happening**: AI in ONSLAUGHT and SIEGE states (temperament 0.6-1.0)
- **Player feeling**: "Full-scale war, everything coming at me"
- **Attack patterns**: Massive coordinated assaults, multiple fronts, siege tactics
- **Strategic implication**: Survival mode, players must go on offense or be overwhelmed

### Visible AI Behaviors

Players directly experience these temperament-driven behaviors:

1. **Attack Wave Frequency**: Higher temperament = more frequent attacks
2. **Attack Size**: Higher temperament = larger squad sizes
3. **Behavioral State**: 
   - Low temperament: AI expands, builds bases, occasional raids
   - Medium temperament: Balanced attack/expansion
   - High temperament: All-out assault, siege behavior
4. **Reinforcement Behavior**: AI calls backup when attacking (more aggressive at high temperament)
5. **Retreat Behavior**: AI retreats when taking heavy losses (more likely at low temperament)

---

## 2. When Does It Feel Fair vs. Unfair?

### Fair Moments ✅

**Predictable Escalation**
- Players can see evolution percentage and anticipate AI aggression
- Temperament changes correlate with player actions (destroying nests calms AI)
- **Why it feels fair**: "I know what I'm working toward, and I can influence it"

**Responsive to Player Actions**
- Destroying enemy units/buildings reduces AI aggression
- Building defenses successfully calms nearby bases
- **Why it feels fair**: "My actions matter, I can push back"

**Varied Attack Patterns**
- AI doesn't always attack the same way
- Different states produce different tactics (raids vs. sieges vs. assaults)
- **Why it feels fair**: "I need to adapt my strategy, not just build one perfect defense"

**Reinforcement Logic**
- AI calls backup when attacking nests under pressure
- Makes tactical sense from AI perspective
- **Why it feels fair**: "It's smart, not cheap"

### Unfair Moments ⚠️

**Sudden State Changes**
- AI can jump from "calm" to "onslaught" based on hidden calculations
- Players don't see temperament value directly
- **Why it feels unfair**: "One minute it was quiet, now I'm under full assault"

**Compound Pressure**
- Multiple bases can reach high temperament simultaneously
- Rocket launches, artillery blasts, ion cannon use all spike temperament
- **Why it feels unfair**: "I did something smart (destroyed a nest) and now everything attacks"

**Hidden Mechanics**
- Temperament score is invisible to players
- State transitions use probability rolls within temperature ranges
- **Why it feels unfair**: "I don't know why it's attacking now"

**Evolution-Driven Escalation**
- AI gets stronger regardless of player skill
- High evolution = high aggression, even if player is doing well
- **Why it feels unfair**: "I'm winning but the game makes it harder anyway"

**The "Punishment" Dynamic**
- From code analysis: "While functional, it creates predictable, rigid patterns that feel 'punishing' rather than challenging"
- Successful player actions (destroying nests) can backfire by triggering high-temperament responses
- **Why it feels unfair**: "Winning makes it worse"

---

## 3. What Creates Frustrating Moments?

### The "Backfire Effect"

**Scenario**: Player successfully destroys a major enemy nest with artillery
**What happens**: 
- `artilleryBlasts` counter increases
- Temperament increases by `13.924864` points per blast
- AI may shift to RAIDING or ONSLAUGHT state
- Multiple bases send larger attack waves

**Player frustration**: "I just won a major battle and now I'm under heavier attack?"

**Root cause**: The system rewards destruction with increased aggression, creating a paradox where success feels like failure.

### The "Hidden Threshold" Problem

**Scenario**: Player has been defending well for hours
**What happens**:
- AI slowly accumulates temperament through various factors
- At some invisible threshold, state changes to ONSLAUGHT
- Attack frequency and size jump dramatically
- Player is overwhelmed unexpectedly

**Player frustration**: "I was doing fine, then suddenly everything went wrong"

**Root cause**: Temperament is calculated internally with no player visibility. State transitions use probability rolls that can't be anticipated.

### The "Snowball" Effect

**Scenario**: Player loses a major defense
**What happens**:
- `lostEnemyUnits` counter is low (player killed few)
- `destroyPlayerBuildings` counter is high
- Temperament increases significantly
- AI enters aggressive state with more resources
- Next attack is stronger, player loses more

**Player frustration**: "One mistake and I'm spiraling"

**Root cause**: The system compounds failures. Losing makes the AI stronger, which makes losing more likely.

### The "No Breathing Room" Problem

**Scenario**: Player reaches late game
**What happens**:
- Multiple bases at high temperament
- ONSLAUGHT state = 2x AI points per cycle
- Constant attack waves from multiple directions
- No downtime for repairs or expansion

**Player frustration**: "I can't catch my breath"

**Root cause**: High temperament states don't allow recovery periods. AI maintains pressure continuously.

### The "Random Probability" Frustration

**Scenario**: AI is in temperament range 0.6-0.8
**What happens**:
- State selection uses probability rolls:
  - 40% AGGRESSIVE
  - 20% ONSLAUGHT
  - 20% RAIDING
  - 20% PEACEFUL (if enabled)
- Roll could be anything each cycle
- Player can't predict what's coming

**Player frustration**: "I don't know if I'm getting a raid or an assault"

**Root cause**: Randomness within temperament ranges creates unpredictability that feels arbitrary rather than strategic.

---

## 4. What Creates Satisfying Moments?

### The "Cat and Mouse" Dynamic

**Scenario**: Player uses hit-and-run tactics against enemy bases
**What happens**:
- Player destroys some units, retreats before major counterattack
- AI temperament fluctuates but doesn't reach extremes
- Player maintains control of engagement timing
- Can pick off bases one by one

**Player satisfaction**: "I'm outmaneuvering the AI"

**Why it works**: Player feels clever using system mechanics to their advantage.

### The "Desperate Defense" Victory

**Scenario**: Player's base is surrounded, low on resources
**What happens**:
- AI at high temperament, launching massive assault
- Player uses last defenses, clever positioning
- Successfully repels attack, AI temperament drops
- Player can rebuild from victory

**Player satisfaction**: "Against all odds, I held them off"

**Why it works**: High-stakes moment with meaningful comeback potential.

### The "Tactical Mastery" Moment

**Scenario**: Player has studied AI behavior patterns
**What happens**:
- Player knows destroying nests will spike aggression
- Plans counter-attacks and defenses in advance
- Uses terrain, turrets, and unit positioning optimally
- Destroys nest, survives the retaliation, gains ground

**Player satisfaction**: "I predicted what would happen and was ready"

**Why it works**: Player feels skilled for understanding and preparing for system behavior.

### The "Escalation Control" Win

**Scenario**: Player carefully manages AI aggression
**What happens**:
- Player destroys units gradually, not all at once
- Keeps AI temperament in moderate range
- Attacks during low-temperament periods
- Maintains steady progress without triggering full war

**Player satisfaction**: "I'm controlling the pace of the game"

**Why it works**: Player feels strategic mastery over system mechanics.

### The "Teamwork" Experience

**Scenario**: Multiplayer game, coordinated defense
**What happens**:
- Multiple players defend against high-temperament assault
- Players specialize (some on turrets, some on mobile defense)
- Successfully repel coordinated AI attack
- Celebrate victory together

**Player satisfaction**: "We did it together"

**Why it works**: Social element amplifies satisfaction of overcoming challenge.

---

## 5. How Does Temperament Affect Player Strategy?

### Defensive Strategies

#### The "Fortress" Approach
- **Philosophy**: Build impenetrable defenses, wait out AI aggression
- **Temperament interaction**: High temperament = sustained attacks on fortress
- **Pros**: Can survive almost anything with enough preparation
- **Cons**: Resource intensive, no progress if purely defensive
- **Best for**: Players who prefer methodical, defensive play

#### The "Active Defense" Approach
- **Philosophy**: Maintain mobile forces to intercept attacks
- **Temperament interaction**: Kill units before they reach base, keep temperament moderate
- **Pros**: More resource-efficient, maintains flexibility
- **Cons**: Requires skill, risky if attack is too large
- **Best for**: Players who enjoy tactical combat

#### The "Preemptive Strike" Approach
- **Philosophy**: Attack AI bases before they can attack you
- **Temperament interaction**: Destroy nests carefully, prepare for retaliation
- **Pros**: Reduces long-term threat, gains ground
- **Cons**: Triggers temperament spikes, requires strong army
- **Best for**: Aggressive players who want to control the game pace

### Offensive Strategies

#### The "Slow Burn" Campaign
- **Philosophy**: Gradually eliminate AI bases, one at a time
- **Temperament interaction**: Keep aggression moderate, avoid triggering multiple bases
- **Pros**: Sustainable, manageable retaliation
- **Cons**: Slow, requires patience
- **Best for**: Strategic players who plan long-term

#### The "Lightning War" Campaign
- **Philosophy**: Massive offensive, destroy everything quickly
- **Temperament interaction**: Accept high temperament, overwhelm with superior force
- **Pros**: Fast resolution, decisive
- **Cons**: Requires massive resources, high risk
- **Best for**: Aggressive players with strong economies

#### The "Guerrilla" Campaign
- **Philosophy**: Hit-and-run attacks, avoid direct confrontation
- **Temperament interaction**: Keep AI off-balance, prevent stable aggression levels
- **Pros**: Resource-efficient, hard for AI to counter
- **Cons**: Slow progress, requires micro-management
- **Best for**: Players who enjoy tactical depth

### Resource Management Strategies

#### Temperament-Aware Economy
- **Key insight**: AI aggression affects resource needs
- **Strategy**: Stockpile resources before high-temperament periods
- **Tactics**: 
  - Build defensive structures during low-temperament periods
  - Expand production during calm periods
  - Hoard ammunition during aggressive phases

#### Risk/Reward Balancing
- **Key insight**: Destroying nests reduces long-term threat but increases short-term danger
- **Strategy**: Time major offensives when defenses are strongest
- **Tactics**:
  - Destroy nests when fully prepared for retaliation
  - Use artillery sparingly to avoid temperament spikes
  - Focus on unit destruction over building destruction to moderate temperament impact

### Multiplayer Coordination Strategies

#### Role Specialization
- **Tank players**: Hold defensive positions during high-temperament assaults
- **DPS players**: Launch counter-attacks during low-temperament periods
- **Support players**: Manage resources, repair defenses, resupply

#### Temperament Monitoring
- **Shared awareness**: Players track AI aggression collectively
- **Coordinated strikes**: Attack when multiple players are ready
- **Relay defense**: Rotate defensive duties during sustained assaults

---

## 6. The Psychological Experience

### The "Tension Curve"

Rampant's temperament system creates a specific psychological experience:

1. **Early Game**: Mild tension, "something is watching me"
2. **Mid Game**: Rising tension, "I need to prepare"
3. **Late Game**: High tension, "full survival mode"
4. **Post-Victory**: Relief, "I survived the planet"

### The "Learned Helplessness" Risk

**Problem**: When temperament spikes feel inevitable and uncontrollable, players may feel:
- "No matter what I do, it gets worse"
- "I can't win, only delay defeat"
- "The game is punishing me for playing well"

**Mitigation**: Players who understand the system can:
- Predict temperament changes
- Prepare for retaliation
- Use system mechanics strategically

### The "Flow State" Balance

**Ideal**: Temperament creates challenge that matches player skill
- Too easy: Low temperament, boring
- Too hard: High temperament, frustrating
- Just right: Moderate temperament, engaging

**Problem**: The system doesn't adapt to player skill, only to game state
- Skilled players may find early game too easy
- New players may find late game impossible
- No built-in difficulty adjustment based on performance

---

## 7. Comparison to Vanilla Factorio AI

### Vanilla AI Experience
- **Pattern**: Pollution-based attacks, predictable expansion
- **Player feeling**: "I caused this attack by polluting"
- **Strategy**: Manage pollution, build defenses at chokepoints
- **Frustration**: "I can't expand without triggering attacks"

### Rampant AI Experience
- **Pattern**: Temperament-based behavior, varied tactics
- **Player feeling**: "The planet has moods, I need to read them"
- **Strategy**: Manage AI aggression, use varied tactics
- **Frustration**: "I don't know why it's attacking now"

### Key Differences

| Aspect | Vanilla | Rampant |
|--------|---------|---------|
| **Trigger** | Pollution | Temperament (multiple factors) |
| **Predictability** | High (pollution = attack) | Medium (hidden calculations) |
| **Variety** | Low (always attack) | High (varied states) |
| **Player Control** | Low (pollution inevitable) | Medium (can influence temperament) |
| **Tactical Depth** | Medium (defend chokepoints) | High (adapt to AI state) |
| **Frustration Source** | "Can't expand" | "Don't understand why" |

---

## 8. Recommendations for Improved Player Experience

### Immediate Wins (Low Effort, High Impact)

1. **Visible Temperament Indicator**
   - Show AI aggression level in UI
   - Color-coded: Green (calm) → Red (furious)
   - **Benefit**: Players can anticipate attacks

2. **State Description Tooltip**
   - Display current AI state (RAIDING, SIEGE, etc.)
   - Explain what each state means
   - **Benefit**: Reduces confusion about behavior

3. **Temperament Feedback on Actions**
   - "Destroying this nest will increase AI aggression"
   - "Your attack has calmed nearby bases"
   - **Benefit**: Players understand cause-and-effect

### Medium-Term Improvements

4. **Temperament Decay System**
   - AI calms down over time without player provocation
   - Prevents permanent high-aggression states
   - **Benefit**: Creates breathing room, more natural rhythm

5. **Base Personality Variation**
   - Different bases have different temperament profiles
   - Some are naturally aggressive, others cautious
   - **Benefit**: More variety, less predictable patterns

6. **Player Manipulation Mechanics**
   - Decoy systems to agitate specific bases
   - Stealth operations to calm bases
   - **Benefit**: Active strategic layer beyond defense

### Long-Term Vision

7. **Multi-Dimensional Temperament**
   - Separate traits: aggression, caution, desperation
   - More nuanced AI behavior
   - **Benefit**: Richer, more realistic AI personalities

8. **Learning AI**
   - AI adapts to player tactics over time
   - Counters strategies that work too well
   - **Benefit**: Players must vary tactics, prevents stale meta

---

## 9. Conclusion: The Temperament Experience

### The Core Tension

Rampant's temperament system creates a fundamental tension:

**"The planet is alive and reactive"** vs **"I don't understand why it's reacting"**

This tension is both the system's greatest strength and weakness:
- **Strength**: Creates dynamic, unpredictable gameplay
- **Weakness**: Can feel arbitrary and unfair

### The Player Archetypes

Different players experience temperament differently:

1. **The Strategist**: Loves analyzing patterns, predicting attacks
   - Experience: "I can outthink the AI"
   - Satisfaction: High (if they understand the system)

2. **The Tactician**: Enjoys combat, adapting to situations
   - Experience: "Every battle is different"
   - Satisfaction: Medium-High (varied challenges)

3. **The Builder**: Prefers construction over combat
   - Experience: "I can't build in peace"
   - Satisfaction: Low (constant interruption)

4. **The Explorer**: Wants to explore the map
   - Experience: "I can't expand without war"
   - Satisfaction: Medium (challenge vs. freedom)

### The Ultimate Experience

At its best, Rampant's temperament system creates:
- **Dynamic tension** that keeps players engaged
- **Varied challenges** that prevent monotony
- **Strategic depth** for players who want it
- **Memorable moments** of triumph and desperation

At its worst, it creates:
- **Frustration** from hidden mechanics
- **Unfairness** from sudden escalations
- **Helplessness** from compounding failures
- **Confusion** from unpredictable behavior

### The Verdict

The temperament system is **ambitious and mostly successful**, but needs better player communication. The core mechanics create engaging gameplay, but the hidden nature of temperament calculations creates friction. Making the system more transparent would preserve the dynamic gameplay while reducing frustration.

**Key insight**: Players can handle difficulty, but they need to understand *why* it's happening.

---

*Document created: 2026-02-28*
*Analysis based on codebase examination, documentation, and changelog history*
*Intended for mod developers seeking to improve player experience*