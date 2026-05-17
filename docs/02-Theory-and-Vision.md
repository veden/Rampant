# Theory and Vision

## Document Purpose

This document describes the **what** and **why** of Rampant's Enemy Logistics System. It articulates the vision, gameplay philosophy, and conceptual framework that guide implementation. This is the "ideal world" description before technical constraints are applied.

---

## 1. Core Vision

### 1.1 The Fundamental Shift

**Current State**: Rampant is a **defensive friction system** - enemies attack based on hidden mechanics, players react by building stronger defenses, and the game becomes an arms race of "bigger walls vs. bigger attacks."

**Desired State**: Rampant becomes a **dynamic strategic campaign** - enemies have visible logistics networks with physical supply lines, players actively disrupt supply chains through targeted attacks, offensive action provides tangible rewards, and victory comes from strategic mastery rather than factory size.

### 1.2 The Central Metaphor

> **"War is logistics. Logistics is Factorio. The player should win by outmaneuvering the enemy's supply chain, not just out-building them."**

This metaphor drives every design decision:
- **Enemy has supply chains** (resource nodes → Command Centers → nests)
- **Supply chains are VISIBLE** (physical connectors, ground markings)
- **Supply chains can be disrupted** (destroy connectors, Command Centers, territory nodes)
- **Disruption provides rewards** (technology unlocks, resource capture, territory control)

### 1.3 The Player Experience

#### **Early Game: Discovery**
- "The enemy is testing me - I need to learn their patterns"
- "Their buildings are connected by these physical structures..."
- "If I destroy that connector, their attacks got smaller!"
- **Feeling**: Curiosity, experimentation, first breakthroughs

#### **Mid Game: Understanding**
- "I understand their network - I can predict their moves"
- "They're building a Command Center - should I attack now or wait?"
- "The Queen was tough, but she dropped this rare technology module!"
- **Feeling**: Strategic depth, meaningful choices, progression

#### **Late Game: Mastery**
- "I'm dismantling their war machine systematically"
- "I know exactly which Command Centers to cut to starve their network"
- "Their supply is starved, their territory is fragmented"
- **Feeling**: Agency, dominance, strategic mastery

---

## 2. Design Philosophy

### 2.1 Visibility Over Opacity

**Principle**: Enemy behavior should be **observable and learnable**, not hidden or random.

**Why**:
- Hidden mechanics feel **unfair** ("Why are they attacking NOW?")
- Visible patterns feel **strategic** ("I see their supply route, I can cut it")
- Factorio players value **transparency** (learn through observation)

**Implementation**:
- **Physical connectors** show supply routes (not just invisible pheromones)
- **Ground markings** show connection paths (visual feedback)
- **Hover tooltips** show supply levels (on-demand details)
- **Entity behavior** indicates state (aggressive patrols vs. defensive)
- **Audio cues** telegraph events (construction warnings)

**Critical**: **Pheromone cloud alone is insufficient** - players need physical entities to target and interact with.

### 2.2 Agency Over Reactivity

**Principle**: Players should **shape the conflict**, not just react to it.

**Why**:
- Pure defense feels **passive** ("I'm just surviving")
- Offensive options feel **empowering** ("I'm weakening them")
- Agency creates **engagement** (players make choices, see consequences)

**Implementation**:
- Destroy **physical connectors** → supply lines break, attacks weaken
- Destroy **Command Centers** → territory collapses, network fragments
- Capture **resource nodes** → enemy loses supply, player gains resources
- Technology unlocks → new capabilities through combat

### 2.3 Progression Through Engagement

**Principle**: Players should be **rewarded for fighting**, not just surviving.

**Why**:
- Defense-only gameplay feels **stagnant** ("I'm just building walls")
- Combat rewards feel **meaningful** ("I got something from that fight")
- Progression creates **motivation** (want to engage, not avoid)

**Implementation**:
- **Technology modules** from destroying Command Centers (reverse-engineer enemy tech)
- **Resource capture** (mine enemy oil/uranium nodes after destruction)
- **Boss drops** (rare items from superstructures)
- **Territory control** (forward bases with defensive capabilities)

### 2.4 Constraints Create Strategy

**Principle**: Limitations force **meaningful choices**, not infinite scaling.

**Why**:
- Unlimited resources → "build more turrets" (no strategic depth)
- Limited supply → "cut their supply lines" (strategic thinking)
- Enemy adapts → "change my approach" (tactical flexibility)

**Implementation**:
- **Supply-gated spawning** (enemy can't spawn infinite units)
- **Territory expansion** (Command Centers must grow to claim resources)
- **Adaptation timing** (enemy shifts tactics after 2-3 attacks)
- **Resource dependencies** (certain unit types require specific resources)

### 2.5 Organic Growth With Structure

**Principle**: Enemy should grow **organically** but with **visible structure**.

**Why**:
- Random placement feels **chaotic** (no logic to enemy behavior)
- Rigid placement feels **artificial** (predictable patterns)
- Organic growth with structure feels **intelligent** (adaptive but learnable)

**Implementation**:
- **Command Centers** placed sparsely (500+ tiles apart, late-game)
- **Territory pheromone** flows organically from Command Centers
- **Physical connectors** appear where territory overlaps
- **Hives spawn** within territory radius (organized clusters)
- **Gaps remain** between clusters (player can exploit)

---

## 3. Core Systems Overview

### 3.1 Command Center System

**What It Is**: Strategic anchors that emit territory and manage supply.

**Key Properties**:
- **Resource Command Centers**: Have resources attached to territory, generate supply
- **Regular Command Centers**: Consume supply, no resources
- **Territory Emission**: TERRITORY_PHEROMONE (strength 100 at center, diffuses outward)
- **Connection Logic**: Connectors spawn where territory overlap > 40 (emergent, no hard-coded range)
- **Supply Flow**: TERRITORY_SUPPLY flows through connectors (gradual buildup 0%→100% over 5 min)
- **Health Scaling**: CC health scales with player tech tier (1.0x → 2.0x max, diminishing returns)

**Player Interaction**:
- **Destroy Command Center** → territory decays (5%/tick), supply cuts, network fragments
- **Block territory expansion** → prevent resource claims, slow network growth
- **Cut supply lines** → starve downstream Command Centers, units regress to lower tiers
- **Attack during construction** → Easy target (0-100% health buildup), moderate reward

### 3.2 Territory System

**What It Is**: Invisible pheromone field that defines enemy control areas.

**Key Properties**:
- **TERRITORY_PHEROMONE**: Strength 0-100, diffuses from Command Centers
- **TERRITORY_FACTION**: Faction identity (acid, laser, physical, etc.), spreads with territory
- **TERRITORY_SUPPLY**: Supply level 0.0-1.0, flows through connectors
- **Processing**: Integrated into existing 40-chunks/tick diffusion (no separate loops)
- **Decay**: 5%/tick for territory/supply, 10%/tick for faction

**Player Interaction**:
- **Build in territory** → Reduces TERRITORY_PHEROMONE (2% per building)
- **Reclaim territory** → PLAYER_PHEROMONE > TERRITORY_PHEROMONE → gradual flip (~10 min)
- **Scan territory** → Optional tool reveals pheromone levels (color overlay)
- **Block expansion** → Prevent territory from reaching new resources

### 3.3 Independent Triggers (Responsive Pacing)

**What It Is**: Multiple activity metrics that modulate enemy scaling based on HOW player plays.

**Key Triggers**:
- **Building Count** → Network growth rate (linear: base × (1 + buildings/200), cap 2.0x)
- **Technology Tier** → Building/unit health (diminishing: 1.0x → 1.4x → 1.7x → 1.9x → 2.0x)
- **Pollution Output** → Spawn rates (exponential: base × (pollution/baseline)^1.3, cap 3.0x)
- **Combat Events** → Unit health (linear: base × (1 + events/100), cap 2.0x)
- **Offense/Defense Ratio** → Escalation timer (inverse: base / (offense^1.5), range 1-6 hours)
- **Tactic Consistency** → Adaptation speed (linear: base × consistency, range 2-10 attacks)

**Integration with Evolution**:
- Evolution remains master scaling factor (time-based progression)
- Triggers modulate evolution: `final_scaling = evolution × trigger_modifier`
- **Rationale**: Enemy scales to player strategy, not just speed

**Player Impact**:
- Fast builder → Network expands faster, but units not necessarily stronger
- Aggressive fighter → Units get tougher, but also more disruption opportunities
- Defensive player → Faster escalation (superstructures), but slower adaptation
- **Result**: Player strategy shapes HOW enemy scales, not just HOW MUCH

### 3.4 Supply Network

**What It Is**: Hierarchical supply flow from resources to nests.

**Key Properties**:
- **Resource CCs** generate supply (have attached resources)
- **Regular CCs** consume supply (no resources)
- **Supply Lines** = Territory pheromone flow between CCs
- **Supply Gating** = Tier access depends on supply level

**Player Interaction**:
- **Destroy Resource CC** → network weakens globally
- **Cut supply lines** → downstream CCs starve
- **Block expansion** → prevent new resource claims

### 3.4 Resource Attachment

**What It Is**: Resources attach to territory, not Command Center proximity.

**Key Properties**:
- **Resource within territory** → Command Center gains resource bonus
- **Multiple CCs** can share same resource (via territory overlap)
- **Resource bonuses**: Oil (acid), Iron (physical), Coal (expansion), Uranium (elite)
- **No resource** → base faction only, slower growth

**Player Interaction**:
- **Destroy resource node** → enemy loses bonus, player can mine enhanced patch
- **Block territory expansion** → prevent resource claims
- **Capture resource** → player gains access to enhanced patch

### 3.5 Growth Pacing

**What It Is**: Enemy growth responds to player activity.

**Key Properties**:
- **Growth Rate** = base_rate × (1 + player_activity_factor)
- **Player Activity** = pollution + buildings + combat
- **Fast player growth** → fast enemy expansion
- **Slow player growth** → slow enemy expansion

**Player Interaction**:
- **Build aggressively** → enemy responds quickly (pressure)
- **Build slowly** → enemy expands slowly (breathing room)
- **Attack aggressively** → enemy reinforces (counter-pressure)

---

## 4. Gameplay Loop

### 4.1 Discovery Phase (Early Game)

**Player Actions**:
- Explore map, encounter enemy clusters
- Discover **physical connectors** between buildings
- Experiment: "What happens if I destroy this connector?"
- Learn: "Cutting connections weakens their attacks"

**Enemy Behavior**:
- Sparse Command Centers (500+ tiles apart)
- Organic territory expansion
- Natural difficulty curve (sparse near spawn, dense far away)

**Player Feeling**: Curiosity, experimentation, first breakthroughs

### 4.2 Understanding Phase (Mid Game)

**Player Actions**:
- Map enemy network (identify Command Centers, supply lines)
- Plan attacks (target Resource CCs, cut supply lines)
- Choose timing (attack during construction or wait for boss?)
- Collect rewards (technology modules, resource nodes)

**Enemy Behavior**:
- Command Centers expand territory
- Supply network forms (Resource CCs → Regular CCs)
- Adaptation begins (shift faction after 2-3 attacks)

**Player Feeling**: Strategic depth, meaningful choices, progression

### 4.3 Mastery Phase (Late Game)

**Player Actions**:
- Systematically dismantle network (cut supply, destroy CCs)
- Exploit gaps between clusters
- Counter enemy adaptation (change tactics)
- Capture territory (forward bases)

**Enemy Behavior**:
- Hierarchical supply network (Resource CCs critical)
- Adaptive tactics (faction spreading through network)
- Superstructures (Queens, Titans) as escalation

**Player Feeling**: Agency, dominance, strategic mastery

---

## 5. Critical Design Decisions

### 5.1 Physical vs. Abstract Representation

**Decision**: **Both physical entities AND pheromone cloud are required.**

**Rationale**:
- Pheromone cloud = backend logic (environment memory, efficient)
- Physical entities = frontend interaction (player targets, visible)
- **Neither alone is sufficient** - players need physical targets to interact with

**Examples**:
- ❌ **Pheromone-only**: Invisible supply lines (players can't see or cut them)
- ❌ **Physical-only**: Random connectors (no strategic logic behind placement)
- ✅ **Hybrid**: Physical connectors placed by pheromone logic (visible + strategic)

### 5.2 Resource Attachment Model

**Decision**: **Resources attach to territory, not Command Center proximity.**

**Rationale**:
- Command Center must **expand territory** to claim resource
- Resource bonus applies to **all entities within territory**
- Territory can **connect multiple Command Centers** to same resource
- Creates **strategic expansion** (CCs compete for resources)

**Examples**:
- ❌ **Proximity-based**: CC near resource gets bonus (no expansion needed)
- ✅ **Territory-based**: CC must expand territory to claim resource (strategic)

### 5.3 Supply Network Topology

**Decision**: **Hierarchical network (Resource CCs → Regular CCs).**

**Rationale**:
- Creates **meaningful supply lines** (cut = starve downstream)
- **Resource CCs are strategic targets** (high value)
- Network has **hierarchy** (some CCs more important than others)
- Player can **disrupt supply** (destroy CCs, block expansion)

**Examples**:
- ❌ **Flat network**: All CCs equal (no strategic priority)
- ✅ **Hierarchical**: Resource CCs → Regular CCs (clear priorities)

### 5.4 Growth Responsiveness

**Decision**: **Enemy growth responds to player activity (hybrid model).**

**Rationale**:
- **Pollution-based**: Existing RampantEvolution system (familiar)
- **Building-based**: Factory size matters (logistics theme)
- **Combat-based**: Aggression met with counter-pressure (fair)
- **Hybrid**: Comprehensive view of player progress (balanced)

**Examples**:
- ❌ **Static growth**: Enemy expands at fixed rate (predictable, boring)
- ✅ **Responsive growth**: Enemy adapts to player actions (dynamic, fair)

---

## 6. Open Questions for Future Research

### 6.1 Supply Line Visualization

**Question**: How should supply lines be visualized?

- **Option A**: Physical connectors only (simple, may look cluttered)
- **Option B**: Physical connectors + ground markings (clear paths, more entities)
- **Option C**: Ground markings only (violates Physical Visibility Principle)
- **Option D**: Hybrid (physical connectors at chokepoints, ground markings elsewhere)

**Recommendation**: **Option B or D** - players need visible, interruptible paths

### 6.2 Territory Visibility

**Question**: How visible should territory be to players?

- **Option A**: Completely invisible (infer from enemy behavior)
- **Option B**: Ground markings (faint pheromone visualization)
- **Option C**: Player can scan/reveal (limited visibility)
- **Option D**: Optional radar overlay (strategic view)

**Recommendation**: **Option C (scan to reveal)** - rewards exploration, not hand-holding

### 6.3 Command Center Placement

**Question**: Where should Command Centers appear initially?

- **Option A**: Random locations (surprise threats)
- **Option B**: Near player bases (direct pressure)
- **Option C**: At strategic locations (resource nodes, chokepoints)
- **Option D**: Beyond 500+ tiles from spawn (late-game targets)

**Recommendation**: **Option D** - Command Centers as late-game strategic targets

---

## 7. Document History

- **v1.0** (Initial): Basic vision and philosophy
- **v1.1** (Updated): Added Command Center system, territory system, supply network, resource attachment model, growth pacing, refined visibility principles