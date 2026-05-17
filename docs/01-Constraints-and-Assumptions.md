# Constraints and Assumptions

## Document Purpose

This document establishes the technical constraints, design assumptions, and foundational reasoning that guide the development of Rampant's Enemy Logistics System. It serves as the "why" behind design decisions and the boundaries within which we operate.

---

## 1. Technical Constraints

### 1.1 Factorio Engine Limitations

#### **Entity Property Modification**
- **Most entity properties are READ-ONLY at runtime**: `prototype`, `name`, `type`, `max_health`, `bounding_box` cannot be changed
- **Writable properties are limited**: Only `speed` (units), `health`, `energy`, `direction`, `color` can be modified directly
- **No custom bonus types**: Factorio only supports predefined effect types (speed, consumption, pollution, productivity) via beacons
- **Implication**: We must use **prototype variations** (multiple entity definitions) rather than runtime property modification for meaningful progression

#### **Unit Spawning System**
- **Native spawners are hardcoded**: Pollution absorption, spawn cooldowns, and group formation are engine-level behavior
- **We cannot modify spawn rates directly**: Must intercept via `on_unit_group_created` event
- **Group gathering is fixed**: 1-10 minutes gathering time, max 200 units per auto-created group
- **Implication**: We work WITH native spawning (intercept and filter) rather than replacing it

#### **Pathfinding and Movement**
- **Individual pathfinding is expensive**: ~10-20 pathfinding requests per tick before FPS impact
- **Recommendation**: Use pheromone fields (Rampant's existing pattern) or shared routes for large unit counts
- **Implication**: Avoid per-unit pathfinding for supply carriers; use batch processing or pheromone-based movement

#### **Rendering Performance**
- **Rendering API has no TTL**: Render objects require manual cleanup or per-tick updates
- **Decoratives have no native expiration**: Must implement custom decay system
- **Recommendation**: Use `surface.create_decoratives()` with sparse tick queue for cleanup (~0.001ms/tick overhead)
- **Implication**: Visual supply routes use ground decoratives + physical connector entities, not rendered lines

### 1.2 Prototype Budget

#### **Current Rampant Scale**
- **10 tiers × 15 factions × 2-3 unit types × 1-7 variations** = 900-4,200+ unit/building prototypes
- **Additional**: ~200-500 projectiles, particles, damage types
- **Total**: ~1,100-4,700 prototypes depending on configuration

#### **New System Budget**
- **Target**: 750-900 new prototypes (manageable within existing scale)
- **Hard limit**: ~1,100 prototypes (approaching performance concern zone)
- **Allocation**:
  - 400 unit archetypes (9 roles × 5 tiers × 6 variations)
  - 150 resource doctrine variants (shared pool, different ratios)
  - **85 network infrastructure** (Command Centers, connectors, resource nodes, territory markers)
  - 75 superstructures (4 boss types × 15 variants)
  - 80 supporting entities (projectiles, particles, damage types)
  - 20 visualization (ground decoratives, territory markers)
  - 15 rewards/loot
  - 15 territory structures

#### **Implication**
- **Prototype count is NOT a primary constraint** (Factorio handles 5,000+ fine)
- **Design constraint**: Must balance variety against maintainability
- **Decision**: Prefer **fewer tiers (5)** with **more roles (9)** over 10 tiers × few roles
- **Critical**: **Physical connectors are required** for player interaction - cannot be replaced by invisible pheromone alone

### 1.3 Performance Considerations

#### **On_Tick Handlers**
- **Every on_tick handler runs 60 times/second**: Even "empty" handlers have overhead
- **Recommendation**: Use event-driven logic where possible; batch processing for periodic tasks
- **Acceptable**: Sparse tick queue for decorative cleanup (early return 99% of time)
- **Unacceptable**: Per-tick entity property modification for 1000+ units

#### **Spatial Queries**
- **`find_entities_filtered` is expensive for large radii**: Use chunk-based filtering first
- **`count_entities_filtered` is faster**: Use for limit checks
- **Recommendation**: Cache per-chunk data (Rampant's existing pattern)

#### **Entity Count**
- **Safe**: < 1,000 active entities (negligible impact)
- **Acceptable**: 1,000-5,000 entities (batch updates, < 60 tick frequency)
- **Warning**: 5,000-10,000 entities (chunk-based processing required)
- **Danger**: 10,000+ entities (aggressive optimization required)

---

## 2. Design Assumptions

### 2.1 Player Behavior Assumptions

#### **Current Meta Problems**
- **Assumption**: Players default to "build bigger walls" (static defense)
- **Assumption**: Players lack offensive incentives (defending is safer than attacking)
- **Assumption**: Players don't understand enemy behavior (hidden mechanics feel unfair)

#### **Design Response**
- **Escalation mechanics**: Static defense triggers superstructures (punishes pure defense)
- **Offensive rewards**: Destroying infrastructure provides technology, resources, territory
- **Visible patterns**: **Physical connectors**, ground markings, hover tooltips, audio cues (transparent mechanics)
- **Responsive pacing**: Enemy scales to HOW player plays (building, research, combat), not just how fast

### 2.2 Enemy AI Assumptions

#### **Current Rampant AI**
- **Assumption**: Current temperament system feels "punishing" (escalates without player agency)
- **NEW**: Temperament can be enhanced with independent triggers (supply, territory) without replacing it
- **NEW**: Per-base temperament + global evolution creates rich, nuanced behavior

### 2.3 System Integration Assumptions

#### **Pheromone System** (UPDATED)
- **Assumption**: Pheromone cloud is environment's memory (eventually consistent, uniform cost)
- **Assumption**: Pheromones should encode **territory, supply, and faction** information
- **Assumption**: Pheromones should **augment** physical entities, not replace them
- **Critical**: **Pheromones alone are insufficient** - players need physical targets to interact with
- **NEW**: Existing 40-chunks/tick processing can handle 8 pheromone types (not just 5)
- **NEW**: New pheromone types plug into existing diffusion algorithm (no separate loops)

#### **Squad Movement** (NEW)
- **Assumption**: Existing 3-step lookahead pheromone scoring is efficient and effective
- **Assumption**: New scoring functions can plug into existing algorithm without changes
- **Assumption**: Squad movement should remain gradient-following, not full pathfinding
- **NEW**: SUPPLY_CARRIER squads follow TERRITORY_SUPPLY gradient, existing squads unchanged

#### **Temperament System** (NEW)
- **Assumption**: Per-base temperament provides rich, nuanced behavior
- **Assumption**: Independent triggers can modulate temperament without replacing it
- **Assumption**: Supply/territory affecting temperament creates meaningful feedback loops
- **NEW**: Low supply → temperament -1000 (desperate behavior)
- **NEW**: Network fragmentation → temperament -500 (vulnerable behavior)

#### **Evolution Scaling** (NEW)
- **Assumption**: Evolution remains master scaling factor (time-based progression)
- **Assumption**: Independent triggers modulate evolution (activity-based fine-tuning)
- **Assumption**: Power laws (evolution^1.4, ^1.66, ^2.5) create meaningful late-game pressure
- **NEW**: Triggers fine-tune: `final_scaling = evolution × trigger_modifier`
- **NEW**: Caps prevent runaway: health 1.0x-2.0x, spawn rates 0.5x-3.0x
- **Assumption**: Current AI lacks structure (random clusters, no visible logic)

#### **Desired AI Behavior**
- **Assumption**: Enemy should have **visible logistics network** (Command Centers, connectors, supply lines)
- **Assumption**: Enemy should **respond to player actions** (growth rate, adaptation)
- **Assumption**: Enemy should have **hierarchical structure** (Resource CCs → Regular CCs → Nests)

#### **Autoplace System**
- **Assumption**: Existing autoplace creates natural difficulty curve (sparse near spawn, dense far away)
- **Assumption**: We should **augment** autoplace, not replace it
- **Assumption**: Command Centers should appear **beyond 500+ tiles** (late-game strategic targets)
- **Assumption**: Spawner density should be **reduced by 50%** to create gaps for supply lines

#### **Growth Pacing**
- **Assumption**: Enemy growth should respond to **player activity** (pollution + buildings + combat)
- **Assumption**: Growth should be **organic** (Command Centers expand territory, hives spawn within)
- **Assumption**: Growth should have **meaningful gaps** (not blanket coverage)

---

## 3. Critical Design Decisions (Finalized)

### 3.1 Evolution vs. Independent Triggers
**Decision**: **Modulate, don't replace**
- Evolution remains master scaling factor (time-based progression)
- Triggers fine-tune based on player activity (building count, tech tier, pollution, combat)
- **Formula**: `final_scaling = evolution × trigger_modifier`
- **Rationale**: Preserves existing balance, gradual transition, easier to debug

### 3.2 Temperament Integration
**Decision**: **Direct modifier**
- Low supply → temperament -1000 (desperate behavior)
- Network fragmented → temperament -500 (vulnerable behavior)
- **Rationale**: Simple integration with existing state machine, no parallel systems

### 3.3 Squad Movement Enhancement
**Decision**: **New scoring functions, keep 3-step lookahead**
- Add `scoreSupplyLocation()`, `scoreResourceLocation()` for new squad types
- Same 3-step algorithm, new pheromone types in scores
- **Rationale**: Minimal changes, leverages existing infrastructure, cost-effective

### 3.4 Connector Health Buildup
**Decision**: **Gradual (0% → 100% over 5 minutes = 300 ticks)**
- Player can interrupt during construction
- Visual feedback (construction animation)
- No supply flow until complete
- **Rationale**: Tactical depth, visible progression, satisfying gameplay

### 3.5 Health Scaling Curve
**Decision**: **Diminishing returns with 2.0x cap**
- Tier 0: 100%, Tier 1: 140%, Tier 2: 170%, Tier 3: 190%, Tier 4: 200%
- **Rationale**: Prevents late-game health sponges, balanced challenge, fair progression

### 3.6 Pheromone Processing Integration
**Decision**: **Integrated into existing diffusion loop**
- Add 3 new pheromone types (TERRITORY, TERRITORY_FACTION, TERRITORY_SUPPLY) to existing 40-chunks/tick processing
- Same decay pattern as existing types (0.95, 0.90, 0.95 retention)
- **Rationale**: No performance impact, leverages existing infrastructure

---

## 4. Critical Design Principles

### 4.1 Physical Visibility Principle

**Core Rule**: **Players must have physical entities to target and interact with.**

- Pheromone cloud is **backend logic** (environment memory)
- Physical entities are **frontend interaction** (player targets)
- **Both are required** - neither alone is sufficient

**Examples**:
- ❌ **Pheromone-only**: Invisible supply lines (players can't see or cut them)
- ❌ **Physical-only**: Random connectors (no strategic logic behind placement)
- ✅ **Hybrid**: Physical connectors placed by pheromone logic (visible + strategic)

### 3.2 Supply Line Visibility Principle

**Core Rule**: **Supply lines must be visually representable and interruptible.**

- **Minimum viable**: Physical connector entities (destroyable)
- **Enhanced**: Physical connectors + ground markings (clear paths)
- **Optional**: Radar overlay (strategic view, not required)

**Implication**: We **cannot** rely solely on invisible pheromone flow - players need physical targets.

### 3.3 Resource Attachment Principle

**Core Rule**: **Resources attach to territory, not Command Center proximity.**

- Command Center must **expand territory** to claim resource
- Resource bonus applies to **all entities within territory**
- Territory can **connect multiple Command Centers** to same resource

**Implication**: Resource nodes are **territory objects**, not Command Center objects.

### 3.4 Growth Responsiveness Principle

**Core Rule**: **Enemy growth should respond to player progress.**

- Growth rate = base_rate × (1 + player_activity_factor)
- Player activity = pollution + buildings + combat
- Faster player growth → faster enemy expansion
- Player can **slow enemy growth** by staying passive

**Implication**: Enemy is **dynamic opponent**, not static difficulty curve.

---

## 4. Open Questions & Research Needs

### 4.1 Visualization Methods

**Question**: How should supply lines be visualized?

- **Option A**: Physical connectors only (simple, but may look cluttered)
- **Option B**: Physical connectors + ground markings (clear paths, more entities)
- **Option C**: Ground markings only (no physical connectors - violates Physical Visibility Principle)
- **Option D**: Hybrid (physical connectors at chokepoints, ground markings elsewhere)

**Recommendation**: **Option B or D** - players need visible, interruptible paths

### 4.2 Command Center Territory Radius

**Question**: What should Command Center territory radius be?

- **Option A**: 100 tiles (tight clusters, many gaps)
- **Option B**: 150 tiles (medium clusters, medium gaps)
- **Option C**: 200 tiles (large clusters, few gaps)

**Recommendation**: **Option B (150 tiles)** - balanced clusters and gaps

### 4.3 Connection Range

**Question**: How far apart can Command Centers be and still connect?

- **Option A**: 150 tiles (tight network, many CCs needed)
- **Option B**: 300 tiles (medium network, balanced)
- **Option C**: 500 tiles (loose network, fewer CCs needed)

**Recommendation**: **Option B (300 tiles)** - creates meaningful supply lines

### 4.4 Resourceless CC Survival

**Question**: What happens to Command Centers without resources?

- **Option A**: Die immediately (harsh)
- **Option B**: Survive on base supply, can't grow (moderate)
- **Option C**: Must connect to Resource CC within time limit (time-gated)
- **Option D**: Can expand territory to reach resources (player can block)

**Recommendation**: **Option D** - creates dynamic gameplay, player can interfere

---

## 5. Document History

- **v1.0** (Initial): Basic constraints and assumptions
- **v1.1** (Updated): Added Physical Visibility Principle, Supply Line Visibility Principle, updated prototype budget, refined growth pacing assumptions