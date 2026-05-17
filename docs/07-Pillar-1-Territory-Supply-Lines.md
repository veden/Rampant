# Pillar 1: Territory Supply Lines

## Document Purpose

This document describes the complete design for Pillar 1: Territory Supply Lines. It captures the vision, mechanics, constraints, and design rationale for the territory and supply line system that forms the foundation of Rampant's Enemy Logistics System.

---

## 1. Vision

Enemy supply routes are **procedurally generated** based on Command Center territory overlap. **Physical connector entities** span between Command Centers and Territory Beacons where territories overlap, creating visible, defendable, interruptible supply lines. Territory pheromone flows through these connections.

### Core Design Goal

Create enough distance between elements (CC → Beacons → Connectors → Resource Nodes) that players can:
1. **Recognize** what's happening (visual clarity)
2. **Interject** (build defenses, cut supply lines)
3. **Operate safely** (not constantly bombarded by adjacent bases)

---

## 2. Design Philosophy

### 2.1 Physical Visibility Principle

**Core Rule**: Players must have physical entities to target and interact with.

- **Pheromone cloud** = backend logic (invisible, drives behavior)
- **Physical entities** = frontend interaction (visible, destroyable targets)
- **Both required** = neither alone is sufficient

**Application to Pillar 1**:
- ✅ **Territory Beacons** - physical buildings that extend territory
- ✅ **Connectors** - physical structures that carry supply
- ❌ **Pheromone-only** - invisible territory (players can't interact)
- ❌ **Physical-only** - random placement (no strategic logic)

### 2.2 Emergent Behavior

**Core Rule**: Network topology emerges from pheromone diffusion, not hard-coded rules.

- **Beacon placement** = pheromone-driven (territory edges)
- **Connector placement** = territory overlap detection
- **Network density** = emergent from CC distribution
- **No hard-coded radii** = territory extends naturally

### 2.3 Player Agency

**Core Rule**: Players must have meaningful choices with visible consequences.

- **Multiple disruption points** = CC, Beacons, Connectors, Resource Nodes
- **Instant feedback** = destroy connector → supply cuts immediately
- **Safe zones** = beacon chains create intermediate building areas
- **Progressive difficulty** = easy (beacons) → hard (CCs)

---

## 3. Territory Hierarchy

### 3.1 Structure Chain

```
Command Center (strength 100, range ~150 tiles)
    ↓ emits TERRITORY_PHEROMONE
Territory Beacon 1 (strength 50, range ~75 tiles)
    ↓ extends territory
Territory Beacon 2 (strength 50, range ~75 tiles)
    ↓ extends territory
Connector (spawned where territory overlap > 40)
    ↓ carries TERRITORY_SUPPLY
Resource Node (attached via territory overlap)
```

### 3.2 Element Definitions

| Element | Role | Territory Strength | Range | Prototype Count |
|---------|------|-------------------|-------|-----------------|
| **Command Center** | Strategic anchor, emits territory | 100 | ~150 tiles | 15 (3 types × 5 tiers) |
| **Territory Beacon** | Extends territory, creates safe zones | 50 | ~75 tiles | 15 (3 types × 5 tiers) |
| **Connector** | Carries supply between elements | N/A | N/A | 10 (5 types × 2 tiers) |
| **Resource Node** | Attaches to territory, drops modules | N/A | N/A | 20 (4 resources × 5 tiers) |
| **Territory Markers** | Visual ground markings | N/A | N/A | 20 (decoratives) |
| **Total** | | | | **80/85 budget** |

### 3.3 Beacon vs. Connector Distinction

**Critical Design Question**: What's the difference?

| Aspect | Territory Beacon | Connector |
|--------|-----------------|-----------|
| **Purpose** | Extends territory range | Carries supply between elements |
| **Pheromone** | Emits TERRITORY_PHEROMONE | Does not emit (passive) |
| **Placement** | At territory edges (strength 30-60) | Where territory overlap > 40 |
| **Lifecycle** | Spawns emergently, decays on destruction | Gradual buildup (0%→100%), auto-removes |
| **Player Target** | Medium difficulty, tech module drop | Easy (buildup) / Hard (100%), supply disruption |
| **Visual** | Building structure (like small CC) | Bridge-like structure between buildings |

**Key Distinction**: Beacons **emit** territory (active), Connectors **carry** supply (passive).

---

## 4. Beacon Placement: Pheromone-Driven Emergence

### 4.1 Placement Logic

**Not hard-coded rules** - beacons spawn based on territory pheromone gradients:

- **Trigger**: During pheromone diffusion (40 chunks/tick)
- **Condition**: `TERRITORY_PHEROMONE` in range 30-60 (territory edge)
- **Direction**: Outward from CC (gradient direction)
- **Target**: Toward resource nodes when in range
- **Result**: Emergent chains that reach resources

### 4.2 Placement Constraints

| Constraint | Value | Rationale |
|------------|-------|-----------|
| **Minimum Territory** | 30 | Must be within existing territory |
| **Maximum Territory** | 60 | Must be at territory edge |
| **Max Chain Length** | 2-3 beacons | Prevents over-extension, maintains clarity |
| **Beacon Spacing** | 75-150 tiles | Creates safe zones for players |
| **Resource Target** | Within 150 tiles | Ensures beacons connect to resources |

### 4.3 Emergent Behavior

**What Happens**:
1. CC emits territory (strength 100 at center)
2. Territory diffuses outward (5% decay per tick)
3. At edges (strength 30-60), beacon placement checked
4. Beacon spawns, emits additional territory (strength 50)
5. New edge created, repeat process
6. Chain stops when no resources in range or territory too weak

**What Players See**:
- Beacon chains naturally extend toward resources
- No hard-coded paths (emergent topology)
- Multiple possible routes (redundant connections)
- Natural "safe zones" between beacon ranges

---

## 5. Connector Lifecycle

### 5.1 Lifecycle Stages

```
1. Territory Overlap Detected (during pheromone diffusion)
   ↓
2. Connector Scheduled (not instant - gradual buildup)
   ↓
3. Connector Spawns at 0% Health
   ↓
4. Gradual Buildup (+10% every 300 ticks = 5 min to 100%)
   ↓
5. At 100%: Supply Flow Begins (instant)
   ↓
6. Player Can Destroy During Buildup (easy target)
   ↓
7. At 100%: Full Supply Flow (harder to destroy)
   ↓
8. If Destroyed: Supply Cuts Instantly, Downstream Starves
   ↓
9. Auto-Reconnect Attempt (10 minutes later, different location)
```

### 5.2 Buildup Mechanics

| Property | Value | Rationale |
|----------|-------|-----------|
| **Start Health** | 0% | No supply flow during construction |
| **Buildup Rate** | +10% every 300 ticks | 5 minutes to completion |
| **Player Interruption** | Can destroy during buildup | Tactical window (easy target) |
| **At 100%** | Supply flow begins | Clear "complete" signal |
| **Visual** | Construction animation | Visible progress indicator |

### 5.3 Destruction Effects

| Timing | Effect | Player Experience |
|--------|--------|-------------------|
| **During Buildup (0-100%)** | Easy to destroy, no supply cut | "I can interrupt construction" |
| **At 100% (Complete)** | Harder to destroy, supply cuts instantly | "I cut their supply line" |
| **After Destruction** | Auto-reconnect attempt in 10 minutes | "They're rebuilding elsewhere" |

---

## 6. Beacon Destruction Effects

### 6.1 Immediate + Propagating (Eventual Consistency)

**Immediate Effect** (instant feedback):
- Local chunk loses 50% territory strength
- Connected connectors auto-remove (territory < 20)
- Downstream beacons marked for decay

**Propagating Effect** (eventual consistency):
- Decay spreads outward over 60 ticks
- Wave propagates 3 chunks radius
- Smooth territory collapse (not instant disappearance)

### 6.2 Propagation Pattern

```
Tick 0:   Beacon destroyed (center chunk -50%)
Tick 10:  Neighbors decay (-50%)
Tick 20:  Neighbors of neighbors decay (-50%)
Tick 30:  Continue outward...
Tick 60:  Propagation complete
```

**Design Rationale**:
- **Immediate**: Player sees instant consequence (satisfying feedback)
- **Propagating**: Territory collapses smoothly (not jarring)
- **Eventual consistency**: System stabilizes naturally (no manual cleanup)

---

## 7. Player Interjection Zones

### 7.1 Multiple Disruption Points

| Target | Difficulty | Reward | Territory Impact | Safe Zone Created? |
|--------|-----------|--------|------------------|-------------------|
| **Command Center** | Hard (high health) | Large tech module | Entire territory collapses | Yes (large area) |
| **Beacon 1** | Medium (mid health) | Medium tech module | Downstream territory decays | Yes (medium area) |
| **Beacon 2** | Easy (low health) | Small tech module | Local territory decays | Yes (small area) |
| **Resource Node** | Medium (mid health) | Tech module + enhanced patch | Enemy loses resource bonus | Yes (local area) |
| **Connector** | Easy (buildup) / Hard (100%) | Supply disruption | Downstream starvation | Temporary |

### 7.2 Safe Zone Design

**Definition**: Area where players can build without constant bombardment from adjacent enemy bases.

| Property | Value | Rationale |
|----------|-------|-----------|
| **Distance from CC** | 150-300 tiles | Beyond immediate CC threat range |
| **Distance from Beacons** | 75-150 tiles | Beyond beacon threat range |
| **Result** | Players can build defenses safely | Not constantly under attack |

**Strategic Impact**:
- Players can establish forward bases in safe zones
- Safe zones become contested territory (both sides want them)
- Creates natural "front lines" between enemy and player territory

---

## 8. Pheromone Integration

### 8.1 Pheromone Types Used

| Pheromone | Source | Decay | Purpose |
|-----------|--------|-------|---------|
| **TERRITORY_PHEROMONE** | CCs (100), Beacons (50) | 0.95 | Territory strength, beacon placement |
| **TERRITORY_SUPPLY** | Resource CCs | 0.95 | Supply flow through connectors |
| **TERRITORY_FACTION** | CCs + Beacons | 0.90 | Unit composition in territory |

### 8.2 Processing Integration

**No separate loops** - all integrated into existing 40-chunks/tick diffusion:

```
Existing: 40 chunks/tick process PLAYER, BASE, RESOURCE, ENEMY, KAMIKAZE
Enhanced: 40 chunks/tick process PLAYER, BASE, RESOURCE, ENEMY, KAMIKAZE,
          TERRITORY, TERRITORY_SUPPLY, TERRITORY_FACTION
```

**Performance Impact**: Negligible (same number of operations, just 3 more variables).

---

## 9. Design Constraints

### 9.1 Technical Constraints

| Constraint | Value | Rationale |
|------------|-------|-----------|
| **Pheromone Processing** | 40 chunks/tick | Existing infrastructure, no performance impact |
| **Connector Buildup** | Sparse queue (300 ticks) | Not per-tick processing |
| **Beacon Placement** | During diffusion | No separate placement loop |
| **Territory Decay** | Event-driven + propagation | No polling, eventual consistency |

### 9.2 Prototype Budget

| Category | Count | Budget | Status |
|----------|-------|--------|--------|
| Command Centers | 15 | 15 | ✅ |
| Territory Beacons | 15 | 15 | ✅ |
| Connectors | 10 | 10 | ✅ |
| Resource Nodes | 20 | 20 | ✅ |
| Territory Markers | 20 | 20 | ✅ |
| **Total** | **80** | **85** | ✅ (5 remaining) |

### 9.3 Gameplay Constraints

| Constraint | Value | Rationale |
|------------|-------|-----------|
| **Max Beacon Chain** | 2-3 beacons | Prevents over-extension, maintains clarity |
| **Connector Distance** | 75-100 tiles | Balanced interjection frequency |
| **Safe Zone Radius** | 150 tiles minimum | Clear separation from enemy threat |
| **Beacon Health Scale** | 1.0x-2.0x (diminishing) | Progression without sponges |

---

## 10. Design Rationale

### 10.1 Why Territory Beacons?

**Problem**: CCs alone create large territory gaps (150 tiles radius each).

**Solution**: Beacons extend territory (75 tiles each), creating intermediate zones.

**Benefits**:
- ✅ Finer granularity of territory control
- ✅ More tactical targets for players
- ✅ Safe zones for player building
- ✅ Clear visual hierarchy (CC → Beacon → Connector)

**Trade-offs**:
- ⚠️ More prototypes (15 beacon types)
- ⚠️ Slightly more complex hierarchy
- ✅ Within budget (80/85 used)

### 10.2 Why Gradual Connector Buildup?

**Problem**: Instant connectors give no tactical window for player interruption.

**Solution**: 5-minute buildup creates interruption opportunity.

**Benefits**:
- ✅ Tactical depth (attack during construction)
- ✅ Visual feedback (construction animation)
- ✅ Clear state (0-100% health = visible progress)
- ✅ Satisfying gameplay loop (interrupt → prevent supply)

**Trade-offs**:
- ⚠️ More complex (health tracking)
- ⚠️ Requires visual feedback
- ✅ Creates meaningful player choice

### 10.3 Why Eventual Consistency for Decay?

**Problem**: Instant territory collapse feels jarring, no time to react.

**Solution**: 60-tick propagation wave creates smooth collapse.

**Benefits**:
- ✅ Smooth visual transition (not jarring)
- ✅ Player can observe decay spreading
- ✅ Natural system behavior (no manual cleanup)
- ✅ Event-driven (no polling overhead)

**Trade-offs**:
- ⚠️ Slightly delayed effect (60 ticks)
- ⚠️ More complex propagation logic
- ✅ Feels more "organic" and natural

---

## 11. Open Questions (For Implementation)

1. **Beacon Chain Length**: Limit to 2-3 beacons max, or allow emergent chains?
   - **Recommendation**: Limit to 2-3 for clarity

2. **Connector Distance**: 50-75 tiles (tight) vs. 100-150 tiles (spaced)?
   - **Recommendation**: 75-100 tiles (balanced)

3. **Safe Zone Detection**: How do players know they're in a safe zone?
   - **Recommendation**: Visual cues (lighter ground markings, less enemy activity)

4. **Beacon Health Scaling**: Scale with player tech tier or keep consistent?
   - **Recommendation**: Scale with diminishing returns (1.0x-2.0x cap)

---

## 12. Success Criteria

Pillar 1 is successful when:
- ✅ Players can **see** supply lines (physical connectors + ground markings)
- ✅ Players can **interrupt** supply (destroy connectors during buildup)
- ✅ Players can **build safely** (safe zones between beacon ranges)
- ✅ Players understand **hierarchy** (CC → Beacon → Connector → Node)
- ✅ System performs well (no separate loops, integrated diffusion)
- ✅ Prototype budget respected (80/85 used)

---

## 13. Related Documents

- **01-Constraints-and-Assumptions.md**: Technical constraints, prototype budget
- **02-Theory-and-Vision.md**: Core vision, design philosophy
- **05-System-Architecture-And-Integration.md**: Technical blueprint
- **06-Key-Decisions-and-Learnings.md**: Finalized design decisions

---

**Document Version**: 1.0  
**Last Updated**: 2026-03-01  
**Status**: Ready for Implementation  
**Next Step**: Pillar 2: Supply-Gated Progression