# Factorio Tile & Decorative System Research
## For Supply Route Visualization (Without Rendering APIs)

**Date**: 2026-02-28
**Researcher**: Exploration Agent
**Purpose**: Understand available mechanisms for ground-based visual cues for supply routes

---

## 1. Available Tile Types for Visual Differentiation

### Base Game Tile Categories

#### Natural Terrain (Ground-Natural Layer)
- **Grass Variations**: `grass-1`, `grass-2`, `grass-3`, `grass-4`
- **Dirt Variations**: `dirt-1` through `dirt-7`
- **Sand Variations**: `sand-1`, `sand-2`, `sand-3`
- **Desert**: `red-desert-0` through `red-desert-3`

#### Artificial Surfaces (Ground-Artificial Layer)
- **Stone Path**: `stone-path` (vehicle modifier: 1.1x)
- **Concrete**: `concrete` (vehicle modifier: 0.8x)
- **Hazard Concrete**: `hazard-concrete-left`, `hazard-concrete-right`
- **Refined Concrete**: `refined-concrete`
- **Refined Hazard**: `refined-hazard-concrete-left`, `refined-hazard-concrete-right`

#### Water Tiles (Water Layer)
- `water`, `deepwater`, `water-green`, `deepwater-green`
- `water-shallow`, `water-mud`
- Space Age: `water-wube`

#### Special Tiles
- **Nuclear Ground**: `nuclear-ground` (absorbs 50% pollution)
- **Landfill**: `landfill` (creates new ground over water)
- **Lab Tiles**: `lab-dark-1`, `lab-dark-2`, `lab-white`
- **Tutorial**: `tutorial-grid`

### Tile Properties for Route Visualization

**Color Differentiation**: Each tile has a `map_color` property that can be used for visual distinction.

**Vehicle Speed Modifiers**:
- Grass: 1.6x (faster)
- Dirt: 1.4x
- Sand: 1.8x
- Stone Path: 1.1x
- Concrete: 0.8x (slower)

**Pollution Absorption**:
- Water tiles absorb pollution
- Nuclear ground absorbs 50% of pollution
- Other tiles have varying absorption rates

**Custom Tile Creation**:
- **NOT POSSIBLE** at runtime - tiles must be defined at data stage
- Requires prototype definition in `data.lua`
- Cannot create new tile types dynamically

---

## 2. API Methods for Placing/Removing Tiles and Decoratives

### Tile Manipulation API

#### `surface.set_tiles(tiles, correct_tiles, remove_colliding_entities, remove_colliding_decoratives, raise_event, player, undo_index)`

**Parameters**:
- `tiles` (required): Array of `{position = {x, y}, name = "tile-name"}`
- `correct_tiles` (optional, default=true): Apply tile correction logic for seamless transitions
- `remove_colliding_entities` (optional, default=true): "abort_on_collision" or boolean
- `remove_colliding_decoratives` (optional, default=true): Remove decoratives in placement area
- `raise_event` (optional, default=false): Fire `script_raised_set_tiles` event
- `player` (optional): Player for undo queue
- `undo_index` (optional): Undo item index

**Example**:
```lua
surface.set_tiles({
    {position = {x = 10, y = 20}, name = "stone-path"},
    {position = {x = 11, y = 20}, name = "stone-path"},
    {position = {x = 12, y = 20}, name = "stone-path"}
}, true, true, true, false)
```

**Performance Note**: "It is recommended to call this method once for all the tiles you want to change rather than calling it individually for every tile."

#### `surface.get_tile(x, y)`

Returns tile information at position:
```lua
local tile = surface.get_tile(10, 20)
print(tile.name)  -- "grass-1"
print(tile.hidden_tile)  -- tile underneath if any
```

### Decorative Manipulation API

#### `surface.create_decoratives({check_collision, decoratives})`

**Parameters**:
- `check_collision` (optional): Check against entities/tiles
- `decoratives`: Array of `{name = "decal-name", position = {x, y}, tint = color, angle = radians}`

**Example**:
```lua
surface.create_decoratives({
    decoratives = {
        {name = "stone-path-decal", position = {10, 20}},
        {name = "stone-path-decal", position = {11, 20}, tint = {r=1, g=0.8, b=0.2, a=1}}
    }
})
```

#### `surface.destroy_decoratives({area, position, name, collision_mask, from_layer, to_layer, exclude_soft, limit, invert})`

**Parameters**:
- `area` or `position`: Region to destroy in
- `name` (optional): Specific decorative names to destroy
- `collision_mask` (optional): Filter by collision layer
- `from_layer`, `to_layer`: Render layer range
- `exclude_soft` (optional): Exclude decoratives that grow through rails
- `limit` (optional): Max number to destroy
- `invert` (optional): Invert filters

#### `surface.find_decoratives_filtered({area, position, name, collision_mask, from_layer, to_layer, exclude_soft, limit, invert})`

Returns array of decorative entities matching criteria.

### Decorative Types Available

**Decals** (render in tile layers):
- `light-mud-decal`, `dark-mud-decal`
- `enemy-decal`, `enemy-decal-transparent`
- `lichen-decal`, `shroom-decal`, `worms-decal`
- `stone-path-decal` (various concrete decals exist)

**Render Layers for Decals**:
- Must have `render_layer = "decals"` in prototype
- `tile_layer` property controls order within tile layers
- Can be masked by water tiles

---

## 3. Natural Decay vs. Custom Implementation

### Tile Decay
**NO NATURAL DECAY** - Tiles persist indefinitely once placed
- No time-to-live property
- No automatic reversion to underlying tiles
- Must be manually changed or removed via `set_tiles()`

### Decorative Decay
**NO NATURAL DECAY** - Decoratives persist indefinitely
- No built-in time-to-live
- Removed only when:
  - Building placed on top (if `remove_decoratives = "automatic"`)
  - Explicitly destroyed via API
  - Surface destroyed

### Custom Fade Implementation

**Approach 1: Periodic Cleanup (Recommended)**
```lua
-- Track decorated positions with timestamps
local routeDecorations = global.routeDecorations or {}
global.routeDecorations = routeDecorations

-- In on_tick handler (e.g., every 60 ticks = 1 second)
local function decayDecorations(event)
    if event.tick % 60 ~= 0 then return end
    
    for key, data in pairs(routeDecorations) do
        data.age = data.age + 1
        if data.age >= data.maxAge then
            -- Remove decorative
            surface.destroy_decoratives({
                position = data.position,
                name = data.decalName
            })
            routeDecorations[key] = nil
        end
    end
end
```

**Approach 2: Event-Driven Cleanup**
- Track when supply route becomes inactive
- Batch cleanup when routes are deconstructed
- Use `on_built_entity` and `on_player_mined_entity` events

**Approach 3: Gradient Fade (Advanced)**
- Maintain multiple "intensity" levels
- Gradually reduce alpha/tint over time
- Replace with lower-intensity decals before removal

---

## 4. Performance Comparison: Tiles vs. Decoratives vs. Rendering

### Tile Performance

**Pros**:
- Integrated into terrain rendering (efficient batch rendering)
- Automatic transition graphics between tile types
- Persistent without tracking overhead
- No entity count impact

**Cons**:
- **High cost for changes**: Tile correction logic runs after each `set_tiles()` call
- "Many small API calls are generally more performance intensive than one big one"
- Cannot create custom tile types at runtime
- May remove colliding entities/decoratives
- Affects pathfinding (walking speed, vehicle friction)

**Batch Size Recommendation**:
- Call `set_tiles()` once per frame for all route tiles
- Avoid per-tile API calls
- Group by surface and tile type

### Decorative Performance

**Pros**:
- Lightweight compared to entities
- Can be tinted for color variation
- Don't affect pathfinding or movement
- Can be placed densely without collision issues
- Decals render efficiently in tile layers

**Cons**:
- Require tracking for cleanup
- Limited to existing decorative prototypes
- No custom decoratives at runtime
- Can be removed by building placement
- Count toward entity limits (though "optimized-decorative" type is efficient)

**Optimized Decoratives**:
- Use `type = "optimized-decorative"` prototypes
- More efficient than standard decoratives
- Examples: `stone-path-decal`, various ground patches

### Rendering API (NOT AVAILABLE)

**Key Finding**: Factorio has **NO client-side rendering API** for mods
- No `render.draw()` or similar functions
- No custom shaders or overlays
- No persistent visual elements outside game entities
- Chart tags only show icons/labels, not ground markings

**Alternative Visualization Methods**:
1. **Beam entities**: Can connect two points, have duration, customizable
2. **Stream entities**: Fluid-like connections between points
3. **Particle entities**: Temporary visual effects (no control over appearance)
4. **Highlight boxes**: Only visible to specific players, UI-like
5. **Custom chart tags**: Map view only, icon + text at position

### Performance Ranking (Best to Worst for Route Visualization)

1. **Decoratives (Decals)** - Best balance
   - Efficient rendering
   - Ground-level appearance
   - Can be batched
   - Minimal performance impact

2. **Tiles** - Moderate
   - Efficient when batched
   - Expensive to modify frequently
   - Better for permanent routes

3. **Beam/Stream Entities** - Poor for routes
   - High entity count for long routes
   - Visible above ground (not integrated)
   - Better for point-to-point connections

4. **Particles** - Worst
   - Very temporary (few seconds)
   - No positioning control
   - High overhead per particle

---

## 5. Recommended Approach for Supply Route Visualization

### Primary Recommendation: **Decal-Based System**

**Why Decals**:
- Efficient rendering in tile layers
- Ground-level appearance (blends with terrain)
- Can use existing decorative prototypes
- Tinting allows color coding
- No impact on pathfinding
- Lower overhead than entities

**Implementation Strategy**:

```lua
-- Data structure to track route decorations
local routeDecorations = global.supplyRouteDecorations or {}
global.supplyRouteDecorations = routeDecorations

-- Place decorative along route
local function markRouteTile(surface, x, y, routeId, intensity)
    local key = string.format("%d:%d:%s", x, y, routeId)
    
    -- Choose decal based on intensity (0.0 to 1.0)
    local decalName = "stone-path-decal"  -- or other ground decal
    local tint = {r = 1, g = 0.8, b = 0.2, a = intensity}
    
    -- Check if already exists
    local existing = surface.find_decoratives_filtered({
        position = {x, y},
        name = decalName,
        limit = 1
    })[1]
    
    if existing then
        existing.set_tint(tint)
    else
        surface.create_decoratives({
            decoratives = {{
                name = decalName,
                position = {x, y},
                tint = tint
            }}
        })
    end
    
    routeDecorations[key] = {
        surface = surface.index,
        x = x,
        y = y,
        routeId = routeId,
        intensity = intensity,
        age = 0
    }
end

-- Decay system (run every 60 ticks)
local function updateRouteDecorations(event)
    if event.tick % 60 ~= 0 then return end
    
    for key, data in pairs(routeDecorations) then
        local surface = game.get_surface(data.surface)
        if not surface.valid then
            routeDecorations[key] = nil
            continue
        end
        
        -- Reduce intensity over time
        data.intensity = math.max(0, data.intensity - 0.1)
        
        if data.intensity <= 0 then
            -- Remove decal
            local decals = surface.find_decoratives_filtered({
                position = {data.x, data.y},
                limit = 10
            })
            for _, decal in ipairs(decals) do
                decal.destroy()
            end
            routeDecorations[key] = nil
        end
    end
end

script.on_event(defines.events.on_tick, updateRouteDecorations)
```

### Secondary Recommendation: **Tile-Based for Permanent Routes**

**Use Case**: Long-term supply routes that rarely change

```lua
-- Batch tile placement for efficiency
local function markRouteTiles(surface, positions, tileType)
    local tiles = {}
    for _, pos in ipairs(positions) do
        table.insert(tiles, {
            position = pos,
            name = tileType
        })
    end
    
    surface.set_tiles(tiles, true, false, false, false)
end
```

### Hybrid Approach

**Best of Both Worlds**:
- Use **tiles** for base route (permanent, efficient)
- Use **decoratives** for dynamic elements (active/inactive status)
- Use **beam entities** for long-distance connections (visible on chart)

```lua
-- Base route as stone-path tiles
markRouteTiles(surface, routePositions, "stone-path")

-- Active supply as tinted decals
for _, pos in ipairs(routePositions) do
    markRouteTile(surface, pos.x, pos.y, routeId, 1.0)
end

-- Route endpoints as chart tags
game.surfaces[1].add_custom_chart_tag({
    position = routeStart,
    force = player.force,
    icon = {type = "item", name = "iron-chest"},
    text = "Supply Route #1"
})
```

### Connection Detection

**Distance-Based Detection**:
```lua
local function areBuildingsConnected(pos1, pos2, maxDistance)
    local distance = math.sqrt(
        (pos1.x - pos2.x)^2 + (pos1.y - pos2.y)^2
    )
    return distance <= maxDistance
end
```

**Explicit Path Tracking**:
- Store route as ordered list of positions
- Check if adjacent positions are within 1-2 tiles
- Use flood-fill to detect connected segments

**Performance Considerations**:
- Cache route data in global
- Update connections only on build/mining events
- Use chunk-based spatial indexing for large networks

---

## 6. Available Decorative Prototypes for Ground Markings

From base game (`/mnt/gallery/gameFiles/factorio/data/base/prototypes/decorative/decoratives.lua`):

### Ground Decals (render_layer = "decals")
- `stone-path-decal` - Perfect for routes
- Various concrete decals
- `light-mud-decal`, `dark-mud-decal` - Natural look
- `lichen-decal`, `shroom-decal` - Organic markers
- `enemy-decal`, `enemy-decal-transparent` - Warning markers

### Finding Available Decals
```lua
-- List all decal-type decoratives
for name, decorative in pairs(data.raw["optimized-decorative"]) do
    if decorative.render_layer == "decals" then
        log("Available decal: " .. name)
    end
end
```

---

## 7. Event Handling for Route Maintenance

### Relevant Events
```lua
-- Track when buildings are placed/removed near routes
script.on_event(defines.events.on_built_entity, onEntityBuilt)
script.on_event(defines.events.on_robot_built_entity, onEntityBuilt)
script.on_event(defines.events.on_player_mined_entity, onEntityMined)
script.on_event(defines.events.on_robot_mined_entity, onEntityMined)
script.on_event(defines.events.script_raised_set_tiles, onTilesChanged)
```

### Cleanup Strategy
- Listen for mining events on route tiles
- Remove associated decoratives
- Update route connectivity
- Batch cleanup operations

---

## 8. Summary & Recommendations

### For Supply Route Visualization

| Method | Best For | Performance | Flexibility |
|--------|----------|-------------|-------------|
| **Decals** | Dynamic routes | Excellent | High (tinting) |
| **Tiles** | Permanent routes | Good (batched) | Low (fixed types) |
| **Beams** | Point connections | Poor | Medium |
| **Chart Tags** | Route endpoints | Excellent | Low (icons only) |

### Recommended Implementation

**Phase 1**: Decal-based dynamic routes
- Use `stone-path-decal` or similar
- Tint for route status (active/inactive)
- Implement decay system for cleanup

**Phase 2**: Tile-based permanent routes
- Use `stone-path` or `concrete` for base
- Add decals for dynamic elements
- Batch tile operations for performance

**Phase 3**: Enhanced visualization
- Chart tags for route endpoints
- Beams for long-distance connections
- Color coding by route priority/type

### Performance Guidelines

1. **Batch Operations**: Always batch tile/decorative changes
2. **Limit Decorations**: Cap max decoratives per route
3. **Efficient Cleanup**: Use `find_decoratives_filtered` with limits
4. **Chunk-Based**: Process routes by chunk for cache efficiency
5. **Event-Driven**: Update on relevant events, not every tick

### Code Structure Recommendation

```lua
-- Module: RouteVisualizer.lua
local RouteVisualizer = {}

-- Configuration
RouteVisualizer.config = {
    decalType = "stone-path-decal",
    decayRate = 0.1,  -- per second
    maxAge = 60,      -- seconds before removal
    tickInterval = 60 -- update frequency
}

-- Public API
function RouteVisualizer:createRoute(surface, positions, routeId, color) end
function RouteVisualizer:updateRoute(routeId, isActive) end
function RouteVisualizer:removeRoute(routeId) end
function RouteVisualizer:decayRoutes(event) end

-- Event registration
function RouteVisualizer:registerEvents() end

return RouteVisualizer
```

---

## 9. Files Referenced

- `/mnt/backpack/code/games/factorio/luaDefinitions/library/runtime-api/LuaSurface.lua`
- `/mnt/backpack/code/games/factorio/luaDefinitions/library/prototype-api/prototypes/TilePrototype.lua`
- `/mnt/backpack/code/games/factorio/luaDefinitions/library/prototype-api/prototypes/DecorativePrototype.lua`
- `/mnt/gallery/gameFiles/factorio/data/base/prototypes/tile/tiles.lua`
- `/mnt/gallery/gameFiles/factorio/data/base/prototypes/decorative/decoratives.lua`
- `/mnt/backpack/code/games/factorio/Rampant/prototypes/utils/BiterUtils.lua` (existing decal usage)

---

## 10. Next Steps for Implementation

1. **Identify Available Decal Types**: Query `data.raw["optimized-decorative"]` for suitable ground decals
2. **Create Route Visualizer Module**: Implement decal-based system with decay
3. **Test Performance**: Measure FPS impact with 100, 500, 1000 route tiles
4. **Integrate with Rampant AI**: Connect to supply route logic
5. **Add Color Coding**: Implement tint system for route status
6. **Implement Cleanup**: Event-driven removal when routes change

**Suggested Agent**: @fp-specialist for functional implementation of RouteVisualizer module
**Suggested Agent**: @architect-system for performance optimization and chunk-based processing