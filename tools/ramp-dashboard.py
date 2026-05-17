#!/usr/bin/env python3
"""Rampant Factorio AI Dashboard - GTK4

Connects to Factorio via UDP and displays real-time AI stats.

Launch Factorio with: factorio --enable-lua-udp 5200
Then run: python3 ramp-dashboard.py
"""

import gi
gi.require_version('Gtk', '4.0')
gi.require_version('Gdk', '4.0')
from gi.repository import Gtk, GLib, Gdk

import socket
import json
import time
import sys
import threading

FACTORIO_UDP_PORT = 5200
POLL_INTERVAL_MS = 1000  # 1 second


class FactorioUDP:
    """UDP client for Rampant mod communication."""

    def __init__(self):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.sock.bind(('127.0.0.1', 0))
        self.my_port = self.sock.getsockname()[1]
        self.connected = False

    def send(self, cmd):
        """Send command and return parsed JSON response, or None.

        Waits for a response matching the expected keys to avoid grabbing
        replies meant for other commands on the shared UDP socket.
        """
        import sys
        # State command uses multi-packet streaming
        if cmd == "state":
            self.sock.sendto(cmd.encode(), ('127.0.0.1', FACTORIO_UDP_PORT))
            return self._recv_streamed()

        expected = {
            "info": {"tick", "awake"},
            "squads": {"squads"},
            "bases": {"bases"},
        }[cmd]

        deadline = time.monotonic() + 2.0
        try:
            self.sock.sendto(cmd.encode(), ('127.0.0.1', FACTORIO_UDP_PORT))

            while time.monotonic() < deadline:
                remaining = deadline - time.monotonic()
                self.sock.settimeout(min(0.3, remaining))
                try:
                    data, _ = self.sock.recvfrom(65536)
                except (socket.timeout, OSError):
                    continue
                try:
                    pkt = json.loads(data.decode())
                except json.JSONDecodeError:
                    continue
                if not isinstance(pkt, dict):
                    continue
                # Check if this response matches what we expect
                if expected.issubset(pkt.keys()):
                    self.connected = True
                    return pkt
                # Wrong response — keep waiting

            self.connected = False
            return None
        except Exception:
            self.connected = False
            return None

    def _recv_streamed(self):
        """Receive and reassemble a multi-packet streamed response."""
        packets = {}
        req_id = None
        total_packets = None
        deadline = time.monotonic() + 5.0

        while time.monotonic() < deadline:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                break
            self.sock.settimeout(min(0.1, remaining))
            try:
                data, _ = self.sock.recvfrom(131072)
            except socket.timeout:
                break
            except OSError:
                break

            try:
                pkt = json.loads(data.decode())
            except json.JSONDecodeError:
                continue

            if "id" not in pkt:
                self.connected = True
                return pkt

            if req_id is None:
                req_id = pkt["id"]
            elif pkt["id"] != req_id:
                continue

            total_packets = pkt.get("total", 1)
            seq = pkt.get("seq", 0)
            packets[seq] = pkt

            if len(packets) >= total_packets:
                break

        if not packets:
            self.connected = False
            return None

        self.connected = True
        first = packets[1]  # Lua sends seq starting at 1
        all_chunks = []
        for seq in sorted(packets.keys()):
            all_chunks.extend(packets[seq].get("chunks", []))

        return {
            "chunks": all_chunks,
            "count": len(all_chunks),
            "tick": first.get("tick", 0),
        }

    def close(self):
        self.sock.close()


class StatRow(Gtk.Box):
    """A single stat: label + value."""

    def __init__(self, label, value="—"):
        super().__init__(orientation=Gtk.Orientation.HORIZONTAL, spacing=6)
        self.set_size_request(-1, 22)

        lbl = Gtk.Label(label=f"{label}:")
        lbl.add_css_class("bold")
        lbl.set_halign(Gtk.Align.END)
        lbl.set_size_request(120, -1)
        self.append(lbl)

        self.value_label = Gtk.Label(label=str(value))
        self.value_label.set_halign(Gtk.Align.START)
        self.value_label.set_hexpand(True)
        self.append(self.value_label)

    def update(self, value):
        self.value_label.set_text(str(value))


class PheromoneBar(Gtk.Box):
    """Horizontal bar showing pheromone intensity."""

    def __init__(self, label, max_val=1.0):
        super().__init__(orientation=Gtk.Orientation.HORIZONTAL, spacing=6)
        self.set_size_request(-1, 24)
        self.max_val = max_val or 1.0

        lbl = Gtk.Label(label=label)
        lbl.add_css_class("bold")
        lbl.set_halign(Gtk.Align.END)
        lbl.set_size_request(90, -1)
        self.append(lbl)

        self.bar = _PheromoneDrawingArea()
        self.bar.set_hexpand(True)
        self.bar.set_size_request(-1, 16)
        self.append(self.bar)

        self.value_label = Gtk.Label(label="0")
        self.value_label.set_halign(Gtk.Align.START)
        self.value_label.set_size_request(50, -1)
        self.append(self.value_label)

    def set_value(self, val):
        fraction = min(val / self.max_val, 1.0) if self.max_val > 0 else 0
        self.value_label.set_text(f"{val:.1f}")
        self.bar.set_fraction(fraction)
        self.bar.queue_draw()


class _PheromoneDrawingArea(Gtk.DrawingArea):
    """Custom drawing area for pheromone bars."""

    def __init__(self):
        super().__init__()
        self.fraction = 0.0

    def set_fraction(self, frac):
        self.fraction = max(0.0, min(1.0, frac))

    def do_draw(self, cr):
        alloc = self.get_allocation()
        width, height = alloc.width, alloc.height

        # Background
        cr.set_source_rgb(0.1, 0.1, 0.1)
        cr.paint()

        # Bar fill
        bar_w = int(width * self.fraction)
        if bar_w > 0:
            r = 0.2 + 0.8 * self.fraction
            g = 0.8 * (1 - self.fraction)
            cr.set_source_rgb(r, g, 0.1)
            cr.rectangle(0, 0, bar_w, height)
            cr.fill()

        # Border
        cr.set_source_rgb(0.3, 0.3, 0.3)
        cr.rectangle(0, 0, width, height)
        cr.set_line_width(1)
        cr.stroke()


class _PheromoneMapArea(Gtk.Box):
    """Pheromone heatmap — builds texture at display size for full coverage."""

    PHERO_KEYS = ["b", "p", "e", "k", "r"]

    def __init__(self):
        super().__init__(orientation=Gtk.Orientation.VERTICAL, spacing=4)
        self.chunks = {}
        self.selected_idx = 0
        self._awake = None

        self._picture = Gtk.Picture()
        self._picture.set_content_fit(Gtk.ContentFit.CONTAIN)
        self._picture.set_vexpand(True)
        self._picture.set_size_request(-1, 300)
        self.append(self._picture)

        self._label = Gtk.Label(label="Waiting for data...")
        self._label.add_css_class("dim-label")
        self._label.set_halign(Gtk.Align.CENTER)
        self.append(self._label)

    def set_chunks(self, chunks):
        self.chunks = {(c.get("x", 0), c.get("y", 0)): c for c in chunks}
        self._build_texture()

    def set_selected(self, idx):
        self.selected_idx = idx
        self._build_texture()

    def set_awake(self, awake):
        self._awake = awake
        if not self.chunks:
            self._update_empty_label()

    def _update_empty_label(self):
        self._label.set_visible(True)
        if self._awake is False:
            self._label.set_text("AI not yet awake")
        elif self._awake is True:
            self._label.set_text("AI awake, no pheromones yet")
        else:
            self._label.set_text("Connected — waiting for AI")

    @staticmethod
    def _value_to_rgb(fraction):
        import math
        # Power curve spreads low values across the color range
        if fraction <= 0:
            return 0.05, 0.05, 0.15  # dark blue-black
        powered = math.pow(fraction, 0.35)  # compresses high, expands low
        t = min(powered, 1.0)
        if t < 0.33:
            r = 0
            g = t * 3 * 0.6
            b = 0.4 + t * 3 * 0.5
        elif t < 0.66:
            r = (t - 0.33) * 3
            g = 0.6 + (t - 0.33) * 3 * 0.4
            b = 0.9 - (t - 0.33) * 3 * 0.9
        else:
            r = 1.0
            g = 1.0 - (t - 0.66) * 3
            b = 0
        return r, g, b

    def _build_texture(self):
        import cairo
        import io
        from gi.repository import Gdk, GLib

        if not self.chunks:
            self._picture.set_paintable(None)
            self._update_empty_label()
            return

        self._label.set_visible(False)

        key = self.PHERO_KEYS[self.selected_idx]

        # Only include chunks with non-zero values for this pheromone type
        active = {k: v for k, v in self.chunks.items() if v.get(key, 0) > 0}
        if not active:
            self._picture.set_paintable(None)
            self._label.set_visible(True)
            self._label.set_text(f"No {key} pheromone data")
            return

        xs = sorted(set(x for x, y in active))
        ys = sorted(set(y for x, y in active))
        # Compact grid: map each real coord to a dense index, removing gaps
        x_map = {x: i for i, x in enumerate(xs)}
        y_map = {y: i for i, y in enumerate(ys)}
        grid_w = len(xs)
        grid_h = len(ys)

        vals = [v.get(key, 0) for v in active.values()]
        max_val = max(vals) if vals else 1
        if max_val <= 0:
            max_val = 1

        # Build small 1px-per-cell surface (compact grid, no gaps)
        small = cairo.ImageSurface(cairo.FORMAT_RGB24, grid_w, grid_h)
        ctx = cairo.Context(small)
        ctx.set_source_rgb(0.05, 0.05, 0.1)
        ctx.paint()
        for (cx, cy), chunk in active.items():
            val = chunk.get(key, 0)
            frac = val / max_val
            r, g, b = self._value_to_rgb(frac)
            ctx.set_source_rgb(r, g, b)
            ctx.rectangle(x_map[cx], y_map[cy], 1, 1)
            ctx.fill()

        # Subsample huge grids by averaging blocks; keep small grids at full resolution
        max_display = 400
        if grid_w > max_display or grid_h > max_display:
            step_x = max(1, grid_w // max_display)
            step_y = max(1, grid_h // max_display)
            sub_w = (grid_w + step_x - 1) // step_x
            sub_h = (grid_h + step_y - 1) // step_y

            # Rebuild from subsampled compact data
            small2 = cairo.ImageSurface(cairo.FORMAT_RGB24, sub_w, sub_h)
            ctx2 = cairo.Context(small2)
            ctx2.set_source_rgb(0.05, 0.05, 0.1)
            ctx2.paint()
            for sy in range(sub_h):
                for sx in range(sub_w):
                    total = 0.0
                    count = 0
                    for dy in range(step_y):
                        for dx in range(step_x):
                            col = sx*step_x + dx
                            row = sy*step_y + dy
                            if col < grid_w and row < grid_h:
                                rx = xs[col] if col < len(xs) else None
                                ry = ys[row] if row < len(ys) else None
                                if rx is not None and ry is not None:
                                    chunk = active.get((rx, ry))
                                    if chunk:
                                        total += chunk.get(key, 0)
                                        count += 1
                    frac = (total / count) / max_val if count > 0 else 0
                    r, g, b = self._value_to_rgb(frac)
                    ctx2.set_source_rgb(r, g, b)
                    ctx2.rectangle(sx, sy, 1, 1)
                    ctx2.fill()
            small = small2
            grid_w, grid_h = sub_w, sub_h

        # Build texture at display size
        pad = 8
        cell_px = min(128, 1920 / grid_w, 1080 / grid_h)
        cell_px = max(cell_px, 1)
        tex_w = int(grid_w * cell_px) + 2 * pad
        tex_h = int(grid_h * cell_px) + 2 * pad

        big = cairo.ImageSurface(cairo.FORMAT_RGB24, tex_w, tex_h)
        ctx = cairo.Context(big)
        ctx.set_source_rgb(0.05, 0.05, 0.05)
        ctx.paint()
        ctx.translate(pad, pad)
        ctx.scale(cell_px, cell_px)
        ctx.set_source_surface(small, 0, 0)
        ctx.paint()

        buf = io.BytesIO()
        big.write_to_png(buf)
        texture = Gdk.Texture.new_from_bytes(GLib.Bytes.new(buf.getvalue()))
        self._picture.set_paintable(texture)


class SquadRow(Gtk.Box):
    """Row for a single squad."""

    def __init__(self, squad_data):
        super().__init__(orientation=Gtk.Orientation.HORIZONTAL, spacing=8)
        self.set_size_request(-1, 24)

        self.id_lbl = Gtk.Label(label=f"#{squad_data.get('id', '?')}")
        self.id_lbl.add_css_class("bold")
        self.id_lbl.set_size_request(40, -1)
        self.append(self.id_lbl)

        self.pos_lbl = Gtk.Label(
            label=f"({squad_data.get('x', 0):.0f}, {squad_data.get('y', 0):.0f})"
        )
        self.pos_lbl.set_size_request(100, -1)
        self.append(self.pos_lbl)

        self.members_lbl = Gtk.Label(
            label=f"{squad_data.get('members', 0)} units"
        )
        self.members_lbl.set_size_request(70, -1)
        self.append(self.members_lbl)

        builder = "🔨" if squad_data.get("builder") else "⚔️"
        self.type_lbl = Gtk.Label(label=builder)
        self.type_lbl.set_size_request(30, -1)
        self.append(self.type_lbl)


class BaseRow(Gtk.Box):
    """Row for a single base."""

    def __init__(self, base_data):
        super().__init__(orientation=Gtk.Orientation.HORIZONTAL, spacing=8)
        self.set_size_request(-1, 24)

        self.id_lbl = Gtk.Label(label=f"#{base_data.get('id', '?')}")
        self.id_lbl.add_css_class("bold")
        self.id_lbl.set_size_request(40, -1)
        self.append(self.id_lbl)

        self.pos_lbl = Gtk.Label(
            label=f"({base_data.get('x', 0):.0f}, {base_data.get('y', 0):.0f})"
        )
        self.pos_lbl.set_size_request(100, -1)
        self.append(self.pos_lbl)

        tier = base_data.get('tier', 0)
        self.tier_lbl = Gtk.Label(label=f"T{tier}")
        self.tier_lbl.set_size_request(40, -1)
        self.append(self.tier_lbl)


class RampantDashboard(Gtk.ApplicationWindow):
    """Main dashboard window."""

    def __init__(self, **kwargs):
        super().__init__(
            application=kwargs.get('application'),
            title="Rampant AI Dashboard",
            default_width=700,
            default_height=550,
        )

        self.udp = FactorioUDP()
        self.squads_list = []
        self.bases_list = []
        self.chunk_stats = {}
        self._consecutive_failures = 0
        self._demo_mode = False
        self._demo_data = None  # cached demo chunks/squads/bases

        self._build_ui()
        self._start_polling()

    def _build_ui(self):
        # Main vertical box
        main = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)
        main.set_margin_start(12)
        main.set_margin_end(12)
        main.set_margin_top(12)
        main.set_margin_bottom(12)
        self.set_child(main)

        # === Header ===
        header = Gtk.Box(orientation=Gtk.Orientation.HORIZONTAL, spacing=12)
        header.set_margin_bottom(6)

        title = Gtk.Label(label="Rampant AI Dashboard")
        title.add_css_class("title-1")
        header.append(title)

        self.status_dot = Gtk.Label(label="●")
        self.status_dot.add_css_class("error")
        self.status_dot.set_hexpand(True)
        self.status_dot.set_halign(Gtk.Align.END)
        header.append(self.status_dot)

        self.status_lbl = Gtk.Label(label="Disconnected")
        self.status_lbl.set_hexpand(True)
        self.status_lbl.set_halign(Gtk.Align.END)
        header.append(self.status_lbl)

        # Demo button for when Factorio is connected but has no data yet
        demo_btn = Gtk.Button(label="Demo")
        demo_btn.set_size_request(60, -1)
        demo_btn.connect("clicked", lambda b: self._toggle_demo())
        header.append(demo_btn)

        main.append(header)

        # === Game Info Panel ===
        info_frame = Gtk.Frame(label="  Game State  ")
        info_frame.add_css_class("frame")
        info_box = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=2)

        self.row_tick = StatRow("Tick", "—")
        self.row_awake = StatRow("AI Awake", "—")
        self.row_evolution = StatRow("Evolution", "—")
        self.row_surfaces = StatRow("Surfaces", "—")

        for row in [self.row_tick, self.row_awake, self.row_evolution, self.row_surfaces]:
            info_box.append(row)

        info_frame.set_child(info_box)
        main.append(info_frame)

        # === Pheromone Summary ===
        pharm_frame = Gtk.Frame(label="  Pheromones (sampled)  ")
        pharm_frame.add_css_class("frame")
        pharm_box = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=2)

        self.bar_base = PheromoneBar("Base", 100)
        self.bar_player = PheromoneBar("Player", 100)
        self.bar_enemy = PheromoneBar("Enemy", 100)
        self.bar_kamikaze = PheromoneBar("Kamikaze", 100)
        self.bar_resource = PheromoneBar("Resource", 100)

        for bar in [self.bar_base, self.bar_player,
                    self.bar_enemy, self.bar_kamikaze, self.bar_resource]:
            pharm_box.append(bar)

        pharm_frame.set_child(pharm_box)
        main.append(pharm_frame)

        # === Pheromone Map ===
        map_frame = Gtk.Frame(label="  Pheromone Map  ")
        map_frame.add_css_class("frame")
        map_box = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=4)

        # Selector for which pheromone to show
        map_ctrl = Gtk.Box(orientation=Gtk.Orientation.HORIZONTAL, spacing=4)
        map_ctrl.set_margin_bottom(2)
        map_lbl = Gtk.Label(label="Show:")
        map_lbl.add_css_class("bold")
        map_ctrl.append(map_lbl)

        self.map_type = 0  # index into PHERO_KEYS
        self.map_buttons = []
        for i, name in enumerate(["Base", "Player", "Enemy", "Kamikaze", "Res"]):
            btn = Gtk.Button(label=name)
            btn.set_size_request(45, -1)
            btn.connect("clicked", lambda b, idx=i: self._set_map_type(idx))
            self.map_buttons.append(btn)
            map_ctrl.append(btn)
        self.map_buttons[0].add_css_class("suggested-action")

        self.map_legend = Gtk.Label(label="")
        self.map_legend.add_css_class("dim-label")
        self.map_legend.set_halign(Gtk.Align.END)
        self.map_legend.set_hexpand(True)
        map_ctrl.append(self.map_legend)

        map_box.append(map_ctrl)

        self.map_canvas = _PheromoneMapArea()
        map_box.append(self.map_canvas)

        map_frame.set_child(map_box)
        main.append(map_frame)

        # === Squads & Bases side by side ===
        split = Gtk.Box(orientation=Gtk.Orientation.HORIZONTAL, spacing=6)

        # Squads panel
        squads_frame = Gtk.Frame(label="  Squads  ")
        squads_frame.add_css_class("frame")
        squads_scroll = Gtk.ScrolledWindow()
        squads_scroll.set_policy(Gtk.PolicyType.NEVER, Gtk.PolicyType.AUTOMATIC)
        squads_scroll.set_vexpand(True)
        squads_scroll.set_max_content_height(180)
        self.squads_box = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=2)
        squads_scroll.set_child(self.squads_box)
        squads_frame.set_child(squads_scroll)
        split.append(squads_frame)

        # Bases panel
        bases_frame = Gtk.Frame(label="  Bases  ")
        bases_frame.add_css_class("frame")
        bases_scroll = Gtk.ScrolledWindow()
        bases_scroll.set_policy(Gtk.PolicyType.NEVER, Gtk.PolicyType.AUTOMATIC)
        bases_scroll.set_vexpand(True)
        bases_scroll.set_max_content_height(180)
        self.bases_box = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=2)
        bases_scroll.set_child(self.bases_box)
        bases_frame.set_child(bases_scroll)
        split.append(bases_frame)

        main.append(split)

    def _generate_demo_chunks(self):
        """Generate synthetic pheromone chunks for demo mode."""
        import math
        chunks = []
        for x in range(-10, 11):
            for y in range(-10, 11):
                d1 = math.sqrt((x + 5) ** 2 + (y + 4) ** 2)
                d2 = math.sqrt((x - 6) ** 2 + (y - 3) ** 2)
                d_min = min(d1, d2)
                chunks.append({
                    "x": x, "y": y,
                    "m": max(0, 8.0 - d_min * 0.8),
                    "b": max(0, 6.0 - d_min * 0.7),
                    "p": max(0, 5.0 - abs(x) * 0.6 - abs(y) * 0.6),
                    "e": max(0, 4.0 - d_min * 0.5) if d_min < 6 else 0,
                    "r": max(0, 3.0 - abs(x + 8) * 0.4) if x < -3 else 0,
                    "pu": max(0, 2.0 - d_min * 0.3) if d_min < 8 else 0,
                })
        return chunks

    def _inject_demo_data(self):
        """Inject synthetic pheromone data for visualization."""
        import sys
        print("[dashboard] _inject_demo_data called", file=sys.stderr)
        if not self._demo_data:
            self._demo_data = {
                "chunks": self._generate_demo_chunks(),
                "squads": [
                    {"id": 1, "x": -5, "y": -4, "members": 12, "builder": False},
                    {"id": 2, "x": 6, "y": 3, "members": 8, "builder": True},
                    {"id": 3, "x": -2, "y": 1, "members": 5, "builder": False},
                ],
                "bases": [
                    {"id": 1, "x": -5, "y": -4, "tier": 2},
                    {"id": 2, "x": 6, "y": 3, "tier": 1},
                ],
            }
        d = self._demo_data
        self.row_tick.update("DEMO")
        self.row_awake.update(True)
        self.row_evolution.update("0.4500")
        self.row_surfaces.update(1)
        self.map_canvas.set_awake(True)
        self._update_pheromones(d["chunks"])
        self._update_squads(d["squads"])
        self._update_bases(d["bases"])

    def _toggle_demo(self):
        """Toggle demo mode on/off via button."""
        self._demo_mode = not self._demo_mode
        if self._demo_mode:
            self._inject_demo_data()
            self.status_lbl.set_text("Demo mode")
            self.status_dot.remove_css_class("error")
            self.status_dot.add_css_class("success")
        else:
            self.map_canvas.set_chunks([])
            self.map_canvas.set_awake(None)
            if self.udp.connected:
                self.status_lbl.set_text("Connected")
            else:
                self.status_lbl.set_text("Disconnected")

    def _update_ui(self, info, squads_data, bases_data, state_data):
        """Update UI from background poll results. Called on main GTK thread."""
        if info:
            self.row_tick.update(info.get("tick", "?"))
            self.row_awake.update(info.get("awake", "?"))
            self.row_evolution.update(f"{info.get('evolution', 0):.4f}")
            self.row_surfaces.update(info.get("surfaces", "?"))
            self.status_dot.remove_css_class("error")
            self.status_dot.add_css_class("success")
            self._consecutive_failures = 0
            if not self._demo_mode:
                self.status_lbl.set_text("Connected")

            if squads_data:
                self._update_squads(squads_data.get("squads", []))
            if bases_data:
                self._update_bases(bases_data.get("bases", []))
            if state_data:
                raw_chunks = state_data.get("chunks", [])
                chunks = list(raw_chunks) if isinstance(raw_chunks, dict) else raw_chunks
                self.map_canvas.set_awake(info.get("awake", False))
                if chunks or not self._demo_mode:
                    was_demo = self._demo_mode
                    self._demo_mode = False
                    if was_demo:
                        self.status_lbl.set_text("Connected")
                    self._update_pheromones(chunks)
        else:
            self.status_dot.remove_css_class("success")
            self.status_dot.add_css_class("error")
            self._consecutive_failures += 1
            if self._demo_mode:
                self.status_lbl.set_text("Demo mode (no Factorio)")
            else:
                self.status_lbl.set_text("Disconnected")

    def _poll(self):
        """Legacy placeholder — polling now runs in background thread."""
        pass

    def _update_squads(self, squads):
        """Update squad list."""
        # Clear existing rows
        while self.squads_box.get_first_child():
            child = self.squads_box.get_first_child()
            self.squads_box.remove(child)

        if not squads:
            empty = Gtk.Label(label="No active squads")
            empty.add_css_class("dim-label")
            empty.set_halign(Gtk.Align.CENTER)
            self.squads_box.append(empty)
            return

        for sq in sorted(squads, key=lambda s: s.get("id", 0)):
            row = SquadRow(sq)
            self.squads_box.append(row)

    def _update_bases(self, bases):
        """Update base list."""
        while self.bases_box.get_first_child():
            child = self.bases_box.get_first_child()
            self.bases_box.remove(child)

        if not bases:
            empty = Gtk.Label(label="No bases detected")
            empty.add_css_class("dim-label")
            empty.set_halign(Gtk.Align.CENTER)
            self.bases_box.append(empty)
            return

        for b in sorted(bases, key=lambda x: x.get("id", 0)):
            row = BaseRow(b)
            self.bases_box.append(row)

    def _update_pheromones(self, chunks):
        """Compute average pheromone levels from chunk data."""
        if not chunks:
            return  # keep existing display, don't wipe

        totals = {"b": 0, "p": 0, "e": 0, "k": 0, "r": 0}
        n = len(chunks)

        for c in chunks:
            totals["b"] += c.get("b", 0)
            totals["p"] += c.get("p", 0)
            totals["e"] += c.get("e", 0)
            totals["k"] += c.get("k", 0)
            totals["r"] += c.get("r", 0)

        avg = {k: v / n for k, v in totals.items()}
        self.bar_base.set_value(avg.get("b", 0))
        self.bar_player.set_value(avg.get("p", 0))
        self.bar_enemy.set_value(avg.get("e", 0))
        self.bar_kamikaze.set_value(avg.get("k", 0))
        self.bar_resource.set_value(avg.get("r", 0))

        # Update map
        self.map_canvas.set_chunks(chunks)
        key_names = ["Movement", "Base", "Player", "Enemy", "Resource", "Pollution"]
        xs = [c["x"] for c in chunks]
        ys = [c["y"] for c in chunks]
        gw = max(xs) - min(xs) + 1
        gh = max(ys) - min(ys) + 1
        self.map_legend.set_text(
            f"{n} chunks | {gw}x{gh} | {key_names[self.map_type]}"
        )

    def _set_map_type(self, idx):
        for i, btn in enumerate(self.map_buttons):
            if i == idx:
                btn.add_css_class("suggested-action")
            else:
                btn.remove_css_class("suggested-action")
        self.map_type = idx
        self.map_canvas.set_selected(idx)

    def _start_polling(self):
        """Run polling in a background thread to avoid freezing GTK."""
        self._poll_stop = threading.Event()
        self._poll_thread = threading.Thread(target=self._poll_loop, daemon=True)
        self._poll_thread.start()

    def _poll_loop(self):
        """Background polling loop."""
        while not self._poll_stop.is_set():
            try:
                self._poll()
            except Exception as e:
                print(f"[poll] error: {e}", file=sys.stderr)
            # Sleep in small increments so we can stop promptly
            for _ in range(int(POLL_INTERVAL_MS / 10)):
                if self._poll_stop.is_set():
                    break
                time.sleep(0.01)

    def _poll(self):
        """Poll all stats from Factorio. Called from background thread."""
        import sys
        info = self.udp.send("info")
        squads_data = self.udp.send("squads") if info else None
        bases_data = self.udp.send("bases") if info else None
        state_data = self.udp.send("state") if info else None

        # Filter out error responses — don't let them wipe valid display data
        if state_data and isinstance(state_data, dict) and "error" in state_data:
            state_data = None

        # Schedule UI updates on the main GTK thread
        GLib.idle_add(self._update_ui, info, squads_data, bases_data, state_data)

    def close_request(self):
        self._poll_stop.set()
        self._poll_thread.join(timeout=2)
        self.udp.close()
        return True


class RampantApp(Gtk.Application):
    def __init__(self):
        super().__init__(application_id="com.factorio.rampant-dashboard",
                         flags=Gio.ApplicationFlags.FLAGS_NONE)
        self._start_demo = False

    def do_activate(self):
        win = RampantDashboard(application=self)
        if getattr(self, '_start_demo', False):
            win._demo_mode = True
            win._inject_demo_data()
            win.status_lbl.set_text("Demo mode")
            win.status_dot.remove_css_class("error")
            win.status_dot.add_css_class("success")
        win.present()


import gi
gi.require_version('Gio', '2.0')
from gi.repository import Gio

if __name__ == "__main__":
    start_demo = "--demo" in sys.argv
    # Strip our custom flag before GTK sees it
    gtk_args = [a for a in sys.argv if a != "--demo"]

    app = RampantApp()
    app._start_demo = start_demo
    app.run(gtk_args)
