"""Cecelia overview for the README: design first, every step recorded, what you can ask.

    python3 make_readme_overview.py ../../../frontend/public/readme-structure.svg

Three panels, pictograms over words:
  1. Design first: the set's design as the app's design sketch draws it (DesignSketch.vue: one row per
     group, one ring per replicate, a dot per image inside it), the question, the expectation, or
     "exploratory".
  2. Every step is recorded: images -> segment -> populations -> analyse -> claim check around one
     record (settings, version, who, when, design; chains replay it). Each step carries its check
     (ok / look here), and the claim check points back at the design it is checked against. Kiwi and
     Claude Code sit beside it as optional sidekicks, drawn with the app's own schematics
     (ClaudeWaysIn.vue) and accents (--cc-kiwi, --cc-claude).
  3. What you can ask: gate, cluster, track, lens, regions, per replicate.

The design numbers are an example layout (5 vs 3 replicates), not a dataset. Pictograms are drawn,
not data; seeds are fixed so the file is stable between runs.
"""
import importlib.util
import json
import math
import random
import sys
from pathlib import Path
from xml.sax.saxutils import escape

# The look every README figure shares (surface, inks, font), loaded by path: this runs as plain python3.
_spec = importlib.util.spec_from_file_location(
    "figure_style", Path(__file__).resolve().parents[3] / "scripts" / "judge" / "figure_style.py")
style = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(style)

OUT = sys.argv[1]
INK, DIM, FONT, SURFACE = style.INK, style.DIM, style.FONT, style.SURFACE
W, H = 1680, 1090
# Category colours are the app's own Tol-bright palette, read from the file the plots use.
_ROOT = Path(__file__).resolve().parents[3]
TOL = json.loads((_ROOT / "frontend" / "src" / "plots" / "palettes.json").read_text(encoding="utf-8"))[
    "palettes"]["tol-bright"]
DARK, CARD_EDGE, OLIVE, GREY = "#1d1f24", "#d6d3ca", "#efeecf", "#ecebe6"
ACC = TOL[0]
OK, LOOK = "#5fb07a", "#f0b44c"
KIWI, CLAUDE = "#14b8a6", "#d97757"   # --cc-kiwi, --cc-claude (frontend/src/style.css)
out = []


# ---------------------------------------------------------------- primitives
def add(s):
    out.append(s)


def text(x, y, s, size=16, weight="normal", fill=INK, anchor="start", italic=False):
    it = ' font-style="italic"' if italic else ""
    add(f'<text x="{x:.1f}" y="{y:.1f}" font-size="{size}" font-weight="{weight}" fill="{fill}" '
        f'text-anchor="{anchor}"{it}>{escape(s)}</text>')


def rect(x, y, w, h, fill, stroke="none", r=8, dash=False, sw=1.2):
    d = ' stroke-dasharray="5 4"' if dash else ""
    add(f'<rect x="{x:.1f}" y="{y:.1f}" width="{w:.1f}" height="{h:.1f}" rx="{r}" fill="{fill}" '
        f'stroke="{stroke}" stroke-width="{sw}"{d}/>')


def circle(x, y, r, fill, stroke="none", sw=1, op=1):
    add(f'<circle cx="{x:.1f}" cy="{y:.1f}" r="{r}" fill="{fill}" stroke="{stroke}" stroke-width="{sw}" '
        f'opacity="{op}"/>')


def path(d, dash=False, sw=1.5, arrow=True):
    da = ' stroke-dasharray="5 4"' if dash else ""
    m = ' marker-end="url(#ah)"' if arrow else ""
    add(f'<path d="{d}" fill="none" stroke="{DIM}" stroke-width="{sw}"{m}{da}/>')


def group(x, y, k, fn, *a):
    add(f'<g transform="translate({x:.1f},{y:.1f}) scale({k})">')
    fn(0, 0, *a)
    add("</g>")


def head(x, y, num, title):
    circle(x + 26, y - 12, 26, INK)
    text(x + 26, y - 1, str(num), size=30, weight="800", fill="#fff", anchor="middle")
    text(x + 66, y, title, size=44, weight="700")


def badge(x, y, state, r=17):
    circle(x, y, r, state, stroke="#fff", sw=4)
    if state == OK:
        add(f'<path d="M{x-7},{y} L{x-2},{y+6} L{x+8},{y-6}" fill="none" stroke="#fff" stroke-width="3.5" '
            'stroke-linecap="round" stroke-linejoin="round"/>')
    else:
        text(x, y + 8, "!", size=r + 6, weight="800", fill="#fff", anchor="middle")


def arc(cx, cy, r, a1, a2):
    x1, y1 = cx + r * math.cos(a1), cy + r * math.sin(a1)
    x2, y2 = cx + r * math.cos(a2), cy + r * math.sin(a2)
    path(f"M{x1:.1f},{y1:.1f} A{r},{r} 0 0 1 {x2:.1f},{y2:.1f}", sw=4)


# ---------------------------------------------------------------- pictograms (each ~100 x 96 at scale 1)
def cells(seed, n, x, y, w, h, rmin=6, rmax=10):
    r, pts = random.Random(seed), []
    for _ in range(2000):
        if len(pts) == n:
            break
        rr = r.uniform(rmin, rmax)
        px, py = x + rr + r.random() * (w - 2 * rr), y + rr + r.random() * (h - 2 * rr)
        if all(math.hypot(px - a, py - b) > rr + c + 1.5 for a, b, c in pts):
            pts.append((px, py, rr))
    return pts


def pic_images(x, y):
    for k, (dx, dy) in enumerate([(24, 0), (12, 12), (0, 24)]):
        rect(x + dx, y + dy, 92, 80, DARK, r=6)
        r = random.Random(k)
        for _ in range(14):
            circle(x + dx + 8 + r.random() * 76, y + dy + 8 + r.random() * 64, 3 + r.random() * 3,
                   TOL[r.randrange(5)], op=0.85)


def pic_segment(x, y, w=120, h=104):
    rect(x, y, w, h, DARK, r=6)
    for i, (px, py, rr) in enumerate(cells(3, 22, x + 4, y + 4, w - 8, h - 8)):
        circle(px, py, rr * 0.55, TOL[i % 5], op=0.55)
        circle(px, py, rr, "none", stroke=TOL[i % 5], sw=1.4)


def pic_gate(x, y, w=96, h=96):
    add(f'<line x1="{x}" y1="{y+h}" x2="{x+w}" y2="{y+h}" stroke="{DIM}"/>'
        f'<line x1="{x}" y1="{y}" x2="{x}" y2="{y+h}" stroke="{DIM}"/>')
    r = random.Random(11)
    gx, gy, gw, gh = x + 50, y + 8, 40, 38
    for _ in range(70):
        if r.random() < 0.35:
            px, py = r.gauss(x + 70, 8), r.gauss(y + 27, 7)
        else:
            px, py = r.gauss(x + 30, 11), r.gauss(y + 68, 11)
        px, py = min(max(px, x + 3), x + w - 3), min(max(py, y + 3), y + h - 3)
        circle(px, py, 2.4, TOL[1] if gx < px < gx + gw and gy < py < gy + gh else "#9aa7b8")
    rect(gx, gy, gw, gh, "none", stroke=INK, r=2, dash=True, sw=1.3)


def pic_cluster(x, y):
    r = random.Random(5)
    for c, (cx, cy, sd) in enumerate([(30, 34, 9), (70, 30, 8), (52, 70, 10), (84, 72, 6)]):
        for _ in range(26):
            circle(x + r.gauss(cx, sd), y + r.gauss(cy, sd * 0.85), 2.4, TOL[[0, 3, 5, 4][c]], op=0.9)


def pic_tracks(x, y, w=100, h=96):
    rect(x, y, w, h, DARK, r=6)
    r = random.Random(2)
    for i in range(5):
        px, py, ang, pts = x + 12 + r.random() * 70, y + 12 + r.random() * 70, r.random() * 6.28, []
        for _ in range(12):
            pts.append((px, py))
            ang += r.gauss(0, 0.6)
            px = min(max(px + 5 * math.cos(ang), x + 5), x + w - 5)
            py = min(max(py + 5 * math.sin(ang), y + 5), y + h - 5)
        d = "M" + " L".join(f"{a:.1f},{b:.1f}" for a, b in pts)
        add(f'<path d="{d}" fill="none" stroke="{TOL[i]}" stroke-width="1.8" stroke-linejoin="round"/>')
        circle(*pts[-1], 3.2, TOL[i])


def pic_lens(x, y, w=104, h=96):
    rect(x, y, w, h, DARK, r=6)
    lx, ly, lr = x + 58, y + 46, 30
    for i, (px, py, rr) in enumerate(cells(9, 26, x + 3, y + 3, w - 6, h - 6, 5, 8)):
        inside = math.hypot(px - lx, py - ly) < lr
        circle(px, py, rr, TOL[[1, 4, 3][i % 3]] if inside else "#4a4d55", op=0.9 if inside else 1)
    circle(lx, ly, lr, "none", stroke="#ffffff", sw=2)


def pic_regions(x, y, w=104, h=96):
    rect(x, y, w, h, "#f1f0eb", r=6)
    for c, d in [(TOL[4], f"M{x},{y} L{x+60},{y} C{x+50},{y+40} {x+30},{y+50} {x},{y+60} Z"),
                 (TOL[3], f"M{x+60},{y} L{x+w},{y} L{x+w},{y+55} C{x+70},{y+60} {x+55},{y+40} {x+60},{y} Z"),
                 (TOL[5], f"M{x},{y+60} C{x+30},{y+50} {x+50},{y+40} {x+60},{y} C{x+55},{y+40} {x+70},{y+60} "
                          f"{x+w},{y+55} L{x+w},{y+h} L{x},{y+h} Z")]:
        add(f'<path d="{d}" fill="{c}" opacity="0.28"/>')
    r = random.Random(4)
    for _ in range(40):
        circle(x + 4 + r.random() * (w - 8), y + 4 + r.random() * (h - 8), 2.2, "#555")


def pic_superplot(x, y, w=104, h=96):
    """Cell dots faint, one big dot per replicate mean, a bar for the group mean."""
    base = y + h
    add(f'<line x1="{x}" y1="{base}" x2="{x+w}" y2="{base}" stroke="{DIM}"/>')
    r = random.Random(8)
    for gx, reps, mu in [(x + 28, [0, 1, 2], 50), (x + 78, [3, 4], 52)]:
        means = []
        for m in reps:
            mm = r.gauss(mu, 8)
            means.append(mm)
            for _ in range(14):
                circle(gx + r.gauss(0, 7), base - (mm + r.gauss(0, 14)), 1.8, TOL[m], op=0.35)
        for k, mm in enumerate(means):
            circle(gx - 8 + k * 8, base - mm, 5, TOL[reps[k]], stroke="#fff", sw=1.2)
        avg = sum(means) / len(means)
        add(f'<line x1="{gx-16}" y1="{base-avg}" x2="{gx+16}" y2="{base-avg}" stroke="{INK}" stroke-width="2"/>')


def pic_claim(x, y):
    pic_superplot(x, y + 20, 104, 76)
    add(f'<path d="M{x+28},{y+14} L{x+28},{y+8} L{x+78},{y+8} L{x+78},{y+14}" fill="none" '
        f'stroke="{INK}" stroke-width="1.3"/>')


# The app's sidekick schematics, from ClaudeWaysIn.vue (viewBox 0 0 96 60); ACC is the tool's accent.
_FR, _DIM, _INK = 'stroke="#c9c6bd" fill="#f3f2ee"', 'stroke="#8a8a85"', f'stroke="{INK}"'
KIWI_FIG = (f'<rect {_FR} x="2" y="8" width="62" height="48" rx="4"/>'
            '<line stroke="#c9c6bd" x1="2" y1="16" x2="64" y2="16"/>'
            f'<circle {_DIM} cx="16" cy="28" r="4"/><circle {_DIM} cx="20" cy="44" r="3.5"/>'
            f'<circle {_DIM} cx="48" cy="45" r="3.5"/><circle {_INK} cx="36" cy="34" r="5"/>'
            '<circle stroke="ACC" stroke-dasharray="2.5 2.5" cx="36" cy="34" r="9"/>'
            f'<path stroke="ACC" fill="{GREY}" d="M62 4 h28 a4 4 0 0 1 4 4 v12 a4 4 0 0 1 -4 4 h-18 l-6 5 v-5 '
            'h-4 a4 4 0 0 1 -4 -4 v-12 a4 4 0 0 1 4 -4z"/>'
            f'<line {_INK} x1="66" y1="11" x2="88" y2="11"/><line {_DIM} x1="66" y1="17" x2="80" y2="17"/>'
            '<path stroke="ACC" stroke-dasharray="2.5 2.5" d="M64 29 L45 33"/>')
CC_FIG = ('<rect stroke="#c9c6bd" fill="#fafaf7" x="2" y="12" width="44" height="36" rx="4"/>'
          '<path stroke="ACC" d="M9 24 l5 4 l-5 4"/>'
          f'<line {_INK} x1="18" y1="33" x2="26" y2="33"/><line {_DIM} x1="9" y1="40" x2="36" y2="40"/>'
          f'<path {_DIM} d="M48 30 h8 M56 12 v36 M56 12 h4 M56 30 h4 M56 48 h4"/>'
          '<circle stroke="ACC" cx="66" cy="12" r="3"/><circle stroke="ACC" cx="78" cy="12" r="3"/>'
          '<circle stroke="ACC" cx="90" cy="12" r="3"/><path stroke="ACC" d="M69 12 h6 M81 12 h6"/>'
          f'<rect {_INK} x="63" y="24" width="12" height="12" rx="1.5"/>'
          f'<rect {_INK} x="78" y="24" width="12" height="12" rx="1.5"/>'
          f'<path {_DIM} d="M65 33 l3 -3 l3 2 l3 -4 M80 33 v-3 M83 33 v-6 M86 33 v-4"/>'
          f'<rect {_INK} x="63" y="42" width="27" height="14" rx="1.5"/><path {_DIM} d="M67 47 h19 M67 51 h13"/>')


# ---------------------------------------------------------------- panels
def design_panel():
    head(40, 64, 1, "Design first")
    rect(40, 96, 490, 640, OLIVE, r=24)

    def rep(x, y, n, r=28):
        circle(x, y, r, "#ffffff", stroke=ACC, sw=4.5)
        for dx, dy in {1: [(0, 0)], 2: [(-8, 0), (8, 0)], 3: [(-8, -7), (8, -7), (0, 8)]}[n]:
            circle(x + dx, y + dy, 5, ACC)

    for gi, (g, reps) in enumerate([("A", [2, 2, 1, 2, 2]), ("B", [3, 2, 2])]):   # an example layout
        yy = 170 + gi * 80
        text(70, yy + 11, g, size=32, weight="800")
        for k, n in enumerate(reps):
            rep(135 + k * 72, yy, n)
    circle(82, 322, 13, "#ffffff", stroke=ACC, sw=4)
    text(104, 330, "one replicate — the n", size=22, fill=DIM)
    circle(352, 322, 5, ACC)
    text(366, 330, "images inside it", size=22, fill=DIM)
    text(70, 378, "Group: A vs B · n = 5 vs 3", size=26, weight="600")
    add('<line x1="70" y1="410" x2="500" y2="410" stroke="#dcd9b8" stroke-width="2"/>')
    text(70, 458, "Question", size=24, weight="700")
    text(70, 496, "Does it differ between A and B?", size=28)
    text(70, 560, "Expected", size=24, weight="700")
    x = 70
    for lab, on in [("not sure", False), ("no difference", True), ("higher in A", False)]:
        w = 24 + len(lab) * 10.5
        rect(x, 578, w, 44, ACC if on else "#ffffff", stroke=ACC if on else "#cfccc2", r=22, sw=2)
        text(x + w / 2, 607, lab, size=20, fill="#ffffff" if on else DIM, anchor="middle",
             weight="600" if on else "normal")
        x += w + 10
    rect(70, 660, 190, 46, "#fffdf5", stroke=DIM, r=23, dash=True, sw=2)
    text(165, 690, "or: exploratory", size=21, fill=DIM, anchor="middle")


def loop_panel():
    head(580, 64, 2, "Every step is recorded")
    cx, cy, R = 925, 440, 235
    rad = [math.radians(a) for a in (-90, -18, 54, 126, 198)]
    for i in range(4):   # wider gaps where an arc would run into a step's label
        g1 = 0.58 if i == 1 else (0.38 if i == 3 else 0.46)
        g2 = 0.58 if i == 3 else (0.38 if i == 1 else 0.46)
        arc(cx, cy, R, rad[i] + g1, rad[i + 1] - g2)
    for a in rad:
        add(f'<line x1="{cx+96*math.cos(a):.0f}" y1="{cy+96*math.sin(a):.0f}" '
            f'x2="{cx+(R-108)*math.cos(a):.0f}" y2="{cy+(R-108)*math.sin(a):.0f}" '
            'stroke="#cfccc2" stroke-width="3" stroke-dasharray="7 7"/>')
    circle(cx, cy, 96, GREY, stroke=CARD_EDGE, sw=2)
    for t in range(4):
        rect(cx - 33 + t * 18, cy - 66, 12, 17, style.NEUTRAL, r=2.5)
    text(cx, cy - 16, "Record", size=30, weight="700", anchor="middle")
    text(cx, cy + 10, "settings · version", size=17, fill=DIM, anchor="middle")
    text(cx, cy + 30, "who · when · design", size=17, fill=DIM, anchor="middle")
    text(cx, cy + 54, "↻ chains replay it", size=16, weight="700", fill=DIM, anchor="middle")
    steps = [("Images", pic_images, 1.12, 22, 20, OK), ("Segment", pic_segment, 1.25, 15, 25, OK),
             ("Populations", pic_cluster, 1.45, 18, 22, OK), ("Analyse", pic_lens, 1.35, 20, 26, OK),
             ("Claim check", pic_claim, 1.3, 22, 16, LOOK)]
    sz = 180
    for (lab, fn, k, ox, oy, state), a in zip(steps, rad):
        x, y = cx + R * math.cos(a), cy + R * math.sin(a)
        rect(x - sz / 2, y - sz / 2, sz, sz, "#ffffff", stroke=CARD_EDGE, r=18, sw=2)
        group(x - sz / 2 + ox, y - sz / 2 + oy, k, fn)
        text(x, y + sz / 2 + 34, lab, size=28, weight="700", anchor="middle")
        badge(x + sz / 2 - 4, y - sz / 2 + 4, state)
    path(f"M532,140 C650,140 700,{cy-R} {cx-94},{cy-R}", sw=4)                       # design → images
    clx, cly = cx + R * math.cos(rad[4]), cy + R * math.sin(rad[4])
    path(f"M{clx-20},{cly-92} C{clx-40},250 600,250 534,250", dash=True, sw=3.5)    # claim → design
    text(548, 232, "checked against the design", size=19, fill=DIM, italic=True)


def side_panel():
    sx = 1262
    text(sx, 64, "Sidekicks", size=30, weight="700")
    text(sx, 96, "optional · read the same record", size=20, fill=DIM, italic=True)
    for i, (title, u1, u2, acc, fig) in enumerate([
            ("Kiwi", "Answers you can", "check, in the app", KIWI, KIWI_FIG),
            ("Claude Code", "In a terminal —", "it builds for you", CLAUDE, CC_FIG)]):   # claudeOverview.ts
        y0 = 190 + i * 220
        rect(sx, y0, 378, 196, "#ffffff", stroke=CARD_EDGE, r=16, sw=2)
        add(f'<rect x="{sx}" y="{y0}" width="378" height="6" rx="3" fill="{acc}"/>')
        add(f'<g transform="translate({sx+16},{y0+34}) scale(1.3)" fill="none" stroke-width="1.6" '
            f'stroke-linecap="round">' + fig.replace("ACC", acc) + '</g>')
        text(sx + 158, y0 + 70, title, size=28, weight="700")
        text(sx + 158, y0 + 104, u1, size=20, fill=DIM)
        text(sx + 158, y0 + 130, u2, size=20, fill=DIM)
    ly = 670
    text(sx, ly, "Checks", size=28, weight="700")
    badge(sx + 16, ly + 38, OK, 15)
    text(sx + 42, ly + 46, "fine", size=22, fill=DIM)
    badge(sx + 136, ly + 38, LOOK, 15)
    text(sx + 162, ly + 46, "look here", size=22, fill=DIM)
    text(sx, ly + 84, "warn, never block", size=20, fill=DIM, italic=True)


def tools_panel():
    gy = 836
    head(40, gy, 3, "What you can ask")
    tiles = [("gate", pic_gate, 1.45), ("cluster", pic_cluster, 1.45), ("track", pic_tracks, 1.45),
             ("lens", pic_lens, 1.4), ("regions", pic_regions, 1.4), ("per replicate", pic_superplot, 1.4)]
    tw, th = 250, 168
    for i, (lab, fn, k) in enumerate(tiles):
        x = 40 + i * (tw + 20)
        rect(x, gy + 28, tw, th, "#ffffff", stroke=CARD_EDGE, r=16, sw=2)
        group(x + (tw - 100 * k) / 2, gy + 28 + (th - 96 * k) / 2, k, fn)
        text(x + tw / 2, gy + th + 64, lab, size=24, weight="700", anchor="middle")


design_panel()
loop_panel()
side_panel()
tools_panel()
svg_head = [f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" width="{W}" height="{H}" '
            f'font-family="{FONT}">',
            '<defs><marker id="ah" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="5" markerHeight="5" '
            f'orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="{DIM}"/></marker></defs>',
            f'<rect width="{W}" height="{H}" fill="{SURFACE}"/>']
with open(OUT, "w", encoding="utf-8") as f:
    f.write("\n".join(svg_head + out + ["</svg>"]) + "\n")
