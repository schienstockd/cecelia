"""Cecelia structure overview — drawn in the style of docs/todo/recital_figures/.

    python3 make_structure_overview.py structure_overview.svg
    python3 make_structure_overview.py ../../../frontend/public/readme-structure.svg   # the README copy

Pipeline left to right; the layers that make an analysis checkable underneath; AI on top.
Each cell names what exists (code refs in CECELIA_STRUCTURE_AUDIT.md). Orange dashed = gap,
green = added in the 2026-11-07 round.
"""
import sys
from xml.sax.saxutils import escape

OUT = sys.argv[1]
W, H = 1800, 930
BG, INK, GREY, MUTED = "#f9f8f4", "#222", "#666", "#999"
BLUE, PINK, GREEN, ORANGE = "#dde9f2", "#f2e1ea", "#0b7a3e", "#D55E00"
SAND = "#f0f0ee"

LX = 250                 # left edge of the grid
GAP = 18
CW = (W - LX - 40 - 5 * GAP) / 6   # column width

out = [f'<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}" '
       'font-family="Helvetica, Arial, DejaVu Sans, sans-serif">',
       '<defs><marker id="g" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="5" markerHeight="5" '
       'orient="auto"><path d="M0,0 L10,5 L0,10 z" fill="#6b6964"/></marker></defs>',
       f'<rect width="{W}" height="{H}" fill="{BG}"/>']


def t(x, y, s, size=18, fill=INK, anchor="middle", weight="normal"):
    """Text; **word** renders bold."""
    parts, bold, spans = s.split("**"), False, []
    for p in parts:
        if p:
            w = ' font-weight="700"' if bold else ""
            spans.append(f"<tspan{w}>{escape(p)}</tspan>")
        bold = not bold
    fw = f' font-weight="{weight}"' if weight != "normal" else ""
    out.append(f'<text x="{x:.0f}" y="{y:.0f}" font-size="{size}" fill="{fill}" '
               f'text-anchor="{anchor}" xml:space="preserve"{fw}>{"".join(spans)}</text>')


def rect(x, y, w, h, fill="#fff", stroke=INK, sw=2, rx=10, dash=None):
    d = f' stroke-dasharray="{dash}"' if dash else ""
    out.append(f'<rect x="{x:.0f}" y="{y:.0f}" width="{w:.0f}" height="{h:.0f}" rx="{rx}" '
               f'fill="{fill}" stroke="{stroke}" stroke-width="{sw}"{d}/>')


def cx(i):
    return LX + i * (CW + GAP)


def lines(x, y, rows, size=16, step=21):
    """rows: list of (text, kind) with kind in ok|gap|new; centred at x, first baseline y."""
    for k, (s, kind) in enumerate(rows):
        fill = {"ok": INK, "gap": ORANGE, "new": GREEN}[kind]
        t(x, y + k * step, s, size=size, fill=fill)


def cell(x, y, h, rows):
    """A per-stage cell: dashed orange only when everything in it is a gap; rows centred vertically."""
    allgap = all(k == "gap" for _, k in rows)
    rect(x, y, CW, h, fill="#fff", stroke=ORANGE if allgap else INK, sw=2, dash="6 4" if allgap else None)
    step = 22
    lines(x + CW / 2, y + h / 2 + 6 - (len(rows) - 1) * step / 2, rows, step=step)


def layer_label(y, h, name, sub):
    t(32, y + h / 2 - 2, name, size=24, anchor="start", weight="700")
    t(32, y + h / 2 + 22, sub, size=16, fill=GREY, anchor="start")


# ── title
out.append(f'<text x="32" y="58" font-size="32" fill="{INK}"><tspan font-weight="700">Cecelia structure'
           f'</tspan><tspan fill="{GREY}"> · what makes an analysis checkable</tspan></text>')
t(32, 96, "Every step leaves a record, checks read it, chains replay it. AI is optional and reads the same "
          "record", size=20, fill=GREY, anchor="start")
out.append(f'<line x1="32" y1="118" x2="760" y2="118" stroke="#6b6964" stroke-width="2"/>')

# legend (top right)
lgx = 1180
rect(lgx, 36, 22, 16, fill="#fff", sw=2, rx=3); t(lgx + 32, 50, "exists", size=15, fill="#555", anchor="start")
rect(lgx, 62, 22, 16, fill="#fff", stroke=ORANGE, sw=2, rx=3, dash="4 3")
t(lgx + 32, 76, "gap", size=15, fill=ORANGE, anchor="start")
rect(lgx + 150, 36, 22, 16, fill=PINK, sw=2, rx=3); t(lgx + 182, 50, "model involved, optional", size=15,
                                                       fill="#555", anchor="start")
t(lgx + 150 + 11, 77, "■", size=16, fill=GREEN)
t(lgx + 182, 76, "added 2026-11-07", size=15, fill=GREEN, anchor="start")

# ── AI band
y = 140; h = 78
layer_label(y, h, "AI", "optional")
rect(LX, y, W - LX - 40, h, fill=PINK, sw=3, rx=12, dash="8 6")
t(LX + (W - LX - 40) / 2, y + 33, "**Kiwi** and **Claude Code** read the record, QC and checks through the same "
                                   "readers as the app", size=19)
t(LX + (W - LX - 40) / 2, y + 60, "anything they write is stamped (who, model, session) · they design work, "
                                   "you run it · everything below works with AI off", size=16, fill=GREY)

# ── pipeline
y = 250; h = 86
layer_label(y, h, "Pipeline", "what you do")
STAGES = [("Acquire", "microscope settings"), ("Import", "calibration, units"),
          ("Process", "segment · track · correct"), ("Populations", "gating, clusters"),
          ("Analyse", "measures, plots, statistics"), ("Export", "figures, tables")]
for i, (a, b) in enumerate(STAGES):
    rect(cx(i), y, CW, h, fill="#fff", sw=5, rx=12)
    t(cx(i) + CW / 2, y + 38, a, size=24, weight="700")
    t(cx(i) + CW / 2, y + 66, b, size=16, fill=GREY)
    if i < 5:
        x1 = cx(i) + CW + 2
        out.append(f'<line x1="{x1:.0f}" y1="{y + h / 2:.0f}" x2="{x1 + GAP - 4:.0f}" y2="{y + h / 2:.0f}" '
                   f'stroke="#6b6964" stroke-width="3" marker-end="url(#g)"/>')

# ── scheduler bar (under Import → Export: every task)
y = 352; h = 46
rect(cx(1), y, cx(5) + CW - cx(1), h, fill=BLUE, sw=3, rx=10)
t(cx(1) + (cx(5) + CW - cx(1)) / 2, y + 30,
  "**One scheduler runs every task** — checks the params, logs the run (custom modules too)", size=18)

# ── record
y = 418; h = 116
layer_label(y, h, "Record", "what was done")
rect(LX - 10, y, W - LX - 30, h, fill="none", stroke="#bbb", sw=1.5, rx=14)
REC = [
    [("settings from the file", "ok")],
    [("calibration + units", "ok"), ("source: path only,", "gap"), ("no checksum", "gap")],
    [("run log: params, status", "ok"), ("version · device · who", "new"), ("skipped images + why", "new")],
    [("gate tree, who drew it", "ok"), ("no gate history", "gap"), ("GUI edits not logged", "gap")],
    [("analysis tasks logged", "ok"), ("plots + statistics:", "gap"), ("no record", "gap")],
    [("CSV rows trace back", "ok"), ("PDF / SVG carry", "gap"), ("no record", "gap")],
]
for i, rows in enumerate(REC):
    cell(cx(i), y + 12, h - 24, rows)

# ── checks
y = 552; h = 196
layer_label(y, h, "Checks", "can it be trusted")
rect(LX - 10, y, W - LX - 30, h, fill="none", stroke="#bbb", sw=1.5, rx=14)
QC = [
    [("saturation, photon", "ok"), ("limit, depth profile", "ok")],
    [("calibration, partial", "ok"), ("timepoints, pyramid", "ok")],
    [("QC on every run", "ok"), ("cohort QC per set", "ok"), ("(button only)", "gap")],
    [("no QC on gating", "gap")],
    [("spatial QC", "ok"), ("no QC on statistics", "gap")],
    [("export QC", "ok")],
]
for i, rows in enumerate(QC):
    cell(cx(i), y + 12, 90, rows)
t(LX + 4, y + 124, "quality checks at each step · warn, never block", size=15, fill=GREY, anchor="start")
# claim checks bar
rect(cx(1), y + 136, cx(5) + CW - cx(1), 48, fill=BLUE, sw=3, rx=10)
t(cx(1) + (cx(5) + CW - cx(1)) / 2, y + 167,
  "**Claim checks** (rules, no model): record complete? · images treated the same? · right statistical unit?",
  size=17)

# ── replay
y = 766; h = 70
layer_label(y, h, "Replay", "run it again")
rect(cx(1), y + 8, cx(3) + CW - cx(1), h - 16, fill=BLUE, sw=3, rx=10)
t(cx(1) + (cx(3) + CW - cx(1)) / 2, y + 42, "**Chains**: save the steps once, re-run on new images, skip what "
                                            "has not changed", size=17)
rect(cx(4), y + 8, cx(5) + CW - cx(4), h - 16, fill="#fff", stroke=ORANGE, sw=2, dash="6 4")
t(cx(4) + (cx(5) + CW - cx(4)) / 2, y + 42, "re-run from an old run's frozen template", size=17, fill=ORANGE)

# ── footer
t(32, 880, "Gaps cluster under Populations and Analyse: a plotted number traces back to its population, but not "
           "to the gate as it was when plotted.", size=16, fill="#555", anchor="start")
t(32, 906, "v1 2026-11-07, from docs/todo/CECELIA_STRUCTURE_AUDIT.md (code refs per cell). Maturity tiers not "
           "shown: no feature is Tested yet.", size=14, fill=MUTED, anchor="start")

out.append("</svg>")
with open(OUT, "w", encoding="utf-8") as f:
    f.write("\n".join(out))
