"""Cecelia structure overview, in the style of the README's earlier framework diagram.

    python3 make_structure_overview.py structure_overview.svg                                 # plan copy: with gaps
    python3 make_structure_overview.py ../../../frontend/public/readme-structure.svg --readme  # README copy

One band per layer (headline + one line), with a chip per pipeline stage saying what exists there
(code refs in CECELIA_STRUCTURE_AUDIT.md). The README copy is the first diagram a new reader sees, so it
shows only what exists. The plan copy adds the gaps in magenta italics.
"""
import sys
from xml.sax.saxutils import escape

OUT = sys.argv[1]
README = "--readme" in sys.argv[2:]
BG, INK, SUB = "#fafaf7", "#2a2a2a", "#6b6b66"
BLUE, OLIVE, PEACH, PINK = "#dbeaf3", "#efeecf", "#fbe7d4", "#f7d0e0"
ACCENT = "#d94a86"

W = 1100
LX, RX, GAP = 118, 16, 12
CW = (W - LX - RX - 5 * GAP) / 6
out = []


def t(x, y, s, size=13, fill=INK, anchor="middle", weight="normal", italic=False):
    """Text; **word** renders bold."""
    spans, bold = [], False
    for p in s.split("**"):
        if p:
            b = ' font-weight="700"' if bold else ""
            spans.append(f"<tspan{b}>{escape(p)}</tspan>")
        bold = not bold
    attrs = (f' font-weight="{weight}"' if weight != "normal" else "") + (' font-style="italic"' if italic else "")
    out.append(f'<text x="{x:.0f}" y="{y:.0f}" font-size="{size}" fill="{fill}" text-anchor="{anchor}" '
               f'xml:space="preserve"{attrs}>{"".join(spans)}</text>')


def rect(x, y, w, h, fill=BG, stroke=INK, sw=1.4, rx=6, dash=None):
    d = f' stroke-dasharray="{dash}"' if dash else ""
    out.append(f'<rect x="{x:.0f}" y="{y:.0f}" width="{w:.0f}" height="{h:.0f}" rx="{rx}" ry="{rx}" '
               f'fill="{fill}" stroke="{stroke}" stroke-width="{sw}"{d}/>')


def cx(i):
    return LX + i * (CW + GAP)


FULL_X, FULL_W = LX, cx(5) + CW - LX


def label(y, h, name, sub):
    t(LX - 18, y + h / 2 - 1, name, size=17, anchor="end", weight="700")
    t(LX - 18, y + h / 2 + 17, sub, size=12, fill=SUB, anchor="end", italic=True)


def band(y, h, fill, title, sub, dash=None):
    rect(FULL_X, y, FULL_W, h, fill=fill, sw=2.2, rx=12, dash=dash)
    t(FULL_X + FULL_W / 2, y + 24, title, size=16, weight="700")
    t(FULL_X + FULL_W / 2, y + 43, sub, size=13, fill=SUB, italic=True)


def shown(rows_per_stage):
    return [[(s, g) for s, g in rows if not (README and g)] for rows in rows_per_stage]


def chips(y, rows_per_stage):
    """One chip per stage, inside a band. Each entry: list of (text, is_gap). README drops gap lines."""
    rows_all = shown(rows_per_stage)
    n = max(len(r) for r in rows_all)
    h = 10 + 16 * n
    for i, rows in enumerate(rows_all):
        if not rows:
            continue
        allgap = all(g for _, g in rows)
        rect(cx(i) + 8, y, CW - 16, h, stroke=ACCENT if allgap else INK, dash="5,4" if allgap else None)
        y0 = y + h / 2 + 4.5 - (len(rows) - 1) * 8
        for k, (s, g) in enumerate(rows):
            t(cx(i) + CW / 2, y0 + k * 16, s, size=12.5, fill=ACCENT if g else INK, italic=g)
    return h


y = 16

# ── AI
h = 56
label(y, h, "AI", "optional")
band(y, h, PINK, "Optional AI assistant",
     "reads the same record · everything below works with AI off", dash="7,5")
y += h + 16

# ── pipeline
h = 56
label(y, h, "Pipeline", "what you do")
STAGES = [("Acquire", "microscope"), ("Import", "calibration"),
          ("Process", "segment · track"), ("Populations", "gating"),
          ("Analyse", "plots, statistics"), ("Export", "figures, tables")]
for i, (a, b) in enumerate(STAGES):
    rect(cx(i), y, CW, h, fill=BG, sw=2.2, rx=12)
    t(cx(i) + CW / 2, y + 24, a, size=16, weight="700")
    t(cx(i) + CW / 2, y + 43, b, size=12, fill=SUB)
    if i < 5:
        x1 = cx(i) + CW
        out.append(f'<line x1="{x1:.0f}" y1="{y + h / 2:.0f}" x2="{x1 + GAP - 1:.0f}" y2="{y + h / 2:.0f}" '
                   f'stroke="{ACCENT}" stroke-width="2.2" marker-end="url(#arr)"/>')
pipe_bottom = y + h
y += h + 16

# ── record
REC = [
    [("file settings", False)],
    [("calibration", False), ("no checksum", True)],
    [("run log", False)],
    [("gates + author", False), ("no history", True)],
    [("task log", False), ("plots: none", True)],
    [("CSV trace", False), ("PDF: none", True)],
]
n = max(len(r) for r in shown(REC))
h = 56 + 10 + 16 * n + 12
label(y, h, "Record", "what was done")
band(y, h, BLUE, "Every task leaves a record",
     "parameters, version, who and when")
chips(y + 56, REC)
y += h + 16

# ── checks
QC = [
    [("saturation", False)],
    [("calibration", False)],
    [("per run + per set", False)],
    [("gates compared", False), ("no QC", True)],
    [("spatial QC", False), ("stats: no QC", True)],
    [("export QC", False)],
]
n = max(len(r) for r in shown(QC))
h = 56 + 10 + 16 * n + 12
label(y, h, "Checks", "can it be trusted")
band(y, h, OLIVE, "Checks read the record",
     "quality checks at each step · claim checks · warn, never block")
chips(y + 56, QC)
y += h + 16

# ── replay
h = 56
label(y, h, "Replay", "run it again")
band(y, h, PEACH, "Chains replay it",
     "re-run the same steps on new images"
     + ("" if README else " · not from old templates"))
y += h + 26

# ── tagline
t(FULL_X + FULL_W / 2, y + 8, "Analysis you can hand off and check", size=20, weight="700")
t(FULL_X + FULL_W / 2, y + 32, "Structure first. AI is an optional layer on top.", size=14, fill=SUB,
  italic=True)
y += 46
if not README:
    t(FULL_X, y + 8, "magenta italics = gap (docs/todo/CECELIA_STRUCTURE_PLAN.md)", size=12, fill=ACCENT,
      anchor="start", italic=True)
    y += 22

H = y + 8
# dotted stage guides from the pipeline down through the layers, drawn first (behind the bands)
guides = "".join(f'<line x1="{cx(i) + CW / 2:.0f}" y1="{pipe_bottom:.0f}" x2="{cx(i) + CW / 2:.0f}" '
                 f'y2="{H - 120:.0f}" stroke="{SUB}" stroke-width="1" stroke-dasharray="2,4" opacity="0.6"/>'
                 for i in range(6))
head = [f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H:.0f}" '
        "font-family=\"Georgia, 'Times New Roman', serif\">",
        '<defs><marker id="arr" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="7" markerHeight="7" '
        f'orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="{ACCENT}"/></marker></defs>',
        f'<rect width="{W}" height="{H:.0f}" fill="{BG}"/>', guides]
with open(OUT, "w", encoding="utf-8") as f:
    f.write("\n".join(head + out + ["</svg>"]))
