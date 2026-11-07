"""Cecelia structure overview, in the style of the README's earlier framework diagram.

    python3 make_structure_overview.py structure_overview.svg
    python3 make_structure_overview.py ../../../frontend/public/readme-structure.svg   # the README copy

Pipeline left to right; the layers that make an analysis checkable underneath; AI on top.
Each cell names what exists (code refs in CECELIA_STRUCTURE_AUDIT.md). Magenta dashed = gap,
green = added in the 2026-11-07 round.
"""
import sys
from xml.sax.saxutils import escape

OUT = sys.argv[1]
W, H = 1600, 530
BG, INK, SUB = "#fafaf7", "#2a2a2a", "#6b6b66"
BLUE, OLIVE, PEACH, PINK = "#dbeaf3", "#efeecf", "#fbe7d4", "#f7d0e0"
ACCENT, NEW = "#d94a86", "#3a7d44"

LX, RX, GAP = 140, 20, 16
CW = (W - LX - RX - 5 * GAP) / 6

out = [f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" '
       "font-family=\"Georgia, 'Times New Roman', serif\">",
       '<defs><marker id="arr" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="7" markerHeight="7" '
       f'orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="{ACCENT}"/></marker></defs>',
       f'<rect width="{W}" height="{H}" fill="{BG}"/>']


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


def span(i, j):
    """x and width covering columns i..j."""
    return cx(i), cx(j) + CW - cx(i)


def label(y, h, name, sub):
    t(20, y + h / 2 - 1, name, size=17, anchor="start", weight="700")
    t(20, y + h / 2 + 17, sub, size=12, fill=SUB, anchor="start", italic=True)


def cell(x, y, h, rows):
    """A per-stage chip: dashed accent only when all of it is a gap; rows centred vertically."""
    allgap = all(k == "gap" for _, k in rows)
    rect(x, y, CW, h, stroke=ACCENT if allgap else INK, dash="5,4" if allgap else None)
    step = 16
    y0 = y + h / 2 + 4.5 - (len(rows) - 1) * step / 2
    for k, (s, kind) in enumerate(rows):
        t(x + CW / 2, y0 + k * step, s, size=12.5,
          fill={"ok": INK, "gap": ACCENT, "new": NEW}[kind], italic=(kind == "gap"))


def bar(i, j, y, h, text, fill, sw=2.2, size=14, dash=None, stroke=INK, fill_text=INK):
    x, w = span(i, j)
    rect(x, y, w, h, fill=fill, sw=sw, rx=10, dash=dash, stroke=stroke)
    t(x + w / 2, y + h / 2 + 5, text, size=size, fill=fill_text)


# ── AI (optional, on top)
y = 18; h = 50
label(y, h, "AI", "optional")
x, w = span(0, 5)
rect(x, y, w, h, fill=PINK, sw=2.2, rx=12, dash="7,5")
t(x + w / 2, y + 21, "**Kiwi** and **Claude Code** read the record, QC and checks through the same readers "
                     "as the app", size=14)
t(x + w / 2, y + 39, "anything they write is stamped (who, model, session) · they design work, you run it · "
                     "everything below works with AI off", size=12, fill=SUB, italic=True)

# ── pipeline
y = 86; h = 56
label(y, h, "Pipeline", "what you do")
STAGES = [("Acquire", "microscope settings", BLUE), ("Import", "calibration · units", OLIVE),
          ("Process", "segment · track · correct", PEACH), ("Populations", "gating · clusters", BLUE),
          ("Analyse", "measures · plots · statistics", OLIVE), ("Export", "figures · tables", PEACH)]
for i, (a, b, f) in enumerate(STAGES):
    rect(cx(i), y, CW, h, fill=f, sw=2.2, rx=12)
    t(cx(i) + CW / 2, y + 24, a, size=16, weight="700")
    t(cx(i) + CW / 2, y + 43, b, size=12, fill=SUB)
    if i < 5:
        x1 = cx(i) + CW
        out.append(f'<line x1="{x1:.0f}" y1="{y + h / 2:.0f}" x2="{x1 + GAP - 1:.0f}" y2="{y + h / 2:.0f}" '
                   f'stroke="{ACCENT}" stroke-width="2.2" marker-end="url(#arr)"/>')

# ── scheduler
y = 152; h = 30
bar(1, 5, y, h, "**One scheduler runs every task** · checks the params, logs the run (custom modules too)",
    OLIVE, size=13.5)

# ── record
y = 198; h = 66
label(y, h, "Record", "what was done")
REC = [
    [("settings from the file", "ok")],
    [("calibration + units", "ok"), ("source: path only, no checksum", "gap")],
    [("run log: params, status", "ok"), ("version · device · who", "new"), ("skipped images + why", "new")],
    [("gate tree, who drew it", "ok"), ("no gate history", "gap"), ("GUI edits not logged", "gap")],
    [("analysis tasks logged", "ok"), ("plots + statistics: no record", "gap")],
    [("CSV rows trace back", "ok"), ("PDF / SVG carry no record", "gap")],
]
for i, rows in enumerate(REC):
    cell(cx(i), y, h, rows)

# ── checks
y = 280; h = 50
label(y + 14, h, "Checks", "can it be trusted")
QC = [
    [("saturation · photon limit", "ok"), ("depth profile", "ok")],
    [("calibration · partial", "ok"), ("timepoints · pyramid", "ok")],
    [("QC on every run", "ok"), ("cohort QC per set: button only", "gap")],
    [("no QC on gating", "gap")],
    [("spatial QC", "ok"), ("no QC on statistics", "gap")],
    [("export QC", "ok")],
]
for i, rows in enumerate(QC):
    cell(cx(i), y, h, rows)
bar(1, 5, y + 58, 30, "**Claim checks** (rules, no model): record complete? · images treated the same? · "
                      "right statistical unit?", BLUE, size=13.5)
t(cx(0) + CW / 2, y + 78, "QC warns, never blocks", size=12, fill=SUB, italic=True)

# ── replay
y = 388; h = 34
label(y - 2, h, "Replay", "run it again")
bar(1, 3, y, h, "**Chains**: save the steps once, re-run on new images, skip what has not changed", PEACH,
    size=13.5)
bar(4, 5, y, h, "re-run from an old run's frozen template", BG, sw=1.4, size=13, dash="5,4",
    stroke=ACCENT, fill_text=ACCENT)

# ── legend + footnote
y = 446
rect(LX, y, 26, 16, sw=1.4, rx=4); t(LX + 34, y + 13, "exists", size=12.5, anchor="start")
rect(LX + 100, y, 26, 16, stroke=ACCENT, sw=1.4, rx=4, dash="5,4")
t(LX + 134, y + 13, "gap", size=12.5, fill=ACCENT, anchor="start", italic=True)
t(LX + 190, y + 13, "added 2026-11-07", size=12.5, fill=NEW, anchor="start")
rect(LX + 320, y, 26, 16, fill=PINK, sw=1.4, rx=4, dash="5,3")
t(LX + 354, y + 13, "model involved, optional", size=12.5, anchor="start")
t(LX, y + 44, "A plotted number traces back to its population, but not yet to the gate as it was when plotted.",
  size=13, fill=SUB, anchor="start", italic=True)
t(LX, y + 66, "From docs/todo/CECELIA_STRUCTURE_AUDIT.md (code refs per cell). Maturity tiers not shown: no "
              "feature is Tested yet.", size=11.5, fill="#999", anchor="start")

out.append("</svg>")
with open(OUT, "w", encoding="utf-8") as f:
    f.write("\n".join(out))
