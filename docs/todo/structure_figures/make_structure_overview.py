"""Cecelia structure overview, in the style of the other README figures (`readme-findings.svg`).

    python3 make_structure_overview.py structure_overview.svg                      # plan copy
    python3 make_structure_overview.py ../../../frontend/public/readme-structure.svg  # README copy

The pipeline, one record chip per step (what that step leaves on record), the chips funnelling into
one Checks band (per-step checks, and a claim from a plot as the worked example), then replay, with
optional AI on top. Each chip names what the code records; evidence per chip is in
CECELIA_STRUCTURE_PLAN.md → *P2. The schematic*. No gaps are open, so both copies are the same figure.
"""
import importlib.util
import sys
from pathlib import Path
from xml.sax.saxutils import escape

# The look every README figure shares (surface, inks, font), loaded by path: this script runs as plain python3.
_spec = importlib.util.spec_from_file_location(
    "figure_style", Path(__file__).resolve().parents[3] / "scripts" / "judge" / "figure_style.py")
style = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(style)

OUT = sys.argv[1]
INK, DIM, FONT = style.INK, style.DIM, style.FONT
PINK, BLUE, OLIVE, GREEN = "#f3e1ec", "#dbeaf3", "#efeecf", "#d4ece3"   # band tints, as in readme-findings.svg

W, MARGIN, BAND_W = 900, 16, 868
CX = W // 2
STEPS = [("Acquire", "microscope"), ("Import", "calibration"), ("Process", "segment · track"),
         ("Populations", "gating"), ("Analyse", "plots, statistics"), ("Export", "figures, tables")]
RECORD = ["microscope settings", "source fingerprint", "parameters, inputs",
          "gate history", "frozen parameters", "record in the file"]
XS = [81, 229, 377, 525, 673, 820]   # step centres
out = []


def _keep_edge_spaces(s):
    """SVG drops a tspan's leading/trailing space; a no-break space survives."""
    s = escape(s)
    if s.startswith(" "):
        s = "&#160;" + s[1:]
    if s.endswith(" "):
        s = s[:-1] + "&#160;"
    return s


def text(x, y, parts, size=16):
    """One centred line; parts are (text, style), style one of 'b' bold, 'dim', 'i' italic, '' plain."""
    attr = {"b": ' font-weight="700"', "dim": f' fill="{DIM}"', "i": ' font-style="italic"', "": ""}
    spans = "".join(f"<tspan{attr[st]}>{_keep_edge_spaces(s)}</tspan>" for s, st in parts)
    out.append(f'<text x="{x}" y="{y}" font-size="{size}" fill="{INK}" text-anchor="middle">{spans}</text>')


def band(y, h, fill, sw=3, dash=None):
    d = f' stroke-dasharray="{dash}"' if dash else ""
    out.append(f'<rect x="{MARGIN}" y="{y}" rx="10" ry="10" width="{BAND_W}" height="{h}" fill="{fill}" '
               f'stroke="{INK}" stroke-width="{sw}"{d}/>')


def arrow(d):
    out.append(f'<path d="{d}" fill="none" stroke="{DIM}" stroke-width="2" marker-end="url(#arrowDim)"/>')


# ── AI, optional
band(16, 50, PINK, sw=2, dash="7 5")
text(CX, 47, [("Optional AI", "b"), (" · reads the same record; everything below works with it off", "dim")])

# ── pipeline
y = 84
for i, (x, (name, sub)) in enumerate(zip(XS, STEPS)):
    out.append(f'<rect x="{x - 65}" y="{y}" rx="10" ry="10" width="{130 if i < 5 else 128}" height="56" '
               f'fill="#ffffff" stroke="{INK}" stroke-width="3"/>')
    text(x, y + 25, [(name, "b")])
    text(x, y + 45, [(sub, "dim")], size=12)
    if i < 5:
        arrow(f"M{x + 67},{y + 28} L{x + 79},{y + 28}")

# ── record: one chip per step
ry, rh = 158, 74
band(ry, rh, BLUE)
text(CX, ry + 24, [("Every step leaves a record", "b"),
                   (" · each run's settings, inputs, version, who and when", "dim")], size=15)
for x, chip in zip(XS, RECORD):
    arrow(f"M{x},{y + 58} L{x},{ry - 2}")
    out.append(f'<rect x="{x - 60}" y="{ry + 38}" rx="7" ry="7" width="120" height="24" fill="#ffffff" '
               f'stroke="{INK}" stroke-width="1.5"/>')
    text(x, ry + 54, [(chip, "")], size=12)

# ── the record funnels into the checks
cy, ch = ry + rh + 62, 104
for x in XS:
    tx = round(CX + (x - CX) * 0.3)
    arrow(f"M{x},{ry + rh + 2} C{x},{ry + rh + 32} {tx},{cy - 34} {tx},{cy - 6}")
band(cy, ch, OLIVE)
text(CX, cy + 30, [("Checks read the record", "b"), (" · they warn and say what to do, never block", "dim")])
text(CX, cy + 58, [("at each step: ", "dim"),
                   ("saturation · calibration · drift, counts, tracks · gates · statistics", "")], size=14)
text(CX, cy + 84, [("claims from a plot, e.g. ", "dim"), ("“T cells move faster than B cells”", "i"),
                   (": were both processed the same way?", "dim")], size=14)

# ── replay
py = cy + ch + 18
band(py, 50, GREEN)
text(CX, py + 31, [("Chains replay it", "b"), (" · the same steps on new images, or a past run exactly", "dim")])

H = py + 50 + 16
head = [f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {W} {H}" font-family="{escape(FONT)}">',
        '<defs><marker id="arrowDim" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="6" markerHeight="6" '
        f'orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z" fill="{DIM}"/></marker></defs>',
        style.surface(W, H)]
with open(OUT, "w", encoding="utf-8") as f:
    f.write("\n".join(head + out + ["</svg>"]) + "\n")
