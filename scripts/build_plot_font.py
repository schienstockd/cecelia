"""Build `api/assets/plot_font.json`: anti-aliased glyph bitmaps the server-side gate plot
(`api/src/gate_plot_render.jl`) draws its tick labels, axis titles and gate names with — the Julia
API has no text rasteriser, and the plot has to read like the browser's.

Glyphs: printable ASCII + µ × –, from DejaVu Sans (Bitstream Vera license — free to embed and
redistribute, see https://dejavu-fonts.github.io/License.html), at TWICE the CSS sizes the browser
plot uses (the raster is drawn at 2× — `_PLOT_SCALE`): tick labels 10px, axis titles 13px semibold
(DejaVu has no semibold → bold), gate names bold 12px (`GateScatterCell.vue`, `GateOverlay.vue`).
Re-run only to change a size or the character set:

    pixi run python scripts/build_plot_font.py /usr/share/fonts/truetype/dejavu/DejaVuSans.ttf \\
        /usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf
"""
from __future__ import annotations

import base64
import json
import pathlib
import sys

from PIL import Image, ImageDraw, ImageFont

OUT = pathlib.Path(__file__).resolve().parents[1] / "api" / "assets" / "plot_font.json"
CHARS = "".join(chr(c) for c in range(32, 127)) + "µ×–"


def face(path: str, px: int) -> dict:
    font = ImageFont.truetype(path, px)
    ascent, descent = font.getmetrics()
    glyphs = {}
    for ch in CHARS:
        left, top, right, bottom = font.getbbox(ch)
        w, h = max(right - left, 0), max(bottom - top, 0)
        g = {"adv": round(font.getlength(ch)), "x": left, "y": top, "w": w, "h": h}
        if w and h:
            im = Image.new("L", (w, h), 0)
            ImageDraw.Draw(im).text((-left, -top), ch, font=font, fill=255)
            g["a"] = base64.b64encode(im.tobytes()).decode("ascii")   # row-major alpha, w*h bytes
        glyphs[ch] = g
    return {"px": px, "ascent": ascent, "descent": descent, "glyphs": glyphs}


def main(regular: str, bold: str) -> None:
    out = {"source": "DejaVu Sans (Bitstream Vera license), built by scripts/build_plot_font.py",
           "faces": {"tick": face(regular, 20), "title": face(bold, 26), "label": face(bold, 24)}}
    OUT.write_text(json.dumps(out, separators=(",", ":"), ensure_ascii=False), encoding="utf-8")
    print(OUT, OUT.stat().st_size, "bytes")


if __name__ == "__main__":
    main(*sys.argv[1:3])
