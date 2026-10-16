"""The look every README figure shares: the hand-drawn ones in `frontend/public/readme-*.svg` and the
two the weekly judge draws (`chart.py`, `flow_chart.py`).

A light paper surface of its own, so a figure reads the same on GitHub's light and dark themes; dark
ink; a dimmer ink for secondary text and axes; a sans-serif stack; and two neutral fills for segments
that mean "nothing more happens" (gone, waiting). Every colour that tells categories apart comes from
the shared CVD-safe palette (`cecelia.effectiveness.palette.load_hex`), never from here.
"""
from __future__ import annotations

SURFACE = "#fafaf7"
INK = "#2a2a2a"
DIM = "#6b6b66"
#: Neutral fills for "no action" segments: done-with (gone, not a bug) and waiting (parked, backlog).
NEUTRAL = "#b9b6ad"
NEUTRAL_LIGHT = "#dedbd2"
FONT = "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif"


def surface(width: int | float, height: int | float) -> str:
    """The background rect every figure starts with."""
    return f'<rect width="{width}" height="{height}" fill="{SURFACE}"/>'
