"""Gating as pictures — shared by the observer (`server.py`) and the autonomous server.

`gate_plot` and `gate_cells_view` are the same read on both servers: the request (`*_query`), the
reply split into PNG + metadata (`split_png`), and the docstring the tool is registered with. Each
server only supplies its own client (and its own guard) — so the two cannot drift apart. `add_gate`'s
`gate` dict is one contract on both servers too, described once in `GATE_SHAPE_DOC`.
"""
from __future__ import annotations

import base64

GATE_PLOT_DOC = """The gate plot as an image: the cells of population `pop` on `x` × `y` AFTER `transform` (the
space gate coordinates are in), drawn as the app's gating plot draws them — each dot coloured by its
local density — with `pop`'s child gates outlined and named. Axes run from 0 to the whole
segmentation's maximum (grown to fit gate edges near the data; an open-ended threshold's far edge
is clipped at the border), titled with the channel names.
`x`/`y` are gateable columns of segmentation `value_name` (e.g. `mean_intensity_2` = channel index 2;
get_measure_summary lists them, get_image_info lists channel names in index order).
`transform`: {"kind": "linear"} (default) | {"kind": "asinh", "cof": 150} |
{"kind": "log", "floor": 1} | {"kind": "logicle", "T": 4096, "W": 0.5, "M": 4.5, "A": 0}.
Alongside the image: each axis's extent and the gates with their path and coordinates (a
gate reaching past the axes is clipped in the picture, not in the list). The tick labels are RAW
values; gate coordinates and `extent` are TRANSFORMED — with a non-linear transform, read vertices
off gate_histogram's `grid`, not off the ticks."""

GATE_CELLS_VIEW_DOC = """The image with population `pop` marked: one timepoint `t` (default the middle frame), z
max-projected in the saved viewer contrast, with segmentation `value_name`'s outlines — `pop`'s cells
in one colour, the rest of its parent population in another (the reply names both). Outlines are
drawn per z-plane, so a cell spanning several planes shows as stacked rings. `channels`: channel
indices to show (default those visible in the viewer; get_image_info lists names in index order).
`image_version`: which image version is under the outlines (default the active one). Alongside the
image: t, the frame count, and how many cells at t are inside / outside."""

GATE_SHAPE_DOC = """`gate` is in TRANSFORMED coordinates — the space gate_plot reports `extent` and gates in, not its
raw tick labels:
{"kind": "rectangle", "x_channel", "y_channel", "x_transform", "y_transform",
 "x_min", "x_max", "y_min", "y_max"}  or
{"kind": "polygon", "x_channel", "y_channel", "x_transform", "y_transform",
 "vertices": [[x1, y1], [x2, y2], [x3, y3], …]}  (≥ 3 vertices, closed implicitly).
`x_channel`/`y_channel` are gateable columns (e.g. `mean_intensity_2`, `volume_mesh`); each
transform is gate_plot's `transform` for that axis: {"kind": "linear"} (default) |
{"kind": "asinh", "cof": 150} | {"kind": "log", "floor": 1} |
{"kind": "logicle", "T": 4096, "W": 0.5, "M": 4.5, "A": 0}. A gate listed by gate_plot has the
same geometry fields — add the channels and transforms you plotted with. A one-channel threshold
is a rectangle spanning the whole other axis; write its open side at the data's own edge
(gate_histogram's `min`/`max`, or gate_plot's `extent`, a little beyond), not at an arbitrary large
number. A population whose cloud is not axis-aligned is a polygon."""

PLOT_ROUTE = "/api/gating/plot-image"
CELLS_ROUTE = "/api/gating/cells-image"


def with_doc(doc: str):
    """Set a tool's docstring before the server's decorator reads it."""
    def deco(fn):
        fn.__doc__ = doc
        return fn
    return deco


def axis_transform_query(transform: dict | None) -> dict:
    """A gating transform spec → the gating routes' per-axis query keys (`xt`, `xcof`, … for x and y)."""
    q = {}
    for axis in ("x", "y"):
        t = dict(transform or {})
        q[f"{axis}t"] = t.pop("kind", "linear")
        for k, v in t.items():
            q[f"{axis}{k}"] = v
    return q


def plot_query(project_uid: str, image_uid: str, value_name: str, x: str, y: str,
               transform: dict | None, pop: str) -> dict:
    return {"projectUid": project_uid, "imageUid": image_uid, "valueName": value_name,
            "x": x, "y": y, "pop": pop or "root", **axis_transform_query(transform)}


def cells_query(project_uid: str, image_uid: str, value_name: str, pop: str, t: int,
                channels: list[int] | None, image_version: str) -> dict:
    q = {"projectUid": project_uid, "imageUid": image_uid, "valueName": value_name, "pop": pop or "root"}
    if t >= 0:
        q["t"] = t
    if channels:
        q["channels"] = ",".join(str(int(c)) for c in channels)
    if image_version:
        q["imageVersion"] = image_version
    return q


def split_png(reply: dict) -> tuple[bytes, dict]:
    """A picture route's reply → (the PNG bytes, everything else that came with it)."""
    return base64.b64decode(reply.get("png") or ""), {k: v for k, v in reply.items() if k != "png"}
