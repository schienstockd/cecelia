"""Gating for Claude — ONE toolset, registered by both the observer (`server.py`) and the autonomous
server (`autonomous_server.py`).

Every gating tool lives here once: its docstring, its request, and its registration
(`register_gating_tools`). `GatingClient` is the request side; each server's client mixes it in and
supplies only its own guard — the observer none beyond its allow-list, the autonomous server its project
lock, its output-name prefix and the measured-segmentation check (`_gating_guard`). `GATING_ROUTES` is
the routes the toolset needs, which both allow-lists include, so the two servers cannot drift apart.

Writes go ONLY through the Claude-scoped routes — `/api/gating/pop/add-gate`, `set-own-gate`,
`delete-own` — which change nothing the user drew (the server checks every population's `createdBy`).
The general `/api/gating/pop/*` routes (add / set-gate / delete / rename / move) stay GUI-only.
"""
from __future__ import annotations

import base64
import json

GATE_PLOT_DOC = """The gate plot as an image: the cells of population `pop` on `x` × `y` AFTER `transform` (the
space gate coordinates are in), drawn as the app's gating plot draws them — each dot coloured by its
local density — with `pop`'s child gates outlined and named. Axes run from 0 to the whole
segmentation's maximum (grown to fit gate edges near the data; an open-ended threshold's far edge
is clipped at the border), titled with the channel names.
`x`/`y` are gateable columns of segmentation `value_name` (e.g. `mean_intensity_2` = channel index 2;
get_measure_summary lists them, get_image_info lists channel names in index order).
`transform`: {"kind": "linear"} (default) | {"kind": "asinh", "cof": 150} |
{"kind": "log", "floor": 1} | {"kind": "logicle", "T": 4096, "W": 0.5, "M": 4.5, "A": 0}; a logicle or
asinh sent without its parameters takes the channel's own (logicle top = its max — the reply's
`transform` says which). Alongside the image: each axis's extent and the gates with their path and coordinates (a
gate reaching past the axes is clipped in the picture, not in the list). The tick labels are RAW
values; gate coordinates and `extent` are TRANSFORMED — with a non-linear transform, read vertices
off gate_histogram's `grid`, not off the ticks."""

GATE_CELLS_VIEW_DOC = """The image with populations marked — the check of a gate where it matters, on the cells.
One timepoint `t` (default the middle frame) in the saved viewer contrast, with segmentation
`value_name`'s outlines. Two looks:
  - `pop`: its cells in one colour, the rest of its parent population in another (the reply names
    both) — what the gate took and what it left.
  - `pops` (a list of paths): each population's cells in ITS OWN colour, nothing else outlined — the
    viewer's look of a tree. List a parent before its children (later ones draw over earlier).
`z`: [plane] or [lo, hi] (their max) — default the whole stack's max, which piles every plane's
outlines on top of each other; give a few planes round the cells you are judging. `region`:
[x0, y0, x1, y1] in image pixels — zoom in; a whole frame is too small to judge an outline by.
`channels`: channel indices to show (default those visible in the viewer; get_image_info lists names
in index order). `image_version`: which image version is under the outlines (default the active one).
Alongside the image: t, the frame count, and the cells at t per population."""

GATE_EXAMPLES_DOC = """Example cells to JUDGE a gate by — a sheet of crops, each cell at its own t, centred,
z ± `z_half` planes max-projected: the raw crop, then the same crop with that one cell outlined.
`mode="boundary"` (default): the `n` cells just INSIDE the gate's edge (top row, green bar) and the `n`
just OUTSIDE it in the parent (bottom row, magenta bar) — the cells the cut decides, which is where a
gate is right or wrong. `mode="random"`: `n` random cells of the population (are they what the name
says?). Give `pop`, or `parent` + `gate` (add_gate's shape) to look at a gate BEFORE writing it.
`n` ≤ 8; `side_um` the crop side in µm (default 15); `channels` as gate_cells_view.
The reply's `tiles` list each crop in sheet order: row, col, side, label id, t, z, its position on
the gate's axes and its distance to the edge (transformed space, axes scaled to the parent's range)."""

GATE_HISTOGRAM_DOC = """The distribution to choose a gate from: per axis, quantiles + an even-width count table of
segmentation `value_name`'s cells (inside population `pop`), AFTER `transform` — the same space gate
coordinates are in. `x`/`y` are gateable columns (e.g. `mean_intensity_2` = channel index 2); `y`
defaults to `x`. With a `y`, also `grid`: the joint x × y counts on a 20 × 20 grid (`counts[j][i]` =
y-bin j low → high, x-bin i; edges in gate coordinates) — the shape of the cloud, to trace a polygon
round a population or put a cut in a valley. Which columns exist depends on the measure task (mesh
measures such as `volume_mesh` only come from the mesh-measuring tasks); get_image_info lists channel
names in index order.
`transform`: {"kind": "linear"} (default) | {"kind": "asinh", "cof": 150} |
{"kind": "log", "floor": 1} | {"kind": "logicle", "T": 4096, "W": 0.5, "M": 4.5, "A": 0}; a logicle or
asinh sent without its parameters takes the channel's own (logicle top = its max), as gate_plot draws it."""

GATE_STATS_DOC = """Cells in population `pop` and its % of the parent. `pop` may be a gate (`/qc`) or a
tracked set (`/qc/_tracked`, `/_tracked`) — the same paths a task takes as input."""

GATE_SHAPE_DOC = """`gate` is in TRANSFORMED coordinates — the space gate_plot reports `extent` and gates in, not its
raw tick labels:
{"kind": "rectangle", "x_channel", "y_channel", "x_transform", "y_transform",
 "x_min", "x_max", "y_min", "y_max"}  or
{"kind": "polygon", "x_channel", "y_channel", "x_transform", "y_transform",
 "vertices": [[x1, y1], [x2, y2], [x3, y3], …]}  (≥ 3 vertices, closed implicitly).
`x_channel`/`y_channel` are gateable columns (e.g. `mean_intensity_2`, `volume_mesh`); each
transform is gate_plot's `transform` for that axis: {"kind": "linear"} (default) |
{"kind": "asinh", "cof": 150} | {"kind": "log", "floor": 1} |
{"kind": "logicle", "T": 4096, "W": 0.5, "M": 4.5, "A": 0}; a logicle or asinh sent without its
parameters takes the channel's own (logicle top = its max), as gate_plot draws it. A gate listed by gate_plot has the
same geometry fields — add the channels and transforms you plotted with. A one-channel threshold
is a rectangle spanning the whole other axis; write its open side at the data's own edge
(gate_histogram's `min`/`max`, or gate_plot's `extent`, a little beyond), not at an arbitrary large
number. A population whose cloud is not axis-aligned is a polygon."""

ADD_GATE_DOC = """ADD one gated population — a PROPOSED gate the user reviews in the population manager.
It lands as a new child of `parent`, marked as yours (✦); the user deletes or undoes it in one click.
409 if the name is taken under that parent or by another population type (pick another name;
never try to replace theirs); 404 if `parent` does not exist; 422 on a column, transform or
geometry that cannot be a gate — the message says which. Only when the user asked for a gate or
agreed to one.

""" + GATE_SHAPE_DOC + """
`value_name` is the segmentation, exactly; `parent` an existing path as gate_plot's `pop` takes it
("root", "/qc"); `name` plain, no "/" and not starting with "_" — you cannot rename it afterwards.

Place it from gate_histogram's `grid` / gate_plot on `parent` with the SAME transform. Before writing,
`dry_run=True` answers with the counts and writes nothing, and gate_examples with `parent` + `gate`
shows the cells the cut decides. The reply carries the stored `path`, `count` (inside) and `outside`
over `parentCount`, and the `gate` as stored. Tell the user it is a proposal, with the numbers."""

SET_GATE_DOC = """MOVE a gate YOU drew (✦) — same `gate` shape as add_gate; `path` the population.
403 `not_claude_made` when it, or anything gated under it, is the user's — including one of yours they
have since edited; 403 `used_by_runs` when a task (clustering, tracking, …) read it or anything under
it, naming the tasks. Only the user changes those (propose a new gate instead). `dry_run=True` answers with the new counts and writes nothing. The
reply is add_gate's."""

DELETE_GATE_DOC = """REMOVE a gate YOU drew (✦), with everything under it — every one of those must be yours
too, else 403 `not_claude_made` naming the user's; 403 `used_by_runs` when a task read one of them;
409 when a combined (boolean) population of the user's reads it. `dry_run=True` lists what would go and removes nothing. The user can undo it."""

PLOT_ROUTE = "/api/gating/plot-image"
CELLS_ROUTE = "/api/gating/cells-image"
EXAMPLES_ROUTE = "/api/gating/examples"
SUMMARY_ROUTE = "/api/gating/summary"
STATS_ROUTE = "/api/gating/stats"
ADD_ROUTE = "/api/gating/pop/add-gate"
SET_ROUTE = "/api/gating/pop/set-own-gate"
DELETE_ROUTE = "/api/gating/pop/delete-own"

# What the toolset calls — both servers' allow-lists include exactly this set. The three POSTs are
# the Claude-scoped writes (create-only, and change/remove only what Claude drew).
GATING_READ_ROUTES = frozenset({("GET", PLOT_ROUTE), ("GET", CELLS_ROUTE), ("GET", SUMMARY_ROUTE),
                                ("GET", STATS_ROUTE), ("GET", EXAMPLES_ROUTE)})
GATING_WRITE_ROUTES = frozenset({("POST", ADD_ROUTE), ("POST", SET_ROUTE), ("POST", DELETE_ROUTE)})
GATING_ROUTES = GATING_READ_ROUTES | GATING_WRITE_ROUTES


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
                channels: list[int] | None, image_version: str, pops: list[str] | None = None,
                z: list[int] | None = None, region: list[int] | None = None) -> dict:
    q = {"projectUid": project_uid, "imageUid": image_uid, "valueName": value_name, "pop": pop or "root"}
    if t >= 0:
        q["t"] = t
    if channels:
        q["channels"] = ",".join(str(int(c)) for c in channels)
    if image_version:
        q["imageVersion"] = image_version
    if pops:
        q["pops"] = ",".join(pops)
    if z:
        q["z"] = "-".join(str(int(v)) for v in z[:2])
    if region:
        q["region"] = ",".join(str(int(v)) for v in region)
    return q


def split_png(reply: dict) -> tuple[bytes, dict]:
    """A picture route's reply → (the PNG bytes, everything else that came with it)."""
    return base64.b64decode(reply.get("png") or ""), {k: v for k, v in reply.items() if k != "png"}


class GatingClient:
    """The toolset's requests. Mixed into each server's client, which provides `_request` and may
    override `_gating_guard` — called before every request with `write` saying whether it changes a
    population."""

    def _gating_guard(self, project_uid: str, image_uid: str, value_name: str, write: bool) -> None:
        pass

    def _ids(self, project_uid: str, image_uid: str, value_name: str) -> dict:
        return {"projectUid": project_uid, "imageUid": image_uid, "valueName": value_name}

    def gate_histogram(self, project_uid: str, image_uid: str, value_name: str, x: str, y: str,
                       transform: dict | None, pop: str, bins: int) -> dict:
        self._gating_guard(project_uid, image_uid, value_name, False)
        q = {**plot_query(project_uid, image_uid, value_name, x, y, transform, pop), "bins": bins}
        return self._request("GET", SUMMARY_ROUTE, q)

    def gate_plot(self, project_uid: str, image_uid: str, value_name: str, x: str, y: str,
                  transform: dict | None, pop: str) -> tuple[bytes, dict]:
        self._gating_guard(project_uid, image_uid, value_name, False)
        return split_png(self._request("GET", PLOT_ROUTE, plot_query(
            project_uid, image_uid, value_name, x, y, transform, pop)))

    def gate_cells_view(self, project_uid: str, image_uid: str, value_name: str, pop: str, t: int,
                        channels: list[int] | None, image_version: str, pops: list[str] | None = None,
                        z: list[int] | None = None, region: list[int] | None = None) -> tuple[bytes, dict]:
        self._gating_guard(project_uid, image_uid, value_name, False)
        return split_png(self._request("GET", CELLS_ROUTE, cells_query(
            project_uid, image_uid, value_name, pop, t, channels, image_version, pops, z, region)))

    def gate_examples(self, project_uid: str, image_uid: str, value_name: str, pop: str = "",
                      mode: str = "boundary", n: int = 6, parent: str = "", gate: dict | None = None,
                      side_um: float = 15.0, z_half: int = 2, channels: list[int] | None = None,
                      seed: int = 1) -> tuple[bytes, dict]:
        self._gating_guard(project_uid, image_uid, value_name, False)
        q = {**self._ids(project_uid, image_uid, value_name), "mode": mode, "n": n,
             "sideUm": side_um, "zHalf": z_half, "seed": seed}
        if pop:
            q["pop"] = pop
        else:   # a gate not written yet: add_gate's `gate`, as JSON text
            q.update(parent=parent or "root", gate=json.dumps(gate or {}))
        if channels:
            q["channels"] = ",".join(str(int(c)) for c in channels)
        return split_png(self._request("GET", EXAMPLES_ROUTE, q))

    def gate_stats(self, project_uid: str, image_uid: str, value_name: str, pop: str) -> dict:
        self._gating_guard(project_uid, image_uid, value_name, False)
        return self._request("GET", STATS_ROUTE, {**self._ids(project_uid, image_uid, value_name), "pop": pop})

    def add_gate(self, project_uid: str, image_uid: str, value_name: str, name: str, gate: dict,
                 parent: str = "root", colour: str = "#22c55e", dry_run: bool = False) -> dict:
        # Create-only: ONE new gated population under an existing parent (409 on a taken name, 404 on a
        # missing parent, 422 on a column / transform / geometry that cannot be a gate). `gate` goes
        # out untouched — the stored `gate_spec` shape; the server validates every field of it.
        self._gating_guard(project_uid, image_uid, value_name, True)
        body = {**self._ids(project_uid, image_uid, value_name), "parent": parent or "root",
                "name": name, "gate": gate, "dryRun": bool(dry_run)}
        if colour:
            body["colour"] = colour
        return self._request("POST", ADD_ROUTE, body=body)

    def set_gate(self, project_uid: str, image_uid: str, value_name: str, path: str, gate: dict,
                 dry_run: bool = False) -> dict:
        self._gating_guard(project_uid, image_uid, value_name, True)
        return self._request("POST", SET_ROUTE, body={**self._ids(project_uid, image_uid, value_name),
                                                      "path": path, "gate": gate, "dryRun": bool(dry_run)})

    def delete_gate(self, project_uid: str, image_uid: str, value_name: str, path: str,
                    dry_run: bool = False) -> dict:
        self._gating_guard(project_uid, image_uid, value_name, True)
        return self._request("POST", DELETE_ROUTE, body={**self._ids(project_uid, image_uid, value_name),
                                                         "path": path, "dryRun": bool(dry_run)})


def register_gating_tools(tool, client: GatingClient) -> dict:
    """Register the gating toolset on a server: `tool` is its tool decorator (error handling included),
    `client` its `GatingClient`. Returns {name: function} — the same names and signatures on both."""
    from mcp.server.mcpserver import Image   # the MCP SDK only where a server registers tools

    def picture(png: bytes, meta: dict) -> list:
        return [Image(data=png, format="png"), meta]

    @tool
    @with_doc(GATE_HISTOGRAM_DOC)
    def gate_histogram(project_uid: str, image_uid: str, value_name: str, x: str, y: str = "",
                       transform: dict | None = None, pop: str = "root", bins: int = 30) -> dict:
        return client.gate_histogram(project_uid, image_uid, value_name, x, y or x, transform, pop,
                                     max(5, min(bins, 80)))

    @tool
    @with_doc(GATE_PLOT_DOC)
    def gate_plot(project_uid: str, image_uid: str, value_name: str, x: str, y: str,
                  transform: dict | None = None, pop: str = "root") -> list:
        return picture(*client.gate_plot(project_uid, image_uid, value_name, x, y, transform, pop))

    @tool
    @with_doc(GATE_CELLS_VIEW_DOC)
    def gate_cells_view(project_uid: str, image_uid: str, value_name: str, pop: str = "root", t: int = -1,
                        channels: list[int] | None = None, image_version: str = "",
                        pops: list[str] | None = None, z: list[int] | None = None,
                        region: list[int] | None = None) -> list:
        return picture(*client.gate_cells_view(project_uid, image_uid, value_name, pop, t, channels,
                                               image_version, pops, z, region))

    @tool
    @with_doc(GATE_EXAMPLES_DOC)
    def gate_examples(project_uid: str, image_uid: str, value_name: str, pop: str = "",
                      mode: str = "boundary", n: int = 6, parent: str = "", gate: dict | None = None,
                      side_um: float = 15.0, z_half: int = 2, channels: list[int] | None = None) -> list:
        return picture(*client.gate_examples(project_uid, image_uid, value_name, pop, mode, n, parent, gate,
                                             side_um, z_half, channels))

    @tool
    @with_doc(GATE_STATS_DOC)
    def gate_stats(project_uid: str, image_uid: str, value_name: str, pop: str) -> dict:
        return client.gate_stats(project_uid, image_uid, value_name, pop)

    @tool
    @with_doc(ADD_GATE_DOC)
    def add_gate(project_uid: str, image_uid: str, value_name: str, name: str, gate: dict,
                 parent: str = "root", colour: str = "#22c55e", dry_run: bool = False) -> dict:
        return client.add_gate(project_uid, image_uid, value_name, name, gate, parent, colour, dry_run)

    @tool
    @with_doc(SET_GATE_DOC)
    def set_gate(project_uid: str, image_uid: str, value_name: str, path: str, gate: dict,
                 dry_run: bool = False) -> dict:
        return client.set_gate(project_uid, image_uid, value_name, path, gate, dry_run)

    @tool
    @with_doc(DELETE_GATE_DOC)
    def delete_gate(project_uid: str, image_uid: str, value_name: str, path: str,
                    dry_run: bool = False) -> dict:
        return client.delete_gate(project_uid, image_uid, value_name, path, dry_run)

    return {f.__name__: f for f in (gate_histogram, gate_plot, gate_cells_view, gate_examples, gate_stats,
                                    add_gate, set_gate, delete_gate)}
