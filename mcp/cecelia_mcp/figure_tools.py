"""Figures for Claude — ONE toolset, registered by both the observer (`server.py`) and the autonomous
server (`autonomous_server.py`), the way `gating_views.py` is. docs/todo/FIGURES_PLAN.md (P3).

A figure is a still, a filmstrip or a z-strip of one region, rendered by the app from the raw image in
the house style and kept beside the movies. Claude writes a SPEC — what to show — and never rendering
code. `dry_run` renders and keeps nothing: the reply's picture is what Claude looks at before it saves.

Writes are Claude's own only: `set_figure` replaces and `delete_figure` deletes a figure Claude made and
nobody has re-made since; the server refuses anyone else's (403). `add_figure` never overwrites.
"""
from __future__ import annotations

from cecelia_mcp.gating_views import split_png, with_doc

RENDER_ROUTE = "/api/figures/render"
DELETE_ROUTE = "/api/figures/delete-own"
LIST_ROUTE = "/api/movies"
META_ROUTE = "/api/movies/meta"

# What the toolset calls — both servers' allow-lists include exactly this set.
FIGURE_ROUTES = frozenset({("POST", RENDER_ROUTE), ("POST", DELETE_ROUTE), ("GET", LIST_ROUTE),
                           ("GET", META_ROUTE)})

SPEC_DOC = """`spec` (what to show; the image supplies its store, channel names, pixel size and interval):
  - `kind`: "sequence" (one region over `frames`, default) | "zstack" (one `t`, planes `planes`)
  - `center`: [y, x] level-0 pixels — the crop centre; `sizePx`: crop side in level-0 px (square)
  - `z`: centre plane; `zHalf`: planes either side, max-projected (0 = one plane)
  - `frames`: [t, …] (sequence) — at most 64 tiles in all
  - `rows`: [{"channels": [{"c": "mem-TOM", "colour": "#ff00ff", "name": "tdTom"}, …]}, …] — one row of
    tiles per entry; `c` is a channel NAME (get_image_info) or 0-based index, `colour` #rrggbb, `name` the
    legend's (paper) name. A second row with one channel in "#ffffff" shows that channel alone.
  - `valueName`: the image version (default the active one; prefer the denoised version when there is one)
  - `display`: {"percentiles": [1, 99.8]} measured over ALL tiles (default) | {"ranges": {"mem-TOM": [0, 68]}}
    | add "perTile": true for a range per tile (the legend then says so). Shared is what makes frames comparable.
  - `scaleBarUm`: bar length (none if omitted); `interpolation`: "pixels" (default, crisp) | "smooth"
  - `style`: {"tileMm": 20, "dpi": 300, "scaleBarOn": "first"|"last"|"all"} — the house look otherwise.
To find a cell: get_object_ids / pop_df coordinates give y, x, z, t in level-0 pixels."""

ADD_FIGURE_DOC = f"""Make a figure of image `image_uid` from `spec` — rendered by the app at native resolution in the
house style (Cecelia paper Fig. 4a: column titles, coloured legend band, one scale bar) and kept beside the
movies on the Movies & figures page, marked as yours. Returns the figure as a picture (a ≤ 1600 px preview)
and the spec as rendered, with the display ranges it measured — check the channel colours and that the
cell is centred before you report it.
`dry_run=True` renders and keeps NOTHING — look first, then save. `name` is the file name (default
`<image>_figure`); an existing one is never overwritten (a `-2` is added). A spec error names the field.
{SPEC_DOC}"""

SET_FIGURE_DOC = f"""Re-make a figure YOU made (`name`, e.g. "cell_figure.png") from a new `spec` — the same file,
replaced. The user's figures, and yours once the user has re-made them, are refused (403): make a new one
with add_figure instead. `dry_run=True` renders the preview and keeps nothing.
{SPEC_DOC}"""

DELETE_FIGURE_DOC = """Delete a figure YOU made (`name`, e.g. "cell_figure.png"): its file, tiles and entry. The user's
figures, and yours once the user has re-made them, are refused (403). `dry_run=True` says what would go
and removes nothing. The user can delete anything from the Movies & figures page."""

LIST_FIGURES_DOC = """The project's figures: `name`, `imageUid`, `mine` (made by Claude and not re-made by the user —
the ones set_figure / delete_figure may change), its legend channels, and its saved `spec` (the one to
start a set_figure from)."""


class FigureClient:
    """The toolset's requests. Mixed into each server's client, which provides `_request`."""

    def _figure_guard(self, project_uid: str, write: bool) -> None:
        pass

    def render_figure(self, project_uid: str, image_uid: str, spec: dict, name: str = "",
                      replace: bool = False, dry_run: bool = False) -> tuple[bytes, dict]:
        self._figure_guard(project_uid, not dry_run)
        reply = self._request("POST", RENDER_ROUTE, body={
            "projectUid": project_uid, "imageUid": image_uid, "spec": spec, "name": name,
            "replace": bool(replace), "dryRun": bool(dry_run)})
        return split_png(reply)

    def delete_figure(self, project_uid: str, name: str, dry_run: bool = False) -> dict:
        self._figure_guard(project_uid, not dry_run)
        return self._request("POST", DELETE_ROUTE, body={"projectUid": project_uid, "name": name,
                                                         "dryRun": bool(dry_run)})

    def list_figures(self, project_uid: str) -> list[dict]:
        self._figure_guard(project_uid, False)
        rows = (self._request("GET", LIST_ROUTE, {"projectUid": project_uid}) or {}).get("movies", [])
        out = []
        for m in rows:
            if m.get("kind") != "figure":
                continue
            meta = (self._request("GET", META_ROUTE, {"projectUid": project_uid, "name": m["name"]}) or {})
            e = meta.get("entry") or {}
            # `claudeMade` is the server's ownership rule (`claude_made`) — never re-derived here
            out.append({"name": m["name"], "imageUid": m.get("imageUid", ""), "channels": m.get("channels", []),
                        "mine": bool(m.get("claudeMade")), "spec": (e.get("config") or {}).get("spec")})
        return out


def register_figure_tools(tool, client: FigureClient) -> dict:
    """Register the figure toolset on a server: `tool` is its decorator, `client` its `FigureClient`.
    Returns {name: function} — the same names and signatures on both servers."""
    from mcp.server.mcpserver import Image   # the MCP SDK only where a server registers tools

    def picture(png: bytes, meta: dict) -> list:
        return [Image(data=png, format="png"), meta]

    @tool
    @with_doc(ADD_FIGURE_DOC)
    def add_figure(project_uid: str, image_uid: str, spec: dict, name: str = "", dry_run: bool = False) -> list:
        return picture(*client.render_figure(project_uid, image_uid, spec, name, False, dry_run))

    @tool
    @with_doc(SET_FIGURE_DOC)
    def set_figure(project_uid: str, image_uid: str, name: str, spec: dict, dry_run: bool = False) -> list:
        stem = name[:-4] if name.lower().endswith(".png") else name
        return picture(*client.render_figure(project_uid, image_uid, spec, stem, True, dry_run))

    @tool
    @with_doc(DELETE_FIGURE_DOC)
    def delete_figure(project_uid: str, name: str, dry_run: bool = False) -> dict:
        return client.delete_figure(project_uid, name, dry_run)

    @tool
    @with_doc(LIST_FIGURES_DOC)
    def list_figures(project_uid: str) -> list:
        return client.list_figures(project_uid)

    return {"add_figure": add_figure, "set_figure": set_figure, "delete_figure": delete_figure,
            "list_figures": list_figures}
