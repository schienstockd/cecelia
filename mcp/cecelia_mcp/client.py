"""Read-only HTTP client for the Cecelia Julia API, used by the MCP observer server.

Every request goes through an explicit ALLOW-LIST of (method, path) pairs. That list IS the
observer's no-mutation guarantee: the only non-GET routes permitted are ``POST /api/boards/add``
(create-only — adds ONE Analysis board beside the user's own, which they delete in one click; it
cannot modify, rename, reorder or delete a board), ``POST /api/lablog/append``
(append-only), ``POST /api/notebooks/write`` (create-only — 409 on an existing name, so it never
overwrites), ``POST /api/notebooks/describe`` (edits ONLY a notebook's own description string in the
registry sidecar — not its cells), ``POST /api/notebooks/revise`` (which SNAPSHOTS the current
notebook first — a restorable version — then overwrites its cells; it's how a notebook gets a new
version, never a "-v2" copy), ``POST /api/chains/create`` (create-only + server-validated —
writes a chain TEMPLATE the user then runs themselves), and ``POST /api/observer/labarchives/set``
(REPLACES the ELN context sidecar — a cache of an external system of record, so a rewrite loses
nothing). All seven are recoverable / non-destructive to
project & analysis data: no allow-listed route can touch cell data, images, gates, or QC, revise
can't lose a notebook's content (the pre-revision state is always snapshotted), a template is
inert until a human presses Run, and an added board is one tab beside the user's own. Any attempt to call a route not on the list raises
``DisallowedRoute`` — so if a future tool ever wires in a truly destructive route it fails loudly in
tests rather than silently mutating a project.

Note what is deliberately ABSENT: there is no way to *start* a chain run. Launching is a WebSocket
message (``chain:run``) with no HTTP route at all, and this client speaks only HTTP — so "Claude
designs, the user runs" is a property of the transport, not a rule Claude has to remember. Likewise
absent: ``/api/chains/save`` (an unguarded overwrite — that route is the whiteboard saving the user's
own canvas) and ``/api/chains/rename``/``/api/chains/delete`` (renaming or removing the user's chain
is an in-place mutation; both are GUI-only).

Uses only the Python standard library (urllib) so this module — and its tests — carry no third-party
dependency; the ``mcp`` SDK is needed only by ``server.py`` which wires these calls into tools.
"""
from __future__ import annotations

import json
import os
import urllib.error
import urllib.parse
import urllib.request

DEFAULT_BASE_URL = "http://127.0.0.1:8080"

# (method, path) → the ONLY routes the observer may ever call. Read-only except the three writes below.
# Keep this in sync with the backing routes in api/src/routes.jl; the test pins the exact write set.
ALLOWED_ROUTES = frozenset(
    {
        ("POST", "/api/boards/add"),      # WRITE 6/7 — create-only: adds ONE Analysis board, never
                                          # edits/deletes/reorders one. Server-validated against the
                                          # project (unknown plot id, chart the spec doesn't offer, a
                                          # population that doesn't exist → 422 before anything is
                                          # written), 409 on a duplicate name. NOT
                                          # /api/projects/boards, the browser's whole-document autosave
        ("GET", "/api/projects"),
        ("GET", "/api/images"),
        ("GET", "/api/images/meta"),
        ("GET", "/api/objects/find"),    # uid (or name) → WHICH project it lives in. The only read
                                         # that does not start from a projectUid; without it the
                                         # answer to "image p6t4mC" was /api/images per project
                                         # until one matched. Read-only, no lastOpenedAt bump.
        ("GET", "/api/images/tasklog"),
        ("GET", "/api/tasks/history"),
        ("GET", "/api/tasks/definitions"),  # task param specs (valid ranges/defaults/types) for suggestions
        ("GET", "/api/plots/definitions"),  # available plot types (chart types, data needs, scope modes)
        ("GET", "/api/plots/attrs"),     # per-set image ATTRIBUTES (name + distinct values) — the axes a
                                         # board can compare by; the same route the summary canvas and the
                                         # UMAP colour/facet picker use, not a second attribute surface
        ("GET", "/api/qc/cohort"),       # cohort QC: per-set mean/SD + outliers over banked metrics
        ("GET", "/api/analysis/lineage"),  # synthesized pipeline lineage (steps + seg/track/cluster/gating links)
        ("GET", "/api/analysis/populations"),  # population definitions (tree + gate/filter specs)
        ("GET", "/api/analysis/measures"),  # per-pop phenotype + motility summaries (median/quantiles)
        ("GET", "/api/analysis/behaviour"),  # HMM state distribution + transition counts
        ("GET", "/api/analysis/clusters"),  # per clustering run: n clusters, sizes, largest fraction, features
        ("GET", "/api/analysis/spatial"),  # region runs + pairwise cell-type contact log-odds (association/avoidance)
        ("GET", "/api/analysis/chains"),  # whiteboard chains: wired templates (DAG) + recent runs
        ("GET", "/api/analysis/boards"),  # existing Analysis boards + what each slot plots (summary, not layout)
        ("GET", "/api/repl/api"),        # notebook/REPL data-access surface: accessors + docstrings + cookbook
        ("GET", "/api/observer/briefing"),  # session startup context: name/count + flagged images + recent lab log
        ("GET", "/api/observer/labarchives"),  # the ELN context sidecar in full + derived cohort gaps
        ("GET", "/api/logs/recent"),     # the backend console ring (server @info/@warn/@error)
        ("GET", "/api/lablog"),
        ("GET", "/api/notebooks"),         # list a project's notebooks (file, description, version)
        ("GET", "/api/notebooks/content"),  # read a notebook's current source (the "have a look" flow)
        ("GET", "/api/viewer/captures"),   # bidir share-in: newest-first list of what the user shared
        ("GET", "/api/viewer/capture"),    # bidir share-in: one capture envelope + inlined PNG frame
        ("GET", "/api/labels/ids"),        # bidir follow-up: enumerate cell/track ids so mark_cells / mark_tracks stop guessing
        ("GET", "/api/viewer/plots"),      # bidir PR #8 — live plot registry; discover mounted panels so mark_plot's plot_id isn't guessed
        ("GET", "/api/blackboard"),        # bidir Part 4 — list a project's blackboard entries (title + version + updatedAt)
        ("GET", "/api/blackboard/entry"),  # bidir Part 4 — read one entry's Markdown (optionally at a snapshot version)
        # /api/blackboard/search is POST (see below) — a substring query with an optional status filter
        # NB: /api/viewer/capture (POST) is NOT allow-listed. Captures are AUTHORED by the frontend
        # Share button; Claude only READS them. Keeping the write off the observer's surface prevents
        # a fabricated "the user shared this" from ever landing in the project's captures dir.
        ("POST", "/api/lablog/append"),  # write 1/7 — append-only, server-guarded
        ("POST", "/api/notebooks/write"),  # write 2/7 — create-only (409 on existing); serialises cells to a Pluto notebook
        ("POST", "/api/notebooks/describe"),  # write 3/7 — edits ONLY a notebook's description string (registry sidecar); not its content
        ("POST", "/api/notebooks/revise"),  # write 4/7 — SNAPSHOTS the current notebook (restorable), then overwrites its cells (real versioning, no "-v2" copies)
        ("POST", "/api/blackboard/create"),  # bidir Part 4 — new Markdown entry (title + content_md + optional attach_capture_ids)
        ("POST", "/api/blackboard/revise"),  # bidir Part 4 — SNAPSHOTS current content, then overwrites (real versioning, no "-v2" copies)
        ("POST", "/api/blackboard/status"),  # PROJECT_MEMORY_PLAN D3 — flip entry status open|resolved|parked (no snapshot; metadata-only)
        ("POST", "/api/blackboard/outcome"), # PROJECT_MEMORY_PLAN D11 — tag entry good|bad + required note (no snapshot; metadata-only)
        ("POST", "/api/blackboard/search"),  # PROJECT_MEMORY_PLAN D4 — case-insensitive substring over title+body; title matches beat body matches
        # NB: /api/blackboard/{restore,prune,delete} are NOT allow-listed — those are user-driven
        # via Kiwi / the /blackboard page, matching the notebooks discipline (Claude never restores
        # a version FOR the user, and never deletes their notes).
        ("POST", "/api/chains/create"),  # write 5/7 — create-only (409 on existing) + server-validated; authors a chain template the USER then runs. NOT /api/chains/save, which overwrites
        ("POST", "/api/observer/labarchives/set"),  # write 7/7 — REPLACES the LabArchives context
                                          # sidecar (a cache of an external system of record, so a
                                          # rewrite loses nothing; LabArchives is itself versioned).
                                          # Touches no project or analysis data, and never the lab
                                          # log — a change worth keeping is appended there separately.
        # bidir point-out (BIDIR_CONTEXT_PLAN Part 3, PR #4). Marks are EPHEMERAL: a 5-min-default
        # TTL, in-memory only, no persistence. Not "writes" in the destructive sense — nothing on
        # disk changes — but they DO reach the running viewer (via WS `viewer:mark`), so they earn a
        # place on the allow-list rather than sneaking through as a read.
        ("POST", "/api/viewer/marks/tracks"),   # highlight a set of track ids on the open viewer
        ("POST", "/api/viewer/marks/cells"),    # outline a set of label ids (cells) on the mask
        ("POST", "/api/viewer/marks/ui"),       # point at a UI anchor (a data-guide id or a nav path)
        ("POST", "/api/viewer/marks/freeform"), # freeform overlay on a stored capture (cap-…) — 0..1 frame-relative coords
        ("POST", "/api/viewer/marks/tile"),     # highlight ONE landscape/grid tile (e.g. "B3") — no overlay geometry, the grid is the shape
        ("POST", "/api/viewer/marks/plot"),     # bidir PR #4b: point at a spot on a PLOT panel (family + plotId + 0..1 (u,v) + optional sub-cell)
        ("POST", "/api/viewer/seek"),           # RUBBER_DUCK_FIT_PLAN P2: imperative "look at frame (t, z)" — no mark, no capture; reuses pendingViewState.focus
        ("GET",  "/api/viewer/landscape"),      # read the last-published landscape heatmap for (image, t, z); browser publishes, MCP reads
        # bidir Part 5 (push pairing) — BIDIR_PUSH_PLAN PR #1. Ties this session's inbox socket
        # + auth token to the project so PR #2's Julia writer can push a capture-arrived
        # notification directly. Auto-called by middleware on any tool with a project_uid so a
        # fresh session pairs on first use without the user typing anything.
        ("POST", "/api/push/target"),
    }
)


# Per-param fields worth keeping for a suggestion: the key to name it, its type, the valid range, the
# default, and the human label/tip. Everything else in a task spec — top-level UI plumbing
# (env/resource_pool/task/category) and per-param widget internals (option lists, field bindings,
# visibility conditions) — is bloat Claude doesn't need, so `get_module_params` strips it at the MCP
# boundary. The shared /api/tasks/definitions route is untouched (the frontend still gets full specs).
_PARAM_KEEP = ("key", "label", "type", "default", "min", "max", "step", "tip",
               # `field`/`popScope` say WHAT a selection param wants (which versioned field, cells vs
               # tracks). The option LIST is live project state, not in the spec, so these are the only
               # hint available for the params that matter most when wiring a chain — which
               # segmentation feeds which tracking. Without them a selection param is just a name.
               "field", "popScope")

# A `select`'s enum is short (2–6 entries across every task spec today); this is a bloat backstop, not a
# real limit. Keeping the whole list matters — a truncated enum reads as "these are the valid values".
_MAX_SELECT_OPTIONS = 24


def _trim_param(p: dict) -> dict:
    """One param spec → the fields worth keeping (see `_PARAM_KEEP`), plus `options` for a `select` and
    the CHILDREN of a `group` / `section`.

    A `select`'s `options` is a SHORT static enum and the only statement of its legal values — and the
    server validates against it (`_validate_leaf` in app/src/tasks/task.jl), so without it the caller can
    only echo the default or get a 400. That is different from the project-derived pickers
    (channelSelection / popSelection / valueNameSelection), whose candidates are live project state, are
    not in the spec at all, and have to be looked up per project — see `get_module_params`' docstring for
    which tool answers which. Capped so a pathological list can't bloat the payload.

    **Recursion is not optional.** `group` and `section` params hold their real knobs in a nested
    `params` list, and `params` is not a kept field — so a non-recursive trim reported `{"key": "models",
    "type": "group"}` and NOTHING inside it. For cellpose that hid every meaningful knob (`cellDiameter`
    with its µm label and 1–500 range, `cellChannels`, `nucChannels`, `model`), which is exactly the set
    an author needs. The server validates nested params (`_validate_params_against_spec` recurses), so
    hiding them also meant a 400 was reachable on a param the caller could not see.
    """
    out = {k: p[k] for k in _PARAM_KEEP if k in p}
    if p.get("type") == "select" and isinstance(p.get("options"), list):
        out["options"] = [
            o.get("value") if isinstance(o, dict) else o
            for o in p["options"][:_MAX_SELECT_OPTIONS]
        ]
    if p.get("type") in ("group", "section") and isinstance(p.get("params"), list):
        out["params"] = [_trim_param(c) for c in p["params"] if isinstance(c, dict)]
    return out


def _trim_module_params(raw: dict) -> dict:
    """Reduce raw task definitions to `{category: [{fun_name, label, params: [{<kept fields>}]}]}`."""
    out = {}
    for category, specs in (raw or {}).items():
        out[category] = [
            {
                "fun_name": spec.get("fun_name", ""),
                "label": spec.get("label", ""),
                "params": [_trim_param(p) for p in spec.get("params", [])],
            }
            for spec in specs
        ]
    return out


class DisallowedRoute(RuntimeError):
    """A caller attempted a (method, path) not on ALLOWED_ROUTES."""


class ApiError(RuntimeError):
    """The Cecelia API returned an error (or could not be reached)."""

    def __init__(self, status: int, message: str):
        super().__init__(f"HTTP {status}: {message}")
        self.status = status
        self.message = message


class CeceliaClient:
    def __init__(self, base_url: str = DEFAULT_BASE_URL, timeout: float = 30.0):
        self.base_url = base_url.rstrip("/")
        self.timeout = timeout
        # Auto-pair cache: project_uid → (socket_path, token) already POSTed for this session.
        # Skips the round-trip after the first tool call per project as long as the env vars
        # haven't rotated; a Claude Code session restart gets a new socket path (PID-named) and
        # a fresh token, so any future MCP-server subprocess spawns with new env and repairs
        # automatically. Set to None when running outside a Claude Code session (env vars
        # absent) — then _maybe_pair is a no-op.
        self._paired: dict[str, tuple[str, str]] = {}

    def _maybe_pair(self, project_uid: str) -> None:
        """If this Python process was spawned by a Claude Code session and the socket/token env
        vars are readable, register (or refresh) this project's pairing record. Called on every
        tool with a `project_uid`; no-op after the first hit per (project, socket, token). Silent
        on any failure — a broken pair must never break a read tool.
        """
        socket_path = os.environ.get("CLAUDE_CODE_MESSAGING_SOCKET", "")
        token       = os.environ.get("CLAUDE_CODE_MESSAGING_TOKEN", "")
        if not socket_path or not token or not project_uid:
            return
        if self._paired.get(project_uid) == (socket_path, token):
            return
        try:
            self._request("POST", "/api/push/target", body={
                "projectUid":    project_uid,
                "socketPath":    socket_path,
                "token":         token,
                "sessionLabel":  os.environ.get("CLAUDE_CODE_SESSION_ID", "")[:8],
                "pairedFromPid": os.environ.get("CLAUDE_PID", ""),
            })
            self._paired[project_uid] = (socket_path, token)
        except Exception:  # noqa: BLE001 — pairing is best-effort, never surfaces as a tool error
            pass

    def _request(self, method: str, path: str, params: dict | None = None, body: dict | None = None):
        if (method, path) not in ALLOWED_ROUTES:
            raise DisallowedRoute(f"{method} {path} is not an allowed observer route")
        # Bidir Part 5 auto-pair: any request that names a project registers this Claude
        # session's inbox socket for that project. Skips the pairing route itself (else
        # infinite recursion). The first call per (project, socket, token) tuple triggers a
        # single POST; every subsequent call is an in-memory cache hit. Silent on failure.
        if path != "/api/push/target":
            project_uid = ""
            if isinstance(body, dict):
                project_uid = str(body.get("projectUid", "") or "")
            elif isinstance(params, dict):
                project_uid = str(params.get("projectUid", "") or "")
            if project_uid:
                self._maybe_pair(project_uid)
        url = self.base_url + path
        if params:
            q = {k: v for k, v in params.items() if v is not None}  # drop unset optional params
            if q:
                url += "?" + urllib.parse.urlencode(q)
        data = None
        headers = {"Accept": "application/json"}
        if body is not None:
            data = json.dumps(body).encode("utf-8")
            headers["Content-Type"] = "application/json"
        req = urllib.request.Request(url, data=data, method=method, headers=headers)
        try:
            with urllib.request.urlopen(req, timeout=self.timeout) as resp:
                return json.loads(resp.read().decode("utf-8"))
        except urllib.error.HTTPError as e:
            detail = e.read().decode("utf-8", "replace")
            try:
                detail = json.loads(detail).get("error", detail)  # surface the API's {error: …}
            except Exception:
                pass
            raise ApiError(e.code, detail) from e
        except urllib.error.URLError as e:
            raise ApiError(
                0,
                f"cannot reach Cecelia API at {self.base_url} ({e.reason}). Is `pixi run dev` running?",
            ) from e

    # ── read tools ────────────────────────────────────────────────────────────────
    def get_projects(self):
        return self._request("GET", "/api/projects")

    def list_images(self, project_uid: str):
        return self._request("GET", "/api/images", {"projectUid": project_uid})

    def find_object(self, query: str, limit: int | None = None):
        # Where an object lives, given only its uid (or a fragment of its name) — the lookup every
        # other route needs a projectUid for. Server-side across all projects: an exact uid is one
        # stat per project, a name search loads each project's ccid.json files.
        return self._request("GET", "/api/objects/find", {"q": query, "limit": limit})

    def get_analysis_boards(self, project_uid: str):
        return self._request("GET", "/api/analysis/boards", {"projectUid": project_uid})

    def get_image_attributes(self, project_uid: str, set_uid: str, image_uids: str | None = None):
        params = {"projectUid": project_uid, "setUid": set_uid}
        if image_uids:
            params["imageUids"] = image_uids
        return self._request("GET", "/api/plots/attrs", params)

    def get_image_meta(self, project_uid: str, image_uid: str):
        return self._request(
            "GET", "/api/images/meta", {"projectUid": project_uid, "imageUid": image_uid}
        )

    def get_task_log(self, project_uid: str, image_uid: str, fun: str):
        return self._request(
            "GET",
            "/api/images/tasklog",
            {"projectUid": project_uid, "imageUid": image_uid, "fun": fun},
        )

    def get_task_history(self, project_uid: str, limit: int | None = None):
        return self._request(
            "GET", "/api/tasks/history", {"projectUid": project_uid, "limit": limit}
        )

    def get_module_params(self, category: str | None = None):
        # Task param SPECS (valid ranges/defaults/types), project-independent. Optional `category`
        # narrows to one module (the part before the dot in a fun_name, e.g. "tracking"). Trimmed to
        # the suggestion-relevant fields (drops UI-widget plumbing) — see `_trim_module_params`.
        raw = self._request("GET", "/api/tasks/definitions", {"category": category})
        return _trim_module_params(raw)

    def get_available_plots(self, module: str | None = None):
        # Available plot types (chart types, data needs, scope modes), project-independent. Optional
        # `module` narrows to one module page's plots.
        return self._request("GET", "/api/plots/definitions", {"module": module})

    def get_cohort_qc(self, project_uid: str, set_uid: str, fun_name: str,
                      value_name: str | None = None, threshold: float | None = None):
        return self._request(
            "GET",
            "/api/qc/cohort",
            {
                "projectUid": project_uid, "setUid": set_uid, "funName": fun_name,
                "valueName": value_name, "threshold": threshold,
            },
        )

    # Shared caller for the observer's analysis/* summary routes — same (projectUid + optional
    # image/set scope) contract for every slice, so each tool method is a one-liner over its path.
    def _analysis_summary(self, path: str, project_uid: str,
                          image_uid: str | None = None, set_uid: str | None = None):
        return self._request(
            "GET", path,
            {"projectUid": project_uid, "imageUid": image_uid, "setUid": set_uid},
        )

    def get_analysis_lineage(self, project_uid: str, image_uid: str | None = None,
                             set_uid: str | None = None):
        return self._analysis_summary("/api/analysis/lineage", project_uid, image_uid, set_uid)

    def get_populations(self, project_uid: str, image_uid: str | None = None,
                        set_uid: str | None = None):
        return self._analysis_summary("/api/analysis/populations", project_uid, image_uid, set_uid)

    def get_measure_summary(self, project_uid: str, image_uid: str | None = None,
                            set_uid: str | None = None):
        return self._analysis_summary("/api/analysis/measures", project_uid, image_uid, set_uid)

    def get_behaviour_summary(self, project_uid: str, image_uid: str | None = None,
                              set_uid: str | None = None):
        return self._analysis_summary("/api/analysis/behaviour", project_uid, image_uid, set_uid)

    def get_cluster_summary(self, project_uid: str, image_uid: str | None = None,
                            set_uid: str | None = None):
        return self._analysis_summary("/api/analysis/clusters", project_uid, image_uid, set_uid)

    def get_spatial_stats(self, project_uid: str, image_uid: str | None = None,
                          set_uid: str | None = None):
        return self._analysis_summary("/api/analysis/spatial", project_uid, image_uid, set_uid)

    def get_chains(self, project_uid: str):
        return self._analysis_summary("/api/analysis/chains", project_uid)

    def get_repl_api(self):
        # Project-independent: the notebook/REPL data-access surface (accessors + docstrings + cookbook).
        return self._request("GET", "/api/repl/api")

    def get_session_briefing(self, project_uid: str):
        # Startup context for a chat session: name/count + flagged images + recent lab log.
        return self._request("GET", "/api/observer/briefing", {"projectUid": project_uid})

    def read_lab_log(self, project_uid: str):
        return self._request("GET", "/api/lablog", {"projectUid": project_uid})

    def get_labarchives_context(self, project_uid: str):
        # The FULL LabArchives context sidecar + derived cohort gaps (the briefing carries headings only).
        return self._request("GET", "/api/observer/labarchives", {"projectUid": project_uid})

    def get_recent_logs(self):
        # The backend console ring — server-level @info/@warn/@error (task crashes land here, NOT in
        # the per-image task log, which only captures the Python subprocess's stdout). Not scoped to a
        # project (it's the process-wide console).
        return self._request("GET", "/api/logs/recent")

    def list_notebooks(self, project_uid: str):
        return self._request("GET", "/api/notebooks", params={"projectUid": project_uid})

    def get_recent_captures(self, project_uid: str, limit: int | None = None):
        # Newest-first list — capture id + timestamp + surface + address only, no frame bytes.
        return self._request("GET", "/api/viewer/captures",
                             {"projectUid": project_uid, "limit": limit})

    def get_capture(self, project_uid: str, capture_id: str):
        # Full envelope + the frame inlined as a data URL (the MCP tool re-wraps it as an image
        # content block).
        return self._request("GET", "/api/viewer/capture",
                             {"projectUid": project_uid, "captureId": capture_id})

    def get_object_ids(self, project_uid: str, image_uid: str, value_name: str,
                       kind: str = "cells", limit: int = 200, sample: bool = False):
        # Enumerate real cell / track ids so `mark_cells` / `mark_tracks` don't have to guess.
        # `kind` is "cells" | "tracks"; the server rejects anything else with a 400.
        params = {"projectUid": project_uid, "imageUid": image_uid,
                  "valueName": value_name, "kind": kind, "limit": str(limit)}
        if sample:
            params["sample"] = "true"
        return self._request("GET", "/api/labels/ids", params)

    def get_notebook(self, project_uid: str, file: str):
        # Returns {file, scope, content} — the notebook's current Pluto source (with the user's edits).
        return self._request("GET", "/api/notebooks/content",
                             params={"projectUid": project_uid, "file": file})

    # ── the writes (all non-destructive to project & analysis data) ────────────
    def set_labarchives_context(self, project_uid: str, source: dict, sections: list,
                                cohort: list, synced_by: str = "claude"):
        # REPLACE the context sidecar. Cecelia never fetches from LabArchives itself — the connector
        # lives in the user's Claude session — so this is how the context gets in.
        return self._request(
            "POST",
            "/api/observer/labarchives/set",
            body={"projectUid": project_uid, "source": source, "sections": sections,
                  "cohort": cohort, "syncedBy": synced_by},
        )

    def mark_tracks(self, project_uid: str, image_uid: str, value_name: str,
                    track_ids: list[int], focus_id: int | None = None,
                    label: str = "", ttl_s: int | None = None):
        # Point-out at track anchors. Ephemeral by design — see BIDIR_CONTEXT_PLAN Decision 18.
        body: dict = {"projectUid": project_uid, "imageUid": image_uid, "valueName": value_name,
                      "trackIds": track_ids}
        if focus_id is not None: body["focusId"] = focus_id
        if label: body["label"] = label
        if ttl_s is not None: body["ttl_s"] = ttl_s
        return self._request("POST", "/api/viewer/marks/tracks", body=body)

    def mark_cells(self, project_uid: str, image_uid: str, value_name: str,
                   label_ids: list[int], focus_id: int | None = None,
                   label: str = "", ttl_s: int | None = None):
        # Point-out at cell/label anchors. Same shape as mark_tracks; delivers via setPickHighlight.
        body: dict = {"projectUid": project_uid, "imageUid": image_uid, "valueName": value_name,
                      "labelIds": label_ids}
        if focus_id is not None: body["focusId"] = focus_id
        if label: body["label"] = label
        if ttl_s is not None: body["ttl_s"] = ttl_s
        return self._request("POST", "/api/viewer/marks/cells", body=body)

    def mark_ui(self, project_uid: str, anchor: str,
                label: str = "", ttl_s: int | None = None):
        # Point-out at a UI ANCHOR — a `data-guide` id or a `nav:/…` route. Ephemeral, same TTL as
        # the other marks. Resolves via `utils/guideAnchor.ts::resolveAnchor` on the frontend.
        body: dict = {"projectUid": project_uid, "anchor": anchor}
        if label: body["label"] = label
        if ttl_s is not None: body["ttl_s"] = ttl_s
        return self._request("POST", "/api/viewer/marks/ui", body=body)

    def mark_freeform(self, project_uid: str, capture_id: str, overlay: list,
                      label: str = "", ttl_s: int | None = None):
        # Freeform overlay on a stored CAPTURE. `capture_id` is a "cap-…" id from
        # get_recent_captures; `overlay` is the same shape captures_api.jl accepts on share-in
        # (rect | poly | stroke | circle | arrow), coords 0..1 in the frame's own space, so a
        # "point at what you shared" round-trip is exact.
        body: dict = {"projectUid": project_uid, "target": capture_id, "overlay": overlay}
        if label: body["label"] = label
        if ttl_s is not None: body["ttl_s"] = ttl_s
        return self._request("POST", "/api/viewer/marks/freeform", body=body)

    def mark_tile(self, project_uid: str, image_uid: str, cell_id: str,
                  label: str = "", ttl_s: int | None = None):
        # Landscape/grid TILE mark — spreadsheet-style cell id (`B3`) into the same coordinate
        # system GridOverlay + LandscapeOverlay paint. No overlay geometry; the grid is the shape.
        body: dict = {"projectUid": project_uid, "imageUid": image_uid, "cellId": cell_id}
        if label: body["label"] = label
        if ttl_s is not None: body["ttl_s"] = ttl_s
        return self._request("POST", "/api/viewer/marks/tile", body=body)

    def mark_plot(self, project_uid: str, family: str, plot_id: str,
                  u: float, v: float,
                  cell: str = "", label: str = "", ttl_s: int | None = None):
        # PLOT point-out (BIDIR PR #4b). `family` names the plot family
        # (`gate-scatter` / `umap` / `heatmap` / `image-strip` / `cell-cards` / …); `plot_id`
        # addresses one panel (its persistKey). `u`/`v` are 0..1 in that family's own frame
        # (`frontend/src/plots/frame.ts` — `rectFrame` / `letterboxFrame` handle the per-family
        # letterbox math so the mark lands on the plot area, not the surrounding gutter). `cell`
        # optionally addresses a sub-frame for multi-cell families (image-strip cell index,
        # facet label, pairs-matrix (row,col)).
        body: dict = {"projectUid": project_uid, "family": family, "plotId": plot_id,
                      "u": float(u), "v": float(v)}
        if cell: body["cell"] = cell
        if label: body["label"] = label
        if ttl_s is not None: body["ttl_s"] = ttl_s
        return self._request("POST", "/api/viewer/marks/plot", body=body)

    def seek_viewer(self, project_uid: str, image_uid: str,
                    t: int | None = None, z: int | None = None):
        # RUBBER_DUCK_FIT_PLAN P2 — "look at frame (t, z)" imperative. Not a mark: no TTL, no
        # ring, no capture. The Julia handler broadcasts a `viewer:seek` WS frame that the
        # browser applies via the same `pendingViewState.focus` bag Kiwi Refocus uses. At least
        # one of `t` / `z` must be set — a seek with neither is a no-op the caller shouldn't send.
        body: dict = {"projectUid": project_uid, "imageUid": image_uid}
        if t is not None: body["t"] = int(t)
        if z is not None: body["z"] = int(z)
        return self._request("POST", "/api/viewer/seek", body=body)

    def list_plots(self, project_uid: str) -> list[dict]:
        # BIDIR PR #8 — read the live plot registry for a project. Populated by the frontend's
        # `useVisualPanel` (stores/plotRegistry.ts) on panel mount / unmount, keyed by persistKey.
        # Returns the `items` array directly rather than the `{items: [...]}` envelope — Claude
        # only ever wants the list; a wrapper reads as noise on the tool response.
        data = self._request("GET", "/api/viewer/plots", {"projectUid": project_uid})
        return list(data.get("items", []))

    def get_landscape(self, project_uid: str, image_uid: str, value_name: str,
                      t: int = -1, z: int = -1):
        # Read the landscape heatmap the frontend last published for this (image, t, z). GET.
        # `t` / `z` default to -1 which the backend treats as "unspecified" (2D image or a
        # collapsed slice); a match against a published landscape uses the exact same key.
        qs = {"projectUid": project_uid, "imageUid": image_uid,
              "valueName": value_name, "t": str(t), "z": str(z)}
        return self._request("GET", "/api/viewer/landscape", params=qs)

    def append_lab_log(self, project_uid: str, author: str, lines: list[str]):
        return self._request(
            "POST",
            "/api/lablog/append",
            body={"projectUid": project_uid, "author": author, "lines": lines},
        )

    def register_push_target(self, project_uid: str, session_label: str = ""):
        # Explicit re-pair path: reads the same env vars `_maybe_pair` would, but forces a
        # POST even when the in-memory cache says nothing changed. Used by the
        # `register_push_target` MCP tool (`server.py`) so a user can nudge a stale record
        # without waiting for the natural next tool call to auto-refresh it.
        socket_path = os.environ.get("CLAUDE_CODE_MESSAGING_SOCKET", "")
        token       = os.environ.get("CLAUDE_CODE_MESSAGING_TOKEN", "")
        if not socket_path or not token:
            raise ApiError(0, "no CLAUDE_CODE_MESSAGING_SOCKET/TOKEN in this process — is the MCP "
                              "server running under Claude Code v2.1.224+?")
        label = session_label or os.environ.get("CLAUDE_CODE_SESSION_ID", "")[:8]
        pid   = os.environ.get("CLAUDE_PID", "")
        out = self._request("POST", "/api/push/target", body={
            "projectUid":    project_uid,
            "socketPath":    socket_path,
            "token":         token,
            "sessionLabel":  label,
            "pairedFromPid": pid,
        })
        self._paired[project_uid] = (socket_path, token)
        return out

    def create_notebook(self, project_uid: str, name: str, cells: list[str], description: str = ""):
        # Create-only (409 if the name exists). `cells` = Julia cell sources; the env-activation cell
        # is prepended server-side, so the notebook is self-contained/runnable.
        return self._request(
            "POST",
            "/api/notebooks/write",
            body={"projectUid": project_uid, "name": name, "cells": cells, "description": description},
        )

    def set_notebook_description(self, project_uid: str, file: str, description: str):
        # Edits ONLY the notebook's description text in the registry sidecar — never its cells. `file`
        # is the notebook filename as returned by create_notebook (e.g. "speed.jl"); a bare name works
        # too (the server appends .jl). 404 if the notebook doesn't exist.
        return self._request(
            "POST",
            "/api/notebooks/describe",
            body={"projectUid": project_uid, "file": file, "description": description},
        )

    # ── Blackboard (BIDIR_CONTEXT_PLAN Part 4) ──────────────────────────────
    # Read routes (list + entry) are non-mutating; the two write routes (create + revise) are the
    # additive-only MCP surface. restore / prune / delete stay off the MCP client — those are
    # user-driven via the Kiwi / /blackboard page.
    def list_blackboard_entries(self, project_uid: str):
        return self._request("GET", "/api/blackboard", params={"projectUid": project_uid})

    def read_blackboard_entry(self, project_uid: str, entry_id: str, version: int | None = None):
        params: dict = {"projectUid": project_uid, "entryId": entry_id}
        if version is not None:
            params["version"] = str(version)
        return self._request("GET", "/api/blackboard/entry", params=params)

    def create_blackboard_entry(self, project_uid: str, title: str, content_md: str,
                                attach_capture_ids: list[str] | None = None,
                                fingerprint: dict | None = None):
        # `fingerprint` is a PROJECT_MEMORY_PLAN P5.1 dict inferred by the MCP layer (image context
        # snapshot); the server validates it has an integer `v` field and byte-caps it. Passed only
        # when present so an unrelated caller that doesn't infer one lands on the same wire shape as
        # before P5.1.
        body = {"projectUid": project_uid, "title": title, "content": content_md}
        if attach_capture_ids:
            body["attachments"] = attach_capture_ids
        if fingerprint:
            body["fingerprint"] = fingerprint
        return self._request("POST", "/api/blackboard/create", body=body)

    def revise_blackboard_entry(self, project_uid: str, entry_id: str, content_md: str,
                                 attach_capture_ids: list[str] | None = None, note: str = ""):
        body: dict = {"projectUid": project_uid, "entryId": entry_id, "content": content_md}
        # attachments is only sent when explicitly given — omitted → server keeps the existing set.
        # `note` is accepted by the server for a future changelog view but not stored today; we still
        # pass it so a caller can start recording it now.
        if attach_capture_ids is not None:
            body["attachments"] = attach_capture_ids
        if note:
            body["note"] = note
        return self._request("POST", "/api/blackboard/revise", body=body)

    def set_blackboard_status(self, project_uid: str, entry_id: str, status: str):
        # PROJECT_MEMORY_PLAN Decision 3. Additive metadata update; does NOT snapshot. Server rejects
        # a status value outside {open,resolved,parked} with 400 — keep the enum in sync on both sides.
        return self._request("POST", "/api/blackboard/status", body={
            "projectUid": project_uid, "entryId": entry_id, "status": status,
        })

    def set_blackboard_outcome(self, project_uid: str, entry_id: str, verdict: str, note: str):
        # PROJECT_MEMORY_PLAN Decision 11. Additive metadata update; does NOT snapshot. Verdict is
        # good | bad (server 400s on anything else); note is REQUIRED and must be non-empty (the
        # note is what future sessions actually read — a verdict without one is meaningless). No-op
        # (same verdict + same note) is idempotent and returns unchanged:true.
        return self._request("POST", "/api/blackboard/outcome", body={
            "projectUid": project_uid, "entryId": entry_id,
            "verdict": verdict, "note": note,
        })

    def search_blackboard(self, project_uid: str, query: str,
                          status: str | None = None, limit: int | None = None):
        # PROJECT_MEMORY_PLAN Decision 4. Case-insensitive substring over title + body. `status`
        # optional (server rejects invalid); `limit` clamped server-side to [1, 50].
        body: dict = {"projectUid": project_uid, "query": query}
        if status:
            body["status"] = status
        if limit is not None:
            body["limit"] = int(limit)
        return self._request("POST", "/api/blackboard/search", body=body)

    def revise_notebook(self, project_uid: str, file: str, cells: list[str], description: str = ""):
        # New version of an EXISTING notebook: the server snapshots the current one (restorable via the
        # Notebooks page History) then overwrites its cells — real versioning, not a "-v2" copy. `file`
        # is the existing notebook's filename (bare name works; server appends .jl). 409 if it doesn't
        # exist (use create_notebook for a new one). `description` optional — OMITTED from the body when
        # empty so the server keeps the notebook's existing description (it updates only when the key is
        # present; sending "" would blank it, which is why a revised notebook lost its description).
        body = {"projectUid": project_uid, "file": file, "cells": cells}
        if description:
            body["description"] = description
        return self._request("POST", "/api/notebooks/revise", body=body)

    def create_chain(self, project_uid: str, name: str, nodes: list, edges: list,
                     start_targets: list | None = None):
        # Create-only (409 if the name exists) and server-validated (400 naming the offending node or
        # edge). Writes a chain TEMPLATE only — there is no route to run it, so the user launches it
        # from the whiteboard. Params may be sparse; the whiteboard merges each task's spec defaults
        # when it loads the template.
        template: dict = {"name": name, "nodes": nodes, "edges": edges}
        if start_targets:
            template["startTargets"] = start_targets
        return self._request("POST", "/api/chains/create",
                             body={"projectUid": project_uid, "template": template})

    def add_analysis_board(self, project_uid: str, name: str, plots: list, template: str = "",
                           compare_by: str = ""):
        # Create-only: adds ONE board and cannot modify, delete, rename or reorder any existing one
        # (409 on a duplicate name, 422 on a spec the project cannot plot). Deliberately NOT
        # /api/projects/boards, which is the browser's autosave of the WHOLE document — allow-listing
        # that would let one request replace every board in the project.
        body: dict = {"projectUid": project_uid, "name": name, "plots": plots}
        if template:
            body["template"] = template
        if compare_by:
            # board-level: what the plots compare ACROSS images (per_image / summarised / an attribute
            # name). Server-validated against the project's real attributes.
            body["compareBy"] = compare_by
        return self._request("POST", "/api/boards/add", body=body)
