"""Read-only HTTP client for the Cecelia Julia API, used by the MCP observer server.

Every request goes through an explicit ALLOW-LIST of (method, path) pairs. That list IS the
observer's no-mutation guarantee: the only non-GET routes permitted are ``POST /api/boards/add``
(create-only — adds ONE Analysis board beside the user's own, which they delete in one click; it
cannot modify, rename, reorder or delete a board), ``POST /api/lablog/append``
(append-only), ``POST /api/notebooks/write`` (create-only — 409 on an existing name, so it never
overwrites), ``POST /api/notebooks/describe`` (edits ONLY a notebook's own description string in the
registry sidecar — not its cells), ``POST /api/notebooks/revise`` (which SNAPSHOTS the current
notebook first — a restorable version — then overwrites its cells; it's how a notebook gets a new
version, never a "-v2" copy), and ``POST /api/chains/create`` (create-only + server-validated —
writes a chain TEMPLATE the user then runs themselves). All six are recoverable / non-destructive to
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
import urllib.error
import urllib.parse
import urllib.request

DEFAULT_BASE_URL = "http://127.0.0.1:8080"

# (method, path) → the ONLY routes the observer may ever call. Read-only except the three writes below.
# Keep this in sync with the backing routes in api/src/routes.jl; the test pins the exact write set.
ALLOWED_ROUTES = frozenset(
    {
        ("POST", "/api/boards/add"),      # WRITE 6/6 — create-only: adds ONE Analysis board, never
                                          # edits/deletes/reorders one. Server-validated against the
                                          # project (unknown plot id, chart the spec doesn't offer, a
                                          # population that doesn't exist → 422 before anything is
                                          # written), 409 on a duplicate name. NOT
                                          # /api/projects/boards, the browser's whole-document autosave
        ("GET", "/api/projects"),
        ("GET", "/api/images"),
        ("GET", "/api/images/meta"),
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
        ("POST", "/api/lablog/append"),  # write 1/4 — append-only, server-guarded
        ("POST", "/api/notebooks/write"),  # write 2/4 — create-only (409 on existing); serialises cells to a Pluto notebook
        ("POST", "/api/notebooks/describe"),  # write 3/4 — edits ONLY a notebook's description string (registry sidecar); not its content
        ("POST", "/api/notebooks/revise"),  # write 4/4 — SNAPSHOTS the current notebook (restorable), then overwrites its cells (real versioning, no "-v2" copies)
        ("POST", "/api/chains/create"),  # write 5/5 — create-only (409 on existing) + server-validated; authors a chain template the USER then runs. NOT /api/chains/save, which overwrites
        ("POST", "/api/observer/labarchives/set"),  # write 7/7 — REPLACES the LabArchives context
                                          # sidecar (a cache of an external system of record, so a
                                          # rewrite loses nothing; LabArchives is itself versioned).
                                          # Touches no project or analysis data, and never the lab
                                          # log — a change worth keeping is appended there separately.
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

    def _request(self, method: str, path: str, params: dict | None = None, body: dict | None = None):
        if (method, path) not in ALLOWED_ROUTES:
            raise DisallowedRoute(f"{method} {path} is not an allowed observer route")
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

    def append_lab_log(self, project_uid: str, author: str, lines: list[str]):
        return self._request(
            "POST",
            "/api/lablog/append",
            body={"projectUid": project_uid, "author": author, "lines": lines},
        )

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

    def add_analysis_board(self, project_uid: str, name: str, plots: list, template: str = ""):
        # Create-only: adds ONE board and cannot modify, delete, rename or reorder any existing one
        # (409 on a duplicate name, 422 on a spec the project cannot plot). Deliberately NOT
        # /api/projects/boards, which is the browser's autosave of the WHOLE document — allow-listing
        # that would let one request replace every board in the project.
        body: dict = {"projectUid": project_uid, "name": name, "plots": plots}
        if template:
            body["template"] = template
        return self._request("POST", "/api/boards/add", body=body)
