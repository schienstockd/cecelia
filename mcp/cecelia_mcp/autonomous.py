"""The AUTONOMOUS surface: what an unattended agent needs beyond the observer — start a task or a
chain, wait for it, and draw a gate — behind its own allow-list and a hard project lock.

The observer (``client.py``) stays read-only by construction: it speaks HTTP only, and launching is
a WebSocket message. This module is the deliberate, separate exception, served by its own MCP server
(``autonomous_server.py``) that a session gets ONLY when it is started with the lock set:

  ``CECELIA_MCP_PROJECT``  the one project uid every call may touch (required — unset = refuse all)
  ``CECELIA_MCP_PREFIX``   optional: every output a run NAMES (segmentation, HMM column, cluster
                           suffix …) and every label set a downstream task writes INTO must start
                           with it, so an agent's results are never confused with the user's

Fixed-name cleanup outputs (``afCorrected``, ``driftCorrected``, ``denoised``) can't be prefixed —
the specs don't offer a name — which is why the lock is a whole PROJECT: the agent works in a
disposable one (``scripts/agent_eval/app_project.py`` builds it), never in the user's.

Stdlib + ``websockets`` (already a dependency of the observer's listener).
"""
from __future__ import annotations

import json
import os
import secrets
import string
import time

from cecelia_mcp import gating_views as gv
from cecelia_mcp.client import ApiError, DEFAULT_BASE_URL, DisallowedRoute, http_json
from cecelia_mcp.wsclient import api_url_to_ws

PROJECT_ENV = "CECELIA_MCP_PROJECT"
PREFIX_ENV = "CECELIA_MCP_PREFIX"

# (method, path) → the ONLY routes the autonomous server may call. Writes are gating only, and only
# inside the locked project; starting work is WebSocket (`task:run`, `chain:run`), not on this list.
AUTONOMOUS_ROUTES = frozenset({
    ("GET", "/api/tasks"),              # in-flight snapshot (queued/running)
    ("GET", "/api/tasks/recent"),       # terminal outcomes of recently finished tasks
    ("GET", "/api/tasks/history"),      # the on-disk run log (fallback once `recent` has rolled)
    ("GET", "/api/tasks/definitions"),  # task specs — the guard reads output-name declarations here
    ("GET", "/api/images"),             # the project's images + sets (to resolve a set uid)
    ("GET", "/api/chains/get"),         # a chain template (the guard checks every node's params)
    ("GET", "/api/chains/runs"),        # chain runs, newest first
    ("GET", "/api/chains/run"),         # one run's per-image node status
    ("GET", "/api/gating/channels"),    # gateable columns of a segmentation
    ("GET", "/api/gating/summary"),     # quantiles + counts per axis, and the joint x × y grid
    ("GET", "/api/gating/stats"),       # count / % of parent for one population
    ("GET", "/api/gating/plot-image"),  # the gate plot as a PNG (+ axes, named gates)
    ("GET", "/api/gating/cells-image"), # one timepoint with a population's cells outlined (PNG)
    ("POST", "/api/correction-plan/recommend"),  # pure (no write): the metadata-only cleanup plan
    ("POST", "/api/gating/pop/add"),    # WRITE — add a population (gate) to a segmentation
    ("POST", "/api/gating/pop/set-gate"),  # WRITE — move an existing population's gate
    ("POST", "/api/gating/pop/delete"),    # WRITE — remove a population this agent drew
})

_TERMINAL = {"done", "failed", "cancelled", "interrupted", "skipped"}
_UID_ALPHABET = string.ascii_letters + string.digits


def new_task_id() -> str:
    return "".join(secrets.choice(_UID_ALPHABET) for _ in range(6))


class LockViolation(RuntimeError):
    """A call outside the locked project, or an output name without the required prefix."""


def locked_project() -> str:
    return os.environ.get(PROJECT_ENV, "").strip()


def required_prefix() -> str:
    return os.environ.get(PREFIX_ENV, "").strip()


def check_project(project_uid: str) -> None:
    lock = locked_project()
    if not lock:
        raise LockViolation(f"{PROJECT_ENV} is not set — the autonomous server refuses every call")
    if project_uid != lock:
        raise LockViolation(f"project {project_uid!r} is not the locked project {lock!r}; "
                            "this session may only run work in that one")


# ── the output-name guard ──────────────────────────────────────────────────────────────────────────

def _flat_params(spec_params):
    for p in spec_params or []:
        if not isinstance(p, dict):
            continue
        if p.get("type") in ("section", "group"):
            yield from _flat_params(p.get("params"))
        else:
            yield p


def find_spec(definitions: dict, fun_name: str) -> dict | None:
    """The task spec for `fun_name` (`segment.cellpose`) out of /api/tasks/definitions
    ({category: [spec, …]}). Match the spec's own `fun_name`: its `task` key is the internal id
    (`cellposeSegment`), not the name agents and chains use."""
    cat = fun_name.partition(".")[0]
    for spec in definitions.get(cat, []) or []:
        if isinstance(spec, dict) and spec.get("fun_name") == fun_name:
            return spec
    return None


def _pop_value_names(value) -> list[str]:
    """Value names named by a population selection (`"vn/path"` entries); bare paths name none."""
    items = value if isinstance(value, list) else [value]
    out = []
    for it in items:
        s = str(it or "")
        if "/" in s and not s.startswith("/"):
            out.append(s.split("/", 1)[0])
    return out


def output_name_violations(spec: dict, params: dict, prefix: str) -> list[str]:
    """Why `params` would write outside the prefix: a NAMED output (a `namespace` param, or the
    legacy `outputValueName`) without it, or a label set (`field: labels|label_props`) or a
    `vn/pop` selection outside it — downstream tasks write INTO the set they read (tracks, measures,
    HMM states). Empty list = allowed. No prefix = nothing to check."""
    if not prefix:
        return []
    bad = []
    for p in _flat_params(spec.get("params")):
        key = p.get("key")
        if not key:
            continue
        value = params.get(key, p.get("default"))
        if p.get("namespace") or key == "outputValueName" or key == "colName":
            if not str(value or "").startswith(prefix):
                bad.append(f"{key}={value!r} (output names must start with {prefix!r})")
        elif p.get("field") in ("labels", "label_props"):
            if not str(value or "").startswith(prefix):
                bad.append(f"{key}={value!r} (only label sets starting with {prefix!r} may be written)")
        elif p.get("type") == "popSelection":
            for vn in _pop_value_names(value):
                if not vn.startswith(prefix):
                    bad.append(f"{key} names {vn!r} (populations must be in a {prefix!r} label set)")
    return bad


# ── client ─────────────────────────────────────────────────────────────────────────────────────────

class AutonomousClient:
    def __init__(self, base_url: str = DEFAULT_BASE_URL, timeout: float = 60.0):
        self.base_url = base_url.rstrip("/")
        self.timeout = timeout
        self._definitions: dict | None = None
        # task id → (project, fun, image uid, local submit time): the history fallback's lookup key,
        # since the on-disk run log carries no task id
        self._launched: dict[str, tuple[str, str, str, str]] = {}

    def _request(self, method: str, path: str, params: dict | None = None, body: dict | None = None,
                 raw: bool = False):
        if (method, path) not in AUTONOMOUS_ROUTES:
            raise DisallowedRoute(f"{method} {path} is not an allowed autonomous route")
        uid = (body or {}).get("projectUid") or (params or {}).get("projectUid")
        if uid is not None:
            check_project(str(uid))
        return http_json(self.base_url, method, path, params, body, self.timeout, raw=raw)

    def definitions(self) -> dict:
        if self._definitions is None:
            self._definitions = self._request("GET", "/api/tasks/definitions")
        return self._definitions

    def check_task(self, fun_name: str, params: dict) -> dict:
        spec = find_spec(self.definitions(), fun_name)
        if spec is None:
            raise ApiError(400, f"unknown task {fun_name!r} — get_module_params lists them as "
                                "`category.task`")
        bad = output_name_violations(spec, params, required_prefix())
        if bad:
            raise LockViolation(f"{fun_name}: " + "; ".join(bad))
        return spec

    # ── WebSocket launch ──
    def _ws_send(self, message: dict, until, wait_s: float = 4.0) -> list[dict]:
        """Send one message on the API's /ws and collect the frames `until(frame)` accepts, for up to
        `wait_s` — enough to see an immediate refusal; progress is then polled over HTTP."""
        from websockets.sync.client import connect
        seen: list[dict] = []
        try:
            with connect(api_url_to_ws(self.base_url), open_timeout=5) as ws:
                ws.send(json.dumps(message))
                deadline = time.time() + wait_s
                while time.time() < deadline:
                    try:
                        raw = ws.recv(timeout=max(0.05, deadline - time.time()))
                    except TimeoutError:
                        break
                    try:
                        frame = json.loads(raw)
                    except (TypeError, ValueError):
                        continue
                    if isinstance(frame, dict) and until(frame):
                        seen.append(frame)
                        if frame.get("status") in _TERMINAL or str(frame.get("type", "")).endswith(
                                ("failed", "started")):
                            break
        except OSError as e:
            raise ApiError(0, f"cannot reach Cecelia WebSocket at {self.base_url} ({e})") from e
        return seen

    def run_task(self, project_uid: str, fun_name: str, params: dict, image_uids: list[str],
                 set_uid: str = "") -> dict:
        check_project(project_uid)
        spec = self.check_task(fun_name, params)
        scope = spec.get("scope") or "image"
        launches = []
        targets = [None] if scope == "set" else image_uids
        for uid in targets:
            tid = new_task_id()
            msg = {"type": "task:run", "taskId": tid, "funName": fun_name, "projectUid": project_uid,
                   "params": params}
            if scope == "set":
                msg.update({"imageUids": image_uids, "setUid": set_uid})
            else:
                msg["imageUid"] = uid
            self._launched[tid] = (project_uid, fun_name, uid or "", time.strftime("%Y-%m-%dT%H:%M:%S"))
            frames = self._ws_send(msg, lambda f, t=tid: f.get("taskId") == t)
            failed = [f for f in frames if f.get("status") == "failed"]
            logs = [f.get("line") for f in frames if f.get("type") == "task:log" and f.get("line")]
            launches.append({"taskId": tid, "imageUid": uid, "scope": scope,
                             "status": "failed" if failed else "submitted", "log": logs[-5:]})
        return {"funName": fun_name, "tasks": launches}

    def run_chain(self, project_uid: str, chain_name: str, image_uids: list[str]) -> dict:
        check_project(project_uid)
        template = self._request("GET", "/api/chains/get", {"projectUid": project_uid, "name": chain_name})
        problems = []
        for node in template.get("nodes", []) or []:
            fn = node.get("fn") or node.get("fun") or ""
            try:
                self.check_task(fn, node.get("params") or {})
            except (LockViolation, ApiError) as e:
                problems.append(f"node {node.get('id')}: {e}")
        if problems:
            raise LockViolation("chain refused — " + " | ".join(problems))
        started_at = time.time()
        frames = self._ws_send({"type": "chain:run", "projectUid": project_uid, "chain": chain_name,
                                "imageUids": image_uids},
                               lambda f: str(f.get("type", "")).startswith("chain:run"))
        failed = [f for f in frames if f.get("type") == "chain:run:failed"]
        if failed:
            raise ApiError(400, failed[-1].get("error") or "chain run refused")
        run_id = ""
        for _ in range(20):                       # the run.json lands a moment after `started`
            runs = self._request("GET", "/api/chains/runs", {"projectUid": project_uid}).get("runs", [])
            mine = [r for r in runs if r.get("chainName") == chain_name
                    and float(r.get("createdAt") or 0) >= started_at - 5]
            if mine:
                run_id = mine[0]["runId"]
                break
            time.sleep(0.5)
        return {"chain": chain_name, "runId": run_id, "imageCount": len(image_uids)}

    # ── waiting ──
    def task_states(self, task_ids: list[str]) -> dict[str, str]:
        state = {t: "unknown" for t in task_ids}
        for row in self._request("GET", "/api/tasks") or []:
            if row.get("id") in state:
                state[row["id"]] = row.get("status") or "queued"
        for row in self._request("GET", "/api/tasks/recent") or []:
            if row.get("id") in state and state[row["id"]] == "unknown":
                state[row["id"]] = row.get("status") or "done"
        # neither in flight nor in the recent ring (a runner restart, or the ring rolled): the run log
        for tid in [t for t, s in state.items() if s == "unknown" and t in self._launched]:
            proj, fun, uid, at = self._launched[tid]
            hist = self._request("GET", "/api/tasks/history", {"projectUid": proj, "limit": 200})
            for row in hist.get("history", []):
                if row.get("fun") == fun and (not uid or row.get("imageUid") == uid) and \
                        str(row.get("at", "")) >= at:
                    state[tid] = row.get("runStatus") or row.get("status") or "done"
                    break
        return state

    def wait_tasks(self, task_ids: list[str], timeout_s: float, poll_s: float = 5.0) -> dict:
        t0 = time.time()
        while True:
            st = self.task_states(task_ids)
            if all(s in _TERMINAL for s in st.values()) or time.time() - t0 > timeout_s:
                return {"finished": all(s in _TERMINAL for s in st.values()), "states": st,
                        "waitedS": round(time.time() - t0)}
            time.sleep(poll_s)

    def chain_state(self, project_uid: str, run_id: str) -> dict:
        run = self._request("GET", "/api/chains/run", {"projectUid": project_uid, "runId": run_id})
        live = [r for r in self._request("GET", "/api/tasks") or [] if r.get("chain_run_id") == run_id]
        statuses = [s for nodes in (run.get("imageStates") or {}).values() for s in nodes.values()]
        active = bool(live) or any(s in ("queued", "running") for s in statuses)
        return {"runId": run_id, "chain": run.get("chainName"), "active": active,
                "imageStates": run.get("imageStates"),
                "nodes": [{"id": n.get("id"), "fn": n.get("fn")} for n in run.get("nodes") or []]}

    def wait_chain(self, project_uid: str, run_id: str, timeout_s: float, poll_s: float = 5.0) -> dict:
        t0 = time.time()
        idle = 0
        while True:
            st = self.chain_state(project_uid, run_id)
            idle = 0 if st["active"] else idle + 1
            # two idle polls in a row: a node between "done" and the next one's "queued" is not the end
            if idle >= 2 or time.time() - t0 > timeout_s:
                st.update({"finished": not st["active"], "waitedS": round(time.time() - t0)})
                return st
            time.sleep(poll_s)

    def recommend_correction_plan(self, project_uid: str, image_uid: str) -> dict:
        check_project(project_uid)
        plan = self._request("POST", "/api/correction-plan/recommend",
                             body={"projectUid": project_uid, "imageUid": image_uid})
        keep = ("funName", "params", "source", "exclusionReason")
        return {"included": [{k: s.get(k) for k in keep} for s in plan.get("included") or []],
                "excluded": [{k: s.get(k) for k in keep} for s in plan.get("excluded") or []],
                "card": plan.get("presetId")}

    # ── gating ──
    def _require_measured(self, project_uid: str, image_uid: str, value_name: str) -> None:
        """The gating routes silently swap an unknown value_name for the image's active one (a GUI
        tolerance for stale clients) — so a gate drawn on an unmeasured set would land on ANOTHER
        segmentation, and a read reports the wrong set's error. Refuse instead, and say why."""
        got = self._request("GET", "/api/gating/channels", {"projectUid": project_uid,
                                                            "imageUid": image_uid, "valueName": value_name})
        if got.get("valueName") != value_name:
            raise ApiError(400, f"{value_name!r} has no measurements (label_props) on image {image_uid} — "
                                "gating needs a measured segmentation: run a *Measure task (e.g. "
                                "segment.cellposeMeasure, or segment.measureLabels on existing labels)")

    def gate_histogram(self, project_uid: str, image_uid: str, value_name: str, x: str, y: str,
                       transform: dict | None, pop: str, bins: int) -> dict:
        self._require_measured(project_uid, image_uid, value_name)
        q = {**gv.plot_query(project_uid, image_uid, value_name, x, y, transform, pop), "bins": bins}
        return self._request("GET", "/api/gating/summary", q)

    def gate_plot(self, project_uid: str, image_uid: str, value_name: str, x: str, y: str,
                  transform: dict | None, pop: str) -> tuple[bytes, dict]:
        self._require_measured(project_uid, image_uid, value_name)
        return gv.split_png(self._request("GET", gv.PLOT_ROUTE, gv.plot_query(
            project_uid, image_uid, value_name, x, y, transform, pop)))

    def gate_cells_view(self, project_uid: str, image_uid: str, value_name: str, pop: str, t: int,
                        channels: list[int] | None, image_version: str) -> tuple[bytes, dict]:
        self._require_measured(project_uid, image_uid, value_name)
        return gv.split_png(self._request("GET", gv.CELLS_ROUTE, gv.cells_query(
            project_uid, image_uid, value_name, pop, t, channels, image_version)))

    def gating_post(self, route: str, project_uid: str, image_uid: str, value_name: str, **fields) -> dict:
        # `route` (not `path`): `path` is a FIELD here — the population path set-gate/delete address
        prefix = required_prefix()
        if prefix and not value_name.startswith(prefix):
            raise LockViolation(f"gates may only be drawn on label sets starting with {prefix!r}")
        self._require_measured(project_uid, image_uid, value_name)
        body = {"projectUid": project_uid, "imageUid": image_uid, "valueName": value_name,
                "popType": "flow", **{k: v for k, v in fields.items() if v is not None}}
        return self._request("POST", route, body=body)

    def pop_stats(self, project_uid: str, image_uid: str, value_name: str, pop: str) -> dict:
        self._require_measured(project_uid, image_uid, value_name)
        return self._request("GET", "/api/gating/stats", {"projectUid": project_uid, "imageUid": image_uid,
                                                          "valueName": value_name, "pop": pop})
