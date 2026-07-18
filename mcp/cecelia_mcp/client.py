"""Read-only HTTP client for the Cecelia Julia API, used by the MCP observer server.

Every request goes through an explicit ALLOW-LIST of (method, path) pairs. That list IS the
observer's no-mutation guarantee: the only non-GET route permitted is ``POST /api/lablog/append``
(append-only, itself server-guarded). Any attempt to call a route not on the list raises
``DisallowedRoute`` — so if a future tool ever wires in a mutating route it fails loudly in tests
rather than silently mutating a project.

Uses only the Python standard library (urllib) so this module — and its tests — carry no third-party
dependency; the ``mcp`` SDK is needed only by ``server.py`` which wires these calls into tools.
"""
from __future__ import annotations

import json
import urllib.error
import urllib.parse
import urllib.request

DEFAULT_BASE_URL = "http://127.0.0.1:8080"

# (method, path) → the ONLY routes the observer may ever call. Read-only except the append route.
# Keep this in sync with the backing routes in api/src/routes.jl; the test asserts append is the
# sole write.
ALLOWED_ROUTES = frozenset(
    {
        ("GET", "/api/projects"),
        ("GET", "/api/images"),
        ("GET", "/api/images/meta"),
        ("GET", "/api/images/tasklog"),
        ("GET", "/api/tasks/history"),
        ("GET", "/api/qc/cohort"),       # cohort QC: per-set mean/SD + outliers over banked metrics
        ("GET", "/api/analysis/lineage"),  # synthesized pipeline lineage (steps + seg/track/cluster/gating links)
        ("GET", "/api/analysis/populations"),  # population definitions (tree + gate/filter specs)
        ("GET", "/api/analysis/measures"),  # per-pop phenotype + motility summaries (median/quantiles)
        ("GET", "/api/logs/recent"),     # the backend console ring (server @info/@warn/@error)
        ("GET", "/api/lablog"),
        ("POST", "/api/lablog/append"),  # the ONLY write — append-only, server-guarded
    }
)


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

    def read_lab_log(self, project_uid: str):
        return self._request("GET", "/api/lablog", {"projectUid": project_uid})

    def get_recent_logs(self):
        # The backend console ring — server-level @info/@warn/@error (task crashes land here, NOT in
        # the per-image task log, which only captures the Python subprocess's stdout). Not scoped to a
        # project (it's the process-wide console).
        return self._request("GET", "/api/logs/recent")

    # ── the one write (append-only) ─────────────────────────────────────────────────
    def append_lab_log(self, project_uid: str, author: str, lines: list[str]):
        return self._request(
            "POST",
            "/api/lablog/append",
            body={"projectUid": project_uid, "author": author, "lines": lines},
        )
