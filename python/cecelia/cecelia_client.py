"""
Thin HTTP client for the Julia gating API — the Python *membership* source.

Julia is the sole gate evaluator (docs/POPULATION.md). Python tasks and notebooks never
re-derive membership: they ask the running API which cells belong to a population and read
the bulk measurement columns locally from the H5AD (see `label_props_utils.py`). This keeps
the logicle transform + gate logic in exactly one place and avoids a `flowutils`/`juliacall`
dependency. The same client is used in notebook development and in shipped task modules — the
API server is running in both (it is what launches production tasks).

Uses only the stdlib (`urllib`) so it adds no dependency to the napari venv.
"""
import json
import os
import urllib.parse

import numpy as np

from cecelia.utils import loopback


def default_base_url() -> str:
    """This user's backend: `CECELIA_API_URL` if given, else 127.0.0.1 on this user's port
    (`CECELIA_PORT`, or 8080 + 10 × `CECELIA_PORT_SLOT` — app/src/ports.jl; a task inherits both).
    The scheme here is only a first guess; `loopback.open_url` finds the one the server speaks."""
    url = os.environ.get("CECELIA_API_URL", "").strip()
    if url:
        return url
    port = os.environ.get("CECELIA_PORT", "").strip() or str(8080 + 10 * int(os.environ.get("CECELIA_PORT_SLOT", "0") or 0))
    return f"http://127.0.0.1:{port}"


class CeceliaClient:
    def __init__(self, base_url: str | None = None,
                 project_uid: str = None, image_uid: str = None, timeout: float = 30.0,
                 token: str | None = None):
        self.base_url = (base_url or default_base_url()).rstrip("/")
        # The API only answers its own user (app/src/api_token.jl). A task the backend spawned has the
        # token in its env; anywhere else, pass it (`<config_dir>/api-token`).
        self.token = token if token is not None else os.environ.get("CECELIA_API_TOKEN", "")
        self.project_uid = project_uid
        self.image_uid = image_uid
        self.timeout = timeout

    # ── internal ──────────────────────────────────────────────────────────────────
    @staticmethod
    def _path(path: str, params: dict) -> str:
        q = urllib.parse.urlencode({k: v for k, v in params.items() if v is not None})
        return f"{path}?{q}"

    def _open(self, path: str, params: dict):
        """GET `path` on the backend, on whichever scheme it serves (`loopback.open_url`). An HTTP
        error status is raised as-is; nothing reachable is a `ConnectionError`."""
        headers = {"Authorization": f"Bearer {self.token}"} if self.token else {}
        return loopback.open_url(self.base_url, self._path(path, params), headers=headers,
                                 timeout=self.timeout)

    def _common(self, value_name, pop_type):
        return {"projectUid": self.project_uid, "imageUid": self.image_uid,
                "valueName": value_name, "popType": pop_type}

    # ── membership ──────────────────────────────────────────────────────────────────
    def cells_in_pops(self, pop_type, pops, value_name: str = "default") -> dict:
        """Return ``{pop_path: [label_ids]}`` for one or more populations (JSON)."""
        if isinstance(pops, str):
            pops = [pops]
        params = self._common(value_name, pop_type)
        params["pops"] = ",".join(pops)
        with self._open("/api/gating/membership", params) as r:
            return json.loads(r.read().decode())["membership"]

    def cells_in_pop(self, pop_type, pop, value_name: str = "default") -> np.ndarray:
        """Label IDs of a single population as an ``int32`` array (binary transfer — fast
        for low-selectivity pops at the 10^6 scale)."""
        params = self._common(value_name, pop_type)
        params["pops"] = pop
        params["binary"] = "1"
        with self._open("/api/gating/membership", params) as r:
            return np.frombuffer(r.read(), dtype="<i4")
