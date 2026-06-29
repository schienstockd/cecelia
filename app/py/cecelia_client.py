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
import urllib.parse
import urllib.request

import numpy as np


class CeceliaClient:
    def __init__(self, base_url: str = "http://localhost:8080",
                 project_uid: str = None, image_uid: str = None, timeout: float = 30.0):
        self.base_url = base_url.rstrip("/")
        self.project_uid = project_uid
        self.image_uid = image_uid
        self.timeout = timeout

    # ── internal ──────────────────────────────────────────────────────────────────
    def _url(self, path: str, params: dict) -> str:
        q = urllib.parse.urlencode({k: v for k, v in params.items() if v is not None})
        return f"{self.base_url}{path}?{q}"

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
        url = self._url("/api/gating/membership", params)
        with urllib.request.urlopen(url, timeout=self.timeout) as r:
            return json.loads(r.read().decode())["membership"]

    def cells_in_pop(self, pop_type, pop, value_name: str = "default") -> np.ndarray:
        """Label IDs of a single population as an ``int32`` array (binary transfer — fast
        for low-selectivity pops at the 10^6 scale)."""
        params = self._common(value_name, pop_type)
        params["pops"] = pop
        params["binary"] = "1"
        url = self._url("/api/gating/membership", params)
        with urllib.request.urlopen(url, timeout=self.timeout) as r:
            return np.frombuffer(r.read(), dtype="<i4")
