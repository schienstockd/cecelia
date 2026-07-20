"""Unit tests for the read-only Cecelia API client — the observer's no-mutation guarantee.

Pure stdlib (unittest + mock); urllib is mocked so no server is needed. These pin the allow-list,
URL/param building, body shaping, and error mapping — the logic that keeps the observer read-only.
"""
import io
import json
import unittest
import urllib.error
from unittest import mock

from cecelia_mcp.client import ALLOWED_ROUTES, ApiError, CeceliaClient, DisallowedRoute


class _FakeResp:
    def __init__(self, payload):
        self._data = json.dumps(payload).encode()

    def read(self):
        return self._data

    def __enter__(self):
        return self

    def __exit__(self, *a):
        return False


def _patch_urlopen(payload=None):
    return mock.patch(
        "cecelia_mcp.client.urllib.request.urlopen",
        return_value=_FakeResp({} if payload is None else payload),
    )


class ClientTest(unittest.TestCase):
    def setUp(self):
        self.c = CeceliaClient(base_url="http://x:8080")

    def test_allowlist_blocks_unknown_routes(self):
        # a mutating route that exists in the API must NOT be reachable through the client
        with self.assertRaises(DisallowedRoute):
            self.c._request("POST", "/api/images/delete", body={"x": 1})
        with self.assertRaises(DisallowedRoute):
            self.c._request("GET", "/api/gating/save")

    def test_writes_are_only_the_three_non_destructive_routes(self):
        # The no-mutation guarantee: the only non-GET routes are lab-log append (append-only), notebook
        # write (create-only), and notebook describe (description text only). None can edit/delete cell
        # data, images, gates, QC, or a notebook's content.
        writes = sorted((m, p) for (m, p) in ALLOWED_ROUTES if m != "GET")
        self.assertEqual(writes, [
            ("POST", "/api/lablog/append"),
            ("POST", "/api/notebooks/describe"),
            ("POST", "/api/notebooks/write"),
        ])

    def test_create_notebook_posts_cells(self):
        with _patch_urlopen({"ok": True, "file": "speed.jl"}) as u:
            self.c.create_notebook("p", "speed", ["using Cecelia", "df = 1"], description="d")
        req = u.call_args[0][0]
        self.assertEqual(req.method, "POST")
        self.assertTrue(req.full_url.endswith("/api/notebooks/write"))
        self.assertEqual(
            json.loads(req.data.decode()),
            {"projectUid": "p", "name": "speed", "cells": ["using Cecelia", "df = 1"], "description": "d"},
        )

    def test_set_notebook_description_posts(self):
        with _patch_urlopen({"ok": True}) as u:
            self.c.set_notebook_description("p", "speed.jl", "shorter blurb")
        req = u.call_args[0][0]
        self.assertEqual(req.method, "POST")
        self.assertTrue(req.full_url.endswith("/api/notebooks/describe"))
        self.assertEqual(
            json.loads(req.data.decode()),
            {"projectUid": "p", "file": "speed.jl", "description": "shorter blurb"},
        )

    def test_notebook_read_routes_allow_listed(self):
        self.assertIn(("GET", "/api/notebooks"), ALLOWED_ROUTES)
        self.assertIn(("GET", "/api/notebooks/content"), ALLOWED_ROUTES)

    def test_list_notebooks_builds_url(self):
        with _patch_urlopen({"notebooks": []}) as u:
            self.c.list_notebooks("p")
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertIn("/api/notebooks?projectUid=p", req.full_url)

    def test_get_notebook_builds_url(self):
        with _patch_urlopen({"file": "speed.jl", "scope": "project", "content": "x=1"}) as u:
            self.c.get_notebook("p", "speed.jl")
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertIn("/api/notebooks/content?", req.full_url)
        self.assertIn("file=speed.jl", req.full_url)

    def test_list_images_builds_url(self):
        with _patch_urlopen({"images": []}) as u:
            self.c.list_images("proj1")
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertIn("/api/images?projectUid=proj1", req.full_url)

    def test_task_log_encodes_all_params(self):
        with _patch_urlopen({"exists": False, "content": ""}) as u:
            self.c.get_task_log("p", "img1", "segment.cellpose")
        url = u.call_args[0][0].full_url
        self.assertIn("imageUid=img1", url)
        self.assertIn("fun=segment.cellpose", url)

    def test_task_history_drops_none_limit(self):
        with _patch_urlopen({"history": []}) as u:
            self.c.get_task_history("p")
        self.assertNotIn("limit", u.call_args[0][0].full_url)
        with _patch_urlopen({"history": []}) as u:
            self.c.get_task_history("p", limit=5)
        self.assertIn("limit=5", u.call_args[0][0].full_url)

    def test_module_params_builds_url_and_trims_spec(self):
        self.assertIn(("GET", "/api/tasks/definitions"), ALLOWED_ROUTES)
        raw = {"tracking": [{
            "task": "bayesianTracking", "fun_name": "tracking.bayesian_tracking",
            "label": "Bayesian Tracking", "category": "Tracking",
            "env": ["local"], "resource_pool": "default",
            "params": [
                {"key": "maxSearchRadius", "label": "Max search radius", "type": "int",
                 "min": 1, "max": 200, "step": 1, "default": 20, "tip": "px/frame"},
                {"key": "valueName", "type": "valueNameSelection", "field": "labels",
                 "default": "default", "options": ["a", "b", "c"]},
            ],
        }]}
        with _patch_urlopen(raw) as u:
            out = self.c.get_module_params("tracking")
        url = u.call_args[0][0].full_url
        self.assertIn("/api/tasks/definitions?", url)
        self.assertIn("category=tracking", url)
        spec = out["tracking"][0]
        self.assertEqual(spec["fun_name"], "tracking.bayesian_tracking")
        self.assertNotIn("env", spec)               # top-level UI plumbing stripped
        self.assertNotIn("resource_pool", spec)
        p_knob, p_sel = spec["params"]
        self.assertEqual((p_knob["min"], p_knob["max"], p_knob["default"]), (1, 200, 20))
        self.assertNotIn("field", p_sel)            # per-param widget internals stripped
        self.assertNotIn("options", p_sel)          # big option lists stripped (the payload win)
        with _patch_urlopen({}) as u:               # no category → all modules
            self.c.get_module_params()
        self.assertNotIn("category", u.call_args[0][0].full_url)

    def test_available_plots_builds_url_and_drops_unset_module(self):
        self.assertIn(("GET", "/api/plots/definitions"), ALLOWED_ROUTES)
        with _patch_urlopen([{"id": "cell_properties"}]) as u:
            out = self.c.get_available_plots("behaviourAnalysis")
        url = u.call_args[0][0].full_url
        self.assertIn("/api/plots/definitions?", url)
        self.assertIn("module=behaviourAnalysis", url)
        self.assertEqual(out[0]["id"], "cell_properties")
        with _patch_urlopen([]) as u:                       # no module → all plots
            self.c.get_available_plots()
        self.assertNotIn("module", u.call_args[0][0].full_url)

    def test_cohort_qc_builds_url_and_drops_unset(self):
        self.assertIn(("GET", "/api/qc/cohort"), ALLOWED_ROUTES)
        with _patch_urlopen({"metrics": {}}) as u:
            self.c.get_cohort_qc("p", "set1", "segment.measureLabels")
        url = u.call_args[0][0].full_url
        self.assertIn("/api/qc/cohort?", url)
        self.assertIn("setUid=set1", url)
        self.assertIn("funName=segment.measureLabels", url)
        self.assertNotIn("threshold", url)            # unset optional dropped
        # explicit optionals are passed through
        with _patch_urlopen({"metrics": {}}) as u:
            self.c.get_cohort_qc("p", "set1", "tracking.bayesian_tracking",
                                 value_name="A", threshold=3.0)
        url = u.call_args[0][0].full_url
        self.assertIn("valueName=A", url)
        self.assertIn("threshold=3.0", url)

    def test_analysis_lineage_builds_url_and_drops_unset(self):
        self.assertIn(("GET", "/api/analysis/lineage"), ALLOWED_ROUTES)
        with _patch_urlopen({"images": []}) as u:
            self.c.get_analysis_lineage("p")                       # whole project — no image/set scope
        url = u.call_args[0][0].full_url
        self.assertIn("/api/analysis/lineage?", url)
        self.assertIn("projectUid=p", url)
        self.assertNotIn("imageUid", url)                          # unset optionals dropped
        self.assertNotIn("setUid", url)
        with _patch_urlopen({"images": []}) as u:
            self.c.get_analysis_lineage("p", image_uid="i1")       # scoped to one image
        self.assertIn("imageUid=i1", u.call_args[0][0].full_url)

    def test_populations_builds_url_and_drops_unset(self):
        self.assertIn(("GET", "/api/analysis/populations"), ALLOWED_ROUTES)
        with _patch_urlopen({"images": []}) as u:
            self.c.get_populations("p", set_uid="s1")              # scoped to one set
        url = u.call_args[0][0].full_url
        self.assertIn("/api/analysis/populations?", url)
        self.assertIn("setUid=s1", url)
        self.assertNotIn("imageUid", url)                          # unset optional dropped

    def test_measure_summary_builds_url_and_drops_unset(self):
        self.assertIn(("GET", "/api/analysis/measures"), ALLOWED_ROUTES)
        with _patch_urlopen({"images": []}) as u:
            self.c.get_measure_summary("p", image_uid="i1")       # scoped to one image
        url = u.call_args[0][0].full_url
        self.assertIn("/api/analysis/measures?", url)
        self.assertIn("imageUid=i1", url)
        self.assertNotIn("setUid", url)                           # unset optional dropped

    def test_behaviour_and_cluster_summary_are_allowed_gets(self):
        self.assertIn(("GET", "/api/analysis/behaviour"), ALLOWED_ROUTES)
        self.assertIn(("GET", "/api/analysis/clusters"), ALLOWED_ROUTES)
        with _patch_urlopen({"images": []}) as u:
            self.c.get_behaviour_summary("p", set_uid="s1")
        self.assertIn("/api/analysis/behaviour?", u.call_args[0][0].full_url)
        self.assertIn("setUid=s1", u.call_args[0][0].full_url)
        with _patch_urlopen({"images": []}) as u:
            self.c.get_cluster_summary("p", image_uid="i1")
        self.assertIn("/api/analysis/clusters?", u.call_args[0][0].full_url)
        self.assertIn("imageUid=i1", u.call_args[0][0].full_url)

    def test_chains_is_an_allowed_project_level_get(self):
        self.assertIn(("GET", "/api/analysis/chains"), ALLOWED_ROUTES)
        with _patch_urlopen({"templates": [], "runs": []}) as u:
            self.c.get_chains("p")
        url = u.call_args[0][0].full_url
        self.assertIn("/api/analysis/chains?", url)
        self.assertIn("projectUid=p", url)
        self.assertNotIn("imageUid", url)                          # project-level: no scope params
        self.assertNotIn("setUid", url)

    def test_repl_api_is_an_allowed_project_independent_get(self):
        self.assertIn(("GET", "/api/repl/api"), ALLOWED_ROUTES)
        with _patch_urlopen({"api": [{"name": "pop_df"}], "doc": "cookbook"}) as u:
            out = self.c.get_repl_api()
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertTrue(req.full_url.endswith("/api/repl/api"))     # no query params
        self.assertEqual(out["api"][0]["name"], "pop_df")

    def test_session_briefing_builds_url(self):
        self.assertIn(("GET", "/api/observer/briefing"), ALLOWED_ROUTES)
        with _patch_urlopen({"projectUid": "p", "flagged": [], "recentLabLog": []}) as u:
            out = self.c.get_session_briefing("p")
        url = u.call_args[0][0].full_url
        self.assertIn("/api/observer/briefing?", url)
        self.assertIn("projectUid=p", url)
        self.assertEqual(out["projectUid"], "p")

    def test_recent_logs_is_an_allowed_get(self):
        self.assertIn(("GET", "/api/logs/recent"), ALLOWED_ROUTES)
        with _patch_urlopen({"logs": [{"level": "error", "message": "boom"}]}) as u:
            out = self.c.get_recent_logs()
        req = u.call_args[0][0]
        self.assertEqual(req.method, "GET")
        self.assertTrue(req.full_url.endswith("/api/logs/recent"))
        self.assertEqual(out["logs"][0]["message"], "boom")

    def test_append_posts_json_body(self):
        with _patch_urlopen({"ok": True}) as u:
            self.c.append_lab_log("p", "Claude", ["hello", "world"])
        req = u.call_args[0][0]
        self.assertEqual(req.method, "POST")
        self.assertEqual(req.get_header("Content-type"), "application/json")
        self.assertEqual(
            json.loads(req.data.decode()),
            {"projectUid": "p", "author": "Claude", "lines": ["hello", "world"]},
        )

    def test_http_error_becomes_apierror_with_detail(self):
        err = urllib.error.HTTPError(
            "http://x", 404, "nf", {}, io.BytesIO(json.dumps({"error": "Project not found"}).encode())
        )
        with mock.patch("cecelia_mcp.client.urllib.request.urlopen", side_effect=err):
            with self.assertRaises(ApiError) as ctx:
                self.c.list_images("nope")
        self.assertEqual(ctx.exception.status, 404)
        self.assertIn("Project not found", ctx.exception.message)

    def test_unreachable_api_becomes_apierror(self):
        with mock.patch(
            "cecelia_mcp.client.urllib.request.urlopen",
            side_effect=urllib.error.URLError("refused"),
        ):
            with self.assertRaises(ApiError) as ctx:
                self.c.read_lab_log("p")
        self.assertEqual(ctx.exception.status, 0)
        self.assertIn("cannot reach Cecelia API", ctx.exception.message)


if __name__ == "__main__":
    unittest.main()
