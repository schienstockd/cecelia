"""The autonomous surface's guard: project lock, route allow-list, output-name prefix, spec lookup."""
import asyncio
import base64
import os
import unittest
from unittest import mock

from cecelia_mcp import autonomous as au
from cecelia_mcp.client import DisallowedRoute

DEFS = {"segment": [
    # `task` is the internal id; agents and chains use `fun_name`
    {"task": "cellposeSegment", "fun_name": "segment.cellpose", "scope": "image", "params": [
        {"key": "valueName", "type": "valueNameInput", "namespace": "labels", "default": "default"},
        {"key": "models", "type": "group", "params": [{"key": "cellDiameter", "type": "int"}]},
    ]},
], "tracking": [
    {"task": "bayesianTrackMeasures", "fun_name": "tracking.bayesian_track_measures", "params": [
        {"key": "valueName", "type": "valueNameSelection", "field": "labels", "default": "default"},
        {"key": "popsToTrack", "type": "popSelection", "default": []},
    ]},
]}


class FindSpec(unittest.TestCase):
    def test_matches_fun_name_not_internal_task_id(self):
        self.assertEqual(au.find_spec(DEFS, "segment.cellpose")["task"], "cellposeSegment")
        self.assertIsNone(au.find_spec(DEFS, "segment.cellposeSegment"))
        self.assertIsNone(au.find_spec(DEFS, "nope.cellpose"))


class OutputNames(unittest.TestCase):
    def test_no_prefix_allows_everything(self):
        self.assertEqual(au.output_name_violations(DEFS["segment"][0], {}, ""), [])

    def test_named_output_needs_prefix(self):
        spec = DEFS["segment"][0]
        self.assertTrue(au.output_name_violations(spec, {"valueName": "cells"}, "agent"))
        self.assertEqual(au.output_name_violations(spec, {"valueName": "agentCells"}, "agent"), [])

    def test_downstream_writes_into_input_set_and_pops(self):
        spec = DEFS["tracking"][0]
        self.assertTrue(au.output_name_violations(spec, {"valueName": "OTI"}, "agent"))
        bad = au.output_name_violations(spec, {"valueName": "agentOTI", "popsToTrack": ["OTI/qc"]}, "agent")
        self.assertEqual(len(bad), 1)
        self.assertEqual(au.output_name_violations(
            spec, {"valueName": "agentOTI", "popsToTrack": ["agentOTI/qc", "/qc"]}, "agent"), [])


class Lock(unittest.TestCase):
    def test_unset_lock_refuses_everything(self):
        with mock.patch.dict(os.environ, {au.PROJECT_ENV: ""}):
            with self.assertRaises(au.LockViolation):
                au.check_project("abc123")

    def test_other_project_refused(self):
        with mock.patch.dict(os.environ, {au.PROJECT_ENV: "copy01"}):
            au.check_project("copy01")
            with self.assertRaises(au.LockViolation):
                au.check_project("tSJpBI")

    def test_request_checks_route_then_project(self):
        c = au.AutonomousClient("http://127.0.0.1:1")
        with self.assertRaises(DisallowedRoute):
            c._request("POST", "/api/chains/save", body={"projectUid": "copy01"})
        with mock.patch.dict(os.environ, {au.PROJECT_ENV: "copy01"}):
            with self.assertRaises(au.LockViolation):
                c._request("GET", "/api/gating/stats", {"projectUid": "tSJpBI"})

    def test_write_routes_are_gating_only(self):
        # the one non-gating POST is the correction-plan RECOMMEND, which is pure (save/mount are not here)
        writes = {path for m, path in au.AUTONOMOUS_ROUTES if m != "GET"} - {"/api/correction-plan/recommend"}
        self.assertTrue(writes)
        self.assertTrue(all(p.startswith("/api/gating/pop/") for p in writes))


class RequireMeasured(unittest.TestCase):
    def test_refuses_when_api_resolves_a_different_value_name(self):
        c = au.AutonomousClient("http://x")
        with mock.patch.dict(os.environ, {au.PROJECT_ENV: "copy01"}), \
                mock.patch.object(c, "_request", return_value={"valueName": "default"}) as req:
            with self.assertRaisesRegex(au.ApiError, "no measurements"):
                c.gating_post("/api/gating/pop/add", "copy01", "img", "T", name="pos")
            self.assertEqual(req.call_count, 1)          # never reached the write
        with mock.patch.object(c, "_request", return_value={"valueName": "T"}):
            c._require_measured("copy01", "img", "T")


class GatingPost(unittest.TestCase):
    def test_population_path_is_a_body_field_not_the_route(self):
        c = au.AutonomousClient("http://x")
        with mock.patch.dict(os.environ, {au.PROJECT_ENV: "copy01"}), \
                mock.patch.object(c, "_require_measured"), \
                mock.patch.object(c, "_request", return_value={"ok": True}) as req:
            c.gating_post("/api/gating/pop/set-gate", "copy01", "img", "T", path="/pos", gate={"kind": "rectangle"})
        method, route = req.call_args[0][:2]
        self.assertEqual((method, route), ("POST", "/api/gating/pop/set-gate"))
        self.assertEqual(req.call_args.kwargs["body"]["path"], "/pos")


class GatingPictures(unittest.TestCase):
    def _client(self, reply):
        c = au.AutonomousClient("http://x")
        return c, mock.patch.object(c, "_request", return_value=reply)

    def test_gate_plot_decodes_the_png_and_sends_the_transform(self):
        c, req = self._client({"png": base64.b64encode(b"PNGBYTES").decode(), "n": 3, "gates": []})
        with mock.patch.dict(os.environ, {au.PROJECT_ENV: "copy01"}), \
                mock.patch.object(c, "_require_measured") as measured, req as r:
            png, meta = c.gate_plot("copy01", "img", "T", "mean_intensity_2", "area",
                                    {"kind": "asinh", "cof": 5}, "/qc")
        self.assertEqual((png, meta), (b"PNGBYTES", {"n": 3, "gates": []}))
        measured.assert_called_once()
        method, route, q = r.call_args[0][:3]
        self.assertEqual((method, route), ("GET", "/api/gating/plot-image"))
        self.assertEqual((q["xt"], q["xcof"], q["yt"], q["ycof"], q["pop"]), ("asinh", 5, "asinh", 5, "/qc"))
        self.assertIn((method, route), au.AUTONOMOUS_ROUTES)

    def test_gate_histogram_relays_the_summary_route(self):
        # the numbers come from Julia (`axis_summary` / `grid_summary`) — the client only relays
        c, req = self._client({"n": 3, "x": {"n": 3}, "y": {"n": 3}, "grid": {"n": 3}})
        with mock.patch.dict(os.environ, {au.PROJECT_ENV: "copy01"}), \
                mock.patch.object(c, "_require_measured") as measured, req as r:
            out = c.gate_histogram("copy01", "img", "T", "mean_intensity_2", "area",
                                   {"kind": "asinh", "cof": 5}, "/qc", 30)
        self.assertEqual(out["grid"], {"n": 3})
        measured.assert_called_once()
        method, route, q = r.call_args[0][:3]
        self.assertEqual((method, route), ("GET", "/api/gating/summary"))
        self.assertEqual((q["xt"], q["xcof"], q["pop"], q["bins"]), ("asinh", 5, "/qc", 30))
        self.assertIn((method, route), au.AUTONOMOUS_ROUTES)

    def test_cells_view_only_sends_what_was_asked(self):
        c, req = self._client({"png": base64.b64encode(b"P").decode(), "t": 4})
        with mock.patch.dict(os.environ, {au.PROJECT_ENV: "copy01"}), \
                mock.patch.object(c, "_require_measured"), req as r:
            c.gate_cells_view("copy01", "img", "T", "/qc", -1, None, "")
            q = r.call_args[0][2]
            self.assertFalse({"t", "channels", "imageVersion"} & set(q))
            c.gate_cells_view("copy01", "img", "T", "/qc", 7, [2, 3], "driftCorrected")
            q = r.call_args[0][2]
        self.assertEqual((q["t"], q["channels"], q["imageVersion"]), (7, "2,3", "driftCorrected"))
        self.assertIn(("GET", "/api/gating/cells-image"), au.AUTONOMOUS_ROUTES)


class GatingPicturesShared(unittest.TestCase):
    def test_observer_and_autonomous_send_the_same_read(self):
        # one request builder and one docstring for both servers — they cannot drift apart
        from cecelia_mcp import autonomous_server, server
        from cecelia_mcp.client import ALLOWED_ROUTES, CeceliaClient
        reply = {"png": base64.b64encode(b"P").decode(), "n": 1}
        obs, auto = CeceliaClient("http://x"), au.AutonomousClient("http://x")
        args = ("copy01", "img", "T", "mean_intensity_0", "area", {"kind": "log", "floor": 2}, "/qc")
        with mock.patch.dict(os.environ, {au.PROJECT_ENV: "copy01"}), \
                mock.patch.object(obs, "_request", return_value=reply) as r1, \
                mock.patch.object(auto, "_request", return_value=reply) as r2, \
                mock.patch.object(auto, "_require_measured"):
            self.assertEqual(obs.gate_plot(*args), auto.gate_plot(*args))
        self.assertEqual(r1.call_args, r2.call_args)
        self.assertIn(("GET", "/api/gating/plot-image"), ALLOWED_ROUTES)
        self.assertIn(("GET", "/api/gating/cells-image"), ALLOWED_ROUTES)
        for mod in (server, autonomous_server):
            tools = {t.name: t for t in asyncio.run(mod.mcp.list_tools())}
            self.assertTrue(tools["gate_plot"].description.startswith("The gate plot as an image"))
            self.assertTrue(tools["gate_cells_view"].description.startswith("The image with population"))


if __name__ == "__main__":
    unittest.main()
