"""The autonomous surface's guard: project lock, route allow-list, output-name prefix, spec lookup."""
import os
import struct
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


class Histogram(unittest.TestCase):
    def test_decode_and_bimodal_bins(self):
        vals = [1.0] * 50 + [9.0] * 30
        raw = b"".join(struct.pack("<2f", v, 0.0) for v in vals)
        xs, ys = au.decode_pairs(raw)
        self.assertEqual(len(xs), 80)
        h = au.histogram(xs, bins=4)
        self.assertEqual((h["n"], h["min"], h["max"]), (80, 1.0, 9.0))
        self.assertEqual([b["n"] for b in h["bins"]], [50, 0, 0, 30])

    def test_empty_and_nan(self):
        self.assertEqual(au.histogram([float("nan")]), {"n": 0})


if __name__ == "__main__":
    unittest.main()
