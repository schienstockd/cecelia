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


class Overwrite(unittest.TestCase):
    """An unattended run may only replace its OWN outputs: `overwrite: true` needs the session prefix
    on every name the task writes, and is refused outright when there is no prefix."""
    SPEC = {"task": "cellposeSegment", "fun_name": "segment.cellpose", "params": [
        {"key": "outputValueName", "type": "valueNameInput", "namespace": "labels", "default": "default"},
        {"key": "overwrite", "type": "bool", "default": False},
    ]}

    def test_refused_outright_without_a_prefix(self):
        bad = au.output_name_violations(self.SPEC, {"outputValueName": "agentA", "overwrite": True}, "")
        self.assertEqual(len(bad), 1)
        self.assertIn("overwrite=true is refused", bad[0])
        self.assertIn("choose a new output name", bad[0])
        # without the toggle, no prefix still means no check at all
        self.assertEqual(au.output_name_violations(self.SPEC, {"outputValueName": "default"}, ""), [])

    def test_refused_on_a_name_outside_the_prefix(self):
        bad = au.output_name_violations(self.SPEC, {"outputValueName": "default", "overwrite": True}, "agent")
        self.assertTrue(any("overwrite=true on outputValueName='default' is refused" in b for b in bad))
        self.assertTrue(any("choose a new output name" in b for b in bad))

    def test_allowed_on_the_sessions_own_output(self):
        self.assertEqual(au.output_name_violations(
            self.SPEC, {"outputValueName": "agentSeg", "overwrite": True}, "agent"), [])

    def test_check_task_raises_lock_violation(self):
        c = au.AutonomousClient()
        c._definitions = {"segment": [self.SPEC]}
        with mock.patch.dict(os.environ, {au.PREFIX_ENV: ""}):
            with self.assertRaises(au.LockViolation) as cm:
                c.check_task("segment.cellpose", {"outputValueName": "x", "overwrite": True})
        self.assertIn("segment.cellpose: overwrite=true is refused", str(cm.exception))


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
        writes = {path for m, path in au.AUTONOMOUS_ROUTES if m != "GET"}
        self.assertTrue(writes)
        self.assertTrue(all(p.startswith("/api/gating/pop/") for p in writes))
        # headless mode's creation routes are a separate set, and additive (no delete/rename/move)
        self.assertFalse(au.NO_GUI_ROUTES & au.AUTONOMOUS_ROUTES)
        self.assertTrue(all(p.endswith(("/create", "/register")) for _, p in au.NO_GUI_ROUTES))


class NoGuiLock(unittest.TestCase):
    """Headless mode (CECELIA_MCP_NO_GUI=1, HEADLESS_MODE_PLAN D2): the lock is the named project plus
    the projects this session created, and creation routes open — only there."""

    def setUp(self):
        au._session_projects.clear()
        self.addCleanup(au._session_projects.clear)

    def test_created_project_joins_the_lock_only_in_headless_mode(self):
        au.allow_session_project("new001")
        with mock.patch.dict(os.environ, {au.PROJECT_ENV: "named1", "CECELIA_MCP_NO_GUI": "1"}):
            au.check_project("named1")
            au.check_project("new001")
            with self.assertRaisesRegex(au.LockViolation, "pixi run headless"):
                au.check_project("other1")
        # the eval sandbox: a session project never widens the lock
        with mock.patch.dict(os.environ, {au.PROJECT_ENV: "named1", "CECELIA_MCP_NO_GUI": ""}):
            with self.assertRaises(au.LockViolation):
                au.check_project("new001")

    def test_new_project_session_starts_empty_and_says_how_to_start(self):
        with mock.patch.dict(os.environ, {au.PROJECT_ENV: "", "CECELIA_MCP_NO_GUI": "1"}):
            with self.assertRaisesRegex(au.LockViolation, "create_project"):
                au.check_project("abc123")
            self.assertEqual(au.allowed_projects(), [])

    def test_creation_routes_open_only_in_headless_mode(self):
        c = au.AutonomousClient("http://127.0.0.1:1")
        with mock.patch.dict(os.environ, {"CECELIA_MCP_NO_GUI": ""}):
            with self.assertRaises(DisallowedRoute):
                c._request("POST", "/api/projects/create", body={"name": "x"})
        with mock.patch.dict(os.environ, {"CECELIA_MCP_NO_GUI": "1", au.PROJECT_ENV: "named1"}), \
                mock.patch.object(au, "http_json", return_value={"uid": "s1"}) as http:
            c._request("POST", "/api/sets/create", body={"projectUid": "named1", "name": "a"})
            self.assertEqual(http.call_count, 1)
            with self.assertRaises(au.LockViolation):        # still inside the lock
                c._request("POST", "/api/sets/create", body={"projectUid": "other1", "name": "a"})

    def test_create_project_adds_it_to_the_lock(self):
        c = au.AutonomousClient("http://x")
        reply = {"project": {"uid": "new001", "name": "Expt", "path": "/p/new001"}}
        with mock.patch.dict(os.environ, {au.PROJECT_ENV: "", "CECELIA_MCP_NO_GUI": "1"}), \
                mock.patch.object(au, "http_json", return_value=reply):
            self.assertEqual(c.create_project("Expt")["projectUid"], "new001")
            au.check_project("new001")

    def test_register_names_the_paths_it_skipped(self):
        import tempfile
        c = au.AutonomousClient("http://x")
        with tempfile.NamedTemporaryFile(suffix=".tif") as f, \
                mock.patch.dict(os.environ, {au.PROJECT_ENV: "named1", "CECELIA_MCP_NO_GUI": "1"}), \
                mock.patch.object(au, "http_json", return_value={"images": [
                    {"uid": "i1", "name": "a", "filepath": f.name}]}) as http:
            got = c.register_images("named1", "s1", [f.name, "/no/such.tif", "rel.tif"])
            self.assertEqual(got["skipped"], ["/no/such.tif", "rel.tif"])
            self.assertEqual(got["images"][0]["imageUid"], "i1")
            self.assertEqual(http.call_args[0][4]["filepaths"], [f.name])
            with self.assertRaisesRegex(au.ApiError, "absolute path"):
                c.register_images("named1", "s1", ["rel.tif"])

    def test_switch_rejects_a_typo(self):
        from cecelia_mcp.no_gui import no_gui_enabled
        with mock.patch.dict(os.environ, {"CECELIA_MCP_NO_GUI": "yes"}):
            with self.assertRaises(ValueError):
                no_gui_enabled()


class NoGuiTools(unittest.TestCase):
    """The creation tools exist only in headless mode: the eval sandbox's agent never sees them."""

    def _tool_names(self, no_gui: str) -> set[str]:
        import importlib
        from cecelia_mcp import autonomous_server
        with mock.patch.dict(os.environ, {"CECELIA_MCP_NO_GUI": no_gui}):
            mod = importlib.reload(autonomous_server)
            names = {t.name for t in asyncio.run(mod.mcp.list_tools())}
            text = mod.INSTRUCTIONS
        importlib.reload(autonomous_server)
        return names, text

    def test_registered_and_named_only_with_the_switch(self):
        creation = {"create_project", "create_set", "register_images"}
        names, text = self._tool_names("")
        self.assertFalse(creation & names)
        self.assertNotIn("register_images", text)
        names, text = self._tool_names("1")
        self.assertTrue(creation <= names)
        for t in creation | {"importImages.omezarr"}:
            self.assertIn(t, text)


class RunChainStoredName(unittest.TestCase):
    def test_runs_under_the_name_the_server_stored(self):
        # create_chain repairs "Drift + segment" to "Drift segment"; an agent that then runs the name
        # it SENT must still start the stored chain — the GET resolves it, chain:run must use the result
        c = au.AutonomousClient("http://x")
        replies = {"/api/chains/get": {"name": "Drift segment", "nodes": []},
                   "/api/chains/runs": {"runs": [{"chainName": "Drift segment", "runId": "r1",
                                                  "createdAt": 9e12}]}}
        with mock.patch.dict(os.environ, {au.PROJECT_ENV: "copy01"}), \
                mock.patch.object(c, "_request", side_effect=lambda m, r, *a, **k: replies[r]), \
                mock.patch.object(c, "_ws_send", return_value=[{"type": "chain:run:started"}]) as ws:
            out = c.run_chain("copy01", "Drift + segment", ["img1"])
        self.assertEqual(ws.call_args[0][0]["chain"], "Drift segment")
        self.assertEqual((out["chain"], out["runId"]), ("Drift segment", "r1"))


class TaskStatesHistoryFallback(unittest.TestCase):
    def test_a_capped_history_is_read_whole_before_giving_up(self):
        c = au.AutonomousClient("http://x")
        c._launched["t1"] = ("tSJpBI", "segment.cellpose", "img1", "2026-10-27T10:00:00")
        row = {"fun": "segment.cellpose", "imageUid": "img1", "at": "2026-10-27T10:00:05", "runStatus": "failed"}
        def reply(method, route, params=None, *a, **k):
            if route == "/api/tasks/history":
                return ({"history": [row], "total": 201, "truncated": False} if params["limit"] == 201
                        else {"history": [], "total": 201, "truncated": True})
            return []
        with mock.patch.object(c, "_request", side_effect=reply):
            self.assertEqual({"t1": "failed"}, c.task_states(["t1"]))


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
