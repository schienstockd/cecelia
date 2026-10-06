"""Task discovery: the `CECELIA_MCP_DISCOVERY` toggle and `get_task_catalogue` (TASK_DISCOVERY_PLAN)."""
import json
import os
import pathlib
import re
import subprocess
import sys
import unittest
from unittest import mock

from cecelia_mcp import discovery
from cecelia_mcp.client import CeceliaClient

from tests.test_client import _patch_urlopen

REPO = pathlib.Path(__file__).resolve().parents[2]
MCP_DIR = REPO / "mcp"


def _spec(fun, **kw):
    return {"fun_name": fun, "label": fun.split(".")[1], "params": [], **kw}


DEFS = {
    "segment": [_spec("segment.cellpose", purpose="Find cells", useWhen=["Round cells"], notWhen=[])],
    "cleanupImages": [
        _spec("cleanupImages.denoise", purpose="Remove shot noise",
              useWhen=["Photon-limited", {"text": "After Drift correction", "check": "driftCorrected"}],
              notWhen=["Bright, saturated signal"]),
        _spec("cleanupImages.smooth", purpose="Smooth"),
    ],
    "importImages": [_spec("importImages.remove", hidden=True, purpose="x")],
    "myPlugin": [_spec("myPlugin.thing", purpose="A plugin's own")],
}


class DiscoveryEnabledTest(unittest.TestCase):
    def test_default_and_on_are_on(self):
        for v in ("", "on", "ON", " on "):
            with mock.patch.dict(os.environ, {discovery.DISCOVERY_ENV: v}):
                self.assertTrue(discovery.discovery_enabled())

    def test_off_is_off(self):
        with mock.patch.dict(os.environ, {discovery.DISCOVERY_ENV: "off"}):
            self.assertFalse(discovery.discovery_enabled())

    def test_a_typo_fails_loudly(self):
        # an experiment arm must never run silently as the other arm
        with mock.patch.dict(os.environ, {discovery.DISCOVERY_ENV: "of"}):
            with self.assertRaises(ValueError):
                discovery.discovery_enabled()


class TaskCatalogueTest(unittest.TestCase):
    def test_grouped_by_stage_in_pipeline_order_hidden_left_out(self):
        out = discovery.task_catalogue(DEFS)
        self.assertEqual([s["stage"] for s in out["stages"]], ["cleanup", "segment", "myPlugin"])
        cleanup = out["stages"][0]["tasks"]
        self.assertEqual([t["fun_name"] for t in cleanup], ["cleanupImages.denoise", "cleanupImages.smooth"])
        self.assertEqual(cleanup[0]["notWhen"], ["Bright, saturated signal"])
        self.assertEqual(cleanup[0]["useWhen"], ["Photon-limited", "After Drift correction"])
        self.assertEqual(cleanup[1]["useWhen"], [])           # absent → empty list, same shape
        self.assertNotIn("params", cleanup[0])
        funs = [t["fun_name"] for s in out["stages"] for t in s["tasks"]]
        self.assertNotIn("importImages.remove", funs)

    def test_stage_narrows_and_an_unknown_one_names_the_known(self):
        out = discovery.task_catalogue(DEFS, "segment")
        self.assertEqual([s["stage"] for s in out["stages"]], ["segment"])
        with self.assertRaises(ValueError) as cm:
            discovery.task_catalogue(DEFS, "nope")
        self.assertIn("cleanup", str(cm.exception))

    def test_every_builtin_category_has_a_stage(self):
        # a built-in module missing from `_STAGES` would sort after every real stage as its own group
        tasks = REPO / "app" / "src" / "tasks"
        cats = {p.parent.name for p in tasks.glob("*/*.json")} - {"fragments", "testTasks"}
        self.assertEqual(sorted(c for c in cats if c not in discovery._STAGE_OF), [])

    def test_agrees_with_the_lineage_stages(self):
        # the Julia twin (`_stage_of`, app/src/ai/lineage.jl) names the same stage for every category it
        # knows. The ORDER is the plan's (Decision 7: behaviour before cluster, since Cluster tracks
        # reads HMM states); lineage's rollup order puts cluster first.
        src = (REPO / "app" / "src" / "ai" / "lineage.jl").read_text(encoding="utf-8")
        body = src[src.index("function _stage_of"):src.index("const _LINEAGE_STAGE_ORDER")]
        julia = {}
        for line in body.splitlines():
            m = re.search(r'&& return "(\w+)"', line)
            for cat in re.findall(r'cat == "(\w+)"', line):
                julia[cat] = m.group(1)
        self.assertGreater(len(julia), 5)
        self.assertEqual({c: discovery.stage_of(c) for c in julia}, julia)

    def test_client_reads_the_definitions_route(self):
        c = CeceliaClient(base_url="http://x")
        with _patch_urlopen(DEFS) as u:
            out = c.get_task_catalogue("cleanup")
        self.assertIn("/api/tasks/definitions", u.call_args[0][0].full_url)
        self.assertEqual(out["stages"][0]["stage"], "cleanup")


class ModuleParamsFieldsTest(unittest.TestCase):
    def _params(self, env):
        c = CeceliaClient(base_url="http://x")
        with mock.patch.dict(os.environ, {discovery.DISCOVERY_ENV: env}), _patch_urlopen(DEFS):
            return c.get_module_params(fun_name="cleanupImages.denoise")["cleanupImages"][0]

    def test_on_carries_the_fields(self):
        spec = self._params("on")
        self.assertEqual(spec["purpose"], "Remove shot noise")
        self.assertEqual(spec["notWhen"], ["Bright, saturated signal"])
        # a checked line leaves the MCP as its text — the check is a GUI-only advisory
        self.assertEqual(spec["useWhen"], ["Photon-limited", "After Drift correction"])

    def test_off_strips_them(self):
        spec = self._params("off")
        for k in discovery.DISCOVERY_FIELDS:
            self.assertNotIn(k, spec)


class ServerToggleTest(unittest.TestCase):
    """Registration happens at import, so each arm is a fresh interpreter."""

    def _server_view(self, env: str) -> dict:
        code = ("import asyncio, json; from cecelia_mcp import server, guidance; "
                "names = sorted(t.name for t in asyncio.run(server.mcp.list_tools())); "
                "print(json.dumps({'names': names, 'guidance': guidance.BRIEFING_GUIDANCE, "
                "'params_doc': server.get_module_params.__doc__}))")
        out = subprocess.run([sys.executable, "-c", code], cwd=MCP_DIR, capture_output=True, text=True,
                             encoding="utf-8", check=True,
                             env={**os.environ, "PYTHONPATH": str(MCP_DIR), discovery.DISCOVERY_ENV: env})
        return json.loads(out.stdout)

    def test_on_registers_the_catalogue_and_says_when_to_read_it(self):
        view = self._server_view("on")
        self.assertIn("get_task_catalogue", view["names"])
        self.assertIn("get_task_catalogue", view["guidance"])
        self.assertNotIn("{catalogue}", view["guidance"])
        self.assertIn("useWhen", view["params_doc"])

    def test_off_leaves_it_unregistered_and_unmentioned(self):
        view = self._server_view("off")
        self.assertNotIn("get_task_catalogue", view["names"])
        self.assertIn("get_module_params", view["names"])
        self.assertNotIn("get_task_catalogue", view["guidance"])
        self.assertNotIn("{catalogue}", view["guidance"])
        self.assertNotIn("useWhen", view["params_doc"])   # the tool text does not describe absent fields
        self.assertNotIn("{discovery}", view["params_doc"])


if __name__ == "__main__":
    unittest.main()
