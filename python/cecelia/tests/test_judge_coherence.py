"""Tests for `scripts/judge/coherence.py` — the weekly judge's whole-repo coherence counts.

The scans run over a temp tree; the external tools (jscpd, madge, vulture) get a fake runner, so
nothing spawns npx or uvx. Run with `pixi run test-py`.
"""
from __future__ import annotations

import importlib.util
import pathlib
import subprocess
import tempfile
import unittest
from unittest import mock

_REPO = pathlib.Path(__file__).resolve().parents[3]


def _load():
    spec = importlib.util.spec_from_file_location("coherence", _REPO / "scripts" / "judge" / "coherence.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class _Tree(unittest.TestCase):
    def setUp(self):
        self.c = _load()
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.root = pathlib.Path(self._tmp.name)
        (self.root / "pixi.toml").write_text("[tasks]\n", encoding="utf-8")

    def write(self, rel: str, text: str) -> None:
        p = self.root / rel
        p.parent.mkdir(parents=True, exist_ok=True)
        p.write_text(text, encoding="utf-8")


class RoutesTest(_Tree):
    def test_a_route_counts_only_when_nothing_outside_tests_names_it(self):
        self.write("api/src/server.jl", '"/api/a" => f, "/api/b/x" => g, "/api/c" => h, "/api/d" => k,')
        self.write("frontend/src/x.ts", "fetch('/api/a?id=1'); fetch(`${apiBase}/api/b/x`)")
        self.write("mcp/m.py", 'get(f"/api/c/{uid}")')
        self.write("frontend/src/x.test.ts", "fetch('/api/d')")   # a test is not a caller
        self.assertEqual(self.c.count_routes_uncalled(self.root), 1)

    def test_a_route_marked_arch_exempt_is_not_counted(self):
        self.write("api/src/server.jl", '"/api/a" => f,   # ARCH-EXEMPT: a smoke test\n"/api/b" => g,\n')
        self.assertEqual(self.c.count_routes_uncalled(self.root), 1)

    def test_a_route_named_only_in_a_comment_or_docstring_is_uncalled(self):
        self.write("api/src/server.jl", '"/api/a" => f, "/api/b" => g, "/api/c" => h,')
        self.write("python/cecelia/r.py", '"""Called by `/api/a` when importing."""\n# see "/api/b"\nx = 1\n')
        self.write("frontend/src/x.ts", "// posts to '/api/c'\nconst y = 2\n")
        self.assertEqual(self.c.count_routes_uncalled(self.root), 3)


class InventoryTest(_Tree):
    def test_a_cited_name_counts_when_no_source_file_has_it(self):
        self.write("docs/inventory/" + "a.md", "- `real_helper(x)`, `gone_helper`, `createLogger()`, `plain`")
        self.write("app/src/a.jl", "real_helper(x) = x")
        # `createLogger` is a dependency's; `plain` has no underscore and no call, so it isn't a citation
        self.assertEqual(self.c.count_inventory_stale(self.root), 1)


class ToolsTest(_Tree):
    def test_vulture_counts_dead_names_but_not_unused_imports(self):
        out = ("a.py:1: unused import 'x' (90% confidence)\n"
               "a.py:2: unused variable 'y' (100% confidence)\n")
        fake = mock.Mock(return_value=subprocess.CompletedProcess([], 3, out, ""))
        with mock.patch.object(self.c.shutil, "which", lambda name: f"/bin/{name}"):
            self.assertEqual(self.c.count_py_dead(self.root, fake), 1)

    def test_a_missing_tool_leaves_its_count_empty_and_the_rest_count(self):
        with mock.patch.object(self.c.shutil, "which", lambda _name: None):
            got = self.c.run(self.root)
        self.assertIsNone(got["counts"]["clones"])
        self.assertIn("not on PATH", got["errors"]["clones"])
        self.assertEqual(got["counts"]["routes_uncalled"], 0)


class TrendTest(unittest.TestCase):
    def setUp(self):
        self.c = _load()

    def rec(self, **counts):
        return {"coherence": {"counts": counts}}

    def test_rising_needs_two_rises_in_a_row(self):
        got = self.c.trend({"counts": {"clones": 12, "cycles": 3}},
                           [self.rec(clones=10, cycles=3), self.rec(clones=11, cycles=2)])
        self.assertEqual(got["previous"], {"clones": 11, "cycles": 2})
        self.assertEqual(got["rising"], ["clones"])   # cycles rose once, after a fall

    def test_a_first_pass_has_no_previous_and_a_missing_count_never_rises(self):
        self.assertNotIn("previous", self.c.trend({"counts": {"clones": 5}}, [{}]))
        got = self.c.trend({"counts": {"clones": None}}, [self.rec(clones=1), self.rec(clones=2)])
        self.assertNotIn("rising", got)


if __name__ == "__main__":
    unittest.main()
