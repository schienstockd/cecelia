"""The test value check (`effectiveness/test_value.py`, TEST_VALUE_PLAN.md → P4): which tests a staged
diff adds, the `passes on base` and `untested` findings, their log rows, and recital and the commit
hook knowing the `tv-` slugs. The subprocess seams are faked: no Julia, no mutmut."""
from __future__ import annotations

import importlib.util
import os
import pathlib
import subprocess
import tempfile
import unittest
from unittest import mock

from cecelia.effectiveness import read_events
from cecelia.effectiveness import test_value as tv
from cecelia.effectiveness.git_context import parse_diff
from cecelia.effectiveness.recital import run_recital

_REPO = pathlib.Path(__file__).resolve().parents[3]

_SRC = """diff --git a/python/cecelia/utils/czi_meta.py b/python/cecelia/utils/czi_meta.py
--- a/python/cecelia/utils/czi_meta.py
+++ b/python/cecelia/utils/czi_meta.py
@@ -10,1 +10,1 @@
-    return a
+    return b
"""
_PY_TEST = """diff --git a/python/cecelia/tests/test_czi_meta.py b/python/cecelia/tests/test_czi_meta.py
--- a/python/cecelia/tests/test_czi_meta.py
+++ b/python/cecelia/tests/test_czi_meta.py
@@ -3,0 +4,3 @@
+    def test_the_band(self):
+        self.assertEqual(1, 1)
+
"""
_PY_TEST_TEXT = "import unittest\n\nclass Band(unittest.TestCase):\n    def test_the_band(self):\n        self.assertEqual(1, 1)\n\n    def test_old(self):\n        pass\n"
_JL_TEST = """diff --git a/app/test/suite/acq.jl b/app/test/suite/acq.jl
--- a/app/test/suite/acq.jl
+++ b/app/test/suite/acq.jl
@@ -20,0 +21,2 @@
+    # a comment first
+    @test f(1) == 2
"""
_GLOBS = ["python/cecelia/utils/*"]


class AddedTestsTest(unittest.TestCase):
    def test_the_test_methods_and_suite_files_the_diff_adds(self):
        a = tv.added_tests(parse_diff(_PY_TEST + _JL_TEST), lambda p: _PY_TEST_TEXT)
        self.assertEqual(a.python, {"python/cecelia/tests/test_czi_meta.py": ["cecelia.tests.test_czi_meta.Band.test_the_band"]})
        self.assertEqual(a.julia, ["app/test/suite/acq.jl"])
        self.assertEqual(a.first, ("python/cecelia/tests/test_czi_meta.py", 4))

    def test_a_julia_location_is_the_first_added_test_line(self):
        self.assertEqual(tv.added_tests(parse_diff(_JL_TEST), lambda p: "").first, ("app/test/suite/acq.jl", 22))

    def test_source_is_app_src_and_python_cecelia_not_tests(self):
        self.assertEqual(tv.source_changes(parse_diff(_SRC + _PY_TEST + _JL_TEST)), ["python/cecelia/utils/czi_meta.py"])


class BulletsTest(unittest.TestCase):
    added = tv.Added({"python/cecelia/tests/test_czi_meta.py": ["m.C.test_a"]}, ["app/test/suite/acq.jl"], ("x.py", 4))

    def test_passes_on_base_only_when_no_language_failed_there(self):
        self.assertIn("[**passes on base**]", tv.base_bullet(self.added, {"py": False, "jl": False}))
        self.assertIsNone(tv.base_bullet(self.added, {"py": False, "jl": True}))
        self.assertIsNone(tv.base_bullet(self.added, {}))   # nothing ran: no claim either way

    def test_one_untested_finding_per_function_at_its_def(self):
        rows = [{"file": "f.py", "results": ["killed", "survived", "survived", "survived", "survived"],
                 "survivor_detail": [{"fn": "_band", "def": 140, "line": 143 + i, "orig": "a", "new": "b"} for i in range(4)]
                 + [{"fn": "_other", "def": 200, "line": 201, "orig": "x == 1", "new": "x != 1"}]}]
        out = tv.mutation_bullets(rows)
        self.assertEqual(len(out), 2)
        self.assertTrue(out[0].startswith("- **f.py:140** — `_band`: 4 of 5 mutants"))
        self.assertIn("(+1 more)", out[0])
        self.assertIn("never from this code's own output", out[0])   # the definition rule, every time
        self.assertTrue(out[1].endswith("[**untested**]"))

    def test_lines_nothing_mutated_are_named(self):
        rows = [{"file": "api/src/viewer_api.jl", "lines": 5, "note": "not mutated (not in module Cecelia or python/cecelia)"},
                {"file": "a.jl", "lines": 2, "note": "no mutant on a changed line", "results": []},
                {"file": "b.py", "results": ["killed"]}]
        note = tv.unseen_note(rows + [{"file": "c.jl", "results": ["killed"], "partial": "1 of 5 mutants run before the time budget"}])
        self.assertIn("_Partly mutated: `c.jl` (1 of 5 mutants run before the time budget)._", note)
        self.assertIn("`api/src/viewer_api.jl`", note)
        self.assertNotIn("a.jl", note)
        self.assertNotIn("b.py", note)


class PythonOnBaseTest(unittest.TestCase):
    def test_a_scripts_module_a_test_loads_by_path_is_heads_not_the_changes(self):
        with tempfile.TemporaryDirectory() as d:
            root = pathlib.Path(d)
            git = lambda *a: subprocess.run(["git", "-c", "user.name=t", "-c", "user.email=t@t", *a], cwd=root,   # noqa: E731
                                            check=True, capture_output=True)
            git("init", "-q")
            (root / "scripts").mkdir()
            (root / "scripts/m.py").write_text("X = 1\n", encoding="utf-8")
            (root / "python/pkg/tests").mkdir(parents=True)
            (root / "app").mkdir()
            for f in ("python/pkg/__init__.py", "python/pkg/tests/__init__.py", "app/x.jl"):
                (root / f).write_text("", encoding="utf-8")
            git("add", ".")
            git("commit", "-q", "-m", "base")
            (root / "scripts/m.py").write_text("X = 2\n", encoding="utf-8")   # the change
            test = ("import pathlib, runpy, unittest\n"
                    "class T(unittest.TestCase):\n"
                    "    def test_x(self):\n"
                    "        m = runpy.run_path(str(pathlib.Path(__file__).resolve().parents[3] / 'scripts/m.py'))\n"
                    "        self.assertEqual(m['X'], 2)\n")
            path = "python/pkg/tests/test_m.py"
            self.assertTrue(tv.python_on_base(root, {path: ["pkg.tests.test_m.T.test_x"]}, lambda p: test))


class JuliaOnBaseTest(unittest.TestCase):
    def run_with(self, out_text, sources=("app/src/a.jl",)):
        def fake(cmd, **kw):
            pathlib.Path(cmd[3]).write_text(out_text, encoding="utf-8")   # julia_on_base.jl's OUT.txt
        with mock.patch.object(tv.subprocess, "run", fake), mock.patch.object(tv, "_head_text", lambda r, p: ""):
            return tv.julia_on_base(_REPO, list(sources), ["app/test/suite/acq.jl"])

    def test_a_failing_test_is_a_failure_and_none_is_a_pass(self):
        self.assertTrue(self.run_with("fail app/test/suite/acq.jl:22\n"))
        self.assertFalse(self.run_with(""))

    def test_old_code_that_wont_load_is_no_verdict_even_beside_failures(self):
        # its methods were deleted and never put back: a failure would say nothing about the change
        with self.assertRaisesRegex(tv.CannotJudge, "wouldn't load: app/src/a.jl: LoadError"):
            self.run_with(f"base-error {_REPO}/app/src/a.jl: LoadError: x\nfail app/test/suite/acq.jl:22\n")

    def test_the_module_root_is_never_swapped(self):
        with self.assertRaisesRegex(tv.CannotJudge, "only `app/src/Cecelia.jl` changed"):
            self.run_with("", sources=("app/src/Cecelia.jl",))
        seen = []
        with mock.patch.object(tv.subprocess, "run", lambda cmd, **kw: seen.append(cmd) or
                               pathlib.Path(cmd[3]).write_text("", encoding="utf-8")), \
                mock.patch.object(tv, "_head_text", lambda r, p: ""):
            tv.julia_on_base(_REPO, ["app/src/Cecelia.jl", "app/src/a.jl"], ["app/test/suite/acq.jl"])
        self.assertFalse(any("Cecelia.jl=" in a for a in seen[0]))


class OnBaseCacheTest(unittest.TestCase):
    def test_a_round_over_the_same_change_reuses_the_verdict(self):
        with tempfile.TemporaryDirectory() as d:
            root = pathlib.Path(d)
            subprocess.run(["git", "init", "-q"], cwd=root, check=True)
            added = tv.Added({}, ["app/test/suite/acq.jl"], ("app/test/suite/acq.jl", 1))
            calls = []
            text = {"app/src/a.jl": "f() = 1", "app/test/suite/acq.jl": "@test f() == 1"}

            def jl(root, sources, suites):
                calls.append(1)
                raise tv.CannotJudge("wouldn't load")
            with mock.patch.object(tv, "julia_on_base", jl):
                first = tv.default_on_base(root, added, ["app/src/a.jl"], text.get)
                again = tv.default_on_base(root, added, ["app/src/a.jl"], text.get)
                text["app/test/suite/acq.jl"] = "@test f() == 2"   # the test changed: run again
                tv.default_on_base(root, added, ["app/src/a.jl"], text.get)
            self.assertEqual(first, {"unjudged": {"jl": "wouldn't load"}})
            self.assertEqual(again, {**first, "cached": True})
            self.assertEqual(len(calls), 2)


class RunCheckTest(unittest.TestCase):
    def setUp(self):
        d = tempfile.TemporaryDirectory()
        self.addCleanup(d.cleanup)
        self.log = pathlib.Path(d.name) / "events.jsonl"
        env = mock.patch.dict(os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.log)})
        env.start()
        self.addCleanup(env.stop)

    def run_check(self, diff, *, on_base=None, mutate=None):
        on_base = on_base or (lambda *a: self.fail("on_base ran"))
        mutate = mutate or (lambda root: self.fail("mutation ran"))
        return tv.run_test_value_check(diff, root=_REPO, on_base=on_base, mutate=mutate,
                                       staged=lambda p: _PY_TEST_TEXT, result_globs=_GLOBS, branch="b", commit="c")

    def test_tests_only_or_docs_only_skips_both(self):
        self.assertIn("skipped", self.run_check(_PY_TEST))

    def test_added_tests_passing_on_base_is_a_tagged_finding(self):
        body = self.run_check(_SRC + _PY_TEST, on_base=lambda *a: {"py": False},
                              mutate=lambda root: {"files": []})
        self.assertRegex(body, r"- \[tv-[0-9a-f]{8}\] \*\*python/cecelia/tests/test_czi_meta\.py:4\*\*")
        rows = [e for e in read_events() if e["event"] == "test_value_finding"]
        self.assertEqual([r["payload"]["marker"] for r in rows], ["passes on base"])
        self.assertTrue(rows[0]["payload"]["slug"].startswith("tv-"))
        run = [e for e in read_events() if e["event"] == "test_value_run"][0]["payload"]
        self.assertEqual((run["on_base"], run["findings"]), ({"py": False}, 1))

    def test_a_test_that_fails_on_base_and_killed_mutants_flag_nothing(self):
        body = self.run_check(_SRC + _PY_TEST, on_base=lambda *a: {"py": True},
                              mutate=lambda root: {"files": [{"file": "python/cecelia/utils/czi_meta.py",
                                                              "results": ["killed"], "survivor_detail": []}]})
        self.assertEqual(body, "_Test value: run — nothing flagged_")

    def test_a_language_not_judged_on_base_is_said_and_raises_no_finding(self):
        body = self.run_check(_SRC + _PY_TEST, on_base=lambda *a: {"py": False, "unjudged": {"jl": "it broke"}},
                              mutate=lambda root: {"files": []})
        self.assertIn("_Julia on base: not judged — it broke_", body)
        self.assertIn("[**passes on base**]", body)   # Python's verdict still stands
        body = self.run_check(_SRC + _PY_TEST, on_base=lambda *a: {"unjudged": {"jl": "it broke"}},
                              mutate=lambda root: {"files": []})
        self.assertNotIn("passes on base", body)

    def test_a_broken_check_is_an_error_line_not_a_lost_recital(self):
        def boom(*a):
            raise RuntimeError("julia missing")
        body = self.run_check(_SRC + _PY_TEST, on_base=boom, mutate=lambda root: {"files": []})
        self.assertIn("test value check error** — on base: RuntimeError: julia missing", body)


class RecitalAndHookTest(unittest.TestCase):
    def setUp(self):
        d = tempfile.TemporaryDirectory()
        self.addCleanup(d.cleanup)
        env = mock.patch.dict(os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(pathlib.Path(d.name) / "e.jsonl")})
        env.start()
        self.addCleanup(env.stop)
        self.rounds = pathlib.Path(d.name)

    def test_recital_carries_the_section_and_survives_its_crash(self):
        quiet = lambda prompt: "no fanout audit needed"   # noqa: E731
        body = run_recital(_SRC, claude_runner=quiet, rounds_dir=self.rounds,
                           test_value_check=lambda: "_Test value: run — nothing flagged_")
        self.assertIn("_Test value: run — nothing flagged_", body)

        def crash():
            raise ValueError("bad rows")
        body = run_recital(_SRC, claude_runner=quiet, rounds_dir=self.rounds, test_value_check=crash)
        self.assertIn("ValueError: bad rows", body)
        self.assertIn("_Test value: not run — the reviewer errored_", body)

    def test_the_commit_hook_resolves_a_tv_slug(self):
        spec = importlib.util.spec_from_file_location("hook", _REPO / ".claude/hooks/check_commit_recital.py")
        hook = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(hook)
        self.assertEqual(hook._finding_event_for_slug("tv-0123abcd"), "test_value_finding_resolved")
        self.assertEqual(hook._parse_pairs("[tv-0123abcd: false_positive: refactor]"),
                         [("tv-0123abcd", "false_positive", "refactor")])


if __name__ == "__main__":
    unittest.main()
