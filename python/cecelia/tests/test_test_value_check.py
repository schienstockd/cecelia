"""The test value check (`effectiveness/test_value.py`, TEST_VALUE_PLAN.md → P4): which tests a staged
diff adds, the `passes on base` and `untested` findings, their log rows, and recital and the commit
hook knowing the `tv-` slugs. The subprocess seams are faked: no Julia, no mutmut."""
from __future__ import annotations

import importlib.util
import os
import pathlib
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
        note = tv.unseen_note(rows)
        self.assertIn("`api/src/viewer_api.jl`", note)
        self.assertNotIn("a.jl", note)
        self.assertNotIn("b.py", note)


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
