"""Unit tests for the mechanical maintainability lint — `python/cecelia/effectiveness/maintainability_lint.py`
— and the shared diff parser it builds on (`git_context.parse_diff`)."""
import json
import os
import pathlib
import tempfile
import unittest
from unittest import mock

from cecelia.effectiveness.git_context import parse_diff
from cecelia.effectiveness.maintainability_lint import (
    comment_text,
    format_section,
    is_task_file,
    lint_diff,
    looks_like_uid,
    run_maintainability_lint,
)


def _diff(path: str, added: list[str], *, context: list[str] = (), removed: list[str] = (),
          new: bool = False) -> str:
    """A one-hunk staged diff for `path`: context lines, then removals, then additions."""
    head = f"diff --git a/{path} b/{path}\n"
    head += "new file mode 100644\n--- /dev/null\n" if new else f"--- a/{path}\n"
    head += f"+++ b/{path}\n@@ -1,{len(context) + len(removed)} +1,{len(context) + len(added)} @@\n"
    body = [f" {c}" for c in context] + [f"-{r}" for r in removed] + [f"+{a}" for a in added]
    return head + "\n".join(body) + "\n"


def _lint(diff: str, lines: dict[str, int] | None = None):
    return lint_diff(diff, after_lines=lambda p: (lines or {}).get(p))


class ParseDiffTest(unittest.TestCase):
    def test_files_hunks_and_new_side_line_numbers(self):
        diff = _diff("a.jl", ["x", "y"], context=["c"], removed=["old"]) + _diff("b.ts", ["z"], new=True)
        a, b = parse_diff(diff)
        self.assertEqual((a.path, a.is_new, b.path, b.is_new), ("a.jl", False, "b.ts", True))
        self.assertEqual([(ln.text, ln.lineno) for ln in a.added], [("x", 2), ("y", 3)])
        self.assertEqual([ln.text for ln in a.removed], ["old"])

    def test_header_lines_are_not_body(self):
        # `--- a/…` / `+++ b/…` sit before the first `@@` and must not count as a removal / addition.
        [fd] = parse_diff(_diff("a.jl", ["x"]))
        self.assertEqual(([ln.text for ln in fd.added], fd.removed), (["x"], []))


class CommentTextTest(unittest.TestCase):
    def test_hash_and_slash_comments(self):
        self.assertEqual(comment_text("a.jl", "    # hello"), "hello")
        self.assertEqual(comment_text("a.py", "x = 1  # trailing"), "trailing")
        self.assertEqual(comment_text("a.ts", "  // hello"), "hello")
        self.assertEqual(comment_text("a.ts", "const x = 1 // trailing"), "trailing")

    def test_code_and_strings_are_not_comments(self):
        self.assertIsNone(comment_text("a.jl", 'x = "a # b"'))
        self.assertIsNone(comment_text("a.ts", "const u = 'http://x'"))
        self.assertIsNone(comment_text("a.jl", "x = 1"))


class DatasetUidTest(unittest.TestCase):
    # Every real uid the 600-PR replay found, plus the doc's own examples. The filter was tuned on
    # that replay, so this list is the guard against a later tweak dropping a real one.
    REAL = ("fXgbTl 4kS67f 2h06xA Dml3RG f8gzA2 x4E5HU VJy1Nx SispLk nG1jSi ttRMjQ p6t4mC "
            "c91ICQ 3w4IY5 1SqevM p6T4MC EaMaVq 35uedD WIaUjL").split()
    # The word shapes the same replay flagged before the filter was tightened.
    NOISE = "GitHub SetBar UiMark show3D cpSAM2 setAll useApi UInt16 abcdef ABCDEF".split()

    def test_real_uids_flagged(self):
        self.assertEqual([t for t in self.REAL if not looks_like_uid(t)], [])

    def test_word_shapes_not_flagged(self):
        self.assertEqual([t for t in self.NOISE if looks_like_uid(t)], [])


class IncidentHistoryTest(unittest.TestCase):
    def _checks(self, path, added, **kw):
        return [f.check for f in _lint(_diff(path, added, **kw))]

    def test_each_pattern(self):
        self.assertEqual(self._checks("app/src/x.jl", ["# a live turn on 4kS67f came back"]),
                         ["incident:dataset_uid"])
        self.assertEqual(self._checks("frontend/src/x.ts", ["// Dominik, 2026-09-10: keep it"]),
                         ["incident:dated_authorship"])
        self.assertEqual(self._checks("app/src/x.jl", ["# same class as commit 860da24b"]),
                         ["incident:commit_sha"])
        self.assertEqual(self._checks("frontend/src/x.ts", ["// see P9 slice 4"]),
                         ["incident:phase_code"])

    def test_only_added_comment_lines(self):
        self.assertEqual(self._checks("app/src/x.jl", ['uid = "4kS67f"']), [])
        self.assertEqual(self._checks("app/src/x.jl", ["x = 1"], context=["# on 4kS67f"]), [])

    def test_tests_and_docs_are_skipped(self):
        self.assertEqual(self._checks("python/cecelia/tests/test_x.py", ["# fixture 4kS67f"]), [])
        self.assertEqual(self._checks("notes.md", ["on 4kS67f"]), [])

    def test_constant_provenance_is_protected(self):
        # `MAINTAINABILITY.md` → *No incident history*: a measurement defending the number is kept.
        added = ["// Measured on f8gzA2, 2026-09-04: 0.12 s per plane", "export const GATED_SEC_PER_PLANE = 0.12"]
        self.assertEqual(self._checks("frontend/src/tasks/smoothVis.ts", added), [])

    def test_exempt_marker_on_line_or_above(self):
        self.assertEqual(self._checks("app/src/x.jl", ["# 4kS67f  MAINT-EXEMPT: parser example"]), [])
        self.assertEqual(self._checks("app/src/x.jl", ["# MAINT-EXEMPT: parser example", "# token 3w4IY5"]), [])
        self.assertEqual(self._checks("app/src/x.jl", ["# MAINT-EXEMPT:", "# token 3w4IY5"]),
                         ["incident:dataset_uid"])


class SizeTest(unittest.TestCase):
    TASK = "app/src/tasks/segment/cellpose.jl"

    def _size(self, path, added, after, **kw):
        return [f.detail for f in _lint(_diff(path, ["x"] * added, **kw), {path: after}) if f.check == "size"]

    def test_crossing_the_limit(self):
        self.assertEqual(self._size(self.TASK, 43, 242), ["199 → 242 lines, crosses 200"])

    def test_growth_past_the_limit(self):
        self.assertEqual(self._size(self.TASK, 57, 267), ["210 → 267 lines, +57 past 200"])
        self.assertEqual(self._size(self.TASK, 19, 400), [])  # old debt, small change: quiet

    def test_under_the_limit(self):
        self.assertEqual(self._size(self.TASK, 50, 200), [])

    def test_task_files_only(self):
        self.assertTrue(is_task_file(self.TASK))
        for path in ("app/src/tasks/task.jl", "app/src/tasks/chain/execute.jl",
                     "app/src/tasks/scheduler/run.jl", "app/src/tasks/task/spec.jl",
                     "app/src/tasks/testTasks/set_task.jl", "app/src/gating/x.jl"):
            self.assertFalse(is_task_file(path), path)


class FormatSectionTest(unittest.TestCase):
    def test_tails(self):
        self.assertEqual(format_section([], files_checked=0), "_Maintainability lint: skipped — no source files_")
        self.assertEqual(format_section([], files_checked=3), "_Maintainability lint: run — nothing flagged_")

    def test_warnings_name_location_and_fix(self):
        section = format_section(_lint(_diff("app/src/x.jl", ["# on 4kS67f"])), files_checked=1)
        self.assertIn("`app/src/x.jl:1`", section)
        self.assertIn("dataset uid", section)
        self.assertIn("MAINT-EXEMPT", section)
        self.assertTrue(section.endswith("_Maintainability lint: run_"))


class RunMaintainabilityLintTest(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.log_path = pathlib.Path(self._tmp.name) / "events.jsonl"
        patch = mock.patch.dict(os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.log_path)})
        patch.start()
        self.addCleanup(patch.stop)

    def test_emits_one_event_with_the_flagged_locations(self):
        diff = _diff("app/src/x.jl", ["# on 4kS67f"]) + _diff("notes.md", ["prose"])
        run_maintainability_lint(diff, after_lines=lambda p: None, branch="feat/x")
        [event] = [json.loads(ln) for ln in self.log_path.read_text(encoding="utf-8").splitlines()]
        self.assertEqual(event["event"], "maintainability_lint_run")
        self.assertEqual(event["branch"], "feat/x")
        payload = event["payload"]
        self.assertEqual((payload["files_checked"], payload["warnings_emitted"]), (1, 1))
        self.assertEqual(payload["findings"], [{"check": "incident:dataset_uid", "path": "app/src/x.jl",
                                                "line": 1, "detail": "4kS67f"}])


if __name__ == "__main__":
    unittest.main()
