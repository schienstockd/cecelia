"""The changelog check: a changed task default needs a CHANGELOG line (`changelog_check.py`)."""
from __future__ import annotations

import os
import pathlib
import tempfile
import unittest
from unittest import mock

from cecelia.effectiveness import read_events
from cecelia.effectiveness.changelog_check import default_changes, format_section, run_changelog_check


def _diff(path: str, removed: list[str], added: list[str], *, new: bool = False) -> str:
    head = f"diff --git a/{path} b/{path}\n" + ("new file mode 100644\n" if new else "")
    body = "".join(f"-{r}\n" for r in removed) + "".join(f"+{a}\n" for a in added)
    return (head + f"--- a/{path}\n+++ b/{path}\n"
            f"@@ -10,{len(removed) + 1} +10,{len(added) + 1} @@\n   \"step\": 0.25,\n{body}")


class DefaultChangesTest(unittest.TestCase):
    def test_a_spec_default_that_moved_is_one_change(self):
        # 34c919f6: the embedding blur's spec default, 0.5 -> 1.5
        (c,) = default_changes(_diff("app/src/tasks/segment/coastal.json",
                                     ['              "default": 0.5,'], ['              "default": 1.5,']))
        self.assertEqual((c.old, c.new), ("0.5", "1.5"))

    def test_a_value_inside_a_group_default_block_counts(self):
        # 92e5feb8: the models group's own default entry
        (c,) = default_changes(_diff("app/src/tasks/segment/coastal.json",
                                     ['          "embeddingBlurSigma": 0.5,'], ['          "embeddingBlurSigma": 1.5,']))
        self.assertEqual((c.key, c.old, c.new), ("embeddingBlurSigma", "0.5", "1.5"))

    def test_metadata_arrays_new_params_and_tests_are_quiet(self):
        quiet = [
            _diff("app/src/tasks/segment/coastal.json", ['  "max": 3.0,'], ['  "max": 5.0,']),           # metadata
            _diff("app/src/tasks/x/a.json", ['  "useWhen": ["a"],'], ['  "useWhen": ["b"],']),         # array
            _diff("app/src/tasks/x/a.json", [], ['  "default": 2,']),                                  # no key: not a param
            _diff("app/src/tasks/x/new.json", ['  "default": 1,'], ['  "default": 2,'], new=True),    # a new file
            _diff("python/cecelia/tests/test_x.py", ["p.get('k', 1)"], ["p.get('k', 2)"]),             # a test
        ]
        for d in quiet:
            self.assertEqual(default_changes(d), [], d)

    def test_a_new_parameter_on_an_existing_task_is_listed(self):
        # #1705: flowThreshold arrived at 1.0 where cellpose had used its own 0.4; no old line to compare
        (c,) = default_changes(_diff("app/src/tasks/segment/cellpose.json", [],
                                     ['              "key": "flowThreshold",', '              "default": 1.0,']))
        self.assertEqual((c.key, c.new_param), ("flowThreshold", True))
        self.assertIn("new parameter `flowThreshold`", format_section([c], noted=False))
        moved = _diff("app/src/tasks/x/a.json", ['  "key": "a",'], ['  "key": "a",'])   # an edited, not a new, param
        self.assertEqual(default_changes(moved), [])

    def test_a_python_fallback_for_the_same_key_counts(self):
        (c,) = default_changes(_diff("python/cecelia/utils/coastal_utils.py",
                                     ["  s = mp.get('embeddingBlurSigma', 0.5)"], ["  s = mp.get('embeddingBlurSigma', 1.5)"]))
        self.assertEqual((c.key, c.old, c.new), ("embeddingBlurSigma", "0.5", "1.5"))


class RunTest(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.log = pathlib.Path(tmp.name) / "events.jsonl"
        env = mock.patch.dict(os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.log)})
        env.start()
        self.addCleanup(env.stop)
        self.change = _diff("app/src/tasks/segment/coastal.json", ['  "default": 0.5,'], ['  "default": 1.5,'])

    def _row(self):
        (row,) = [e["payload"] for e in read_events(self.log) if e["event"] == "changelog_check_run"]
        return row

    def test_a_default_with_no_changelog_line_warns(self):
        body = run_changelog_check(self.change, branch_noted=lambda: False)
        self.assertIn("coastal.json:11` — default `0.5` → `1.5`", body)
        self.assertIn("Read this first", body)
        self.assertEqual((self._row()["defaults_changed"], self._row()["warnings_emitted"]), (1, 1))

    def test_a_changelog_line_in_the_diff_or_on_the_branch_quiets_it(self):
        with_line = self.change + _diff("CHANGELOG.md", [], ["- **Coastal:** embedding blur 1.5 (#1). Re-run."])
        self.assertIn("`CHANGELOG.md` updated", run_changelog_check(with_line, branch_noted=lambda: False))
        self.assertIn("`CHANGELOG.md` updated", run_changelog_check(self.change, branch_noted=lambda: True))

    def test_no_default_changed_skips(self):
        body = run_changelog_check(_diff("app/src/tasks/x/a.json", ['  "label": "A",'], ['  "label": "B",']))
        self.assertEqual(body, "_Changelog check: skipped — no default changed_")
        self.assertEqual(self._row()["defaults_changed"], 0)


if __name__ == "__main__":
    unittest.main()
