"""The commit hook blocks an agent commit whose result review did not run.

A usage limit makes the reviewer error; recital still prints a body (stamp included), so without
this gate a result-path change went in unreviewed. Kept apart from `test_check_commit_recital.py`,
whose fixtures carry a repro marker the hook's gate 4 trips on when that file is edited.
"""
from __future__ import annotations

import os
import pathlib
import tempfile
import unittest
from unittest import mock

from cecelia.effectiveness import append_event
from cecelia.effectiveness.recital import ERRORED_TAIL, recital_stamp
from cecelia.tests.test_check_commit_recital import _load_hook


class ErroredResultReviewTest(unittest.TestCase):
    def setUp(self):
        self.hook = _load_hook()
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        env = mock.patch.dict(os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(pathlib.Path(self._tmp.name) / "e.jsonl"),
                                           "CLAUDECODE": "1"})
        env.start()
        self.addCleanup(env.stop)
        self.head = "9" * 40
        for name, value in (("_current_head_sha", self.head), ("_current_branch", "feat/x"),
                            ("_current_pr", None), ("_in_sequencer_commit", False)):
            p = mock.patch.object(self.hook, name, return_value=value)
            p.start()
            self.addCleanup(p.stop)
        append_event("convention_check_run", {"duration_s": 1.0}, commit=self.head, branch="feat/x")

    def _message(self, result_tail: str) -> str:
        return (f"fix: thing\n\n_Fanout audit: no fanout audit needed_\n\n{result_tail}\n\n"
                f"{recital_stamp('feat/x', self.head)}\n")

    def test_an_errored_result_review_blocks(self):
        reason = self.hook.agent_recital_gate(self._message(f"_Result review: {ERRORED_TAIL}_"))
        self.assertIn("result review did not run", reason or "")

    def test_a_result_review_with_no_git_dir_blocks_too(self):
        reason = self.hook.agent_recital_gate(self._message("_Result review: not run — no git dir to key the review on_"))
        self.assertIn("result review did not run", reason or "")

    def test_a_skipped_or_run_result_review_passes(self):
        for tail in ("_Result review: skipped — no result paths_", "_Result review: run_",
                     "_Result review: no result review needed_"):
            self.assertIsNone(self.hook.agent_recital_gate(self._message(tail)), tail)

    def test_a_finding_that_quotes_the_tail_does_not_block(self):
        quoted = ("- [fanout-9079fc00] **recital.py:687** — returns `_Result review: not run — no git "
                  "dir to key the review on_`, which the gate misses [**plausible**]\n\n_Result review: run_")
        self.assertIsNone(self.hook.agent_recital_gate(self._message(quoted)))

    def test_another_reviewer_erroring_does_not_block(self):
        self.assertIsNone(self.hook.agent_recital_gate(self._message(f"_Convention check: {ERRORED_TAIL}_")))


if __name__ == "__main__":
    unittest.main()
