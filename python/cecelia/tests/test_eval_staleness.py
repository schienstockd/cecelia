"""Tests for `cecelia.effectiveness.eval_staleness` — the recital console's stopped-eval warning.

Design: docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md → Decision 13 / phase 4. Run with `pixi run test-py`.
"""
import datetime as dt
import json
import pathlib
import tempfile
import unittest

from cecelia.effectiveness.eval_staleness import eval_record_warning


class EvalStalenessTest(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.store = pathlib.Path(tmp.name) / "eval-runs"
        self.store.mkdir()

    def _record(self, date, kind="pass"):
        (self.store / f"{date}.json").write_text(json.dumps({"kind": kind}), encoding="utf-8")

    def test_fires_on_an_old_store(self):
        self._record("2026-09-01")
        msg = eval_record_warning(self.store, today=dt.date(2026, 9, 15))
        self.assertIn("no run record for 14 days", msg)

    def test_silent_on_a_fresh_store(self):
        self._record("2026-09-01")
        self._record("2026-09-08")
        self.assertIsNone(eval_record_warning(self.store, today=dt.date(2026, 9, 17)))   # 9 = 7 + 2

    def test_a_recent_failure_says_so(self):
        self._record("2026-09-08", kind="failure")
        self.assertIn("pass failed", eval_record_warning(self.store, today=dt.date(2026, 9, 9)))

    def test_no_store_is_silent(self):
        self.assertIsNone(eval_record_warning(self.store.parent / "missing"))

    def test_files_that_are_not_dated_records_are_ignored(self):
        (self.store / "notes.json").write_text("{}", encoding="utf-8")
        self.assertIsNone(eval_record_warning(self.store, today=dt.date(2026, 9, 9)))


if __name__ == "__main__":
    unittest.main()
