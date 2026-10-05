"""Tests for `cecelia.effectiveness.judge_staleness` — the recital console's stopped-judge warning.

Design: docs/ai-assist/WEEKLY_JUDGE.md. Run with `pixi run test-py`.
"""
import datetime as dt
import json
import pathlib
import tempfile
import unittest

from cecelia.effectiveness.judge_staleness import judge_record_warning


class JudgeStalenessTest(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.store = pathlib.Path(tmp.name) / "judge-runs"
        self.store.mkdir()

    def _record(self, date, kind="pass"):
        (self.store / f"{date}.json").write_text(json.dumps({"kind": kind}), encoding="utf-8")

    def test_fires_on_an_old_store(self):
        self._record("2026-09-01")
        msg = judge_record_warning(self.store, today=dt.date(2026, 9, 15))
        self.assertIn("no run record for 14 days", msg)

    def test_silent_on_a_fresh_store(self):
        self._record("2026-09-01")
        self._record("2026-09-08")
        self.assertIsNone(judge_record_warning(self.store, today=dt.date(2026, 9, 17)))   # 9 = 7 + 2

    def test_a_recent_failure_says_so(self):
        self._record("2026-09-08", kind="failure")
        self.assertIn("pass failed", judge_record_warning(self.store, today=dt.date(2026, 9, 9)))

    def test_a_pass_with_a_failed_judge_says_so(self):
        (self.store / "2026-09-08.json").write_text(json.dumps(
            {"kind": "pass", "run": {"failed": {"sweep": "boom", "rules": "boom"}}}), encoding="utf-8")
        msg = judge_record_warning(self.store, today=dt.date(2026, 9, 9))
        self.assertIn("its rules and sweep judge failed", msg)

    def test_no_store_is_silent(self):
        self.assertIsNone(judge_record_warning(self.store.parent / "missing"))

    def test_files_that_are_not_dated_records_are_ignored(self):
        (self.store / "notes.json").write_text("{}", encoding="utf-8")
        self.assertIsNone(judge_record_warning(self.store, today=dt.date(2026, 9, 9)))


if __name__ == "__main__":
    unittest.main()
