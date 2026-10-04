"""Tests for `scripts/judge/review.py` — the owner's bug queue and its event log.

Design: docs/ai-assist/WEEKLY_JUDGE.md. Keypresses are scripted; the log is a temp file.

Run with `pixi run test-py`.
"""
from __future__ import annotations

import importlib.util
import io
import json
import pathlib
import unittest

from cecelia.tests.test_judge_record import _bug, _Fixture

_REPO = pathlib.Path(__file__).resolve().parents[3]
_REVIEW_PATH = _REPO / "scripts" / "judge" / "review.py"


def _load_review():
    spec = importlib.util.spec_from_file_location("review", _REVIEW_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class _ReviewFixture(_Fixture):
    def setUp(self):
        super().setUp()
        self.rv = _load_review()
        self.log = self.tmp / "review.jsonl"
        decide = {"verdict": "decide", "date": "2026-10-05", "question": "Is this intended?",
                  "recommendation": "guard it", "evidence": "a.py:3"}
        self.record = self.build(bugs=[_bug("B1", verify=decide), _bug("B2", verify=decide), _bug("B3")])

    def queue(self, *presses):
        it = iter(presses)
        out = io.StringIO()
        n = self.rv.run_queue(self.record, read=lambda prompt: next(it), out=out, path=self.log, use_colour=False)
        return n, out.getvalue()


class EventLogTest(_ReviewFixture):
    def test_round_trip_and_a_torn_line_is_skipped(self):
        self.rv.append_review("bug_status", "2026-10-05", "B1", "wont_fix", path=self.log)
        with self.log.open("a", encoding="utf-8") as fh:
            fh.write('{"half a line\n')
            fh.write(json.dumps({"schema_version": 9, "event": "bug_status"}) + "\n")
        self.assertEqual([(r["ref"], r["value"]) for r in self.rv.read_reviews(self.log)], [("B1", "wont_fix")])

    def test_an_unknown_event_is_refused(self):
        with self.assertRaises(ValueError):
            self.rv.append_review("proposal_decision", "2026-10-05", "P1", "accept", path=self.log)

    def test_apply_folds_bug_answers_into_a_copy(self):
        self.rv.append_review("bug_status", "2026-10-05", "B2", "wont_fix", path=self.log)
        applied = self.rv.apply_reviews(self.record, self.rv.read_reviews(self.log))
        self.assertEqual([b["status"] for b in applied["bugs"]], ["open", "wont_fix", "open"])
        self.assertEqual(self.record["bugs"][1]["status"], "open")


class QueueTest(_ReviewFixture):
    def test_the_queue_is_the_decide_bugs(self):
        self.assertEqual([i["ref"] for i in self.rv.pending(self.record, [])], ["B1", "B2"])

    def test_one_key_per_answer_and_a_bad_key_asks_again(self):
        n, out = self.queue("x", "w", "o")
        self.assertEqual(n, 2)
        self.assertIn("press one of", out)
        self.assertIn("Question: Is this intended?", out)
        self.assertEqual([r["value"] for r in self.rv.read_reviews(self.log)], ["wont_fix", "open"])

    def test_undo_writes_a_correction_and_asks_that_bug_again(self):
        n, _ = self.queue("w", "z", "o", "q")
        rows = self.rv.read_reviews(self.log)
        self.assertEqual([(r["ref"], r["value"]) for r in rows], [("B1", "wont_fix"), ("B1", None), ("B1", "open")])
        self.assertEqual(n, 1)

    def test_skip_and_quit_leave_bugs_pending_and_an_answered_record_says_so(self):
        _, out = self.queue("n", "q")
        self.assertIn("0/2 answered", out)
        self.queue("w", "w")
        _, out = self.queue()
        self.assertIn("Nothing to review", out)


if __name__ == "__main__":
    unittest.main()
