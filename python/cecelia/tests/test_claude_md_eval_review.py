"""Tests for `scripts/claude_md_eval/review.py` — the owner's review queue and its event log.

Design: docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md → Decision 15 and phase 6. Records come from the
synthetic fixture in `test_claude_md_eval_record`; keypresses are scripted.

Run with `pixi run test-py`.
"""
from __future__ import annotations

import importlib.util
import io
import json
import pathlib
import unittest

from cecelia.tests.test_claude_md_eval_record import _Fixture

_REPO = pathlib.Path(__file__).resolve().parents[3]
_REVIEW_PATH = _REPO / "scripts" / "claude_md_eval" / "review.py"


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
        self.record = self.rv._record.build(self.events, "2026-09-30", annotations={
            "findings": [self._finding(id="F1"),
                         self._finding(id="F2", slug="p2", **{"class": "decision"})],
            "proposals": [{"id": "P1", "kind": "setup", "summary": "do x", "sources": ["F1"],
                           "hypothesis": "fixes F1; expect p1 to pass"}],
            "spot_check": ["F1"]})

    def keys(self, *presses):
        it = iter(presses)
        return lambda prompt: next(it)

    def queue(self, *presses):
        out = io.StringIO()
        n = self.rv.run_queue(self.record, read=self.keys(*presses), out=out, path=self.log, use_colour=False)
        return n, out.getvalue()


class EventLogTest(_ReviewFixture):
    def test_round_trip_and_a_torn_line_is_skipped(self):
        self.rv.append_review("proposal_decision", "2026-09-30", "P1", "accept", path=self.log)
        with self.log.open("a", encoding="utf-8") as fh:
            fh.write('{"half a line\n')
            fh.write(json.dumps({"schema_version": 9, "event": "proposal_decision"}) + "\n")
        rows = self.rv.read_reviews(self.log)
        self.assertEqual([(r["ref"], r["value"]) for r in rows], [("P1", "accept")])

    def test_an_undo_is_a_correcting_event_that_reopens_the_item(self):
        row = self.rv.append_review("proposal_decision", "2026-09-30", "P1", "accept", path=self.log)
        self.assertNotIn({"kind": "proposal", "ref": "P1"}, self.rv.pending(self.record, self.rv.read_reviews(self.log)))
        self.rv.append_review("proposal_decision", "2026-09-30", "P1", None, corrects=row["id"], path=self.log)
        rows = self.rv.read_reviews(self.log)
        self.assertEqual(len(rows), 2)                       # append-only: the answer is still there
        self.assertIn({"kind": "proposal", "ref": "P1"}, self.rv.pending(self.record, rows))

    def test_an_unknown_event_is_refused(self):
        with self.assertRaises(ValueError):
            self.rv.append_review("delete_record", "2026-09-30", "F1", "x", path=self.log)

    def test_apply_folds_answers_into_a_copy(self):
        for event, ref, value in (("finding_status", "F2", "resolved"), ("proposal_decision", "P1", "reject"),
                                  ("spot_check_label", "F1", "false")):
            self.rv.append_review(event, "2026-09-30", ref, value, note="answer" if ref == "F2" else None,
                                  path=self.log)
        self.rv.append_review("finding_status", "2026-09-23", "F1", "dropped", path=self.log)   # another record
        applied = self.rv.apply_reviews(self.record, self.rv.read_reviews(self.log))
        status = {f["id"]: f["status"] for f in applied["findings"]}
        self.assertEqual(status, {"F1": "open", "F2": "resolved"})
        self.assertEqual(applied["findings"][1]["owner_note"], "answer")
        self.assertEqual(applied["proposals"][0]["decision"], "reject")
        self.assertEqual(applied["tracking"]["spot_check_labels"], {"F1": "false"})
        self.assertEqual(self.record["findings"][1]["status"], "open")   # the input is untouched
        self.assertEqual(self.rv._record.validate(applied), [])


class QueueTest(_ReviewFixture):
    def test_the_queue_holds_decisions_proposals_and_spot_checks(self):
        self.assertEqual(self.record["queue"], [{"kind": "decision", "ref": "F2"}, {"kind": "proposal", "ref": "P1"},
                                                {"kind": "spot_check", "ref": "F1"}])

    def test_one_key_per_answer_and_a_bad_key_asks_again(self):
        n, out = self.queue("o", "x", "a", "r")
        self.assertEqual(n, 3)
        self.assertIn("press one of", out)
        self.assertIn("3/3 answered", out)
        values = [(r["ref"], r["value"]) for r in self.rv.read_reviews(self.log)]
        self.assertEqual(values, [("F2", "open"), ("P1", "accept"), ("F1", "real")])

    def test_resolving_a_decision_records_the_answer(self):
        self.queue("r", "docstrings count", "q")
        row = self.rv.read_reviews(self.log)[0]
        self.assertEqual((row["value"], row["note"]), ("resolved", "docstrings count"))

    def test_undo_writes_a_correction_and_asks_that_item_again(self):
        n, out = self.queue("o", "z", "d", "q")
        rows = self.rv.read_reviews(self.log)
        self.assertEqual([(r["ref"], r["value"]) for r in rows], [("F2", "open"), ("F2", None), ("F2", "dropped")])
        self.assertEqual(rows[1]["corrects"], rows[0]["id"])
        self.assertEqual(n, 1)
        self.assertIn("undid finding_status F2 = open", out)

    def test_quit_and_skip_leave_items_pending_with_progress(self):
        _, out = self.queue("n", "a", "q")
        self.assertIn("1/3 answered for 2026-09-30; 2 left", out)

    def test_an_answered_record_shows_its_empty_state(self):
        self.queue("o", "a", "r")
        n, out = self.queue()
        self.assertEqual(n, 0)
        self.assertIn("Nothing to review for 2026-09-30: all 3 item(s) answered.", out)


class SupervisorAddsTest(_ReviewFixture):
    def test_spot_checks_come_every_fourth_run_and_are_stable(self):
        findings = [{"id": f"F{i}"} for i in range(1, 9)]
        self.assertEqual(self.rv.spot_check_sample(findings, run_number=3, seed="d"), [])
        first = self.rv.spot_check_sample(findings, run_number=4, seed="2026-10-27")
        self.assertTrue(3 <= len(first) <= 5)
        self.assertEqual(first, self.rv.spot_check_sample(findings, run_number=4, seed="2026-10-27"))
        self.assertEqual(self.rv.spot_check_sample(findings[:2], run_number=8, seed="d"), ["F1", "F2"])

    def test_the_loop_review_comes_on_the_eighth_run_with_its_numbers(self):
        self.assertIsNone(self.rv.loop_review(self.record, [], run_number=7, reviews=[]))
        self.rv.append_review("proposal_decision", "2026-09-30", "P1", "accept", path=self.log)
        self.rv.append_review("spot_check_label", "2026-09-30", "F1", "false", path=self.log)
        loop = self.rv.loop_review(self.record, [self.record], run_number=8, reviews=self.rv.read_reviews(self.log))
        self.assertEqual((loop["runs"], loop["accepted"], loop["false_positive_rate"]), (8, 1, "1/1"))
        self.assertEqual(loop["scores"], ["2026-09-30 0/3", "2026-09-30 0/3"])
        rec = self.rv._record.build(self.events, "2026-09-30", annotations={"loop_review": loop})
        self.assertIn({"kind": "loop_review", "ref": "loop"}, rec["queue"])
        self.assertIn("## Loop review", self.rv._record.render_markdown(rec))


class MainTest(_ReviewFixture):
    def test_a_missing_or_other_schema_record_is_a_clear_error(self):
        err = io.StringIO()
        import contextlib
        with contextlib.redirect_stderr(err):
            self.assertEqual(self.rv.main(["--date", "2026-01-01"]), 1)
            old = self.tmp / "eval-runs" / "2026-02-02.json"
            old.parent.mkdir(parents=True, exist_ok=True)
            old.write_text(json.dumps({"schema_version": 0}), encoding="utf-8")
            self.assertEqual(self.rv.main(["--date", "2026-02-02"]), 1)
            self.assertEqual(self.rv.main([]), 1)    # nothing valid in the store
        self.assertIn("no run record at", err.getvalue())
        self.assertIn("schema_version 0, this code reads 1", err.getvalue())
        self.assertIn("no run records in", err.getvalue())


if __name__ == "__main__":
    unittest.main()
