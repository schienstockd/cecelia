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
        n = self.rv.run_queue(self.record, read=lambda prompt: next(it), out=out, path=self.log, use_colour=False,
                              width=80)
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
        self.assertIn("  Question\n    Is this intended?", out)
        self.assertIn("  decide  a.py:3  feat/x · fanout-b1", out)
        self.assertIn("✓ B1 → won't fix", out)
        self.assertEqual([r["value"] for r in self.rv.read_reviews(self.log)], ["wont_fix", "open"])

    def test_undo_writes_a_correction_and_asks_that_bug_again(self):
        n, _ = self.queue("w", "z", "o", "q")
        rows = self.rv.read_reviews(self.log)
        self.assertEqual([(r["ref"], r["value"]) for r in rows], [("B1", "wont_fix"), ("B1", None), ("B1", "open")])
        self.assertEqual(n, 1)

    def test_an_answer_is_recorded_and_folded_into_the_record(self):
        n, out = self.queue("a", "Only stop* is supported; document it.", "q")
        self.assertEqual(n, 1)
        self.assertIn("✓ B1 → answer", out)
        rows = self.rv.read_reviews(self.log)
        self.assertEqual([(r["ref"], r["value"], r.get("note")) for r in rows],
                         [("B1", "open", "Only stop* is supported; document it.")])
        applied = self.rv.apply_reviews(self.record, rows)
        self.assertEqual(applied["bugs"][0]["owner_answer"], "Only stop* is supported; document it.")
        self.assertIn("**Owner's answer** (follow this, not the recommendation): Only stop*",
                      self.rv._record.render_markdown(applied))

    def test_an_empty_answer_records_nothing_and_asks_again(self):
        _, out = self.queue("a", "", "q")
        self.assertIn("empty answer", out)
        self.assertEqual(self.rv.read_reviews(self.log), [])

    def test_undoing_an_answer_drops_it(self):
        self.queue("a", "my answer", "z", "o", "q")
        applied = self.rv.apply_reviews(self.record, self.rv.read_reviews(self.log))
        self.assertEqual((applied["bugs"][0]["status"], applied["bugs"][0].get("owner_answer")), ("open", None))

    def test_skip_and_quit_leave_bugs_pending_and_an_answered_record_says_so(self):
        _, out = self.queue("n", "q")
        self.assertIn("0 of 2 answered · 2 left", out)
        self.queue("w", "w")
        _, out = self.queue()
        self.assertIn("nothing to review: all 2 answered", out)


class FullscreenTest(_ReviewFixture):
    def test_one_card_per_screen_and_the_shell_gets_the_outcome(self):
        it = iter(["o", "q"])
        out = io.StringIO()
        self.rv.run_queue(self.record, read=lambda prompt: next(it), out=out, path=self.log,
                          use_colour=False, width=80, fullscreen=True)
        text = out.getvalue()
        self.assertTrue(text.startswith(self.rv._ENTER))
        self.assertEqual(text.count(self.rv._CLEAR), 2)              # B1, then B2 with B1's answer on top
        self.assertIn("✓ B1 → keep open", text.split(self.rv._CLEAR)[2])
        after = text.split(self.rv._LEAVE)[1]                        # back on the normal screen
        self.assertIn("1 of 2 answered · 1 left", after)

    def test_a_card_taller_than_the_terminal_says_what_it_cut(self):
        lines = self.rv._fit([str(i) for i in range(10)], 4, use_colour=False)
        self.assertEqual(lines[:3], ["0", "1", "2"])
        self.assertIn("7 more line(s)", lines[3])


class EvidenceTest(unittest.TestCase):
    def setUp(self):
        self.rv = _load_review()

    def test_one_bullet_per_sentence_and_reference_lists_stay_whole(self):
        text = ("pixi.toml:220 `prod` runs bare julia. app.py:26-44 is the only resolver, e.g. for app. "
                "The docs say so (docs/A.md:1; docs/B.md:2).")
        lines = self.rv._paragraph(text, width=100, use_colour=False, bullets=True)
        self.assertEqual(lines, ["    • pixi.toml:220 `prod` runs bare julia.",
                                 "    • app.py:26-44 is the only resolver, e.g. for app.",
                                 "    • The docs say so (docs/A.md:1; docs/B.md:2)."])

    def test_a_coloured_bullet_does_not_leak_its_escape_code(self):
        lines = self.rv._paragraph("python/x.py:4 replaces it. pixi.toml:1 says so.", width=100,
                                   use_colour=True, bullets=True)
        self.assertNotIn("0mpython", "".join(lines))
        self.assertIn(self.rv._col(self.rv.SKY_BLUE, "python/x.py:4", use_colour=True), lines[0])

    def test_code_and_file_line_references_are_coloured(self):
        line = self.rv._highlight("see `run_py` at app.py:12-14, :40", use_colour=True)
        self.assertIn(self.rv._col(self.rv.YELLOW, "`run_py`", use_colour=True), line)
        # one file's line list is one reference
        self.assertIn(self.rv._col(self.rv.SKY_BLUE, "app.py:12-14, :40", use_colour=True), line)
        self.assertIn(self.rv._col(self.rv.SKY_BLUE, ":7", use_colour=True),
                      self.rv._highlight("then :7 again", use_colour=True))


if __name__ == "__main__":
    unittest.main()
