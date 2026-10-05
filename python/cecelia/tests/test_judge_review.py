"""Tests for `scripts/judge/review.py` — the owner's bug queue and its event log.

Design: docs/ai-assist/WEEKLY_JUDGE.md. Keypresses are scripted; the log is a temp file.

Run with `pixi run test-py`.
"""
from __future__ import annotations

import importlib.util
import io
import json
import os
import pathlib
import sys
import threading
import time
import unittest
from unittest import mock

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

    def keys(self, *presses):
        """Scripted keypresses; running out is end of input, which the queue treats as quit."""
        it = iter(presses)

        def read(prompt):
            try:
                return next(it)
            except StopIteration:
                raise EOFError from None
        return read

    def queue(self, *presses, **kw):
        out = io.StringIO()
        self.launched = []
        launch = kw.pop("launch", lambda prompt, cwd: self.launched.append(prompt))
        n = self.rv.run_queue(self.record, read=self.keys(*presses), out=out, path=self.log, use_colour=False,
                              width=80, launch=launch, **kw)
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
    def test_the_queue_is_the_decide_bugs_then_the_work_list(self):
        self.assertEqual([(i["kind"], i["ref"]) for i in self.rv.pending(self.record, [])],
                         [("decide", "B1"), ("decide", "B2"), ("work", "B3")])

    def test_one_key_per_answer_and_a_bad_key_asks_again(self):
        n, out = self.queue("x", "w", "o")
        self.assertEqual(n, 2)
        self.assertIn("press one of", out)
        self.assertIn("  Question\n    Is this intended?", out)
        self.assertIn("  decide  a.py:3  feat/x · fanout-b1", out)
        self.assertIn("✓ B1 → won't fix", out)
        self.assertEqual([r["value"] for r in self.rv.read_reviews(self.log)], ["wont_fix", "open"])

    def test_keys_come_from_press_and_only_the_typed_answer_from_read(self):
        lines = self.keys("my answer")
        out = io.StringIO()
        n = self.rv.run_queue(self.record, press=self.keys("w", "a", "q"), read=lines, out=out, path=self.log,
                              use_colour=False, width=80, launch=lambda prompt, cwd: None)
        self.assertEqual(n, 2)
        self.assertEqual([(r["ref"], r["value"], r.get("note")) for r in self.rv.read_reviews(self.log)],
                         [("B1", "wont_fix", None), ("B2", "open", "my answer")])

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
        self.assertIn("0 answered this session · 3 left", out)
        self.queue("w", "w", "w")
        _, out = self.queue()
        self.assertIn("nothing to review: every open bug is answered", out)


class WorkTest(_ReviewFixture):
    def setUp(self):
        super().setUp()
        fix = {"verdict": "fix", "date": "2026-10-05", "effect": "drops the tail", "evidence": "b.py:9"}
        self.record = self.build(bugs=[_bug("B1", verify=fix), _bug("B2"), _bug("B3", status="gone")])

    def test_live_bugs_lead_the_work_list_and_closed_ones_are_not_on_it(self):
        self.record["bugs"].append(_bug("B4", verify={"verdict": "guard", "trigger": "a map-shaped input"}))
        self.assertEqual([(i["kind"], i["ref"]) for i in self.rv.pending(self.record, [])],
                         [("work", "B1"), ("work", "B4"), ("work", "B2")])

    def test_fix_now_briefs_a_session_and_is_not_offered_again(self):
        n, out = self.queue("f")
        self.assertEqual(n, 1)
        self.assertIn("  Bug\n    desc B1", out)
        self.assertIn("  Verified\n    drops the tail", out)
        self.assertIn("✓ B1 → fix session ended", out)
        brief = self.launched[0]
        self.assertIn("Work bug B1 (`fanout-b1`)", brief)
        self.assertIn(str(self.tmp / "judge-runs" / "2026-10-05.json"), brief)
        self.assertIn("Verified (fix): drops the tail", brief)
        self.assertIn("name `fanout-b1` in the commit message", brief)
        self.assertIn(f"from `{self.rv.main_checkout()}`, run `pixi run bootstrap-worktree fix-fanout-b1`", brief)
        self.assertEqual([(r["event"], r["value"]) for r in self.rv.read_reviews(self.log)], [("bug_work", "fix_session")])
        self.assertEqual([i["ref"] for i in self.rv.pending(self.record, self.rv.read_reviews(self.log))], ["B2"])

    def test_a_session_that_cannot_start_records_nothing(self):
        _, out = self.queue("f", launch=lambda prompt, cwd: "claude CLI not on PATH")
        self.assertIn("claude CLI not on PATH", out)
        self.assertEqual(self.rv.read_reviews(self.log), [])

    def test_the_session_gets_the_real_screen(self):
        seen = []
        _, text = self.queue("f", "q", fullscreen=True, launch=lambda prompt, cwd: seen.append(cwd))
        self.assertEqual(seen, [self.rv.workspace()])   # where sessions start, not this checkout
        self.assertEqual(text.count(self.rv._LEAVE), 2)    # around the session, and on quitting
        self.assertEqual(text.count(self.rv._ENTER), 2)

    def test_won_t_fix_on_a_work_item_closes_it(self):
        self.queue("w")
        self.assertEqual(self.rv.apply_reviews(self.record, self.rv.read_reviews(self.log))["bugs"][0]["status"],
                         "wont_fix")


class WorkspaceTest(unittest.TestCase):
    def test_the_workspace_holds_the_main_checkout_whichever_worktree_runs_this(self):
        rv = _load_review()
        main = rv.main_checkout()
        self.assertTrue((main / ".git").is_dir())        # the main checkout, not a worktree's `.git` file
        self.assertEqual(rv.workspace(), main.parent)


class DecideThenWorkTest(_ReviewFixture):
    def test_a_decide_bug_kept_open_joins_the_work_list_in_the_same_session(self):
        _, out = self.queue("a", "Document it in SHIPPING.md.", "n", "f")
        self.assertIn("── B1 · work", out)
        self.assertIn("  Your answer\n    Document it in SHIPPING.md.", out)
        self.assertIn("My answer, which decides the fix over the agent's recommendation: Document it in SHIPPING.md.",
                      self.launched[0])
        self.assertNotIn("Recommendation: guard it", self.launched[0])


class FullscreenTest(_ReviewFixture):
    def test_one_card_per_screen_and_the_shell_gets_the_outcome(self):
        _, text = self.queue("o", "q", fullscreen=True)
        self.assertTrue(text.startswith(self.rv._ENTER))
        self.assertEqual(text.count(self.rv._CLEAR), 2)              # B1, then B2 with B1's answer on top
        self.assertIn("✓ B1 → keep open", text.split(self.rv._CLEAR)[2])
        after = text.split(self.rv._LEAVE)[1]                        # back on the normal screen
        self.assertIn("1 answered this session · 3 left", after)   # B2, and B1 + B3 to work

    def card_rows(self, text: str, paint: int) -> list[str]:
        return text.split(self.rv._CLEAR)[paint].splitlines()

    def test_page_down_scrolls_a_card_taller_than_the_terminal(self):
        with mock.patch.object(self.rv, "_terminal_size", return_value=(80, 12)):   # a 7-row card window
            _, text = self.queue(self.rv.PGDN, self.rv.PGDN, self.rv.PGUP, "q", fullscreen=True)
        first, down, end, up = (self.card_rows(text, i) for i in (1, 2, 3, 4))
        self.assertIn("── B1 · decide", first[2])
        self.assertIn("more line(s) · PgDn", first[-2])
        self.assertIn("more line(s) · PgUp", down[2])
        self.assertNotIn("── B1 · decide", "\n".join(down))
        self.assertIn("a.py:3", end[-2])               # the evidence, last line of the card
        self.assertNotIn("PgDn", "\n".join(end))
        self.assertNotEqual(up, end)                   # PgUp from the clamped end moves at once
        self.assertEqual(self.rv.read_reviews(self.log), [])   # scrolling answers nothing

    def test_a_resize_repaints_the_card_at_the_new_size(self):
        size = [(80, 40)]
        presses = iter([self.rv.RESIZE, "q"])

        def press(prompt):
            key = next(presses)
            if key == self.rv.RESIZE:
                size[0] = (80, 12)   # the terminal shrinks while the card waits for a key
            return key
        with mock.patch.object(self.rv, "_terminal_size", side_effect=lambda *_: size[0]):
            _, text = self.queue(press=press, fullscreen=True)
        self.assertNotIn("PgDn", text.split(self.rv._CLEAR)[1])
        self.assertIn("more line(s) · PgDn", text.split(self.rv._CLEAR)[2])


class EvidenceTest(unittest.TestCase):
    def setUp(self):
        self.rv = _load_review()

    def test_one_bullet_per_sentence_and_reference_lists_stay_whole(self):
        text = ("pixi.toml:220 `prod` runs bare julia. app.py:26-44 is the only resolver, e.g. for app. "
                "The docs say so (a.md:1; b.md:2).")
        lines = self.rv._paragraph(text, width=100, use_colour=False, bullets=True)
        self.assertEqual(lines, ["    • pixi.toml:220 `prod` runs bare julia.",
                                 "    • app.py:26-44 is the only resolver, e.g. for app.",
                                 "    • The docs say so (a.md:1; b.md:2)."])

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


@unittest.skipIf(sys.platform == "win32", "pty and termios are POSIX-only")
class ReadKeyTest(unittest.TestCase):
    """`read_key` on a real (pseudo-)terminal: one byte answers, no Enter."""

    def press(self, typed: bytes | None) -> str:
        """`typed` once the key is awaited; None resizes the terminal instead (a real SIGWINCH)."""
        import pty
        import signal
        import termios
        master, slave = pty.openpty()
        self.addCleanup(os.close, master)
        self.addCleanup(os.close, slave)
        before = termios.tcgetattr(slave)

        def type_once_in_cbreak():   # setcbreak drops typeahead, so type once the key is awaited
            for _ in range(500):
                if not termios.tcgetattr(slave)[3] & termios.ICANON:
                    if typed is None:
                        os.kill(os.getpid(), signal.SIGWINCH)
                    else:
                        os.write(master, typed)
                    return
                time.sleep(0.01)
        threading.Thread(target=type_once_in_cbreak, daemon=True).start()
        stdin = mock.Mock(fileno=lambda: slave)
        try:
            with mock.patch.object(sys, "stdin", stdin), mock.patch.object(sys, "stdout", io.StringIO()):
                return _load_review().read_key("› ")
        finally:
            after = termios.tcgetattr(slave)
            # macOS's kernel sets PENDIN (reprint pending input) on leaving cbreak; not ours to restore
            pendin = getattr(termios, "PENDIN", 0)
            before[3], after[3] = before[3] & ~pendin, after[3] & ~pendin
            self.assertEqual(after, before)   # the line discipline is put back

    def test_one_key_without_enter(self):
        self.assertEqual(self.press(b"f"), "f")

    def test_page_down_is_named(self):
        self.assertEqual(self.press(b"\x1b[6~"), _load_review().PGDN)

    def test_a_resize_while_waiting_returns_to_repaint(self):
        self.assertEqual(self.press(None), _load_review().RESIZE)

    def test_an_arrow_key_is_not_its_trailing_letter(self):
        self.assertEqual(self.press(b"\x1b[A"), "")
        self.assertEqual(self.press(b"\x1b[D"), "")   # ← expands in the recital console, not here

    def test_ctrl_d_is_end_of_input(self):
        with self.assertRaises(EOFError):
            self.press(b"\x04")
