"""`judge-review --auto` outliving its terminal and showing on the consoles: the run's state file
(`effectiveness/auto_run.py`), the line it becomes, `--detach`, the terminal-loss fallback and the
one-at-a-time lock. Sessions and every git / gh call are faked; nothing is spawned."""
from __future__ import annotations

import datetime as _dt
import io
import json
import os
import pathlib
import subprocess
from unittest import mock

from cecelia.effectiveness import auto_run
from cecelia.effectiveness.console import DashboardState, render_dashboard, _ANSI_RE
from cecelia.tests.test_judge_auto_fix import FIX, _answer, _AutoFixture
from cecelia.tests.test_judge_record import _bug

NOW = _dt.datetime(2026, 11, 4, 9, 0, tzinfo=_dt.timezone.utc)


def _proc(root: pathlib.Path, pid: int, start: str) -> None:
    """A `/proc/<pid>/stat` whose field 22 (start time) is `start`, as `proc_start` reads it."""
    (root / str(pid)).mkdir(parents=True, exist_ok=True)
    (root / str(pid) / "stat").write_text(f"{pid} (python) S " + " ".join(["0"] * 18 + [start, "0"]),
                                          encoding="utf-8")


class StatusLineTest(_AutoFixture):
    def setUp(self):
        super().setUp()
        self.proc = self.tmp / "proc"
        _proc(self.proc, 4242, "777")
        self.tracker = auto_run.Tracker(pid=4242, proc=self.proc)

    def line(self, now=NOW):
        return auto_run.status_line(now=now, proc=self.proc)

    def test_no_run_ever_shows_nothing(self):
        self.assertIsNone(self.line())

    def test_a_running_run_says_where_it_is(self):
        self.tracker.start(record="2026-11-03", total=35, until=None, log="/x/auto.log")
        self.tracker.update(current="B13", done=4, spent=3.8)
        kind, text = self.line()
        self.assertEqual(kind, "running")
        self.assertIn("judge --auto · 4/35 sessions · $3.80 · on B13", text)
        self.assertIn("log /x/auto.log", text)

    def test_a_run_whose_process_is_gone_without_a_finish_died(self):
        self.tracker.start(record="2026-11-03", total=35, until=None, log="/x/auto.log")
        self.tracker.update(done=2)
        (self.proc / "4242" / "stat").unlink()
        kind, text = self.line()
        self.assertEqual(kind, "died")
        self.assertIn("after 2/35 sessions", text)
        self.assertIn("log /x/auto.log", text)

    def test_a_reused_pid_is_not_the_run(self):
        self.tracker.start(record="2026-11-03", total=3, until=None, log=None)
        _proc(self.proc, 4242, "999")   # another process got the pid
        self.assertEqual(self.line()[0], "died")

    def test_a_finished_run_shows_how_it_ended_then_goes(self):
        self.tracker.start(record="2026-11-03", total=3, until=None, log=None)
        self.tracker.update(done=3, spent=4.5)
        self.tracker.finish("usage limit")
        ended = auto_run._parse(auto_run.read()["finished"])
        kind, text = self.line(now=ended + _dt.timedelta(hours=1))
        self.assertEqual(kind, "finished")
        self.assertIn("(usage limit) · 3/3 sessions · $4.50", text)
        self.assertIsNone(self.line(now=ended + _dt.timedelta(hours=auto_run.RECENT_HOURS + 1)))

    def test_the_console_shows_it_under_the_header_and_on_the_agents_view(self):
        state = DashboardState()
        state.auto_run = ("died", "judge --auto died: no process since 19:11, after 2/35 sessions · $0.00")
        for agents_only in (False, True):
            frame = _ANSI_RE.sub("", render_dashboard(state, self.tmp / "events.jsonl", width=120,
                                                      agents_only=agents_only))
            self.assertIn("judge --auto died: no process since 19:11", frame)


class RunTracksTest(_AutoFixture):
    def test_the_run_writes_its_progress_and_end(self):
        record = self.build(bugs=[_bug("B4", verify=FIX, file="c.py", branch="feat/y"),
                                  _bug("B5", verify=FIX, file="d.py", branch="feat/z")])
        self.changed = "a.py\n"
        seen = []
        tracker = auto_run.Tracker()
        real = tracker.update
        tracker.update = lambda **kw: (real(**kw), seen.append(dict(auto_run.read())))[0]
        answers = [_answer("not_real") | {"bugs": [{"key": k, "outcome": "not_real", "reason": "no"}]}
                   for k in ("fanout-b4", "fanout-b5")]
        with mock.patch.object(self.af, "landed", return_value={}), \
                mock.patch.dict(os.environ, {auto_run.LOG_ENV: "/x/auto.log"}):
            self.af.run(record, out=io.StringIO(), path=self.log, auto_log=self.auto, main=self.main,
                        session=self.session(*answers), sh=self.sh(), tracker=tracker)
        self.assertEqual([(s["current"], s["done"]) for s in seen[:2]], [("B4", 0), ("B4", 1)])
        final = auto_run.read()
        self.assertEqual((final["total"], final["done"], final["spent"], final["end"], final["log"]),
                         (2, 2, 3.0, "done", "/x/auto.log"))
        self.assertIsNotNone(final["finished"])

    def test_a_dry_run_writes_nothing(self):
        with mock.patch.object(self.af, "landed", return_value={}):
            self.af.run(self.record, out=io.StringIO(), dry_run=True, path=self.log, auto_log=self.auto,
                        main=self.main, tracker=auto_run.Tracker())
        self.assertFalse(auto_run.state_path().exists())


class _Gone(io.StringIO):
    """A terminal that closed: every write and flush fails as a hung-up tty's does."""

    def write(self, text):
        raise OSError(5, "Input/output error")

    def flush(self):
        raise OSError(5, "Input/output error")

    def isatty(self):
        return True


class TerminalLostTest(_AutoFixture):
    def test_output_moves_to_the_log_once_and_the_tracker_learns_where(self):
        path, switched = self.tmp / "cron" / "auto.log", []
        out = self.af.FallbackOut(_Gone(), path, switched.append)
        err = self.af.FallbackOut(_Gone(), path, shared=out)
        self.addCleanup(lambda: out._file and out._file.close())
        out.write("session B13\n")
        err.write("a warning\n")
        out.flush()
        self.assertEqual(switched, [path])
        self.assertFalse(out.isatty())   # the spinner stops drawing frames
        text = path.read_text(encoding="utf-8")
        self.assertIn("the terminal went away", text)
        self.assertTrue(text.endswith("session B13\na warning\n"))

    def test_a_session_line_after_the_terminal_went_has_no_escape_codes(self):
        path = self.tmp / "cron" / "auto.log"
        out = self.af.FallbackOut(_Gone(), path)
        self.addCleanup(lambda: out._file and out._file.close())
        with self.af.Progress("agent working", out=out):
            out.write("")   # nothing yet: still the terminal
            out.write("x\n")   # the terminal goes mid-session
        text = path.read_text(encoding="utf-8")
        self.assertIn("✓ agent working", text)
        self.assertNotIn("\033", text)

    def test_colour_decided_while_there_was_a_terminal_is_dropped_in_the_file(self):
        path = self.tmp / "cron" / "auto.log"
        out = self.af.FallbackOut(_Gone(), path)
        self.addCleanup(lambda: out._file and out._file.close())
        out.write("\r\033[K  \033[38;2;86;180;233m✓\033[0m B13 fixed\n")
        self.assertTrue(path.read_text(encoding="utf-8").endswith("  ✓ B13 fixed\n"))

    def test_a_live_terminal_is_written_as_before(self):
        term = io.StringIO()
        out = self.af.FallbackOut(term, self.tmp / "never.log")
        out.write("hello\n")
        self.assertEqual(term.getvalue(), "hello\n")
        self.assertFalse((self.tmp / "never.log").exists())


class DetachTest(_AutoFixture):
    def test_detach_restarts_itself_in_its_own_session_logging_to_a_file(self):
        seen = {}

        def popen(argv, **kw):
            seen.update(argv=argv, **kw)
            return mock.Mock(pid=31337)
        out = io.StringIO()
        with mock.patch.dict(os.environ, {"CECELIA_EVAL_CRON_LOG_DIR": str(self.tmp / "cron")}):
            pid = self.af.detach(["--auto", "--detach", "--limit", "3"], pathlib.Path("/r/scripts/judge/review.py"),
                                 out=out, now=NOW, popen=popen)
        log = self.tmp / "cron" / "auto-20261104T090000Z.log"
        self.assertEqual(pid, 31337)
        self.assertEqual(seen["argv"][1:], ["/r/scripts/judge/review.py", "--auto", "--limit", "3"])
        self.assertTrue(seen["start_new_session"])
        self.assertIs(seen["stdin"], subprocess.DEVNULL)
        self.assertEqual(seen["env"][auto_run.LOG_ENV], str(log))
        self.assertIn(str(log), out.getvalue())

    def test_detach_needs_a_real_auto_run(self):
        err = io.StringIO()
        with mock.patch("sys.stderr", err):
            self.assertEqual(self.rv.main(["--detach"]), 2)
            self.assertEqual(self.rv.main(["--auto", "--dry-run", "--detach"]), 2)

    def test_a_second_run_starts_nothing_while_one_holds_the_lock(self):
        self.rec.write(self.record)
        fake = mock.Mock()
        held = auto_run.run_lock()
        self.addCleanup(held.close)
        err = io.StringIO()
        with mock.patch.object(self.rv, "_load_sibling", return_value=fake), mock.patch("sys.stderr", err):
            self.assertEqual(self.rv.main(["--auto"]), 1)
        fake.run.assert_not_called()
        fake.detach.assert_not_called()
        self.assertIn("another run is going", err.getvalue())

    def test_a_crash_is_written_as_the_end(self):
        self.rec.write(self.record)

        def boom(record, **kw):
            kw["tracker"].start(record=record["date"], total=1, until=None, log=None)
            raise RuntimeError("disk full")
        fake = mock.Mock(run=boom, unattended=lambda tracker: None)
        with mock.patch.object(self.rv, "_load_sibling", return_value=fake):
            with self.assertRaises(RuntimeError):
                self.rv.main(["--auto"])
        self.assertEqual(auto_run.read()["end"], "crashed: RuntimeError: disk full")
        lock = auto_run.run_lock()   # released
        self.assertIsNotNone(lock)
        lock.close()


if __name__ == "__main__":
    import unittest
    unittest.main()
