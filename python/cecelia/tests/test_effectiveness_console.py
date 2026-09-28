"""Tests for the recital-side live console (`pixi run recital-console`).

Pinned invariants — the ones that would silently drift the console away from what a reader
comes to it for:

- **Findings render marker + slug + file:line + description** — the whole point of the console
  is that the reader can see WHAT was flagged without opening the roundup. Losing any of those
  fields breaks that.
- **Meta rows are dropped, not rendered as blanks** — `plan_logged`/`prompt_logged` carry no
  per-run signal; padding them into the stream would drown the flags.
- **The `--stream` mode is byte-for-byte plain** — auto-set when stdout isn't a TTY, so
  `pixi run recital-console | tee out.log` produces a file no ANSI stripper is needed for.
- **`--since` grammar accepts the readable forms** — `1h`/`30m`/`today`/ISO. A typo raises
  rather than silently matching zero events.
- **The follow loop survives partial writes and rotations** — a malformed line is skipped,
  a truncated file resets the offset. Same contract as `read_events`.
- **Legacy `sibling_audit_*` folds under the same `fanout` label** — the log is append-only
  and pre-rename rows live in it forever; two adjacent labels for the same mechanism would
  be a UX regression.
"""

from __future__ import annotations

import datetime as _dt
import io
import json
import pathlib
import tempfile
import threading
import time
import unittest

from cecelia.effectiveness import append_event
from cecelia.effectiveness.console import (
    DashboardState,
    _parse_since,
    _tail,
    follow_events,
    format_event,
    main,
    render_dashboard,
)


def _finding(**kw) -> dict:
    """Build a `fanout_audit_finding` event with sensible defaults — one place to tweak."""
    payload = {
        "slug": "fanout-abcd1234",
        "file": "python/cecelia/effectiveness/log.py",
        "line": 121,
        "desc": "guard not casefolded — twin below still raises on mixed-case",
        "marker": "confirmed",
    }
    payload.update(kw.pop("payload", {}))
    return {
        "event": "fanout_audit_finding",
        "ts": "2026-09-27T10:00:00Z",
        "pr": "#1263",
        "branch": None,
        "commit": None,
        "session": "test-session",
        "source": "live",
        "schema_version": 1,
        "payload": payload,
        **kw,
    }


class FormatEventTest(unittest.TestCase):
    def test_finding_row_carries_all_reader_fields(self):
        # marker + slug + file:line + description are the four things a reader comes here for.
        line = format_event(_finding(), use_colour=False)
        self.assertIsNotNone(line)
        # Marker in brackets so it reads as a tag, not part of the description.
        self.assertIn("[confirmed]", line)
        self.assertIn("fanout-abcd1234", line)
        self.assertIn("python/cecelia/effectiveness/log.py:121", line)
        self.assertIn("guard not casefolded", line)
        # Compact mechanism tag ("fnut") sits in the leftmost column.
        self.assertIn("fnut", line)
        # PR context anchors the row to the change (compact `#1263` — no `pr=` prefix).
        self.assertIn("#1263", line)

    def test_finding_without_description_still_renders(self):
        # Some retrospective rows have no `desc`. The row must still print (head line only) —
        # dropping it would hide the fact that the finding was raised.
        line = format_event(_finding(payload={"desc": ""}), use_colour=False)
        self.assertIsNotNone(line)
        self.assertIn("fanout-abcd1234", line)
        self.assertNotIn("↳", line)

    def test_run_row_shows_duration_and_context(self):
        event = {
            "event": "convention_check_run",
            "ts": "2026-09-27T10:01:00Z",
            "pr": None, "branch": "feat/preview-send-fix", "commit": "abc1234deadbeef",
            "session": "s", "source": "live", "schema_version": 1,
            "payload": {"duration_s": 51.2},
        }
        line = format_event(event, use_colour=False)
        self.assertIn("conv", line)
        self.assertIn("RUN", line)
        self.assertIn("51.2s", line)
        # Branch drops its `feat/` prefix — the eye is looking for the slug, not the type.
        self.assertIn("preview-send-fix", line)
        self.assertNotIn("feat/", line)
        # Commit is compacted as `@abc1234` (7 chars, matches `git log --oneline`).
        self.assertIn("@abc1234", line)
        self.assertNotIn("deadbeef", line)

    def test_citation_currency_quiet_run_is_dropped(self):
        # A citation-currency run with no warnings and no staged files carries no reader
        # signal — same reason `plan_logged` is dropped. Every recital emits one; showing
        # them all is the noise the console exists to remove.
        event = {
            "event": "citation_currency_run", "ts": "2026-09-27T10:02:00Z",
            "pr": None, "branch": None, "commit": None,
            "session": "s", "source": "live", "schema_version": 1,
            "payload": {"duration_s": 0.075, "citations_indexed": 26,
                        "staged_files_checked": 0, "warnings_emitted": 0},
        }
        self.assertIsNone(format_event(event, use_colour=False))

    def test_citation_currency_signal_run_renders(self):
        # A run with warnings OR staged files renders — the counts are the whole signal for
        # this mechanism. Constant fields (`citations_indexed`) don't appear.
        event = {
            "event": "citation_currency_run", "ts": "2026-09-27T10:02:00Z",
            "pr": None, "branch": None, "commit": None,
            "session": "s", "source": "live", "schema_version": 1,
            "payload": {"duration_s": 0.1, "citations_indexed": 26,
                        "staged_files_checked": 13, "warnings_emitted": 1},
        }
        line = format_event(event, use_colour=False)
        self.assertIsNotNone(line)
        self.assertIn("1 warn", line)
        self.assertIn("13 staged", line)
        self.assertNotIn("26", line)  # citations_indexed is constant — never shown

    def test_resolved_row_shows_outcome_and_slug(self):
        event = {
            "event": "fanout_audit_finding_resolved", "ts": "2026-09-27T10:05:00Z",
            "pr": "#1263", "branch": None, "commit": None,
            "session": "s", "source": "live", "schema_version": 1,
            "payload": {"slug": "fanout-abcd1234", "outcome": "fixed_pre_commit"},
        }
        line = format_event(event, use_colour=False)
        self.assertIn("RSLV", line)  # abbreviated so all verbs occupy the same column
        self.assertIn("fixed_pre_commit", line)
        self.assertIn("fanout-abcd1234", line)

    def test_ratchet_hit_shows_ratchet_id_and_outcome(self):
        event = {
            "event": "ratchet_hit", "ts": "2026-09-27T10:03:00Z",
            "pr": None, "branch": None, "commit": None,
            "session": "s", "source": "live", "schema_version": 1,
            "payload": {"ratchet_id": "zarr-access", "outcome": "fixed_pre_commit"},
        }
        line = format_event(event, use_colour=False)
        self.assertIn("ratc", line)
        self.assertIn("HIT", line)
        self.assertIn("zarr-access", line)
        self.assertIn("fixed_pre_commit", line)

    def test_sibling_audit_legacy_event_renders_under_fanout_label(self):
        # Pre-rename rows live in the append-only log forever — CLAUDE.md and rollup.py fold
        # them under the same header, the console must too or the reader sees two mechanisms
        # (`sibling` AND `fanout`) for what is actually the same one.
        event = {
            "event": "sibling_audit_finding", "ts": "2026-09-27T10:00:00Z",
            "pr": None, "branch": None, "commit": None,
            "session": "s", "source": "live", "schema_version": 1,
            "payload": {"slug": "legacy-1", "file": "x.jl", "line": 1,
                        "desc": "pre-rename row", "marker": "confirmed"},
        }
        line = format_event(event, use_colour=False)
        # The mechanism tag — the leftmost fixed column — reads as `fnut` (fanout), not any
        # legacy label. (A slug carrying `sibling` is separately legal; this just checks the tag.)
        mechanism_column = line.split("FIND")[0]
        self.assertIn("fnut", mechanism_column)
        self.assertNotIn("sibling", mechanism_column)

    def test_meta_rows_return_none(self):
        # plan_logged / prompt_logged carry no per-run signal — padding them into the stream
        # would drown the flags the reader is here for.
        for name in ("plan_logged", "prompt_logged", "attention_tick"):
            event = {"event": name, "ts": "2026-09-27T10:00:00Z", "payload": {},
                     "pr": None, "branch": None, "commit": None, "session": "s",
                     "source": "live", "schema_version": 1}
            self.assertIsNone(format_event(event, use_colour=False),
                              f"{name} should not render a line")

    def test_no_colour_mode_emits_no_ansi(self):
        # `pixi run recital-console | tee out.log` must produce a file that reads cleanly with
        # no ANSI stripping. If any escape leaks through, that guarantee is broken.
        line = format_event(_finding(), use_colour=False)
        self.assertNotIn("\033[", line)

    def test_colour_mode_emits_ansi(self):
        # Regression guard on the flag itself — trivial, but a bug that silently drops all
        # colour would go unnoticed until someone opened the console interactively.
        line = format_event(_finding(), use_colour=True)
        self.assertIn("\033[", line)


class ParseSinceTest(unittest.TestCase):
    def test_relative_units(self):
        now = _dt.datetime.now(_dt.timezone.utc)
        # 1h ≈ now - 1 hour, within a small delta (the parser calls `datetime.now` internally).
        got = _parse_since("1h")
        self.assertLess(abs((now - _dt.timedelta(hours=1) - got).total_seconds()), 5)
        for spec in ("30m", "45s", "2d"):
            self.assertLess(_parse_since(spec), now)

    def test_today_is_midnight_utc(self):
        # `today` = UTC midnight of the current day, so a `--since today` run on a fresh
        # morning still shows yesterday-evening's work if the user runs after midnight-local.
        got = _parse_since("today")
        self.assertEqual((got.hour, got.minute, got.second), (0, 0, 0))

    def test_iso_timestamp(self):
        got = _parse_since("2026-01-15T12:34:56Z")
        self.assertEqual(got.year, 2026)
        self.assertEqual(got.month, 1)
        self.assertEqual(got.hour, 12)

    def test_typo_raises(self):
        # A typo (like `1hour` instead of `1h`) must fail loud, not silently match nothing.
        for bad in ("1hour", "yesterday", "abc", ""):
            with self.assertRaises(ValueError, msg=f"{bad!r} should raise"):
                _parse_since(bad)


class TailTest(unittest.TestCase):
    def _events(self, n: int, start_min: int = 0):
        return [
            {"event": "fanout_audit_run",
             "ts": f"2026-09-27T10:{start_min + i:02d}:00Z",
             "payload": {"duration_s": 1.0}}
            for i in range(n)
        ]

    def test_tail_n_returns_last_n(self):
        events = self._events(10)
        got = _tail(events, 3, None)
        self.assertEqual([e["ts"] for e in got],
                         ["2026-09-27T10:07:00Z", "2026-09-27T10:08:00Z", "2026-09-27T10:09:00Z"])

    def test_tail_zero_returns_all(self):
        events = self._events(5)
        self.assertEqual(len(_tail(events, 0, None)), 5)

    def test_since_beats_tail(self):
        # --since overrides --tail for the backlog: if the user asks for "today" they want
        # every event today even if that's more than 50.
        events = self._events(10)
        cutoff = _dt.datetime(2026, 9, 27, 10, 7, 0, tzinfo=_dt.timezone.utc)
        got = _tail(events, 3, cutoff)
        # All events at or after 10:07 — three of them (07, 08, 09).
        self.assertEqual(len(got), 3)
        self.assertTrue(all(e["ts"] >= "2026-09-27T10:07" for e in got))


class FollowEventsTest(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.log_path = pathlib.Path(self._tmp.name) / "events.jsonl"

    def test_reads_events_appended_after_start(self):
        # Prime with one event, note the offset, then append two more.
        append_event("fanout_audit_run", {"duration_s": 1.0}, log_path=self.log_path)
        offset = self.log_path.stat().st_size
        append_event("fanout_audit_run", {"duration_s": 2.0}, log_path=self.log_path)
        append_event("convention_check_run", {"duration_s": 3.0}, log_path=self.log_path)
        got = list(follow_events(self.log_path, start_offset=offset,
                                 stop_after_one_pass=True))
        self.assertEqual(len(got), 2)
        self.assertEqual(got[0]["event"], "fanout_audit_run")
        self.assertEqual(got[1]["event"], "convention_check_run")

    def test_skips_malformed_lines(self):
        # A partial write at process kill must not wedge the follow loop — same contract as
        # `read_events`. Simulated by writing a raw broken line to the file.
        with self.log_path.open("w", encoding="utf-8") as fh:
            fh.write(json.dumps({"event": "fanout_audit_run", "payload": {}}) + "\n")
            fh.write("{malformed\n")
            fh.write(json.dumps({"event": "convention_check_run", "payload": {}}) + "\n")
        got = list(follow_events(self.log_path, start_offset=0, stop_after_one_pass=True))
        self.assertEqual(len(got), 2)

    def test_survives_truncation(self):
        # Log rotation → the file shrinks. The generator resets its offset to 0 and keeps
        # reading, instead of infinitely waiting for the old offset to be reached.
        append_event("fanout_audit_run", {"duration_s": 1.0}, log_path=self.log_path)
        big_offset = self.log_path.stat().st_size * 10  # pretend we'd read further
        # Truncate + write a new event.
        self.log_path.write_text(
            json.dumps({"event": "convention_check_run", "payload": {}}) + "\n",
            encoding="utf-8",
        )
        got = list(follow_events(self.log_path, start_offset=big_offset,
                                 stop_after_one_pass=True))
        self.assertEqual(len(got), 1)
        self.assertEqual(got[0]["event"], "convention_check_run")

    def test_follow_yields_after_delay(self):
        # End-to-end: prime the log with one event (so the generator has a known EOF offset
        # to start from), start the worker, then append two more from the main thread. The
        # worker must pick them up before the timeout. Short poll interval + tight timeout
        # so it can't hang CI.
        append_event("fanout_audit_run", {"duration_s": 0.5}, log_path=self.log_path)
        start = self.log_path.stat().st_size
        results: list[dict] = []
        gen_done = threading.Event()

        def _run():
            for event in follow_events(self.log_path, poll_interval=0.05,
                                       start_offset=start):
                results.append(event)
                if len(results) >= 2:
                    gen_done.set()
                    return

        worker = threading.Thread(target=_run, daemon=True)
        worker.start()
        time.sleep(0.1)
        append_event("fanout_audit_run", {"duration_s": 1.0}, log_path=self.log_path)
        append_event("convention_check_run", {"duration_s": 2.0}, log_path=self.log_path)
        self.assertTrue(gen_done.wait(timeout=5.0), "follow loop didn't pick up new events")
        worker.join(timeout=1.0)
        self.assertEqual(len(results), 2)


class MainTest(unittest.TestCase):
    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.log_path = pathlib.Path(self._tmp.name) / "events.jsonl"

    def test_no_follow_stream_prints_backlog_and_exits(self):
        # Non-TTY / --stream path: append-only formatted event stream, no dashboard.
        append_event("fanout_audit_run", {"duration_s": 1.0},
                     pr="#1263", log_path=self.log_path)
        append_event("fanout_audit_finding",
                     {"slug": "fanout-x", "file": "a.py", "line": 1,
                      "desc": "hello", "marker": "confirmed"},
                     pr="#1263", log_path=self.log_path)
        buf = io.StringIO()
        rc = main(["--no-follow", "--log-path", str(self.log_path), "--stream"],
                  stdout=buf, is_tty=False)
        self.assertEqual(rc, 0)
        out = buf.getvalue()
        self.assertIn("RUN", out)
        self.assertIn("FIND", out)  # abbreviated verb
        self.assertIn("hello", out)
        # `--stream` forces no colour.
        self.assertNotIn("\033[", out)

    def test_no_follow_dashboard_paints_once(self):
        # TTY dashboard path: on --no-follow it paints the dashboard once and exits. The
        # painted frame must include the title + counters, so a user can see the whole shape
        # of the log in one glance.
        append_event("fanout_audit_run", {"duration_s": 1.0}, log_path=self.log_path)
        append_event("fanout_audit_finding",
                     {"slug": "fanout-x", "file": "a.py", "line": 1,
                      "desc": "clone the guard casefold", "marker": "confirmed"},
                     log_path=self.log_path)
        buf = io.StringIO()
        rc = main(["--no-follow", "--log-path", str(self.log_path)],
                  stdout=buf, is_tty=True)
        self.assertEqual(rc, 0)
        out = buf.getvalue()
        # Title + section headers present.
        self.assertIn("Cecelia recital console", out)
        self.assertIn("by mechanism", out)
        self.assertIn("recent findings", out)
        self.assertIn("activity", out)
        # Counters land — 1 run + 1 confirmed finding.
        self.assertIn("1 run", out)
        self.assertIn("1 confirmed", out)
        # Cursor is restored on exit — otherwise the terminal is broken after `pixi run`.
        self.assertIn("\033[?25h", out)


class DashboardTest(unittest.TestCase):
    """The pure renderer — build a state, ask for a frame, assert on the frame."""

    def _state_with(self, events: list[dict]) -> DashboardState:
        state = DashboardState()
        for e in events:
            state.add(e)
        return state

    def test_empty_state_renders_placeholder(self):
        # A freshly-installed log with no events yet: dashboard still paints, tells the user
        # nothing has landed. A blank screen would look like a broken tool.
        state = DashboardState()
        out = render_dashboard(state, pathlib.Path("/tmp/x"), width=100, use_colour=False)
        self.assertIn("Cecelia recital console", out)
        self.assertIn("no events yet", out)
        self.assertIn("waiting for events", out)

    def test_counters_sum_across_mechanisms(self):
        events = [
            {"event": "fanout_audit_run", "ts": "2026-09-27T10:00:00Z",
             "payload": {"duration_s": 10.0}, "pr": None, "branch": None, "commit": None,
             "session": "s", "source": "live", "schema_version": 1},
            {"event": "convention_check_run", "ts": "2026-09-27T10:01:00Z",
             "payload": {"duration_s": 5.0}, "pr": None, "branch": None, "commit": None,
             "session": "s", "source": "live", "schema_version": 1},
            {"event": "fanout_audit_finding", "ts": "2026-09-27T10:00:00Z",
             "payload": {"slug": "f1", "file": "a.jl", "line": 1, "desc": "d",
                         "marker": "confirmed"},
             "pr": None, "branch": None, "commit": None,
             "session": "s", "source": "live", "schema_version": 1},
            {"event": "convention_check_finding", "ts": "2026-09-27T10:01:00Z",
             "payload": {"slug": "c1", "file": "b.py", "line": 2, "desc": "d",
                         "marker": "should reuse"},
             "pr": None, "branch": None, "commit": None,
             "session": "s", "source": "live", "schema_version": 1},
        ]
        out = render_dashboard(self._state_with(events), pathlib.Path("/tmp/x"),
                               width=120, use_colour=False)
        # Header totals — sum across mechanisms so the eye lands on total volume first.
        self.assertIn("2 runs", out)
        self.assertIn("2 findings", out)
        self.assertIn("1 confirmed", out)
        self.assertIn("1 should reuse", out)
        # Per-mechanism rows both present.
        self.assertIn("fnut", out)
        self.assertIn("conv", out)

    def test_findings_pane_shows_descriptions(self):
        # The whole cockpit point: recent findings render with their wrapped description so
        # the reader sees WHAT was flagged without opening the roundup.
        events = [
            {"event": "fanout_audit_finding", "ts": "2026-09-27T10:00:00Z",
             "payload": {"slug": "f1", "file": "log.py", "line": 121,
                         "desc": "twin guard not casefolded — mixed-case outcome still raises",
                         "marker": "confirmed"},
             "pr": "#1263", "branch": None, "commit": None,
             "session": "s", "source": "live", "schema_version": 1},
        ]
        out = render_dashboard(self._state_with(events), pathlib.Path("/tmp/x"),
                               width=120, use_colour=False)
        self.assertIn("recent findings", out)
        self.assertIn("twin guard not casefolded", out)
        self.assertIn("log.py:121", out)
        self.assertIn("#1263", out)

    def test_frame_fits_declared_height(self):
        # The whole reason `height` exists: the frame must fit the viewport, or the top of
        # the dashboard scrolls off-screen and the counters are invisible. Pack in a mix of
        # runs + findings and confirm the rendered frame is ≤ height rows.
        events = []
        for i in range(15):
            events.append({
                "event": "fanout_audit_run", "ts": f"2026-09-27T10:{i:02d}:00Z",
                "payload": {"duration_s": 1.0}, "pr": None, "branch": None, "commit": None,
                "session": "s", "source": "live", "schema_version": 1,
            })
        for i in range(8):
            events.append({
                "event": "fanout_audit_finding", "ts": f"2026-09-27T11:{i:02d}:00Z",
                "payload": {"slug": f"f{i}", "file": "x.py", "line": i,
                            "desc": ("a very long description that will surely wrap several times "
                                     "if not capped, which is exactly the failure we are pinning here ") * 3,
                            "marker": "confirmed"},
                "pr": None, "branch": None, "commit": None,
                "session": "s", "source": "live", "schema_version": 1,
            })
        state = self._state_with(events)
        for h in (25, 40, 60):
            out = render_dashboard(state, pathlib.Path("/tmp/x"),
                                   width=100, height=h, use_colour=False)
            n_lines = out.count("\n") + 1
            self.assertLessEqual(n_lines, h,
                                 f"frame overflows at height={h}: got {n_lines} lines")

    def test_findings_pane_is_bounded(self):
        # 20 findings in → the pane holds the most recent MAX_FINDINGS only. Bounded pane
        # is what keeps the dashboard readable on a busy day.
        events = []
        for i in range(20):
            events.append({
                "event": "fanout_audit_finding", "ts": f"2026-09-27T10:{i:02d}:00Z",
                "payload": {"slug": f"f{i}", "file": "x.py", "line": i,
                            "desc": f"finding {i}", "marker": "confirmed"},
                "pr": None, "branch": None, "commit": None,
                "session": "s", "source": "live", "schema_version": 1,
            })
        state = self._state_with(events)
        # The state itself bounds — the render just walks whatever's in the deque.
        self.assertLessEqual(len(state.findings), 6)
        out = render_dashboard(state, pathlib.Path("/tmp/x"), width=120, use_colour=False)
        # Newest survives, oldest evicted.
        self.assertIn("finding 19", out)
        self.assertNotIn("finding 0 ", out)


if __name__ == "__main__":
    unittest.main()
