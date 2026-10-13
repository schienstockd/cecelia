"""Tests for the agents pane: `claude_sessions` (which sessions are live, where each one is working),
`agents` (the per-branch recital join) and the pane rendering in `console`.

Every test builds a fake `~/.claude` and a fake `/proc` under a temp folder — never the machine's
real sessions. Pinned invariants:

- **Liveness is pid + start time.** A session file whose pid is gone, or now belongs to another
  process (`procStart` differs), is not a live session.
- **The working folder is the transcript's last `cwd`,** not the session file's launch `cwd`.
- **An unreadable session file shows as a row,** so a Claude Code format change is visible.
- **Open findings are per branch:** a resolution on another branch doesn't close a finding.
"""
from __future__ import annotations

import io
import json
import os
import pathlib
import tempfile
import unittest
from unittest import mock

from cecelia.effectiveness import console
import subprocess
import sys
import threading
import time

from cecelia.effectiveness.agents import (
    BADGE_SHAPES, AgentRow, PrCache, branch_recital, ci_state, pr_by_branch, session_badge, sort_rows)
from cecelia.effectiveness.claude_sessions import (
    Session, context_used, latest_recap, live_sessions, proc_start, read_context, recent_cwds,
    save_context, session_name)
from cecelia.effectiveness.log import unresolved_findings


def _stat_line(pid: int, start: str, comm: str = "claude") -> str:
    """A `/proc/<pid>/stat` line with `start` as field 22."""
    rest = ["S"] + ["0"] * 18 + [start] + ["0"] * 30   # fields 3..52; index 19 = field 22
    return f"{pid} ({comm}) " + " ".join(rest)


class ContextTest(unittest.TestCase):
    def test_used_percentage_from_statusline_input_null_after_compact(self):
        self.assertEqual(context_used({"context_window": {"used_percentage": 38}}), 38.0)
        self.assertIsNone(context_used({"context_window": {"used_percentage": None}}))
        self.assertIsNone(context_used({"context_window": "x"}))
        self.assertIsNone(context_used({}))

    def test_saved_per_session_rewritten_only_on_change_and_ids_stay_file_names(self):
        with tempfile.TemporaryDirectory() as d:
            folder = pathlib.Path(d) / "ctx"
            self.assertIsNone(read_context("abc", folder))
            save_context("abc", 15.0, folder)
            self.assertEqual(read_context("abc", folder), 15.0)
            path = folder / "abc.json"
            os.utime(path, (0, 0))
            save_context("abc", 15.0, folder)
            self.assertEqual(path.stat().st_mtime, 0)          # unchanged: not rewritten
            save_context("abc", None, folder)
            self.assertIsNone(read_context("abc", folder))
            save_context("../evil", 50.0, folder)
            self.assertEqual(sorted(p.name for p in pathlib.Path(d).rglob("*")), ["abc.json", "ctx"])

    def test_the_bar_and_the_number_share_one_scale(self):
        bar = lambda p: console.context_bar(p, use_colour=False)
        self.assertEqual(bar(15), "▄▁▁  15%")
        self.assertEqual(bar(0), "▁▁▁   0%")
        self.assertEqual(bar(50), "▇▄▁  50%")
        self.assertEqual(bar(100), "▇▇▇ 100%")   # no `█`: it would touch the row above
        self.assertEqual(bar(None), "—       ")
        self.assertEqual(len(bar(38)), 8)
        self.assertNotIn(console._RED, console.context_bar(59, use_colour=True))
        self.assertIn(console._ORANGE, console.context_bar(60, use_colour=True))
        self.assertIn(console._RED, console.context_bar(80, use_colour=True))


class _FakeHome(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        root = pathlib.Path(tmp.name)
        self.home, self.proc = root / "claude", root / "proc"
        (self.home / "sessions").mkdir(parents=True)
        (self.home / "projects" / "-ws").mkdir(parents=True)
        self.proc.mkdir()

    def process(self, pid: int, start: str, comm: str = "claude") -> None:
        (self.proc / str(pid)).mkdir()
        (self.proc / str(pid) / "stat").write_text(_stat_line(pid, start, comm), encoding="utf-8")

    def session(self, pid: int, start: str, sid: str, *, name: str = "s", status: str = "idle",
                cwd: str = "/ws", transcript_cwds: list[str] | None = None) -> None:
        meta = {"pid": pid, "sessionId": sid, "cwd": cwd, "procStart": start, "name": name,
                "status": status}
        (self.home / "sessions" / f"{pid}.json").write_text(json.dumps(meta), encoding="utf-8")
        if transcript_cwds is not None:
            lines = [json.dumps({"type": "user", "cwd": c}) for c in transcript_cwds]
            (self.home / "projects" / "-ws" / f"{sid}.jsonl").write_text(
                "\n".join(lines) + "\n", encoding="utf-8")

    def live(self) -> list[Session] | None:
        return live_sessions(self.home, proc=self.proc, context=self.home / "ctx")


class LiveSessionsTest(_FakeHome):
    def test_live_session_takes_the_last_transcript_cwd(self):
        self.process(10, "500")
        self.session(10, "500", "a", name="cecelia-56", transcript_cwds=["/ws", "/ws/wt-a", "/ws/wt-b"])
        self.assertEqual(self.live(), [Session(10, "cecelia-56", "idle", "/ws/wt-b", session_id="a",
                                               recent_cwds=("/ws/wt-b", "/ws/wt-a", "/ws"))])

    def test_live_session_carries_its_saved_context_fill(self):
        self.process(10, "500")
        self.session(10, "500", "a", transcript_cwds=["/ws"])
        save_context("a", 42.0, self.home / "ctx")
        self.assertEqual(self.live()[0].context_pct, 42.0)

    def test_session_name_looks_up_the_id_without_proc(self):
        self.session(10, "500", "abc", name="cecelia-56")
        self.assertEqual(session_name("abc", self.home), "cecelia-56")
        self.assertIsNone(session_name("nope", self.home))

    def test_reused_pid_is_not_live(self):
        self.process(10, "999")             # same pid, a different process now
        self.session(10, "500", "a", transcript_cwds=["/ws/wt"])
        self.assertEqual(self.live(), [])

    def test_stale_file_without_a_process_is_skipped(self):
        self.session(10, "500", "a", transcript_cwds=["/ws/wt"])
        self.assertEqual(self.live(), [])

    def test_no_transcript_yet_falls_back_to_the_launch_folder(self):
        self.process(10, "500")
        self.session(10, "500", "a", cwd="/ws")
        self.assertEqual(self.live()[0].cwd, "/ws")

    def test_unparseable_session_file_of_a_live_pid_is_a_visible_row(self):
        self.process(10, "500")
        (self.home / "sessions" / "10.json").write_text("{not json", encoding="utf-8")
        self.assertEqual(self.live(), [Session(10, "10", "?", None, readable=False)])

    def test_a_headless_claude_p_run_is_not_a_window(self):
        self.process(10, "500")
        self.session(10, "500", "a", transcript_cwds=["/ws/wt"])
        meta = json.loads((self.home / "sessions" / "10.json").read_text(encoding="utf-8"))
        (self.home / "sessions" / "10.json").write_text(
            json.dumps({**meta, "kind": "interactive", "entrypoint": "sdk-cli"}), encoding="utf-8")
        self.assertEqual(self.live(), [])

    def test_no_sessions_folder_means_no_sessions(self):
        self.assertEqual(live_sessions(self.home / "missing", proc=self.proc), [])

    def test_no_proc_means_unknown(self):
        self.assertIsNone(live_sessions(self.home, proc=self.proc / "missing"))

    def test_proc_start_counts_fields_after_a_command_name_with_spaces(self):
        self.process(10, "777", comm="claude (x) y")
        self.assertEqual(proc_start(10, self.proc), "777")
        self.assertIsNone(proc_start(11, self.proc))


class RecentCwdsTest(unittest.TestCase):
    def test_reads_only_the_tail_and_skips_a_cut_first_line(self):
        with tempfile.TemporaryDirectory() as d:
            path = pathlib.Path(d) / "t.jsonl"
            early = json.dumps({"cwd": "/early", "pad": "x" * 500})
            late = [json.dumps({"type": "progress"}), json.dumps({"cwd": "/late"}),
                    json.dumps({"type": "summary"})]
            path.write_text("\n".join([early, *late]) + "\n", encoding="utf-8")
            self.assertEqual(recent_cwds(path, tail_bytes=200), ("/late",))
            path.write_text(early + "\n" + late[0] + "\n", encoding="utf-8")
            self.assertEqual(recent_cwds(path, tail_bytes=60), ())   # only a cut line has a cwd

    def test_newest_first_without_repeats(self):
        with tempfile.TemporaryDirectory() as d:
            path = pathlib.Path(d) / "t.jsonl"
            path.write_text("\n".join(json.dumps({"cwd": c}) for c in ("/a", "/wt", "/ws", "/wt", "/ws")),
                            encoding="utf-8")
            self.assertEqual(recent_cwds(path), ("/ws", "/wt", "/a"))


class LatestRecapTest(unittest.TestCase):
    def recap(self, *entries: dict, tail_bytes: int = 512 * 1024):
        with tempfile.TemporaryDirectory() as d:
            path = pathlib.Path(d) / "t.jsonl"
            path.write_text("\n".join(json.dumps(e) for e in entries) + "\n", encoding="utf-8")
            return latest_recap(path, tail_bytes=tail_bytes)

    @staticmethod
    def summary(text: str, ts: str = "2026-10-13T01:00:00Z") -> dict:
        return {"type": "system", "subtype": "away_summary", "content": text, "timestamp": ts}

    def test_the_newest_recap_wins(self):
        self.assertEqual(self.recap(self.summary("old"), self.summary("new", "t2")), ("t2", "new"))

    def test_work_driven_by_tool_results_and_notifications_keeps_it_current(self):
        notification = {"type": "user", "origin": {"kind": "task-notification"},
                        "message": {"content": "<task-notification>"}}
        peer = {"type": "user", "origin": {"kind": "peer"}, "message": {"content": "hi"}}
        tool_result = {"type": "user", "message": {"content": [{"type": "tool_result"}]}}
        meta = {"type": "user", "isMeta": True, "message": {"content": "reminder"}}
        self.assertEqual(self.recap(self.summary("r"), notification, peer, tool_result, meta,
                                    {"type": "assistant"}), ("2026-10-13T01:00:00Z", "r"))

    def test_a_prompt_typed_after_it_makes_it_out_of_date(self):
        self.assertIsNone(self.recap(self.summary("r"), {"type": "user", "message": {"content": "go"}}))
        typed_list = {"type": "user", "message": {"content": [{"type": "text", "text": "go"}]}}
        self.assertIsNone(self.recap(self.summary("r"), typed_list))
        human = {"type": "user", "origin": {"kind": "human"}, "message": {"content": "go"}}
        self.assertIsNone(self.recap(self.summary("r"), human))

    def test_claude_codes_settings_hint_is_dropped(self):
        self.assertEqual(self.recap(self.summary("Done. Next: commit. (disable recaps in /config)"))[1],
                         "Done. Next: commit.")

    def test_none_without_a_recap_in_the_tail(self):
        self.assertIsNone(self.recap({"type": "assistant"}))
        self.assertIsNone(self.recap(self.summary("r"), {"type": "assistant", "pad": "x" * 500},
                                     tail_bytes=200))


def _row(event: str, branch: str, slug: str | None = None, commit: str = "c1", **payload) -> dict:
    return {"event": event, "branch": branch, "commit": commit, "ts": payload.pop("ts", ""),
            "payload": {**({"slug": slug} if slug else {}), **payload}}


class RecitalJoinTest(unittest.TestCase):
    def test_open_findings_are_per_branch(self):
        rows = [_row("fanout_audit_finding", "a", "f1"), _row("fanout_audit_finding", "b", "f1"),
                _row("fanout_audit_finding_resolved", "b", "f1")]
        self.assertEqual(list(unresolved_findings(rows, "a")), ["f1"])
        self.assertEqual(unresolved_findings(rows, "b"), {})

    def test_commit_narrows_findings_but_resolutions_count_from_any_commit(self):
        rows = [_row("convention_check_finding", "a", "c1-f", commit="c1"),
                _row("convention_check_finding", "a", "c2-f", commit="c2"),
                _row("convention_check_finding_resolved", "a", "c1-f", commit="c2")]
        self.assertEqual(unresolved_findings(rows, "a", commit="c1"), {})
        self.assertEqual(list(unresolved_findings(rows, "a", commit="c2")), ["c2-f"])
        self.assertEqual(list(unresolved_findings(rows, "a")), ["c2-f"])

    def test_branch_recital_counts_rounds_and_ignores_advisory_findings(self):
        rows = [_row("fanout_audit_run", "a"), _row("fanout_audit_run", "a"),
                _row("convention_check_run", "a"), _row("fanout_audit_run", "b"),
                _row("fanout_audit_finding", "a", "f1"), _row("fanout_audit_advisory", "a")]
        rounds, findings = branch_recital(rows, "a")
        self.assertEqual((rounds, [o for _, o, _ in findings]), (2, ["unresolved"]))

    def test_each_finding_carries_its_newest_outcome_and_open_ones_come_first(self):
        rows = [_row("fanout_audit_finding", "a", "f1", ts="1"),
                _row("convention_check_finding", "a", "f2", ts="2"),
                _row("fanout_audit_finding", "a", "f3", ts="3"),
                _row("fanout_audit_finding_resolved", "a", "f1", outcome="dropped_no_action", ts="4"),
                _row("fanout_audit_finding_resolved", "a", "f1", outcome="fixed_pre_commit", ts="5"),
                _row("convention_check_finding_resolved", "a", "f2", outcome="shipped_with_finding",
                     reason="needed for the release")]
        _, findings = branch_recital(rows, "a")
        self.assertEqual([(f["payload"]["slug"], o) for f, o, _ in findings],
                         [("f3", "unresolved"), ("f2", "shipped_with_finding"), ("f1", "fixed_pre_commit")])
        self.assertEqual(findings[1][2]["payload"]["reason"], "needed for the release")

    def test_idle_first_busy_last_unreadable_at_the_bottom(self):
        rows = [AgentRow("u", "?", None, None, readable=False), AgentRow("b", "busy", "w", "x"),
                AgentRow("s", "shell", "w", "x"), AgentRow("i", "idle", "w", "x")]
        self.assertEqual([r.name for r in sort_rows(rows)], ["i", "s", "b", "u"])


class PrCiTest(unittest.TestCase):
    def test_ci_state_failure_beats_running_beats_pass(self):
        ok = {"__typename": "CheckRun", "status": "COMPLETED", "conclusion": "SUCCESS"}
        running = {"__typename": "CheckRun", "status": "IN_PROGRESS", "conclusion": ""}
        failed = {"__typename": "CheckRun", "status": "COMPLETED", "conclusion": "FAILURE"}
        pending_ctx = {"__typename": "StatusContext", "state": "PENDING"}
        self.assertIsNone(ci_state([]))
        self.assertEqual(ci_state([ok, ok]), "pass")
        self.assertEqual(ci_state([ok, running]), "running")
        self.assertEqual(ci_state([ok, pending_ctx]), "running")
        self.assertEqual(ci_state([running, failed]), "fail")
        self.assertEqual(ci_state([{"__typename": "StatusContext", "state": "ERROR"}]), "fail")

    def test_newest_pr_wins_per_branch(self):
        prs = [{"number": 9, "headRefName": "a"}, {"number": 3, "headRefName": "a"},
               {"number": 5, "headRefName": "b"}]
        self.assertEqual({b: p["number"] for b, p in pr_by_branch(prs).items()}, {"a": 9, "b": 5})

    def test_a_merged_session_that_is_not_working_is_done_and_sorts_after_idle(self):
        rows = [AgentRow("b", "busy", "w", "x"), AgentRow("d", "idle", "w", "y", pr_state="MERGED"),
                AgentRow("i", "idle", "w", "z"), AgentRow("m", "busy", "w", "v", pr_state="MERGED")]
        self.assertEqual([r.name for r in sort_rows(rows)], ["i", "d", "b", "m"])
        self.assertTrue(rows[1].done)
        self.assertFalse(rows[3].done)      # still working after the merge
        state = console.DashboardState()
        state.agents = [rows[1]]
        self.assertEqual(console.agents_block(state, 100, use_colour=False)[2].split()[1], "done")

    def test_idle_with_failed_ci_sorts_first(self):
        rows = [AgentRow("a", "idle", "w", "x", ci="pass"), AgentRow("b", "idle", "w", "y", ci="fail")]
        self.assertEqual([r.name for r in sort_rows(rows)], ["b", "a"])

    def test_cache_fetches_in_the_background_and_keeps_the_last_good_list(self):
        gate, calls = threading.Event(), []

        def fetch():
            calls.append(1)
            gate.wait(2)
            return None if len(calls) > 1 else [{"number": 1}]

        cache = PrCache(max_age=0.0, fetch=fetch)
        self.assertEqual(cache.get(), [])          # returns at once; the fetch is still blocked
        gate.set()
        deadline = time.monotonic() + 2
        while cache.get() == [] and time.monotonic() < deadline:
            time.sleep(0.01)
        self.assertEqual(cache._prs, [{"number": 1}])
        while len(calls) < 2 and time.monotonic() < deadline:
            cache.get(); time.sleep(0.01)
        time.sleep(0.05)
        self.assertEqual(cache.get(), [{"number": 1}])   # a failed refresh keeps the old list


class BadgeTest(unittest.TestCase):
    def test_a_session_always_gets_the_same_badge_and_ids_spread_over_shapes(self):
        self.assertEqual(session_badge("abc"), session_badge("abc"))
        self.assertIn(session_badge("abc", use_colour=False), BADGE_SHAPES)
        self.assertGreater(len({session_badge(f"id-{i}") for i in range(40)}), 10)

    def test_the_statusline_wrapper_puts_badge_and_name_before_the_wrapped_line(self):
        script = pathlib.Path(__file__).resolve().parents[3] / "scripts" / "statusline_badge.py"
        with tempfile.TemporaryDirectory() as d:
            (pathlib.Path(d) / "sessions").mkdir()
            (pathlib.Path(d) / "sessions" / "1.json").write_text(
                json.dumps({"sessionId": "abc", "name": "cecelia-56"}), encoding="utf-8")
            # Its own cache folder too: the wrapper saves the context fill there.
            env = {**os.environ, "CLAUDE_CONFIG_DIR": d, "XDG_CACHE_HOME": d}
            wrapped = [sys.executable, "-c", "import sys; sys.stdin.read(); print('first'); print('second')"]

            def run(stdin: bytes, argv: list[str]) -> str:
                # The wrapped command's own line endings pass through untouched: `\r\n` on Windows.
                return subprocess.run([sys.executable, str(script), *argv], input=stdin, env=env,
                                      capture_output=True, check=True
                                      ).stdout.decode("utf-8").replace("\r\n", "\n")

            out = run(b'{"session_id": "abc"}', wrapped)
            self.assertEqual(out, f"{session_badge('abc')} cecelia-56  first\nsecond\n")
            self.assertEqual(run(b"not json", wrapped), "first\nsecond\n")    # badge unknown
            self.assertEqual(run(b'{"session_id": "abc"}', ["/no/such/cmd"]),
                             f"{session_badge('abc')} cecelia-56\n")             # wrapped failed
            run(b'{"session_id": "abc", "context_window": {"used_percentage": 15}}', wrapped)
            self.assertEqual(read_context("abc", pathlib.Path(d) / "cecelia-agents"), 15.0)


class PaneTest(unittest.TestCase):
    def test_rows_render_plain_and_fit_the_width(self):
        state = console.DashboardState()
        state.agents = [AgentRow("cecelia-56", "idle", "cecelia-" + "w" * 80, "feat/x", 3, 2,
                                 pr=1632, pr_state="OPEN", ci="fail"),
                        AgentRow("123", "?", None, None, readable=False),
                        AgentRow("cecelia-9c", "idle", "wt", "y", pr=1631, pr_state="MERGED", ci="pass")]
        lines = console.agents_block(state, 80, use_colour=False)
        self.assertIn("agents", lines[0])
        self.assertEqual(lines[1].split(),
                         ["agent", "state", "ctx", "worktree", "branch", "recitals", "open", "pr", "ci"])
        self.assertEqual(lines[2].split()[-4:], ["3", "2", "#1632", "fail"])
        self.assertEqual(lines[2].index("cecelia-56"), lines[1].index("agent"))   # badge column
        self.assertEqual(lines[4].split()[-2:], ["#1631", "merged"])   # merged hides its CI
        self.assertIn("…", lines[2])                       # the long worktree name is cut
        self.assertIn("unreadable", lines[3])
        self.assertTrue(all(len(line) <= 80 for line in lines))

    def test_a_long_session_name_is_cut_to_its_column(self):
        state = console.DashboardState()
        state.agents = [AgentRow("cecelia-prune-session-gate-13", "busy", "w", "b", session_id="s"),
                        AgentRow("cecelia-02", "busy", "w", "b", session_id="t")]
        lines = console.agents_block(state, 100, use_colour=False)
        self.assertEqual(lines[2].index(" busy"), lines[3].index(" busy"))

    def test_right_arrow_view_lists_every_finding_like_the_recital_console(self):
        finding = _row("fanout_audit_finding", "x", "f1", file="a.py", line=3, desc="guard missing",
                       marker="confirmed")
        fixed = _row("convention_check_finding", "x", "f2", file="b.py", line=9, desc="old helper",
                     marker="should reuse")
        res = _row("fanout_audit_finding_resolved", "x", "f1", outcome="dropped_no_action",
                   reason="out of scope")
        state = console.DashboardState()
        state.agents = [AgentRow("s", "idle", "w", "x", findings=(
                            (finding, "dropped_no_action", res), (fixed, "fixed_pre_commit", None))),
                        AgentRow("t", "busy", "v", "y")]
        collapsed = console.agents_block(state, 100, use_colour=False)
        self.assertIn("→ recaps + findings", collapsed[0])
        self.assertNotIn("guard missing", "\n".join(collapsed))
        state.agents_expanded = True
        lines = console.agents_block(state, 100, use_colour=False)
        text = "\n".join(lines)
        self.assertIn("0 open · 1 fixed · 0 shipped · 0 false pos · 1 dropped", text)
        # The recital console's own head line, the outcome in front, the description under it.
        head = next(line for line in lines if "a.py:3" in line)
        self.assertRegex(head, r"^\s+dropped\s+\S+ fnut FIND confirmed  a\.py:3$")
        self.assertIn("↳ guard missing", text)
        self.assertIn("why out of scope", text)
        self.assertIn("fixed", next(line for line in lines if "b.py:9" in line))
        self.assertIn("no recital findings", text)
        self.assertTrue(all(len(line) <= 100 for line in lines))

    def test_right_arrow_view_shows_the_recap_above_the_findings(self):
        state = console.DashboardState()
        text = "We're building the agents console. " * 6
        state.agents = [AgentRow("s", "idle", "w", "x", recap=("2026-10-13T01:02:45Z", text)),
                        AgentRow("t", "idle", None, None, recap=("2026-10-13T01:02:45Z", "no repo"))]
        self.assertNotIn("recap", "\n".join(console.agents_block(state, 80, use_colour=False)[1:]))
        state.agents_expanded = True
        lines = console.agents_block(state, 80, use_colour=False)
        first = next(i for i, line in enumerate(lines) if "recap" in line)
        self.assertRegex(lines[first], r"^\s+recap \d+[smhd] ago\s+We're building")
        self.assertIn("no recital findings", "\n".join(lines[first:]))
        wrapped = lines[first + 1]
        self.assertEqual(len(wrapped) - len(wrapped.lstrip()), lines[first].index("We're"))
        self.assertIn("no repo", "\n".join(lines))   # a session outside any repo still has one
        self.assertTrue(all(len(line) <= 80 for line in lines))

    def test_recap_age_is_its_largest_whole_unit(self):
        now = console._dt.datetime(2026, 10, 13, 12, 0, tzinfo=console._dt.timezone.utc)
        self.assertEqual([console._fmt_ago(ts, now) for ts in (
            "2026-10-13T11:59:20Z", "2026-10-13T11:47:00Z", "2026-10-13T09:30:00Z",
            "2026-10-10T12:00:00Z", "garbage")], ["40s", "13m", "2h", "3d", "?"])

    def test_the_agents_view_scrolls_when_it_outgrows_the_terminal(self):
        state = console.DashboardState()
        state.agents = [AgentRow(f"s{i}", "idle", "w", "x") for i in range(40)]
        frame = console.render_dashboard(state, pathlib.Path("/x"), width=100, height=15,
                                         use_colour=False, agents_only=True)
        self.assertLessEqual(len(frame.splitlines()), 15)
        self.assertIn("PgDn", frame)

    def test_no_pane_until_agents_are_read(self):
        self.assertEqual(console.agents_block(console.DashboardState(), 80, use_colour=False), [])

    def test_piped_agents_prints_one_snapshot(self):
        # No live sessions on any OS — macOS and Windows have no /proc, which reads as "unknown".
        with tempfile.TemporaryDirectory() as d, \
                mock.patch.object(console, "live_sessions", return_value=[]):
            buf = io.StringIO()
            with mock.patch.object(console, "recent_prs", return_value=None):
                rc = console.main(["--agents", "--log-path", str(pathlib.Path(d) / "e.jsonl")],
                                  stdout=buf, is_tty=False)
        self.assertEqual(rc, 0)
        self.assertIn("no live Claude Code sessions", buf.getvalue())
        self.assertNotIn("\033[", buf.getvalue())


if __name__ == "__main__":
    unittest.main()
