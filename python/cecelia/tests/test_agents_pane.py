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

from cecelia.effectiveness import agents, console
import subprocess
import sys
import threading
import time

from cecelia.effectiveness.agents import (
    BADGE_COLOURS, AgentRow, PrCache, badge_slots, branch_recital, ci_state, pr_by_branch, session_badge,
    sort_rows)
from cecelia.effectiveness.claude_sessions import (
    Session, context_used, ends_with_question, latest_recap, live_sessions, proc_start, read_context, recent_cwds,
    save_context, session_name)
from cecelia.effectiveness.log import unresolved_findings


def _stat_line(pid: int, start: str, comm: str = "claude", ppid: int = 0) -> str:
    """A `/proc/<pid>/stat` line with `ppid` as field 4 and `start` as field 22."""
    rest = ["S", str(ppid)] + ["0"] * 17 + [start] + ["0"] * 30   # fields 3..52; index 19 = field 22
    return f"{pid} ({comm}) " + " ".join(rest)


#: `agent_rows` saves badge slots under `$XDG_CACHE_HOME`: made-up sessions must not overwrite the
#: real file, which would hand every open window a new colour.
_cache = tempfile.TemporaryDirectory()
_env = mock.patch.dict(os.environ, {"XDG_CACHE_HOME": _cache.name})


def setUpModule():
    _env.start()


def tearDownModule():
    _env.stop()
    _cache.cleanup()


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

    def process(self, pid: int, start: str, comm: str = "claude", ppid: int = 0,
                argv: tuple[str, ...] = ("claude",)) -> None:
        (self.proc / str(pid)).mkdir()
        (self.proc / str(pid) / "stat").write_text(_stat_line(pid, start, comm, ppid), encoding="utf-8")
        (self.proc / str(pid) / "cmdline").write_bytes(b"\0".join(a.encode() for a in argv) + b"\0")

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

    def test_a_session_opened_by_judge_review_is_marked(self):
        self.process(5, "1", argv=("pixi", "run", "judge-review"))
        self.process(6, "1", ppid=5, argv=("/env/bin/python", "scripts/judge/review.py"))
        self.process(10, "500", ppid=6)
        self.process(11, "501", ppid=5)
        self.process(12, "502", argv=("python", "scripts/other/review.py"))
        self.process(13, "503", ppid=12)
        for pid, start, sid in ((10, "500", "a"), (11, "501", "b"), (13, "503", "c")):
            self.session(pid, start, sid, transcript_cwds=["/ws"])
        self.assertEqual({s.session_id: s.judge for s in self.live()}, {"a": True, "b": False, "c": False})

    def test_a_session_with_recital_anywhere_below_it_is_marked(self):
        self.process(10, "500")
        self.process(20, "1", ppid=10, argv=("bash", "-c", "pixi run recital"))
        self.process(21, "1", ppid=20, argv=("/env/bin/python", "scripts/recital.py"))
        self.process(11, "501")
        self.process(30, "1", ppid=11, argv=("python", "scripts/other.py"))
        self.process(12, "502", ppid=21)   # recital *above* a session doesn't count
        for pid, start, sid in ((10, "500", "a"), (11, "501", "b"), (12, "502", "c")):
            self.session(pid, start, sid, transcript_cwds=["/ws"])
        self.assertEqual({s.session_id: s.recital for s in self.live()}, {"a": True, "b": False, "c": False})

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

    def test_the_auto_fix_agent_is_shown_but_not_its_recital_reviewers(self):
        def headless(pid, sid):
            self.session(pid, "500", sid, name="cecelia-9", transcript_cwds=["/ws/cecelia-fix-x"])
            meta = json.loads((self.home / "sessions" / f"{pid}.json").read_text(encoding="utf-8"))
            (self.home / "sessions" / f"{pid}.json").write_text(
                json.dumps({**meta, "entrypoint": "sdk-cli"}), encoding="utf-8")
        review = ("python", "scripts/judge/review.py", "--auto", "--limit", "2")
        self.process(5, "100", comm="python", argv=review)
        self.process(10, "500", ppid=5)                                                # the fix agent
        self.process(6, "100", comm="python", ppid=5, argv=("python", "/ws/scripts/recital.py", "--stdin"))
        self.process(11, "500", ppid=6)                                                # a reviewer
        headless(10, "a")
        headless(11, "b")
        live = self.live()
        self.assertEqual([(s.pid, s.name, s.auto, s.judge, s.cwd) for s in live],
                         [(10, "auto-fix", True, False, "/ws/cecelia-fix-x")])

    def test_the_daemons_warm_spare_is_not_a_session_but_a_background_job_is(self):
        for pid, extra in ((10, {"kind": "bg", "spare": True}), (11, {"kind": "bg"})):
            self.process(pid, "500")
            self.session(pid, "500", f"s{pid}", name=f"n{pid}")
            meta = json.loads((self.home / "sessions" / f"{pid}.json").read_text(encoding="utf-8"))
            (self.home / "sessions" / f"{pid}.json").write_text(json.dumps({**meta, **extra}), encoding="utf-8")
        self.assertEqual([s.name for s in self.live()], ["n11"])

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


class EndsWithQuestionTest(unittest.TestCase):
    def asks(self, *entries: dict) -> bool:
        with tempfile.TemporaryDirectory() as d:
            path = pathlib.Path(d) / "t.jsonl"
            path.write_text("\n".join(json.dumps(e) for e in entries) + "\n", encoding="utf-8")
            return ends_with_question(path)

    @staticmethod
    def said(*blocks: dict) -> dict:
        return {"type": "assistant", "message": {"content": list(blocks)}}

    text = staticmethod(lambda t: {"type": "text", "text": t})
    thinking = {"type": "thinking", "thinking": "…"}

    def test_a_reply_ending_on_a_question_asks_after_thinking_and_bookkeeping(self):
        self.assertTrue(self.asks(self.said(self.thinking), self.said(self.text("Push it?\n")),
                                  {"type": "system", "subtype": "away_summary"}, {"type": "attachment"}))
        self.assertTrue(self.asks(self.said(self.text("**Go?**"))))

    def test_a_statement_a_tool_call_or_anything_newer_from_the_user_does_not(self):
        self.assertFalse(self.asks(self.said(self.text("I'll report when CI finishes."))))
        self.assertFalse(self.asks(self.said(self.text("Go?"), {"type": "tool_use"})))
        for after in ({"type": "user", "message": {"content": "go"}},
                      {"type": "user", "message": {"content": [{"type": "tool_result"}]}},
                      {"type": "user", "origin": {"kind": "task-notification"},
                       "message": {"content": "<task-notification>"}}):
            self.assertFalse(self.asks(self.said(self.text("Go?")), after))
        self.assertFalse(self.asks({"type": "system"}))


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

    def test_idle_waiting_on_running_ci_sorts_after_idle_and_done_but_before_shell(self):
        rows = [AgentRow("a", "idle", "w", "x", pr=1, pr_state="OPEN", ci="running"),
                AgentRow("s", "shell", "w", "x"),
                AgentRow("d", "idle", "w", "x", pr=2, pr_state="MERGED", ci="pass"),
                AgentRow("z", "idle", "w", "y"),
                AgentRow("p", "idle", "w", "x", pr=3, pr_state="OPEN", ci="pass")]
        self.assertEqual([r.name for r in sort_rows(rows)], ["p", "z", "d", "a", "s"])

    def test_a_conflicting_open_pr_shows_conflict_and_sorts_with_failed_ci(self):
        rows = [AgentRow("a", "idle", "w", "x", pr=1, pr_state="OPEN", ci="pass"),
                AgentRow("c", "idle", "w", "y", pr=2, pr_state="OPEN", ci="running", conflict=True)]
        self.assertEqual([r.name for r in sort_rows(rows)], ["c", "a"])
        self.assertEqual(console._pr_word(rows[1])[0], "conflict")
        self.assertEqual(console._pr_word(rows[0])[0], "pass")

    def test_conflict_is_read_only_from_an_open_pr_marked_conflicting(self):
        prs = [{"number": 1, "headRefName": "x", "state": "OPEN", "mergeable": "CONFLICTING"},
               {"number": 2, "headRefName": "y", "state": "OPEN", "mergeable": "UNKNOWN"},
               {"number": 3, "headRefName": "z", "state": "MERGED", "mergeable": "CONFLICTING"}]
        sessions = [Session(1, "s1", "idle", "/x"), Session(2, "s2", "idle", "/y"),
                    Session(3, "s3", "idle", "/z")]
        with mock.patch.object(agents, "_checkout", side_effect=lambda s: s.cwd), \
                mock.patch.object(agents, "git_output", side_effect=lambda *a, cwd: cwd[1:]):
            rows = {r.name: r.conflict for r in agents.agent_rows(sessions, [], prs)}
        self.assertEqual(rows, {"s1": True, "s2": False, "s3": False})

    def test_a_session_asking_sorts_first_and_says_asks(self):
        rows = [AgentRow("f", "idle", "w", "x", pr=1, pr_state="OPEN", ci="fail"),
                AgentRow("q", "shell", "w", "y", pr=2, pr_state="OPEN", ci="running", asks=True)]
        self.assertEqual([r.name for r in sort_rows(rows)], ["q", "f"])
        state = console.DashboardState()
        state.agents = rows
        self.assertEqual(console.agents_block(state, 100, use_colour=False)[3].split()[1], "asks")
        coloured = console.agents_block(state, 100, use_colour=True)[3]
        self.assertIn(console._MAGENTA + "asks", coloured)   # apart from idle's yellow

    def test_a_waiting_judge_review_session_comes_first_and_carries_a_star(self):
        rows = [AgentRow("q", "idle", "w", "y", asks=True),
                AgentRow("cecelia-28", "idle", "w", "x", judge=True),
                AgentRow("j", "busy", "w", "z", judge=True)]
        self.assertEqual([r.name for r in sort_rows(rows)], ["cecelia-28", "q", "j"])
        state = console.DashboardState()
        state.agents = sort_rows(rows)
        lines = console.agents_block(state, 100, use_colour=False)
        self.assertIn("* judge-review", lines[0])
        self.assertEqual(lines[2].split()[0], "cecelia-28*")
        self.assertEqual(lines[2].index(" idle"), lines[3].index(" asks"))   # columns still line up
        self.assertEqual(console._name_cell(AgentRow("auto-fix", "busy", "w", "x", auto=True)), "auto-fix^   ")
        self.assertEqual(console._name_cell(AgentRow("cecelia-prune-x", "idle", "w", "x", judge=True)),
                         "cecelia-pr…*")

    def test_a_judge_review_session_waiting_on_ci_waits_with_the_others_but_leads_them(self):
        rows = [AgentRow("a", "idle", "w", "x", pr=1, pr_state="OPEN", ci="running"),
                AgentRow("j", "idle", "w", "y", pr=2, pr_state="OPEN", ci="running", judge=True),
                AgentRow("p", "idle", "w", "z", pr=3, pr_state="OPEN", ci="pass")]
        self.assertEqual([r.name for r in sort_rows(rows)], ["p", "j", "a"])

    def test_a_busy_session_never_asks(self):
        sessions = [Session(1, "s1", "busy", "/x", asks=True), Session(2, "s2", "shell", "/y", asks=True)]
        with mock.patch.object(agents, "_checkout", return_value=None):
            self.assertEqual({r.name: r.asks for r in agents.agent_rows(sessions, [], [])},
                             {"s1": False, "s2": True})

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
    def test_live_sessions_hold_different_colours_until_they_run_out(self):
        with tempfile.TemporaryDirectory() as d:
            folder = pathlib.Path(d)
            self.assertEqual(badge_slots(["a", "b", "c"], folder), {"a": 0, "b": 1, "c": 2})
            # b closes: a and c keep theirs, the newcomer takes the freed one.
            self.assertEqual(badge_slots(["a", "c", "d"], folder), {"a": 0, "c": 2, "d": 1})
            ids = [f"id-{i}" for i in range(len(BADGE_COLOURS) + 1)]
            slots = badge_slots(ids, folder)
            self.assertEqual(len({slots[i] for i in ids[:-1]}), len(BADGE_COLOURS))
            self.assertEqual(slots[ids[-1]], len(BADGE_COLOURS))     # past the palette: wraps
            self.assertEqual(session_badge("x", slot=len(BADGE_COLOURS)), session_badge("y", slot=0))

    def test_the_badge_is_a_dot_in_its_colour(self):
        self.assertEqual(session_badge("abc", use_colour=False), "●")
        self.assertEqual(session_badge("abc", slot=3), f"{BADGE_COLOURS[3]}●\033[0m")
        self.assertEqual(session_badge("abc"), session_badge("abc"))   # no slot: from the id

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
            # Its slot as the pane assigns it: the first free one, saved for the pane to read back.
            badge = f"{session_badge('abc', slot=0)} cecelia-56"
            self.assertEqual(out, f"{badge}  first\nsecond\n")
            self.assertEqual(run(b"not json", wrapped), "first\nsecond\n")    # badge unknown
            self.assertEqual(run(b'{"session_id": "abc"}', ["/no/such/cmd"]),
                             f"{badge}\n")                                     # wrapped failed
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

    def test_a_session_running_recital_shows_recite_in_the_state_column(self):
        state = console.DashboardState()
        state.agents = [AgentRow("a", "busy", "w", "b", recital=True), AgentRow("b", "busy", "w", "b")]
        lines = console.agents_block(state, 100, use_colour=False)
        self.assertEqual(lines[2].split()[1], "recite")
        self.assertEqual(lines[2].index("recite"), lines[3].index("busy"))

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
        collapsed = console.agents_block(state, 100, use_colour=False, keys=True)
        self.assertIn("→ recaps + findings", collapsed[0])
        self.assertNotIn("guard missing", "\n".join(collapsed))
        state.agents_expanded = True
        lines = console.agents_block(state, 100, use_colour=False, keys=True)
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

    def test_recital_console_offers_no_arrow_and_badges_each_finding_with_its_session(self):
        # There ← / → open the findings pane's full text, so the agents header must not offer
        # them; instead a finding's branch row leads with the badge and short name of the session
        # on that branch — two sessions can share a badge, never a name.
        state = console.DashboardState()
        state.agents = [AgentRow("cecelia-99", "idle", "w", "feat/x", session_id="abc")]
        state.add(_row("fanout_audit_finding", "feat/x", "f1", file="a.py", line=3, desc="d",
                       marker="confirmed"))
        state.add(_row("fanout_audit_finding", "gone", "f2", file="b.py", line=4, desc="d",
                       marker="confirmed"))
        # Its session has moved on to feat/x since; the row's `session` still names it.
        state.add({**_row("fanout_audit_finding", "earlier", "f3", file="c.py", line=5, desc="d",
                          marker="confirmed"), "session": "abc"})
        # Logged by a session that has closed: another session now on the branch didn't raise it.
        state.add({**_row("fanout_audit_finding", "feat/x", "f4", file="d.py", line=6, desc="d",
                          marker="confirmed"), "session": "closed"})
        frame = console.render_dashboard(state, pathlib.Path("/x"), width=100, height=60,
                                         use_colour=False).splitlines()
        rows = [line.strip() for line in frame]
        badge = session_badge("abc", use_colour=False)
        self.assertNotIn("recaps", next(line for line in frame if "── agents" in line))
        self.assertIn(f"{badge} 99 x@c1", rows)          # no `session` logged: matched by branch
        self.assertIn(f"{badge} 99 earlier@c1", rows)
        self.assertEqual(rows.count("x@c1"), 1)          # the closed session's finding
        self.assertIn("gone@c1", rows)                   # no live session: no badge

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
