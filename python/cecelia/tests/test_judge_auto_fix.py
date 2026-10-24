"""`judge-review --auto` (`scripts/judge/auto_fix.py`): which bugs it takes, the agent's fence, and
what it records. The sessions and every git / gh / pixi call are faked: nothing reaches GitHub."""
from __future__ import annotations

import importlib.util
import io
import json
import pathlib
import shutil
import subprocess
import unittest
from unittest import mock

from cecelia.effectiveness.console import _ANSI_RE
from cecelia.tests.test_judge_record import _bug, _Fixture

_REPO = pathlib.Path(__file__).resolve().parents[3]


def _load(name):
    spec = importlib.util.spec_from_file_location(name, _REPO / "scripts" / "judge" / f"{name}.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


FIX = {"verdict": "fix", "date": "2026-10-05", "effect": "breaks", "evidence": "a.py:3"}
def _check(name, conclusion="SUCCESS", status="COMPLETED", job=None):
    return {"name": name, "status": status, "conclusion": conclusion,
            **({"detailsUrl": f"https://github.com/o/r/actions/runs/5/job/{job}"} if job else {})}


GREEN = {"statusCheckRollup": [_check("py"), _check("julia")], "mergeable": "MERGEABLE"}
RECITAL = "_Fanout audit: run_\n[fanout-x1] **confirmed** …\n\n_Recital stamp: fix-fanout-b1@aaaaaaaaaaaa_\n"


class _AutoFixture(_Fixture):
    def setUp(self):
        super().setUp()
        self.af = _load("auto_fix")
        self.rv = self.af._rv
        self.log = self.tmp / "review.jsonl"
        self.auto = self.tmp / "auto_fix.jsonl"
        self.main = self.tmp / "ws" / "cecelia-feijoa"
        self.main.mkdir(parents=True)
        self.record = self.build(bugs=[_bug("B1", verify=FIX, issue={"number": 7}), _bug("B2"),
                                       _bug("B3", verify={"verdict": "decide", "date": "2026-10-05"}, file="b.py")])
        self.calls, self.messages, self.bodies = [], [], []
        self.views, self.conflicted, self.unstaged, self.changed = [GREEN], "", "", ""
        self.porcelain, self.head = " M a.py\n", "b" * 40
        self.branches, self.ahead = "", "0\n"   # `git branch --list` (none left over) · its commits
        slept = mock.patch.object(self.af, "_sleep", lambda s: None)
        slept.start()
        self.addCleanup(slept.stop)
        removed = mock.patch.object(self.af.agent_sandbox, "remove_worktree",
                                    lambda repo, dest: self.calls.append(["remove_worktree", str(dest)]))
        removed.start()
        self.addCleanup(removed.stop)

    def sh(self, fail=(), diff="", names="a.py\n"):
        def run(argv, cwd, input=None, **_):
            argv = list(argv)
            if argv[0] == "git":   # drop the `-c` overrides; `self.git_c` keeps what they were
                while argv[1] == "-c":
                    self.git_c.add(argv[2].split("=")[0])
                    del argv[1:3]
            elif argv[1].endswith("recital.py"):
                argv = ["pixi", "run", "recital"] + argv[2:]
                self.recital_input = input
            self.calls.append(argv)
            if argv[:3] == ["git", "diff", "--cached"]:
                if "--name-only" in argv:
                    return subprocess.CompletedProcess(argv, 0, names, "")
                # a list: the staged diff each time it is read, the last one from then on
                out = diff if isinstance(diff, str) else (diff.pop(0) if len(diff) > 1 else diff[0])
                return subprocess.CompletedProcess(argv, 0, out, "")
            if argv[:2] == ["git", "diff"] and "--diff-filter=U" in argv:
                return subprocess.CompletedProcess(argv, 0, self.conflicted, "")
            if argv == ["git", "diff", "--name-only"]:   # what the agent changed during a merge
                return subprocess.CompletedProcess(argv, 0, self.unstaged, "")
            if argv == ["git", "diff", "--name-only", "HEAD"]:   # what a left-behind worktree holds
                return subprocess.CompletedProcess(argv, 0, self.changed, "")
            if argv == ["git", "status", "--porcelain"]:   # the worktree after a follow-up
                return subprocess.CompletedProcess(argv, 0, self.porcelain, "")
            if argv[:3] == ["git", "branch", "--list"]:
                return subprocess.CompletedProcess(argv, 0, self.branches, "")
            if argv[:3] == ["git", "rev-list", "--count"] and "refs/heads/" in argv[3]:
                return subprocess.CompletedProcess(argv, 0, self.ahead, "")
            if argv == ["git", "rev-parse", "HEAD"]:
                return subprocess.CompletedProcess(argv, 0, self.head + "\n", "")
            if argv[:3] == ["gh", "pr", "view"]:   # `self.views`: the PR's states in turn, the last one from then on
                view = self.views[0] if len(self.views) == 1 else self.views.pop(0)
                return subprocess.CompletedProcess(argv, 0, json.dumps(view), "")
            if argv[:2] == ["git", "commit"] and argv[2] == "-F":
                self.messages.append(pathlib.Path(argv[3]).read_text(encoding="utf-8"))
            if argv[:3] == ["gh", "pr", "create"]:
                self.bodies.append(pathlib.Path(argv[argv.index("--body-file") + 1]).read_text(encoding="utf-8"))
            out = {"recital": RECITAL, "create": "https://github.com/o/r/pull/9\n"}.get(argv[2] if len(argv) > 2 else "", "")
            return subprocess.CompletedProcess(argv, 1 if argv[1] in fail else 0, out, "boom" if argv[1] in fail else "")
        self.git_c = set()
        return run

    def session(self, *answers, touch=None):
        it = iter(answers)
        self.prompts = []

        def run(prompt, cwd, schema, *, resume=None, main, **_):
            self.prompts.append((prompt, resume))
            if touch:
                (cwd / touch).parent.mkdir(parents=True, exist_ok=True)
                (cwd / touch).write_text("x", encoding="utf-8")
            a = next(it)
            return (a, 1.5, "sid-1", None) if a is not None else (None, 0.5, None, "session failed (exit 1): x")
        return run

    def work(self, *answers, fail=(), diff="", names="a.py\n", touch=None):
        out = io.StringIO()
        bugs = [self.record["bugs"][0]]
        spent = self.af.work_one(self.record, bugs, main=self.main, out=out, session=self.session(*answers, touch=touch),
                                 sh=self.sh(fail, diff, names), path=self.log, auto_log=self.auto)
        return spent, out.getvalue()


def _answer(outcome="fixed", visual=False):
    return {"bugs": [{"key": "fanout-b1", "outcome": outcome, "reason": "because"}], "title": "fix(a): b",
            "summary": "What broke; fanout-b1 fixed.", "visual": visual}


class ItemsTest(_AutoFixture):
    def test_only_the_bugs_verify_called_fix(self):
        items = self.rv.auto_items(self.record, [])
        self.assertEqual([it["ref"] for it in items], ["B1"])

    def test_a_bug_the_owner_or_an_auto_session_already_took_is_skipped(self):
        self.rv.append_review("bug_work", "2026-10-05", "B1", "fix_session", path=self.log)
        self.assertEqual(self.rv.auto_items(self.record, self.rv.read_reviews(self.log)), [])
        self.assertEqual(self.rv.auto_items(self.record, [], tried={"fanout-b1": {"outcome": "left"}}), [])
        landed = {**self.record, "bugs": [{**self.record["bugs"][0], "fix_landed": [{"commit": "e44eada5"}]},
                                          *self.record["bugs"][1:]]}
        self.assertEqual(self.rv.auto_items(landed, []), [])
        # a failed session logged nothing per bug: tried again
        self.assertEqual(len(self.rv.auto_items(self.record, [], tried={})), 1)


class LandedTest(_AutoFixture):

    def test_a_bug_whose_fix_landed_on_main_gets_no_session(self):
        fix = {"commit": "e44eada5" + "0" * 32, "subject": "fix(viewer): swatch (fanout-b1)", "pr": 12}
        out = io.StringIO()
        with mock.patch.object(self.af, "landed", return_value={"fanout-b1": [fix]}), \
                mock.patch.object(self.af, "work_one") as work:
            ran = self.af.run(self.record, out=out, path=self.log, auto_log=self.auto, main=self.main,
                              sh=lambda *a, **k: subprocess.CompletedProcess(a, 0, "", ""))
        self.assertEqual((ran, work.call_count), (0, 0))
        self.assertIn("0 fix bug(s) in 0 session(s) · 1 already fixed on main", out.getvalue())
        row = self.rv.read_auto(self.auto)["fanout-b1"]
        self.assertEqual((row["outcome"], row["reason"]), ("landed", "e44eada5 fix(viewer): swatch (fanout-b1) (#12)"))
        self.assertEqual(self.rv.auto_items(self.record, [], tried=self.rv.read_auto(self.auto)), [])

    def test_each_session_opens_with_its_bug_card(self):
        out = io.StringIO()
        with mock.patch.object(self.af, "landed", return_value={}), mock.patch.object(self.af, "work_one", return_value=0.0):
            self.af.run(self.record, out=out, path=self.log, auto_log=self.auto, main=self.main,
                        sh=lambda *a, **k: subprocess.CompletedProcess(a, 0, "", ""))
        text = out.getvalue()
        self.assertIn("── B1 fanout-b1 ", text)
        self.assertIn("desc B1", text)   # the card, as the `[f]` queue shows it
        self.assertLess(text.index("desc B1"), text.index("── 1 session(s)"))

    def test_with_no_limit_it_works_every_bundle_and_a_handed_back_one_costs_none(self):
        record = self.build(bugs=[_bug("B1", verify=FIX), _bug("B4", verify=FIX, file="c.py", branch="feat/y"),
                                  _bug("B5", verify=FIX, file="d.py", branch="feat/z")])
        WorkTest.left_behind(self, "cecelia-fix-fanout-b1")
        self.changed = "a.py\n"
        out = io.StringIO()
        answers = [_answer("not_real") | {"bugs": [{"key": k, "outcome": "not_real", "reason": "no"}]}
                   for k in ("fanout-b4", "fanout-b5")]
        with mock.patch.object(self.af, "landed", return_value={}):
            ran = self.af.run(record, out=out, path=self.log, auto_log=self.auto, main=self.main,
                              session=self.session(*answers), sh=self.sh())
        self.assertEqual(ran, 2)
        self.assertIn("running 3", out.getvalue())
        self.assertIn("── 2 session(s) · $3.00 · 1 handed back ─", out.getvalue())

    def test_a_landed_bug_leaves_its_bundle(self):
        by_id = {"B1": {"key": "a"}, "B2": {"key": "b"}, "B3": {"key": "c"}}
        items = [{"kind": "bundle", "ref": "B1", "refs": ["B1", "B2"]}, {"kind": "work", "ref": "B3"}]
        self.assertEqual(self.af._without(items, by_id, {"a"}), [{"kind": "work", "ref": "B2"}, items[1]])
        self.assertEqual(self.af._without(items, by_id, {"c"}), [items[0]])


class OnlyNewestTest(_AutoFixture):
    def test_auto_refuses_an_older_record(self):
        self.rec.write(self.record)
        self.rec.write(self.build(date="2026-10-12", bugs=[_bug("B1", verify=FIX | {"date": "2026-10-12"})]))
        err = io.StringIO()
        with mock.patch("sys.stderr", err):
            self.assertEqual(self.rv.main(["--auto", "--dry-run", "--date", "2026-10-05"]), 1)
        self.assertIn("isn't the newest record (2026-10-12)", err.getvalue())

    def test_the_newest_record_runs(self):
        self.rec.write(self.record)
        seen = {}
        fake = mock.Mock(run=lambda record, **kw: seen.update(date=record["date"], **kw))
        with mock.patch.object(self.rv, "_load_sibling", return_value=fake):
            self.assertEqual(self.rv.main(["--auto", "--dry-run"]), 0)
        self.assertEqual((seen["date"], seen["dry_run"]), ("2026-10-05", True))


class FenceTest(_AutoFixture):
    def test_the_agent_is_sandboxed_without_credentials_and_cant_write_the_main_checkout(self):
        s = self.af.settings(self.main)
        self.assertEqual(s["sandbox"]["network"], {"deniedDomains": ["*"]})
        self.assertFalse(s["sandbox"]["allowUnsandboxedCommands"])
        deny = s["permissions"]["deny"]
        self.assertIn("Read(~/.config/gh/**)", deny)
        self.assertNotIn("Write(~/**)", deny)   # the worktree is under ~; acceptEdits keeps edits in it
        self.assertTrue(any(d.startswith("Edit(") and d.endswith("cecelia-feijoa/**)") for d in deny))
        self.assertIn("Write(~/.ssh/**)", deny)

    def session(self, *events, code=0, stderr=""):
        seen = {}

        class Popen:
            def __init__(self, argv, **kw):
                seen.update(argv=argv, env=kw["env"], cwd=kw["cwd"])
                kw["stderr"].write(stderr)
                self.stdin = io.StringIO()
                self.stdout = io.StringIO("".join(json.dumps(e) + "\n" for e in events) + "not json\n")

            def wait(self):
                seen["prompt"] = self.stdin.getvalue() if not self.stdin.closed else "closed"
                return code

            def kill(self):
                pass
        err = io.StringIO()
        with mock.patch.object(self.af, "resolve_claude_bin", return_value="claude"), \
                mock.patch.object(self.af.subprocess, "Popen", Popen), mock.patch("sys.stdout", err):
            got = self.af.default_session("p", self.tmp, {}, resume="s0", main=self.main)
        return got, seen, err.getvalue()

    def test_the_session_is_fenced_and_never_skips_permissions(self):
        tool = {"type": "assistant", "message": {"content": [
            {"type": "tool_use", "name": "Edit", "input": {"file_path": f"{self.tmp}/app/src/a.jl"}}]}}
        result = {"type": "result", "structured_output": {"tags": []}, "session_id": "s", "total_cost_usd": 0.4}
        (answer, cost, sid, failed), seen, progress = self.session(tool, result)
        self.assertEqual((answer, cost, sid, failed), ({"tags": []}, 0.4, "s", None))
        # not a terminal: no spinner, one line when it's done
        self.assertEqual(progress, "  ✓ agent working · 0.0s · 1 tool call · $0.40\n")
        argv = seen["argv"]
        self.assertNotIn("--dangerously-skip-permissions", argv)
        self.assertEqual(argv[argv.index("--permission-mode") + 1], "acceptEdits")
        self.assertEqual(json.loads(argv[argv.index("--settings") + 1]), self.af.settings(self.main))
        self.assertEqual(argv[argv.index("--resume") + 1], "s0")
        self.assertIn("--strict-mcp-config", argv)
        self.assertEqual(seen["env"]["CECELIA_OBSERVER_NO_PAIR"], "1")
        self.assertEqual(argv[argv.index("--output-format") + 1], "stream-json")

    def test_a_session_cut_off_by_the_usage_limit_waits_and_resumes_itself(self):
        limit = "You've hit your session limit · resets 2:20pm (Australia/Sydney)"
        runs = iter([[{"type": "system", "session_id": "s1"},
                      {"type": "result", "is_error": True, "result": limit, "session_id": "s1", "total_cost_usd": 1.0}],
                     [{"type": "result", "structured_output": {"tags": []}, "session_id": "s1", "total_cost_usd": 0.5}]])
        seen = []

        class Popen:
            def __init__(self, argv, **kw):
                seen.append(argv)
                # the prompt, recorded as written: a recording `close` would run again when GC finalises it
                self.stdin = mock.Mock(write=seen.append)
                self.stdout = io.StringIO("".join(json.dumps(e) + "\n" for e in next(runs)))

            def wait(self):
                return 0

            def kill(self):
                pass
        waited = []
        with mock.patch.object(self.af, "resolve_claude_bin", return_value="claude"), \
                mock.patch.object(self.af.subprocess, "Popen", Popen), mock.patch("sys.stdout", io.StringIO()), \
                mock.patch.object(self.af, "wait_for_reset", lambda m, **kw: waited.append(m) or True):
            got = self.af.default_session("fix it", self.tmp, {}, main=self.main)
        self.assertEqual(got, ({"tags": []}, 1.5, "s1", None))
        self.assertEqual(waited, [limit])
        first, prompt1, second, prompt2 = seen
        self.assertNotIn("--resume", first)
        self.assertEqual(prompt1, "fix it")
        self.assertEqual(second[second.index("--resume") + 1], "s1")   # the same session, carried on
        self.assertEqual(prompt2, self.af.RESUME_PROMPT)

    def _limited_sessions(self, *runs):
        """`default_session` against a fake CLI that plays `runs` (each a list of stream events) in turn;
        a run past the last fails the test instead of hanging it. (result, argvs)."""
        runs, seen = iter(runs), []

        class Popen:
            def __init__(self, argv, **kw):
                seen.append(argv)
                self.stdin = mock.Mock()
                self.stdout = io.StringIO("".join(json.dumps(e) + "\n" for e in next(runs)))

            def wait(self):
                return 0

            def kill(self):
                pass
        with mock.patch.object(self.af, "resolve_claude_bin", return_value="claude"), \
                mock.patch.object(self.af.subprocess, "Popen", Popen), mock.patch("sys.stdout", io.StringIO()):
            return self.af.default_session("fix it", self.tmp, {}, main=self.main), seen

    def test_an_unreadable_reset_stops_after_max_limit_waits_instead_of_waiting_for_days(self):
        # a weekly limit this code can't read: an hour's wait each time, so the old loop never stopped
        limit = "You've hit your weekly limit · resets in a while"
        run = [{"type": "system", "session_id": "s1"},
               {"type": "result", "is_error": True, "api_error_status": 429, "result": limit, "session_id": "s1"}]
        slept = []
        with mock.patch.object(self.af, "_sleep", slept.append), self.assertRaises(self.af.RateLimited) as e:
            self._limited_sessions(*[run] * (self.af.MAX_LIMIT_WAITS + 1))
        self.assertEqual(str(e.exception), limit)
        self.assertEqual(len(slept), self.af.MAX_LIMIT_WAITS)

    def test_the_streams_exact_reset_stops_a_weekly_limit_at_once(self):
        import datetime as dt
        # the CLI's text names no readable time; its refusal event says the limit lifts in 3 days
        lifts = int((dt.datetime.now(dt.timezone.utc) + dt.timedelta(days=3)).timestamp())
        run = [{"type": "system", "session_id": "s1"},
               {"type": "rate_limit_event", "rate_limit_info": {"status": "rejected", "resetsAt": lifts,
                                                                "rateLimitType": "seven_day"}},
               {"type": "result", "is_error": True, "api_error_status": 429, "result": "You've hit your weekly limit",
                "session_id": "s1"}]
        with mock.patch.object(self.af, "_sleep", side_effect=AssertionError("waited")), \
                self.assertRaises(self.af.RateLimited) as e:
            self._limited_sessions(run)
        self.assertEqual(str(e.exception), f"You've hit your weekly limit|{lifts}")
        # a refusal an hour off is waited out, to its epoch, and the session resumed
        soon = int((dt.datetime.now(dt.timezone.utc) + dt.timedelta(minutes=30)).timestamp())
        run[1] = {"type": "rate_limit_event", "rate_limit_info": {"status": "rejected", "resetsAt": soon}}
        ok = [{"type": "result", "structured_output": {"tags": []}, "session_id": "s1"}]
        slept = []
        with mock.patch.object(self.af, "_sleep", slept.append):
            got, seen = self._limited_sessions(run, ok)
        self.assertEqual(got[0], {"tags": []})
        self.assertEqual(len(slept), 1)
        self.assertAlmostEqual(slept[0], 31 * 60, delta=5)   # a minute past the stream's reset, not the 1h fallback
        self.assertIn("--resume", seen[1])

    def test_waiting_out_the_limit(self):
        import datetime as dt
        zone = __import__("zoneinfo").ZoneInfo("Australia/Sydney")
        now = dt.datetime(2026, 10, 14, 13, 0, tzinfo=zone)
        slept = []
        with mock.patch.object(self.af, "_sleep", slept.append), mock.patch("sys.stdout", io.StringIO()) as out:
            self.assertTrue(self.af.wait_for_reset("limit · resets 2:20pm (Australia/Sydney)", now=now))
            self.assertFalse(self.af.wait_for_reset("limit · resets 11pm (Australia/Sydney)", now=now))   # 10h: stop
        self.assertEqual(slept, [81 * 60])   # one wait: to a minute past the reset
        self.assertIn("usage limit · resumes 14:21", out.getvalue())

    def test_a_session_without_a_result_fails(self):
        (answer, _, _, failed), _, _ = self.session({"type": "system"}, code=1)
        self.assertIsNone(answer)
        self.assertIn("exit 1", failed)
        # another error on stderr, or the agent's own prose about a rate limit: still an ordinary failure
        prose = {"type": "assistant", "message": {"model": "claude-opus-5-5", "content": [
            {"type": "text", "text": "You've hit your session limit, says the API."}]}}
        with mock.patch.object(self.af, "wait_for_reset", side_effect=AssertionError("waited")):
            (answer, _, _, failed), _, _ = self.session(prose, code=1, stderr="Prompt is too long\n")
        self.assertIsNone(answer)
        self.assertIn("exit 1", failed)

    def test_a_session_the_cli_aborts_on_the_limit_without_a_result_is_limited(self):
        limit = "You've hit your session limit · resets 12:10am (Australia/Sydney)"
        synthetic = {"type": "assistant", "session_id": "s1", "message": {
            "model": "<synthetic>", "content": [{"type": "text", "text": limit}]}}
        for events, stderr in ((({"type": "system", "session_id": "s1"},), f"{limit}\n"),
                               (({"type": "system", "session_id": "s1"}, synthetic), "")):
            waited = []
            with mock.patch.object(self.af, "wait_for_reset", lambda m, **kw: waited.append(m) or False):
                with self.assertRaises(self.af.RateLimited) as e:
                    self.session(*events, code=1, stderr=stderr)
            self.assertEqual(waited, [limit])   # waited on, not logged as a failure
            self.assertEqual(str(e.exception), limit)

    def test_the_progress_line(self):
        tty = mock.Mock(isatty=lambda: True)
        p = self.af.Progress("agent fixing", out=tty, cwd=pathlib.Path("/w"))
        plain = lambda line: _ANSI_RE.sub("", line)   # noqa: E731
        self.assertRegex(plain(p.line(0)), r"^  ⠋ agent fixing · 0\.\ds$")
        p.event({"message": {"content": [{"type": "tool_use", "name": "Bash", "input": {"command": "pixi run test-py\nx"}}]}})
        self.assertTrue(plain(p.line(1)).endswith("· 1 tool call · Bash pixi run test-py"))
        self.assertIn("\x1b[", p.line(1))   # a terminal gets colour
        self.assertEqual(tty.write.call_count, 0)   # nothing until entered

    def test_a_tool_call_drops_the_worktree_path(self):
        w = pathlib.Path("/h/cecelia-fix-x")
        self.assertEqual(self.af.describe_call("Bash", {"command": f"cd {w}; grep -n a {w}/b.py"}, w), "Bash grep -n a b.py")
        self.assertEqual(self.af.describe_call("Bash", {"command": f"cd {w}/frontend && npx vitest"}, w),
                         "Bash cd frontend && npx vitest")
        self.assertEqual(self.af.describe_call("Read", {"file_path": f"{w}/app/a.jl"}, w), "Read app/a.jl")

    def test_outcome_rows_wrap_under_the_reason_and_stop_at_two_lines(self):
        long = "Real: " + "word " * 60 + ". A second sentence that never shows."
        rows = self.af.outcome_rows([("fanout-1", "left", long), ("conv-22", "not_real", "Already fixed.")],
                                    use_colour=False)
        self.assertEqual(rows[0][:30], "    left      fanout-1  Real: ")
        self.assertTrue(rows[1].startswith(" " * 24) and rows[1].endswith(" …"))
        self.assertEqual(rows[2], "    not real  conv-22   Already fixed.")

    def test_the_agent_env_holds_no_token(self):
        with mock.patch.dict(self.af.os.environ, {"GH_TOKEN": "t", "GITHUB_TOKEN": "t", "DBUS_SESSION_BUS_ADDRESS": "b"}):
            env = self.af.agent_sandbox.agent_env()
        self.assertFalse({"GH_TOKEN", "GITHUB_TOKEN", "DBUS_SESSION_BUS_ADDRESS"} & env.keys())

    def test_protected_paths(self):
        for p in (".github/workflows/ci.yml", ".githooks/commit-msg", ".claude/settings.json", "pixi.toml",
                  "app/CLAUDE.md", "scripts/judge/auto_fix.py"):
            self.assertTrue(self.af.protected(p), p)
        for p in ("scripts/judge/review.py", "app/src/a.jl", "app/CLAUDE.md.bak"):
            self.assertFalse(self.af.protected(p), p)


class WorkTest(_AutoFixture):
    def test_a_fix_is_committed_with_its_tags_pushed_and_opened_as_a_pr(self):
        spent, out = self.work(_answer(visual=True), {"tags": ["[fanout-x1: fixed_pre_commit]"]})
        self.assertEqual(spent, 3.0)
        firsts = [c[:3] for c in self.calls]
        self.assertEqual(firsts[:2], [["git", "branch", "--list"], ["pixi", "run", "bootstrap-worktree"]])
        self.assertIn(["pixi", "run", "recital"], firsts)
        self.assertEqual(self.prompts[1][1], "sid-1")   # tagging resumes the fix session
        self.assertIn(RECITAL.strip(), self.prompts[1][0])
        msg = self.messages[0]
        for part in ("fix(a): b", "fanout-b1", "Refs #7", "[fanout-x1: fixed_pre_commit]", "_Recital stamp:",
                     "Co-Authored-By:"):
            self.assertIn(part, msg)
        self.assertNotIn("Fixes #7", msg)
        create = next(c for c in self.calls if c[:3] == ["gh", "pr", "create"])
        self.assertNotIn("--draft", create)   # a normal PR; the body says to look first
        body = next(m for m in self.bodies)
        self.assertIn("Changes what is drawn on screen", body)
        self.assertFalse(any(c[:3] == ["gh", "pr", "merge"] for c in self.calls))
        # git never runs the worktree's hooks or fsmonitor; the recital reads the diff we staged
        self.assertEqual(self.git_c, {"core.fsmonitor", "core.hooksPath"})
        self.assertEqual(self.recital_input, "")
        rows = self.rv.read_reviews(self.log)
        self.assertEqual([(r["ref"], r["value"], r["note"]) for r in rows],
                         [("B1", "fix_session", "auto PR https://github.com/o/r/pull/9")])
        self.assertEqual(self.rv.read_auto(self.auto)["fanout-b1"]["outcome"], "fixed")
        self.assertIn("→ PR https://github.com/o/r/pull/9 · changes what is drawn: look first", out)

    def test_not_real_commits_nothing_removes_the_worktree_and_shows_on_the_card(self):
        _, out = self.work(_answer("not_real"))
        self.assertIn("not real  fanout-b1  because", out)
        self.assertIn("nothing fixed: no PR", out)
        self.assertFalse(any(c[:2] in (["git", "commit"], ["git", "push"]) for c in self.calls))
        self.assertIn(["remove_worktree", str(self.main.parent / "cecelia-fix-fanout-b1")], self.calls)
        self.assertEqual(self.rv.read_reviews(self.log), [])   # stays in the owner's queue
        self.assertEqual(self.rv.auto_path(), self.auto)   # the card reads the default log
        card = "\n".join(self.rv.describe(self.record, {"kind": "work", "ref": "B1"}, use_colour=False))
        self.assertIn("not real: because", card)

    def test_recital_runs_again_while_acting_on_it_changes_the_diff(self):
        tags = lambda t: {"tags": [t]}   # noqa: E731
        _, out = self.work(_answer(), tags("[fanout-x1: fixed_pre_commit]"), tags("[fanout-x2: fixed_pre_commit]"),
                           tags("[fanout-x1: fixed_pre_commit]"), diff=["d1", "d2", "d3", "d3"])
        recitals = [c for c in self.calls if c[:3] == ["pixi", "run", "recital"]]
        self.assertEqual(len(recitals), 3)   # d1, then d2 and d3 after the agent changed it; d3 again: done
        self.assertIn("✓ recital · round 3", out)
        self.assertIn("round 2, after your changes", self.prompts[2][0])
        msg = self.messages[0]
        self.assertEqual(msg.count("[fanout-x1: fixed_pre_commit]"), 1)   # every round's tags, once
        self.assertIn("[fanout-x2: fixed_pre_commit]", msg)

    def test_a_diff_that_never_settles_goes_back_to_the_owner(self):
        rounds = self.af.RECITAL_ROUNDS
        _, out = self.work(_answer(), *[{"tags": []}] * rounds, diff=[f"d{i}" for i in range(rounds + 2)])
        self.assertFalse(any(c[:2] == ["git", "commit"] for c in self.calls))
        self.assertIn(f"its diff still changed after {rounds} recital rounds", out)
        self.assertIn("back in your judge-review queue", out)
        shutil.rmtree(self.main.parent / "cecelia-fix-fanout-b1", ignore_errors=True)

    def limited(self, *answers):
        """`session` that answers `answers`, then hits the usage limit."""
        inner = self.session(*answers)
        it = iter(answers)

        def run(prompt, cwd, schema, **kw):
            if next(it, None) is None:
                raise self.af.RateLimited("You've hit your limit · resets 1:40am")
            return inner(prompt, cwd, schema, **kw)
        return run

    def test_a_usage_limit_during_the_fix_starts_the_bundle_afresh_next_run(self):
        out = io.StringIO()
        with self.assertRaises(self.af.RateLimited):
            self.af.work_one(self.record, [self.record["bugs"][0]], main=self.main, out=out,
                             session=self.limited(), sh=self.sh(), path=self.log, auto_log=self.auto)
        self.assertIn(["remove_worktree", str(self.main.parent / "cecelia-fix-fanout-b1")], self.calls)
        self.assertEqual(self.rv.read_auto(self.auto), {})   # no attempt counted
        self.assertEqual(len(self.rv.auto_items(self.record, [], tried={})), 1)
        self.assertIn("usage limit during the fix", out.getvalue())

    def test_a_usage_limit_after_the_fix_keeps_it_for_f(self):
        tree = self.main.parent / "cecelia-fix-fanout-b1"
        out = io.StringIO()
        session = self.limited(_answer())

        def touched(prompt, cwd, schema, **kw):
            (cwd / "a.py").parent.mkdir(parents=True, exist_ok=True)
            (cwd / "a.py").write_text("x", encoding="utf-8")
            return session(prompt, cwd, schema, **kw)
        with self.assertRaises(self.af.RateLimited):
            self.af.work_one(self.record, [self.record["bugs"][0]], main=self.main, out=out,
                             session=touched, sh=self.sh(), path=self.log, auto_log=self.auto)
        row = self.rv.read_auto(self.auto)["fanout-b1"]
        self.assertEqual((row["outcome"], row["tree"]), ("left", str(tree)))
        self.assertIn("back in your judge-review queue", out.getvalue())
        self.assertFalse(any(c[:2] == ["git", "commit"] for c in self.calls))
        shutil.rmtree(tree)

    def test_a_recital_whose_reviewers_errored_commits_nothing(self):
        errored = ("_Fanout audit (evidence):_\n\n> **RECITAL SCRIPT ERROR** — claude exited 1; stdout:\n"
                   "{}\n\n_Recital stamp: fix-fanout-b1@aaaaaaaaaaaa_\n")
        tree = self.main.parent / "cecelia-fix-fanout-b1"
        for text, raises in (("boom", False), ("You've hit your session limit · resets 2:20pm", True)):
            with self.subTest(raises=raises), mock.patch(f"{__name__}.RECITAL", errored.format(text)), \
                    mock.patch.object(self.af, "wait_for_reset", return_value=False):   # a weekly limit: no wait
                self.calls.clear()
                out = io.StringIO()
                run = lambda: self.af.work_one(self.record, [self.record["bugs"][0]], main=self.main, out=out,   # noqa: E731
                                               session=self.session(_answer()), sh=self.sh(), path=self.log,
                                               auto_log=self.auto)
                if raises:
                    self.assertRaises(self.af.RateLimited, run)
                else:
                    run()
                self.assertFalse(any(c[:2] == ["git", "commit"] for c in self.calls))
                self.assertEqual(len(self.prompts), 1)   # the agent never acted on it
                self.assertIn("recital's reviewers errored" + (" on the usage limit" if raises else ""), out.getvalue())
                self.assertEqual(self.rv.read_auto(self.auto)["fanout-b1"]["tree"], str(tree))
                shutil.rmtree(tree, ignore_errors=True)

    def test_a_recital_cut_off_by_the_limit_waits_and_runs_again(self):
        limited = ("_Fanout audit (evidence):_\n\n> **RECITAL SCRIPT ERROR** — claude exited 1; stdout:\n"
                   "You've hit your session limit · resets 2:20pm\n\n_Recital stamp: fix-fanout-b1@aaaaaaaaaaaa_\n")
        bodies = iter([limited, RECITAL])
        inner = self.sh()

        def sh(argv, cwd, **kw):
            done = inner(argv, cwd, **kw)
            if len(argv) > 1 and str(argv[1]).endswith("recital.py"):
                done = subprocess.CompletedProcess(argv, 0, next(bodies), "")
            return done
        out = io.StringIO()
        with mock.patch.object(self.af, "wait_for_reset", return_value=True) as wait:
            self.af.work_one(self.record, [self.record["bugs"][0]], main=self.main, out=out,
                             session=self.session(_answer(), {"tags": []}), sh=sh, path=self.log, auto_log=self.auto)
        self.assertEqual(wait.call_count, 1)
        self.assertEqual(len([c for c in self.calls if c[:3] == ["pixi", "run", "recital"]]), 2)
        self.assertTrue(any(c[:2] == ["git", "commit"] for c in self.calls))
        self.assertIn("→ PR https://github.com/o/r/pull/9", out.getvalue())

    def test_a_recital_the_limit_keeps_cutting_off_stops_after_max_limit_waits(self):
        limited = ("_Fanout audit (evidence):_\n\n> **RECITAL SCRIPT ERROR** — claude exited 1; stdout:\n"
                   "You've hit your weekly limit\n\n_Recital stamp: fix-fanout-b1@aaaaaaaaaaaa_\n")
        inner, runs = self.sh(), []

        def sh(argv, cwd, **kw):
            done = inner(argv, cwd, **kw)
            if len(argv) > 1 and str(argv[1]).endswith("recital.py"):
                runs.append(argv)
                if len(runs) > self.af.MAX_LIMIT_WAITS + 1:
                    raise AssertionError("recital run again past MAX_LIMIT_WAITS")
                done = subprocess.CompletedProcess(argv, 0, limited, "")
            return done
        with mock.patch.object(self.af, "wait_for_reset", return_value=True) as wait, \
                self.assertRaises(self.af.RateLimited):
            self.af.work_one(self.record, [self.record["bugs"][0]], main=self.main, out=io.StringIO(),
                             session=self.session(_answer(), {"tags": []}), sh=sh, path=self.log, auto_log=self.auto)
        self.assertEqual(wait.call_count, self.af.MAX_LIMIT_WAITS)
        self.assertEqual(len(runs), self.af.MAX_LIMIT_WAITS + 1)

    RED = {"statusCheckRollup": [_check("py", "FAILURE", job=7), _check("julia")], "mergeable": "MERGEABLE"}

    def test_red_ci_goes_back_to_the_agent_with_its_log_and_the_fix_is_shipped(self):
        self.views = [self.RED, GREEN]
        _, out = self.work(_answer(), {"tags": []},
                           {"outcome": "fixed", "title": "fix(test): seed the fixture", "summary": "The fixture needed a seed."},
                           {"tags": ["[fanout-x9: fixed_pre_commit]"]})
        self.assertIn(["gh", "api", "repos/{owner}/{repo}/actions/jobs/7/logs"], self.calls)
        self.assertIn("CI failed: py", self.prompts[2][0])
        self.assertEqual(self.prompts[2][1], "sid-1")   # the same session
        self.assertEqual(len(self.messages), 2)   # the fix, then the CI fix, each through recital
        self.assertTrue(self.messages[1].startswith("fix(test): seed the fixture\n"))   # the agent's subject
        self.assertIn("[fanout-x9: fixed_pre_commit]", self.messages[1])
        self.assertIn("green and mergeable: https://github.com/o/r/pull/9", out)
        self.assertEqual([r["value"] for r in self.rv.read_reviews(self.log)], ["fix_session"])

    def test_the_log_excerpt_is_what_led_to_the_error(self):
        log = "\n".join([f"2026-10-13T22:44:{i:02d}.0Z line {i}" for i in range(300)]
                        + ["2026-10-13T22:46:58.8Z FAILED (failures=1)", "2026-10-13T22:47:00.6Z ##[error]Process completed",
                           "2026-10-13T22:47:02.5Z Cleaning up orphan processes"] + ["x"] * 50)
        got = self.af.log_excerpt(log, lines=5).splitlines()
        self.assertEqual(got[:5], ["line 296", "line 297", "line 298", "line 299", "FAILED (failures=1)"])
        self.assertIn("##[error]Process completed", got)
        self.assertEqual(self.af.log_excerpt("a\nb\nc", lines=2), "b\nc")   # no error line: the tail

    def test_the_checks_of_a_commit_not_yet_on_the_pr_are_not_read(self):
        # just after a push GitHub still shows the last head, and its failed checks
        stale = {**self.RED, "headRefOid": "a" * 40}
        self.views = [stale, stale, {**GREEN, "headRefOid": self.head}]
        _, out = self.work(_answer(), {"tags": []})
        self.assertEqual(len(self.prompts), 2)   # no CI round: the red belonged to another commit
        self.assertIn("green and mergeable", out)

    def test_a_ci_follow_up_that_changes_nothing_isnt_shipped(self):
        self.views, self.porcelain = [self.RED, GREEN], ""
        _, out = self.work(_answer(), {"tags": []}, {"outcome": "fixed", "title": "", "summary": "Already fixed."})
        self.assertEqual(len(self.messages), 1)
        self.assertEqual(len([c for c in self.calls if c[:3] == ["pixi", "run", "recital"]]), 1)
        self.assertIn("the agent changed nothing: Already fixed.", out)
        self.assertIn("green and mergeable", out)

    def test_an_unrelated_failure_is_re_run_once_not_changed(self):
        self.views = [self.RED, GREEN]
        _, out = self.work(_answer(), {"tags": []}, {"outcome": "unrelated", "title": "", "summary": "A runner timeout."})
        self.assertIn(["gh", "run", "rerun", "5", "--failed"], self.calls)
        self.assertEqual(len(self.messages), 1)
        self.assertIn("green and mergeable", out)

    def test_a_conflict_with_main_is_merged_and_resolved_by_the_agent(self):
        self.views, self.conflicted = [{"statusCheckRollup": [], "mergeable": "CONFLICTING"}, GREEN], "a.py\n"
        _, out = self.work(_answer(), {"tags": []}, {"outcome": "fixed", "title": "", "summary": "Kept both imports."},
                           fail=("merge",), touch="a.py")
        self.assertIn(["git", "merge", "--no-edit", "origin/main"], self.calls)
        self.assertIn("`a.py`", self.prompts[2][0])   # the conflicted files
        self.assertIn(["git", "commit", "--no-edit"], self.calls)   # the merge commit
        self.assertIn("green and mergeable", out)
        shutil.rmtree(self.main.parent / "cecelia-fix-fanout-b1")

    def test_a_resolution_that_strays_beyond_the_conflicted_files_is_not_committed(self):
        # a merge commit skips recital: the agent may touch only what conflicted
        self.views, self.conflicted = [{"statusCheckRollup": [], "mergeable": "CONFLICTING"}], "a.py\n"
        self.unstaged = "a.py\nb.py\n"
        _, out = self.work(_answer(), {"tags": []}, {"outcome": "fixed", "title": "", "summary": "Resolved."},
                           fail=("merge",), touch="a.py")
        self.assertNotIn(["git", "commit", "--no-edit"], self.calls)
        self.assertIn(["git", "merge", "--abort"], self.calls)
        self.assertIn("it touched b.py", out)
        row = self.rv.read_auto(self.auto)["fanout-b1"]
        self.assertEqual((row["stage"], row["pr_url"]), ("pr", "https://github.com/o/r/pull/9"))   # any stop after the PR
        with mock.patch.object(self.rv, "auto_path", return_value=self.auto):
            brief = self.rv.fix_brief(self.record, self.record["bugs"][0])
        self.assertIn("Its PR: https://github.com/o/r/pull/9", brief)
        self.assertTrue(row["reason"].startswith("Stopped at its PR https://github.com/o/r/pull/9: resolving"))
        shutil.rmtree(self.main.parent / "cecelia-fix-fanout-b1")

    def test_a_weekly_limit_while_fixing_ci_hands_the_pr_back(self):
        self.views = [self.RED]
        tree = self.main.parent / "cecelia-fix-fanout-b1"
        session = self.limited(_answer(), {"tags": []})

        def touched(prompt, cwd, schema, **kw):
            (cwd / "a.py").parent.mkdir(parents=True, exist_ok=True)
            (cwd / "a.py").write_text("x", encoding="utf-8")
            return session(prompt, cwd, schema, **kw)
        out = io.StringIO()
        with self.assertRaises(self.af.RateLimited):
            self.af.work_one(self.record, [self.record["bugs"][0]], main=self.main, out=out, session=touched,
                             sh=self.sh(), path=self.log, auto_log=self.auto)
        row = self.rv.read_auto(self.auto)["fanout-b1"]
        self.assertEqual((row["outcome"], row["stage"], row["tree"]), ("left", "pr", str(tree)))
        self.assertIn("usage limit while fixing CI", out.getvalue())
        shutil.rmtree(tree)

    def test_still_red_after_its_rounds_goes_back_to_the_owner_with_the_pr(self):
        self.views = [self.RED]
        fixed = {"outcome": "fixed", "title": "fix: again", "summary": "Tried again."}
        _, out = self.work(_answer(), {"tags": []}, *[fixed, {"tags": []}] * self.af.CI_ROUNDS, touch="a.py")
        self.assertIn(f"CI still fails after {self.af.CI_ROUNDS} rounds of fixes: https://github.com/o/r/pull/9", out)
        row = self.rv.read_auto(self.auto)["fanout-b1"]
        self.assertEqual((row["outcome"], row["stage"]), ("left", "pr"))
        self.assertEqual(self.rv.read_reviews(self.log), [])   # not answered: back in the queue
        shutil.rmtree(self.main.parent / "cecelia-fix-fanout-b1")

    def test_a_failed_push_briefs_f_on_the_commit_not_a_staged_diff(self):
        tree = self.main.parent / "cecelia-fix-fanout-b1"
        self.work(_answer(), {"tags": []}, fail=("push",), touch="a.py")
        self.assertEqual(self.rv.read_auto(self.auto)["fanout-b1"]["stage"], "committed")
        with mock.patch.object(self.rv, "auto_path", return_value=self.auto):
            brief = self.rv.fix_brief(self.record, self.record["bugs"][0])
        self.assertIn("committed its work there but the push failed", brief)
        self.assertEqual(set(self.rv._STAGED), set(self.af.STAGES))   # every stage has its brief
        shutil.rmtree(tree)

    def test_a_blocked_commit_leaves_the_bug_for_the_owner(self):
        _, out = self.work(_answer(), {"tags": []}, fail=("commit",))
        self.assertEqual(self.rv.read_reviews(self.log), [])
        self.assertEqual(self.rv.read_auto(self.auto)["fanout-b1"]["outcome"], "left")
        self.assertFalse(any(c[:2] == ["git", "push"] for c in self.calls))
        self.assertIn("back in your judge-review queue: B1 fanout-b1", out)

    def test_a_credential_in_the_diff_is_never_committed(self):
        token = "+x = 'ghp_" + "a" * 36 + "'\n"
        _, out = self.work(_answer(), {"tags": []}, diff=token)
        self.assertFalse(any(c[:2] in (["git", "commit"], ["git", "push"]) for c in self.calls))
        self.assertEqual(self.rv.read_auto(self.auto)["fanout-b1"]["outcome"], "left")
        self.assertIn("looks like a credential", out)

    def test_a_failed_session_is_tried_again_up_to_the_cap(self):
        self.work(None)
        row = self.rv.read_auto(self.auto)["fanout-b1"]
        self.assertEqual((row["outcome"], row["attempt"]), ("failed", 1))
        self.assertIn(["remove_worktree", str(self.main.parent / "cecelia-fix-fanout-b1")], self.calls)
        self.assertEqual(self.rv.read_reviews(self.log), [])
        self.assertEqual(len(self.rv.auto_items(self.record, [], tried=self.rv.read_auto(self.auto))), 1)
        self.work(None)
        self.assertEqual(self.rv.read_auto(self.auto)["fanout-b1"]["attempt"], 2)
        self.assertEqual(self.rv.auto_items(self.record, [], tried=self.rv.read_auto(self.auto)), [])

    def test_an_agent_that_writes_what_runs_outside_its_sandbox_stops_before_it_runs(self):
        for touch in (".githooks/commit-msg", ".claude/settings.local.json", ".git"):
            with self.subTest(touch=touch):
                self.calls.clear()
                self.auto.unlink(missing_ok=True)
                tree = self.main.parent / "cecelia-fix-fanout-b1"
                _, out = self.work(_answer(), touch=touch)
                # not even `git add`: past the check for a branch left over, before the worktree
                self.assertEqual([c for c in self.calls if c[0] != "pixi" and c[1:3] != ["branch", "--list"]], [])
                self.assertIn("run outside its sandbox", out)
                self.assertIn("[f] there starts fresh", out)
                row = self.rv.read_auto(self.auto)["fanout-b1"]
                self.assertEqual((row["outcome"], row.get("tree")), ("left", None))   # never worked in again
                shutil.rmtree(tree)

    def test_a_diff_into_ci_or_the_fence_is_never_pushed(self):
        tree = self.main.parent / "cecelia-fix-fanout-b1"
        _, out = self.work(_answer(), names="a.py\n.github/workflows/ci.yml\n", touch="a.py")
        self.assertFalse(any(c[:2] in (["git", "commit"], ["git", "push"]) for c in self.calls))
        self.assertFalse(any(c[:3] == ["pixi", "run", "recital"] for c in self.calls))
        self.assertIn(".github/workflows/ci.yml", out)
        # back in the owner's queue, and [f] there picks the work up in the worktree
        self.assertIn("back in your judge-review queue: B1 fanout-b1 · [f] there opens a session on the work in "
                      "cecelia-fix-fanout-b1", out)
        row = self.rv.read_auto(self.auto)["fanout-b1"]
        self.assertEqual((row["outcome"], row["tree"]), ("left", str(tree)))
        self.assertTrue(row["reason"].startswith("Stopped before the PR: the diff touches .github/workflows/ci.yml"))
        bug = self.record["bugs"][0]
        with mock.patch.object(self.rv, "auto_path", return_value=self.auto):
            self.assertEqual(self.rv.auto_tree(bug), tree)
            brief = self.rv.fix_brief(self.record, bug)
            card = "\n".join(self.rv.describe(self.record, {"kind": "work", "ref": "B1"}, use_colour=False))
        self.assertIn(f"already worked this in `{tree}`", brief)
        self.assertNotIn("bootstrap-worktree", brief)
        self.assertIn("because. Its work is in `cecelia-fix-fanout-b1`: [f] picks it up there", " ".join(card.split()))
        shutil.rmtree(tree)
        with mock.patch.object(self.rv, "auto_path", return_value=self.auto):
            self.assertIsNone(self.rv.auto_tree(bug))   # pruned: [f] starts fresh again

    def left_behind(self, name="cecelia-fix-fanout-b1", pointer=True):
        """A worktree an earlier run left with no record, its `.git` pointer this repo's."""
        tree = self.main.parent / name
        gitdir = self.main / ".git" / "worktrees" / name
        tree.mkdir()
        gitdir.mkdir(parents=True)
        (gitdir / "gitdir").write_text(str(tree / ".git") + "\n", encoding="utf-8")
        (tree / ".git").write_text(f"gitdir: {gitdir if pointer else '/elsewhere'}\n", encoding="utf-8")
        return tree

    def test_a_worktree_left_without_a_record_goes_back_to_the_queue(self):
        tree = self.left_behind()
        self.changed = "a.py\n"
        spent, out = self.work(_answer())
        self.assertIsNone(spent)   # not a session
        self.assertEqual(self.prompts, [])
        row = self.rv.read_auto(self.auto)["fanout-b1"]
        self.assertEqual((row["outcome"], row["tree"], row["stage"]), ("left", str(tree), "staged"))
        self.assertIn("back in your judge-review queue: B1 fanout-b1 · [f] there opens a session on the work", out)

    def test_one_whose_pointer_isnt_ours_or_that_touches_the_fence_is_not_worked_in(self):
        for pointer, changed in ((False, "a.py\n"), (True, ".githooks/commit-msg\n")):
            with self.subTest(pointer=pointer):
                self.auto.unlink(missing_ok=True)
                tree = self.left_behind(pointer=pointer)
                self.changed = changed
                _, out = self.work(_answer())
                self.assertIsNone(self.rv.read_auto(self.auto)["fanout-b1"].get("tree"))
                self.assertIn("[f] there starts fresh", out)
                shutil.rmtree(tree)
                shutil.rmtree(self.main / ".git")

    def test_an_empty_one_is_removed_and_the_bundle_runs(self):
        tree = self.left_behind()
        self.work(_answer("not_real"))
        self.assertIn(["remove_worktree", str(tree)], self.calls)
        self.assertEqual(len(self.prompts), 1)   # the bundle ran

    def test_a_branch_left_without_its_worktree_and_no_commits_is_deleted_and_the_bundle_runs(self):
        self.branches = "  fix-fanout-b1\n"
        self.work(_answer("not_real"))
        firsts = [c[:3] for c in self.calls]
        bootstrap = firsts.index(["pixi", "run", "bootstrap-worktree"])
        self.assertLess(firsts.index(["git", "worktree", "prune"]), bootstrap)
        self.assertLess(self.calls.index(["git", "branch", "-D", "fix-fanout-b1"]), bootstrap)
        self.assertEqual(len(self.prompts), 1)   # the bundle ran

    def test_a_branch_left_with_commits_of_its_own_is_kept_and_goes_back_to_the_queue(self):
        self.branches, self.ahead = "  fix-fanout-b1\n", "2\n"
        spent, out = self.work(_answer())
        self.assertIsNone(spent)
        self.assertEqual(self.prompts, [])
        self.assertNotIn(["git", "branch", "-D", "fix-fanout-b1"], self.calls)
        self.assertFalse(any(c[:3] == ["pixi", "run", "bootstrap-worktree"] for c in self.calls))
        row = self.rv.read_auto(self.auto)["fanout-b1"]
        self.assertEqual(row["outcome"], "left")
        self.assertIn("branch fix-fanout-b1", row["reason"])
        self.assertIn("2 commits of its own", out)

    def test_a_failure_shows_gits_reason_not_only_the_stack_trace_after_it(self):
        err = ("fatal: a branch named 'fix-fanout-b1' already exists\nERROR: LoadError: failed process: "
               + "Process(`git worktree add`, ProcessExited(128))\n" + "Stacktrace:\n [1] pipeline_error\n" * 40)
        tail = self.af._tail(subprocess.CompletedProcess([], 1, "→ fetching origin\n", err))
        self.assertTrue(tail.startswith("fatal: a branch named 'fix-fanout-b1' already exists"))
        self.assertLess(len(tail), 600)
        short = subprocess.CompletedProcess([], 1, "", "error: boom")
        self.assertEqual(self.af._tail(short), "error: boom")   # already in the tail: not said twice


if __name__ == "__main__":
    unittest.main()
