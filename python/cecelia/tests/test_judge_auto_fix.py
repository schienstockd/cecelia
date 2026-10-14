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
                return subprocess.CompletedProcess(argv, 0, names if "--name-only" in argv else diff, "")
            if argv[:2] == ["git", "commit"]:
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

    def session(self, *events, code=0):
        seen = {}

        class Popen:
            def __init__(self, argv, **kw):
                seen.update(argv=argv, env=kw["env"], cwd=kw["cwd"])
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

    def test_a_session_without_a_result_fails(self):
        (answer, _, _, failed), _, _ = self.session({"type": "system"}, code=1)
        self.assertIsNone(answer)
        self.assertIn("exit 1", failed)

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
        self.assertEqual(firsts[0], ["pixi", "run", "bootstrap-worktree"])
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

    def test_a_blocked_commit_leaves_the_bug_for_the_owner(self):
        _, out = self.work(_answer(), {"tags": []}, fail=("commit",))
        self.assertEqual(self.rv.read_reviews(self.log), [])
        self.assertEqual(self.rv.read_auto(self.auto)["fanout-b1"]["outcome"], "left")
        self.assertFalse(any(c[:2] == ["git", "push"] for c in self.calls))
        self.assertIn("worktree left at", out)

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
                self.assertEqual([c for c in self.calls if c[0] != "pixi"], [])   # not even `git add`
                self.assertIn("run outside its sandbox", out)
                self.assertEqual(self.rv.read_auto(self.auto)["fanout-b1"]["outcome"], "left")
                shutil.rmtree(tree)

    def test_a_diff_into_ci_or_the_fence_is_never_pushed(self):
        _, out = self.work(_answer(), names="a.py\n.github/workflows/ci.yml\n")
        self.assertFalse(any(c[:2] in (["git", "commit"], ["git", "push"]) for c in self.calls))
        self.assertFalse(any(c[:3] == ["pixi", "run", "recital"] for c in self.calls))
        self.assertIn(".github/workflows/ci.yml", out)

    def test_an_existing_worktree_is_never_reused(self):
        (self.main.parent / "cecelia-fix-fanout-b1").mkdir()
        _, out = self.work(_answer())
        self.assertEqual(self.calls, [])
        self.assertIn("exists already", out)


if __name__ == "__main__":
    unittest.main()
