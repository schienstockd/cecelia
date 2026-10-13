"""`judge-review --auto` (`scripts/judge/auto_fix.py`): which bugs it takes, the agent's fence, and
what it records. The sessions and every git / gh / pixi call are faked: nothing reaches GitHub."""
from __future__ import annotations

import importlib.util
import io
import json
import pathlib
import subprocess
import unittest
from unittest import mock

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
        self.calls, self.messages = [], []
        removed = mock.patch.object(self.af.agent_sandbox, "remove_worktree",
                                    lambda repo, dest: self.calls.append(["remove_worktree", str(dest)]))
        removed.start()
        self.addCleanup(removed.stop)

    def sh(self, fail=(), diff=""):
        def run(argv, cwd, **_):
            if argv[:3] == ["git", "diff", "--cached"]:
                self.calls.append(list(argv))
                return subprocess.CompletedProcess(argv, 0, diff, "")
            self.calls.append(list(argv))
            if argv[:2] == ["git", "commit"]:
                self.messages.append(pathlib.Path(argv[3]).read_text(encoding="utf-8"))
            out = {"recital": RECITAL, "create": "https://github.com/o/r/pull/9\n"}.get(argv[2] if len(argv) > 2 else "", "")
            return subprocess.CompletedProcess(argv, 1 if argv[1] in fail else 0, out, "boom" if argv[1] in fail else "")
        return run

    def session(self, *answers):
        it = iter(answers)
        self.prompts = []

        def run(prompt, cwd, schema, *, resume=None, main):
            self.prompts.append((prompt, resume))
            a = next(it)
            return (a, 1.5, "sid-1", None) if a is not None else (None, 0.5, None, "session failed (exit 1): x")
        return run

    def work(self, *answers, fail=(), diff=""):
        out = io.StringIO()
        bugs = [self.record["bugs"][0]]
        spent = self.af.work_one(self.record, bugs, main=self.main, out=out, session=self.session(*answers),
                                 sh=self.sh(fail, diff), path=self.log, auto_log=self.auto)
        return spent, out.getvalue()


def _answer(outcome="fixed", draft=False):
    return {"bugs": [{"key": "fanout-b1", "outcome": outcome, "reason": "because"}], "title": "fix(a): b",
            "summary": "What broke; fanout-b1 fixed.", "draft": draft}


class ItemsTest(_AutoFixture):
    def test_only_the_bugs_verify_called_fix(self):
        items = self.rv.auto_items(self.record, [])
        self.assertEqual([it["ref"] for it in items], ["B1"])

    def test_a_bug_the_owner_or_an_auto_session_already_took_is_skipped(self):
        self.rv.append_review("bug_work", "2026-10-05", "B1", "fix_session", path=self.log)
        self.assertEqual(self.rv.auto_items(self.record, self.rv.read_reviews(self.log)), [])
        self.assertEqual(self.rv.auto_items(self.record, [], tried={"fanout-b1": {"outcome": "left"}}), [])
        # a failed session logged nothing per bug: tried again
        self.assertEqual(len(self.rv.auto_items(self.record, [], tried={})), 1)


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

    def test_the_session_is_fenced_and_never_skips_permissions(self):
        seen = {}

        def run(argv, **kw):
            seen.update(argv=argv, env=kw["env"], cwd=kw["cwd"])
            return subprocess.CompletedProcess(argv, 0, '{"structured_output": {"tags": []}, "session_id": "s"}', "")
        with mock.patch.object(self.af, "resolve_claude_bin", return_value="claude"), \
                mock.patch.object(self.af.subprocess, "run", run):
            answer, _, sid, failed = self.af.default_session("p", self.tmp, {}, resume="s0", main=self.main)
        self.assertEqual((answer, sid, failed), ({"tags": []}, "s", None))
        argv = seen["argv"]
        self.assertNotIn("--dangerously-skip-permissions", argv)
        self.assertEqual(argv[argv.index("--permission-mode") + 1], "acceptEdits")
        self.assertEqual(json.loads(argv[argv.index("--settings") + 1]), self.af.settings(self.main))
        self.assertEqual(argv[argv.index("--resume") + 1], "s0")
        self.assertIn("--strict-mcp-config", argv)
        self.assertEqual(seen["env"]["CECELIA_OBSERVER_NO_PAIR"], "1")


class WorkTest(_AutoFixture):
    def test_a_fix_is_committed_with_its_tags_pushed_and_opened_as_a_pr(self):
        spent, out = self.work(_answer(draft=True), {"tags": ["[fanout-x1: fixed_pre_commit]"]})
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
        self.assertIn("--draft", create)
        self.assertFalse(any(c[:3] == ["gh", "pr", "merge"] for c in self.calls))
        rows = self.rv.read_reviews(self.log)
        self.assertEqual([(r["ref"], r["value"], r["note"]) for r in rows],
                         [("B1", "fix_session", "auto PR https://github.com/o/r/pull/9")])
        self.assertEqual(self.rv.read_auto(self.auto)["fanout-b1"]["outcome"], "fixed")
        self.assertIn("PR (draft): https://github.com/o/r/pull/9", out)

    def test_not_real_commits_nothing_removes_the_worktree_and_shows_on_the_card(self):
        self.work(_answer("not_real"))
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

    def test_a_failed_session_logs_nothing_so_the_next_run_tries_again(self):
        self.work(None)
        self.assertEqual(self.rv.read_auto(self.auto), {})
        self.assertEqual(self.rv.read_reviews(self.log), [])

    def test_an_existing_worktree_is_never_reused(self):
        (self.main.parent / "cecelia-fix-fanout-b1").mkdir()
        _, out = self.work(_answer())
        self.assertEqual(self.calls, [])
        self.assertIn("exists already", out)


if __name__ == "__main__":
    unittest.main()
