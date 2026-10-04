"""Tests for `scripts/judge/weekly.py` — the weekly judge pass.

Design: docs/ai-assist/WEEKLY_JUDGE.md. No test spawns `claude`, `gh` or `pixi`: the sweep, verify
and rule steps are patched or injected, and publish gets a fake runner. The store is a temp dir.

Run with `pixi run test-py`.
"""
from __future__ import annotations

import importlib.util
import json
import os
import pathlib
import subprocess
import sys
import unittest
from unittest import mock

from cecelia.tests.test_judge_record import _bug, _Fixture

_REPO = pathlib.Path(__file__).resolve().parents[3]
_WEEKLY_PATH = _REPO / "scripts" / "judge" / "weekly.py"


def _load_weekly():
    spec = importlib.util.spec_from_file_location("weekly", _WEEKLY_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class _WeeklyFixture(_Fixture):
    def setUp(self):
        super().setUp()
        self.w = _load_weekly()
        self.rec = self.w._record   # the module the pass writes with
        self.swept = {}

        def sweep(events, *, date, sha, previous, judge=None, merged_prs=None, meter=None):
            self.swept.update(previous=previous, sha=sha)
            meter.update(input=10, cache_write=0, cache_read=2000, output=300)
            return [_bug("B1"), _bug("B2", verify={"verdict": "decide", "date": date})], 0.4

        def verify(bugs, *, date, sha, agent=None):
            return bugs, {"groups": 1, "verified": 1, "failed": 0, "waiting": 0, "usd": 2.0,
                          "tokens": {"input": 5, "cache_write": 1000, "cache_read": 50000, "output": 4000}}

        def propose(events, *, date, assign=None, meter=None):
            meter.update(input=1, output=200)
            rows = [{"rule": "CLAUDE.md → *Testing*", "findings": 4, "sessions": 3, "agent_made": 4, "legacy": 0}]
            props = [{"id": "P1", "kind": "tighten", "rule": rows[0]["rule"], "summary": "s", "sources": ["a"]}]
            return rows, props, {"agent_made": 4, "legacy": 0, "unknown": 0}, 0.5

        for mod, name, fn in ((self.w._bugs, "sweep", sweep), (self.w._verify, "verify", verify),
                              (self.w._rules, "propose", propose)):
            p = mock.patch.object(mod, name, fn)
            p.start()
            self.addCleanup(p.stop)

    def run_pass(self, **kw):
        return self.w.weekly(ref="HEAD", live=False, date=kw.pop("date", "2026-10-05"), **kw)


class WeeklyTest(_WeeklyFixture):
    def test_a_pass_makes_a_valid_record_with_its_spend_summed(self):
        record = self.run_pass()
        self.assertEqual(self.rec.validate(record), [])
        self.assertEqual(record["run"]["sha"], self.swept["sha"])
        self.assertEqual(record["run"]["spend"]["total_usd"], 2.9)
        self.assertEqual(record["queue"], [{"kind": "bug", "ref": "B2"}])
        self.assertEqual(record["proposals"][0]["kind"], "tighten")
        self.assertEqual((record["run"]["rules_window_days"], record["run"]["min_sessions"]), (30, 3))

    def test_tokens_are_kept_per_step_and_summed(self):
        tok = self.run_pass()["run"]["spend"]["tokens"]
        self.assertEqual(tok["total"], {"input": 16, "cache_write": 1000, "cache_read": 52000, "output": 4500})
        self.assertEqual(tok["rules"], {"input": 1, "output": 200})
        md = self.rec.render_markdown(self.run_pass())
        self.assertIn("| Tokens | 4.5k out · 16 in (+ cache read 52.0k, cache write 1.0k) — sweep 2.3k · "
                      "verify 55.0k · rules 201 |", md)

    def test_owner_answers_go_into_the_previous_record_before_its_bugs_are_carried(self):
        prev = self.build("2026-09-28", bugs=[_bug("B1")])
        self.rec.write(prev)
        self.w._review.append_review("bug_status", "2026-09-28", "B1", "wont_fix")
        stored = self.tmp / "judge-runs" / "2026-09-28.json"
        self.run_pass(persist=False)
        self.assertEqual(self.rec.load(stored)["bugs"][0]["status"], "open")       # a dry run writes nothing
        self.assertEqual(self.swept["previous"]["bugs"][0]["status"], "wont_fix")
        self.run_pass()
        self.assertEqual(self.rec.load(stored)["bugs"][0]["status"], "wont_fix")

    def test_a_crash_still_writes_a_failure_record_and_its_pr(self):
        def crash(**kwargs):
            kwargs["state"].update(stage="verify", sha="abc123")
            raise RuntimeError("agent down")
        with mock.patch.object(self.w, "weekly", crash), \
                mock.patch.object(self.w, "publish", return_value="url") as publish:
            self.assertEqual(self.w.main(["--date", "2026-10-05"]), 1)
        self.assertEqual(publish.call_args.args[0]["kind"], "failure")
        rec = self.rec.load(self.tmp / "judge-runs" / "2026-10-05.json")
        self.assertEqual((rec["kind"], rec["run"]["stage"], rec["run"]["sha"]), ("failure", "verify", "abc123"))

    def test_a_failure_never_replaces_a_pass_record(self):
        self.rec.write(self.build("2026-10-05"))
        with mock.patch.object(self.w, "weekly", side_effect=RuntimeError("boom")), \
                mock.patch.object(self.w, "publish") as publish:
            self.assertEqual(self.w.main(["--date", "2026-10-05"]), 1)
        publish.assert_not_called()
        self.assertEqual(self.rec.load(self.tmp / "judge-runs" / "2026-10-05.json")["kind"], "pass")

    def test_a_rerun_the_same_day_replaces_its_record(self):
        record = self.run_pass()
        self.rec.write(record)
        with mock.patch.object(self.w, "weekly", return_value=record), \
                mock.patch.object(self.w, "publish", return_value="url"):
            self.assertEqual(self.w.main([]), 0)
            self.assertEqual(self.w.main(["--no-pr"]), 0)


class PublishTest(_WeeklyFixture):
    def _fake_run(self, open_prs):
        self.cmds = []

        def run(cmd, input=None, **kw):
            self.cmds.append((cmd, input))
            out = ""
            if cmd[1:3] == ["pr", "list"]:
                out = json.dumps(open_prs)
            elif cmd[1:3] == ["pr", "create"]:
                out = "https://github.com/o/r/pull/9\n"
            elif cmd[-1] == "recital":
                out = "_Fanout audit: skipped — docs-only diff_\n"
            return subprocess.CompletedProcess(cmd, 0, out, "")
        return run

    def test_commits_the_record_then_closes_the_older_run_pr(self):
        wt = self.tmp / "wt"
        url = self.w.publish(self.run_pass(), worktree=wt, run=self._fake_run(
            [{"number": 5, "headRefName": "judge-run/2026-09-28", "url": "u5"},
             {"number": 6, "headRefName": "feat/other", "url": "u6"}]))
        self.assertEqual(url, "https://github.com/o/r/pull/9")
        names = [" ".join(c[1:4]) for c, _ in self.cmds]
        self.assertEqual(names[0], "checkout --quiet -B")
        self.assertIn("run audit-rollup", names)
        commit = next(i for c, i in self.cmds if c[1] == "commit")
        self.assertTrue(commit.startswith("judge: run record 2026-10-05 (2 open bugs)"))
        self.assertIn("docs-only diff", commit)
        self.assertIn("Co-Authored-By:", commit)
        body = next(i for c, i in self.cmds if c[1:3] == ["pr", "create"])
        self.assertIn("**Bugs: 2 open** (2 new)", body)
        self.assertIn("1 for you to decide", body)
        self.assertIn("P1 · tighten", body)
        closed = [c for c, _ in self.cmds if c[1:3] == ["pr", "close"]]
        self.assertEqual([c[3] for c in closed], ["5"])             # never the unrelated PR
        self.assertTrue((wt / "docs" / "ai-assist" / "judge-runs" / "2026-10-05.md").is_file())

    def test_a_rerun_of_the_same_date_reuses_its_pr(self):
        self.w.publish(self.run_pass(), worktree=self.tmp / "wt", run=self._fake_run(
            [{"number": 7, "headRefName": "judge-run/2026-10-05", "url": "u7"}]))
        self.assertFalse([c for c, _ in self.cmds if c[1:3] in (["pr", "create"], ["pr", "close"])])

    def test_a_failure_record_skips_the_rollup(self):
        failed = self.rec.failure_record("2026-10-05", stage="bugs", error="boom", sha="abc")
        self.w.publish(failed, worktree=self.tmp / "wt", run=self._fake_run([]))
        self.assertFalse([c for c, _ in self.cmds if "rollup" in c[-1]])
        body = next(i for c, i in self.cmds if c[1:3] == ["pr", "create"])
        self.assertIn("**failed** at `bugs`", body)


class CleanEnvTest(_WeeklyFixture):
    def test_the_outer_pixi_activation_is_dropped(self):
        outer = {"PIXI_PROJECT_MANIFEST": "/a/pixi.toml", "CONDA_PREFIX": "/a/.pixi/envs/default",
                 "PYTHONPATH": "/a/python", "HOME": "/h",
                 "PATH": os.pathsep.join(["/a/.pixi/envs/default/bin", "/h/.pixi/bin", "/usr/bin"])}
        with mock.patch.dict(os.environ, outer, clear=True):
            env = self.w.clean_env(X="1")
        self.assertEqual(env, {"HOME": "/h", "X": "1", "PATH": os.pathsep.join(["/h/.pixi/bin", "/usr/bin"])})


@unittest.skipIf(sys.platform == "win32", "stands `true` in for pixi")
class WorktreeTest(_WeeklyFixture):
    def _git(self, *args, cwd):
        subprocess.run(["git", *args], cwd=cwd, check=True, capture_output=True)

    def test_the_worktree_is_created_then_reset_with_its_env_kept(self):
        repo = self.tmp / "repo"
        repo.mkdir()
        self._git("init", "-q", cwd=repo)
        (repo / "f.txt").write_text("one\n", encoding="utf-8")
        self._git("add", "f.txt", cwd=repo)
        self._git("-c", "user.email=t@t", "-c", "user.name=t", "commit", "-qm", "one", cwd=repo)
        sha = self.w.git_output("rev-parse", "HEAD", cwd=str(repo))
        wt = self.tmp / "wt"
        which = self.w.shutil.which
        true = which("true")   # /bin/true on Linux, /usr/bin/true on macOS
        pixi_is_true = lambda name: true if name == "pixi" else which(name)   # noqa: E731
        with mock.patch.object(self.w.shutil, "which", pixi_is_true):
            self.w.prepare_worktree(wt, sha, repo)
            (wt / "f.txt").write_text("dirty\n", encoding="utf-8")
            (wt / ".pixi").mkdir()
            (wt / "stray.txt").write_text("x", encoding="utf-8")
            self.w.prepare_worktree(wt, sha, repo)
        self.assertEqual((wt / "f.txt").read_text(encoding="utf-8"), "one\n")
        # last run's `judge-run/*` branch must not move when the worktree resets to a newer SHA
        self._git("checkout", "-q", "-B", "judge-run/old", cwd=wt)
        (repo / "f.txt").write_text("two\n", encoding="utf-8")
        self._git("-c", "user.email=t@t", "-c", "user.name=t", "commit", "-qam", "two", cwd=repo)
        new = self.w.git_output("rev-parse", "HEAD", cwd=str(repo))
        with mock.patch.object(self.w.shutil, "which", pixi_is_true):
            self.w.prepare_worktree(wt, new, repo)
        self.assertEqual(self.w.git_output("rev-parse", "judge-run/old", cwd=str(repo)), sha)
        self.assertTrue((wt / ".pixi").is_dir())
        self.assertFalse((wt / "stray.txt").exists())


if __name__ == "__main__":
    unittest.main()
