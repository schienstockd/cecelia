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
        # never the real projects dir: the run-review scan reads an empty one unless a test says so
        env = mock.patch.dict(os.environ, {"CECELIA_AGENT_APP_PROJECTS": str(self.tmp / "no-projects")})
        env.start()
        self.addCleanup(env.stop)
        self.swept = {}

        self.fail_steps: dict = {}   # step → error its judge "failed" with

        def sweep(events, *, date, sha, previous, judge=None, merged_prs=None, meter=None, failures=None):
            self.swept.update(previous=previous, sha=sha)
            if isinstance(self.fail_steps.get("sweep"), BaseException):
                raise self.fail_steps["sweep"]
            if self.fail_steps.get("sweep"):
                failures["sweep"] = self.fail_steps["sweep"]
            meter.update(input=10, cache_write=0, cache_read=2000, output=300)
            return [_bug("B1"), _bug("B2", verify={"verdict": "decide", "date": date})], 0.4

        def verify(bugs, *, date, sha, agent=None):
            return bugs, {"groups": 1, "verified": 1, "failed": 0, "waiting": 0, "usd": 2.0,
                          "tokens": {"input": 5, "cache_write": 1000, "cache_read": 50000, "output": 4000}}

        def propose(events, *, date, assign=None, meter=None, failures=None):
            if self.fail_steps.get("rules"):
                failures["rules"] = self.fail_steps["rules"]
                return [], [], {"agent_made": 0, "legacy": 0, "unknown": 0}, 0.0
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

    def _limited(self, retry_left, message="You've hit your session limit · resets 1:40am (Australia/Sydney)"):
        self.fail_steps["sweep"] = self.w._judge.RateLimited(message)
        with mock.patch.dict(os.environ, {"JUDGE_RETRY_LEFT": str(retry_left)}), \
                mock.patch.object(self.w, "publish", return_value="url") as publish:
            code = self.w.main(["--date", "2026-10-05"])
        return code, publish, json.loads(self.w.ratelimit_path().read_text(encoding="utf-8"))

    def test_a_usage_limit_fails_the_pass_instead_of_recording_nothing_judged(self):
        code, publish, limit = self._limited(0)
        self.assertEqual(code, self.w.EX_TEMPFAIL)
        rec = self.rec.load(self.tmp / "judge-runs" / "2026-10-05.json")
        self.assertEqual((rec["kind"], rec["run"]["stage"]), ("failure", "bugs"))
        self.assertIn("session limit · resets 1:40am", rec["run"]["error"])
        # no attempts left: this one's failure is the PR
        self.assertEqual(publish.call_args.args[0]["kind"], "failure")
        self.assertFalse(limit["retry"])
        self.assertEqual(limit["stage"], "bugs")

    def test_a_usage_limit_with_a_retry_to_follow_opens_no_pr(self):
        # 1:40am Sydney is up to 24h off on the real clock: any reset is "close enough" here
        with mock.patch.object(self.w, "RETRY_MAX_WAIT", self.w._dt.timedelta(days=1)):
            code, publish, limit = self._limited(2)
        self.assertEqual(code, self.w.EX_TEMPFAIL)
        publish.assert_not_called()
        self.assertTrue(limit["retry"])
        reset = self.w._dt.datetime.fromisoformat(limit["reset"])
        self.assertLess(reset - self.w._dt.datetime.now(self.w._dt.timezone.utc), self.w._dt.timedelta(days=1))
        # the failure record is still written; the retry's pass record replaces it
        self.assertEqual(self.rec.load(self.tmp / "judge-runs" / "2026-10-05.json")["kind"], "failure")

    def test_a_reset_too_far_off_is_not_waited_for(self):
        with mock.patch.object(self.w, "RETRY_MAX_WAIT", self.w._dt.timedelta(minutes=1)):
            code, publish, limit = self._limited(2)
        self.assertEqual((code, limit["retry"]), (self.w.EX_TEMPFAIL, False))
        self.assertEqual(publish.call_args.args[0]["kind"], "failure")

    def test_a_failed_step_is_recorded_and_said_not_read_as_a_quiet_week(self):
        self.fail_steps.update(sweep="judge failed (exit 1): overloaded", rules="judge failed (exit 1): `boom`")
        record = self.run_pass()
        self.assertEqual(self.rec.validate(record), [])
        self.assertEqual(set(record["run"]["failed"]), {"sweep", "rules"})
        body = self.w._pr_body(record)
        self.assertIn("**The bug sweep's judge failed** (`judge failed (exit 1): overloaded`)", body)
        self.assertIn("**Rules: the judge failed** (`judge failed (exit 1): 'boom'`)", body)
        self.assertNotIn("nothing broken in enough sessions", body)
        md = self.rec.render_markdown(record)
        self.assertIn("| Failed | sweep: `judge failed (exit 1): overloaded` |", md)
        self.assertIn("The rule-mapping judge failed this pass", md)
        self.fail_steps.clear()
        self.assertNotIn("failed", self.run_pass()["run"])

    def test_the_headline_counts_verified_bugs_and_landed_fixes(self):
        fix = {"commit": "abcdef0123" + "0" * 30, "subject": "fix: x (fanout-b1)", "pr": 1430}
        bugs = [_bug("B1", fix_landed=[fix], first_seen="2026-09-28", opened="2026-09-28"),
                _bug("B2", verify={"verdict": "fix", "date": "2026-10-05"}),
                _bug("B3", status="gone", fix_landed=[{**fix, "pr": None}]),
                _bug("B4", status="unjudged", fix_landed=[fix])]
        record = self.build("2026-10-05", bugs=bugs)
        body = self.w._pr_body(record)
        # B1 is open but no agent checked it: "verified" counts only B2
        # the confirmed one is among the fixed; the other two are still waiting on the judge
        self.assertIn("**Bugs: 2 open** (1 verified, 1 new), 1 fixed since the last pass, "
                      "2 more with a fix landed, awaiting re-check, 1 waiting for the judge.", body)
        md = self.rec.render_markdown(record)
        self.assertIn("**Fix landed:** `abcdef01` #1430 — awaiting re-check", md)
        self.assertIn("**Fix landed:** `abcdef01` — confirmed fixed", md)
        self.assertIn("A fix landed for 3 bug(s) since the last pass (a commit names the bug's key): "
                      "1 confirmed fixed, 2 awaiting re-check.", md)
        self.assertIn("· `fanout-b4` — still there · fix landed `abcdef01` #1430", md)

    def test_a_landed_fix_verify_dismissed_counts_as_fixed_in_the_headline(self):
        fix = [{"commit": "abcdef0123", "subject": "fix"}]
        record = self.build("2026-10-05", bugs=[_bug("B1", status="dismissed", fix_landed=fix),
                                                _bug("B2", status="gone"), _bug("B3", status="dismissed")])
        self.assertIn("2 fixed since the last pass, 0 waiting", self.w._pr_body(record))

    def test_the_headline_says_parked_bugs_and_the_backlog_against_the_last_pass(self):
        self.rec.write(self.build("2026-09-28", bugs=[_bug(f"B{i}", status="unjudged") for i in range(1, 4)]))
        record = self.run_pass()
        self.assertEqual(record["run"]["backlog_last"], 3)
        record["bugs"] += [_bug("B9", status="parked", verify={"verdict": "guard", "date": "2026-10-05"}),
                           _bug("B10", status="parked", verify={"verdict": "guard", "date": "2026-10-05"}, recheck="x")]
        self.assertIn("**Bugs: 2 open** (1 verified, 2 new), 2 parked (can't happen yet; 1 due a re-check), 0 fixed since the last pass, "
                      "0 waiting for the judge (last pass 3)", self.w._pr_body(record))

    def test_the_issue_mirror_reports_on_the_record_and_never_fails_the_pass(self):
        from cecelia.tests.test_judge_issues import REPO, FakeGitHub
        record = self.run_pass()
        self.rec.write(record)
        github = FakeGitHub()
        report = self.w.mirror_issues(record, gh=self.w._issues.Gh(REPO, github))
        self.assertEqual(report["filed"], ["fanout-b2"])   # B2 is `decide`; B1 has no verdict yet
        self.assertEqual(self.rec.load(self.tmp / "judge-runs" / "2026-10-05.json")["run"]["issues"], report)
        github.crash_on = ["api", "user"]
        broken = self.w.mirror_issues(record, gh=self.w._issues.Gh(REPO, github))
        self.assertEqual(broken, {"error": "RuntimeError: pass died"})
        self.assertEqual(self.rec.load(self.tmp / "judge-runs" / "2026-10-05.json")["run"]["issues"], broken)

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


class IssueLayoutTest(_WeeklyFixture):
    """`--issues` / `JUDGE_ISSUES=1` (plan P4): issues + a status comment, a PR only for rules."""

    def _main(self, argv, *, proposals=True, env=None):
        record = self.run_pass()
        if not proposals:
            record["proposals"] = []
        with mock.patch.dict(os.environ, env or {}), \
                mock.patch.object(self.w, "weekly", return_value=record), \
                mock.patch.object(self.w, "mirror_issues", return_value={"filed": []}) as mirror, \
                mock.patch.object(self.w, "publish", return_value="pr-url") as publish, \
                mock.patch.object(self.w, "close_run_prs", return_value=[]) as close, \
                mock.patch.object(self.w, "post_status", return_value="status") as status:
            code = self.w.main(argv)
        return code, mirror, publish, close, status

    def test_proposals_get_a_rules_only_pr_and_every_pass_a_status_comment(self):
        code, mirror, publish, close, status = self._main(["--issues"])
        self.assertEqual(code, 0)
        mirror.assert_called_once()
        self.assertTrue(publish.call_args.kwargs["rules_only"])
        self.assertEqual(status.call_args.kwargs["pr"], "pr-url")
        close.assert_not_called()   # publish closes the older ones itself

    def test_no_proposals_no_pr_and_the_old_record_prs_are_closed(self):
        _, _, publish, close, status = self._main(["--issues"], proposals=False)
        publish.assert_not_called()
        close.assert_called_once()
        self.assertIsNone(status.call_args.kwargs["pr"])

    def test_the_timer_turns_it_on_through_the_environment(self):
        _, mirror, publish, _, _ = self._main([], env={"JUDGE_ISSUES": "1"})
        mirror.assert_called_once()
        self.assertTrue(publish.call_args.kwargs["rules_only"])
        _, mirror, publish, _, status = self._main([])
        mirror.assert_not_called()
        status.assert_not_called()
        self.assertNotIn("rules_only", publish.call_args.kwargs)

    def test_a_failed_pass_comments_on_the_status_issue_instead_of_a_failed_pr(self):
        def crash(**kwargs):
            kwargs["state"].update(stage="verify", sha="abc123")
            raise RuntimeError("agent down")
        with mock.patch.object(self.w, "weekly", crash), \
                mock.patch.object(self.w, "publish") as publish, \
                mock.patch.object(self.w, "post_status", return_value="status") as status:
            self.assertEqual(self.w.main(["--issues", "--date", "2026-10-05"]), 1)
        publish.assert_not_called()
        self.assertEqual(status.call_args.args[0]["kind"], "failure")

    def test_the_status_comment_goes_through_the_issue_mirror(self):
        from cecelia.tests.test_judge_issues import REPO, FakeGitHub
        record = self.run_pass()
        self.rec.write(record)
        github = FakeGitHub()
        line = self.w.post_status(record, {"filed": []}, gh=self.w._issues.Gh(REPO, github))
        self.assertEqual(line, f"status comment on https://github.com/{REPO}/issues/1")
        self.assertIn("**Pass 2026-10-05**", github.issues[1]["comments"][0])

    def test_the_record_is_rendered_beside_its_json(self):
        self.rec.write(self.run_pass())
        self.assertTrue((self.tmp / "judge-runs" / "2026-10-05.md").read_text(encoding="utf-8").startswith("# Weekly judge"))


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
        self.assertIn("**Bugs: 2 open** (1 verified, 2 new)", body)
        self.assertIn("1 for you to decide", body)
        self.assertIn("P1 · tighten", body)
        closed = [c for c, _ in self.cmds if c[1:3] == ["pr", "close"]]
        self.assertEqual([c[3] for c in closed], ["5"])             # never the unrelated PR
        self.assertTrue((wt / "docs" / "ai-assist" / "judge-runs" / "2026-10-05.md").is_file())

    def test_a_rules_only_pr_carries_the_rollup_not_the_record(self):
        wt = self.tmp / "wt"
        self.w.publish(self.run_pass(), worktree=wt, rules_only=True, run=self._fake_run(
            [{"number": 5, "headRefName": "judge-run/2026-09-28", "url": "u5"}]))
        commit = next((c, i) for c, i in self.cmds if c[1] == "commit")
        self.assertIn("--allow-empty", commit[0])
        self.assertTrue(commit[1].startswith("judge: rule proposals 2026-10-05 (1)"))
        body = next(i for c, i in self.cmds if c[1:3] == ["pr", "create"])
        self.assertIn("P1 · tighten", body)
        self.assertNotIn("**Bugs:", body)
        self.assertFalse((wt / "docs" / "ai-assist" / "judge-runs").exists())
        self.assertEqual([c[3] for c, _ in self.cmds if c[1:3] == ["pr", "close"]], ["5"])

    def test_a_rerun_of_the_same_date_reuses_its_pr(self):
        self.w.publish(self.run_pass(), worktree=self.tmp / "wt", run=self._fake_run(
            [{"number": 7, "headRefName": "judge-run/2026-10-05", "url": "u7"}]))
        self.assertFalse([c for c, _ in self.cmds if c[1:3] in (["pr", "create"], ["pr", "close"])])

    def test_a_failure_record_skips_the_rollup_and_closes_no_older_pr(self):
        failed = self.rec.failure_record("2026-10-05", stage="bugs", error="boom", sha="abc")
        self.w.publish(failed, worktree=self.tmp / "wt", run=self._fake_run(
            [{"number": 5, "headRefName": "judge-run/2026-09-28", "url": "u5"}]))
        self.assertFalse([c for c, _ in self.cmds if "rollup" in c[-1]])
        self.assertFalse([c for c, _ in self.cmds if c[1:3] == ["pr", "close"]])   # last week's bugs stay the work list
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
