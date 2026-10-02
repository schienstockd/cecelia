"""Tests for `scripts/claude_md_eval/supervise.py` — the supervised eval pass.

Design: docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md → phase 3. No test spawns `claude` or runs the
suite: the judge, the suite and the retries are injected or patched. The log and traces are the
synthetic fixture from `test_claude_md_eval_record`.

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

from cecelia.tests.test_claude_md_eval_record import _Fixture, _event

_REPO = pathlib.Path(__file__).resolve().parents[3]
_SUPERVISE_PATH = _REPO / "scripts" / "claude_md_eval" / "supervise.py"


def _load_supervise():
    spec = importlib.util.spec_from_file_location("supervise", _SUPERVISE_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _verdict(cls="genuine", evidence="+BAD", **over):
    return {"class": cls, "title": f"{cls} title", "diagnosis": "why", "evidence": evidence,
            "proposed_fix": "fix it", "files": ["a.py"], "question": ""} | over


class _SuperviseFixture(_Fixture):
    def setUp(self):
        super().setUp()
        self.sup = _load_supervise()
        prompts = self.tmp / "prompts"
        p = mock.patch.object(self.sup._run_prompt, "_PROMPTS_DIR", prompts)
        p.start()
        self.addCleanup(p.stop)
        # setup size is measured on this checkout; its growth must not change what these tests see
        b = mock.patch.dict(self.sup._record.SETUP_BUDGET, {k: 10**6 for k in self.sup._record.SETUP_BUDGET})
        b.start()
        self.addCleanup(b.stop)
        with open(self.tmp / "events.jsonl", "w", encoding="utf-8") as fh:
            fh.writelines(json.dumps(e) + "\n" for e in self.events)
        self.calls = []

    def assign(self, prompt):
        # curation's finding→rule judge; injected so no test can ever reach a real `claude`
        return {"assignments": []}, 0.0

    def judge(self, verdict=None, cost=0.1):
        def fn(prompt):
            self.calls.append(prompt)
            return (verdict or _verdict()), cost
        return fn


class TriageTest(_SuperviseFixture):
    def _failures(self):
        draft = self.sup._record.build(self.events, "2026-09-30")
        return [t for t in draft["traces"] if (t["scores"]["rescored"] or t["scores"]["raw"]) != "compliant"]

    def test_an_errored_run_is_infra_without_a_judge_call(self):
        judged, spend = self.sup.triage(self._failures(), judge=self.judge())
        self.assertEqual([j["verdict"]["class"] for j in judged], ["genuine", "infra"])
        self.assertEqual((len(self.calls), spend["judge_calls"], spend["cost_usd"]), (1, 1, 0.1))

    def test_the_judge_gets_the_run_as_data_after_the_brief(self):
        self.sup.triage(self._failures(), judge=self.judge())
        prompt = self.calls[0]
        self.assertTrue(prompt.startswith(self.sup._JUDGE_BRIEF))
        self.assertIn("RULE: R", prompt)
        self.assertIn("ANTI-SIGNAL MATCHES (added lines):\n+BAD", prompt)
        self.assertIn("PROMPT FILE (task + scorer): scripts/claude_md_eval/prompts/p1.md", prompt)

    def test_a_quote_not_in_the_trace_is_marked_unverified(self):
        judged, _ = self.sup.triage(self._failures(), judge=self.judge(_verdict(evidence="+NOT THERE")))
        self.assertFalse(judged[0]["verified"])

    def test_the_budget_stops_judging_and_counts_what_it_skipped(self):
        judged, spend = self.sup.triage(self._failures(), judge=self.judge(), budget=0)
        self.assertEqual((spend["skipped"], len(self.calls)), (1, 0))
        self.assertEqual([j["verdict"]["class"] for j in judged], ["infra"])

    def test_a_failed_judge_call_is_counted_not_fatal(self):
        def broken(prompt):
            raise self.sup.SupervisorError("triage", "judge failed")
        judged, spend = self.sup.triage(self._failures(), judge=broken)
        self.assertEqual(spend["failed"], 1)


class GroupFindingsTest(_SuperviseFixture):
    def _judged(self, *classes):
        return [{"prompt_id": "p1", "trace": f"t{i}", "verdict": _verdict(c), "verified": True}
                for i, c in enumerate(classes)]

    def test_one_finding_per_prompt_and_class(self):
        findings, _ = self.sup.group_findings(self._judged("genuine", "genuine", "decision"), date="d")
        self.assertEqual([(f["class"], f["runs"]) for f in findings], [("decision", 1), ("genuine", 2)])

    def test_recurring_only_when_the_previous_record_had_it(self):
        prev = {"findings": [{"slug": "p1", "class": "genuine"}]}
        findings, _ = self.sup.group_findings(self._judged("genuine", "decision"), date="d", previous=prev)
        self.assertEqual({f["class"]: f["recurrence"] for f in findings},
                         {"genuine": "recurring", "decision": "watch"})

    def test_a_dropped_finding_is_not_recurring_but_a_resolved_one_is(self):
        prev = {"findings": [{"slug": "p1", "class": "genuine", "status": "dropped"},
                             {"slug": "p1", "class": "decision", "status": "resolved"}]}
        findings, _ = self.sup.group_findings(self._judged("genuine", "decision"), date="d", previous=prev)
        self.assertEqual({f["class"]: f["recurrence"] for f in findings},
                         {"genuine": "watch", "decision": "recurring"})

    def test_an_open_owner_decision_wins_over_this_runs_judge(self):
        prev = {"date": "2026-09-30", "findings": [
            {"id": "F6", "slug": "p1", "class": "decision", "status": "open", "proposed_fix": "(a) or (b)?"}]}
        findings, actions = self.sup.group_findings(self._judged("scorer_bug", "infra"), date="d", previous=prev)
        self.assertEqual(sorted((f["class"], f["runs"]) for f in findings), [("decision", 1), ("infra", 1)])
        carried = next(f for f in findings if f["class"] == "decision")
        self.assertEqual(carried["proposed_fix"], "(a) or (b)?")
        self.assertIn("Still waiting on the owner's decision F6 from 2026-09-30", carried["diagnosis"])
        self.assertEqual(actions, [])                    # a decision is the owner's, not a next action
        answered = {"date": "2026-09-30", "findings": [dict(prev["findings"][0], status="resolved")]}
        findings, _ = self.sup.group_findings(self._judged("scorer_bug"), date="d", previous=answered)
        self.assertEqual([f["class"] for f in findings], ["scorer_bug"])   # answered: the judge decides again

    def test_actions_put_scorer_bugs_first_and_skip_decisions(self):
        _, actions = self.sup.group_findings(self._judged("genuine", "scorer_bug", "decision"), date="d")
        self.assertEqual([a["verify"].split()[2] for a in actions],
                         ["claude-md-eval-record", "claude-md-eval"])


class CleanEnvTest(_SuperviseFixture):
    def test_the_outer_pixi_activation_is_dropped(self):
        outer = {"PIXI_PROJECT_MANIFEST": "/a/pixi.toml", "CONDA_PREFIX": "/a/.pixi/envs/default",
                 "PYTHONPATH": "/a/python", "HOME": "/h",
                 "PATH": os.pathsep.join(["/a/.pixi/envs/default/bin", "/h/.pixi/bin", "/usr/bin"])}
        with mock.patch.dict(os.environ, outer, clear=True):
            env = self.sup.clean_env(X="1")
        self.assertEqual(env, {"HOME": "/h", "X": "1", "PATH": os.pathsep.join(["/h/.pixi/bin", "/usr/bin"])})

    def test_a_windows_env_bin_is_dropped_too(self):
        # no drive letter: on POSIX `os.pathsep` is ":" and would split `C:` apart
        path = os.pathsep.join([r"\x\.pixi\envs\y\bin", r"\Users\h\.pixi\bin"])
        with mock.patch.dict(os.environ, {"PATH": path}, clear=True):
            env = self.sup.clean_env()
        self.assertEqual(env["PATH"], r"\Users\h\.pixi\bin")


class RetryTest(_SuperviseFixture):
    def test_retries_until_a_run_does_not_error_and_never_more_than_twice(self):
        rows = [e for e in self.events if e["event"] == "claude_md_eval_run" and e["session"] == "eval-1"]
        verdicts = iter(["error", "noncompliant"])
        logged: list[dict] = []

        def retry(worktree, session, prompt_id):
            logged.append(_event("claude_md_eval_run", f"2026-09-30T11:0{len(logged)}:00Z",
                                 {"prompt_id": prompt_id, "verdict": next(verdicts), "trace_dir": "t"}))

        out = self.sup.retry_infra(rows, worktree=self.tmp, session="eval-1",
                                   suite_ts="2026-09-30T10:01:00Z", retry=retry, events=lambda: logged)
        self.assertEqual([(r["attempt"], r["verdict"]) for r in out], [(1, "error"), (2, "noncompliant")])


class SuperviseTest(_SuperviseFixture):
    def test_a_logged_pass_gets_a_valid_record_with_the_judge_spend_apart(self):
        record = self.sup.supervise(session="eval-1", judge=self.judge(), assign=self.assign)
        self.assertEqual(self.sup._record.validate(record), [])
        self.assertEqual(record["run"]["supervisor"]["judge_calls"], 1)
        self.assertEqual(record["run"]["cost_usd"], 0.3)   # the suite's, unchanged
        self.assertEqual([f["class"] for f in record["findings"]], ["genuine", "infra"])

    def test_a_setup_over_budget_adds_a_decision_finding(self):
        with mock.patch.dict(self.sup._record.SETUP_BUDGET, {"claude_md_lines": 1}):
            record = self.sup.supervise(session="eval-1", judge=self.judge(), assign=self.assign)
        self.assertEqual(self.sup._record.validate(record), [])
        size = [f for f in record["findings"] if f["slug"] == "setup-size"]
        self.assertEqual([(f["id"], f["class"]) for f in size], [("F3", "decision")])
        self.assertIn({"kind": "decision", "ref": "F3"}, record["queue"])

    def test_owner_answers_are_applied_to_the_earlier_record_first(self):
        prev = self.sup._record.build(self.events, "2026-09-30", annotations={"findings": [
            {"id": "F1", "slug": "p1", "class": "genuine", "status": "open", "recurrence": "watch",
             "evidence": [{"trace": "t", "excerpt": "+BAD"}], "diagnosis": "d", "proposed_fix": "x"}]})
        prev["date"] = "2026-09-23"
        self.sup._record.write(prev)
        review = self.sup._load_sibling("review")
        review.append_review("finding_status", "2026-09-23", "F1", "resolved")
        stored = self.tmp / "eval-runs" / "2026-09-23.json"

        self.sup.supervise(session="eval-1", judge=self.judge(), assign=self.assign, persist=False)
        self.assertEqual(self.sup._record.load(stored)["findings"][0]["status"], "open")    # a dry run
        record = self.sup.supervise(session="eval-1", judge=self.judge(), assign=self.assign)
        self.assertEqual(self.sup._record.load(stored)["findings"][0]["status"], "resolved")
        self.assertEqual(record["delta"]["previous"], "2026-09-23")
        self.assertEqual(record["delta"]["still_open"], [])     # resolved by the owner, so not "still open"

    def test_a_crash_still_writes_a_failure_record(self):
        def crash(**kwargs):
            kwargs["state"].update(stage="suite", sha="abc123")
            raise self.sup.SupervisorError("suite", "claude-md-eval exited 1")
        with mock.patch.object(self.sup, "supervise", crash), \
                mock.patch.object(self.sup, "publish", return_value="url") as publish:
            self.assertEqual(self.sup.main(["--date", "2026-10-05"]), 1)
        self.assertEqual(publish.call_args.args[0]["kind"], "failure")   # its PR too (Decision 13)
        rec = self.sup._record.load(self.tmp / "eval-runs" / "2026-10-05.json")
        self.assertEqual((rec["kind"], rec["run"]["stage"], rec["run"]["sha"]), ("failure", "suite", "abc123"))
        self.assertIn("FAILED", self.sup._record.render_markdown(rec))

    def test_a_live_rerun_the_same_day_replaces_its_record(self):
        record = self.sup.supervise(session="eval-1", judge=self.judge(), assign=self.assign)
        self.sup._record.write(record)
        with mock.patch.object(self.sup, "supervise", return_value=record), \
                mock.patch.object(self.sup, "publish", return_value="url"):
            self.assertEqual(self.sup.main([]), 0)                          # live: replaces
            with self.assertRaises(self.sup._record.RecordError):
                self.sup._record.write(record)                              # still guarded by default
            self.assertEqual(self.sup.main(["--session", "eval-1"]), 1)     # re-triage: needs --force
        self.assertEqual(sorted(p.stem for p in (self.tmp / "eval-runs").glob("*.json")), ["2026-09-30"])

    def test_a_failure_never_replaces_a_pass_record(self):
        self.sup._record.write(self.sup._record.build(self.events, "2026-09-30"))
        with mock.patch.object(self.sup, "supervise", side_effect=RuntimeError("boom")), \
                mock.patch.object(self.sup, "publish") as publish:
            self.assertEqual(self.sup.main(["--date", "2026-09-30"]), 1)
        publish.assert_not_called()
        self.assertEqual(self.sup._record.load(self.tmp / "eval-runs" / "2026-09-30.json")["kind"], "pass")


class PublishTest(_SuperviseFixture):
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

    def _record(self):
        return self.sup.supervise(session="eval-1", judge=self.judge(), assign=self.assign)

    def test_commits_the_record_and_rollups_then_closes_the_older_run_pr(self):
        wt = self.tmp / "wt"
        url = self.sup.publish(self._record(), worktree=wt, run=self._fake_run(
            [{"number": 5, "headRefName": "eval-run/2026-09-23", "url": "u5"},
             {"number": 6, "headRefName": "feat/other", "url": "u6"}]))
        self.assertEqual(url, "https://github.com/o/r/pull/9")
        names = [" ".join(c[1:4]) for c, _ in self.cmds]
        self.assertEqual(names[0], "checkout --quiet -B")
        self.assertIn("run claude-md-eval-rollup", names)
        self.assertIn("run audit-rollup", names)
        commit = next(i for c, i in self.cmds if c[1] == "commit")
        self.assertIn("docs-only diff", commit)
        self.assertIn("Co-Authored-By:", commit)
        closed = [c for c, _ in self.cmds if c[1:3] == ["pr", "close"]]
        self.assertEqual([c[3] for c in closed], ["5"])             # never the unrelated PR
        self.assertIn("Superseded by https://github.com/o/r/pull/9", closed[0][-1])
        self.assertTrue((wt / "docs" / "ai-assist" / "eval-runs" / "2026-09-30.md").is_file())

    def test_a_rerun_of_the_same_date_reuses_its_pr(self):
        self.sup.publish(self._record(), worktree=self.tmp / "wt", run=self._fake_run(
            [{"number": 7, "headRefName": "eval-run/2026-09-30", "url": "u7"}]))
        self.assertFalse([c for c, _ in self.cmds if c[1:3] in (["pr", "create"], ["pr", "close"])])

    def test_a_failure_record_skips_the_rollups(self):
        failed = self.sup._record.failure_record("2026-09-30", stage="suite", error="boom", sha="abc")
        self.sup.publish(failed, worktree=self.tmp / "wt", run=self._fake_run([]))
        self.assertFalse([c for c, _ in self.cmds if "rollup" in c[-1]])
        body = next(i for c, i in self.cmds if c[1:3] == ["pr", "create"])
        self.assertIn("**failed** at `suite`", body)


@unittest.skipIf(sys.platform == "win32", "stands `true` in for pixi")
class WorktreeTest(_SuperviseFixture):
    def _git(self, *args, cwd):
        subprocess.run(["git", *args], cwd=cwd, check=True, capture_output=True)

    def test_the_worktree_is_created_then_reset_with_its_env_kept(self):
        repo = self.tmp / "repo"
        repo.mkdir()
        self._git("init", "-q", cwd=repo)
        (repo / "f.txt").write_text("one\n", encoding="utf-8")
        self._git("add", "f.txt", cwd=repo)
        self._git("-c", "user.email=t@t", "-c", "user.name=t", "commit", "-qm", "one", cwd=repo)
        sha = self.sup.git_output("rev-parse", "HEAD", cwd=str(repo))
        wt = self.tmp / "wt"
        which = self.sup.shutil.which
        true = which("true")   # /bin/true on Linux, /usr/bin/true on macOS
        pixi_is_true = lambda name: true if name == "pixi" else which(name)   # noqa: E731
        with mock.patch.object(self.sup.shutil, "which", pixi_is_true):
            self.sup.prepare_worktree(wt, sha, repo)
            (wt / "f.txt").write_text("dirty\n", encoding="utf-8")
            (wt / ".pixi").mkdir()
            (wt / "stray.txt").write_text("x", encoding="utf-8")
            self.sup.prepare_worktree(wt, sha, repo)
        self.assertEqual((wt / "f.txt").read_text(encoding="utf-8"), "one\n")
        # last run's `eval-run/*` branch must not move when the worktree resets to a newer SHA
        self._git("checkout", "-q", "-B", "eval-run/old", cwd=wt)
        (repo / "f.txt").write_text("two\n", encoding="utf-8")
        self._git("-c", "user.email=t@t", "-c", "user.name=t", "commit", "-qam", "two", cwd=repo)
        new = self.sup.git_output("rev-parse", "HEAD", cwd=str(repo))
        with mock.patch.object(self.sup.shutil, "which", pixi_is_true):
            self.sup.prepare_worktree(wt, new, repo)
        self.assertEqual(self.sup.git_output("rev-parse", "eval-run/old", cwd=str(repo)), sha)
        self.assertTrue((wt / ".pixi").is_dir())
        self.assertFalse((wt / "stray.txt").exists())


if __name__ == "__main__":
    unittest.main()
