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
        with open(self.tmp / "events.jsonl", "w", encoding="utf-8") as fh:
            fh.writelines(json.dumps(e) + "\n" for e in self.events)
        self.calls = []

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

    def test_actions_put_scorer_bugs_first_and_skip_decisions(self):
        _, actions = self.sup.group_findings(self._judged("genuine", "scorer_bug", "decision"), date="d")
        self.assertEqual([a["verify"].split()[2] for a in actions],
                         ["claude-md-eval-record", "claude-md-eval"])


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
        record = self.sup.supervise(session="eval-1", judge=self.judge())
        self.assertEqual(self.sup._record.validate(record), [])
        self.assertEqual(record["run"]["supervisor"]["judge_calls"], 1)
        self.assertEqual(record["run"]["cost_usd"], 0.3)   # the suite's, unchanged
        self.assertEqual([f["class"] for f in record["findings"]], ["genuine", "infra"])

    def test_a_crash_still_writes_a_failure_record(self):
        def crash(**kwargs):
            kwargs["state"].update(stage="suite", sha="abc123")
            raise self.sup.SupervisorError("suite", "claude-md-eval exited 1")
        with mock.patch.object(self.sup, "supervise", crash):
            self.assertEqual(self.sup.main(["--date", "2026-10-05"]), 1)
        rec = self.sup._record.load(self.tmp / "eval-runs" / "2026-10-05.json")
        self.assertEqual((rec["kind"], rec["run"]["stage"], rec["run"]["sha"]), ("failure", "suite", "abc123"))
        self.assertIn("FAILED", self.sup._record.render_markdown(rec))

    def test_a_failure_never_replaces_a_pass_record(self):
        self.sup._record.write(self.sup._record.build(self.events, "2026-09-30"))
        with mock.patch.object(self.sup, "supervise", side_effect=RuntimeError("boom")):
            self.assertEqual(self.sup.main(["--date", "2026-09-30"]), 1)
        self.assertEqual(self.sup._record.load(self.tmp / "eval-runs" / "2026-09-30.json")["kind"], "pass")


@unittest.skipIf(sys.platform == "win32", "uses /bin/true for pixi")
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
        pixi_is_true = lambda name: "/bin/true" if name == "pixi" else which(name)   # noqa: E731
        with mock.patch.object(self.sup.shutil, "which", pixi_is_true):
            self.sup.prepare_worktree(wt, sha, repo)
            (wt / "f.txt").write_text("dirty\n", encoding="utf-8")
            (wt / ".pixi").mkdir()
            (wt / "stray.txt").write_text("x", encoding="utf-8")
            self.sup.prepare_worktree(wt, sha, repo)
        self.assertEqual((wt / "f.txt").read_text(encoding="utf-8"), "one\n")
        self.assertTrue((wt / ".pixi").is_dir())
        self.assertFalse((wt / "stray.txt").exists())


if __name__ == "__main__":
    unittest.main()
