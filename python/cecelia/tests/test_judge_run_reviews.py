"""Tests for `scripts/judge/run_reviews.py` — a person's causes on agent run records reach the judge.

Design: docs/todo/GUIDE_RUNS_PLAN.md P2, docs/ai-assist/WEEKLY_JUDGE.md. The run records are the
committed fixture `test-data/agent-runs/projects` (never the real projects dir); the log and the
record store are temp files. Nothing spawns `claude` or `gh`.

Run with `pixi run test-py`.
"""
from __future__ import annotations

import importlib.util
import os
import pathlib
import tempfile
import unittest
from unittest import mock

from cecelia.effectiveness import read_events
from cecelia.tests.test_judge_record import _Fixture
from cecelia.tests.test_judge_weekly import _WeeklyFixture

_REPO = pathlib.Path(__file__).resolve().parents[3]
_JUDGE = _REPO / "scripts" / "judge"
PROJECTS = _REPO / "test-data" / "agent-runs" / "projects"
RUN_A = "bb-20261006T030000-aaaaaa"
PASS_TS = "2026-10-12T22:59:00Z"


def _load(name):
    spec = importlib.util.spec_from_file_location(name, _JUDGE / f"{name}.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class ScanTest(unittest.TestCase):
    def setUp(self):
        self.r = _load("run_reviews")
        self.found = self.r.scan(PROJECTS)
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.log = pathlib.Path(tmp.name) / "events.jsonl"

    def test_only_a_person_s_bad_with_a_cause_on_a_run_record(self):
        got = {(v["entry"][-6:], v["section"], v["cause"]) for v in self.found}
        # not: a Claude proposal (a/d02), a bad from before causes (a/d05), a good (a/d04), a note's s01
        self.assertEqual(got, {("aaaaaa", "d01", "guide"), ("aaaaaa", "d03", "agent"), ("aaaaaa", "m01", "platform"),
                               ("bbbbbb", "d02", "agent"), ("cccccc", "d01", "agent")})
        d01 = next(v for v in self.found if v["entry"] == RUN_A and v["section"] == "d01")
        self.assertEqual((d01["project"], d01["guide"], d01["run"]), ("RvwPrj", "intravital", "20261006T015831"))
        self.assertEqual(d01["heading"], "segment · yDfwP7 · cellpose on merged channels")

    def test_a_missing_projects_dir_is_no_records(self):
        self.assertEqual(self.r.scan(PROJECTS / "nope"), [])

    def test_guide_and_platform_are_logged_once_across_passes(self):
        log = self.log
        rows = self.r.log_new(self.found, [], pass_ts=PASS_TS, log_path=log)
        self.assertEqual(sorted(r["payload"]["section"] for r in rows), ["d01", "m01"])
        p = next(r["payload"] for r in rows if r["payload"]["section"] == "d01")
        self.assertEqual((p["kind"], p["cause"], p["guide"], p["entry"], p["run"]),
                         ("review", "guide", "intravital", RUN_A, "20261006T015831"))
        self.assertIn("the guide never asks for AF correction", p["desc"])
        self.assertTrue(p["key"].startswith("rev-") and p["file"] is None)
        self.assertEqual(rows[0]["ts"], "2026-10-12T22:58:59Z")   # before the pass: not re-read by the next
        logged = list(read_events(log))
        self.assertEqual(len(logged), 2)
        # the next pass: the same verdicts are already in the log
        self.assertEqual(self.r.log_new(self.found, logged, pass_ts=PASS_TS, log_path=log), [])
        self.assertEqual(len(list(read_events(log))), 2)

    def test_a_dry_run_builds_the_rows_and_writes_nothing(self):
        log = self.log
        rows = self.r.log_new(self.found, [], pass_ts=PASS_TS, write=False, log_path=log)
        self.assertEqual(len(rows), 2)
        self.assertFalse(log.exists())

    def test_agent_causes_are_listed_per_guide_and_a_two_run_guide_is_flagged(self):
        causes = {c["guide"]: c for c in self.r.agent_causes(self.found)}
        self.assertEqual(set(causes), {"intravital", None})
        self.assertEqual((causes["intravital"]["runs"], causes["intravital"]["gap"]), (2, True))
        self.assertEqual(sorted(n["note"] for n in causes["intravital"]["notes"]),
                         ["no QC gate before tracking", "tracked before gating out debris"])
        self.assertEqual((causes[None]["runs"], causes[None]["gap"]), (1, False))


class ToTheQueueTest(_Fixture):
    """The logged rows → the sweep's bugs → the record and `judge-review`'s work list."""

    def setUp(self):
        super().setUp()
        self.r, self.b, self.v, self.rv = _load("run_reviews"), _load("bugs"), _load("verify"), _load("review")
        self.rows = self.r.log_new(self.r.scan(PROJECTS), [], pass_ts=PASS_TS, write=False)

    def sweep(self):
        bugs, _ = self.b.sweep(self.rows, date="2026-10-12", sha="a" * 40, previous=None, no_judge=True,
                               merged_prs=lambda since: [])
        return bugs

    def test_each_is_an_open_bug_that_skips_the_judge(self):
        bugs = self.sweep()
        self.assertEqual(len(bugs), 2)
        for b in bugs:
            self.assertEqual((b["status"], b["kind"], b["marker"], b.get("review")), ("open", "agent_run", "run review", True))
            self.assertIn("verify checks whether the fix is in", b["why"])
        self.assertEqual({self.rec.bug_location(b) for b in bugs},
                         {"run review · guide · d01", "run review · platform · m01"})

    def test_they_go_to_one_verify_agent_per_run_with_the_review_brief(self):
        groups = self.v.groups(self.v.eligible(self.sweep()))
        self.assertEqual(len(groups), 1)   # both from run A
        prompt = self.v.prompt_for(groups[0])
        self.assertIn("A BUG marked `run review`", prompt)
        self.assertIn("cause guide, run 20261006T015831, guide intravital", prompt)

    def test_they_show_in_the_record_and_on_the_judge_review_work_list(self):
        record = self.build("2026-10-12", bugs=self.sweep())
        record["agent_causes"] = self.r.agent_causes(self.r.scan(PROJECTS))
        self.assertEqual(self.rec.validate(record), [])
        self.assertEqual(len(self.rv.work_items(record)), 2)
        self.assertEqual(len(self.rv.pending(record, [])), 2)
        card = "\n".join(self.rv.describe(record, self.rv.work_items(record)[0], use_colour=False))
        self.assertIn("run review", card)
        md = self.rec.render_markdown(record)
        self.assertIn("**Run review** (cause `guide`", md)
        self.assertIn("## Agent causes per guide", md)
        self.assertIn("### `intravital` — 2 run(s) · possible guide gap", md)


class WeeklyPassTest(_WeeklyFixture):
    def test_a_pass_logs_new_reviews_before_the_sweep_and_lists_agent_causes(self):
        with mock.patch.dict(os.environ, {"CECELIA_AGENT_APP_PROJECTS": str(PROJECTS)}):
            record = self.run_pass()
            self.assertEqual(sum(r["event"] == "agent_run_finding" for r in read_events()), 2)
            self.run_pass(date="2026-10-12")   # a second pass logs nothing new
            self.assertEqual(sum(r["event"] == "agent_run_finding" for r in read_events()), 2)
        self.assertEqual([c["guide"] for c in record["agent_causes"]], ["intravital", None])


if __name__ == "__main__":
    unittest.main()
