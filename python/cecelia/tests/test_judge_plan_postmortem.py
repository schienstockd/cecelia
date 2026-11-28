"""Tests for `scripts/judge/plan_postmortem.py`: what later plan edits say about the plan review.

Design: docs/ai-assist/WEEKLY_JUDGE.md → *Plan post-mortem*. The judge is injected; `plan_edits` runs
on a throwaway git repo with a real merge, so the log parsing is held to git's own output. Nothing
spawns `claude`.
"""
from __future__ import annotations

import importlib.util
import os
import pathlib
import subprocess
import tempfile
import unittest

_REPO = pathlib.Path(__file__).resolve().parents[3]


def _load(name: str):
    spec = importlib.util.spec_from_file_location(name, _REPO / "scripts" / "judge" / f"{name}.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


#: Built, not written out: the doc-pointer convention test reads `docs/…/X.md` in code as a real pointer.
_TODO = "docs/" + "todo/"
_A, _B, _C = _TODO + "A_PLAN.md", _TODO + "B_PLAN.md", _TODO + "C_PLAN.md"


def _run_row(plan: str, ts: str) -> dict:
    return {"event": "plan_review_run", "ts": ts, "payload": {"plans": [plan]}}


def _finding(slug: str, plan: str, ts: str, marker: str = "design flaw") -> dict:
    return {"event": "plan_review_finding", "ts": ts,
            "payload": {"slug": slug, "file": plan, "line": 10, "marker": marker, "desc": f"desc {slug}"}}


def _resolved(slug: str, outcome: str, ts: str, reason: str = "") -> dict:
    return {"event": "plan_review_finding_resolved", "ts": ts,
            "payload": {"slug": slug, "outcome": outcome, **({"reason": reason} if reason else {})}}


class PlanEditsTest(unittest.TestCase):
    """Against real git: a merge that modifies a plan is one change; creating a plan is not an edit."""

    def setUp(self):
        self.pm = _load("plan_postmortem")
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.root = pathlib.Path(self._tmp.name)
        env = {**os.environ, "GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@t", "GIT_COMMITTER_NAME": "t",
               "GIT_COMMITTER_EMAIL": "t@t", "GIT_CONFIG_GLOBAL": os.devnull, "GIT_CONFIG_SYSTEM": os.devnull}

        def git(*args, date="2026-11-01T00:00:00+00:00"):
            subprocess.run(["git", *args], cwd=self.root, check=True, capture_output=True,
                           env={**env, "GIT_AUTHOR_DATE": date, "GIT_COMMITTER_DATE": date})

        def write(rel, text):
            (self.root / rel).parent.mkdir(parents=True, exist_ok=True)
            (self.root / rel).write_text(text, encoding="utf-8")

        git("init", "-q", "-b", "main")
        write(_A, "# A\nDecision 1: old\n")
        git("add", ".")
        git("commit", "-q", "-m", "add A")                           # creates A: not an edit
        git("checkout", "-q", "-b", "feat")
        write(_A, "# A\nDecision 1: new\n")
        write(_B, "# B\n")
        git("add", ".")
        git("commit", "-q", "-m", "fix A, add B", date="2026-11-10T00:00:00+00:00")
        git("checkout", "-q", "main")
        git("merge", "-q", "--no-ff", "feat", "-m", "Merge feat", date="2026-11-11T00:00:00+00:00")
        self.git = self.pm._git_in(self.root)
        self.head = self.git("rev-parse", "HEAD")

    def test_a_merge_that_modifies_a_plan_is_one_edit_and_a_new_plan_is_none(self):
        edits = self.pm.plan_edits(self.git, self.head, since="2026-01-01")
        self.assertEqual([(e["sha"], e["plan"]) for e in edits], [(self.head, _A)])

    def test_a_judged_change_is_not_judged_again(self):
        self.assertEqual(self.pm.plan_edits(self.git, self.head, since="2026-01-01", judged={self.head}), [])

    def test_the_diff_is_against_the_first_parent(self):
        diff = self.git("diff", f"{self.head}^1", self.head, "--", _A)
        self.assertIn("+Decision 1: new", diff)


class JudgeEditsTest(unittest.TestCase):
    def setUp(self):
        self.pm = _load("plan_postmortem")
        self.edit = {"sha": "c" * 40, "ts": "2026-11-20T00:00:00Z", "plan": _A}
        self.git = lambda *a: "@@ -1 +1 @@\n-Decision 1: old\n+Decision 1: new\n"
        self.prompts: list[str] = []

    def judge(self, answer):
        def call(prompt):
            self.prompts.append(prompt)
            return answer, 0.25, {"input": 1, "output": 2}
        return call

    def test_an_edit_with_no_earlier_review_on_its_plan_is_left_out_not_recorded(self):
        later_review = [_run_row(_A, "2026-11-25T00:00:00Z"), _run_row(_B, "2026-11-01T00:00:00Z")]
        self.assertEqual(self.pm.reviewed_edits([self.edit], []), [])
        self.assertEqual(self.pm.reviewed_edits([self.edit], later_review), [])
        self.assertEqual(self.pm.reviewed_edits([self.edit], [_run_row(_A, "2026-11-01T00:00:00Z")]), [self.edit])

    def test_only_a_tagged_through_finding_it_knows_can_be_vindicated(self):
        events = [_run_row(_A, "2026-11-01T00:00:00Z"),
                  _finding("plan-11111111", _A, "2026-11-01T00:00:00Z"),
                  _finding("plan-22222222", _A, "2026-11-01T00:00:00Z"),
                  _resolved("plan-11111111", "false_positive", "2026-11-02T00:00:00Z", "reviewer misread"),
                  _resolved("plan-22222222", "fixed_pre_commit", "2026-11-02T00:00:00Z")]
        answer = {"vindicated": [{"slug": "plan-11111111", "why": "decision rewritten"},
                                 {"slug": "plan-22222222", "why": "already fixed"},
                                 {"slug": "plan-99999999", "why": "made up"}],
                  "misses": [{"marker": "wrong claim", "what": "default stated as invariant", "lesson": "check json"},
                             {"marker": "typo", "what": "not a marker", "lesson": "-"}]}
        meter: dict = {}
        cases, judged, cost = self.pm.judge_edits([self.edit], events, git=self.git, judge=self.judge(answer),
                                                  meter=meter)
        self.assertEqual([(c["kind"], c.get("slug") or c["marker"]) for c in cases],
                         [("vindicated", "plan-11111111"), ("miss", "wrong claim")])
        self.assertIn("tagged false_positive", self.prompts[0])
        self.assertIn("+Decision 1: new", self.prompts[0])
        self.assertEqual((judged, cost), ([self.edit["sha"]], 0.25))

    def test_a_failed_call_stops_and_leaves_the_edit_for_the_next_pass(self):
        events = [_run_row(_A, "2026-11-01T00:00:00Z")]

        def broken(prompt):
            raise self.pm._judge.JudgeError("boom")
        failures: dict = {}
        cases, judged, _ = self.pm.judge_edits([self.edit], events, git=self.git, judge=broken, failures=failures)
        self.assertEqual((cases, judged), ([], []))
        self.assertIn("boom", failures["plan_postmortem"])


class ProposalsTest(unittest.TestCase):
    def setUp(self):
        self.pm = _load("plan_postmortem")

    def _miss(self, plan, ts="2026-11-20T00:00:00Z", marker="already exists"):
        return {"kind": "miss", "plan": plan, "marker": marker, "sha": "d" * 40, "ts": ts, "what": "w", "lesson": "l"}

    def test_a_pattern_needs_two_plans(self):
        self.assertEqual(self.pm.proposals_from([self._miss(_A), self._miss(_A)], cut={}), [])
        props = self.pm.proposals_from([self._miss(_A), self._miss(_B)], cut={})
        self.assertEqual([(p["id"], p["kind"]) for p in props], [("M1", "plan_miss")])
        self.assertIn("(already exists)", props[0]["rule"])
        self.assertEqual(len(props[0]["cases"]), 2)

    def test_cases_before_the_last_edit_or_answer_do_not_count(self):
        target = self.pm.target("plan_miss", "already exists")
        cases = [self._miss(_A, ts="2026-11-01T00:00:00Z"), self._miss(_B, ts="2026-11-20T00:00:00Z")]
        self.assertEqual(self.pm.proposals_from(cases, cut={target: "2026-11-10T00:00:00Z"}), [])

    def test_noise_needs_three_findings_as_well(self):
        noise = [{"kind": "noise", "plan": p, "marker": "design flaw", "slug": f"plan-{i}", "ts": "2026-11-20T00:00:00Z",
                  "what": "fp"} for i, p in enumerate((_A, _B))]
        self.assertEqual(self.pm.proposals_from(noise, cut={}), [])
        noise.append({**noise[0], "plan": _C, "slug": "plan-9"})
        self.assertEqual([p["kind"] for p in self.pm.proposals_from(noise, cut={})], ["plan_noise"])

    def test_vindicated_cases_propose_an_author_convention(self):
        cases = [{"kind": "vindicated", "plan": p, "marker": m, "slug": s, "sha": "e" * 40,
                  "ts": "2026-11-20T00:00:00Z", "what": "w"}
                 for p, m, s in ((_A, "design flaw", "plan-1"), (_B, "wrong claim", "plan-2"))]
        props = self.pm.proposals_from(cases, cut={})
        self.assertEqual([(p["kind"], p["rule"]) for p in props],
                         [("plan_dismissed", self.pm.target("plan_dismissed", None))])

    def test_noise_skips_a_finding_a_later_edit_vindicated(self):
        events = [_finding("plan-1", _A, "2026-11-01T00:00:00Z"),
                  _resolved("plan-1", "false_positive", "2026-11-02T00:00:00Z", "no")]
        self.assertEqual(len(self.pm.noise_cases(events, vindicated=set())), 1)
        self.assertEqual(self.pm.noise_cases(events, vindicated={"plan-1"}), [])


class RunTest(unittest.TestCase):
    def test_earlier_cases_count_and_earlier_changes_are_not_judged_again(self):
        pm = _load("plan_postmortem")
        old = {"kind": "miss", "plan": _B, "marker": "design flaw", "sha": "a" * 40, "ts": "2026-11-05T00:00:00Z",
               "what": "w", "lesson": "l"}
        history = [{"plan_postmortem": {"judged": ["a" * 40], "cases": [old]}}]
        log = "@" + "a" * 40 + "\t2026-11-05T00:00:00Z\nM\t" + _B + "\n@" + "b" * 40 + "\t2026-11-20T00:00:00Z\nM\t" + _A

        def git(*args):
            if args[0] == "log" and "--name-status" in args:
                return log
            if args[0] == "diff":
                return "@@\n-x\n+y\n"
            return ""   # no doc edits: nothing cuts the window
        events = [_run_row(_A, "2026-11-01T00:00:00Z")]
        answer = {"vindicated": [], "misses": [{"marker": "design flaw", "what": "w2", "lesson": "l2"}]}
        prompts = []
        section, props, cost = pm.run(events, date="2026-11-28", sha="HEAD", history=history, git=git,
                                      judge=lambda p: (prompts.append(p), (answer, 0.5, {}))[1])
        self.assertEqual(len(prompts), 1)                       # the B change was judged last pass
        self.assertEqual(section["judged"], ["b" * 40])
        self.assertEqual([p["kind"] for p in props], ["plan_miss"])   # A this pass + B from history
        self.assertEqual(cost, 0.5)


class KindsTest(unittest.TestCase):
    def test_the_record_accepts_exactly_the_rule_and_plan_kinds(self):
        record_mod, pm = _load("record"), _load("plan_postmortem")
        self.assertEqual(set(record_mod.PROPOSAL_KINDS), set(record_mod.RULE_KINDS) | set(pm.KINDS))

    def test_a_plan_proposal_is_not_counted_as_a_rule_proposal(self):
        record_mod = _load("record")
        rec = {"proposals": [{"id": "P1", "kind": "tighten"}, {"id": "M1", "kind": "plan_miss"}]}
        self.assertEqual([p["id"] for p in record_mod.rule_proposals(rec)], ["P1"])
        self.assertEqual([p["id"] for p in record_mod.plan_proposals(rec)], ["M1"])


class RecordAndReviewTest(unittest.TestCase):
    def test_a_plan_proposal_validates_renders_and_briefs_its_cases(self):
        record_mod, review_mod, pm = _load("record"), _load("review"), _load("plan_postmortem")
        case = {"kind": "miss", "plan": _A, "marker": "already exists", "sha": "f" * 40,
                "ts": "2026-11-20T00:00:00Z", "what": "dwell metric was in another plan", "lesson": "grep synonyms"}
        props = pm.proposals_from([case, {**case, "plan": _B}], cut={})
        rec = record_mod.build("2026-11-28", ts="2026-11-28T00:00:00Z", sha="0" * 40, bugs=[], rules=[],
                               proposals=props, spend={"total_usd": 0.0},
                               plan_postmortem={"edits": 2, "judged": ["f" * 40], "cases": [case], "noise": 0})
        self.assertEqual(record_mod.validate(rec), [])
        md = record_mod.render_markdown(rec)
        self.assertIn("## Plan post-mortem", md)
        self.assertIn("dwell metric was in another plan", md)
        self.assertNotIn("M1", md.split("## Plan post-mortem")[0])   # the rules section doesn't list it
        brief = review_mod.rule_brief(rec, props[0], [])
        self.assertIn("The cases behind it (2)", brief)
        self.assertIn("grep synonyms", brief)
        card = "\n".join(review_mod.describe_rule(rec, props[0], [], use_colour=False))
        self.assertIn("Cases (2)", card)


if __name__ == "__main__":
    unittest.main()
