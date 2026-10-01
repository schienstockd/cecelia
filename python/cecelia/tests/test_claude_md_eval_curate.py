"""Tests for `scripts/claude_md_eval/curate.py` — curation proposals (retire / add / setup).

Design: docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md → Decisions 1, 8, 12 and phase 5. Records are
minimal synthetic dicts; the finding→rule judge is injected. Nothing spawns `claude`.

Run with `pixi run test-py`.
"""
from __future__ import annotations

import importlib.util
import pathlib
import tempfile
import unittest

_REPO = pathlib.Path(__file__).resolve().parents[3]
_CURATE_PATH = _REPO / "scripts" / "claude_md_eval" / "curate.py"


def _load_curate():
    spec = importlib.util.spec_from_file_location("curate", _CURATE_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _rec(date, scores, *, prompt_hash="h1", sandbox="s1", retries=(), findings=(), cost=1.0):
    return {
        "date": date, "kind": "pass",
        "run": {"prompt_set": {"hash": prompt_hash}, "sandbox": sandbox, "full_catalog": True,
                "retries": list(retries)},
        "results": {"per_prompt": {pid: {"raw": {"compliant": c, "total": 3}} for pid, c in scores.items()}},
        "traces": [{"prompt_id": pid, "cost_usd": cost} for pid in scores],
        "findings": list(findings),
    }


def _finding_event(slug, ts="2026-10-01T00:00:00Z"):
    return {"event": "convention_check_finding", "ts": ts,
            "payload": {"slug": slug, "file": "a.py", "desc": f"desc {slug}", "marker": "should reuse"}}


class RetireTest(unittest.TestCase):
    def setUp(self):
        self.c = _load_curate()

    def _history(self, **over):
        return [_rec("2026-09-16", {"p1": 3, "canary": 3}), _rec("2026-09-23", {"p1": 3, "canary": 3}, **over)]

    def test_three_green_comparable_passes_propose_a_retire(self):
        out = self.c.retire_proposals(_rec("2026-09-30", {"p1": 3, "canary": 3}), self._history())
        self.assertEqual([(p["prompt"], p["sources"]) for p in out],
                         [("p1", ["2026-09-30", "2026-09-23", "2026-09-16"])])   # canary never

    def test_an_infra_retry_in_the_window_blocks_it(self):
        out = self.c.retire_proposals(_rec("2026-09-30", {"p1": 3}),
                                      self._history(retries=[{"prompt_id": "p1", "attempt": 1}]))
        self.assertEqual(out, [])

    def test_a_sandbox_or_prompt_set_change_ends_the_streak(self):
        for over in ({"sandbox": "s0"}, {"prompt_hash": "h0"}):
            self.assertEqual(self.c.retire_proposals(_rec("2026-09-30", {"p1": 3}), self._history(**over)), [], over)

    def test_one_red_pass_in_the_window_blocks_it(self):
        history = [_rec("2026-09-16", {"p1": 3}), _rec("2026-09-23", {"p1": 2})]
        self.assertEqual(self.c.retire_proposals(_rec("2026-09-30", {"p1": 3}), history), [])


class AddTest(unittest.TestCase):
    def setUp(self):
        self.c = _load_curate()
        self.rules = ["CLAUDE.md → *Testing*", "CLAUDE.md → *Windows compatibility*"]
        self.prompts = [{"id": "dir-size", "rule": "r", "rule_section": "CLAUDE.md → *Windows compatibility*"}]
        self.prompt_seen = []

    def _assign(self, mapping):
        def fn(prompt):
            self.prompt_seen.append(prompt)
            return {"assignments": [{"slug": s, "rule": r, "covered_by": c} for s, (r, c) in mapping.items()]}, 0.2
        return fn

    def _findings(self, *slugs):
        return [_finding_event(s) for s in slugs]

    def test_three_uncovered_findings_on_one_rule_propose_an_add_citing_them(self):
        out, cost = self.c.add_proposals(self._findings("a", "b", "c", "d"), rule_list=self.rules,
                                         prompts=self.prompts, assign=self._assign({
                                             "a": (self.rules[0], ""), "b": (self.rules[0], ""),
                                             "c": (self.rules[0], ""), "d": (self.rules[1], "dir-size")}))
        self.assertEqual([(p["rule"], p["sources"]) for p in out], [(self.rules[0], ["a", "b", "c"])])
        self.assertEqual(cost, 0.2)
        self.assertIn("desc a", self.prompt_seen[0])

    def test_a_covered_rule_or_an_invented_one_is_never_proposed(self):
        out, _ = self.c.add_proposals(self._findings("a", "b", "c"), rule_list=self.rules, prompts=self.prompts,
                                      assign=self._assign({"a": (self.rules[1], "dir-size"),
                                                           "b": ("NOT_A_RULE.md → *Invented*", ""),
                                                           "c": (self.rules[1], "dir-size")}))
        self.assertEqual(out, [])

    def test_fewer_than_three_findings_skip_the_judge(self):
        out, cost = self.c.add_proposals(self._findings("a", "b"), rule_list=self.rules, prompts=self.prompts,
                                         assign=self._assign({}))
        self.assertEqual((out, cost, self.prompt_seen), ([], 0.0, []))

    def test_red_team_and_old_findings_are_not_counted(self):
        events = [_finding_event("fanout-c8482224"), _finding_event("x", ts="2026-08-01T00:00:00Z"),
                  _finding_event("y")]
        kept = self.c.recent_findings(events, today=self.c._dt.date(2026, 10, 1))
        self.assertEqual([e["payload"]["slug"] for e in kept], ["y"])

    def test_an_add_over_the_cap_pairs_with_a_removal(self):
        adds = [{"kind": "add", "rule": "r", "sources": [], "summary": "Add"}]
        current = _rec("2026-09-30", {"p1": 3, "p2": 3}, cost=10.0)    # $20 already
        out = self.c.pair_with_removals(adds, [{"prompt": "p2"}], current, [])
        self.assertEqual(out[0]["paired_removal"], "p2")
        fresh = [{"kind": "add", "rule": "r", "sources": [], "summary": "Add"}]
        under = self.c.pair_with_removals(fresh, [], _rec("2026-09-30", {"p1": 3}, cost=1.0), [])
        self.assertNotIn("paired_removal", under[0])

    def test_rules_are_the_claude_md_sections_in_rule_section_form(self):
        with tempfile.TemporaryDirectory() as d:
            (pathlib.Path(d) / "CLAUDE.md").write_text("# T\n## Testing\nx\n## Git & commits\n", encoding="utf-8")
            self.assertEqual(self.c.rules(pathlib.Path(d)), ["CLAUDE.md → *Testing*", "CLAUDE.md → *Git & commits*"])


class FindingProposalTest(unittest.TestCase):
    def test_only_recurring_genuine_and_scorer_findings_and_the_delta_reads_the_hypothesis(self):
        c = _load_curate()
        findings = [
            {"id": "F1", "slug": "p1", "class": "genuine", "status": "open", "recurrence": "recurring",
             "title": "t1", "proposed_fix": "x"},
            {"id": "F2", "slug": "p2", "class": "genuine", "status": "open", "recurrence": "watch",
             "title": "t2", "proposed_fix": "x"},
            {"id": "F3", "slug": "p3", "class": "decision", "status": "open", "recurrence": "recurring",
             "title": "t3", "proposed_fix": "x"},
        ]
        out = c.finding_proposals(_rec("2026-09-30", {}, findings=findings))
        self.assertEqual([(p["kind"], p["sources"]) for p in out], [("setup", ["F1"])])
        self.assertEqual(c._record._HYPOTHESIS_RE.search(out[0]["hypothesis"]).group(1), "p1")


if __name__ == "__main__":
    unittest.main()
