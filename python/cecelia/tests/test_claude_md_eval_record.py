"""Tests for `scripts/claude_md_eval/record.py` — eval run records (build, validate, store, render).

Design: docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md → *Run record (fields)*. Every test works on a
synthetic effectiveness log + trace dirs in a temp dir; nothing touches `~/.cecelia-effectiveness`.

Run with `pixi run test-py`.
"""
from __future__ import annotations

import copy
import importlib.util
import json
import os
import pathlib
import tempfile
import textwrap
import unittest
from unittest import mock

_REPO = pathlib.Path(__file__).resolve().parents[3]
_RECORD_PATH = _REPO / "scripts" / "claude_md_eval" / "record.py"


def _load_record():
    spec = importlib.util.spec_from_file_location("record", _RECORD_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _event(event: str, ts: str, payload: dict, *, session: str = "eval-1") -> dict:
    return {"event": event, "ts": ts, "session": session, "branch": "main", "commit": "blob1",
            "schema_version": 1, "source": "live", "payload": payload}


class _Fixture(unittest.TestCase):
    """One prompt, one full pass of 3 runs, plus rows that must NOT be part of it."""

    def setUp(self):
        self.rec = _load_record()
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.tmp = pathlib.Path(tmp.name)
        log = self.tmp / "events.jsonl"
        patcher = mock.patch.dict(os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(log)})
        patcher.start()
        self.addCleanup(patcher.stop)

        prompts = self.tmp / "prompts"
        prompts.mkdir()
        (prompts / "p1.md").write_text(textwrap.dedent("""\
            ---
            id: p1
            rule: R
            compliant_signal: 'GOOD'
            anti_signal: 'BAD'
            ---
            do it
            """), encoding="utf-8")
        p = mock.patch.object(self.rec._run_prompt, "_PROMPTS_DIR", prompts)
        p.start()
        self.addCleanup(p.stop)

        # r1 was logged noncompliant under an older scorer but passes today's; r2 really fails;
        # r3 errored and has nothing to rescore.
        runs = [("r1", "+GOOD", "noncompliant", None), ("r2", "+BAD", "noncompliant", None),
                ("r3", "", "error", "claude spawn timed out after 300s")]
        self.events = [
            _event("claude_md_eval_suite", "2026-09-29T10:00:00Z",
                   {"prompt_ids": ["p1"], "runs_per_prompt": 3, "full_catalog": True, "arm": "with",
                    "totals": {"cost_usd": 1.0}}),
            # same session, before the pass's window: an earlier pass or an ad-hoc run
            _event("claude_md_eval_run", "2026-09-30T09:58:00Z", {"prompt_id": "p1", "verdict": "compliant"}),
            # another session in the same window: a parallel ad-hoc run
            _event("claude_md_eval_run", "2026-09-30T10:00:30Z",
                   {"prompt_id": "p1", "verdict": "compliant"}, session="other"),
        ]
        for i, (name, diff, verdict, error) in enumerate(runs, 1):
            trace = self.tmp / "traces" / name
            trace.mkdir(parents=True)
            (trace / "diff.patch").write_text(diff + "\n", encoding="utf-8")
            init = {"type": "system", "subtype": "init", "claude_code_version": "9.9.9", "model": "m"}
            (trace / "stream.jsonl").write_text(json.dumps(init) + "\n", encoding="utf-8")
            (trace / "meta.json").write_text(json.dumps({"verdict": verdict, "error": error}),
                                             encoding="utf-8")
            payload = {"prompt_id": "p1", "verdict": verdict, "run_number": i, "arm": "with",
                       "cost_usd": 0.1, "turns": 3, "trace_dir": str(trace)}
            if error:
                payload["error"] = error
            self.events.append(_event("claude_md_eval_run", f"2026-09-30T10:00:0{i}Z", payload))
        self.events.append(_event("claude_md_eval_suite", "2026-09-30T10:01:00Z",
                                  {"prompt_ids": ["p1"], "runs_per_prompt": 3, "full_catalog": True,
                                   "arm": "with", "duration_s": 30, "totals": {"cost_usd": 0.3}}))

    def _finding(self, **over) -> dict:
        f = {"id": "F1", "slug": "p1", "class": "genuine", "status": "open", "recurrence": "watch",
             "evidence": [{"trace": "traces/r2", "excerpt": "+BAD"}],
             "diagnosis": "d", "proposed_fix": "x"}
        return f | over


class BuildTest(_Fixture):
    def test_replay_rescores_each_trace_and_keeps_errors(self):
        r = self.rec.build(self.events, "2026-09-30")
        self.assertEqual([t["scores"] for t in r["traces"]],
                         [{"raw": "noncompliant", "rescored": "compliant"},
                          {"raw": "noncompliant", "rescored": "noncompliant"},
                          {"raw": "error", "rescored": "error"}])
        self.assertEqual((r["results"]["raw"]["compliant"], r["results"]["rescored"]["compliant"]), (0, 1))
        self.assertEqual(r["run"]["claude_code_version"], "9.9.9")
        self.assertEqual(self.rec.validate(r), [])

    def test_pass_takes_only_its_own_session_inside_its_window(self):
        suite = self.rec.find_suite(self.events, "2026-09-30")
        runs = self.rec._rollup.pass_runs(self.events, suite)
        self.assertEqual([e["payload"]["run_number"] for e in runs], [1, 2, 3])

    def test_replay_without_a_sha_records_none_not_a_guess(self):
        # the log's `commit` is the CLAUDE.md blob; it must not be passed off as the repo SHA
        r = self.rec.build(self.events, "2026-09-30")
        self.assertIsNone(r["run"]["sha"])
        self.assertEqual(r["run"]["claude_md_blob"], "blob1")

    def test_no_pass_on_the_date_is_a_clear_error(self):
        with self.assertRaisesRegex(self.rec.RecordError, "no claude_md_eval_suite row on 2026-01-01"):
            self.rec.build(self.events, "2026-01-01")

    def test_decision_findings_and_proposals_go_on_the_owner_queue(self):
        notes = {"findings": [self._finding(), self._finding(id="F2", **{"class": "decision"}),
                              self._finding(id="F3", status="resolved", **{"class": "decision"})],
                 "proposals": [{"id": "P1", "kind": "setup", "summary": "s", "sources": []}]}
        r = self.rec.build(self.events, "2026-09-30", annotations=notes)
        self.assertEqual(r["queue"], [{"kind": "decision", "ref": "F2"}, {"kind": "proposal", "ref": "P1"}])

    def test_sandbox_hash_moves_with_the_settings(self):
        base = self.rec._run_prompt._SANDBOX_SETTINGS
        changed = copy.deepcopy(base)
        changed["sandbox"]["network"] = {}
        self.assertNotEqual(self.rec.sandbox_hash(base), self.rec.sandbox_hash(changed))
        self.assertEqual(self.rec.build(self.events, "2026-09-30", sandboxed=True)["run"]["sandbox"],
                         self.rec.sandbox_hash())


class ValidateTest(_Fixture):
    def test_bad_finding_fields_are_each_named(self):
        r = self.rec.build(self.events, "2026-09-30", annotations={"findings": [
            self._finding(**{"class": "bogus"}, evidence=[{"trace": "t"}])]})
        errs = self.rec.validate(r)
        self.assertTrue(any("class 'bogus'" in e for e in errs), errs)
        self.assertTrue(any("needs a `trace` and an `excerpt`" in e for e in errs), errs)

    def test_a_proposal_needs_its_fields_and_a_known_kind(self):
        r = self.rec.build(self.events, "2026-09-30", annotations={"proposals": [{"id": "P1", "kind": "apply"}]})
        errs = self.rec.validate(r)
        self.assertIn("proposal P1: missing `summary`", errs)
        self.assertTrue(any("kind 'apply'" in e for e in errs), errs)

    def test_queue_ref_must_name_a_finding_or_proposal(self):
        r = self.rec.build(self.events, "2026-09-30")
        r["queue"].append({"kind": "decision", "ref": "F9"})
        self.assertIn("queue: 'F9' names no finding, proposal or bug", self.rec.validate(r))


class DeltaTest(_Fixture):
    def _previous(self, **run):
        prev = self.rec.build(self.events, "2026-09-30", annotations={
            "findings": [self._finding(id="F1"), self._finding(id="F2", slug="gone")],
            "proposals": [{"id": "P1", "kind": "setup", "summary": "s", "sources": ["F1"],
                           "hypothesis": "fixes F1; expect p1 to pass"},
                          {"id": "P2", "kind": "setup", "summary": "s", "sources": [],
                           "hypothesis": "expect `absent` to pass"}]})
        prev["date"] = "2026-09-23"
        prev["run"].update(run)
        return prev

    def test_findings_match_on_slug_and_class(self):
        now = self.rec.build(self.events, "2026-09-30", annotations={"findings": [
            self._finding(id="F1"), self._finding(id="F2", **{"class": "decision"})]})
        d = self.rec.delta(now, self._previous())
        self.assertEqual((d["opened"], d["still_open"], d["resolved"]), (["F2"], ["F1"], ["F2 `gone` genuine"]))
        self.assertEqual(d["changed"], [])

    def test_a_class_change_is_reclassified_not_resolved_and_opened(self):
        now = self.rec.build(self.events, "2026-09-30", annotations={"findings": [
            self._finding(id="F1", **{"class": "scorer_bug"})]})
        d = self.rec.delta(now, self._previous())
        self.assertEqual((d["opened"], d["reclassified"], d["resolved"]),
                         ([], ["F1 → F1 `p1` genuine → scorer_bug"], ["F2 `gone` genuine"]))

    def test_each_hypothesis_is_checked_against_this_run(self):
        now = self.rec.build(self.events, "2026-09-30")
        d = self.rec.delta(now, self._previous())
        self.assertEqual([(h["proposal"], h["held"]) for h in d["hypotheses"]], [("P1", False), ("P2", None)])

    def test_a_version_change_is_named_next_to_the_score(self):
        now = self.rec.build(self.events, "2026-09-30", sandboxed=True)
        d = self.rec.delta(now, self._previous())
        self.assertEqual((d["changed"], d["confounded"]), (["sandbox"], []))
        self.assertIn("Only sandbox changed.", self.rec.render_markdown({**now, "delta": d}))

    def test_both_passes_are_scored_by_todays_scorer(self):
        # the earlier pass logged 0/3; its r1 trace passes today's scorer, as it does in this one
        d = self.rec.delta(self.rec.build(self.events, "2026-09-30"), self._previous())
        self.assertEqual(d["score"], {"previous": "1/3", "now": "1/3", "previous_logged": "0/3"})
        self.assertEqual(d["moves"], [])

    def test_a_lost_trace_names_the_scorer_as_changed(self):
        prev = self._previous()
        prev["traces"][0]["trace"] = str(self.tmp / "gone")
        self.assertEqual(self.rec.delta(self.rec.build(self.events, "2026-09-30"), prev)["changed"], ["scorer"])

    def test_several_changes_together_are_confounded_and_no_hypothesis_holds(self):
        now = self.rec.build(self.events, "2026-09-30", sandboxed=True)
        for t in now["traces"]:
            t["scores"]["rescored"] = "compliant"
        prev = self._previous(claude_md_blob="older", claude_code_version="1.0")
        d = self.rec.delta(now, prev)
        self.assertEqual(d["confounded"], ["sandbox", "CLAUDE.md"])   # Claude Code is named, not counted
        self.assertIn("Claude Code", d["changed"])
        md = self.rec.render_markdown({**now, "delta": d})
        self.assertIn("**Confounded: sandbox, CLAUDE.md.**", md)
        self.assertIn("**consistent, not proven (confounded)**", md)
        self.assertNotIn("**held**", md)

    def test_only_a_move_between_the_ends_is_a_change(self):
        now = self.rec.build(self.events, "2026-09-30")
        for t in now["traces"]:
            t["scores"]["rescored"] = "compliant"
        cases = {"0/3": "change", "1/3": "noisy", "3/3": None}
        for before, kind in cases.items():
            c = int(before[0])
            prev_now = ({"p1": {"compliant": c, "noncompliant": 3 - c, "error": 0, "total": 3}}, 0)
            moves = self.rec.delta(now, self._previous(), previous_now=prev_now)["moves"]
            self.assertEqual([m["kind"] for m in moves], [kind] if kind else [], before)

    def test_result_state_reads_errors_as_unscored(self):
        rs = self.rec.result_state
        self.assertEqual([rs({"compliant": 2, "error": 1, "total": 3}), rs({"compliant": 0, "total": 3}),
                          rs({"compliant": 1, "total": 3}), rs({"error": 3, "total": 3}), rs(None)],
                         ["pass", "fail", "noisy", None, None])

    def test_a_delta_stored_before_rescoring_still_renders_as_logged(self):
        now = self.rec.build(self.events, "2026-09-30")
        legacy = {"previous": "2026-09-23", "opened": [], "still_open": [], "resolved": [],
                  "score": {"previous": "3/27", "now": "24/27"}, "changed": ["sandbox"],
                  "setup_growth": [], "hypotheses": []}
        md = self.rec.render_markdown({**now, "delta": legacy})
        self.assertIn("3/27 → 24/27 as logged — **not comparable directly:** sandbox changed.", md)
        self.assertNotIn("Moves:", md)

    def test_with_delta_reads_the_previous_record_from_the_store(self):
        self.rec.write(self._previous())
        self.assertEqual(self.rec.with_delta(self.rec.build(self.events, "2026-09-30"))["delta"]["previous"],
                         "2026-09-23")
        self.assertIsNone(self.rec.delta(self.rec.build(self.events, "2026-09-30"), None))


class SetupBudgetTest(_Fixture):
    def test_over_budget_is_an_open_decision_for_the_owner(self):
        size = {"ref": "abc12345", "claude_md_lines": 500, "frontend_claude_md_lines": 10}
        self.assertEqual(self.rec.over_budget(size, {"claude_md_lines": 451, "frontend_claude_md_lines": 52}),
                         ["claude_md_lines 500/451"])
        with mock.patch.dict(self.rec.SETUP_BUDGET, {"claude_md_lines": 451}):
            f = self.rec.budget_finding(size, "F3", recurring=True)
            self.assertIsNone(self.rec.budget_finding({**size, "claude_md_lines": 451}, "F3"))
        self.assertEqual((f["id"], f["class"], f["status"], f["recurrence"]), ("F3", "decision", "open", "recurring"))
        r = self.rec.build(self.events, "2026-09-30", annotations={"findings": [f]})
        self.assertEqual(self.rec.validate(r), [])
        self.assertIn({"kind": "decision", "ref": "F3"}, r["queue"])

    def test_the_setup_size_section_shows_used_against_budget(self):
        md = self.rec.render_markdown(self.rec.build(self.events, "2026-09-30"))
        self.assertRegex(md, rf"CLAUDE\.md \d+/{self.rec.SETUP_BUDGET['claude_md_lines']} lines")


class SupervisorSpendTest(_Fixture):
    def test_the_total_is_split_into_judge_bug_sweep_and_curation(self):
        # the 2026-10-02 pass: no judge call, yet $0.87 spent on the bug sweep and curation
        sup = {"judge_calls": 0, "cost_usd": 0.8655, "bugs_usd": 0.4959, "curation_usd": 0.3696}
        self.assertEqual(self.rec.supervisor_spend(sup),
                         "$0.87 · judge 0 call(s) $0.00 · bug sweep $0.50 · curation $0.37")

    def test_the_supervisor_line_shows_how_curation_binned_the_findings(self):
        sup = {"judge_calls": 0, "cost_usd": 0.1, "finding_bins": {"agent_made": 59, "legacy": 77, "unknown": 8}}
        md = self.rec.render_markdown(self.rec.build(self.events, "2026-09-30", annotations={"supervisor": sup}))
        self.assertIn("findings 59 agent made · 77 legacy · 8 unknown", md)

    def test_an_older_record_without_the_parts_reads_as_all_judge(self):
        self.assertEqual(self.rec.supervisor_spend({"judge_calls": 1, "cost_usd": 0.1}),
                         "$0.10 · judge 1 call(s) $0.10 · bug sweep $0.00 · curation $0.00")


class StoreTest(_Fixture):
    def test_write_then_load_round_trips_and_mirrors_json_and_markdown(self):
        r = self.rec.build(self.events, "2026-09-30", annotations={"findings": [self._finding()]})
        mirror = self.tmp / "mirror"
        paths = self.rec.write(r, mirror=True, mirror_dir=mirror)
        self.assertEqual(paths[0], self.tmp / "eval-runs" / "2026-09-30.json")
        self.assertEqual(self.rec.load(paths[0]), r)
        self.assertEqual(json.loads((mirror / "2026-09-30.json").read_text(encoding="utf-8")), r)
        md = (mirror / "2026-09-30.md").read_text(encoding="utf-8")
        self.assertIn("Point a session at this file", md)
        self.assertIn("**0/3 as logged → 1/3 rescored**", md)
        self.assertIn("### F1 · `p1` · genuine · open · watch", md)

    def test_an_existing_record_is_not_replaced_without_force(self):
        r = self.rec.build(self.events, "2026-09-30")
        self.rec.write(r)
        with self.assertRaisesRegex(self.rec.RecordError, "pass --force"):
            self.rec.write(r)
        self.rec.write(r, force=True)

    def test_a_rescore_keeps_the_existing_records_annotations(self):
        r = self.rec.build(self.events, "2026-09-30", annotations={"findings": [self._finding()],
                                                                     "sha": "abc"})
        self.rec.write(r)
        notes = self.rec.carried_annotations(self.tmp / "eval-runs" / "2026-09-30.json")
        again = self.rec.build(self.events, "2026-09-30", annotations=notes)
        self.assertEqual((again["findings"], again["run"]["sha"]), (r["findings"], "abc"))
        self.assertIsNone(self.rec.carried_annotations(self.tmp / "eval-runs" / "nope.json"))

    def test_an_invalid_record_is_never_written(self):
        r = self.rec.build(self.events, "2026-09-30", annotations={"findings": [self._finding(status="?")]})
        with self.assertRaises(self.rec.RecordError):
            self.rec.write(r)
        self.assertFalse((self.tmp / "eval-runs" / "2026-09-30.json").exists())

    def test_missing_or_other_schema_record_is_a_clear_error(self):
        with self.assertRaisesRegex(self.rec.RecordError, "no run record at"):
            self.rec.load(self.tmp / "nope.json")
        old = self.tmp / "old.json"
        old.write_text(json.dumps({"schema_version": 0}), encoding="utf-8")
        with self.assertRaisesRegex(self.rec.RecordError, "schema_version 0, this code reads 1"):
            self.rec.load(old)


if __name__ == "__main__":
    unittest.main()
