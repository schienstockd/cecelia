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
        self.assertIn("queue: 'F9' names no finding or proposal", self.rec.validate(r))


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
        self.assertEqual(d["changed"], ["sandbox"])
        self.assertIn("not comparable directly:** sandbox changed", self.rec.render_markdown({**now, "delta": d}))

    def test_with_delta_reads_the_previous_record_from_the_store(self):
        self.rec.write(self._previous())
        self.assertEqual(self.rec.with_delta(self.rec.build(self.events, "2026-09-30"))["delta"]["previous"],
                         "2026-09-23")
        self.assertIsNone(self.rec.delta(self.rec.build(self.events, "2026-09-30"), None))


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
