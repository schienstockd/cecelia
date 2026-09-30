"""Tests for `scripts/claude_md_eval/analyse.py` — the diagnostic read of the eval log.

Covers the streak verdicts across full WITH-arm passes and the trace lookup that points a
failing verdict at the runs to read.
"""
from __future__ import annotations

import importlib.util
import pathlib
import unittest

_REPO = pathlib.Path(__file__).resolve().parents[3]


def _load():
    spec = importlib.util.spec_from_file_location(
        "claude_md_eval_analyse", _REPO / "scripts" / "claude_md_eval" / "analyse.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _suite(ts: str, per_prompt: dict, *, arm: str = "with", full: bool = True,
           session: str = "s") -> dict:
    return {"event": "claude_md_eval_suite", "ts": ts, "session": session,
            "payload": {"prompt_ids": sorted(per_prompt), "full_catalog": full, "arm": arm,
                        "runs_per_prompt": 3, "per_prompt": per_prompt, "totals": {},
                        "duration_s": 600}}


def _run(pid: str, verdict: str, trace: str, *, session: str = "s",
         ts: str = "2026-09-15T00:00:00Z") -> dict:
    return {"event": "claude_md_eval_run", "ts": ts, "session": session,
            "payload": {"prompt_id": pid, "verdict": verdict, "trace_dir": trace}}


OK = {"compliant": 3}
BAD = {"noncompliant": 3}
MIXED = {"compliant": 2, "noncompliant": 1}


class PromptVerdictsTest(unittest.TestCase):
    def setUp(self):
        self.mod = _load()

    def _verdicts(self, events, catalog):
        return {r["prompt_id"]: r for r in self.mod.prompt_verdicts(events, catalog)}

    def test_streak_of_three_clean_passes_is_closed_two_is_passing(self):
        events = [_suite("2026-09-01T00:00:00Z", {"a": OK}), _suite("2026-09-08T00:00:00Z", {"a": OK, "b": OK}),
                  _suite("2026-09-15T00:00:00Z", {"a": OK, "b": OK})]
        v = self._verdicts(events, ["a", "b"])
        self.assertEqual(v["a"]["verdict"], "closed")
        self.assertEqual(v["b"]["verdict"], "passing")

    def test_canary_never_closes(self):
        events = [_suite(f"2026-09-0{i}", {"canary": OK}) for i in range(1, 5)]
        self.assertEqual(self._verdicts(events, ["canary"])["canary"]["verdict"], "passing")

    def test_failing_drift_errored_new(self):
        events = [_suite("2026-09-15", {"a": BAD, "b": MIXED, "c": {"error": 3}})]
        v = self._verdicts(events, ["a", "b", "c", "d"])
        self.assertEqual([v[k]["verdict"] for k in "abcd"],
                         ["failing", "drift", "errored", "new"])

    def test_partial_and_without_rows_are_not_history(self):
        events = [_suite("2026-09-15", {"a": BAD}),
                  _suite("2026-09-16", {"a": OK}, full=False),
                  _suite("2026-09-17", {"a": OK}, arm="without")]
        v = self._verdicts(events, ["a"])
        self.assertEqual(v["a"]["verdict"], "failing")
        self.assertEqual(v["a"]["history"], [(0, 3, 0)])

    def test_traces_are_latest_pass_non_compliant_runs_only(self):
        # Same session on both passes (launched from one interactive Claude Code session):
        # the duration window still keeps the older pass's traces out.
        events = [_suite("2026-09-08T00:10:00Z", {"a": BAD}),
                  _run("a", "noncompliant", "/t/old", ts="2026-09-08T00:05:00Z"),
                  _run("a", "compliant", "/t/ok", ts="2026-09-15T00:05:00Z"),
                  _run("a", "noncompliant", "/t/bad", ts="2026-09-15T00:06:00Z"),
                  _run("a", "noncompliant", "/t/other-session", ts="2026-09-15T00:06:00Z",
                       session="x"),
                  _suite("2026-09-15T00:10:00Z", {"a": MIXED})]
        self.assertEqual(self._verdicts(events, ["a"])["a"]["traces"], ["/t/bad"])


if __name__ == "__main__":
    unittest.main()
