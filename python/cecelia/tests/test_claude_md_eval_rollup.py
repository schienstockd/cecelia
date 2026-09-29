"""Tests for the CLAUDE.md compliance-eval rollup.

Design: `docs/todo/CLAUDE_MD_EVAL_PLAN.md` → *Rollup* (P2.5). `render_eval_rollup(events)`
takes an iterable of `claude_md_eval_*` rows and produces the markdown that lands at
`docs/ai-assist/CLAUDE_MD_EVAL.md`. Renderer is pure — tests build the row lists inline
and assert on the returned string.

Invariants pinned:

- **No suite rows → no-data page** (not a crash). A rollup called on a fresh log renders
  a page that says "no data yet" and points to the eval command.
- **Rule text is picked up from the latest `_run` row per prompt.** A prompt rewording
  lands here on the next pass without the rollup reading the catalog directly.
- **Ablation section only when an ablation row exists.** A suite-only pass shouldn't
  render an empty ablation table.
- **Failing rules section only when the latest pass has <100%-compliant prompts.** A
  clean pass shouldn't render an empty section.
- **Trend section only when ≥2 passes have been logged.** A single-row trend restates
  the latest-suite table; suppress until it's meaningful.
- **Legacy rows without `arm` / `cost_usd` render cleanly.** Rows written before
  2026-09-28 don't carry those fields — the rollup treats missing arm as `with` and
  missing cost as "—".
"""
from __future__ import annotations

import importlib.util
import pathlib
import unittest

_REPO = pathlib.Path(__file__).resolve().parents[3]
_ROLLUP_PATH = _REPO / "scripts" / "claude_md_eval" / "rollup.py"


def _load_rollup():
    spec = importlib.util.spec_from_file_location("claude_md_eval_rollup", _ROLLUP_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _suite_row(*, ts: str, commit: str = "a" * 40, arm: str = "with",
               runs: int = 3, per_prompt: dict, cost_total: float = 0.0) -> dict:
    totals = {
        "compliant": sum(p.get("compliant", 0) for p in per_prompt.values()),
        "noncompliant": sum(p.get("noncompliant", 0) for p in per_prompt.values()),
        "error": sum(p.get("error", 0) for p in per_prompt.values()),
        "cost_usd": cost_total,
    }
    return {
        "event": "claude_md_eval_suite",
        "ts": ts,
        "commit": commit,
        "branch": "main",
        "payload": {
            "prompt_ids": sorted(per_prompt.keys()),
            "runs_per_prompt": runs,
            "arm": arm,
            "per_prompt": per_prompt,
            "totals": totals,
            "duration_s": 12.3,
        },
    }


def _run_row(*, ts: str, prompt_id: str, rule: str, verdict: str = "compliant",
             commit: str = "a" * 40) -> dict:
    return {
        "event": "claude_md_eval_run",
        "ts": ts,
        "commit": commit,
        "branch": "main",
        "payload": {
            "prompt_id": prompt_id,
            "rule": rule,
            "verdict": verdict,
            "compliant_hits": 1 if verdict == "compliant" else 0,
            "anti_hits": 1 if verdict == "noncompliant" else 0,
            "diff_bytes": 100,
            "duration_s": 1.0,
            "run_number": 1,
            "runs_total": 3,
        },
    }


def _ablation_row(*, ts: str, commit: str = "a" * 40,
                  per_prompt: dict, runs: int = 3) -> dict:
    totals = {
        "with_compliant": sum(p["with_compliant"] for p in per_prompt.values()),
        "without_compliant": sum(p["without_compliant"] for p in per_prompt.values()),
        "delta_compliant": sum(p["delta_compliant"] for p in per_prompt.values()),
        "with_cost_usd": sum(p.get("with_cost_usd", 0.0) for p in per_prompt.values()),
        "without_cost_usd": sum(p.get("without_cost_usd", 0.0)
                                for p in per_prompt.values()),
        "delta_cost_usd": sum(p.get("delta_cost_usd", 0.0) for p in per_prompt.values()),
    }
    return {
        "event": "claude_md_eval_ablation",
        "ts": ts,
        "commit": commit,
        "branch": "main",
        "payload": {
            "prompt_ids": sorted(per_prompt.keys()),
            "runs_per_arm": runs,
            "per_prompt": per_prompt,
            "totals": totals,
            "duration_s": 30.0,
        },
    }


class RenderEmptyLogTest(unittest.TestCase):
    def test_no_suite_rows_renders_no_data_page(self):
        rollup = _load_rollup()
        md = rollup.render_eval_rollup([], rendered_ts="2026-09-28T12:00:00Z")
        self.assertIn("CLAUDE.md compliance eval", md)
        self.assertIn("no `claude_md_eval_suite` rows", md)
        self.assertIn("pixi run claude-md-eval", md)

    def test_unrelated_events_are_ignored(self):
        rollup = _load_rollup()
        # A fanout_audit_finding shouldn't drag the rollup into another section.
        events = [{"event": "fanout_audit_finding", "ts": "2026-09-27T00:00:00Z",
                   "payload": {"desc": "spurious"}}]
        md = rollup.render_eval_rollup(events, rendered_ts="2026-09-28T12:00:00Z")
        self.assertIn("no `claude_md_eval_suite` rows", md)


class SuiteSectionTest(unittest.TestCase):
    def setUp(self):
        self.rollup = _load_rollup()

    def test_latest_suite_renders_table_and_totals(self):
        events = [
            _run_row(ts="2026-09-28T09:00:00Z", prompt_id="h5ad-read",
                     rule="Always use LabelPropsView"),
            _run_row(ts="2026-09-28T09:00:00Z", prompt_id="zarr-read",
                     rule="Always use zarr_utils"),
            _suite_row(ts="2026-09-28T09:00:00Z",
                       per_prompt={
                           "h5ad-read": {"compliant": 3, "noncompliant": 0, "error": 0,
                                         "cost_usd": 0.24},
                           "zarr-read": {"compliant": 2, "noncompliant": 1, "error": 0,
                                         "cost_usd": 0.31},
                       }, cost_total=0.55),
        ]
        md = self.rollup.render_eval_rollup(events, rendered_ts="2026-09-28T12:00:00Z")
        self.assertIn("## Latest suite", md)
        self.assertIn("`h5ad-read`", md)
        self.assertIn("3/3", md)
        self.assertIn("2/3", md)
        self.assertIn("Always use LabelPropsView", md)
        self.assertIn("$0.55", md)  # totals row
        self.assertIn("$0.240", md)  # per-prompt row

    def test_two_arms_latest_row_is_chosen(self):
        # A `without` pass came after a `with` pass; latest-suite section renders
        # the newest row regardless of arm.
        events = [
            _suite_row(ts="2026-09-28T09:00:00Z", arm="with",
                       per_prompt={"a": {"compliant": 3}}),
            _suite_row(ts="2026-09-28T10:00:00Z", arm="without",
                       per_prompt={"a": {"compliant": 1, "noncompliant": 2}}),
        ]
        md = self.rollup.render_eval_rollup(events, rendered_ts="2026-09-28T12:00:00Z")
        self.assertIn("**Arm:** `without`", md)
        self.assertIn("1/3", md)

    def test_legacy_suite_without_arm_or_cost_renders(self):
        # Pre-2026-09-28 rows lack `arm` and `cost_usd`. Rollup treats missing arm as
        # `with` (production shape at the time) and cost as unknown.
        events = [
            {"event": "claude_md_eval_suite",
             "ts": "2026-09-01T00:00:00Z", "commit": "b" * 40,
             "payload": {"prompt_ids": ["a"], "runs_per_prompt": 3,
                         "per_prompt": {"a": {"compliant": 3, "noncompliant": 0,
                                              "error": 0}},
                         "totals": {"compliant": 3, "noncompliant": 0, "error": 0}}}
        ]
        md = self.rollup.render_eval_rollup(events, rendered_ts="2026-09-28T12:00:00Z")
        self.assertIn("**Arm:** `with`", md)
        # Missing cost renders as em dash, not a $0.000 lie.
        self.assertIn("| — |", md)


class FailingSectionTest(unittest.TestCase):
    def setUp(self):
        self.rollup = _load_rollup()

    def test_clean_pass_hides_failing_section(self):
        events = [
            _run_row(ts="2026-09-28T09:00:00Z", prompt_id="a", rule="Rule A"),
            _suite_row(ts="2026-09-28T09:00:00Z",
                       per_prompt={"a": {"compliant": 3, "noncompliant": 0, "error": 0}}),
        ]
        md = self.rollup.render_eval_rollup(events, rendered_ts="2026-09-28T12:00:00Z")
        self.assertNotIn("Failing rules", md)

    def test_failing_prompt_is_called_out_with_rule(self):
        events = [
            _run_row(ts="2026-09-28T09:00:00Z", prompt_id="discovery-first",
                     rule="Grep docs/inventory/*.md before writing new code"),
            _suite_row(ts="2026-09-28T09:00:00Z",
                       per_prompt={
                           "clean": {"compliant": 3, "noncompliant": 0, "error": 0},
                           "discovery-first": {"compliant": 0, "noncompliant": 3,
                                               "error": 0},
                       }),
        ]
        md = self.rollup.render_eval_rollup(events, rendered_ts="2026-09-28T12:00:00Z")
        self.assertIn("## Failing rules", md)
        self.assertIn("`discovery-first` — 0/3 compliant", md)
        self.assertIn("Grep docs/inventory", md)
        self.assertNotIn("`clean`", md.split("## Failing rules")[1])


class AblationSectionTest(unittest.TestCase):
    def setUp(self):
        self.rollup = _load_rollup()

    def test_no_ablation_row_hides_section(self):
        events = [
            _run_row(ts="2026-09-28T09:00:00Z", prompt_id="a", rule="R"),
            _suite_row(ts="2026-09-28T09:00:00Z",
                       per_prompt={"a": {"compliant": 3}}),
        ]
        md = self.rollup.render_eval_rollup(events, rendered_ts="2026-09-28T12:00:00Z")
        self.assertNotIn("Latest ablation", md)

    def test_ablation_row_renders_deltas(self):
        events = [
            _suite_row(ts="2026-09-28T09:00:00Z",
                       per_prompt={"a": {"compliant": 3}}),
            _ablation_row(ts="2026-09-28T10:00:00Z",
                          per_prompt={
                              "a": {"with_compliant": 3, "without_compliant": 1,
                                    "delta_compliant": 2, "with_cost_usd": 0.26,
                                    "without_cost_usd": 0.16, "delta_cost_usd": 0.10},
                          }),
        ]
        md = self.rollup.render_eval_rollup(events, rendered_ts="2026-09-28T12:00:00Z")
        self.assertIn("## Latest ablation", md)
        self.assertIn("+2", md)
        self.assertIn("+$0.100", md)
        # D12 discipline line is stapled to the section (not editorial — a plan-locked
        # requirement).
        self.assertIn("N≥3", md)
        self.assertIn("trace per arm", md)

    def test_ablation_suppresses_delta_when_without_arm_errored(self):
        # 2026-09-29 case: WITHOUT arm errored 12/12 on a Claude Code post-update
        # transient. The old rollup published a bogus Δ=+4 that any reader would
        # misread as CLAUDE.md compliance evidence. Explicit error counts trigger
        # the guard.
        events = [
            _suite_row(ts="2026-09-28T09:00:00Z",
                       per_prompt={"a": {"compliant": 3}}),
            _ablation_row(ts="2026-09-28T10:00:00Z",
                          per_prompt={
                              "a": {"with_compliant": 3, "without_compliant": 0,
                                    "delta_compliant": 3,
                                    "with_cost_usd": 1.71, "without_cost_usd": 0.0,
                                    "delta_cost_usd": 1.71,
                                    "with_error": 0, "without_error": 3},
                              "b": {"with_compliant": 3, "without_compliant": 0,
                                    "delta_compliant": 3,
                                    "with_cost_usd": 1.30, "without_cost_usd": 0.0,
                                    "delta_cost_usd": 1.30,
                                    "with_error": 0, "without_error": 3},
                          }),
        ]
        md = self.rollup.render_eval_rollup(events, rendered_ts="2026-09-28T12:00:00Z")
        self.assertIn("## Latest ablation", md)
        self.assertIn("Δ suppressed", md)
        self.assertIn("WITHOUT arm errored", md)
        # The numeric delta table must not render — no `+3` cells, no `ΔCompliant`
        # header. (Also verifies the guard fires before the table body is composed.)
        self.assertNotIn("ΔCompliant", md)
        self.assertNotIn("+3", md)

    def test_ablation_suppresses_delta_when_legacy_row_shows_cost_asymmetry(self):
        # Old ablation rows written before per-arm error counts landed have to fall
        # back to a cost-based sentinel: if one arm's total cost is <5% of the other's
        # (and the other's is meaningful), treat as errored.
        events = [
            _suite_row(ts="2026-09-28T09:00:00Z",
                       per_prompt={"a": {"compliant": 3}}),
            _ablation_row(ts="2026-09-28T10:00:00Z",
                          per_prompt={
                              # No `with_error`/`without_error` keys.
                              "a": {"with_compliant": 3, "without_compliant": 0,
                                    "delta_compliant": 3,
                                    "with_cost_usd": 1.71, "without_cost_usd": 0.0,
                                    "delta_cost_usd": 1.71},
                              "b": {"with_compliant": 3, "without_compliant": 0,
                                    "delta_compliant": 3,
                                    "with_cost_usd": 1.30, "without_cost_usd": 0.0,
                                    "delta_cost_usd": 1.30},
                          }),
        ]
        md = self.rollup.render_eval_rollup(events, rendered_ts="2026-09-28T12:00:00Z")
        self.assertIn("Δ suppressed", md)


class TrendSectionTest(unittest.TestCase):
    def setUp(self):
        self.rollup = _load_rollup()

    def test_single_pass_suppresses_trend(self):
        events = [_suite_row(ts="2026-09-28T09:00:00Z",
                             per_prompt={"a": {"compliant": 3}})]
        md = self.rollup.render_eval_rollup(events, rendered_ts="2026-09-28T12:00:00Z")
        self.assertNotIn("Trend", md)

    def test_multiple_passes_render_trend_with_sha_marker(self):
        events = [
            _suite_row(ts="2026-09-27T09:00:00Z", commit="a" * 40,
                       per_prompt={"a": {"compliant": 2, "noncompliant": 1, "error": 0}}),
            _suite_row(ts="2026-09-28T09:00:00Z", commit="b" * 40,
                       per_prompt={"a": {"compliant": 3, "noncompliant": 0, "error": 0}}),
        ]
        md = self.rollup.render_eval_rollup(events, rendered_ts="2026-09-28T12:00:00Z")
        self.assertIn("## Trend", md)
        # Most-recent first (b before a); the older row (a) carries the → marker
        # because the SHA changed between the newer and older row.
        trend = md.split("## Trend")[1]
        b_idx = trend.find("`bbbbbbbb`")
        a_idx = trend.find("`aaaaaaaa`")
        self.assertGreater(a_idx, b_idx)  # b appears first (newer)
        self.assertIn("→", trend)


if __name__ == "__main__":
    unittest.main()
