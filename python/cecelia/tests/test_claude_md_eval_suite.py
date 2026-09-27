"""Tests for the CLAUDE.md compliance eval suite driver.

Design: `docs/todo/CLAUDE_MD_EVAL_PLAN.md` → P2 driver. The driver iterates every prompt in
`scripts/claude_md_eval/prompts/`, calls `run_one_prompt` per id, and emits a suite summary
row. Tests inject a fake `run_one` so no real `claude -p` spawns fire.

Invariants pinned:

- **Every prompt gets called exactly once per suite invocation.** Missing a prompt is worse
  than any per-prompt failure — it silently shrinks the coverage the rollup renders against.
- **One `claude_md_eval_suite` row per invocation.** The row aggregates all prompts; a missing
  row means the trend line loses a full data point.
- **A per-prompt failure emits an `error` verdict for that prompt and does not stop the run.**
  10 prompts, one hangs → 9 real verdicts + one recorded failure, not a wedged suite.
- **`--only` / `--exclude` filter the prompt list**; unknown ids in `--only` are a hard error
  (typo protection), not a silent skip.
"""
from __future__ import annotations

import importlib.util
import os
import pathlib
import tempfile
import textwrap
import unittest
from unittest import mock

from cecelia.effectiveness import EVENT_TYPES, read_events

_REPO = pathlib.Path(__file__).resolve().parents[3]
_SUITE_PATH = _REPO / "scripts" / "claude_md_eval" / "run_suite.py"


def _load_suite():
    spec = importlib.util.spec_from_file_location("run_suite", _SUITE_PATH)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _fake_row(prompt_id: str, verdict: str) -> dict:
    return {
        "event": "claude_md_eval_run",
        "commit": "a" * 40,
        "payload": {"prompt_id": prompt_id, "verdict": verdict,
                    "compliant_hits": 1 if verdict == "compliant" else 0,
                    "anti_hits": 1 if verdict == "noncompliant" else 0,
                    "diff_bytes": 100, "duration_s": 1.0, "run_number": 1, "runs_total": 1},
    }


class SuiteFilterTest(unittest.TestCase):
    def setUp(self):
        self.suite = _load_suite()

    def test_only_narrows_to_named_ids(self):
        ids = self.suite._filter_ids(["a", "b", "c"], only="a,c", exclude=None)
        self.assertEqual(ids, ["a", "c"])

    def test_only_with_unknown_id_raises(self):
        with self.assertRaises(SystemExit) as cm:
            self.suite._filter_ids(["a", "b"], only="a,does_not_exist", exclude=None)
        self.assertIn("does_not_exist", str(cm.exception))

    def test_exclude_removes_named_ids(self):
        ids = self.suite._filter_ids(["a", "b", "c"], only=None, exclude="b")
        self.assertEqual(ids, ["a", "c"])

    def test_only_and_exclude_compose(self):
        # `only` first (narrow), then `exclude` (drop) — matches the argparse doc.
        ids = self.suite._filter_ids(["a", "b", "c"], only="a,b", exclude="b")
        self.assertEqual(ids, ["a"])


class ListPromptIdsTest(unittest.TestCase):
    """`_list_prompt_ids` reads the real prompt catalog on disk — the check is that our seed
    prompts are visible + sorted, so a rollup renders in a stable order."""

    def test_catalog_contains_expected_seed_prompts(self):
        suite = _load_suite()
        ids = suite._list_prompt_ids()
        # Sanity — the catalog has AT LEAST the seed prompts. Adding more later is fine.
        expected = {"h5ad-read", "h5ad-write", "zarr-read", "zarr-write", "spawn-python",
                    "kill-process-tree", "dir-size", "utf-8-json-write", "cite-algorithm"}
        self.assertTrue(expected.issubset(set(ids)),
                        f"catalog missing: {sorted(expected - set(ids))}")

    def test_ids_are_returned_sorted(self):
        suite = _load_suite()
        ids = suite._list_prompt_ids()
        self.assertEqual(ids, sorted(ids))


class RunSuiteTest(unittest.TestCase):
    """End-to-end with an injected fake `run_one` — no real `claude -p` spawns."""

    def setUp(self):
        self.suite = _load_suite()
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.tmpdir = pathlib.Path(self._tmp.name)
        self.log_path = self.tmpdir / "events.jsonl"
        self._env_patch = mock.patch.dict(
            os.environ, {"CECELIA_EFFECTIVENESS_LOG": str(self.log_path)},
        )
        self._env_patch.start()
        self.addCleanup(self._env_patch.stop)
        self._blob_patch = mock.patch.object(
            self.suite._run_prompt, "claude_md_blob_sha", return_value="b" * 40,
        )
        self._blob_patch.start()
        self.addCleanup(self._blob_patch.stop)
        # The suite driver captures `branch` for the `_suite` summary row (same rollup-join
        # reason as `_run`/`_pass`). Stub so tests don't shell out to git.
        self._branch_patch = mock.patch.object(
            self.suite, "_current_branch", return_value="test-branch",
        )
        self._branch_patch.start()
        self.addCleanup(self._branch_patch.stop)

    def _events(self):
        return list(read_events(self.log_path))

    def test_event_type_is_declared(self):
        self.assertIn("claude_md_eval_suite", EVENT_TYPES)

    def test_calls_run_one_per_prompt_and_emits_suite_row(self):
        calls: list[str] = []
        def fake_run_one(prompt_id: str, **kw):
            calls.append(prompt_id)
            return [_fake_row(prompt_id, "compliant")]
        summary = self.suite.run_suite(
            runs=1, timeout=60, claude_path="fake", worktree_root=self.tmpdir,
            keep_worktrees=False, only="h5ad-read,zarr-read", run_one=fake_run_one,
        )
        self.assertEqual(calls, ["h5ad-read", "zarr-read"])
        self.assertEqual(summary["totals"]["compliant"], 2)
        # Exactly one suite row landed in the log.
        suite_rows = [e for e in self._events() if e["event"] == "claude_md_eval_suite"]
        self.assertEqual(len(suite_rows), 1)
        self.assertEqual(suite_rows[0]["commit"], "b" * 40)
        self.assertEqual(suite_rows[0]["branch"], "test-branch")
        self.assertEqual(suite_rows[0]["payload"]["prompt_ids"], ["h5ad-read", "zarr-read"])

    def test_per_prompt_error_does_not_stop_the_suite(self):
        seen: list[str] = []
        def fake_run_one(prompt_id: str, **kw):
            seen.append(prompt_id)
            if prompt_id == "zarr-read":
                raise RuntimeError("simulated hang / crash")
            return [_fake_row(prompt_id, "compliant")]
        summary = self.suite.run_suite(
            runs=2, timeout=60, claude_path="fake", worktree_root=self.tmpdir,
            keep_worktrees=False, only="h5ad-read,zarr-read,dir-size",
            run_one=fake_run_one,
        )
        # All three prompts were attempted (order is alphabetical — see
        # `_list_prompt_ids`, sorted for stable rollup order).
        self.assertEqual(sorted(seen), sorted(["h5ad-read", "zarr-read", "dir-size"]))
        self.assertEqual(len(seen), 3)
        # The failing prompt records `error` = runs (the full set was lost), zero others.
        self.assertEqual(summary["per_prompt"]["zarr-read"],
                         {"compliant": 0, "noncompliant": 0, "error": 2})
        # Successful prompts still get counted.
        self.assertEqual(summary["per_prompt"]["h5ad-read"]["compliant"], 1)
        self.assertEqual(summary["per_prompt"]["dir-size"]["compliant"], 1)

    def test_totals_aggregate_across_prompts(self):
        def fake_run_one(prompt_id: str, **kw):
            # Mixed verdicts across the two prompts.
            if prompt_id == "h5ad-read":
                return [_fake_row(prompt_id, "compliant"),
                        _fake_row(prompt_id, "noncompliant")]
            return [_fake_row(prompt_id, "noncompliant"),
                    _fake_row(prompt_id, "noncompliant")]
        summary = self.suite.run_suite(
            runs=2, timeout=60, claude_path="fake", worktree_root=self.tmpdir,
            keep_worktrees=False, only="h5ad-read,zarr-read", run_one=fake_run_one,
        )
        self.assertEqual(summary["totals"], {"compliant": 1, "noncompliant": 3, "error": 0})


if __name__ == "__main__":
    unittest.main()
