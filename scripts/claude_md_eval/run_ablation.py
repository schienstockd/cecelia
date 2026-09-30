#!/usr/bin/env python3
"""CLAUDE.md compliance eval — ablation driver.

Fires the P2 suite twice (arm=with, arm=without), computes per-prompt deltas
(compliant runs, cost) and emits one `claude_md_eval_ablation` row to the effectiveness
log. Design: docs/todo/CLAUDE_MD_EVAL_PLAN.md → *Ablation instrument (2026-09-28)*.

Instrument: `run_prompt.py._make_detached_worktree(arm="without")` strips every
CLAUDE.md from the throwaway worktree before `claude -p` spawns. Same loading
mechanism as production (Claude Code reads CLAUDE.md from cwd); only presence
flips. Sonnet D12: at least one trace read per arm before quoting Δ anywhere.

Cost: doubles wall clock + doubles API spend of a normal suite pass. N=3 × 9 prompts
× 2 arms × ~$0.20/run ≈ **$11/pass**. Guard against accidental repeated runs.

Usage:
    pixi run claude-md-eval-ablation                       # full catalog, 3 runs per arm
    pixi run claude-md-eval-ablation --runs 1              # 1 run per arm (~5 min, ~$4)
    pixi run claude-md-eval-ablation --only discovery-first  # just one prompt (~$0.80)
"""
from __future__ import annotations

import argparse
import pathlib
import shutil
import sys
import time

_REPO = pathlib.Path(__file__).resolve().parents[2]

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import append_event  # noqa: E402
from cecelia.effectiveness.git_context import current_branch as _current_branch  # noqa: E402

import importlib.util as _importlib_util  # noqa: E402
_SUITE_PATH = _REPO / "scripts" / "claude_md_eval" / "run_suite.py"
_spec = _importlib_util.spec_from_file_location("run_suite", _SUITE_PATH)
_run_suite = _importlib_util.module_from_spec(_spec)
_spec.loader.exec_module(_run_suite)


def run_ablation(*, runs: int, timeout: int, claude_path: str,
                 worktree_root: pathlib.Path, keep_worktrees: bool,
                 only: str | None = None, exclude: str | None = None) -> dict:
    start = time.monotonic()
    print(f"\n>>> WITH CLAUDE.md arm (runs={runs})", flush=True)
    with_summary = _run_suite.run_suite(
        runs=runs, timeout=timeout, claude_path=claude_path,
        worktree_root=worktree_root, keep_worktrees=keep_worktrees,
        arm="with", only=only, exclude=exclude,
    )
    print(f"\n>>> WITHOUT CLAUDE.md arm (runs={runs})", flush=True)
    without_summary = _run_suite.run_suite(
        runs=runs, timeout=timeout, claude_path=claude_path,
        worktree_root=worktree_root, keep_worktrees=keep_worktrees,
        arm="without", only=only, exclude=exclude,
    )
    duration = time.monotonic() - start

    prompt_ids = with_summary["prompt_ids"]
    per_prompt: dict[str, dict] = {}
    for pid in prompt_ids:
        w = with_summary["per_prompt"].get(pid, {"compliant": 0, "cost_usd": 0.0, "error": 0})
        wo = without_summary["per_prompt"].get(pid, {"compliant": 0, "cost_usd": 0.0, "error": 0})
        per_prompt[pid] = {
            "with_compliant": w["compliant"],
            "without_compliant": wo["compliant"],
            "delta_compliant": w["compliant"] - wo["compliant"],
            "with_cost_usd": w["cost_usd"],
            "without_cost_usd": wo["cost_usd"],
            "delta_cost_usd": round(w["cost_usd"] - wo["cost_usd"], 4),
            # Per-arm error counts recorded so the rollup can suppress the Δ table
            # when an arm broke transiently (2026-09-29 claude 2.1.284 auth transient
            # errored 12/12 WITHOUT runs and published a bogus Δ=+4). Fallback
            # to 0 keeps old ablation rows renderable.
            "with_error": w.get("error", 0),
            "without_error": wo.get("error", 0),
        }
    totals = {
        "with_compliant": with_summary["totals"]["compliant"],
        "without_compliant": without_summary["totals"]["compliant"],
        "delta_compliant": with_summary["totals"]["compliant"]
                           - without_summary["totals"]["compliant"],
        "with_cost_usd": with_summary["totals"]["cost_usd"],
        "without_cost_usd": without_summary["totals"]["cost_usd"],
        "delta_cost_usd": round(with_summary["totals"]["cost_usd"]
                                - without_summary["totals"]["cost_usd"], 4),
        "with_error": with_summary["totals"].get("error", 0),
        "without_error": without_summary["totals"].get("error", 0),
    }
    payload = {
        "prompt_ids": prompt_ids,
        "full_catalog": only is None and exclude is None,
        "runs_per_arm": runs,
        "per_prompt": per_prompt,
        "totals": totals,
        "duration_s": round(duration, 2),
    }
    blob_sha = _run_suite._run_prompt.claude_md_blob_sha(_REPO)
    append_event("claude_md_eval_ablation", payload,
                 commit=blob_sha, branch=_current_branch())
    _print_ablation_summary(prompt_ids, per_prompt, totals, runs, duration, blob_sha)
    # Re-render the rollup once after the `_ablation` row is appended — the two inner
    # `run_suite()` calls each fire `_render_rollup_safely()` themselves, but both
    # predate this row, so without a final render the "Latest ablation" section stays
    # one pass behind until the user manually runs `pixi run claude-md-eval-rollup`.
    _run_suite._render_rollup_safely()
    return payload


def _arm_errored(per_prompt: dict, runs: int, arm: str) -> bool:
    """Terminal sibling of `rollup.py._ablation_arm_errored` — same suppression rule.

    Same 2026-09-29 case: an operator watching the shell during the transient WITHOUT
    breakage would otherwise see `TOTAL 4 0 +4` and misread it as a real CLAUDE.md Δ.
    The rollup silences this in the doc; this silences it in the terminal output. Kept
    duplicated (not imported) — rollup.py is a pure renderer with no side-effects and
    a callsite here would drag its whole import graph into this driver.
    """
    if runs <= 0:
        return False
    key = f"{arm}_error"
    return sum(1 for r in per_prompt.values() if r.get(key, 0) >= runs) \
        >= max(1, len(per_prompt) // 2)


def _print_ablation_summary(ids, per_prompt, totals, runs, duration, blob_sha):
    print("", flush=True)
    print(f"ablation summary — {len(ids)} prompt(s) × {runs} run(s) × 2 arms, "
          f"{duration:.1f}s wall clock, CLAUDE.md blob {blob_sha or '<unknown>'}, "
          f"total cost ${totals['with_cost_usd'] + totals['without_cost_usd']:.2f}",
          flush=True)
    without_errored = _arm_errored(per_prompt, runs, "without")
    with_errored = _arm_errored(per_prompt, runs, "with")
    if without_errored or with_errored:
        broken = "WITHOUT" if without_errored else "WITH"
        print(f"  ⚠ {broken} arm errored across ≥50% of runs — Δ suppressed. "
              f"An arm-wide error means the numeric delta is not evidence about "
              f"CLAUDE.md; it is evidence the arm broke. Re-run once the underlying "
              f"cause is fixed.", flush=True)
        print("", flush=True)
        return
    max_id = max((len(i) for i in ids), default=10)
    print(f"  {'prompt':<{max_id}}  with  without  Δ    with$    without$  Δ$", flush=True)
    for pid in ids:
        c = per_prompt[pid]
        print(f"  {pid:<{max_id}}  {c['with_compliant']:>4}  {c['without_compliant']:>7}  "
              f"{c['delta_compliant']:+d}    ${c['with_cost_usd']:>6.3f}  "
              f"${c['without_cost_usd']:>7.3f}  ${c['delta_cost_usd']:+.3f}", flush=True)
    print(f"  {'TOTAL':<{max_id}}  {totals['with_compliant']:>4}  "
          f"{totals['without_compliant']:>7}  {totals['delta_compliant']:+d}    "
          f"${totals['with_cost_usd']:>6.2f}  ${totals['without_cost_usd']:>7.2f}  "
          f"${totals['delta_cost_usd']:+.2f}", flush=True)
    print("", flush=True)
    print("D12: before quoting Δ anywhere, read at least one trace per arm — a null-delta "
          "case may still be silent-no-load; a positive-delta case may be an artefact.",
          flush=True)


def main() -> int:
    ap = argparse.ArgumentParser(
        description="Run the CLAUDE.md compliance eval catalog twice (with + without) and "
                    "emit per-prompt deltas.")
    ap.add_argument("--runs", type=int, default=3,
                    help="Fresh agents per prompt PER ARM (default 3 — so 6 total per prompt)")
    ap.add_argument("--timeout", type=int, default=300)
    ap.add_argument("--claude-path", default=shutil.which("claude"))
    ap.add_argument("--worktree-root", type=pathlib.Path, default=_REPO.parent)
    ap.add_argument("--keep-worktrees", action="store_true")
    ap.add_argument("--only", help="Comma-separated prompt ids to run (default: all)")
    ap.add_argument("--exclude", help="Comma-separated prompt ids to skip")
    args = ap.parse_args()

    if not args.claude_path:
        print("no `claude` binary on PATH", file=sys.stderr)
        return 2

    run_ablation(
        runs=args.runs, timeout=args.timeout,
        claude_path=args.claude_path, worktree_root=args.worktree_root,
        keep_worktrees=args.keep_worktrees,
        only=args.only, exclude=args.exclude,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
