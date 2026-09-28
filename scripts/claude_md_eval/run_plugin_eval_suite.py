#!/usr/bin/env python3
"""Iterate the plugin-eval catalog under `evals/*/` and run each case.

Design: docs/todo/CLAUDE_MD_EVAL_PORT_PLAN.md → P1 (2026-09-28). Mirror of the bespoke
`run_suite.py` — one `claude_md_eval_suite` summary per invocation, per-prompt failures
recorded as `error` verdicts, don't wedge the whole suite.

Sequential over cases (plugin-eval handles its own per-case concurrency internally if we
pass `--concurrency`, but we keep the adapter one-case-per-invocation so failures are
scoped to a single case). Wall clock at N=3 runs × 9 prompts × 2 arms × ~25s = ~22 min.

Usage:
    pixi run claude-md-eval-plugin                            # arm=with, 3 runs, full catalog
    pixi run claude-md-eval-plugin --only h5ad-read           # subset
    pixi run claude-md-eval-plugin --arm without --runs 3     # ablation without-arm
    python scripts/claude_md_eval/run_plugin_eval_suite.py --arm with --runs 1  # smoke
"""
from __future__ import annotations

import argparse
import pathlib
import shutil
import sys
import time

_REPO = pathlib.Path(__file__).resolve().parents[2]
_EVALS_DIR = _REPO / "evals"

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import append_event  # noqa: E402
from cecelia.effectiveness.git_context import current_branch as _current_branch  # noqa: E402

import importlib.util as _importlib_util  # noqa: E402

_ADAPTER = _REPO / "scripts" / "claude_md_eval" / "run_plugin_eval.py"
_spec = _importlib_util.spec_from_file_location("rpe", _ADAPTER)
_rpe = _importlib_util.module_from_spec(_spec)
_spec.loader.exec_module(_rpe)


def _list_case_ids() -> list[str]:
    """Every subdir under evals/ that carries a case.yaml, sorted. Excludes `_shared/`."""
    return sorted(
        p.parent.name for p in _EVALS_DIR.glob("*/case.yaml")
        if not p.parent.name.startswith("_")
    )


def _filter_ids(all_ids: list[str], only: str | None, exclude: str | None) -> list[str]:
    ids = list(all_ids)
    if only:
        wanted = {s.strip() for s in only.split(",") if s.strip()}
        ids = [i for i in ids if i in wanted]
        missing = wanted - set(ids)
        if missing:
            raise SystemExit(f"--only names unknown cases: {sorted(missing)}")
    if exclude:
        drop = {s.strip() for s in exclude.split(",") if s.strip()}
        ids = [i for i in ids if i not in drop]
    return ids


def run_suite(*, runs: int, arm: str, claude_path: str, plugin_root: pathlib.Path,
              only: str | None = None, exclude: str | None = None) -> dict:
    ids = _filter_ids(_list_case_ids(), only, exclude)
    if not ids:
        raise SystemExit("no cases to run (catalog empty or filtered out)")

    blob_sha = _rpe.claude_md_blob_sha(plugin_root)
    branch = _current_branch()
    per_prompt: dict[str, dict] = {}
    start = time.monotonic()

    for case_id in ids:
        print(f"\n=== {case_id} ({runs} run{'s' if runs != 1 else ''}, arm={arm}) ===",
              flush=True)
        try:
            plugin_json = _rpe.run_plugin_eval(
                case_id, runs=runs, arm=arm,
                claude_path=claude_path, plugin_root=plugin_root,
            )
            rows = _rpe.reshape_and_emit(plugin_json, case_id, arm, plugin_root, runs)
        except Exception as e:  # noqa: BLE001 — one case's failure must not wedge the suite
            print(f"  ERROR: {type(e).__name__}: {e}", flush=True)
            per_prompt[case_id] = {"compliant": 0, "noncompliant": 0, "error": runs,
                                   "cost_usd": 0}
            continue
        verdicts = [r["payload"]["verdict"] for r in rows]
        cost = sum(r["payload"].get("cost_usd", 0) for r in rows)
        per_prompt[case_id] = {
            "compliant": verdicts.count("compliant"),
            "noncompliant": verdicts.count("noncompliant"),
            "error": verdicts.count("error"),
            "cost_usd": round(cost, 4),
        }
        for r in rows:
            p = r["payload"]
            print(f"  run {p['run_number']}/{p['runs_total']}: {p['verdict']} "
                  f"(compliant={p['compliant_hits']}, anti={p['anti_hits']}, "
                  f"{p['duration_s']}s, ${p['cost_usd']:.3f})", flush=True)

    duration = time.monotonic() - start
    totals = {
        "compliant": sum(p["compliant"] for p in per_prompt.values()),
        "noncompliant": sum(p["noncompliant"] for p in per_prompt.values()),
        "error": sum(p["error"] for p in per_prompt.values()),
        "cost_usd": round(sum(p["cost_usd"] for p in per_prompt.values()), 4),
    }
    summary = {
        "prompt_ids": ids,
        "runs_per_prompt": runs,
        "arm": arm,
        "per_prompt": per_prompt,
        "totals": totals,
        "duration_s": round(duration, 2),
    }
    append_event("claude_md_eval_suite", summary, commit=blob_sha, branch=branch)
    _print_summary(ids, per_prompt, totals, runs, arm, duration, blob_sha)
    return summary


def _print_summary(ids, per_prompt, totals, runs, arm, duration, blob_sha) -> None:
    print("", flush=True)
    print(f"suite summary (arm={arm}) — {len(ids)} prompt(s) × {runs} run(s) = "
          f"{len(ids) * runs} spawn(s), {duration:.1f}s wall clock, "
          f"CLAUDE.md blob {blob_sha or '<unknown>'}, "
          f"total cost ${totals['cost_usd']:.2f}", flush=True)
    max_id = max((len(i) for i in ids), default=10)
    print(f"  {'prompt':<{max_id}}  compliant  noncompliant  error  cost", flush=True)
    for prompt_id in ids:
        c = per_prompt.get(prompt_id,
                           {"compliant": 0, "noncompliant": 0, "error": 0, "cost_usd": 0})
        print(f"  {prompt_id:<{max_id}}  {c['compliant']:>9}  {c['noncompliant']:>12}  "
              f"{c['error']:>5}  ${c['cost_usd']:>6.3f}", flush=True)
    print(f"  {'TOTAL':<{max_id}}  {totals['compliant']:>9}  {totals['noncompliant']:>12}  "
          f"{totals['error']:>5}  ${totals['cost_usd']:>6.2f}", flush=True)


def main() -> int:
    ap = argparse.ArgumentParser(description="Run the whole CLAUDE.md compliance eval "
                                             "catalog via `claude plugin eval` in one arm.")
    ap.add_argument("--runs", type=int, default=3, help="Fresh agents per prompt (default 3)")
    ap.add_argument("--arm", choices=("with", "without"), default="with",
                    help="CLAUDE.md ablation arm — `with` loads full CLAUDE.md, `without` "
                         "strips it (D5/D8 of the port plan)")
    ap.add_argument("--claude-path", default=shutil.which("claude"))
    ap.add_argument("--plugin-root", type=pathlib.Path, default=_REPO)
    ap.add_argument("--only", help="Comma-separated case ids to run (default: all).")
    ap.add_argument("--exclude", help="Comma-separated case ids to skip.")
    ap.add_argument("--dry-run", action="store_true",
                    help="List cases that would run; skip all spawns.")
    args = ap.parse_args()

    ids = _filter_ids(_list_case_ids(), args.only, args.exclude)
    if args.dry_run:
        print(f"dry-run (arm={args.arm}): {len(ids)} case(s), {args.runs} run(s) each")
        for i in ids:
            print(f"  {i}")
        return 0

    if not args.claude_path:
        print("no `claude` binary on PATH — pass --claude-path", file=sys.stderr)
        return 2

    print(f"claude-md-eval-plugin: {len(ids)} case(s) × {args.runs} run(s), "
          f"arm={args.arm} claude={args.claude_path}", flush=True)
    run_suite(
        runs=args.runs, arm=args.arm,
        claude_path=args.claude_path, plugin_root=args.plugin_root,
        only=args.only, exclude=args.exclude,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
