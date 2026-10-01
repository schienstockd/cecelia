#!/usr/bin/env python3
"""CLAUDE.md compliance eval — P2 suite driver.

Iterates every `.md` prompt under `scripts/claude_md_eval/prompts/` and invokes
`run_one_prompt` (from the P1 runner) for each. Emits one `claude_md_eval_suite` summary row
per full invocation, plus the per-run + per-pass rows the P1 code already writes. Prints an
aggregate summary table at the end so a caller sees results without reading the log.

Sequential by design — parallel spawns would race for gh + git, and rate-limit friendliness
matters when we're standing up fresh `claude -p` invocations back to back. Wall clock at
N=3 runs per prompt — measured 22 min for 12 prompts (2026-09-29); deliberately budgeted for.

Usage:
    pixi run claude-md-eval                                        # full catalog, 3 runs each
    pixi run claude-md-eval --runs 1                               # 1 run per prompt (~5 min)
    pixi run claude-md-eval --only h5ad-read,zarr-read             # subset by id
    pixi run claude-md-eval --exclude cite-algorithm               # skip a prompt
    python scripts/claude_md_eval/run_suite.py --dry-run           # list prompts + skip spawns
"""
from __future__ import annotations

import argparse
import pathlib
import shutil
import sys
import time
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]
_PROMPTS_DIR = _REPO / "scripts" / "claude_md_eval" / "prompts"

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import append_event  # noqa: E402
from cecelia.effectiveness.git_context import current_branch as _current_branch  # noqa: E402

# Reuse the P1 runner's public surface. `run_prompt` isn't a package — spec-loaded from disk
# so this script works whether or not it's installed as a module.
import importlib.util as _importlib_util  # noqa: E402

_RUNNER_PATH = _REPO / "scripts" / "claude_md_eval" / "run_prompt.py"
_spec = _importlib_util.spec_from_file_location("run_prompt", _RUNNER_PATH)
_run_prompt = _importlib_util.module_from_spec(_spec)
_spec.loader.exec_module(_run_prompt)


def _list_prompt_ids() -> list[str]:
    """Every `.md` prompt in the catalog, sorted for stable iteration + rollup."""
    return sorted(p.stem for p in _PROMPTS_DIR.glob("*.md"))


def _filter_ids(all_ids: list[str], only: str | None, exclude: str | None) -> list[str]:
    ids = list(all_ids)
    if only:
        wanted = {s.strip() for s in only.split(",") if s.strip()}
        ids = [i for i in ids if i in wanted]
        missing = wanted - set(ids)
        if missing:
            raise SystemExit(f"--only names unknown prompts: {sorted(missing)}")
    if exclude:
        drop = {s.strip() for s in exclude.split(",") if s.strip()}
        ids = [i for i in ids if i not in drop]
    return ids


def run_suite(
    *, runs: int, timeout: int, claude_path: str | None,
    worktree_root: pathlib.Path, keep_worktrees: bool, arm: str = "with",
    only: str | None = None, exclude: str | None = None,
    run_one: _t.Callable[..., list[dict]] | None = None,
) -> dict:
    """Iterate every prompt, invoke run_one per prompt, emit one suite summary row.

    `run_one` is an injectable seam so tests bypass the real `claude -p` spawn. Default is the
    P1 `run_one_prompt` function. `arm` is threaded through (with/without CLAUDE.md ablation).
    Returns the summary payload as a dict (also emitted as a `claude_md_eval_suite` row).
    """
    run_one = run_one or _run_prompt.run_one_prompt
    # Set a synthetic session id BEFORE the first append_event so the suite row + every
    # per-prompt row shares one identifier. Called here (in addition to inside
    # `run_one_prompt`) so the `claude_md_eval_suite` summary row emitted below also
    # gets a real session, not `unknown`.
    _run_prompt._ensure_eval_session()
    ids = _filter_ids(_list_prompt_ids(), only, exclude)
    if not ids:
        raise SystemExit("no prompts to run (catalog empty or filtered out)")

    blob_sha = _run_prompt.claude_md_blob_sha(_REPO)
    per_prompt: dict[str, dict] = {}
    start = time.monotonic()
    for prompt_id in ids:
        print(f"\n=== {prompt_id} ({runs} run{'s' if runs != 1 else ''}, arm={arm}) ===",
              flush=True)
        try:
            rows = run_one(
                prompt_id, runs=runs, timeout=timeout,
                claude_path=claude_path, worktree_root=worktree_root,
                keep_worktrees=keep_worktrees, arm=arm,
            )
        except Exception as e:  # noqa: BLE001 — best-effort: one prompt's failure must not wedge the whole suite
            print(f"  ERROR: {type(e).__name__}: {e}", flush=True)
            per_prompt[prompt_id] = {"compliant": 0, "noncompliant": 0, "error": runs,
                                     "cost_usd": 0.0}
            continue
        verdicts = [r["payload"]["verdict"] for r in rows]
        cost = sum(r["payload"].get("cost_usd", 0) for r in rows)
        per_prompt[prompt_id] = {
            "compliant": verdicts.count("compliant"),
            "noncompliant": verdicts.count("noncompliant"),
            "error": verdicts.count("error"),
            "cost_usd": round(cost, 4),
        }
    duration = time.monotonic() - start

    totals = {
        "compliant": sum(p["compliant"] for p in per_prompt.values()),
        "noncompliant": sum(p["noncompliant"] for p in per_prompt.values()),
        "error": sum(p["error"] for p in per_prompt.values()),
        "cost_usd": round(sum(p["cost_usd"] for p in per_prompt.values()), 4),
    }
    summary_payload = {
        "prompt_ids": ids,
        # The rollup leads with the newest full pass — a `--only` spot-check must not
        # stand in for the state of the setup.
        "full_catalog": only is None and exclude is None,
        "runs_per_prompt": runs,
        "arm": arm,
        "per_prompt": per_prompt,
        "totals": totals,
        "duration_s": round(duration, 2),
    }
    append_event("claude_md_eval_suite", summary_payload,
                 commit=blob_sha, branch=_current_branch())
    _print_summary(ids, per_prompt, totals, runs, arm, duration, blob_sha)
    _render_rollup_safely()
    return summary_payload


def _render_rollup_safely() -> None:
    """Regenerate `docs/ai-assist/CLAUDE_MD_EVAL.md` at the end of every pass.

    Wrapped so a rollup-render failure never masks a successful suite — the suite row
    is already appended before we get here; the artifact is a side-effect, not the
    contract. Errors print to stderr and are otherwise swallowed.
    """
    try:
        _render_rollup_mod = _importlib_util.spec_from_file_location(
            "render_rollup", _REPO / "scripts" / "claude_md_eval" / "render_rollup.py")
        mod = _importlib_util.module_from_spec(_render_rollup_mod)
        _render_rollup_mod.loader.exec_module(mod)
        target = mod.render_to_file()
        print(f"rollup: {target}", flush=True)
    except Exception as e:  # noqa: BLE001 — best-effort side-effect
        print(f"rollup render failed ({type(e).__name__}): {e}", file=sys.stderr,
              flush=True)


def _print_summary(ids: list[str], per_prompt: dict[str, dict],
                   totals: dict, runs: int, arm: str, duration: float,
                   blob_sha: str | None) -> None:
    total_runs = len(ids) * runs
    print("", flush=True)
    print(f"suite summary (arm={arm}) — {len(ids)} prompt(s) × {runs} run(s) = {total_runs} "
          f"spawn(s), {duration:.1f}s wall clock, CLAUDE.md blob {blob_sha or '<unknown>'}, "
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
    ap = argparse.ArgumentParser(description="Run the whole CLAUDE.md compliance eval catalog.")
    ap.add_argument("--runs", type=int, default=3, help="Fresh agents per prompt (default 3)")
    ap.add_argument("--timeout", type=int, default=300, help="Per-spawn timeout in seconds (default 300)")
    ap.add_argument("--claude-path", default=shutil.which("claude"),
                    help="Path to the `claude` binary (defaults to `which claude`).")
    ap.add_argument("--worktree-root", type=pathlib.Path, default=_run_prompt._WORKTREE_ROOT_DEFAULT,
                    help=f"Parent dir for throwaway worktrees (default {_run_prompt._WORKTREE_ROOT_DEFAULT})")
    ap.add_argument("--keep-worktrees", action="store_true",
                    help="Don't remove worktrees after each run (debugging).")
    ap.add_argument("--arm", choices=("with", "without"), default="with",
                    help="CLAUDE.md ablation arm — `with` (default) keeps CLAUDE.md in the "
                         "worktree, `without` strips it before spawn. Use "
                         "`pixi run claude-md-eval-ablation` to fire both arms + compute delta.")
    ap.add_argument("--only", help="Comma-separated prompt ids to run (default: all).")
    ap.add_argument("--exclude", help="Comma-separated prompt ids to skip.")
    ap.add_argument("--dry-run", action="store_true",
                    help="List the prompts that would run; skip all spawns.")
    args = ap.parse_args()

    ids = _filter_ids(_list_prompt_ids(), args.only, args.exclude)
    if args.dry_run:
        print(f"dry-run (arm={args.arm}): {len(ids)} prompt(s), {args.runs} run(s) each")
        for i in ids:
            print(f"  {i}")
        return 0

    print(f"claude-md-eval: {len(ids)} prompt(s) × {args.runs} run(s), arm={args.arm} "
          f"timeout={args.timeout}s claude={args.claude_path}", flush=True)
    run_suite(
        runs=args.runs, timeout=args.timeout,
        claude_path=args.claude_path, worktree_root=args.worktree_root,
        keep_worktrees=args.keep_worktrees, arm=args.arm,
        only=args.only, exclude=args.exclude,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
