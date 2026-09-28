#!/usr/bin/env python3
"""Adapter: wrap `claude plugin eval` for a single CLAUDE.md compliance case.

Design: docs/todo/CLAUDE_MD_EVAL_PORT_PLAN.md → P0 spike (2026-09-28).

Invokes `claude plugin eval --json` on ONE eval case in ONE ablation arm (with-CLAUDE.md
or without-CLAUDE.md), reads the resulting per-run structured output, and reshapes it
into the existing `claude_md_eval_run` + `claude_md_eval_pass` event schema so the
effectiveness-log rollup + recital pipeline don't move (D2 of the port plan).

Ablation is via our scaffold's `CLAUDE_CODE_EVAL_CLAUDE_MD_ARM` env var, NOT
plugin-eval's built-in `--ablation with-without` (which flips *the plugin*, not
CLAUDE.md — D5). We pass `--ablation none` and drive the swap ourselves; the caller
runs this script twice with different `--arm` values and computes deltas.

Usage:
    python scripts/claude_md_eval/run_plugin_eval.py h5ad-read --runs 3 --arm with
    python scripts/claude_md_eval/run_plugin_eval.py h5ad-read --runs 3 --arm without
"""
from __future__ import annotations

import argparse
import json
import os
import pathlib
import shutil
import subprocess
import sys
import tempfile

_REPO = pathlib.Path(__file__).resolve().parents[2]

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import append_event  # noqa: E402
from cecelia.effectiveness.git_context import current_branch as _current_branch  # noqa: E402

# The bespoke runner's prompt frontmatter carried a `rule:` string per prompt (used by
# the rollup for human-readable rule names). Case.yaml doesn't have a first-class `rule`
# field, so for the P0 spike we keep the mapping here. When the catalog grows past ~2
# prompts, promote this into an `evals/<case>/rule.md` sidecar or a top-level catalog.
_RULE_BY_CASE = {
    "cite-algorithm": "Cite sources for non-trivial algorithms",
    "dir-size": "Windows compatibility — `_dir_bytes` for directory size",
    "h5ad-read": "H5AD / cell-data access — always go through the readers/writers",
    "h5ad-write": "H5AD / cell-data access — always go through the readers/writers",
    "kill-process-tree": "Windows compatibility — `_kill_tree` / `free_port` for process kill",
    "spawn-python": "Spawning Python — always go through `run_py`",
    "utf-8-json-write": "Windows compatibility — always pass `encoding=\"utf-8\"` to Python text I/O",
    "zarr-read": "Image / OME-ZARR access — always go through `zarr_utils`",
    "zarr-write": "Image / OME-ZARR access — always go through `zarr_utils`",
}


def claude_md_blob_sha(cwd: pathlib.Path) -> str | None:
    """Content-addressed SHA of CLAUDE.md at HEAD — same anchor the bespoke runner used."""
    result = subprocess.run(
        ["git", "rev-parse", "HEAD:CLAUDE.md"],
        cwd=str(cwd), capture_output=True, text=True,
        timeout=10.0, check=False, encoding="utf-8",
    )
    if result.returncode != 0:
        return None
    return result.stdout.strip() or None


def run_plugin_eval(case: str, *, runs: int, arm: str, claude_path: str,
                    plugin_root: pathlib.Path) -> dict:
    """Invoke `claude plugin eval --ablation none` for one case in one CLAUDE.md arm."""
    tmp = tempfile.NamedTemporaryFile("r+", suffix=".json", delete=False)
    tmp.close()
    jpath = pathlib.Path(tmp.name)
    try:
        env = {**os.environ, "CLAUDE_CODE_EVAL_CLAUDE_MD_ARM": arm}
        cmd = [
            claude_path, "plugin", "eval", str(plugin_root),
            "--case", case,
            "--runs", str(runs),
            "--scaffold",
            "--trust-plugin",
            "--allow-tools", "Bash,Write,Edit",
            "--ablation", "none",
            "--json", str(jpath),
        ]
        # Per-run agent budget is ~5min; give the whole invocation runs*5min + slack.
        timeout = runs * 360 + 60
        result = subprocess.run(
            cmd, cwd=str(plugin_root), capture_output=True, text=True,
            env=env, timeout=timeout, check=False, encoding="utf-8",
        )
        if result.returncode != 0 and not jpath.stat().st_size:
            raise RuntimeError(f"plugin eval exited {result.returncode}: "
                               f"{(result.stderr or '')[:500]}")
        with jpath.open() as f:
            return json.load(f)
    finally:
        jpath.unlink(missing_ok=True)


def reshape_and_emit(plugin_json: dict, case: str, arm: str,
                     plugin_root: pathlib.Path, runs: int) -> list[dict]:
    """Reshape plugin-eval per-run scores into `claude_md_eval_run` + `_pass` events."""
    blob_sha = claude_md_blob_sha(plugin_root)
    branch = _current_branch()
    rule = _RULE_BY_CASE.get(case, f"(unregistered rule for case {case!r})")

    case_data = next((c for c in plugin_json.get("cases", []) if c["name"] == case), None)
    if case_data is None:
        raise RuntimeError(f"case {case!r} not present in plugin-eval output")

    # `--ablation none` produces `arms: {"with": [...runs]}` — a single arm regardless of
    # our CLAUDE.md swap (plugin-eval's "with" means "with-plugin-loaded", orthogonal to
    # our scaffold's CLAUDE.md arm). Fall back to any single arm to survive minor schema
    # variance across plugin-eval versions.
    per_arm_runs = case_data["arms"].get("with") or next(iter(case_data["arms"].values()))

    rows: list[dict] = []
    for i, run_data in enumerate(per_arm_runs, start=1):
        graders = {g["name"]: g for g in run_data.get("graders", [])}
        compliant_g = graders.get("compliant", {})
        anti_g = graders.get("anti", {})
        run_error = run_data.get("error")

        if run_error:
            verdict, compliant_hits, anti_hits = "error", 0, 0
        else:
            compliant_hits = int(bool(compliant_g.get("passed")))
            # anti grader `match: not_contains` — `passed=True` means pattern ABSENT
            # (good). We invert to match the bespoke schema where `anti_hits` counts
            # bypass matches (bad).
            anti_hits = int(not anti_g.get("passed", True))
            if anti_hits > 0:
                verdict = "noncompliant"
            elif compliant_hits > 0:
                verdict = "compliant"
            else:
                verdict = "noncompliant"  # honest floor: neither signal fired

        payload = {
            "prompt_id": case,
            "rule": rule,
            "verdict": verdict,
            "compliant_hits": compliant_hits,
            "anti_hits": anti_hits,
            "duration_s": round(run_data.get("durationSeconds", 0), 2),
            "cost_usd": round(run_data.get("costUsd", 0), 4),
            "run_number": i,
            "runs_total": runs,
            "arm": arm,
            "trace_path": run_data.get("tracePath") or None,
            "turns": run_data.get("turns", 0),
        }
        if run_error:
            payload["error"] = run_error
        row = append_event("claude_md_eval_run", payload,
                           commit=blob_sha, branch=branch)
        rows.append(row)

    verdicts = [r["payload"]["verdict"] for r in rows]
    summary = {
        "prompt_id": case,
        "rule": rule,
        "runs_total": runs,
        "arm": arm,
        "compliant": verdicts.count("compliant"),
        "noncompliant": verdicts.count("noncompliant"),
        "error": verdicts.count("error"),
        "total_cost_usd": round(
            sum(r["payload"].get("cost_usd", 0) for r in rows), 4,
        ),
    }
    append_event("claude_md_eval_pass", summary,
                 commit=blob_sha, branch=branch)
    return rows


def _print_summary(rows: list[dict], arm: str) -> None:
    if not rows:
        print("no runs — nothing to summarise", file=sys.stderr)
        return
    print("", flush=True)
    print(f"summary (arm={arm}) — {len(rows)} run(s), "
          f"CLAUDE.md blob {rows[0].get('commit') or '<unknown>'}, "
          f"cost ${sum(r['payload'].get('cost_usd', 0) for r in rows):.3f}", flush=True)
    counts: dict[str, int] = {}
    for r in rows:
        v = r["payload"]["verdict"]
        counts[v] = counts.get(v, 0) + 1
    for verdict in ("compliant", "noncompliant", "error"):
        if verdict in counts:
            print(f"  {verdict:14s} {counts[verdict]}/{len(rows)}", flush=True)


def main() -> int:
    ap = argparse.ArgumentParser(
        description="Run one CLAUDE.md compliance case via `claude plugin eval` and "
                    "emit `claude_md_eval_run` + `_pass` rows to the effectiveness log.",
    )
    ap.add_argument("case", help="Case name (directory stem under evals/)")
    ap.add_argument("--runs", type=int, default=3,
                    help="Fresh agents to spawn (default 3)")
    ap.add_argument("--arm", choices=("with", "without"), default="with",
                    help="CLAUDE.md ablation arm — `with` loads full CLAUDE.md, "
                         "`without` strips it (D5/D8 of the port plan)")
    ap.add_argument("--claude-path", default=shutil.which("claude"),
                    help="Path to the `claude` binary (default: `which claude`)")
    ap.add_argument("--plugin-root", type=pathlib.Path, default=_REPO,
                    help="Plugin root (default: this repo)")
    args = ap.parse_args()

    if not args.claude_path:
        print("no `claude` binary on PATH — pass --claude-path", file=sys.stderr)
        return 2

    print(f"plugin-eval: case={args.case} runs={args.runs} arm={args.arm}", flush=True)
    plugin_json = run_plugin_eval(
        args.case, runs=args.runs, arm=args.arm,
        claude_path=args.claude_path, plugin_root=args.plugin_root,
    )
    rows = reshape_and_emit(plugin_json, args.case, args.arm, args.plugin_root, args.runs)
    for r in rows:
        p = r["payload"]
        print(f"  run {p['run_number']}/{p['runs_total']}: {p['verdict']} "
              f"(compliant={p['compliant_hits']}, anti={p['anti_hits']}, "
              f"{p['duration_s']}s, ${p['cost_usd']:.3f}, turns={p['turns']})", flush=True)
    _print_summary(rows, args.arm)
    return 0


if __name__ == "__main__":
    sys.exit(main())
