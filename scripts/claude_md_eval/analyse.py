#!/usr/bin/env python3
"""Read the latest CLAUDE.md eval pass the diagnostic way — what's weak, and where the trace is.

The eval exists to find where a fresh Claude struggles with our dev setup, so the setup can
be changed until it doesn't. This prints, per prompt in the current catalog, its streak
across full WITH-arm passes and the trace dirs of the latest pass's failing runs. Spawns
no agents; safe to run any time.

    pixi run claude-md-eval-analyse
    pixi run claude-md-eval-analyse --passes 6        # longer history window

Verdicts:
- `closed`   — 3/3 on the last `RETIRE_STREAK` full passes: weakness gone, retire the probe.
- `passing`  — 3/3 now, streak still short of retirement.
- `drift`    — mixed: sometimes reaches the canonical, sometimes not. Read the traces.
- `failing`  — 0/N: Claude does not get there from this setup. Read the traces for why,
               then change the setup (CLAUDE.md section, inventory, helper, hook, ratchet).
- `errored`  — every run errored; the eval broke, not the setup.
- `new`      — in the catalog but no full pass has run it yet.
"""
from __future__ import annotations

import argparse
import datetime as _dt
import importlib.util as _importlib_util
import os
import pathlib
import shutil
import subprocess
import sys
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]
_PROMPTS_DIR = _REPO / "scripts" / "claude_md_eval" / "prompts"

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import read_events  # noqa: E402
from cecelia.effectiveness.log import default_log_path  # noqa: E402

# Full-pass selection lives in the rollup — one definition of "a full pass" for both views.
_spec = _importlib_util.spec_from_file_location(
    "claude_md_eval_rollup", _REPO / "scripts" / "claude_md_eval" / "rollup.py")
_rollup = _importlib_util.module_from_spec(_spec)
_spec.loader.exec_module(_rollup)

RETIRE_STREAK = 3
# Pre-flight for the eval itself (did CLAUDE.md load?) — not a weakness probe, never retires.
_INFRA_PROMPTS = {"canary"}


def _parse_ts(ts: str) -> _dt.datetime:
    try:
        return _dt.datetime.fromisoformat(ts.replace("Z", "+00:00"))
    except ValueError:
        return _dt.datetime.min.replace(tzinfo=_dt.timezone.utc)


def _counts(per_prompt: dict, pid: str) -> tuple[int, int, int] | None:
    r = per_prompt.get(pid)
    if not r:
        return None
    c, n, e = r.get("compliant", 0), r.get("noncompliant", 0), r.get("error", 0)
    return (c, n, e) if c + n + e else None


def prompt_verdicts(events: _t.Sequence[dict], catalog: _t.Sequence[str],
                    *, passes: int = 6) -> list[dict]:
    """One dict per catalog prompt: verdict, per-pass history (newest first), trace dirs."""
    full = [s for s in _rollup._suites(events)
            if _rollup._suite_arm(s) == "with" and _rollup._is_full_pass(s)][:passes]
    latest = full[0] if full else None
    # Run rows share the suite row's session id (`_ensure_eval_session`) and land inside
    # its `duration_s` window — both, because a pass launched from an interactive Claude
    # Code session inherits that session's id, shared with any other pass run from it.
    traces: dict[str, list[str]] = {}
    if latest is not None:
        end = _parse_ts(latest.get("ts", ""))
        start = end - _dt.timedelta(seconds=(latest.get("payload", {}) or {}).get("duration_s", 0) + 60)
        for e in events:
            p = e.get("payload", {}) or {}
            if (e.get("event") == "claude_md_eval_run" and p.get("trace_dir")
                    and e.get("session") == latest.get("session")
                    and start <= _parse_ts(e.get("ts", "")) <= end
                    and p.get("verdict") != "compliant"):
                traces.setdefault(p.get("prompt_id"), []).append(p["trace_dir"])

    out = []
    for pid in catalog:
        history = [_counts((s.get("payload", {}) or {}).get("per_prompt") or {}, pid)
                   for s in full]
        ran = [h for h in history if h is not None]
        now = history[0] if history else None
        if now is None:
            verdict = "new"
        else:
            c, n, e = now
            total = c + n + e
            if e == total:
                verdict = "errored"
            elif c == total:
                streak = 0
                for h in history:
                    if h is None or h[0] != sum(h):
                        break
                    streak += 1
                verdict = ("closed" if streak >= RETIRE_STREAK and pid not in _INFRA_PROMPTS
                           else "passing")
            elif c == 0:
                verdict = "failing"
            else:
                verdict = "drift"
        out.append({"prompt_id": pid, "verdict": verdict, "history": history,
                    "passes_run": len(ran), "traces": sorted(traces.get(pid, []))})
    return out


def _fmt_history(history: list) -> str:
    return " ".join("—" if h is None else f"{h[0]}/{sum(h)}" for h in history) or "—"


def _cron_log_dir() -> pathlib.Path:
    """Same resolution as `cron_pass.sh`: `CECELIA_EVAL_CRON_LOG_DIR`, else `cron/` beside the log."""
    raw = os.environ.get("CECELIA_EVAL_CRON_LOG_DIR")
    return pathlib.Path(raw).expanduser() if raw else default_log_path().parent / "cron"


def _cron_status() -> list[str]:
    lines = []
    cron_dir = _cron_log_dir()
    logs = sorted(cron_dir.glob("eval-*.log")) if cron_dir.is_dir() else []
    if logs:
        tail = logs[-1].read_text(encoding="utf-8", errors="replace").splitlines()
        finished = any("cron pass finished" in ln for ln in tail)
        lines.append(f"  last cron log: {logs[-1]} ({'finished' if finished else 'DID NOT FINISH'})")
        if not finished:
            lines += [f"    {ln}" for ln in tail[-5:]]
    systemctl = shutil.which("systemctl")
    if systemctl:
        res = subprocess.run([systemctl, "--user", "list-timers", "--all", "claude-md-eval*"],
                             capture_output=True, text=True, check=False, encoding="utf-8")
        timers = [ln for ln in res.stdout.splitlines() if "claude-md-eval" in ln]
        lines += [f"  timer: {ln.strip()}" for ln in timers] or ["  timer: none scheduled"]
    return lines


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--passes", type=int, default=6, help="Full passes of history to show.")
    args = ap.parse_args(argv)

    events = list(read_events())
    catalog = sorted(p.stem for p in _PROMPTS_DIR.glob("*.md"))
    full = [s for s in _rollup._suites(events)
            if _rollup._suite_arm(s) == "with" and _rollup._is_full_pass(s)]

    print("=== Cron ===")
    print("\n".join(_cron_status()) or "  no cron logs")
    print()
    if full:
        s = full[0]
        t = (s.get("payload", {}) or {}).get("totals") or {}
        print(f"=== Latest full pass: {_rollup._fmt_ts(s.get('ts', ''))} UTC · "
              f"blob {_rollup._short_sha(s.get('commit'))} · "
              f"${t.get('cost_usd', 0):.2f} ===")
    else:
        print("=== No full pass logged yet ===")
    print(f"  history = newest first, over the last {args.passes} full passes")
    print()
    order = {"errored": 0, "failing": 1, "drift": 2, "new": 3, "passing": 4, "closed": 5}
    rows = sorted(prompt_verdicts(events, catalog, passes=args.passes),
                  key=lambda r: (order[r["verdict"]], r["prompt_id"]))
    for r in rows:
        print(f"  {r['verdict']:<8} {r['prompt_id']:<26} {_fmt_history(r['history'])}")
        for tr in r["traces"]:
            print(f"             trace: {tr}")
    print()
    print("  failing/drift → read the traces: why didn't Claude get there? Fix the setup")
    print("                  (CLAUDE.md section, inventory, helper, hook, ratchet) — not the probe.")
    print(f"  closed        → {RETIRE_STREAK} clean full passes in a row: retire to prompts/retired/.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
