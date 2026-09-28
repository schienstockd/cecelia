#!/usr/bin/env python3
"""CLAUDE.md compliance eval — P1 runner (one prompt, N fresh agents).

Design + rationale: docs/todo/CLAUDE_MD_EVAL_PLAN.md → *Runner*.

Given a prompt id, spawns N fresh `claude -p` agents in isolated detached worktrees off the
current HEAD, captures each resulting diff, scores it by regex against the prompt's declared
`compliant_signal` / `anti_signal`, and emits one `claude_md_eval_run` row per (prompt, run)
to the effectiveness log plus one `claude_md_eval_pass` summary row per invocation.

Row-level `commit` is the CLAUDE.md **blob** SHA — content-addressed, so the trend baseline
for a prompt only moves when CLAUDE.md itself actually changes, not on every commit.

Runs sequentially — no parallelism in P1. `claude -p` calls are ~30s–3min each; three runs is
about 15 minutes of wall clock. Timeout defaults to 300s per spawn; a timed-out run emits an
`error` row rather than crashing the pass.

Usage:
    pixi run claude-md-eval-one <prompt_id>              # 3 runs (default)
    pixi run claude-md-eval-one <prompt_id> --runs 5
    python scripts/claude_md_eval/run_prompt.py <prompt_id> --dry-run     # parse + print only
"""
from __future__ import annotations

import argparse
import os
import pathlib
import re
import shutil
import subprocess
import sys
import time
import typing as _t
import uuid

_REPO = pathlib.Path(__file__).resolve().parents[2]
_PROMPTS_DIR = _REPO / "scripts" / "claude_md_eval" / "prompts"
_WORKTREE_ROOT_DEFAULT = _REPO.parent  # sibling of the primary checkout, per project convention
_DEFAULT_TIMEOUT_SEC = 300
_DEFAULT_RUNS = 3

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import append_event  # noqa: E402
from cecelia.effectiveness.git_context import current_branch as _current_branch  # noqa: E402

# Sibling module — parses `claude -p --output-format=stream-json --verbose` stdout for
# ordered tool_calls + cost/turns. Spec-loaded so this script works whether the
# `scripts/claude_md_eval/` dir is on sys.path or not.
import importlib.util as _importlib_util  # noqa: E402
_transcript_spec = _importlib_util.spec_from_file_location(
    "_ce_transcript", pathlib.Path(__file__).parent / "transcript.py")
_transcript = _importlib_util.module_from_spec(_transcript_spec)
_transcript_spec.loader.exec_module(_transcript)
parse_stream_json = _transcript.parse_stream_json
TranscriptSignals = _transcript.TranscriptSignals

_FRONTMATTER_RE = re.compile(r"^---\n(.*?)\n---\n(.*)$", re.DOTALL)


class PromptParseError(ValueError):
    pass


class ClaudeSpawnError(RuntimeError):
    pass


def parse_prompt(path: pathlib.Path) -> tuple[dict[str, str], str]:
    """Return (frontmatter_dict, task_body). Frontmatter is `key: value` lines, quoted or bare."""
    text = path.read_text(encoding="utf-8")
    m = _FRONTMATTER_RE.match(text)
    if not m:
        raise PromptParseError(f"{path}: no `---`-delimited frontmatter block found")
    meta_text, body = m.groups()
    meta: dict[str, str] = {}
    for raw in meta_text.split("\n"):
        line = raw.rstrip()
        if not line or line.lstrip().startswith("#"):
            continue
        if ":" not in line:
            raise PromptParseError(f"{path}: frontmatter line without colon: {line!r}")
        k, _, v = line.partition(":")
        v = v.strip()
        if len(v) >= 2 and v[0] == v[-1] and v[0] in ("'", '"'):
            v = v[1:-1]
        meta[k.strip()] = v
    for required in ("id", "rule"):
        if required not in meta:
            raise PromptParseError(f"{path}: frontmatter missing required key {required!r}")
    # Must have AT LEAST ONE grader declaration — regex-based (compliant/anti signals) or
    # tool-order-based (before/after tool + arg-match). A prompt with neither can't score.
    has_regex = "compliant_signal" in meta or "anti_signal" in meta
    has_tool_order = any(k in meta for k in (
        "tool_order_before_tool", "tool_order_before_tools",
        "tool_order_after_tool", "tool_order_after_tools",
    ))
    if not (has_regex or has_tool_order):
        raise PromptParseError(
            f"{path}: frontmatter must declare at least one grader — either "
            f"`compliant_signal`/`anti_signal` (regex on diff) or "
            f"`tool_order_before_tool`/`tool_order_after_tool` (tool-log order)")
    return meta, body.strip()


def _split_tool_list(raw: str | None) -> list[str]:
    """Comma-separated tool list → clean list. `None`/empty → []."""
    if not raw:
        return []
    return [t.strip() for t in raw.split(",") if t.strip()]


def _additions_only(diff: str) -> str:
    """Reduce a unified diff to just the `+` lines (excluding the `+++ b/foo` file header).

    Rationale: `compliant_signal` / `anti_signal` should score the agent's CHOICE — code the
    agent actually wrote — not incidental text elsewhere in the diff. Two artefacts this
    guards against:

    - **CLAUDE.md deletions in arm=without.** The ablation strips every CLAUDE.md before
      the spawn, so a plain regex on the diff matches the DELETED prose (`use zarr_utils`,
      `never zarr.open`) and reports inflated compliant/anti hits — surfaced by the
      `crop-failure` without-arm pilot on PR #1274.
    - **Pasted anti-pattern snippets.** A prompt that pastes broken code the agent must
      fix (e.g. `crop-failure.md` shows `import zarr` + `zarr.open(...)`). If the file
      is being CREATED, all its content lands as `+` lines regardless of what the agent
      changed — but if the paste is in a pre-existing file the agent edits minimally,
      only the actually-changed lines are `+`, which is the honest read.

    A less-strict alternative (matching on the full diff) would flag agents for prose
    they read but didn't write. The strictest alternative (matching on the resulting file
    only) would miss regressions where the agent replaced canonical code with anti-pattern
    code but added net-zero net lines. Additions-only is the middle ground.
    """
    return "\n".join(
        line for line in diff.splitlines()
        if line.startswith("+") and not line.startswith("+++")
    )


def _regex_hits(diff: str, meta: dict[str, str]) -> tuple[int, int]:
    """Compliant + anti regex hit counts against `diff`. Missing signal → 0 hits.

    Regex runs against the additions-only slice of the diff (see `_additions_only`) —
    scoring the agent's CHOICE, not text elsewhere in the diff.
    """
    additions = _additions_only(diff)
    compliant_signal = meta.get("compliant_signal", "")
    anti_signal = meta.get("anti_signal", "")
    compliant_hits = len(re.findall(compliant_signal, additions)) if compliant_signal else 0
    anti_hits = len(re.findall(anti_signal, additions)) if anti_signal else 0
    return compliant_hits, anti_hits


def score_diff(diff: str, meta: dict[str, str]) -> tuple[str, int, int]:
    """Legacy scorer — kept for tests + any caller not yet threading tool_calls.

    Applies compliant/anti regexes to the diff (via `_regex_hits`). Returns
    (outcome, compliant_hits, anti_hits). Callers with a tool-order grader should
    use `score_all` instead — both share `_regex_hits` so the regex logic can't drift.
    """
    compliant_hits, anti_hits = _regex_hits(diff, meta)
    if anti_hits > 0:
        outcome = "noncompliant"
    elif compliant_hits > 0:
        outcome = "compliant"
    else:
        outcome = "noncompliant"
    return outcome, compliant_hits, anti_hits


def score_all(diff: str, signals: "TranscriptSignals",
              meta: dict[str, str]) -> tuple[str, dict[str, _t.Any]]:
    """Combine every declared grader into a single verdict.

    Compliant iff EVERY declared grader passes:
      - `compliant_signal` regex matches the diff (if declared),
      - `anti_signal` regex does NOT match the diff (if declared),
      - `tool_order_before_tool` fired with matching `tool_order_before_arg_match`
        before the first `tool_order_after_tool` (if declared).

    Returns (verdict, details) where details carries the per-grader outcomes for the
    emitted row (`compliant_hits`, `anti_hits`, `tool_order_passed` — the last is
    None for prompts that don't declare a tool_order grader).
    """
    compliant_hits, anti_hits = _regex_hits(diff, meta)

    tool_order_passed: bool | None = None
    if any(k in meta for k in ("tool_order_before_tool", "tool_order_before_tools",
                                "tool_order_after_tool", "tool_order_after_tools")):
        # Plural keys (`_tools`) accept a comma-separated list of alternatives and win
        # over singular (`_tool`) — the indirect-prompt tier uses lists so a Read of
        # `INVENTORY.md` scores the same as a Grep, and an Edit/MultiEdit is treated
        # as write-shaped alongside Write.
        before = _split_tool_list(meta.get("tool_order_before_tools")) or \
                 meta.get("tool_order_before_tool", "")
        after = _split_tool_list(meta.get("tool_order_after_tools")) or \
                meta.get("tool_order_after_tool", "Write")
        tool_order_passed = signals.tool_order_passes(
            before_tool=before,
            before_arg_match=meta.get("tool_order_before_arg_match", ""),
            after_tool=after,
        )

    graders_pass = []
    if meta.get("compliant_signal"):
        graders_pass.append(compliant_hits > 0)
    if meta.get("anti_signal"):
        graders_pass.append(anti_hits == 0)
    if tool_order_passed is not None:
        graders_pass.append(tool_order_passed)

    verdict = "compliant" if graders_pass and all(graders_pass) else "noncompliant"
    details = {
        "compliant_hits": compliant_hits,
        "anti_hits": anti_hits,
        "tool_order_passed": tool_order_passed,
    }
    return verdict, details


def _ensure_eval_session() -> str:
    """Set `CLAUDE_CODE_SESSION_ID` to a synthetic `eval-<uuid8>` if it's not already set.

    `append_event` falls back to `"unknown"` when the env var is absent, which is the
    default state inside a `pixi run <task>` subprocess — pixi doesn't propagate
    `CLAUDE_CODE_SESSION_ID` from the launching shell. Every `claude_md_eval_*` row
    written before this fix carried `sess=unknown`, breaking one useful signal:
    grouping every row of one eval pass under a single session id. Idempotent — if
    the env var IS set (nested Claude Code session, CI with an explicit id), we
    respect it. Returns the id in effect.
    """
    existing = os.environ.get("CLAUDE_CODE_SESSION_ID")
    if existing:
        return existing
    synthetic = f"eval-{uuid.uuid4().hex[:8]}"
    os.environ["CLAUDE_CODE_SESSION_ID"] = synthetic
    return synthetic


def claude_md_blob_sha(cwd: pathlib.Path) -> str | None:
    """Content-addressed SHA of CLAUDE.md at HEAD. None if not a repo / file missing."""
    try:
        result = subprocess.run(
            ["git", "rev-parse", "HEAD:CLAUDE.md"],
            cwd=cwd, capture_output=True, text=True, timeout=10.0, check=False, encoding="utf-8",
        )
    except (subprocess.TimeoutExpired, OSError):
        return None
    if result.returncode != 0:
        return None
    sha = result.stdout.strip()
    return sha or None


def default_claude_runner(worktree: pathlib.Path, prompt_body: str, *,
                          timeout: int, claude_path: str | None,
                          **_ignored) -> subprocess.CompletedProcess:
    """Real `claude -p` invocation. Injectable via CLI for tests.

    Deliberately raises `ClaudeSpawnError` with a clear message when `claude_path` is empty —
    silently falling back to a bare `"claude"` string (which then errors with the less legible
    `[Errno 2] No such file or directory: 'claude'`) is the Windows-compat pitfall documented
    in `CLAUDE.md → Windows compatibility`. Recital's sibling `_resolve_claude_bin` raises for
    the same reason; when a third `claude -p` caller lands, extract these into a shared
    `resolve_claude_bin()` helper (finding conv-97a704d6 from this PR's convention check).

    Trailing `**_ignored` swallows kwargs that legacy call sites may still pass (e.g.
    the `sandbox_log` kwarg that briefly existed for the log-isolation mechanism before
    it was trimmed as unnecessary — see plan doc *Log-isolation scaffolding removed*).
    """
    if not claude_path:
        raise ClaudeSpawnError(
            "no `claude` binary found on PATH — pass --claude-path=/path/to/claude, or install "
            "claude. See CLAUDE.md → Windows compatibility."
        )
    # `--output-format=stream-json --verbose` — emits one JSON event per line on stdout,
    # including every assistant tool_use block AND a terminal `result` event carrying
    # `total_cost_usd` + `num_turns`. Parsed by `transcript.parse_stream_json` for the
    # tool_order grader + cost/turns emission. Was `--output-format=text` before the
    # 2026-09-28 additions — the text mode discarded tool_use structure.
    return subprocess.run(
        [claude_path, "-p", "--dangerously-skip-permissions",
         "--output-format", "stream-json", "--verbose"],
        input=prompt_body, cwd=str(worktree),
        capture_output=True, text=True, timeout=timeout, check=False, encoding="utf-8",
    )


def _capture_diff(worktree: pathlib.Path) -> str:
    """Every file change the agent made — including new files, EXCLUDING CLAUDE.md files.

    `git diff HEAD` alone omits untracked files, so a prompt that asks the agent to CREATE a
    module (typical — most rules-under-test involve a new helper somewhere) would score zero
    even when the agent wrote a perfect file. Stage first (`git add -A`), then diff the index
    against HEAD, which captures new file content the same as modifications. Safe because the
    worktree is thrown away right after this call.

    `**/CLAUDE.md` is excluded via pathspec: in arm=without runs, `_make_detached_worktree`
    strips every CLAUDE.md before the spawn, so `git diff --cached HEAD` would include those
    deletions and the compliant_signal regex would match the DELETED prose ("... use
    `zarr_utils.` ..." etc.) — reporting `compliant_hits=7` for a run where the agent wrote
    zero references to the canonical util. That's a scoring artefact, not agent behaviour,
    and it would flip verdicts wrong on any prompt where anti_signal doesn't also fire to
    force the noncompliant read. Excluding the deletions from the scored diff keeps
    `compliant_hits` honest across arms. Surfaced by the `crop-failure` without-arm pilot
    (PR #1274 second comment).
    """
    subprocess.run(
        ["git", "add", "-A"],
        cwd=str(worktree), capture_output=True, text=True, timeout=30.0, check=False, encoding="utf-8",
    )
    result = subprocess.run(
        ["git", "diff", "--cached", "HEAD", "--", ".", ":(exclude,glob)**/CLAUDE.md",
         ":(exclude)CLAUDE.md"],
        cwd=str(worktree), capture_output=True, text=True, timeout=30.0, check=False, encoding="utf-8",
    )
    if result.returncode != 0:
        return ""
    return result.stdout


def _make_detached_worktree(primary_repo: pathlib.Path, worktree_root: pathlib.Path,
                            prompt_id: str, *, arm: str = "with") -> pathlib.Path:
    """Create a detached worktree off HEAD at `<worktree_root>/cecelia-eval-<prompt_id>-<uuid>`.

    `arm` controls the CLAUDE.md ablation: `with` keeps every CLAUDE.md (root + nested
    frontend/app), `without` strips them all AFTER the worktree is created but BEFORE
    the agent spawns. This is the bespoke-runner equivalent of the plugin-eval port's
    scaffold-swap — and closer to production semantics because the loading mechanism
    is identical in both arms (Claude Code loads CLAUDE.md from the working dir); we
    just make it absent in the without-arm.
    """
    tag = uuid.uuid4().hex[:8]
    dest = worktree_root / f"cecelia-eval-{prompt_id}-{tag}"
    subprocess.run(
        ["git", "worktree", "add", "--detach", str(dest), "HEAD"],
        cwd=str(primary_repo), check=True, capture_output=True, text=True, encoding="utf-8",
    )
    env_src = primary_repo / ".env"
    if env_src.is_file():
        shutil.copy(str(env_src), str(dest / ".env"))
    if arm == "without":
        # Strip every CLAUDE.md — root, frontend/, app/, and any nested. Skip .git/.
        for p in dest.rglob("CLAUDE.md"):
            if ".git" in p.parts:
                continue
            p.unlink()
    return dest


def _remove_worktree(primary_repo: pathlib.Path, dest: pathlib.Path) -> None:
    """Best-effort — `git worktree remove --force` then rmtree any stragglers."""
    subprocess.run(
        ["git", "worktree", "remove", "--force", str(dest)],
        cwd=str(primary_repo), check=False, capture_output=True, text=True, encoding="utf-8",
    )
    if dest.exists():
        shutil.rmtree(str(dest), ignore_errors=True)


def run_one_prompt(prompt_id: str, *, runs: int, timeout: int, claude_path: str | None,
                   worktree_root: pathlib.Path, keep_worktrees: bool, arm: str = "with",
                   claude_runner: _t.Callable[..., subprocess.CompletedProcess] | None = None,
                   primary_repo: pathlib.Path | None = None) -> list[dict]:
    """Run one prompt N times, emit rows to the effectiveness log, return the row list."""
    primary_repo = primary_repo or _REPO
    claude_runner = claude_runner or default_claude_runner
    _ensure_eval_session()
    prompt_path = _PROMPTS_DIR / f"{prompt_id}.md"
    if not prompt_path.is_file():
        raise PromptParseError(f"no prompt at {prompt_path}")
    meta, body = parse_prompt(prompt_path)
    blob_sha = claude_md_blob_sha(primary_repo)
    # `branch` = the invoker's branch at pixi-run time (usually `main` on cecelia-feijoa).
    # Captured once per prompt; every `_run` and `_pass` row on this pass carries it so the
    # rollup can join to a PR later via `gh pr list --head <branch>` when `pr` is null.
    branch = _current_branch()

    rows: list[dict] = []
    for run_number in range(1, runs + 1):
        worktree = _make_detached_worktree(primary_repo, worktree_root, prompt_id, arm=arm)
        start = time.monotonic()
        diff = ""
        signals = TranscriptSignals()
        error: str | None = None
        try:
            proc = claude_runner(worktree, body, timeout=timeout, claude_path=claude_path)
            if proc.returncode != 0:
                error = f"claude exited {proc.returncode}: {(proc.stderr or '')[:400]}"
            diff = _capture_diff(worktree)
            # Parse stream-json stdout for tool-call sequence + cost/turns. A malformed
            # stream (e.g. an old `claude` binary that ignores the flag) yields zero
            # tool_calls / zero cost — captured as parse_errors on the signals object.
            signals = parse_stream_json(proc.stdout or "")
        except subprocess.TimeoutExpired:
            error = f"claude spawn timed out after {timeout}s"
        except (OSError, ClaudeSpawnError) as e:
            error = f"{type(e).__name__}: {e}"
        duration = time.monotonic() - start
        if error is not None:
            verdict = "error"
            details = {"compliant_hits": 0, "anti_hits": 0, "tool_order_passed": None}
        else:
            verdict, details = score_all(diff, signals, meta)

        payload: dict = {
            "prompt_id": prompt_id,
            "rule": meta["rule"],
            # `verdict` (not `outcome`) — `outcome` is a reserved payload key validated against
            # `OUTCOME_VOCABULARY` in log.py; eval verdicts (`compliant`/`noncompliant`/`error`)
            # are a separate closed set that must not collide with the reviewer vocab.
            "verdict": verdict,
            "compliant_hits": details["compliant_hits"],
            "anti_hits": details["anti_hits"],
            "tool_order_passed": details["tool_order_passed"],
            "arm": arm,
            "diff_bytes": len(diff.encode("utf-8")),
            "duration_s": round(duration, 2),
            "cost_usd": round(signals.cost_usd, 4),
            "turns": signals.turns,
            "run_number": run_number,
            "runs_total": runs,
        }
        if error is not None:
            payload["error"] = error
        row = append_event("claude_md_eval_run", payload, commit=blob_sha, branch=branch)
        rows.append(row)
        tool_bit = ""
        if details["tool_order_passed"] is not None:
            tool_bit = f", tool_order={'pass' if details['tool_order_passed'] else 'FAIL'}"
        print(f"  run {run_number}/{runs} (arm={arm}): {verdict} "
              f"(compliant={details['compliant_hits']}, anti={details['anti_hits']}"
              f"{tool_bit}, ${payload['cost_usd']:.3f}, {payload['turns']} turns, "
              f"{payload['duration_s']}s)", flush=True)
        if not keep_worktrees:
            _remove_worktree(primary_repo, worktree)
        elif error is None:
            print(f"    worktree kept at {worktree}", flush=True)

    # Pass-level summary row.
    outcomes = [r["payload"]["verdict"] for r in rows]
    total_cost = sum(r["payload"].get("cost_usd", 0) for r in rows)
    summary_payload = {
        "prompt_id": prompt_id,
        "rule": meta["rule"],
        "runs_total": runs,
        "arm": arm,
        "compliant": outcomes.count("compliant"),
        "noncompliant": outcomes.count("noncompliant"),
        "error": outcomes.count("error"),
        "total_cost_usd": round(total_cost, 4),
    }
    append_event("claude_md_eval_pass", summary_payload, commit=blob_sha, branch=branch)
    return rows


def _print_summary(rows: list[dict]) -> None:
    if not rows:
        print("no runs — nothing to summarise", file=sys.stderr)
        return
    counts: dict[str, int] = {}
    for r in rows:
        v = r["payload"]["verdict"]
        counts[v] = counts.get(v, 0) + 1
    total = len(rows)
    print("", flush=True)
    print(f"summary — {total} run(s), CLAUDE.md blob {rows[0].get('commit') or '<unknown>'}",
          flush=True)
    for verdict in ("compliant", "noncompliant", "error"):
        if verdict in counts:
            print(f"  {verdict:14s} {counts[verdict]}/{total}", flush=True)


def main() -> int:
    ap = argparse.ArgumentParser(description="Run one CLAUDE.md compliance eval prompt N times.")
    ap.add_argument("prompt_id", help="Prompt id (filename stem under scripts/claude_md_eval/prompts/)")
    ap.add_argument("--runs", type=int, default=_DEFAULT_RUNS,
                    help=f"How many fresh agents to spawn (default {_DEFAULT_RUNS})")
    ap.add_argument("--timeout", type=int, default=_DEFAULT_TIMEOUT_SEC,
                    help=f"Per-spawn timeout in seconds (default {_DEFAULT_TIMEOUT_SEC})")
    ap.add_argument("--claude-path", default=shutil.which("claude"),
                    help="Path to the `claude` binary (defaults to `which claude`; passes as "
                         "None if not found — the runner raises a clear error rather than "
                         "silently falling back to a bare `claude` string)")
    ap.add_argument("--worktree-root", type=pathlib.Path, default=_WORKTREE_ROOT_DEFAULT,
                    help="Parent directory for the throwaway worktrees (default: sibling of repo)")
    ap.add_argument("--keep-worktrees", action="store_true",
                    help="Don't remove worktrees after each run (debugging).")
    ap.add_argument("--arm", choices=("with", "without"), default="with",
                    help="CLAUDE.md ablation arm — `with` keeps CLAUDE.md in the worktree "
                         "(default); `without` strips every CLAUDE.md (root + nested) before "
                         "spawn. Used by `pixi run claude-md-eval-ablation` to run each prompt "
                         "in both arms and compute the per-prompt delta.")
    ap.add_argument("--dry-run", action="store_true",
                    help="Parse the prompt and print the task; do not spawn any agent.")
    args = ap.parse_args()

    prompt_path = _PROMPTS_DIR / f"{args.prompt_id}.md"
    try:
        meta, body = parse_prompt(prompt_path)
    except PromptParseError as e:
        print(f"prompt parse failed: {e}", file=sys.stderr)
        return 2

    if args.dry_run:
        print(f"prompt_id: {meta['id']}")
        print(f"rule:      {meta['rule']}")
        print(f"compliant: {meta.get('compliant_signal', '(none)')}")
        print(f"anti:      {meta.get('anti_signal', '(none)')}")
        if any(k in meta for k in ("tool_order_before_tool", "tool_order_before_tools",
                                    "tool_order_after_tool", "tool_order_after_tools")):
            # Print whichever form is authored — plural list wins over singular so an
            # indirect-tier prompt (which uses only plural keys) shows a real before/after
            # column instead of `?`. Same key-precedence as `score_all`.
            before = _split_tool_list(meta.get("tool_order_before_tools")) or \
                     meta.get("tool_order_before_tool", "?")
            after = _split_tool_list(meta.get("tool_order_after_tools")) or \
                    meta.get("tool_order_after_tool", "Write")
            print(f"tool_order: before={before} "
                  f"arg_match={meta.get('tool_order_before_arg_match', '(none)')} "
                  f"after={after}")
        print(f"task ({len(body)} chars):\n{body}")
        return 0

    print(f"claude-md-eval-one: prompt={args.prompt_id} runs={args.runs} arm={args.arm} "
          f"timeout={args.timeout}s claude={args.claude_path}", flush=True)
    rows = run_one_prompt(
        args.prompt_id, runs=args.runs, timeout=args.timeout,
        claude_path=args.claude_path, worktree_root=args.worktree_root,
        keep_worktrees=args.keep_worktrees, arm=args.arm,
    )
    _print_summary(rows)
    return 0


if __name__ == "__main__":
    sys.exit(main())
