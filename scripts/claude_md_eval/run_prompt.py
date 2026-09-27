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
    for required in ("id", "rule", "compliant_signal", "anti_signal"):
        if required not in meta:
            raise PromptParseError(f"{path}: frontmatter missing required key {required!r}")
    return meta, body.strip()


def score_diff(diff: str, meta: dict[str, str]) -> tuple[str, int, int]:
    """Apply the compliant/anti regexes to the diff. Returns (outcome, compliant_hits, anti_hits).

    outcome ∈ {compliant, noncompliant, error}. `error` is reserved for empty-diff and spawn
    failures upstream; this function returns compliant/noncompliant only.
    """
    compliant_hits = len(re.findall(meta["compliant_signal"], diff))
    anti_hits = len(re.findall(meta["anti_signal"], diff))
    if anti_hits > 0:
        outcome = "noncompliant"
    elif compliant_hits > 0:
        outcome = "compliant"
    else:
        # Neither signal matched — the agent didn't touch the rule's surface at all. Treat as
        # noncompliant with zero hits; the summary makes it visible.
        outcome = "noncompliant"
    return outcome, compliant_hits, anti_hits


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
                          timeout: int, claude_path: str | None) -> subprocess.CompletedProcess:
    """Real `claude -p` invocation. Injectable via CLI for tests.

    Deliberately raises `ClaudeSpawnError` with a clear message when `claude_path` is empty —
    silently falling back to a bare `"claude"` string (which then errors with the less legible
    `[Errno 2] No such file or directory: 'claude'`) is the Windows-compat pitfall documented
    in `CLAUDE.md → Windows compatibility`. Recital's sibling `_resolve_claude_bin` raises for
    the same reason; when a third `claude -p` caller lands, extract these into a shared
    `resolve_claude_bin()` helper (finding conv-97a704d6 from this PR's convention check).
    """
    if not claude_path:
        raise ClaudeSpawnError(
            "no `claude` binary found on PATH — pass --claude-path=/path/to/claude, or install "
            "claude. See CLAUDE.md → Windows compatibility."
        )
    return subprocess.run(
        [claude_path, "-p", "--dangerously-skip-permissions"],
        input=prompt_body, cwd=str(worktree),
        capture_output=True, text=True, timeout=timeout, check=False, encoding="utf-8",
    )


def _capture_diff(worktree: pathlib.Path) -> str:
    """Every file change the agent made — including new files.

    `git diff HEAD` alone omits untracked files, so a prompt that asks the agent to CREATE a
    module (typical — most rules-under-test involve a new helper somewhere) would score zero
    even when the agent wrote a perfect file. Stage first (`git add -A`), then diff the index
    against HEAD, which captures new file content the same as modifications. Safe because the
    worktree is thrown away right after this call.
    """
    subprocess.run(
        ["git", "add", "-A"],
        cwd=str(worktree), capture_output=True, text=True, timeout=30.0, check=False, encoding="utf-8",
    )
    result = subprocess.run(
        ["git", "diff", "--cached", "HEAD"],
        cwd=str(worktree), capture_output=True, text=True, timeout=30.0, check=False, encoding="utf-8",
    )
    if result.returncode != 0:
        return ""
    return result.stdout


def _make_detached_worktree(primary_repo: pathlib.Path, worktree_root: pathlib.Path,
                            prompt_id: str) -> pathlib.Path:
    """Create a detached worktree off HEAD at `<worktree_root>/cecelia-eval-<prompt_id>-<uuid>`."""
    tag = uuid.uuid4().hex[:8]
    dest = worktree_root / f"cecelia-eval-{prompt_id}-{tag}"
    subprocess.run(
        ["git", "worktree", "add", "--detach", str(dest), "HEAD"],
        cwd=str(primary_repo), check=True, capture_output=True, text=True, encoding="utf-8",
    )
    env_src = primary_repo / ".env"
    if env_src.is_file():
        shutil.copy(str(env_src), str(dest / ".env"))
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
                   worktree_root: pathlib.Path, keep_worktrees: bool,
                   claude_runner: _t.Callable[..., subprocess.CompletedProcess] | None = None,
                   primary_repo: pathlib.Path | None = None) -> list[dict]:
    """Run one prompt N times, emit rows to the effectiveness log, return the row list."""
    primary_repo = primary_repo or _REPO
    claude_runner = claude_runner or default_claude_runner
    prompt_path = _PROMPTS_DIR / f"{prompt_id}.md"
    if not prompt_path.is_file():
        raise PromptParseError(f"no prompt at {prompt_path}")
    meta, body = parse_prompt(prompt_path)
    blob_sha = claude_md_blob_sha(primary_repo)

    rows: list[dict] = []
    for run_number in range(1, runs + 1):
        worktree = _make_detached_worktree(primary_repo, worktree_root, prompt_id)
        start = time.monotonic()
        diff = ""
        error: str | None = None
        try:
            proc = claude_runner(worktree, body, timeout=timeout, claude_path=claude_path)
            if proc.returncode != 0:
                error = f"claude exited {proc.returncode}: {(proc.stderr or '')[:400]}"
            diff = _capture_diff(worktree)
        except subprocess.TimeoutExpired:
            error = f"claude spawn timed out after {timeout}s"
        except (OSError, ClaudeSpawnError) as e:
            error = f"{type(e).__name__}: {e}"
        duration = time.monotonic() - start
        if error is not None:
            outcome, compliant_hits, anti_hits = "error", 0, 0
        else:
            outcome, compliant_hits, anti_hits = score_diff(diff, meta)

        payload: dict = {
            "prompt_id": prompt_id,
            "rule": meta["rule"],
            # `verdict` (not `outcome`) — `outcome` is a reserved payload key validated against
            # `OUTCOME_VOCABULARY` in log.py; eval verdicts (`compliant`/`noncompliant`/`error`)
            # are a separate closed set that must not collide with the reviewer vocab.
            "verdict": outcome,
            "compliant_hits": compliant_hits,
            "anti_hits": anti_hits,
            "diff_bytes": len(diff.encode("utf-8")),
            "duration_s": round(duration, 2),
            "run_number": run_number,
            "runs_total": runs,
        }
        if error is not None:
            payload["error"] = error
        row = append_event("claude_md_eval_run", payload, commit=blob_sha)
        rows.append(row)
        print(f"  run {run_number}/{runs}: {outcome} "
              f"(compliant={compliant_hits}, anti={anti_hits}, diff={payload['diff_bytes']}b, "
              f"{payload['duration_s']}s)", flush=True)
        if not keep_worktrees:
            _remove_worktree(primary_repo, worktree)
        elif error is None:
            print(f"    worktree kept at {worktree}", flush=True)

    # Pass-level summary row.
    outcomes = [r["payload"]["verdict"] for r in rows]
    summary_payload = {
        "prompt_id": prompt_id,
        "rule": meta["rule"],
        "runs_total": runs,
        "compliant": outcomes.count("compliant"),
        "noncompliant": outcomes.count("noncompliant"),
        "error": outcomes.count("error"),
    }
    append_event("claude_md_eval_pass", summary_payload, commit=blob_sha)
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
        print(f"compliant: {meta['compliant_signal']}")
        print(f"anti:      {meta['anti_signal']}")
        print(f"task ({len(body)} chars):\n{body}")
        return 0

    print(f"claude-md-eval-one: prompt={args.prompt_id} runs={args.runs} "
          f"timeout={args.timeout}s claude={args.claude_path}", flush=True)
    rows = run_one_prompt(
        args.prompt_id, runs=args.runs, timeout=args.timeout,
        claude_path=args.claude_path, worktree_root=args.worktree_root,
        keep_worktrees=args.keep_worktrees,
    )
    _print_summary(rows)
    return 0


if __name__ == "__main__":
    sys.exit(main())
