#!/usr/bin/env python3
"""`judge-review --auto` — work the bugs verify called `fix` without the owner, one PR per session.

One bundle (`review.bundles`) per session, sessions one after another. The agent never holds a
credential: this script does what touches the outside world, the agent only edits.

1. `pixi run bootstrap-worktree fix-<key>` off a fresh origin/main (pixi and npm need the network).
2. The fix agent, in that worktree, under `agent_sandbox.SANDBOX_SETTINGS` minus its blanket
   write-deny on `~` (the worktree is a sibling checkout under `~`): `acceptEdits` keeps file edits
   in the worktree, the sandbox keeps Bash's writes there, and nothing reaches the network,
   `~/.config/gh` or `~/.ssh`. It confirms, fixes, tests, and reports per bug.
3. This script stages the worktree and runs `pixi run recital` (its reviewers are `claude -p` runs,
   which the sandbox can't make), then resumes the agent with the body: it acts on the findings
   and returns a `[slug: outcome]` tag per finding, as CLAUDE.md asks of any commit.
4. This script commits (the commit-msg hook checks the tags as for any commit), pushes, and opens
   the PR: a draft when the agent says the fix changes what is drawn on screen. Never a merge.

A fixed bug gets the `fix_session` event a `fix now` gets (its note names the PR); a bug the agent
called `not_real` or `left` is logged in `auto_fix.jsonl` with why, stays in the queue, shows the
reason on its card, and isn't tried again. A session that fails logs nothing per bug, so the next
run tries it again; a commit or push that fails leaves the worktree for the owner.
"""
from __future__ import annotations

import copy
import importlib.util as _importlib_util
import json
import os
import pathlib
import re
import subprocess
import sys
import tempfile
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import agent_sandbox  # noqa: E402
from cecelia.effectiveness.claude_cli import failure_text, read_result, resolve_claude_bin  # noqa: E402


def _load_review():
    spec = _importlib_util.spec_from_file_location("_judge_review", pathlib.Path(__file__).parent / "review.py")
    mod = _importlib_util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_rv = _load_review()

#: What the agent reports per bug: `fixed` (in its diff), `not_real` (nothing to fix) or `left`.
OUTCOMES = ("fixed", "not_real", "left")
FIX_SCHEMA = {
    "type": "object",
    "properties": {
        "bugs": {"type": "array", "items": {
            "type": "object",
            "properties": {"key": {"type": "string"}, "outcome": {"enum": list(OUTCOMES)},
                           "reason": {"type": "string"}},
            "required": ["key", "outcome", "reason"], "additionalProperties": False}},
        "title": {"type": "string"},
        "summary": {"type": "string"},
        "draft": {"type": "boolean"}},
    "required": ["bugs", "title", "summary", "draft"], "additionalProperties": False}
TAGS_SCHEMA = {
    "type": "object",
    "properties": {"tags": {"type": "array", "items": {"type": "string"}}},
    "required": ["tags"], "additionalProperties": False}
TOOLS = "Read,Edit,Write,Glob,Grep,Bash"
SESSION_USD = 10.0   # each phase's budget
TIMEOUT_SEC = 3 * 3600
#: A credential in the staged diff: the agent's sandbox can't reach the network, but what it writes
#: into the worktree, this script pushes. GitHub, Anthropic and SSH-key shapes.
SECRET_RE = re.compile(r"\b(?:gh[pousr]_[A-Za-z0-9]{30,}|github_pat_[A-Za-z0-9_]{30,}|sk-ant-[A-Za-z0-9_-]{20,})"
                       r"|-----BEGIN [A-Z ]*PRIVATE KEY-----")
TRAILER = "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
PR_FOOTER = "🤖 Generated with [Claude Code](https://claude.com/claude-code)"


def _home_rule(path: pathlib.Path) -> str:
    """`path` as a permission-rule path: `~/…` under home, `//…` (absolute) otherwise."""
    try:
        return "~/" + path.resolve().relative_to(pathlib.Path.home().resolve()).as_posix()
    except ValueError:
        return "/" + path.resolve().as_posix()


def settings(main: pathlib.Path) -> dict:
    """The fix agent's fence: the verify sandbox, without the write-deny on all of `~` (its worktree
    is under `~`; `acceptEdits` keeps edits in it), with writes to the main checkout and the dotfiles
    denied by name as a second fence."""
    s = copy.deepcopy(agent_sandbox.SANDBOX_SETTINGS)
    keep = [d for d in s["permissions"]["deny"] if not d.endswith("(~/**)")]
    named = [_home_rule(main) + "/**", "~/.claude/**", "~/.ssh/**", "~/.gnupg/**", "~/.config/**",
             "~/.julia/environments/**", "~/.bashrc", "~/.profile", "~/.gitconfig"]
    s["permissions"]["deny"] = keep + [f"{tool}({p})" for tool in ("Write", "Edit", "NotebookEdit") for p in named]
    return s


def fix_brief(record: dict, bugs: _t.Sequence[dict], tree: pathlib.Path, branch: str) -> str:
    """Phase 1: the bugs as a `fix now` brief has them, then confirm, fix and test in `tree`."""
    one = len(bugs) == 1
    lines = [f"Work {'bug ' + bugs[0]['id'] if one else f'these {len(bugs)} bugs'} from {_rv._record_line(record)}. "
             "You are running unattended: nobody will answer a question, so decide and carry on."
             + ("" if one else " They share a reviewed branch or a file, so their fixes likely touch the same code.")]
    for b in bugs:
        lines += ["", f"## {b['id']} (`{b['key']}`)", *_rv._bug_lines(b)]
    lines += ["", f"You are in `{tree}`, a worktree on branch `{branch}` fresh off origin/main, with its own pixi "
              "env and frontend deps. Work only there. There is no network, and you can't commit: git writes "
              "and the PR are done for you once you finish.",
              "", "Confirm each bug here first. One that isn't real: change nothing for it and report it "
              "`not_real` with why. Fix the real ones, including every sibling call site with the same shape; "
              "add a test per fix that fails without it (for a duplicate, make sure the helper's tests cover "
              "what the copy did), and run the matching tests. One you can't finish (it needs a decision this "
              "brief doesn't settle, or a fix you can't test): undo your changes for it and report it `left` "
              "with why, rather than guess.",
              "", "Report every key. `title` is the commit's subject line; `summary` is its body: what was wrong "
              "and what changed, naming each fixed bug's key. Set `draft` if the fix changes what is drawn on "
              "screen: the owner looks at those before they merge."]
    return "\n".join(lines)


def tags_brief(recital: str) -> str:
    """Phase 2: the recital of the staged diff; act on it and tag each finding."""
    return ("Your changes are staged and `pixi run recital` reviewed them. Its body:\n\n" + recital + "\n\n"
            "Act on the findings as CLAUDE.md → *Git & commits* says: fix what should be fixed (in the same "
            "worktree, then re-run the matching tests). Then return one `[<slug>: <outcome>]` tag per "
            "outcome-tagged finding (copy each slug verbatim; `fixed_pre_commit`, or "
            "`shipped_with_finding` / `false_positive` / `dropped_no_action` with `: <reason>`). Return an "
            "empty list if it has none.")


def default_session(prompt: str, cwd: pathlib.Path, schema: dict, *, resume: str | None = None,
                    main: pathlib.Path, budget_usd: float = SESSION_USD,
                    timeout: float = TIMEOUT_SEC) -> tuple[dict | None, float, str | None, str | None]:
    """One fenced `claude -p` in `cwd`. (structured answer, cost, session id, failure); the answer is
    None when it failed. Raises `RateLimited` when the seat is out of quota."""
    claude = resolve_claude_bin()
    if not claude:
        return None, 0.0, None, "claude CLI not on PATH"
    argv = [claude, "-p", "--permission-mode", "acceptEdits", "--settings", json.dumps(settings(main)),
            "--tools", TOOLS, "--strict-mcp-config", "--output-format", "json",
            "--max-budget-usd", str(budget_usd), "--json-schema", json.dumps(schema)]
    if resume:
        argv += ["--resume", resume]
    try:
        proc = subprocess.run(argv, input=prompt, cwd=str(cwd), env={**os.environ, "CECELIA_OBSERVER_NO_PAIR": "1"},
                              capture_output=True, text=True, encoding="utf-8", timeout=timeout, check=False)
    except (OSError, subprocess.TimeoutExpired) as e:
        return None, 0.0, None, f"session failed: {e}"
    out = read_result(proc)
    answer = out.get("structured_output")
    cost = float(out.get("total_cost_usd") or 0.0)
    if proc.returncode != 0 or out.get("is_error") or not isinstance(answer, dict):
        return None, cost, out.get("session_id"), f"session failed (exit {proc.returncode}): {failure_text(proc, out)}"
    return answer, cost, out.get("session_id"), None


def default_sh(argv: _t.Sequence[str], cwd: pathlib.Path, *, timeout: float = 3600) -> subprocess.CompletedProcess:
    return subprocess.run(list(argv), cwd=str(cwd), capture_output=True, text=True, encoding="utf-8",
                          timeout=timeout, check=False)


def _tail(proc: subprocess.CompletedProcess) -> str:
    return (proc.stderr or proc.stdout or "").strip()[-400:]


def commit_message(record: dict, bugs: _t.Sequence[dict], answer: dict, tags: _t.Sequence[str], recital: str) -> str:
    fixed = [b for b in bugs if b["key"] in _fixed_keys(answer)]
    refs = [f"Refs #{_rv._issue_of(b)}" for b in fixed if _rv._issue_of(b)]
    head = [answer["title"].strip(), "", answer["summary"].strip(), "",
            "Unattended fix from the weekly judge record of " + record["date"] + " (`judge-review --auto`): "
            + ", ".join(b["key"] for b in fixed) + "."]
    return "\n".join([*head, *([""] + refs if refs else []), *([""] + list(tags) if tags else []),
                      "", recital.strip(), "", TRAILER]) + "\n"


def pr_body(record: dict, bugs: _t.Sequence[dict], answer: dict) -> str:
    said = {r["key"]: r for r in answer["bugs"]}
    rows = [f"- `{b['key']}` ({b['id']}): **{said.get(b['key'], {}).get('outcome', 'left')}** — "
            f"{said.get(b['key'], {}).get('reason', '')}" for b in bugs]
    return "\n".join([answer["summary"].strip(), "", f"From the weekly judge record of {record['date']}, "
                      "worked unattended by `pixi run judge-review --auto`. Nothing here is merged until you merge it.",
                      "", *rows, "", PR_FOOTER]) + "\n"


def _fixed_keys(answer: dict) -> set[str]:
    return {r["key"] for r in answer.get("bugs", []) if r.get("outcome") == "fixed"}


def _log(record: dict, bugs: _t.Sequence[dict], answer: dict, pr: str, auto_log: pathlib.Path | None,
         *, fixed_as: str | None = None) -> None:
    """One `auto_fix.jsonl` row per bug. `fixed_as`: what a fixed bug becomes when its PR didn't open (`left`)."""
    said = {r.get("key"): r for r in answer.get("bugs", [])}
    for b in bugs:
        r = said.get(b["key"]) or {"outcome": "left", "reason": "the session didn't report it"}
        outcome = r.get("outcome") if r.get("outcome") in OUTCOMES else "left"
        reason = r.get("reason") or ""
        if outcome == "fixed" and fixed_as:
            outcome, reason = fixed_as, f"{reason} — then {pr}"
        _rv.append_auto({"record": record["date"], "ref": b["id"], "key": b["key"], "outcome": outcome,
                         "reason": reason, "pr_url": pr if outcome == "fixed" else ""}, auto_log)


def work_one(record: dict, bugs: _t.Sequence[dict], *, main: pathlib.Path, out: _t.TextIO,
             session: _t.Callable = default_session, sh: _t.Callable = default_sh,
             path: pathlib.Path | None = None, auto_log: pathlib.Path | None = None) -> float:
    """One bundle through to a PR. Returns what its sessions spent."""
    name = ("fix-" if len(bugs) == 1 else "fix-bundle-") + bugs[0]["key"]
    branch = name
    tree = main.parent / f"cecelia-{name}"
    say = lambda msg: print(f"  {msg}", file=out, flush=True)   # noqa: E731
    if tree.exists():
        say(f"{tree} exists already: skipped (remove it, or finish it by hand)")
        return 0.0
    made = sh(["pixi", "run", "bootstrap-worktree", name, branch, "origin/main"], main)
    if made.returncode != 0:
        say(f"bootstrap-worktree failed: {_tail(made)}")
        return 0.0
    answer, spent, sid, failed = session(fix_brief(record, bugs, tree, branch), tree, FIX_SCHEMA, main=main)
    if failed:
        say(f"{failed} (${spent:.2f}); worktree left at {tree}")
        return spent
    for r in answer["bugs"]:
        say(f"{r['key']}: {r['outcome']} — {r['reason']}")
    if not _fixed_keys(answer):
        _log(record, bugs, answer, "", auto_log)
        agent_sandbox.remove_worktree(main, tree)
        sh(["git", "branch", "-D", branch], main)
        return spent
    sh(["git", "add", "-A"], tree)
    rec = sh(["pixi", "run", "recital"], tree)
    recital = rec.stdout or ""
    if "_Recital stamp:" not in recital:
        _log(record, bugs, answer, "", auto_log, fixed_as="left")
        say(f"recital failed: {_tail(rec)}; worktree left at {tree}")
        return spent
    tagged, cost, _, failed = session(tags_brief(recital), tree, TAGS_SCHEMA, resume=sid, main=main)
    spent += cost
    if failed:
        _log(record, bugs, answer, "", auto_log, fixed_as="left")
        say(f"tagging the recital: {failed}; worktree left at {tree}")
        return spent
    sh(["git", "add", "-A"], tree)
    if SECRET_RE.search(sh(["git", "diff", "--cached"], tree).stdout or ""):
        _log(record, bugs, answer, "", auto_log, fixed_as="left")
        say(f"the staged diff holds what looks like a credential: not committed; worktree left at {tree}")
        return spent
    with tempfile.NamedTemporaryFile("w", encoding="utf-8", suffix=".txt", delete=False) as fh:
        fh.write(commit_message(record, bugs, answer, tagged["tags"], recital))
    try:
        steps = [["git", "commit", "-F", fh.name], ["git", "push", "-u", "origin", branch]]
        for argv in steps:
            done = sh(argv, tree)
            if done.returncode != 0:
                _log(record, bugs, answer, "", auto_log, fixed_as="left")
                say(f"`{' '.join(argv[:2])}` failed: {_tail(done)}; worktree left at {tree}")
                return spent
    finally:
        os.unlink(fh.name)
    with tempfile.NamedTemporaryFile("w", encoding="utf-8", suffix=".md", delete=False) as fh:
        fh.write(pr_body(record, bugs, answer))
    try:
        opened = sh(["gh", "pr", "create", "--base", "main", "--head", branch, "--title", answer["title"].strip(),
                     "--body-file", fh.name, *(["--draft"] if answer["draft"] else [])], tree)
    finally:
        os.unlink(fh.name)
    if opened.returncode != 0:
        _log(record, bugs, answer, "", auto_log, fixed_as="left")
        say(f"gh pr create failed: {_tail(opened)}; branch {branch} is pushed")
        return spent
    pr = (opened.stdout or "").strip().splitlines()[-1]
    _log(record, bugs, answer, pr, auto_log)
    for b in bugs:
        if b["key"] in _fixed_keys(answer):
            _rv.append_review("bug_work", record["date"], b["id"], "fix_session", note=f"auto PR {pr}", path=path)
    say(f"PR{' (draft)' if answer['draft'] else ''}: {pr}")
    return spent


def run(record: dict, *, limit: int = 1, dry_run: bool = False, out: _t.TextIO = sys.stdout,
        path: pathlib.Path | None = None, auto_log: pathlib.Path | None = None, older: _t.Sequence[dict] = (),
        main: pathlib.Path | None = None, **kw) -> int:
    """Up to `limit` bundles from `review.auto_items`, one after another. Returns the sessions run."""
    main = main or _rv.main_checkout()
    items = _rv.auto_items(record, _rv.read_reviews(path), older, _rv.read_auto(auto_log))
    by_id = {b["id"]: b for b in record.get("bugs", [])}
    n = sum(len(it.get("refs") or [it["ref"]]) for it in items)
    print(f"judge-review --auto: {record['date']} · {n} fix bug(s) in {len(items)} session(s) · running "
          f"{min(limit, len(items))}" + (" (dry run)" if dry_run else ""), file=out)
    spent, ran = 0.0, 0
    if items[:limit] and not dry_run:
        fetched = (kw.get("sh") or default_sh)(["git", "fetch", "-q", "origin"], main)
        if fetched.returncode != 0:
            print(f"  git fetch failed: {_tail(fetched)}", file=out)
            return 0
    for item in items[:limit]:
        bugs = [by_id[r] for r in item.get("refs") or [item["ref"]]]
        label = " + ".join(f"{b['id']} {b['key']}" for b in bugs)
        if dry_run:
            name = ("fix-" if len(bugs) == 1 else "fix-bundle-") + bugs[0]["key"]
            print(f"\n── {label} ──\n" + fix_brief(record, bugs, main.parent / f"cecelia-{name}", name), file=out)
            continue
        print(f"{label}:", file=out, flush=True)
        spent += work_one(record, bugs, main=main, out=out, path=path, auto_log=auto_log, **kw)
        ran += 1
    if not dry_run:
        left = len(items) - ran
        print(f"{ran} session(s) · ${spent:.2f} spent" + (f" · {left} left: run again" if left > 0 else ""), file=out)
    return ran
