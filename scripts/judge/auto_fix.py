#!/usr/bin/env python3
"""`judge-review --auto` — work the bugs verify called `fix` without the owner, one PR per session.

One bundle (`review.bundles`) per session, sessions one after another. The agent never holds a
credential: this script does what touches the outside world, the agent only edits.

1. `pixi run bootstrap-worktree fix-<key>` off a fresh origin/main (pixi and npm need the network).
2. The fix agent, in that worktree, under `agent_sandbox.SANDBOX_SETTINGS` minus its blanket
   write-deny on `~` (the worktree is a sibling checkout under `~`): `acceptEdits` keeps file edits
   in the worktree, the sandbox keeps Bash's writes there, and nothing reaches the network,
   `~/.config/gh` or `~/.ssh`. It confirms, fixes, tests, and reports per bug.
3. This script stages the worktree and runs the recital on the staged diff (its reviewers are
   `claude -p` runs, which the sandbox can't make), then resumes the agent with the body: it acts on
   the findings and returns a `[slug: outcome]` tag per finding, as CLAUDE.md asks of any commit.
4. This script commits (the commit-msg hook checks the tags as for any commit), pushes, and opens
   the PR: a draft when the agent says the fix changes what is drawn on screen. Never a merge.

What runs outside the sandbox after the agent is this checkout's, never the worktree's: the recital
and the commit-msg hook run from here with this env's python, and git runs with the hooks path
pointed here. Before each of those steps, nothing the agent wrote may reach them: the worktree's
`EXEC_SURFACE` (its `.git` pointer, `.githooks`, `.claude`, `.mcp.json`, tracked or not) must be as
bootstrap left it, and the diff may not touch `PROTECTED` (CI runs a pushed workflow with the repo's
secrets; the rest is the fence and what the next session runs under) or hold a credential.

A fixed bug gets the `fix_session` event a `fix now` gets (its note names the PR); a bug the agent
called `not_real` or `left` is logged in `auto_fix.jsonl` with why, stays in the queue, shows the
reason on its card, and isn't tried again. A session that fails is logged `failed` and its worktree
removed, so the next run tries it again, up to `review.AUTO_ATTEMPTS` sessions; a guard, commit or
push that stops leaves the worktree for the owner, and the bug `left`, so nothing is pushed twice.
"""
from __future__ import annotations

import copy
import hashlib
import importlib.util as _importlib_util
import json
import os
import pathlib
import re
import shlex
import subprocess
import sys
import tempfile
import threading
import time
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import agent_sandbox  # noqa: E402
from cecelia.effectiveness.claude_cli import failure_text, read_result, resolve_claude_bin  # noqa: E402
# one look with `judge-review` and `pixi run recital-console`: dividers, palette, hanging-indent wrap
from cecelia.effectiveness.console import _BOLD, _DIM, _clip, _col, _fmt_duration, _hr, _terminal_size, _wrap_desc  # noqa: E402
from cecelia.effectiveness.palette import BLUISH_GREEN, GREY, ORANGE, SKY_BLUE, VERMILLION  # noqa: E402


def _load_review():
    spec = _importlib_util.spec_from_file_location("_judge_review", pathlib.Path(__file__).parent / "review.py")
    mod = _importlib_util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_rv = _load_review()
_bugs = _rv._load_sibling("bugs")

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
#: What the script runs or reads in the worktree after the agent, outside the sandbox.
EXEC_SURFACE = (".git", ".githooks", ".claude", ".mcp.json")
#: What an unattended PR may not touch (a `/` suffix: the directory; `CLAUDE.md`: at any depth).
PROTECTED = (".github/", ".githooks/", ".claude/", ".mcp.json", "pixi.toml", "pixi.lock", "CLAUDE.md",
             "scripts/judge/auto_fix.py", "python/cecelia/effectiveness/agent_sandbox.py")
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


def snapshot(tree: pathlib.Path) -> dict[str, str]:
    """Each file (or link) of the worktree's `EXEC_SURFACE`, ignored ones included → its hash (or target)."""
    out = {}
    for name in EXEC_SURFACE:
        top = tree / name
        if top.is_symlink() or top.is_file():
            files = [top]
        elif top.is_dir():
            files = sorted(f for f in top.rglob("*") if f.is_symlink() or f.is_file())
        else:
            continue
        for f in files:
            out[f.relative_to(tree).as_posix()] = (f"-> {os.readlink(f)}" if f.is_symlink()
                                                  else hashlib.sha256(f.read_bytes()).hexdigest())
    return out


def protected(path: str) -> bool:
    return path.rsplit("/", 1)[-1] == "CLAUDE.md" or any(
        path.startswith(p) if p.endswith("/") else path == p for p in PROTECTED)


def trusted_hooks(where: pathlib.Path) -> pathlib.Path:
    """A hooks dir whose commit-msg runs this checkout's check, not the worktree's."""
    hook = where / "commit-msg"
    check = _REPO / ".claude" / "hooks" / "check_commit_recital.py"
    hook.write_text(f'#!/bin/sh\nexec {shlex.quote(sys.executable)} {shlex.quote(str(check))} --commit-msg "$1"\n',
                    encoding="utf-8")
    hook.chmod(0o755)
    return where


def git(hooks: pathlib.Path, *args: str) -> list[str]:
    return ["git", "-c", "core.fsmonitor=false", "-c", f"core.hooksPath={hooks}", *args]


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
              "", "Report every key. Open each `reason` with one short sentence that says it all (for `left`: "
              "what stopped you); the detail follows. `title` is the commit's subject line; `summary` is its body: what was wrong "
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


def describe_call(name: str, inp: dict, cwd: pathlib.Path | None = None) -> str:
    """One tool call, short: `Edit app/src/x.jl`, `Bash pixi run test-py`."""
    arg = str(inp.get("file_path") or inp.get("command") or inp.get("pattern") or inp.get("path") or "")
    arg = arg.strip().splitlines()[0] if arg.strip() else ""
    if cwd:   # the worktree is where it works anyway: `cd <tree> && x` → `x`, `<tree>/a.py` → `a.py`
        arg = re.sub(rf"^cd {re.escape(str(cwd))}/?\s*(?:&&|;)\s*", "", arg).replace(f"{cwd}/", "")
    return f"{name} {arg}".strip()


class Progress:
    """A long step's live line: spinner, the step, elapsed time and, for an agent, its tool calls and the
    latest. On exit a `✓` (or `✗`) line with what it took; off a terminal, only that line."""
    FRAMES = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

    def __init__(self, label: str, *, out: _t.TextIO | None = None, cwd: pathlib.Path | None = None):
        self.label, self.out, self.cwd = label, out or sys.stdout, cwd
        self.tty = self.out.isatty()
        self.calls, self.last, self.note, self.failed = 0, "", "", False
        self.t0 = time.monotonic()
        self._stop = threading.Event()
        self._thread = threading.Thread(target=self._spin, daemon=True)

    def __enter__(self) -> "Progress":
        if self.tty:
            self._thread.start()
        return self

    def __exit__(self, kind, *exc) -> None:
        self._stop.set()
        if self.tty:
            self._thread.join()
            self.out.write("\r\033[K")
        bad = self.failed or kind is not None
        mark = _col(VERMILLION if bad else BLUISH_GREEN, "✗" if bad else "✓", use_colour=self.tty)
        print(f"  {mark} {self.label}" + _col(_DIM, self.stats(), use_colour=self.tty), file=self.out, flush=True)

    def stats(self) -> str:
        calls = [f"{self.calls} tool call{'s' if self.calls != 1 else ''}"] if self.calls else []
        return " · " + " · ".join([_fmt_duration(time.monotonic() - self.t0), *calls, *([self.note] if self.note else [])])

    def event(self, ev: dict) -> None:
        """A `stream-json` event: counts its tool calls."""
        for call in agent_sandbox.tool_uses(ev):
            self.calls += 1
            self.last = describe_call(call["tool"], call["input"], self.cwd)

    def line(self, frame: int) -> str:
        spin = _col(SKY_BLUE, self.FRAMES[frame % len(self.FRAMES)], use_colour=self.tty)
        tail = self.stats() + (f" · {self.last}" if self.last else "")
        return _clip(f"  {spin} {self.label}" + _col(_DIM, tail, use_colour=self.tty), _terminal_size()[0] - 1)

    def _spin(self) -> None:
        frame = 0
        while not self._stop.wait(0.1):
            self.out.write("\r\033[K" + self.line(frame))
            self.out.flush()
            frame += 1


#: An outcome's word and colour, in the session's rows and on the run's landed lines.
_OUTCOME = {"fixed": ("fixed", BLUISH_GREEN), "not_real": ("not real", GREY), "left": ("left", ORANGE),
            "landed": ("landed", GREY), "failed": ("failed", VERMILLION)}
_REASON_LINES = 2   # the card and auto_fix.jsonl keep the whole reason


def _width() -> int:
    return min(_terminal_size()[0], _rv._MAX_WIDTH)


def outcome_rows(rows: _t.Sequence[tuple[str, str, str]], *, use_colour: bool) -> list[str]:
    """`fixed  fanout-1a2b  Its first sentence of why…` per (key, outcome, reason), the reason wrapped
    under itself and cut at `_REASON_LINES`."""
    keyw = max((len(k) for k, _, _ in rows), default=0)
    out = []
    for key, outcome, reason in rows:
        word, colour = _OUTCOME.get(outcome, (outcome, GREY))
        head = f"    {word:<8}  {key:<{keyw}}  "
        first = _rv._SENTENCE.split(" ".join(str(reason).split()))[0] or "—"
        lines = _wrap_desc(first, width=_width(), indent=" " * len(head), first_prefix=head).splitlines()
        if len(lines) > _REASON_LINES:
            lines = lines[:_REASON_LINES]
            lines[-1] = lines[-1].rstrip() + " …"
        lines = [lines[0][:len(head)] + _rv._highlight(lines[0][len(head):], use_colour=use_colour),
                 *(" " * len(head) + _rv._highlight(ln[len(head):], use_colour=use_colour) for ln in lines[1:])]
        lines[0] = ("    " + _col(colour, f"{word:<8}", use_colour=use_colour) + "  "
                    + _col(_BOLD, f"{key:<{keyw}}", use_colour=use_colour) + lines[0][len(head) - 2:])
        out += lines
    return out


def default_session(prompt: str, cwd: pathlib.Path, schema: dict, *, resume: str | None = None,
                    main: pathlib.Path, label: str = "agent working", out: _t.TextIO | None = None,
                    budget_usd: float = SESSION_USD,
                    timeout: float = TIMEOUT_SEC) -> tuple[dict | None, float, str | None, str | None]:
    """One fenced `claude -p` in `cwd`, streamed so `Progress` can show it working. (structured answer,
    cost, session id, failure); the answer is None when it failed. Raises `RateLimited` when the seat
    is out of quota."""
    claude = resolve_claude_bin()
    if not claude:
        return None, 0.0, None, "claude CLI not on PATH"
    argv = [claude, "-p", "--permission-mode", "acceptEdits", "--settings", json.dumps(settings(main)),
            "--tools", TOOLS, "--strict-mcp-config", "--output-format", "stream-json", "--verbose",
            "--max-budget-usd", str(budget_usd), "--json-schema", json.dumps(schema)]
    if resume:
        argv += ["--resume", resume]
    result, timed_out = {}, threading.Event()
    try:
        with tempfile.TemporaryFile("w+", encoding="utf-8") as err, Progress(label, out=out, cwd=cwd) as progress:
            proc = subprocess.Popen(argv, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=err, cwd=str(cwd),
                                    env=agent_sandbox.agent_env(), text=True, encoding="utf-8")
            timer = threading.Timer(timeout, lambda: (timed_out.set(), proc.kill()))
            timer.start()
            try:
                proc.stdin.write(prompt)
                proc.stdin.close()
                for line in proc.stdout:
                    try:
                        ev = json.loads(line)
                    except ValueError:
                        continue
                    if isinstance(ev, dict):
                        progress.event(ev)
                        result = ev if ev.get("type") == "result" else result
                code = proc.wait()
            finally:
                timer.cancel()
            progress.note = f"${float(result.get('total_cost_usd') or 0.0):.2f}"
            progress.failed = code != 0 or timed_out.is_set() or not isinstance(result.get("structured_output"), dict)
            err.seek(0)
            stderr = err.read()
    except OSError as e:
        return None, 0.0, None, f"session failed: {e}"
    if timed_out.is_set():
        return None, 0.0, result.get("session_id"), f"session failed: timed out after {timeout:.0f}s"
    done = subprocess.CompletedProcess(argv, code, json.dumps(result) if result else "", stderr)
    out = read_result(done)
    answer = out.get("structured_output")
    cost = float(out.get("total_cost_usd") or 0.0)
    if code != 0 or out.get("is_error") or not isinstance(answer, dict):
        return None, cost, out.get("session_id"), f"session failed (exit {code}): {failure_text(done, out)}"
    return answer, cost, out.get("session_id"), None


def default_sh(argv: _t.Sequence[str], cwd: pathlib.Path, *, input: str | None = None,
               timeout: float = 3600) -> subprocess.CompletedProcess:
    return subprocess.run(list(argv), cwd=str(cwd), input=input, capture_output=True, text=True, encoding="utf-8",
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


def _log_failed(record: dict, bugs: _t.Sequence[dict], why: str, auto_log: pathlib.Path | None) -> None:
    tried = _rv.read_auto(auto_log)
    for b in bugs:
        last = tried.get(b["key"]) or {}
        attempt = last.get("attempt", 1) + 1 if last.get("outcome") == "failed" else 1
        _rv.append_auto({"record": record["date"], "ref": b["id"], "key": b["key"], "outcome": "failed",
                         "reason": why, "attempt": attempt, "pr_url": ""}, auto_log)


def guard(tree: pathlib.Path, before: dict[str, str], hooks: pathlib.Path, sh: _t.Callable) -> tuple[str | None, str]:
    """Stage the worktree, if the agent left what runs outside the sandbox alone. (why it stops, staged diff)."""
    now = snapshot(tree)
    changed = sorted(k for k in before.keys() | now.keys() if before.get(k) != now.get(k))
    if changed:
        return f"the agent changed {', '.join(changed[:5])}, which run outside its sandbox: nothing run there", ""
    sh(git(hooks, "add", "-A"), tree)
    names = (sh(git(hooks, "diff", "--cached", "--name-only"), tree).stdout or "").split("\n")
    touched = [n for n in names if n and protected(n)]
    if touched:
        return f"the diff touches {', '.join(touched[:5])} (CI, hooks or the fence): fix it by hand", ""
    diff = sh(git(hooks, "diff", "--cached"), tree).stdout or ""
    if SECRET_RE.search(diff):
        return "the staged diff holds what looks like a credential: not committed", ""
    return None, diff


def work_one(record: dict, bugs: _t.Sequence[dict], *, main: pathlib.Path, out: _t.TextIO,
             session: _t.Callable = default_session, sh: _t.Callable = default_sh,
             path: pathlib.Path | None = None, auto_log: pathlib.Path | None = None) -> float:
    """One bundle through to a PR. Returns what its sessions spent."""
    name = ("fix-" if len(bugs) == 1 else "fix-bundle-") + bugs[0]["key"]
    branch = name
    tree = main.parent / f"cecelia-{name}"
    colour = out.isatty()

    def say(msg: str, *, bad: bool = False) -> None:
        mark = _col(VERMILLION, "✗", use_colour=colour) + " " if bad else "  "
        print(f"  {mark}" + (msg if bad else _col(_DIM, msg, use_colour=colour)), file=out, flush=True)
    if tree.exists():
        say(f"{tree} exists already: skipped (remove it, or finish it by hand)", bad=True)
        return 0.0
    with Progress("worktree", out=out) as step:
        made = sh(["pixi", "run", "bootstrap-worktree", name, branch, "origin/main"], main)
        step.failed = made.returncode != 0
    if made.returncode != 0:
        say(f"bootstrap-worktree failed: {_tail(made)}", bad=True)
        return 0.0
    with tempfile.TemporaryDirectory(prefix="auto-fix-hooks-") as hooks_dir:
        return _work(record, bugs, tree, branch, trusted_hooks(pathlib.Path(hooks_dir)), main=main, say=say,
                     out=out, session=session, sh=sh, path=path, auto_log=auto_log)


def _work(record, bugs, tree, branch, hooks, *, main, say, out, session, sh, path, auto_log) -> float:
    before = snapshot(tree)
    answer, spent, sid, failed = session(fix_brief(record, bugs, tree, branch), tree, FIX_SCHEMA, main=main,
                                         label="agent fixing", out=out)
    if failed:
        _log_failed(record, bugs, f"{failed} (${spent:.2f})", auto_log)
        say(f"{failed}; worktree removed, tried again next run", bad=True)
        agent_sandbox.remove_worktree(main, tree)
        sh(["git", "branch", "-D", branch], main)
        return spent
    for line in outcome_rows([(r["key"], r["outcome"], r["reason"]) for r in answer["bugs"]],
                             use_colour=out.isatty()):
        print(line, file=out)
    if not _fixed_keys(answer):
        _log(record, bugs, answer, "", auto_log)
        agent_sandbox.remove_worktree(main, tree)
        sh(["git", "branch", "-D", branch], main)
        say("nothing fixed: no PR, worktree removed")
        return spent

    def stop(why: str) -> float:
        _log(record, bugs, answer, "", auto_log, fixed_as="left")
        say(f"{why}; worktree left at {tree}", bad=True)
        return spent

    why, diff = guard(tree, before, hooks, sh)
    if why:
        return stop(why)
    with Progress("recital", out=out) as step:
        rec = sh([sys.executable, str(_REPO / "scripts" / "recital.py"), "--stdin"], tree, input=diff)
        recital = rec.stdout or ""
        step.failed = "_Recital stamp:" not in recital
    if "_Recital stamp:" not in recital:
        return stop(f"recital failed: {_tail(rec)}")
    tagged, cost, _, failed = session(tags_brief(recital), tree, TAGS_SCHEMA, resume=sid, main=main,
                                      label="agent acting on the recital", out=out)
    spent += cost
    if failed:
        return stop(f"tagging the recital: {failed}")
    why, _ = guard(tree, before, hooks, sh)
    if why:
        return stop(why)
    with tempfile.NamedTemporaryFile("w", encoding="utf-8", suffix=".txt", delete=False) as fh:
        fh.write(commit_message(record, bugs, answer, tagged["tags"], recital))
    try:
        for argv in (git(hooks, "commit", "-F", fh.name), git(hooks, "push", "-u", "origin", branch)):
            done = sh(argv, tree)
            if done.returncode != 0:
                return stop(f"`git {argv[5]}` failed: {_tail(done)}")
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
        return stop(f"gh pr create failed: {_tail(opened)}; branch {branch} is pushed")
    pr = (opened.stdout or "").strip().splitlines()[-1]
    _log(record, bugs, answer, pr, auto_log)
    for b in bugs:
        if b["key"] in _fixed_keys(answer):
            _rv.append_review("bug_work", record["date"], b["id"], "fix_session", note=f"auto PR {pr}", path=path)
    print(f"  {_col(BLUISH_GREEN, '→', use_colour=out.isatty())} "
          + _col(_BOLD, f"{'draft PR' if answer['draft'] else 'PR'} {pr}", use_colour=out.isatty()), file=out, flush=True)
    return spent


def landed(record: dict, keys: _t.Iterable[str], main: pathlib.Path) -> dict[str, list[dict]]:
    """Keys a commit on origin/main names since the record's run: fixed by hand after the pass."""
    return _bugs.landed_fixes(keys, (record.get("run") or {}).get("sha"), "origin/main", main)


def _without(items: list[dict], by_id: dict, keys: _t.Container[str]) -> list[dict]:
    """`items` with the bugs of `keys` taken out of their bundles; a bundle left empty, dropped."""
    out = []
    for it in items:
        refs = [r for r in it.get("refs") or [it["ref"]] if by_id[r]["key"] not in keys]
        if refs == (it.get("refs") or [it["ref"]]):
            out.append(it)
        elif refs:
            out.append({"kind": "bundle", "ref": refs[0], "refs": refs} if len(refs) > 1
                       else {"kind": "work", "ref": refs[0]})
    return out


def run(record: dict, *, limit: int = 1, dry_run: bool = False, out: _t.TextIO = sys.stdout,
        path: pathlib.Path | None = None, auto_log: pathlib.Path | None = None, older: _t.Sequence[dict] = (),
        main: pathlib.Path | None = None, **kw) -> int:
    """Up to `limit` bundles from `review.auto_items`, one after another. Returns the sessions run."""
    main = main or _rv.main_checkout()
    items = _rv.auto_items(record, _rv.read_reviews(path), older, _rv.read_auto(auto_log))
    by_id = {b["id"]: b for b in record.get("bugs", [])}
    colour, width = out.isatty(), _width()
    if items and not dry_run:
        fetched = (kw.get("sh") or default_sh)(["git", "fetch", "-q", "origin"], main)
        if fetched.returncode != 0:
            print(f"  {_col(VERMILLION, '✗', use_colour=colour)} git fetch failed: {_tail(fetched)}", file=out)
            return 0
    keys = {by_id[r]["key"]: by_id[r] for it in items for r in it.get("refs") or [it["ref"]]}
    done = landed(record, keys, main) if items else {}
    rows = []
    for k, commits in done.items():
        c = commits[0]
        why = f"{c['commit'][:8]} {c['subject']}" + (f" (#{c['pr']})" if c.get("pr") else "")
        rows.append((k, "landed", why))
        if not dry_run:
            _rv.append_auto({"record": record["date"], "ref": keys[k]["id"], "key": k, "outcome": "landed",
                             "reason": why, "pr_url": ""}, auto_log)
    items = _without(items, by_id, done)
    n = sum(len(it.get("refs") or [it["ref"]]) for it in items)
    print(_col(_BOLD, "judge-review --auto", use_colour=colour)
          + _col(_DIM, f" · {record['date']} · {n} fix bug(s) in {len(items)} session(s)"
                 + (f" · {len(done)} already fixed on main" if done else "")
                 + f" · running {min(limit, len(items))}" + (" · dry run" if dry_run else ""), use_colour=colour),
          file=out, flush=True)
    for line in outcome_rows(rows, use_colour=colour):
        print(line, file=out)
    spent, ran = 0.0, 0
    for item in items[:limit]:
        bugs = [by_id[r] for r in item.get("refs") or [item["ref"]]]
        label = (f"{bugs[0]['id']} {bugs[0]['key']}" if len(bugs) == 1
                 else " + ".join(b["id"] for b in bugs) + f" · {len(bugs)} bugs, one session")
        print("\n" + _hr(label, width, use_colour=colour), file=out, flush=True)
        # the bugs as the `[f]` queue shows them; the brief carries the evidence
        print("\n".join(_rv.describe(record, item, width=width, use_colour=colour, evidence=False)) + "\n",
              file=out, flush=True)
        if dry_run:
            name = ("fix-" if len(bugs) == 1 else "fix-bundle-") + bugs[0]["key"]
            print(fix_brief(record, bugs, main.parent / f"cecelia-{name}", name), file=out)
            continue
        spent += work_one(record, bugs, main=main, out=out, path=path, auto_log=auto_log, **kw)
        ran += 1
    if not dry_run:
        left = len(items) - ran
        print("\n" + _hr(f"{ran} session(s) · ${spent:.2f}" + (f" · {left} left: run again" if left > 0 else ""),
                         width, use_colour=colour), file=out)
    return ran
