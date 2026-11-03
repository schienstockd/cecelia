#!/usr/bin/env python3
"""`judge-deps` — work the dependency-bot PRs whose CI is red, unattended, one PR per session.

The bots: Dependabot (npm + GitHub Actions, `.github/dependabot.yml`) and the monthly `update-deps`
workflow (pixi + Julia). A green bot PR is the owner's to merge. A red one means a bump broke
something, and finding which bump and why is the part nobody wants to do by hand: this does it.

Per red PR, the same pipeline as `judge-review --auto` (`auto_fix.run_job`: the sandbox, the recital
rounds, commit, push, PR, the CI rounds), with a different brief and log:

1. `bootstrap-worktree deps-<n>` off the bot's branch, not main: the bumped packages are what the
   agent has to reproduce the failure with, and it has no network to install them itself.
2. The agent finds the bump that broke it and fixes our code for the new version. A fix it can't
   make in our code (an upstream regression with no workaround) is `left`, with the hold it proposes:
   a Dependabot `ignore` or a pixi cap plus its `docs/TODO.md` item. Those files are fenced
   (`auto_fix.PROTECTED`), so the owner makes the hold.
3. The PR it opens carries the bump and the fix together (`docs/DEV.md` → *When CI goes red*: never
   push to the bot's branch). Once it's green, the bot PR gets a comment pointing at it: Dependabot
   closes its own PR when the versions reach main; an `update-deps` PR is closed by hand.

A PR is tried once per head commit (`deps_fix.jsonl`, beside `auto_fix.jsonl`): a bot that rebases
or bumps again gets a fresh try. The bot PR's body is third-party release notes, so the brief carries
only its version table, never the notes. Design: docs/ai-assist/WEEKLY_JUDGE.md → *Red dependency PRs*.

    pixi run judge-deps --dry-run      # the red PRs and the briefs it would send
    pixi run judge-deps [--limit 1]    # work them (the weekly pass runs this before --auto)
"""
from __future__ import annotations

import argparse
import importlib.util as _importlib_util
import json
import pathlib
import sys
import time
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness.agents import ci_state  # noqa: E402
from cecelia.effectiveness.claude_cli import RateLimited, limit_exit  # noqa: E402
from cecelia.effectiveness.console import _BOLD, _DIM, _col, _hr  # noqa: E402
from cecelia.effectiveness.log import read_events  # noqa: E402
from cecelia.effectiveness.palette import SKY_BLUE, VERMILLION  # noqa: E402


def _load_auto_fix():
    spec = _importlib_util.spec_from_file_location("_judge_auto_fix", pathlib.Path(__file__).parent / "auto_fix.py")
    mod = _importlib_util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_af = _load_auto_fix()
_rv = _af._rv

#: The bots' branches: Dependabot's `dependabot/<ecosystem>/…`, and the `update-deps` workflow's.
UPDATE_DEPS_BRANCH = "update-deps"
#: How much of the version table the brief carries (a group bump lists one row per package).
TABLE_ROWS = 40


def is_bot_branch(branch: str) -> bool:
    return branch.startswith("dependabot/") or branch == UPDATE_DEPS_BRANCH


def deps_path() -> pathlib.Path:
    """`deps_fix.jsonl` beside `auto_fix.jsonl`: one row per (PR, head commit) tried."""
    return _rv.auto_path().parent / "deps_fix.jsonl"


def read_tried(path: pathlib.Path | None = None) -> set[tuple[int, str]]:
    return {(row["pr"], row["head"]) for row in read_events(path or deps_path()) if row.get("pr") and row.get("head")}


def append_tried(row: dict, path: pathlib.Path | None = None) -> None:
    _rv._append_jsonl(path or deps_path(), {"ts": _rv._now(), **row})


def red_prs(sh: _t.Callable, main: pathlib.Path) -> list[dict] | None:
    """The open bot PRs whose CI finished red, oldest first; None when `gh` couldn't say."""
    done = sh(["gh", "pr", "list", "--state", "open", "--limit", "100", "--json",
               "number,title,url,body,headRefName,headRefOid,statusCheckRollup"], main)
    try:
        prs = json.loads(done.stdout) if done.returncode == 0 else None
    except ValueError:
        prs = None
    if prs is None:
        return None
    return sorted((p for p in prs if is_bot_branch(p.get("headRefName") or "")
                   and ci_state(p.get("statusCheckRollup") or []) == "fail"), key=lambda p: p["number"])


def version_table(body: str) -> str:
    """The bot PR's `| package | from | to |` rows and nothing else: the rest of a Dependabot body is
    third-party release notes and commit messages, which an unattended agent shouldn't be handed."""
    rows = [ln for ln in (body or "").splitlines() if ln.lstrip().startswith("|")]
    more = len(rows) - TABLE_ROWS
    return "\n".join(rows[:TABLE_ROWS] + ([f"(… {more} more rows)"] if more > 0 else [])) or "(no version table)"


def key_of(pr: dict) -> str:
    return f"deps-{pr['number']}"


def deps_brief(pr: dict, failed: _t.Sequence[str], logs: str, tree: pathlib.Path, branch: str) -> str:
    return "\n".join([
        f"Dependency PR #{pr['number']} ({pr['title']}, branch `{pr['headRefName']}`) fails CI. Find which "
        "bump broke it and make our code work with the new version. You are running unattended: nobody "
        "will answer a question, so decide and carry on.",
        "", "## What it bumps", version_table(pr.get("body") or ""),
        "", "## Failed checks", *[f"- {n}" for n in failed],
        "", "## Their logs (up to the first error)", logs or "(no log)",
        "", f"You are in `{tree}`, a worktree on branch `{branch}` off the bot's branch: the bumped packages "
        "are installed (frontend/node_modules, the pixi env, the Julia depot). Work only there. There is no "
        "network, and you can't commit: git writes and the PR are done for you once you finish.",
        "", "First reproduce the failure here with the command the failed check runs (`docs/DEV.md` → "
        "*Testing*). Then name the bump that causes it: read the error against the bumped packages' code "
        "and changelogs in the installed packages, or try an older version from the local package cache "
        "(`npm install --offline --no-save <pkg>@<old>`) if it has one; put it back to the bumped version "
        "after. Look for an upstream issue only in what is installed; you can't browse.",
        "", "Fix our code so it works with the new version, at every call site with the same shape, and "
        "rerun the failing check until it passes. Keep the bump: never pin, downgrade or edit lockfiles, "
        "`pixi.toml`, `.github/` or `CLAUDE.md` (the commit is refused). A comment at the fix names the "
        "upstream cause when there is one.",
        "", f"Report one entry, key `{key_of(pr)}`: `fixed` (your fix is in the worktree); `not_real` "
        "(the failure doesn't reproduce here or isn't from the bump: a flaky or unrelated check); or "
        "`left` (no fix in our code: an upstream regression to wait out). For `left`, change nothing and "
        "say which package and versions to hold, and how: a Dependabot `ignore` entry with a version range, "
        "or a pixi cap, each with the `docs/TODO.md` item that lifts it. Open each `reason` with one "
        "short sentence that says it all. `title` is the commit subject; `summary` its body: the culprit "
        "bump, what changed in it, and what you changed. Set `visual` if the fix changes what is drawn "
        "on screen."])


def deps_job(pr: dict, brief: str, tree: pathlib.Path, *, sh: _t.Callable, main: pathlib.Path,
             log_path: pathlib.Path | None) -> "_af.Job":
    key, n = key_of(pr), pr["number"]
    bot = "Dependabot" if pr["headRefName"].startswith("dependabot/") else "the update-deps workflow"

    def log(answer: dict, url: str, *, stopped: str | None = None, tree: pathlib.Path | None = None,
            stage: str = "staged") -> None:
        r = next((b for b in answer.get("bugs", []) if b.get("key") == key), None) or \
            {"outcome": "left", "reason": "the session didn't report it"}
        outcome = r.get("outcome") if r.get("outcome") in _af.OUTCOMES else "left"
        reason = r.get("reason", "")
        if stopped:
            outcome, reason = "left", f"Stopped {'at ' + url if url else 'before the PR'}: {stopped}. {reason}"
        append_tried({"pr": n, "head": pr["headRefOid"], "outcome": outcome, "reason": reason, "pr_url": url,
                      **({"tree": str(tree), "stage": stage if stage in _af.STAGES else "staged"}
                         if stopped and tree else {})}, log_path)

    def log_failed(why: str) -> None:
        append_tried({"pr": n, "head": pr["headRefOid"], "outcome": "failed", "reason": why, "pr_url": ""}, log_path)

    def commit_message(answer: dict, tags: _t.Sequence[str], recital: str, stamp: dict) -> str:
        return "\n".join([answer["title"].strip()[:72], "", answer["summary"].strip(), "",
                          f"Unattended fix for #{n} ({bot}), from the weekly judge pass (`judge-deps`). "
                          "This branch carries the bump too.", *([""] + list(tags) if tags else []), "",
                          recital.strip(), "", *_af.attribution_trailers(stamp or {})]) + "\n"

    def pr_body(answer: dict, stamp: dict) -> str:
        r = next((b for b in answer.get("bugs", []) if b.get("key") == key), {})
        closes = (f"Dependabot closes #{n} itself once these versions are on main." if bot == "Dependabot"
                  else f"Close #{n} by hand after merging: the next run rebuilds `{UPDATE_DEPS_BRANCH}` from main.")
        return "\n".join([answer["summary"].strip(), "",
                          f"Carries #{n}'s bump plus the fix that makes CI pass, worked unattended by the weekly "
                          f"judge pass (`pixi run judge-deps`). Nothing here is merged until you merge it. {closes}",
                          *(["", "**Changes what is drawn on screen:** look at it before you merge."]
                            if answer.get("visual") else []),
                          "", f"- `{key}`: **{r.get('outcome', 'left')}** — {r.get('reason', '')}", "",
                          _af.footer(stamp or {})]) + "\n"

    def handback(trusted: bool) -> str:
        return (f"back to you: #{n} stays red · " + (f"the work is in {tree.name}" if trusted else
                f"{tree.name} is left for you to inspect, not to work in"))

    def on_green(answer: dict, url: str) -> None:
        sh(["gh", "pr", "comment", str(n), "--body",
            f"CI was red here; {url} carries this bump plus the fix and is green. Opened unattended by "
            f"Claude Code (the weekly judge's `judge-deps`), not by a person."], main)

    return _af.Job(brief=brief, log=log, log_failed=log_failed, commit_message=commit_message, pr_body=pr_body,
                   handback=handback, on_green=on_green, tool="judge-deps")


def work_pr(pr: dict, *, main: pathlib.Path, out: _t.TextIO, session: _t.Callable, sh: _t.Callable,
            log_path: pathlib.Path | None, dry_run: bool = False) -> float | None:
    """One red PR through to a green PR; what its sessions spent, None when it ran none."""
    name = key_of(pr)
    tree = main.parent / f"cecelia-{name}"
    say = _af.sayer(out)
    red = [c for c in pr.get("statusCheckRollup") or [] if ci_state([c]) == "fail"]
    failed = [c.get("name") or c.get("context") or "?" for c in red]
    brief = deps_brief(pr, failed, _af.failed_logs(red, main, sh), tree, name)
    if dry_run:
        print(brief, file=out)
        return None
    left_over = tree.exists() or (sh(["git", "branch", "--list", name], main).stdout or "").strip()
    if left_over:
        why = f"{tree.name if tree.exists() else 'branch ' + name} is left from an earlier run; yours to look at"
        append_tried({"pr": pr["number"], "head": pr["headRefOid"], "outcome": "left", "reason": why, "pr_url": ""},
                     log_path)
        say(why, bad=True)
        return None
    job = deps_job(pr, brief, tree, sh=sh, main=main, log_path=log_path)
    return _af.run_job(job, tree, name, main=main, out=out, say=say, session=session, sh=sh,
                       base=f"origin/{pr['headRefName']}")


def run(*, limit: int | None = None, dry_run: bool = False, out: _t.TextIO = sys.stdout,
        main: pathlib.Path | None = None, until: float | None = None, clock: _t.Callable[[], float] = time.time,
        log_path: pathlib.Path | None = None, session: _t.Callable | None = None,
        sh: _t.Callable | None = None) -> int:
    """Every red bot PR not yet tried at its head (up to `limit`); the sessions run. Past `until`
    (epoch seconds) it starts no new one."""
    main = main or _rv.main_checkout()
    sh, session = sh or _af.default_sh, session or _af.default_session
    colour = out.isatty()
    prs = red_prs(sh, main)
    if prs is None:
        print(f"  {_col(VERMILLION, '✗', use_colour=colour)} gh pr list failed", file=out)
        return 0
    tried = read_tried(log_path)
    todo = [p for p in prs if (p["number"], p["headRefOid"]) not in tried]
    print(_col(_BOLD, "judge-deps", use_colour=colour)
          + _col(_DIM, f" · {len(prs)} red bot PR(s) · {len(prs) - len(todo)} tried at this commit already"
                 + (" · dry run" if dry_run else ""), use_colour=colour), file=out, flush=True)
    if todo and not dry_run:
        fetched = sh(["git", "fetch", "-q", "origin"], main)
        if fetched.returncode != 0:
            print(f"  {_col(VERMILLION, '✗', use_colour=colour)} git fetch failed: {_af._tail(fetched)}", file=out)
            return 0
    ran, spent = 0, 0.0
    for pr in todo[:len(todo) if limit is None else limit]:
        if until is not None and not dry_run and clock() >= until:
            print(_col(_DIM, "  past the deadline: the rest wait for next week", use_colour=colour), file=out)
            break
        print("\n" + _hr(f"#{pr['number']} {pr['title']}", _af._width(), use_colour=colour), file=out, flush=True)
        try:
            cost = work_pr(pr, main=main, out=out, session=session, sh=sh, log_path=log_path, dry_run=dry_run)
        except _af.SetupFailed:   # off its own bot branch, so one PR's failure says nothing of the next
            continue
        if cost is not None:
            ran, spent = ran + 1, spent + cost
    if ran:
        print("\n" + _col(SKY_BLUE, f"{ran} session(s) · ${spent:.2f}", use_colour=colour), file=out)
    return ran


def main(argv: _t.Sequence[str] | None = None) -> int:
    ap = argparse.ArgumentParser(prog="judge-deps", description=__doc__.split("\n\n")[0])
    ap.add_argument("--limit", type=int, default=None, help="at most this many sessions (default: all)")
    ap.add_argument("--dry-run", action="store_true", help="print the red PRs' briefs, run nothing")
    ap.add_argument("--until", type=float, default=None, help="start no session after this time (epoch seconds)")
    args = ap.parse_args(argv)
    try:
        run(limit=args.limit, dry_run=args.dry_run, until=args.until)
    except RateLimited as e:
        return limit_exit("judge-deps", e)
    return 0


if __name__ == "__main__":
    sys.exit(main())
