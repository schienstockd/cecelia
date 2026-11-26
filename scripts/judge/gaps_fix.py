#!/usr/bin/env python3
"""`judge-gaps` — close coherence gaps from `docs/architecture/gaps.md`, unattended, one gap per session.

The architecture views (`docs/architecture/`) list where the code breaks its declared rules, and
`test_architecture_gaps.py` stops the list from growing. This makes it shrink: each weekly pass takes
the easiest few open gaps and closes them through the same pipeline as `judge-review --auto`
(`auto_fix.run_job`: the sandbox, the recital rounds, commit, push, PR, the CI rounds). It never merges.

Which gaps: the kinds in `CLOSABLE`, broken ends first, then the rest; within a kind, the order
`gaps.md` lists them (easiest first). Not `route-uncalled`: deleting a route nobody calls, or keeping it
for a consumer to come, is the owner's call, not an agent's. A gap is tried once
(`gaps_fix.jsonl`, beside `auto_fix.jsonl`): `left` or `not_real` stays in `gaps.md` for the owner.

The agent closes the gap in code, regenerates the views, lowers the ceiling
(`pixi run arch-gaps-baseline`) and runs the tests the change touches. Design:
docs/ai-assist/WEEKLY_JUDGE.md → *Coherence gaps*.

    pixi run judge-gaps --dry-run      # the gaps it would take and the briefs it would send
    pixi run judge-gaps [--limit 1]    # work them (the weekly pass runs this after judge-deps)
"""
from __future__ import annotations

import argparse
import hashlib
import importlib.util as _importlib_util
import json
import pathlib
import sys
import time
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness.claude_cli import RateLimited, limit_exit  # noqa: E402
from cecelia.effectiveness.console import _BOLD, _DIM, _col, _hr  # noqa: E402
from cecelia.effectiveness.log import read_events  # noqa: E402
from cecelia.effectiveness.palette import SKY_BLUE, VERMILLION  # noqa: E402


def _load(name: str, path: pathlib.Path):
    spec = _importlib_util.spec_from_file_location(name, path)
    mod = _importlib_util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_af = _load("_judge_auto_fix", pathlib.Path(__file__).parent / "auto_fix.py")
_rv = _af._rv

#: Gap kinds an agent may close on its own, in the order they are taken.
CLOSABLE = ("ws-unhandled", "ws-no-emitter", "ws-no-handler", "runner-unlaunched", "shared-file-unguarded",
            "viewer-store", "store-cycle", "py-cycle", "julia-upward")
#: Sessions per weekly pass when no --limit is given: a few small PRs a week, not a flood.
WEEKLY = 3


def gaps_module(main: pathlib.Path):
    return _load("_arch_gaps", main / "scripts" / "architecture" / "gaps.py")


def key_of(gap: dict) -> str:
    """Stable across runs for the same gap: kind + subject."""
    h = hashlib.sha1(f"{gap['kind']}\0{gap['subject']}".encode("utf-8")).hexdigest()[:8]
    return f"gap-{gap['kind']}-{h}"


def gaps_path() -> pathlib.Path:
    """`gaps_fix.jsonl` beside `auto_fix.jsonl`: one row per gap tried."""
    return _rv.auto_path().parent / "gaps_fix.jsonl"


def read_tried(path: pathlib.Path | None = None) -> set[str]:
    return {r.get("key") for r in read_events(path or gaps_path())}


def append_tried(row: dict, path: pathlib.Path | None = None) -> None:
    _rv._append_jsonl(path or gaps_path(), {"ts": _rv._now(), **row})


def open_gaps(main: pathlib.Path) -> list[dict]:
    """The committed gaps on `main`, closable kinds only, in the order they are taken."""
    g = gaps_module(main)
    items = g.load(main / "docs" / "architecture" / "gaps")
    order = {k: i for i, k in enumerate(CLOSABLE)}
    picked = [it for it in items if it["kind"] in order]
    return sorted(picked, key=lambda it: (order[it["kind"]], it.get("rank", 0), it["subject"]))


def gaps_brief(gap: dict, meaning: str, close: str, tree: pathlib.Path, branch: str) -> str:
    where = "\n".join(f"- `{w}`" for w in gap.get("where") or []) or "- (see the detail)"
    return "\n".join([
        "Close one coherence gap from `docs/architecture/gaps.md`. You are running unattended: nobody will "
        "answer a question, so decide and carry on.",
        "", f"## The gap (`{gap['kind']}`)", f"{meaning}: **{gap['subject']}**"
        + (f" — {gap['detail']}" if gap.get("detail") else ""),
        "", "## Where", where,
        "", "## To close it", close + ".",
        "", "Read `docs/architecture/README.md` → *Rules* first, and the gap's rows in `boundaries.md` / "
        "`julia/areas.md` / `vue/*.md`. Then read the code at both ends before changing anything: the "
        "gap is found by text matching, so it can be a false alarm (a name built at runtime). Change the "
        "smallest thing that closes it, at every call site with the same shape; follow the root and area "
        "`CLAUDE.md` rules and reuse what exists (`docs/inventory/`).",
        "", f"You are in `{tree}`, a worktree on branch `{branch}` off main. Work only there. There is no "
        "network, and you can't commit: git writes and the PR are done for you once you finish.",
        "", "When the code is changed: `pixi run arch-diagrams`, check the gap is gone from "
        "`docs/architecture/gaps.md` and no other gap appeared, then `pixi run arch-gaps-baseline` to lower the "
        "ceiling, and run `pixi run test-py` plus the test category of the code you touched (`CLAUDE.md` → "
        "*Testing*). Never edit `pixi.toml`, `.github/` or a `CLAUDE.md` (the commit is refused), and never "
        "raise the baseline.",
        "", f"Report one entry, key `{key_of(gap)}`: `fixed` (the gap is closed, the views regenerated and the "
        "baseline lowered); `not_real` (a false alarm — say what the extractor misread; change nothing); or "
        "`left` (closing it needs a decision that isn't yours — a design choice, or deleting a feature; "
        "change nothing and say what the choice is). Open each `reason` with one short sentence that says "
        "it all. `title` is the commit subject; `summary` its body: the gap, why it was there, and what you "
        "changed. Set `visual` if the fix changes what is drawn on screen."])


def gaps_job(gap: dict, brief: str, tree: pathlib.Path, *, sh: _t.Callable, main: pathlib.Path,
             log_path: pathlib.Path | None) -> "_af.Job":
    key = key_of(gap)
    base = {"key": key, "kind": gap["kind"], "subject": gap["subject"]}

    def log(answer: dict, url: str, *, stopped: str | None = None, tree: pathlib.Path | None = None,
            stage: str = "staged") -> None:
        r = next((b for b in answer.get("bugs", []) if b.get("key") == key), None) or \
            {"outcome": "left", "reason": "the session didn't report it"}
        outcome = r.get("outcome") if r.get("outcome") in _af.OUTCOMES else "left"
        reason = r.get("reason", "")
        if stopped:
            outcome, reason = "left", f"Stopped {'at ' + url if url else 'before the PR'}: {stopped}. {reason}"
        append_tried({**base, "outcome": outcome, "reason": reason, "pr_url": url,
                      **({"tree": str(tree), "stage": stage if stage in _af.STAGES else "staged"}
                         if stopped and tree else {})}, log_path)

    def log_failed(why: str) -> None:
        append_tried({**base, "outcome": "failed", "reason": why, "pr_url": ""}, log_path)

    def commit_message(answer: dict, tags: _t.Sequence[str], recital: str, stamp: dict) -> str:
        return "\n".join([answer["title"].strip()[:72], "", answer["summary"].strip(), "",
                          f"Unattended fix for the `{gap['kind']}` gap {gap['subject']}, from the weekly judge "
                          "pass (`judge-gaps`).", *([""] + list(tags) if tags else []), "",
                          recital.strip(), "", *_af.attribution_trailers(stamp or {})]) + "\n"

    def pr_body(answer: dict, stamp: dict) -> str:
        r = next((b for b in answer.get("bugs", []) if b.get("key") == key), {})
        return "\n".join([answer["summary"].strip(), "",
                          f"Closes the `{gap['kind']}` gap **{gap['subject']}** from `docs/architecture/gaps.md`, "
                          "worked unattended by the weekly judge pass (`pixi run judge-gaps`). Lowers the ceiling "
                          "in `scripts/architecture/gaps_baseline.json`, so it stays closed. Nothing here is "
                          "merged until you merge it.",
                          *(["", "**Changes what is drawn on screen:** look at it before you merge."]
                            if answer.get("visual") else []),
                          "", f"- `{key}`: **{r.get('outcome', 'left')}** — {r.get('reason', '')}", "",
                          _af.footer(stamp or {})]) + "\n"

    def handback(trusted: bool) -> str:
        return (f"back to you: {gap['subject']} stays open · " + (f"the work is in {tree.name}" if trusted else
                f"{tree.name} is left for you to inspect, not to work in"))

    return _af.Job(brief=brief, log=log, log_failed=log_failed, commit_message=commit_message, pr_body=pr_body,
                   handback=handback, on_green=lambda answer, url: None, tool="judge-gaps")


def work_gap(gap: dict, *, main: pathlib.Path, out: _t.TextIO, session: _t.Callable, sh: _t.Callable,
             log_path: pathlib.Path | None, dry_run: bool = False) -> float | None:
    """One gap through to a green PR; what its sessions spent, None when it ran none."""
    name = key_of(gap)
    tree = main.parent / f"cecelia-{name}"
    say = _af.sayer(out)
    kinds = gaps_module(main).KINDS
    _, meaning, close = kinds[gap["kind"]]
    brief = gaps_brief(gap, meaning, close, tree, name)
    if dry_run:
        print(brief, file=out)
        return None
    left_over = tree.exists() or (sh(["git", "branch", "--list", name], main).stdout or "").strip()
    if left_over:
        why = f"{tree.name if tree.exists() else 'branch ' + name} is left from an earlier run; yours to look at"
        append_tried({"key": name, "kind": gap["kind"], "subject": gap["subject"], "outcome": "left",
                      "reason": why, "pr_url": ""}, log_path)
        say(why, bad=True)
        return None
    job = gaps_job(gap, brief, tree, sh=sh, main=main, log_path=log_path)
    return _af.run_job(job, tree, name, main=main, out=out, say=say, session=session, sh=sh, base="origin/main")


def run(*, limit: int | None = None, dry_run: bool = False, out: _t.TextIO = sys.stdout,
        main: pathlib.Path | None = None, until: float | None = None, clock: _t.Callable[[], float] = time.time,
        log_path: pathlib.Path | None = None, session: _t.Callable | None = None,
        sh: _t.Callable | None = None) -> int:
    """Up to `limit` (default WEEKLY) open closable gaps not tried before; the sessions run. Past `until`
    (epoch seconds) it starts no new one."""
    main = main or _rv.main_checkout()
    sh, session = sh or _af.default_sh, session or _af.default_session
    colour = out.isatty()
    if not (main / "scripts" / "architecture" / "gaps.py").is_file():
        print(_col(_DIM, "judge-gaps · no docs/architecture gap list in this checkout", use_colour=colour), file=out)
        return 0
    gaps = open_gaps(main)
    tried = read_tried(log_path)
    todo = [g for g in gaps if key_of(g) not in tried]
    print(_col(_BOLD, "judge-gaps", use_colour=colour)
          + _col(_DIM, f" · {len(gaps)} closable gap(s) · {len(gaps) - len(todo)} tried before"
                 + (" · dry run" if dry_run else ""), use_colour=colour), file=out, flush=True)
    if todo and not dry_run:
        fetched = sh(["git", "fetch", "-q", "origin"], main)
        if fetched.returncode != 0:
            print(f"  {_col(VERMILLION, '✗', use_colour=colour)} git fetch failed: {_af._tail(fetched)}", file=out)
            return 0
    ran, spent = 0, 0.0
    for gap in todo[:WEEKLY if limit is None else limit]:
        if until is not None and not dry_run and clock() >= until:
            print(_col(_DIM, "  past the deadline: the rest wait for next week", use_colour=colour), file=out)
            break
        print("\n" + _hr(f"{gap['kind']} · {gap['subject']}", _af._width(), use_colour=colour), file=out, flush=True)
        try:
            cost = work_gap(gap, main=main, out=out, session=session, sh=sh, log_path=log_path, dry_run=dry_run)
        except _af.SetupFailed:   # off main: a worktree that can't be made won't be made for the next gap either
            break
        if cost is not None:
            ran, spent = ran + 1, spent + cost
    if ran:
        print("\n" + _col(SKY_BLUE, f"{ran} session(s) · ${spent:.2f}", use_colour=colour), file=out)
    return ran


def main(argv: _t.Sequence[str] | None = None) -> int:
    ap = argparse.ArgumentParser(prog="judge-gaps", description=__doc__.split("\n\n")[0])
    ap.add_argument("--limit", type=int, default=None, help=f"at most this many sessions (default: {WEEKLY})")
    ap.add_argument("--dry-run", action="store_true", help="print the gaps' briefs, run nothing")
    ap.add_argument("--until", type=float, default=None, help="start no session after this time (epoch seconds)")
    args = ap.parse_args(argv)
    if not args.dry_run:
        _af.unattended(name="gaps")   # a closed terminal doesn't end it mid-session
    try:
        run(limit=args.limit, dry_run=args.dry_run, until=args.until, out=sys.stdout)
    except RateLimited as e:
        return limit_exit("judge-gaps", e)
    return 0


if __name__ == "__main__":
    sys.exit(main())
