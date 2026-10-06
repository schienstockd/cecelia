#!/usr/bin/env python3
"""Weekly judge — the pass: pin, sweep the bugs, verify them, tally the rules, record, publish.

Design: docs/ai-assist/WEEKLY_JUDGE.md.

Python does everything deterministic: the lock, pinning `origin/main`, the persistent worktree, the
record and the PR. `claude -p` is used three ways, each capped: the bug sweep's one tool-less call
(`bugs.py`), read-only verify agents in a sandbox (`verify.py`), and the finding→rule mapping
(`rules.py`). The owner's answers since the last pass (`pixi run judge-review`) are folded into the
previous record first, so a `wont_fix` bug is not carried. Then the agent run records' reviews
(`run_reviews.py`): each new `guide` / `platform` cause a person set is logged as an
`agent_run_finding`, so the sweep opens it; `agent` causes are listed per guide in the record.

A crash at any stage still writes a failure record, so a missing week is visible rather than silent.
So does a usage limit (`judge.RateLimited`): no later call could run, and a record where nothing
was judged would read like a quiet week. That one exits `EX_TEMPFAIL` and leaves the reset time in
`judge-ratelimit.json`; `cron_pass.sh` waits for it and reruns the pass, and while a retry will follow
(`JUDGE_RETRY_LEFT` > 0 and the reset within `RETRY_MAX_WAIT`) no FAILED PR is opened. A step whose judge failed any other way still records,
and the record and PR say which step didn't run (`run.failed`).

Usage:
    pixi run judge-weekly                  # pin origin/main, sweep, verify, record, open the PR
    pixi run judge-weekly -- --no-pr       # write the record only
    pixi run judge-weekly -- --dry-run     # print the record; write nothing
"""
from __future__ import annotations

import argparse
import datetime as _dt
import importlib.util as _importlib_util
import json
import os
import pathlib
import shutil
import subprocess
import sys
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import read_events  # noqa: E402
from cecelia.effectiveness.git_context import git_output  # noqa: E402
from cecelia.effectiveness.log import default_log_path  # noqa: E402
from cecelia.utils.atomic_io import write_json_atomic  # noqa: E402


def _load_sibling(name: str):
    spec = _importlib_util.spec_from_file_location(f"_judge_{name}", pathlib.Path(__file__).parent / f"{name}.py")
    mod = _importlib_util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_record = _load_sibling("record")
_review = _load_sibling("review")
_bugs = _load_sibling("bugs")
_verify = _load_sibling("verify")
_rules = _load_sibling("rules")
_judge = _load_sibling("judge")
_run_reviews = _load_sibling("run_reviews")


#: Exit code of a pass stopped by the usage limit (sysexits `EX_TEMPFAIL`): `cron_pass.sh` retries it.
EX_TEMPFAIL = 75
#: A reset further off than this isn't waited for: that attempt is the last, and opens the FAILED PR.
#: `cron_pass.sh` follows the `retry` this decides; it holds no rule of its own.
RETRY_MAX_WAIT = _dt.timedelta(hours=8)


class JudgeRunError(RuntimeError):
    def __init__(self, stage: str, message: str):
        super().__init__(message)
        self.stage = stage


# ── deterministic steps ────────────────────────────────────────────────────────────────────────

def state_dir() -> pathlib.Path:
    return default_log_path().parent


def ratelimit_path() -> pathlib.Path:
    """Where a pass stopped by the usage limit says when it lifts, for `cron_pass.sh`."""
    return state_dir() / "judge-ratelimit.json"


def _note_rate_limit(error: Exception, stage: str) -> bool:
    """Write `judge-ratelimit.json` (`reset`, `message`, `retry`, `stage`); returns whether the wrapper
    will rerun the pass: it has attempts left (`JUDGE_RETRY_LEFT`) and the reset is close enough."""
    now = _dt.datetime.now(_dt.timezone.utc)
    reset = _judge.reset_at(str(error), now)
    try:
        left = int(os.environ.get("JUDGE_RETRY_LEFT") or 0)
    except ValueError:
        left = 0
    retry = left > 0 and reset - now <= RETRY_MAX_WAIT
    path = ratelimit_path()
    path.parent.mkdir(parents=True, exist_ok=True)
    write_json_atomic(path, {"reset": reset.isoformat(), "message": str(error), "retry": retry, "stage": stage})
    print(f"  usage limit: lifts {reset.isoformat()}; "
          + ("the wrapper retries then" if retry else "no retry follows"), file=sys.stderr)
    return retry


def default_worktree() -> pathlib.Path:
    return state_dir() / "judge-worktree"


_PIXI_ENV_PREFIXES = ("PIXI_", "CONDA_")


def clean_env(**extra: str) -> dict:
    """`os.environ` without the activation of the pixi env this script runs in.

    The pass itself runs under `pixi run`, and every command it starts in the pinned
    worktree would otherwise inherit that activation: `PIXI_PROJECT_MANIFEST` (pixi warns and
    may pick the wrong manifest), `PYTHONPATH` and the env's `bin/` on `PATH` (so `import cecelia`
    could load this checkout instead of the pinned one).
    """
    env = {k: v for k, v in os.environ.items()
           if not k.startswith(_PIXI_ENV_PREFIXES) and k != "PYTHONPATH"}
    env["PATH"] = os.pathsep.join(p for p in os.environ.get("PATH", "").split(os.pathsep)
                                  if "/.pixi/envs/" not in p.replace("\\", "/"))   # either separator
    return {**env, **extra}


def _run(cmd: list[str], *, cwd: pathlib.Path, stage: str, env: dict | None = None,
         timeout: float | None = None, input: str | None = None) -> subprocess.CompletedProcess:
    try:
        proc = subprocess.run(cmd, cwd=str(cwd), env=env if env is not None else clean_env(), capture_output=True, text=True, input=input,
                              encoding="utf-8", timeout=timeout, check=False)
    except (OSError, subprocess.TimeoutExpired) as e:
        raise JudgeRunError(stage, f"{cmd[0]} failed: {e}") from e
    if proc.returncode != 0:
        raise JudgeRunError(stage, f"{' '.join(cmd[:3])} exited {proc.returncode}: "
                                     f"{(proc.stderr or proc.stdout or '').strip()[-600:]}")
    return proc


def pin(ref: str, repo: pathlib.Path = _REPO) -> str:
    """Fetch, then resolve `ref` to a commit SHA."""
    git_output("fetch", "--quiet", "origin", cwd=str(repo))   # offline still pins the local ref
    sha = git_output("rev-parse", "--verify", f"{ref}^{{commit}}", cwd=str(repo))
    if not sha:
        raise JudgeRunError("pin", f"can't resolve {ref!r}")
    return sha


def prepare_worktree(path: pathlib.Path, sha: str, repo: pathlib.Path = _REPO) -> None:
    """One persistent worktree, reset to `sha` each run, its `.pixi` reused (publish runs recital there)."""
    if not (path / ".git").exists():
        path.parent.mkdir(parents=True, exist_ok=True)
        _run(["git", "worktree", "add", "--detach", str(path), sha], cwd=repo, stage="worktree")
    else:
        # detach first: the last run left the worktree on its `judge-run/*` branch, and a reset
        # there would move that branch
        _run(["git", "checkout", "--quiet", "--detach"], cwd=path, stage="worktree")
        _run(["git", "reset", "--hard", "--quiet", sha], cwd=path, stage="worktree")
        # keep the env and deps; anything else an earlier run left behind goes
        _run(["git", "clean", "-fdx", "--quiet", "-e", ".pixi", "-e", ".env",
              "-e", "frontend/node_modules"], cwd=path, stage="worktree")
    if (repo / ".env").is_file() and (repo / ".env").resolve() != (path / ".env").resolve():
        shutil.copy(str(repo / ".env"), str(path / ".env"))
    head = git_output("rev-parse", "HEAD", cwd=str(path))
    if head != sha:
        raise JudgeRunError("worktree", f"worktree HEAD is {head}, expected {sha}")
    pixi = shutil.which("pixi")
    if not pixi:
        raise JudgeRunError("worktree", "pixi not on PATH")
    _run([pixi, "install"], cwd=path, stage="worktree")


_ATTRIBUTION = "Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
_PR_FOOTER = "🤖 Generated with [Claude Code](https://claude.com/claude-code)"


def _title(record: dict) -> str:
    if record.get("kind") == "failure":
        return f"judge: run record {record['date']} (FAILED)"
    n = sum(b["status"] == "open" for b in record["bugs"])
    return f"judge: run record {record['date']} ({n} open bug{'s' if n != 1 else ''})"


def _pr_body(record: dict) -> str:
    date, rel = record["date"], f"{_record.MIRROR_REL}/{record['date']}.md"
    if record.get("kind") == "failure":
        return (f"The weekly judge pass for {date} **failed** at `{record['run']['stage']}`: "
                f"`{record['run']['error']}`.\n\nRecord: `{rel}`. The previous run's PR stays open."
                f"\n\n{_PR_FOOTER}\n")
    bugs = record["bugs"]
    n = {k: sum(b["status"] == k for b in bugs) for k in ("open", "gone", "unjudged")}
    new = sum(_record.newly_open(b, date) for b in bugs)
    verified = sum(b["status"] == "open" and bool(b.get("verify")) for b in bugs)
    decide = len(record["queue"])
    landed, confirmed = _record.landed_counts(bugs)
    lines = [f"Weekly judge, {date}. Record: [`{rel}`]({rel}). Point a session at it to work the bugs.", "",
             f"**Bugs: {n['open']} open** ({verified} verified, {new} new), {n['gone']} fixed since the last pass"
             + (f" ({landed} fix(es) landed since the last pass, {confirmed} confirmed gone by the judge)"
                if landed else "")
             + (f", {n['unjudged']} waiting for the judge" if n["unjudged"] else "")
             + (f", {decide} for you to decide (`pixi run judge-review`)" if decide else "") + "."]
    failed = record["run"].get("failed") or {}
    if failed.get("sweep"):
        lines += ["", f"**The bug sweep's judge failed** (`{_record.one_line(failed['sweep'])}`): nothing was "
                      "re-checked, so open bugs stay open and new findings wait for the next pass."]
    agents = (record["run"]["spend"].get("verify") or {}).get("failed")
    if agents:
        lines += ["", f"**Verify:** {agents} bug(s) unverified, their agent failed (the cron log has why)."]
    props = record["proposals"]
    lines += ["", f"**Rules: {len(props)} proposal(s).**" if props else
              f"**Rules: the judge failed** (`{_record.one_line(failed['rules'])}`), nothing tallied."
              if failed.get("rules") else "**Rules:** nothing broken in enough sessions."]
    lines += [f"- {p['id']} · {p['kind']}: {p['summary']}" for p in props]
    lines += ["", f"- Tokens: {_record.tokens_line(record['run']['spend'])}",
              f"- Spend (list price): {_record.spend_line(record['run']['spend'])}", "", _PR_FOOTER]
    return "\n".join(lines) + "\n"


def publish(record: dict, *, worktree: pathlib.Path, run: _t.Callable[..., subprocess.CompletedProcess] = None) -> str:
    """Commit the record on `judge-run/<date>`, open its PR, close older ones (a failure closes none).

    Runs in the persistent worktree, which sits at the pinned SHA. One open PR at a time: the record
    is already in the local store, so closing an unmerged one loses nothing. Returns the PR's URL.
    """
    run = run or (lambda cmd, **kw: _run(cmd, cwd=worktree, stage="publish", **kw))
    pixi, gh = shutil.which("pixi") or "pixi", shutil.which("gh") or "gh"
    branch = f"judge-run/{record['date']}"
    run(["git", "checkout", "--quiet", "-B", branch])
    _record.write(record, mirror=True, force=True, mirror_dir=worktree / _record.MIRROR_REL)
    if record.get("kind") != "failure":
        run([pixi, "run", "audit-rollup"])
    run(["git", "add", "docs/ai-assist"])
    recital = run([pixi, "run", "recital"]).stdout   # docs-only: no reviewer spawns, but the tails
    run(["git", "commit", "--quiet", "-F", "-"], input=f"{_title(record)}\n\n{recital.strip()}\n\n{_ATTRIBUTION}\n")
    run(["git", "push", "--quiet", "--force", "-u", "origin", branch])
    existing = json.loads(run([gh, "pr", "list", "--state", "open", "--json", "number,headRefName,url"]).stdout)
    mine = next((p["url"] for p in existing if p["headRefName"] == branch), None)
    url = mine or run([gh, "pr", "create", "--base", "main", "--head", branch,
                       "--title", _title(record), "--body-file", "-"], input=_pr_body(record)).stdout.strip()
    if record.get("kind") == "failure":   # it supersedes nothing: the last pass's bugs are still the work list
        return url
    for p in existing:
        if p["headRefName"].startswith("judge-run/") and p["headRefName"] != branch:
            run([gh, "pr", "close", str(p["number"]), "--comment", f"Superseded by {url}."])
    return url


# ── the pass ───────────────────────────────────────────────────────────────────────────────────

def _now() -> str:
    return _dt.datetime.now(_dt.timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def weekly(*, ref: str = "origin/main", worktree: pathlib.Path | None = None, date: str | None = None,
           live: bool = True, persist: bool = True, bug_judge: _t.Callable | None = None,
           merged_prs: _t.Callable | None = None, verifier: _t.Callable | None = None,
           assign: _t.Callable | None = None, state: dict | None = None,
           projects: pathlib.Path | None = None) -> dict:
    """Run one pass; returns its record, unwritten.

    `live=False` checks the local `ref` without fetching or preparing the worktree (a re-run, a test).
    `persist=False` (a dry run) leaves the earlier records in the store and the log untouched.
    `projects`: where the agent run records are (default: `run_reviews.projects_dir()`). `state` is
    updated as it goes (`stage`, `sha`), so a crash can say where it stopped.
    """
    state = state if state is not None else {}
    ts = _now()
    date = date or ts[:10]
    state["stage"] = "pin"
    if live:
        sha = state["sha"] = pin(ref)
        state["stage"] = "worktree"
        prepare_worktree(worktree or default_worktree(), sha)
    else:
        sha = state["sha"] = git_output("rev-parse", "--verify", f"{ref}^{{commit}}", cwd=str(_REPO))
        if not sha:
            raise JudgeRunError("pin", f"can't resolve {ref!r}")
    # the owner's answers since the last pass go into the earlier records first, so a `wont_fix`
    # bug is not carried
    state["stage"] = "reviews"
    reviews = _review.read_reviews()
    history = []
    for earlier in _record.pass_records(before=date):
        applied = _review.apply_reviews(earlier, reviews)
        if persist and applied != earlier:
            _record.write(applied, force=True)
        history.append(applied)
    previous = history[-1] if history else None
    events = list(read_events())
    # a person's causes on run records: new `guide` / `platform` ones join the log the sweep reads
    state["stage"] = "run reviews"
    reviewed = _run_reviews.scan(projects)
    events += _run_reviews.log_new(reviewed, events, pass_ts=ts, write=persist)
    state["stage"] = "bugs"
    sweep_tokens: dict = {}
    failed: dict = {}   # step → why its judge didn't run; a usage limit raises instead
    bugs, sweep_usd = _bugs.sweep(events, date=date, sha=sha, previous=previous, judge=bug_judge,
                                  merged_prs=merged_prs, meter=sweep_tokens, failures=failed)
    state["stage"] = "verify"
    bugs, verified = _verify.verify(bugs, date=date, sha=sha, agent=verifier)
    state["stage"] = "rules"
    rules_tokens: dict = {}
    rows, proposals, bins, rules_usd = _rules.propose(events, date=date, assign=assign, meter=rules_tokens,
                                                      failures=failed)
    steps = {"sweep": sweep_tokens, "verify": verified.get("tokens") or {}, "rules": rules_tokens}
    total: dict = {}
    for t in steps.values():
        _judge.add_tokens(total, t)
    spend = {"sweep_usd": round(sweep_usd, 4), "verify_usd": verified["usd"], "rules_usd": round(rules_usd, 4),
             "total_usd": round(sweep_usd + verified["usd"] + rules_usd, 4), "verify": verified,
             "tokens": {**steps, "total": total},
             "finding_bins": bins}
    record = _record.build(date, ts=ts, sha=sha, bugs=bugs, rules=rows, proposals=proposals, spend=spend,
                           agent_causes=_run_reviews.agent_causes(reviewed))
    record["run"].update(rules_window_days=_rules.WINDOW_DAYS, min_sessions=_rules.MIN_SESSIONS,
                         **({"failed": failed} if failed else {}))
    return record


def _write_failure(date: str | None, stage: str, error: Exception, sha: str | None) -> dict | None:
    """Write the failure record, unless that date already has a pass record: never replace data."""
    date = date or _now()[:10]
    dest = _record.store_root() / f"{date}.json"
    try:
        if dest.is_file() and _record.load(dest).get("kind") == "pass":
            print(f"  {dest} holds a pass record; the failure is in the cron log only", file=sys.stderr)
            return None
        failed = _record.failure_record(date, stage=stage, error=f"{type(error).__name__}: {error}", sha=sha)
        print(f"  wrote {_record.write(failed, force=True)[0]}", file=sys.stderr)
        return failed
    except (OSError, ValueError, _record.RecordError) as w:
        print(f"  failure record not written: {w}", file=sys.stderr)
        return None


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--ref", default="origin/main", help="what to pin and check (default origin/main)")
    ap.add_argument("--worktree", type=pathlib.Path, help=f"persistent worktree (default {default_worktree()})")
    ap.add_argument("--date", help="record date (default: today, UTC)")
    ap.add_argument("--dry-run", action="store_true", help="print the record's markdown; write nothing")
    ap.add_argument("--no-pr", action="store_true", help="write the record; don't commit, push or open a PR")
    ap.add_argument("--projects-dir", type=pathlib.Path,
                    help=f"where the agent run records are (default {_run_reviews.projects_dir()})")
    args = ap.parse_args(argv)

    lock_path = state_dir() / "judge.lock"
    lock_path.parent.mkdir(parents=True, exist_ok=True)
    lock = open(lock_path, "w", encoding="utf-8")
    if os.name == "posix":   # the timer runs on Linux; on Windows two passes simply aren't guarded
        import fcntl
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except OSError:
            print("judge-weekly: another pass holds the lock; exiting", file=sys.stderr)
            return 0
    state = {"stage": "start", "sha": None}
    try:
        record = weekly(ref=args.ref, worktree=args.worktree, date=args.date, persist=not args.dry_run,
                        state=state, projects=args.projects_dir.expanduser() if args.projects_dir else None)
        if args.dry_run:
            print(_record.render_markdown(record))
            return 0
        path = _record.write(record, force=True)[0]   # a rerun the same day replaces that day's record
        n = sum(b["status"] == "open" for b in record["bugs"])
        print(f"{record['date']}: {n} open bug(s), {len(record['proposals'])} rule proposal(s)\n"
              f"  tokens: {_record.tokens_line(record['run']['spend'])}\n"
              f"  spend (list price): {_record.spend_line(record['run']['spend'])}\n  wrote {path}")
        if not args.no_pr:
            state["stage"] = "publish"
            print(f"  PR {publish(record, worktree=args.worktree or default_worktree())}")
        return 0
    except Exception as e:  # noqa: BLE001 — any crash still leaves a record
        where = e.stage if isinstance(e, JudgeRunError) else state["stage"]
        print(f"judge-weekly: failed at {where}: {type(e).__name__}: {e}", file=sys.stderr)
        limited = isinstance(e, _judge.RateLimited)
        retrying = limited and not args.dry_run and _note_rate_limit(e, where)
        if not args.dry_run:
            # written even when a retry follows: the retry's pass record replaces it
            failed = _write_failure(args.date, where, e, state["sha"])
            # a failure gets its PR too, once there is a worktree at the pinned SHA to commit from;
            # not while a retry follows, which would open its own
            if failed and not args.no_pr and not retrying and where not in ("start", "pin", "worktree", "publish"):
                try:
                    print(f"  PR {publish(failed, worktree=args.worktree or default_worktree())}", file=sys.stderr)
                except Exception as p:  # noqa: BLE001
                    print(f"  failure PR not opened: {p}", file=sys.stderr)
        return EX_TEMPFAIL if limited else 1
    finally:
        lock.close()


if __name__ == "__main__":
    sys.exit(main())
