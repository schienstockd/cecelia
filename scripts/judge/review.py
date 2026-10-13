#!/usr/bin/env python3
"""Weekly judge — the owner's queue, in the terminal: decide the bugs, then work them.

First the bugs a verify agent sent to the owner (`decide`): follow the agent's recommendation (the bug
stays open), answer it in your own words (a session follows yours instead), or
`wont_fix` to stop carrying it. Then the rest of the open work list: `fix now` starts an
interactive Claude Code session where the owner starts sessions (the folder holding the checkouts),
briefed on that bug and told to make its own worktree; the queue resumes when it exits. Work bugs that
share a reviewed branch or a file come as one bundle (verify's grouping): `fix now` briefs one session
on all of them, `one by one` splits it back into single cards. Every answer is an append-only event in `~/.cecelia-effectiveness/review.jsonl`; an undo is a
correcting event, never an edit. The weekly pass applies the events at the start of the next pass.

Last, the record's rule proposals: `edit the rule` starts a session briefed on the CLAUDE.md section
and the findings that broke it; `reject` drops it. Either way the next pass counts only the findings
made after the answer, so a rule is proposed again only if sessions keep breaking it.

`--auto` works the bugs verify called `fix` without the owner, one sandboxed session per bundle,
each ending in a PR; never a merge (`auto_fix.py`).

Usage:
    pixi run judge-review                # the newest record
    pixi run judge-review --date 2026-10-06
    pixi run judge-review --auto --dry-run      # the briefs it would send
    pixi run judge-review --auto [--limit 3]    # work the first session(s)
"""
from __future__ import annotations

import argparse
import datetime as _dt
import importlib.util as _importlib_util
import json
import os
import pathlib
import re
import subprocess
import sys
import typing as _t
import uuid

_REPO = pathlib.Path(__file__).resolve().parents[2]

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import git_context, read_events  # noqa: E402
from cecelia.effectiveness.claude_cli import RateLimited, limit_exit, resolve_claude_bin  # noqa: E402
# one look with `pixi run recital-console`: its divider, hanging-indent wrap and the shared palette
from cecelia.effectiveness.console import (  # noqa: E402
    _BOLD, _DIM, ENTER_ALT_SCREEN, LEAVE_ALT_SCREEN, LEFT, RESIZE, RIGHT, _col, _hr, _terminal_size, _wrap_desc,
    poll_key, scroll_step, scroll_window)
from cecelia.effectiveness.palette import (  # noqa: E402
    BLUE, BLUISH_GREEN, GREY, ORANGE, REDDISH_PURPLE, SKY_BLUE, VERMILLION, YELLOW)


def _load_sibling(name: str):
    spec = _importlib_util.spec_from_file_location(f"_judge_{name}", pathlib.Path(__file__).parent / f"{name}.py")
    mod = _importlib_util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_record = _load_sibling("record")
_issues = _load_sibling("issues")
_rules = _load_sibling("rules")
_verify = _load_sibling("verify")

REVIEW_SCHEMA_VERSION = 1

#: queue item kind → {key the owner presses: (event, value)}. `decide` items are the record's queue;
#: `work` items are the rest of its open bugs; a `bundle` is work items one session takes together.
#: `s` (one by one) records nothing: it splits the bundle for this session.
ANSWERS: dict[str, dict[str, tuple[str, str] | None]] = {
    "decide": {"w": ("bug_status", "wont_fix"), "o": ("bug_status", "open"), "a": ("bug_status", "open")},
    "work": {"f": ("bug_work", "fix_session"), "w": ("bug_status", "wont_fix")},
    "bundle": {"f": ("bug_work", "fix_session"), "s": None},
    "rule": {"e": ("rule_status", "accepted"), "r": ("rule_status", "rejected")},
}
REVIEW_EVENTS = ("bug_status", "bug_work", "rule_status")
_CONTROL = {"n": "skip", "z": "undo", "q": "quit"}


def review_path() -> pathlib.Path:
    """`review.jsonl` beside the record store and the effectiveness log."""
    return _record.store_root().parent / "review.jsonl"


def append_review(event: str, record_date: str, ref: str, value: str | None, *,
                  note: str | None = None, corrects: str | None = None,
                  path: pathlib.Path | None = None) -> dict:
    """Append one owner event. `value=None` with `corrects` is an undo."""
    if event not in REVIEW_EVENTS:
        raise ValueError(f"unknown review event {event!r}")
    row = {"schema_version": REVIEW_SCHEMA_VERSION, "id": uuid.uuid4().hex[:12],
           "ts": _now(), "event": event, "record": record_date, "ref": ref, "value": value}
    if note:
        row["note"] = note
    if corrects:
        row["corrects"] = corrects
    _append_jsonl(path or review_path(), row)
    return row


def _now() -> str:
    return _dt.datetime.now(_dt.timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z")


def _append_jsonl(path: pathlib.Path, row: dict) -> None:
    """One row, flushed to disk: the owner's logs are append-only."""
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as fh:
        fh.write(json.dumps(row, ensure_ascii=False) + "\n")
        fh.flush()
        os.fsync(fh.fileno())


def read_reviews(path: pathlib.Path | None = None) -> list[dict]:
    """Every owner event, oldest first. Torn lines are skipped by `read_events`; rows of another
    schema or event are skipped here."""
    return [row for row in read_events(path or review_path())
            if row.get("schema_version") == REVIEW_SCHEMA_VERSION and row.get("event") in REVIEW_EVENTS]


def applied_pass_records(before: str | None = None) -> list[dict]:
    """`record.pass_records` with the owner's answers folded in: the view every reader of the
    history wants, late answers to an older record included (as the weekly pass applies them)."""
    reviews = read_reviews()
    records = _record.pass_records(before=before)
    return [apply_reviews(r, reviews, older=records[:i]) for i, r in enumerate(records)]


def current(reviews: _t.Iterable[dict]) -> dict[tuple[str, str, str], dict]:
    """The latest event per (record, event, ref): later events, undos included, win."""
    out: dict[tuple[str, str, str], dict] = {}
    for r in reviews:
        out[(r["record"], r["event"], r["ref"])] = r
    return out


def late_answers(record: dict, now: dict[tuple[str, str, str], dict],
                 older: _t.Sequence[dict]) -> dict[tuple[str, str], dict]:
    """(event, bug key) → the latest answer given to an `older` record after `record`'s pass started."""
    start = (record.get("run") or {}).get("ts") or ""
    late: dict[tuple[str, str], dict] = {}
    for r in older:
        keys = {b["id"]: b["key"] for b in r.get("bugs", [])}
        for (date, event, ref), row in now.items():
            if date == r["date"] and ref in keys and row.get("ts", "") > start and row.get("value") is not None:
                if row["ts"] > late.get((event, keys[ref]), {}).get("ts", ""):
                    late[(event, keys[ref])] = row
    return late


def apply_reviews(record: dict, reviews: _t.Iterable[dict], older: _t.Sequence[dict] = ()) -> dict:
    """`record` with the owner's bug answers folded in. What the weekly pass does to the previous
    record before it carries its bugs: a status answer sets `status` / `owner_decision` /
    `owner_answer`, a fix session sets `fix_session` (its time; the issue mirror labels it).

    `older`: earlier records whose answers came after `record`'s pass started. The owner was still
    working the older record then, so those answers apply here too, matched by bug key; the latest
    answer per bug wins."""
    now = current(reviews)
    record = json.loads(json.dumps(record))   # deep copy; the caller's record is left alone
    late = late_answers(record, now, older)

    def answer(event: str, b: dict) -> dict | None:
        rows = [r for r in (now.get((record["date"], event, b["id"])), late.get((event, b.get("key"))))
                if r and r.get("value") is not None]
        return max(rows, key=lambda r: r.get("ts") or "") if rows else None

    for b in record.get("bugs", []):
        row = answer("bug_status", b)
        if row and row.get("value") in _record.BUG_STATUSES:
            b["status"] = row["value"]
            b["owner_decision"] = row["value"]   # a `decide` bug kept open joins the work list
            if row.get("note"):
                b["owner_answer"] = row["note"]
        work = answer("bug_work", b)
        if work and work.get("value") == "fix_session":
            b["fix_session"] = work["ts"]
    return record


_REPO_SLUG: list = []


def repo_slug() -> str | None:
    """This repo's `owner/name` for issue links, once; None when origin isn't GitHub."""
    if not _REPO_SLUG:
        try:
            _REPO_SLUG.append(_issues.repo_slug())
        except _issues.GhError:
            _REPO_SLUG.append(None)
    return _REPO_SLUG[0]


#: Work-list order: what is known live first, then what the owner already decided, then the rest.
_WORK_ORDER = {"fix": 0, "decide": 1}


def work_items(record: dict) -> list[dict]:
    """Every open bug, live ones first, except a decide item the owner hasn't answered yet and a
    `guard` one: parked (`bugs.py`), though a record from before parking still has it `open`."""
    queued = {item["ref"] for item in record.get("queue", [])}
    bugs = [b for b in record.get("bugs", []) if b["status"] == "open"
            and (b.get("verify") or {}).get("verdict") != "guard"
            and (b["id"] not in queued or b.get("owner_decision") == "open")]
    bugs.sort(key=lambda b: _WORK_ORDER.get((b.get("verify") or {}).get("verdict"), 3))
    return [{"kind": "work", "ref": b["id"]} for b in bugs]


def items_of(record: dict) -> list[dict]:
    """The whole queue: the record's decide items, the work list, then its rule proposals."""
    return ([{"kind": "decide", "ref": item["ref"]} for item in record.get("queue", [])] + work_items(record)
            + [{"kind": "rule", "ref": p["id"]} for p in record.get("proposals", [])])


def pending(record: dict, reviews: _t.Iterable[dict], older: _t.Sequence[dict] = ()) -> list[dict]:
    """The queue's items with no live answer yet, in queue order. An answer given to an `older`
    record after this one's pass started counts too (`apply_reviews`)."""
    reviews = list(reviews)
    now = current(reviews)
    late = late_answers(record, now, older)
    keys = {b["id"]: b.get("key") for b in record.get("bugs", [])}
    start = (record.get("run") or {}).get("ts") or ""
    # a rule answered on an older record after this pass proposed it again: the answer covers this one too
    late_rules = {rule for rule, ts in rule_answers(older, reviews).items() if start and _rules._utc(ts) > _rules._utc(start)}
    rule_of = {p["id"]: p["rule"] for p in record.get("proposals", [])}

    def live(event: str, ref: str) -> str | None:
        row = now.get((record["date"], event, ref))
        if row and row.get("value") is not None:
            return row["value"]
        return (late.get((event, keys.get(ref))) or {}).get("value")

    def answered(item: dict) -> bool:
        if item["kind"] == "rule":
            return ((now.get((record["date"], "rule_status", item["ref"])) or {}).get("value") is not None
                    or rule_of.get(item["ref"]) in late_rules)
        if item["kind"] == "decide":
            return live("bug_status", item["ref"]) is not None
        # a work item is done once a fix session was started or it was answered won't-fix; a decide
        # answer of `open` is what put it on the work list, so it doesn't count here
        return live("bug_work", item["ref"]) is not None or live("bug_status", item["ref"]) == "wont_fix"
    return [item for item in items_of(record) if not answered(item)]


def bundles(record: dict, items: _t.Sequence[dict], apart: _t.Collection[str] = ()) -> list[dict]:
    """`items` with work items joined into one `bundle` per fix session: bugs that share a reviewed
    branch or a file (`verify.groups`, at most its GROUP_MAX), whose fixes likely touch the same code.
    A bundle sits where its first bug was and keeps the work list's order inside; `apart` (bug ids the
    owner split) stay single."""
    by_id = {b["id"]: b for b in record.get("bugs", [])}
    order = [it["ref"] for it in items if it["kind"] == "work" and it["ref"] not in apart]
    group_of = {b["id"]: n for n, g in enumerate(_verify.groups([by_id[r] for r in order])) for b in g}
    members: dict[int, list[str]] = {}
    for r in order:
        members.setdefault(group_of[r], []).append(r)
    out: list[dict] = []
    for it in items:
        grouped = it["kind"] == "work" and it["ref"] in group_of
        refs = members.pop(group_of[it["ref"]], None) if grouped else [it["ref"]]
        if refs is None:
            continue   # in a bundle already placed
        out.append({"kind": "bundle", "ref": refs[0], "refs": refs} if len(refs) > 1 else it)
    return out


def _origin(bug: dict) -> str:
    """Where a bug came from: the reviewed branch, or the agent runs that hit it."""
    if bug.get("review"):
        return (f"marked bad (cause {bug.get('cause')}) by the reviewer of agent run {bug.get('run') or '?'}, "
                f"Blackboard entry `{bug.get('entry')}` in project `{bug.get('project')}`")
    if bug.get("kind") == "agent_run":
        return f"hit in {bug.get('runs') or 1} agent run(s), first at commit `{(bug.get('commit') or '?')[:8]}`"
    return f"raised on branch `{bug.get('branch') or '?'}`"


def _bug_lines(bug: dict) -> list[str]:
    """What a fix brief says about one bug: where, the finding, what verify found, the answer to follow."""
    v = bug.get("verify") or {}
    lines = [f"Where: {_record._bug_where(bug)}, {_origin(bug)}.", f"Finding: {bug['desc']}"]
    lines += [f"Also raised: {a['desc']}" for a in bug.get("also", [])]
    if v:
        lines.append(f"Verified ({v.get('verdict')}): {v.get('effect', '')}")
    if bug.get("owner_answer"):
        lines.append(f"My answer, which decides the fix over the agent's recommendation: {bug['owner_answer']}")
    elif v.get("recommendation"):
        lines.append(f"Recommendation: {v['recommendation']}")
    if v.get("trigger"):
        lines.append(f"Live once: {v['trigger']}")
    if v.get("evidence"):
        lines.append(f"Evidence: {v['evidence']}")
    if bug.get("kind") == "duplicate":
        lines += ["", "This is a duplicate, not a defect: the fix is to call the existing helper the finding "
                  "names and delete the copy, at every call site of the copy."]
    if bug.get("kind") == "stranded":
        lines += ["", "These commits were pushed to the PR's branch after it merged: "
                  + ", ".join(bug.get("commits", [])) + ". Land them in a new PR if they are still wanted."]
    return lines


def _issue_of(bug: dict) -> int | None:
    return (bug.get("issue") or {}).get("number")


def _record_line(record: dict) -> str:
    return f"the weekly judge record of {record['date']} (`{_record.store_root() / (record['date'] + '.json')}`)"


def _worktree_line(name: str) -> str:
    main = main_checkout()
    return (f"work in your own worktree: from `{main}`, run `pixi run bootstrap-worktree {name}` and work in "
            f"the worktree it creates, never in `{main}` itself.")


def fix_brief(record: dict, bug: dict) -> str:
    """The opening prompt of a fix session: the bug, what verify found, and how to work it."""
    lines = [f"Work bug {bug['id']} (`{bug['key']}`) from {_record_line(record)}.", "", *_bug_lines(bug)]
    issue = _issue_of(bug)
    if issue:
        lines.append(f"Tracked as issue #{issue}. Don't read it: this brief is the whole task, and the issue "
                     f"is a mirror of the record. Say `Refs #{issue}` in the commit message, never `fixes` or "
                     "`closes`: the judge closes it once it confirms the fix.")
    lines += ["", "Confirm it on origin/main first. If it isn't real, say so and stop; I'll answer it won't-fix. "
              f"Otherwise {_worktree_line('fix-' + bug['key'])} Fix it there, including every sibling call site "
              "with the same shape, "
              + ("make sure the helper's tests cover what the copy did, " if bug.get("kind") == "duplicate" else
                 "add a test that fails without the fix, ")
              + "run the matching tests and recital, "
              f"and name `{bug['key']}` in the commit message. Ask before committing, as usual."]
    return "\n".join(lines)


def bundle_brief(record: dict, bugs: _t.Sequence[dict]) -> str:
    """The opening prompt of one fix session for a bundle: every bug's brief, then how to work them
    together. Each key still goes in the commit that fixes it: landed-fix detection is per bug."""
    lines = [f"Work these {len(bugs)} bugs from {_record_line(record)} in one session. They share a reviewed "
             "branch or a file, so their fixes likely touch the same code."]
    for b in bugs:
        lines += ["", f"## {b['id']} (`{b['key']}`)", *_bug_lines(b)]
        if _issue_of(b):
            lines.append(f"Tracked as issue #{_issue_of(b)}.")
    issues = [f"#{_issue_of(b)}" for b in bugs if _issue_of(b)]
    keys = ", ".join(f"`{b['key']}`" for b in bugs)
    lines += ["", "Confirm each on origin/main first. One that isn't real: say so and leave it; I'll answer it "
              f"won't-fix. Then {_worktree_line('fix-bundle-' + bugs[0]['key'])} Fix the real ones there, "
              "including every sibling call site with the same shape, add a test per fix that fails without it, "
              f"run the matching tests and recital, and name each fixed bug's key ({keys}) in the commit that "
              "fixes it."
              + (f" The issues ({', '.join(issues)}) mirror the record: don't read them. Say `Refs #N` for each "
                 "one you fix, never `fixes` or `closes`: the judge closes it once it confirms the fix."
                 if issues else "")
              + " End by listing which keys you fixed and which you left, and why. Ask before committing, as usual."]
    return "\n".join(lines)


#: Sessions `--auto` spends on a bug before leaving it to the owner.
AUTO_ATTEMPTS = 2


def auto_path() -> pathlib.Path:
    """`auto_fix.jsonl` beside `review.jsonl`: what each `--auto` session said about each bug."""
    return review_path().parent / "auto_fix.jsonl"


def read_auto(path: pathlib.Path | None = None) -> dict[str, dict]:
    """Bug key → the latest `--auto` outcome for it, over every record."""
    return {row["key"]: row for row in read_events(path or auto_path()) if row.get("key")}


def append_auto(row: dict, path: pathlib.Path | None = None) -> None:
    _append_jsonl(path or auto_path(), {"ts": _now(), **row})


def auto_items(record: dict, reviews: _t.Iterable[dict], older: _t.Sequence[dict] = (),
               tried: _t.Mapping[str, dict] = {}) -> list[dict]:
    """The pending work `--auto` may take, bundled: every bug verify called `fix` with no landed fix
    (`fix_landed`), except one an auto session already called `not_real` or `left`, found `landed`
    on main, or failed on `AUTO_ATTEMPTS` times (its card says why; the owner takes it from there)."""
    by_id = {b["id"]: b for b in record.get("bugs", [])}

    def eligible(ref: str) -> bool:
        b = by_id[ref]
        last = tried.get(b["key"]) or {}
        return ((b.get("verify") or {}).get("verdict") == "fix"
                and not b.get("fix_landed")   # a commit naming it landed: the next pass re-checks it
                and last.get("outcome") not in ("not_real", "left", "landed")
                and not (last.get("outcome") == "failed" and last.get("attempt", 1) >= AUTO_ATTEMPTS))
    return bundles(record, [it for it in pending(record, reviews, older)
                            if it["kind"] == "work" and eligible(it["ref"])])


def rule_answers(records: _t.Iterable[dict], reviews: _t.Iterable[dict]) -> dict[str, str]:
    """rule → when the owner last accepted or rejected a proposal for it, over every record. The
    rules step counts only the findings after it, as after an edit of the section (`rules.since_edit`)."""
    now = current(reviews)
    out: dict[str, str] = {}
    for r in records:
        rules = {p["id"]: p["rule"] for p in r.get("proposals", [])}
        for (date, event, ref), row in now.items():
            if event == "rule_status" and date == r.get("date") and ref in rules and row.get("value") is not None:
                out = _rules.latest(out, {rules[ref]: row.get("ts", "")})
    return out


_RULE_HOW = {"tighten": "Agents keep breaking it in code they wrote. Reword the rule so the next agent can't "
                        "miss it, or turn it into a mechanical check (a test, a lint, a recital step).",
             "ratchet": "Older code keeps the shape alive, and new code copies it. Add a test that bans the "
                        "shape, with the existing sites as an allow-list that may only shrink."}


def source_findings(slugs: _t.Iterable[str], events: _t.Iterable[dict] | None = None) -> list[dict]:
    """The finding rows a proposal cites, from the effectiveness log, in `slugs` order."""
    want = list(slugs)
    found: dict[str, dict] = {}
    for e in (read_events() if events is None else events):
        slug = (e.get("payload") or {}).get("slug")
        if slug in want and e.get("event") in _rules._FINDING_EVENTS:
            found[slug] = e["payload"]
    return [found[s] for s in want if s in found]


def rule_brief(record: dict, p: dict, findings: _t.Sequence[dict]) -> str:
    """The opening prompt of a rule session: the section, why the judge proposes it, and its sources."""
    worktree = f"rule-{p['id'].lower()}-{record['date']}"
    file, _, heading = p["rule"].partition(" → ")
    lines = [f"Work rule proposal {p['id']} from the weekly judge record of {record['date']} "
             f"(`{_record.store_root() / (record['date'] + '.md')}`, section *Rules*).", "",
             f"Rule: `{file}` → section {heading}. Proposal ({p['kind']}): {_why(p)}.",
             _RULE_HOW[p["kind"]], "", f"The findings that broke it ({len(p['sources'])}):"]
    lines += [f"- `{f.get('slug')}` {f.get('file') or ''}: {f.get('desc', '')[:400]}" for f in findings]
    lines += [f"- `{s}` (not in the log any more)" for s in p["sources"][len(findings):]
              if s not in {f.get("slug") for f in findings}]
    lines += ["", "Read the section and the findings, then say in two or three sentences what the findings have "
              "in common and why the rule didn't stop them. If they don't share a cause the rule can fix, say so "
              f"and stop. Otherwise {_worktree_line(worktree)} Keep the rule as short as it is now or shorter; "
              "a mechanical check beats more words. Run recital and ask before committing, as usual."]
    return "\n".join(lines)


def main_checkout(repo: pathlib.Path = _REPO) -> pathlib.Path:
    return git_context.main_checkout(str(repo)) or repo


def workspace(repo: pathlib.Path = _REPO) -> pathlib.Path:
    """Where the owner starts sessions: the folder holding the main checkout and its sibling
    worktrees (`~/cc-workspace/cecelia`). A fix session starts here and makes its own worktree."""
    return main_checkout(repo).parent


def default_launch(prompt: str, cwd: pathlib.Path) -> str | None:
    """An interactive `claude` in `cwd` opening on `prompt`; returns once the owner exits it.
    None when it ran, else why it couldn't start."""
    claude = resolve_claude_bin()
    if not claude:
        return "claude CLI not on PATH"
    try:
        subprocess.run([claude, prompt], cwd=str(cwd), check=False)
    except OSError as e:
        return f"claude failed to start: {e}"
    return None


# ── the terminal queue ─────────────────────────────────────────────────────────────────────────

#: How each answer reads on the prompt and after it, and its colour.
_LABELS = {"w": ("won't fix", VERMILLION), "o": ("follow recommendation", BLUISH_GREEN), "a": ("answer", SKY_BLUE),
           "f": ("fix now", BLUE), "s": ("one by one", GREY), "e": ("edit the rule", BLUE),
           "r": ("reject", VERMILLION)}
_VERDICT_COLOUR = {"decide": ORANGE, "fix": VERMILLION, "guard": YELLOW, "dismiss": GREY}
#: Section label → colour, in the order they are shown; a card shows the ones it has text for.
_SECTIONS = (("Question", SKY_BLUE), ("Bug", SKY_BLUE), ("Verified", ORANGE), ("Auto-fix", BLUE),
             ("Your answer", REDDISH_PURPLE), ("Recommendation", BLUISH_GREEN), ("Live once", YELLOW),
             ("Evidence", GREY))
_MAX_WIDTH = 100   # the recital console's wrap target
#: `file.ext:12`, `file.ext:12-16,40`, and a bare `:368` that continues the previous file.
_REF = re.compile(r"(?<![\w/])(?:[\w./-]+\.\w+)?:\d+(?:[-,:]\s?:?\d+)*")
_CODE = re.compile(r"`[^`\n]+`")
#: A sentence ends at `. ` (evidence sentences often open on a path, `app.py:26 …`). `app.py:26`
#: itself has no space, so it stays whole; `e.g.` / `i.e.` and a `;` inside a reference list are not ends.
_SENTENCE = re.compile(r"(?<!\be\.g\.)(?<!\bi\.e\.)(?<=\.)\s+(?=[\w`(<])")


def _highlight(line: str, *, use_colour: bool) -> str:
    """Code spans in yellow, `file:line` references in sky blue: the two things a reader hunts for."""
    if not use_colour:
        return line
    line = _CODE.sub(lambda m: _col(YELLOW, m.group(), use_colour=True), line)
    return _REF.sub(lambda m: _col(SKY_BLUE, m.group(), use_colour=True), line)


def _paragraph(text: str, *, width: int, use_colour: bool, bullets: bool = False) -> list[str]:
    """Wrapped with a hanging indent; `bullets` puts each sentence on its own `•` line (evidence is a
    list of facts, not prose)."""
    parts = [p.strip() for p in _SENTENCE.split(" ".join(text.split()))] if bullets else [text]
    out = []
    for part in filter(None, parts):
        # wrap and highlight plain text, then colour the bullet: highlighting after an escape code
        # is in the line would read its `0m` as the start of a file name
        first, indent = ("    • ", "      ") if bullets else ("    ", "    ")
        wrapped = [_highlight(ln, use_colour=use_colour) for ln in
                   _wrap_desc(part, width=width, indent=indent, first_prefix=first).splitlines()]
        if bullets and wrapped:
            wrapped[0] = "    " + _col(_DIM, "•", use_colour=use_colour) + wrapped[0][5:]
        out += wrapped
    return out


_FINDINGS: dict[tuple, list[dict]] = {}
_CARD_FINDINGS = 8   # a card shows the first few; the brief carries every one


def _why(p: dict) -> str:
    """The proposal's summary without the rule it starts with: "Agents broke it in 4 sessions (5 findings)"."""
    why = p["summary"].removeprefix(p["rule"] + ": ")
    return why[:1].upper() + why[1:]


def describe_rule(record: dict, p: dict, findings: _t.Sequence[dict], *, width: int = _MAX_WIDTH,
                  use_colour: bool = True) -> list[str]:
    """The card for one rule proposal: the section, what to do about it, and the findings behind it."""
    file, _, heading = p["rule"].partition(" → ")
    out = [f"  {_col(ORANGE, p['kind'], use_colour=use_colour)}  {_col(_BOLD, file + ' → ' + heading, use_colour=use_colour)}",
           "", "  " + _col(SKY_BLUE, "Proposal", use_colour=use_colour)]
    out += _paragraph(f"{_why(p)}. {_RULE_HOW[p['kind']]}", width=width, use_colour=use_colour)
    out += ["", "  " + _col(GREY, f"Findings ({len(p['sources'])})", use_colour=use_colour)]
    for f in findings[:_CARD_FINDINGS]:
        wrapped = [_highlight(ln, use_colour=use_colour) for ln in _wrap_desc(
            f"{f.get('file') or f.get('slug')}: {' '.join(str(f.get('desc', '')).split())[:300]}",
            width=width, indent="      ", first_prefix="    • ").splitlines()]
        if wrapped:
            wrapped[0] = "    " + _col(_DIM, "•", use_colour=use_colour) + wrapped[0][5:]
        out += wrapped
    if len(findings) > _CARD_FINDINGS:
        out.append("    " + _col(_DIM, f"… {len(findings) - _CARD_FINDINGS} more; the session gets them all",
                                 use_colour=use_colour))
    return out


def describe(record: dict, item: dict, *, width: int = _MAX_WIDTH, use_colour: bool = True,
             evidence: bool = True) -> list[str]:
    """The card for one bug: where it is, then the agent's question (a decide item) or the bug and what
    verify found (a work item), the answer or recommendation, and the evidence. A rule item's card is
    `describe_rule`'s; a bundle's is every bug's card without its evidence (the session gets it)."""
    if item["kind"] == "bundle":
        out = ["  " + _col(_BOLD, f"{len(item['refs'])} bugs on one branch or file", use_colour=use_colour)
               + _col(_DIM, ": one session fixes them together; the brief carries each one's evidence",
                      use_colour=use_colour)]
        for ref in item["refs"]:
            out += ["", *describe(record, {"kind": "work", "ref": ref}, width=width, use_colour=use_colour,
                                  evidence=False)]
        return out
    if item["kind"] == "rule":
        p = next(p for p in record.get("proposals", []) if p["id"] == item["ref"])
        key = tuple(p["sources"])
        if key not in _FINDINGS:   # a repaint (scroll, resize) doesn't re-read the log
            _FINDINGS[key] = source_findings(p["sources"])
        return describe_rule(record, p, _FINDINGS[key], width=width, use_colour=use_colour)
    b = next(b for b in record.get("bugs", []) if b["id"] == item["ref"])
    v = b.get("verify") or {}
    where = _record.bug_location(b)
    verdict = v.get("verdict") or b.get("status", "?")
    head = (f"  {_col(_VERDICT_COLOUR.get(verdict, YELLOW), verdict, use_colour=use_colour)}  "
            f"{_col(_BOLD, where, use_colour=use_colour)}  "
            + _col(_DIM, f"{b.get('branch') or ('run ' + str(b.get('run') or '?') if b.get('review') else str(b.get('runs') or 1) + ' agent run(s)' if b.get('kind') == 'agent_run' else '?')}"
                         f" · {b['key']}", use_colour=use_colour))
    out = [head]
    note = _issues.issue_note(record, b, repo_slug())
    if note:
        out.append("  " + _col(_DIM, note, use_colour=use_colour))
    decide = item["kind"] == "decide"
    auto = read_auto().get(b["key"]) or {}
    texts = {"Auto-fix": f"{auto['outcome'].replace('_', ' ')}: {auto.get('reason') or '—'}" if auto else None,
             "Question": (v.get("question") or b["desc"]) if decide else None,
             "Bug": None if decide else b["desc"], "Verified": None if decide else v.get("effect"),
             "Your answer": b.get("owner_answer"),
             "Recommendation": None if b.get("owner_answer") else v.get("recommendation"),
             "Live once": v.get("trigger"), "Evidence": v.get("evidence") if evidence else None}
    for label, colour in _SECTIONS:
        if not texts[label]:
            continue
        out += ["", "  " + _col(colour, label, use_colour=use_colour)]
        out += _paragraph(texts[label], width=width, use_colour=use_colour, bullets=label == "Evidence")
    return out


def _prompt(keys: dict[str, str], *, use_colour: bool) -> str:
    answers = "  ".join(f"{_col(_LABELS[k][1], f'[{k}]', use_colour=use_colour)} {_LABELS[k][0]}"
                        for k in keys)
    control = _col(_DIM, "  ".join(f"[{k}] {v}" for k, v in _CONTROL.items()), use_colour=use_colour)
    return f"  {answers}   {control}  {_col(_BOLD, '›', use_colour=use_colour)} "


#: Full screen: the terminal's alternate screen, as a pager does, so quitting puts the shell back.
_ENTER, _LEAVE, _CLEAR = ENTER_ALT_SCREEN, LEAVE_ALT_SCREEN, "\033[2J\033[H"


def read_key(prompt: str) -> str:
    """One keypress from the terminal, no Enter: the answer keys act at once (`z` undoes a slip).
    PgUp/PgDn and ↑/↓ (the wheel) come back by name and a terminal resize as `RESIZE`, so the card repaints;
    any other escape sequence (an arrow) reads as nothing."""
    import signal
    import termios
    import tty
    sys.stdout.write(prompt)
    sys.stdout.flush()
    fd = sys.stdin.fileno()
    saved = termios.tcgetattr(fd)
    resized = [False]
    try:
        prev_winch = signal.signal(signal.SIGWINCH, lambda *_: resized.__setitem__(0, True))
    except ValueError:   # not the main thread: no resize repaint
        prev_winch = None
    try:
        tty.setcbreak(fd)   # Ctrl-C still works; typeahead is dropped, so a double press skips no card
        key = None
        while key is None and not resized[0]:
            key = poll_key(fd, 0.1)
    finally:
        termios.tcsetattr(fd, termios.TCSADRAIN, saved)
        if prev_winch is not None:
            signal.signal(signal.SIGWINCH, prev_winch)
    if key is None:
        return RESIZE
    if key in (LEFT, RIGHT):   # the recital console's expand keys; nothing here
        key = ""
    if not scroll_step(key, 1):
        sys.stdout.write(key.strip() + "\n")
    return key


def run_queue(record: dict, *, read: _t.Callable[[str], str] = input,
              press: _t.Callable[[str], str] | None = None, out: _t.TextIO = sys.stdout,
              path: pathlib.Path | None = None, use_colour: bool = True, width: int | None = None,
              fullscreen: bool = False, launch: _t.Callable[[str, pathlib.Path], str | None] = default_launch,
              cwd: pathlib.Path | None = None, older: _t.Sequence[dict] = ()) -> int:
    """Walk the queue; returns how many answers were recorded this session (net of undos). `older`:
    the records before this one, whose late answers count as answers here (`apply_reviews`).

    The queue is re-read after every answer, so a decide bug kept open joins the work list at once,
    and a bundle (`bundles`) once its file or branch has another open bug. A bundle's `fix now` records
    a fix session on each of its bugs, and one undo takes them all back.
    `fullscreen` (a terminal) repaints one card per screen, like `pixi run recital-console`; off, the
    cards scroll, which is what a pipe or a test reads. `launch` starts a fix session (`default_launch`)
    in `cwd`, by default the `workspace()` the owner starts sessions in. `press` reads the answer key
    (`read_key` on a terminal, no Enter); `read` reads a line: the typed answer, and the key when
    `press` is None. Full screen, PgUp/PgDn and ↑/↓ (the wheel) scroll a card taller than the terminal, and every paint
    re-measures it, so a resize (`RESIZE` from `press`) re-wraps the card.
    """
    press = press or read
    fixed_width = width

    def size() -> tuple[int, int]:
        cols, rows = _terminal_size((_MAX_WIDTH, 40))
        return fixed_width or min(cols, _MAX_WIDTH), rows
    base = record

    def state() -> tuple[dict, list[dict], int]:
        rec = apply_reviews(base, read_reviews(path), older)
        left = [it for it in pending(rec, read_reviews(path), older) if (it["kind"], it["ref"]) not in skipped]
        return rec, bundles(rec, left, apart), len(items_of(rec))

    skipped: set[tuple[str, str]] = set()
    apart: set[str] = set()   # bundled bugs the owner asked to see one by one
    rec, items, total = state()
    counts = {k: sum(it["kind"] == k for it in items) for k in ANSWERS}
    work = counts["work"] + sum(len(it["refs"]) for it in items if it["kind"] == "bundle")
    sessions = f" in {counts['work'] + counts['bundle']} session(s)" if counts["bundle"] else ""
    title = (_col(_BOLD, "Weekly judge review", use_colour=use_colour) + "  "
             + _col(_DIM, f"{record['date']} · {counts['decide']} to decide · {work} to work{sessions}"
                    + (f" · {counts['rule']} rule proposal(s)" if counts["rule"] else ""), use_colour=use_colour))
    if not items:
        print(title, file=out)
        print(_col(BLUISH_GREEN, "  nothing to review: every open bug and rule proposal is answered", use_colour=use_colour), file=out)
        return 0
    done: list[tuple[list[dict], str, str]] = []   # (event rows, key, card ref) per answer this session, for undo
    status = ""   # the last answer, shown under the title of the next card
    if fullscreen:
        out.write(_ENTER)

    def show(card: _t.Callable[[int], list[str]], offset: int) -> tuple[int, int]:
        """Paint `card(width)` from `offset` rows in; returns the clamped offset and the page step."""
        w, rows = size()
        if fullscreen:
            # title, status, card, then a blank and the prompt row at the bottom
            lines, offset, page = scroll_window(card(w), rows - 5, offset, use_colour=use_colour)
            out.write(_CLEAR + "\n".join([title, status, *lines, ""]) + "\n")
            out.flush()
            return offset, page
        for ln in [*([status] if status else []), *card(w), ""]:
            print(ln, file=out)
        return 0, 1

    if not fullscreen:
        print(title, file=out)
    try:
        while items:
            item = items[0]
            keys = ANSWERS[item["kind"]]
            kind = item["kind"]

            refs = item.get("refs") or [item["ref"]]
            label_ref = " + ".join(refs)

            def card(w: int) -> list[str]:
                return [_hr(f"{label_ref} · {kind} · {len(items)} left", w, use_colour=use_colour),
                        *describe(rec, item, width=w, use_colour=use_colour)]
            offset = 0
            while True:   # scroll and resize repaint this card; any other key answers it
                offset, page = show(card, offset)
                try:
                    pressed = press(_prompt(keys, use_colour=use_colour))
                except EOFError:
                    pressed = "q"
                if scroll_step(pressed, page):
                    offset += scroll_step(pressed, page)
                elif pressed != RESIZE:
                    break
            status = ""
            key = pressed.strip().lower()[:1]
            if key == "q":
                break
            if key == "n":
                skipped.update(("work" if kind == "bundle" else kind, r) for r in refs)
            elif key == "z":
                if not done:
                    status = _col(YELLOW, "  nothing to undo in this session", use_colour=use_colour)
                else:
                    rows, prev_key, prev_ref = done.pop()
                    for prev in rows:
                        append_review(prev["event"], prev["record"], prev["ref"], None, corrects=prev["id"], path=path)
                    status = _col(REDDISH_PURPLE, f"  ↺ undid {prev_ref} = {_LABELS[prev_key][0]}",
                                  use_colour=use_colour)
            elif key == "s" and kind == "bundle":
                apart.update(refs)
                status = _col(GREY, f"  {label_ref} → one by one", use_colour=use_colour)
            elif key not in keys:
                status = _col(YELLOW, f"  press one of: {', '.join([*keys, *_CONTROL])}", use_colour=use_colour)
            else:
                event, value = keys[key]
                note = None
                if key == "a":
                    note = read("  " + _col(SKY_BLUE, "your answer", use_colour=use_colour)
                                + _col(_DIM, " (a session follows it instead of the recommendation)",
                                       use_colour=use_colour)
                                + f" {_col(_BOLD, '›', use_colour=use_colour)} ").strip()
                if key == "a" and not note:
                    status = _col(YELLOW, "  empty answer: nothing recorded", use_colour=use_colour)
                elif key in ("f", "e"):
                    if key == "f":
                        bugs = [next(b for b in rec["bugs"] if b["id"] == r) for r in refs]
                        brief = bundle_brief(rec, bugs) if kind == "bundle" else fix_brief(rec, bugs[0])
                    else:
                        p = next(p for p in rec["proposals"] if p["id"] == item["ref"])
                        brief = rule_brief(rec, p, source_findings(p["sources"]))
                    if fullscreen:   # the session gets the real screen; the queue comes back after
                        out.write(_LEAVE)
                        out.flush()
                    failed = launch(brief, cwd or workspace())
                    if fullscreen:
                        out.write(_ENTER)
                    if failed:
                        status = _col(YELLOW, f"  {failed}", use_colour=use_colour)
                    else:
                        done.append(([append_review(event, record["date"], r, value, path=path) for r in refs],
                                     key, label_ref))
                        status = _col(BLUE, f"  ✓ {label_ref} → {'fix' if key == 'f' else 'rule'} session ended",
                                      use_colour=use_colour)
                else:
                    done.append(([append_review(event, record["date"], item["ref"], value, note=note, path=path)],
                                 key, item["ref"]))
                    label, colour = _LABELS[key]
                    status = _col(colour, f"  ✓ {item['ref']} → {label}", use_colour=use_colour)
            rec, items, total = state()
    finally:
        if fullscreen:
            out.write(_LEAVE)
            out.flush()
    left_n = len(pending(apply_reviews(base, read_reviews(path), older), read_reviews(path), older))
    answered = sum(len(rows) for rows, _, _ in done)   # per bug: a bundle's fix now answers each of its bugs
    if fullscreen:   # the alternate screen is gone; leave the outcome in the shell
        print(title, file=out)
    elif status:
        print(status, file=out)
    print(_hr(f"{answered} answered this session · " + (f"{left_n} left" if left_n else "nothing left"), size()[0],
              use_colour=use_colour), file=out)
    print(_col(_DIM, "  the next weekly pass applies these", use_colour=use_colour), file=out)
    return answered


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--date", help="record to review (default: the newest pass record)")
    ap.add_argument("--auto", action="store_true", help="work the `fix` bugs unattended, one PR per session")
    ap.add_argument("--limit", type=int, default=1, help="--auto: sessions to run (default 1)")
    ap.add_argument("--dry-run", action="store_true", help="--auto: print the briefs, run nothing")
    args = ap.parse_args(argv)
    try:
        if args.date:
            older = _record.pass_records(before=args.date)
            record = apply_reviews(_record.load(_record.store_root() / f"{args.date}.json"), read_reviews(), older)
        else:
            records = applied_pass_records()
            if not records:
                print(f"judge-review: no run records in {_record.store_root()}", file=sys.stderr)
                return 1
            record, older = records[-1], records[:-1]
    except _record.RecordError as e:
        print(f"judge-review: {e}", file=sys.stderr)
        return 1
    if record.get("kind") == "failure":
        print(f"judge-review: {record['date']} is a failure record; nothing to review", file=sys.stderr)
        return 1
    if args.auto:
        newest = (_record.pass_records() or [{}])[-1].get("date")
        if record["date"] != newest:
            # a newer record carries these bugs with the answers given since: an older one would redo them
            print(f"judge-review --auto: {record['date']} isn't the newest record ({newest}); --auto works "
                  "only the newest", file=sys.stderr)
            return 1
        auto_fix = _load_sibling("auto_fix")
        try:
            auto_fix.run(record, limit=args.limit, dry_run=args.dry_run, older=older)
        except RateLimited as e:
            return limit_exit("judge-review --auto", e)
        return 0
    tty = sys.stdout.isatty()
    run_queue(record, press=read_key if tty and sys.stdin.isatty() else None, use_colour=tty, fullscreen=tty,
              older=older)
    return 0


if __name__ == "__main__":
    sys.exit(main())
