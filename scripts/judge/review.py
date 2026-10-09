#!/usr/bin/env python3
"""Weekly judge — the owner's queue, in the terminal: decide the bugs, then work them.

First the bugs a verify agent sent to the owner (`decide`): keep it open (a session follows the
agent's recommendation), answer it in your own words (a session follows yours instead), or
`wont_fix` to stop carrying it. Then the rest of the open work list: `fix now` starts an
interactive Claude Code session where the owner starts sessions (the folder holding the checkouts),
briefed on that bug and told to make its own worktree; the queue resumes when it exits. Every answer is an append-only event in `~/.cecelia-effectiveness/review.jsonl`; an undo is a
correcting event, never an edit. The weekly pass applies the events at the start of the next pass.

Usage:
    pixi run judge-review                # the newest record
    pixi run judge-review --date 2026-10-06
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
from cecelia.effectiveness.claude_cli import resolve_claude_bin  # noqa: E402
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

REVIEW_SCHEMA_VERSION = 1

#: queue item kind → {key the owner presses: (event, value)}. `decide` items are the record's queue;
#: `work` items are the rest of its open bugs.
ANSWERS: dict[str, dict[str, tuple[str, str]]] = {
    "decide": {"w": ("bug_status", "wont_fix"), "o": ("bug_status", "open"), "a": ("bug_status", "open")},
    "work": {"f": ("bug_work", "fix_session"), "w": ("bug_status", "wont_fix")},
}
REVIEW_EVENTS = ("bug_status", "bug_work")
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
           "ts": _dt.datetime.now(_dt.timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z"),
           "event": event, "record": record_date, "ref": ref, "value": value}
    if note:
        row["note"] = note
    if corrects:
        row["corrects"] = corrects
    path = path or review_path()
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as fh:
        fh.write(json.dumps(row, ensure_ascii=False) + "\n")
        fh.flush()
        os.fsync(fh.fileno())
    return row


def read_reviews(path: pathlib.Path | None = None) -> list[dict]:
    """Every owner event, oldest first. Torn lines are skipped by `read_events`; rows of another
    schema or event are skipped here."""
    return [row for row in read_events(path or review_path())
            if row.get("schema_version") == REVIEW_SCHEMA_VERSION and row.get("event") in REVIEW_EVENTS]


def applied_pass_records(before: str | None = None) -> list[dict]:
    """`record.pass_records` with the owner's answers folded in: the view every reader of the
    history wants."""
    reviews = read_reviews()
    return [apply_reviews(r, reviews) for r in _record.pass_records(before=before)]


def current(reviews: _t.Iterable[dict]) -> dict[tuple[str, str, str], dict]:
    """The latest event per (record, event, ref): later events, undos included, win."""
    out: dict[tuple[str, str, str], dict] = {}
    for r in reviews:
        out[(r["record"], r["event"], r["ref"])] = r
    return out


def apply_reviews(record: dict, reviews: _t.Iterable[dict]) -> dict:
    """`record` with the owner's bug answers folded in. What the weekly pass does to the previous
    record before it carries its bugs."""
    now = current(reviews)
    record = json.loads(json.dumps(record))   # deep copy; the caller's record is left alone
    for b in record.get("bugs", []):
        row = now.get((record["date"], "bug_status", b["id"]))
        if row and row.get("value") in _record.BUG_STATUSES:
            b["status"] = row["value"]
            b["owner_decision"] = row["value"]   # a `decide` bug kept open joins the work list
            if row.get("note"):
                b["owner_answer"] = row["note"]
    return record


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
    """The whole queue: the record's decide items, then the work list."""
    return [{"kind": "decide", "ref": item["ref"]} for item in record.get("queue", [])] + work_items(record)


def pending(record: dict, reviews: _t.Iterable[dict]) -> list[dict]:
    """The queue's items with no live answer yet, in queue order."""
    now = current(reviews)

    def live(event: str, ref: str) -> str | None:
        row = now.get((record["date"], event, ref))
        return row.get("value") if row else None

    def answered(item: dict) -> bool:
        if item["kind"] == "decide":
            return live("bug_status", item["ref"]) is not None
        # a work item is done once a fix session was started or it was answered won't-fix; a decide
        # answer of `open` is what put it on the work list, so it doesn't count here
        return live("bug_work", item["ref"]) is not None or live("bug_status", item["ref"]) == "wont_fix"
    return [item for item in items_of(record) if not answered(item)]


def _origin(bug: dict) -> str:
    """Where a bug came from: the reviewed branch, or the agent runs that hit it."""
    if bug.get("review"):
        return (f"marked bad (cause {bug.get('cause')}) by the reviewer of agent run {bug.get('run') or '?'}, "
                f"Blackboard entry `{bug.get('entry')}` in project `{bug.get('project')}`")
    if bug.get("kind") == "agent_run":
        return f"hit in {bug.get('runs') or 1} agent run(s), first at commit `{(bug.get('commit') or '?')[:8]}`"
    return f"raised on branch `{bug.get('branch') or '?'}`"


def fix_brief(record: dict, bug: dict) -> str:
    """The opening prompt of a fix session: the bug, what verify found, and how to work it."""
    v = bug.get("verify") or {}
    main = main_checkout()
    lines = [f"Work bug {bug['id']} (`{bug['key']}`) from the weekly judge record of {record['date']} "
             f"(`{_record.store_root() / (record['date'] + '.json')}`).", "",
             f"Where: {_record._bug_where(bug)}, {_origin(bug)}.", f"Finding: {bug['desc']}"]
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
    if bug.get("kind") == "stranded":
        lines += ["", "These commits were pushed to the PR's branch after it merged: "
                  + ", ".join(bug.get("commits", [])) + ". Land them in a new PR if they are still wanted."]
    lines += ["", "Confirm it on origin/main first. If it isn't real, say so and stop; I'll answer it won't-fix. "
              f"Otherwise work in your own worktree: from `{main}`, run "
              f"`pixi run bootstrap-worktree fix-{bug['key']}` and work in the worktree it creates, never in "
              f"`{main}` itself. Fix it there, including every sibling call site with the same shape, add a test "
              "that fails without the fix, run the matching tests and recital, and name "
              f"`{bug['key']}` in the commit message. Ask before committing, as usual."]
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
_LABELS = {"w": ("won't fix", VERMILLION), "o": ("keep open", BLUISH_GREEN), "a": ("answer", SKY_BLUE),
           "f": ("fix now", BLUE)}
_VERDICT_COLOUR = {"decide": ORANGE, "fix": VERMILLION, "guard": YELLOW, "dismiss": GREY}
#: Section label → colour, in the order they are shown; a card shows the ones it has text for.
_SECTIONS = (("Question", SKY_BLUE), ("Bug", SKY_BLUE), ("Verified", ORANGE), ("Your answer", REDDISH_PURPLE),
             ("Recommendation", BLUISH_GREEN), ("Live once", YELLOW), ("Evidence", GREY))
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


def describe(record: dict, item: dict, *, width: int = _MAX_WIDTH, use_colour: bool = True) -> list[str]:
    """The card for one bug: where it is, then the agent's question (a decide item) or the bug and what
    verify found (a work item), the answer or recommendation, and the evidence."""
    b = next(b for b in record.get("bugs", []) if b["id"] == item["ref"])
    v = b.get("verify") or {}
    where = _record.bug_location(b)
    verdict = v.get("verdict") or b.get("status", "?")
    head = (f"  {_col(_VERDICT_COLOUR.get(verdict, YELLOW), verdict, use_colour=use_colour)}  "
            f"{_col(_BOLD, where, use_colour=use_colour)}  "
            + _col(_DIM, f"{b.get('branch') or ('run ' + str(b.get('run') or '?') if b.get('review') else str(b.get('runs') or 1) + ' agent run(s)' if b.get('kind') == 'agent_run' else '?')}"
                         f" · {b['key']}", use_colour=use_colour))
    out = [head]
    decide = item["kind"] == "decide"
    texts = {"Question": (v.get("question") or b["desc"]) if decide else None,
             "Bug": None if decide else b["desc"], "Verified": None if decide else v.get("effect"),
             "Your answer": b.get("owner_answer"),
             "Recommendation": None if b.get("owner_answer") else v.get("recommendation"),
             "Live once": v.get("trigger"), "Evidence": v.get("evidence")}
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
              cwd: pathlib.Path | None = None) -> int:
    """Walk the queue; returns how many answers were recorded this session (net of undos).

    The queue is re-read after every answer, so a decide bug kept open joins the work list at once.
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
        rec = apply_reviews(base, read_reviews(path))
        left = [it for it in pending(rec, read_reviews(path)) if (it["kind"], it["ref"]) not in skipped]
        return rec, left, len(items_of(rec))

    skipped: set[tuple[str, str]] = set()
    rec, items, total = state()
    decide_n = sum(it["kind"] == "decide" for it in items)
    title = (_col(_BOLD, "Weekly judge review", use_colour=use_colour) + "  "
             + _col(_DIM, f"{record['date']} · {decide_n} to decide · {len(items) - decide_n} to work",
                    use_colour=use_colour))
    if not items:
        print(title, file=out)
        print(_col(BLUISH_GREEN, "  nothing to review: every open bug is answered", use_colour=use_colour), file=out)
        return 0
    done: list[tuple[dict, str]] = []   # (event row, key) answered this session, for undo
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
            kind = "decide" if item["kind"] == "decide" else "work"

            def card(w: int) -> list[str]:
                return [_hr(f"{item['ref']} · {kind} · {len(items)} left", w, use_colour=use_colour),
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
                skipped.add((item["kind"], item["ref"]))
            elif key == "z":
                if not done:
                    status = _col(YELLOW, "  nothing to undo in this session", use_colour=use_colour)
                else:
                    prev, prev_key = done.pop()
                    append_review(prev["event"], prev["record"], prev["ref"], None, corrects=prev["id"], path=path)
                    status = _col(REDDISH_PURPLE, f"  ↺ undid {prev['ref']} = {_LABELS[prev_key][0]}",
                                  use_colour=use_colour)
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
                elif key == "f":
                    bug = next(b for b in rec["bugs"] if b["id"] == item["ref"])
                    if fullscreen:   # the session gets the real screen; the queue comes back after
                        out.write(_LEAVE)
                        out.flush()
                    failed = launch(fix_brief(rec, bug), cwd or workspace())
                    if fullscreen:
                        out.write(_ENTER)
                    if failed:
                        status = _col(YELLOW, f"  {failed}", use_colour=use_colour)
                    else:
                        done.append((append_review(event, record["date"], item["ref"], value, path=path), key))
                        status = _col(BLUE, f"  ✓ {item['ref']} → fix session ended", use_colour=use_colour)
                else:
                    done.append((append_review(event, record["date"], item["ref"], value, note=note, path=path), key))
                    label, colour = _LABELS[key]
                    status = _col(colour, f"  ✓ {item['ref']} → {label}", use_colour=use_colour)
            rec, items, total = state()
    finally:
        if fullscreen:
            out.write(_LEAVE)
            out.flush()
    left_n = len(pending(apply_reviews(base, read_reviews(path)), read_reviews(path)))
    if fullscreen:   # the alternate screen is gone; leave the outcome in the shell
        print(title, file=out)
    elif status:
        print(status, file=out)
    print(_hr(f"{len(done)} answered this session · " + (f"{left_n} left" if left_n else "nothing left"), size()[0],
              use_colour=use_colour), file=out)
    print(_col(_DIM, "  the next weekly pass applies these", use_colour=use_colour), file=out)
    return len(done)


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--date", help="record to review (default: the newest pass record)")
    args = ap.parse_args(argv)
    try:
        if args.date:
            record = apply_reviews(_record.load(_record.store_root() / f"{args.date}.json"), read_reviews())
        else:
            records = applied_pass_records()
            if not records:
                print(f"judge-review: no run records in {_record.store_root()}", file=sys.stderr)
                return 1
            record = records[-1]
    except _record.RecordError as e:
        print(f"judge-review: {e}", file=sys.stderr)
        return 1
    if record.get("kind") == "failure":
        print(f"judge-review: {record['date']} is a failure record; nothing to review", file=sys.stderr)
        return 1
    tty = sys.stdout.isatty()
    run_queue(record, press=read_key if tty and sys.stdin.isatty() else None, use_colour=tty, fullscreen=tty)
    return 0


if __name__ == "__main__":
    sys.exit(main())
