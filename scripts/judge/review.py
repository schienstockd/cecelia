#!/usr/bin/env python3
"""Weekly judge — the owner's review queue, in the terminal.

Walks the bugs a verify agent sent to the owner (`decide`), one at a time, one key per answer:
keep it open (a session follows the agent's recommendation), answer it in your own words (a session
follows yours instead), or `wont_fix` to stop carrying it. Every answer is an append-only event in
`~/.cecelia-effectiveness/review.jsonl`; an undo is a correcting event, never an edit. Nothing here
touches the repo or the record: the weekly pass applies the events at the start of the next pass.

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
import sys
import typing as _t
import uuid

_REPO = pathlib.Path(__file__).resolve().parents[2]

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import read_events  # noqa: E402
# one look with `pixi run recital-console`: its divider, hanging-indent wrap and the shared palette
from cecelia.effectiveness.console import _BOLD, _DIM, _col, _hr, _terminal_size, _wrap_desc  # noqa: E402
from cecelia.effectiveness.palette import (  # noqa: E402
    BLUISH_GREEN, GREY, ORANGE, REDDISH_PURPLE, SKY_BLUE, VERMILLION, YELLOW)


def _load_sibling(name: str):
    spec = _importlib_util.spec_from_file_location(f"_judge_{name}", pathlib.Path(__file__).parent / f"{name}.py")
    mod = _importlib_util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_record = _load_sibling("record")

REVIEW_SCHEMA_VERSION = 1

#: queue item kind → (event name, {key: value}). The key is what the owner presses.
ANSWERS: dict[str, tuple[str, dict[str, str]]] = {
    "bug": ("bug_status", {"w": "wont_fix", "o": "open", "a": "open"}),
}
REVIEW_EVENTS = tuple(e for e, _ in ANSWERS.values())
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
            if row.get("note"):
                b["owner_answer"] = row["note"]
    return record


def pending(record: dict, reviews: _t.Iterable[dict]) -> list[dict]:
    """The record's queue items with no live answer yet, in queue order."""
    now = current(reviews)
    out = []
    for item in record.get("queue", []):
        event = ANSWERS[item["kind"]][0]
        row = now.get((record["date"], event, item["ref"]))
        if row is None or row.get("value") is None:
            out.append(item)
    return out


# ── the terminal queue ─────────────────────────────────────────────────────────────────────────

#: How each answer reads on the prompt and after it, and its colour.
_LABELS = {"w": ("won't fix", VERMILLION), "o": ("keep open", BLUISH_GREEN), "a": ("answer", SKY_BLUE)}
_VERDICT_COLOUR = {"decide": ORANGE, "fix": VERMILLION, "guard": YELLOW, "dismiss": GREY}
#: Section label → colour, in the order they are shown.
_SECTIONS = (("Question", SKY_BLUE), ("Recommendation", BLUISH_GREEN), ("Evidence", GREY))
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
    """The card for one bug: where it is, then the agent's question, recommendation and evidence."""
    b = next(b for b in record.get("bugs", []) if b["id"] == item["ref"])
    v = b.get("verify") or {}
    where = f"PR #{b.get('pr')}" if b.get("kind") == "stranded" else f"{b['file']}:{b['line']}"
    verdict = v.get("verdict") or b.get("status", "?")
    head = (f"  {_col(_VERDICT_COLOUR.get(verdict, YELLOW), verdict, use_colour=use_colour)}  "
            f"{_col(_BOLD, where, use_colour=use_colour)}  "
            + _col(_DIM, f"{b.get('branch') or '?'} · {b['key']}", use_colour=use_colour))
    out = [head]
    texts = {"Question": v.get("question") or b["desc"], "Recommendation": v.get("recommendation"),
             "Evidence": v.get("evidence")}
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
_ENTER, _LEAVE, _CLEAR = "\033[?1049h", "\033[?1049l", "\033[2J\033[H"


def _fit(lines: list[str], height: int, *, use_colour: bool) -> list[str]:
    """`lines` cut to `height` rows, the last kept row saying how much was left out."""
    if len(lines) <= height:
        return lines
    return lines[:height - 1] + [_col(_DIM, f"    … {len(lines) - height + 1} more line(s); "
                                            "widen or heighten the terminal to see them", use_colour=use_colour)]


def run_queue(record: dict, *, read: _t.Callable[[str], str] = input, out: _t.TextIO = sys.stdout,
              path: pathlib.Path | None = None, use_colour: bool = True, width: int | None = None,
              fullscreen: bool = False) -> int:
    """Walk the pending items; returns how many answers were recorded (net of undos).

    `fullscreen` (a terminal) repaints one card per screen, like `pixi run recital-console`; off, the
    cards scroll, which is what a pipe or a test reads.
    """
    cols, rows = _terminal_size((_MAX_WIDTH, 40))
    width = width or min(cols, _MAX_WIDTH)
    reviews = read_reviews(path)
    items = pending(record, reviews)
    total = len(record.get("queue", []))
    title = (_col(_BOLD, "Weekly judge review", use_colour=use_colour) + "  "
             + _col(_DIM, f"{record['date']} · {total} bug(s) for you to decide", use_colour=use_colour))
    if not items:
        print(title, file=out)
        print(_col(BLUISH_GREEN, f"  nothing to review: all {total} answered", use_colour=use_colour), file=out)
        return 0
    done: list[tuple[dict, dict, str]] = []   # (item, event, key) answered this session, for undo
    status = ""   # the last answer, shown under the title of the next card
    if fullscreen:
        out.write(_ENTER)

    def show(lines: list[str]) -> None:
        if fullscreen:
            # title, status, card, then a blank and the prompt row at the bottom
            out.write(_CLEAR + "\n".join([title, status, *_fit(lines, rows - 5, use_colour=use_colour), ""]) + "\n")
            out.flush()
        else:
            for ln in [*([status] if status else []), *lines, ""]:
                print(ln, file=out)

    if not fullscreen:
        print(title, file=out)
    try:
        i = 0
        while i < len(items):
            item = items[i]
            event, keys = ANSWERS[item["kind"]]
            answered = total - len(items) + len(done)
            show([_hr(f"{item['ref']} · {answered + 1} of {total}", width, use_colour=use_colour),
                  *describe(record, item, width=width, use_colour=use_colour)])
            status = ""
            try:
                key = read(_prompt(keys, use_colour=use_colour)).strip().lower()[:1]
            except EOFError:
                key = "q"
            if key == "q":
                break
            if key == "n":
                i += 1
                continue
            if key == "z":
                if not done:
                    status = _col(YELLOW, "  nothing to undo in this session", use_colour=use_colour)
                    continue
                prev_item, prev, prev_key = done.pop()
                append_review(prev["event"], prev["record"], prev["ref"], None, corrects=prev["id"], path=path)
                status = _col(REDDISH_PURPLE, f"  ↺ undid {prev['ref']} = {_LABELS[prev_key][0]}",
                              use_colour=use_colour)
                i = items.index(prev_item)   # ask that one again
                continue
            if key not in keys:
                status = _col(YELLOW, f"  press one of: {', '.join([*keys, *_CONTROL])}", use_colour=use_colour)
                continue
            note = None
            if key == "a":
                note = read("  " + _col(SKY_BLUE, "your answer", use_colour=use_colour)
                            + _col(_DIM, " (a session follows it instead of the recommendation)", use_colour=use_colour)
                            + f" {_col(_BOLD, '›', use_colour=use_colour)} ").strip()
                if not note:
                    status = _col(YELLOW, "  empty answer: nothing recorded", use_colour=use_colour)
                    continue
            row = append_review(event, record["date"], item["ref"], keys[key], note=note, path=path)
            done.append((item, row, key))
            label, colour = _LABELS[key]
            status = _col(colour, f"  ✓ {item['ref']} → {label}", use_colour=use_colour)
            i += 1
    finally:
        if fullscreen:
            out.write(_LEAVE)
            out.flush()
    left = len(pending(record, read_reviews(path)))
    if fullscreen:   # the alternate screen is gone; leave the outcome in the shell
        print(title, file=out)
    elif status:
        print(status, file=out)
    print(_hr(f"{total - left} of {total} answered" + (f" · {left} left" if left else ""), width,
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
    run_queue(record, use_colour=tty, fullscreen=tty)
    return 0


if __name__ == "__main__":
    sys.exit(main())
