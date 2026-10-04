#!/usr/bin/env python3
"""Weekly judge — the owner's review queue, in the terminal.

Walks the bugs a verify agent sent to the owner (`decide`), one at a time, one key per answer:
leave it open, or `wont_fix` to stop carrying it. Every answer is an append-only event in
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
import sys
import typing as _t
import uuid

_REPO = pathlib.Path(__file__).resolve().parents[2]

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import read_events  # noqa: E402
from cecelia.effectiveness.palette import BLUISH_GREEN, GREY, ORANGE, SKY_BLUE, YELLOW  # noqa: E402


def _load_sibling(name: str):
    spec = _importlib_util.spec_from_file_location(f"_judge_{name}", pathlib.Path(__file__).parent / f"{name}.py")
    mod = _importlib_util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_record = _load_sibling("record")

REVIEW_SCHEMA_VERSION = 1

#: queue item kind → (event name, {key: value}). The key is what the owner presses.
ANSWERS: dict[str, tuple[str, dict[str, str]]] = {
    "bug": ("bug_status", {"w": "wont_fix", "o": "open"}),
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

def _col(code: str, text: str, use_colour: bool) -> str:
    return f"{code}{text}\033[0m" if use_colour else text


def describe(record: dict, item: dict) -> list[str]:
    """What the owner needs to answer this one bug, drawn from the record."""
    b = next(b for b in record.get("bugs", []) if b["id"] == item["ref"])
    where = f"PR #{b.get('pr')}" if b.get("kind") == "stranded" else f"{b['file']}:{b['line']}"
    v = b.get("verify") or {}
    return [f"{b['id']} · {b['key']} · {where} · branch {b.get('branch') or '?'}",
            f"Question: {v.get('question') or b['desc']}",
            f"Recommendation: {v.get('recommendation') or '—'}", f"Evidence: {v.get('evidence', '')}",
            "Leave it open (a session acts on the recommendation), or answer wont_fix to stop carrying it."]


def run_queue(record: dict, *, read: _t.Callable[[str], str] = input, out: _t.TextIO = sys.stdout,
              path: pathlib.Path | None = None, use_colour: bool = True) -> int:
    """Walk the pending items; returns how many answers were recorded (net of undos)."""
    reviews = read_reviews(path)
    items = pending(record, reviews)
    total = len(record.get("queue", []))
    if not items:
        print(_col(BLUISH_GREEN, f"Nothing to review for {record['date']}: all {total} item(s) answered.",
                   use_colour), file=out)
        return 0
    done: list[tuple[dict, dict]] = []   # (item, event) answered this session, for undo
    i = 0
    while i < len(items):
        item = items[i]
        event, keys = ANSWERS[item["kind"]]
        answered = total - len(items) + len(done)
        print("", file=out)
        print(_col(SKY_BLUE, f"[{answered + 1}/{total}] {item['kind'].replace('_', ' ')} {item['ref']}",
                   use_colour), file=out)
        for ln in describe(record, item):
            print(f"  {ln}", file=out)
        opts = "  ".join(f"[{k}] {v}" for k, v in {**keys, **_CONTROL}.items())
        try:
            key = read(_col(GREY, opts + " > ", use_colour)).strip().lower()[:1]
        except EOFError:
            key = "q"
        if key == "q":
            break
        if key == "n":
            i += 1
            continue
        if key == "z":
            if not done:
                print(_col(YELLOW, "  nothing to undo in this session", use_colour), file=out)
                continue
            prev_item, prev = done.pop()
            append_review(prev["event"], prev["record"], prev["ref"], None, corrects=prev["id"], path=path)
            print(_col(ORANGE, f"  undid {prev['event']} {prev['ref']} = {prev['value']}", use_colour), file=out)
            i = items.index(prev_item)   # ask that one again
            continue
        if key not in keys:
            print(_col(YELLOW, f"  press one of: {', '.join([*keys, *_CONTROL])}", use_colour), file=out)
            continue
        row = append_review(event, record["date"], item["ref"], keys[key], path=path)
        done.append((item, row))
        print(_col(BLUISH_GREEN, f"  {item['ref']} = {keys[key]}", use_colour), file=out)
        i += 1
    left = len(pending(record, read_reviews(path)))
    print(_col(GREY, f"\n{total - left}/{total} answered for {record['date']}"
                     + (f"; {left} left" if left else ""), use_colour), file=out)
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
    run_queue(record, use_colour=sys.stdout.isatty())
    return 0


if __name__ == "__main__":
    sys.exit(main())
