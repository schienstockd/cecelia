#!/usr/bin/env python3
"""CLI wrapper for the effectiveness log's `append_event`.

Called by the sibling-call audit and convention-check reviewer hooks after a run — the parent
agent emits an event to jsonl so the log accumulates data the rollup script can render later.

Usage:
    python scripts/log_event.py --event sibling_audit_run \\
        --payload '{"hunks_reviewed": 3, "duration_s": 27.4}'

    # OR read the event dict from stdin:
    echo '{"event": "ratchet_hit", "payload": {"ratchet_id": "zarr-access", "outcome": "fixed_pre_commit"}}' \\
        | python scripts/log_event.py --stdin

Exits non-zero on a validation failure (unknown event, bad outcome) so the caller notices.
"""

from __future__ import annotations

import argparse
import json
import sys

from cecelia.effectiveness import UnknownEventError, UnknownOutcomeError, append_event


def _parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--event", help="event type (see EVENT_TYPES). Required unless --stdin.")
    p.add_argument("--payload", default="{}", help="event payload as JSON string (default {}).")
    p.add_argument("--session", help="Claude Code session id; defaults to $CLAUDE_SESSION_ID or 'unknown'.")
    p.add_argument("--source", default="live", help="'live' (default) or 'retrospective_<tag>'.")
    p.add_argument("--pr", help="PR reference, e.g. '#1240'.")
    p.add_argument("--commit", help="Commit SHA (short).")
    p.add_argument("--stdin", action="store_true", help="Read a full event dict from stdin as JSON.")
    return p.parse_args()


def main() -> int:
    args = _parse_args()

    if args.stdin:
        try:
            obj = json.load(sys.stdin)
        except json.JSONDecodeError as e:
            print(f"log_event: bad JSON on stdin: {e}", file=sys.stderr)
            return 2
        event = obj.get("event")
        payload = obj.get("payload") or {}
        session = obj.get("session") or args.session
        source = obj.get("source") or args.source
        pr = obj.get("pr") or args.pr
        commit = obj.get("commit") or args.commit
    else:
        if not args.event:
            print("log_event: --event required (or use --stdin)", file=sys.stderr)
            return 2
        event = args.event
        try:
            payload = json.loads(args.payload)
        except json.JSONDecodeError as e:
            print(f"log_event: bad JSON in --payload: {e}", file=sys.stderr)
            return 2
        session = args.session
        source = args.source
        pr = args.pr
        commit = args.commit

    try:
        row = append_event(
            event=event,
            payload=payload,
            session=session,
            source=source,
            pr=pr,
            commit=commit,
        )
    except (UnknownEventError, UnknownOutcomeError) as e:
        print(f"log_event: {e}", file=sys.stderr)
        return 3

    print(row["ts"])
    return 0


if __name__ == "__main__":
    sys.exit(main())
