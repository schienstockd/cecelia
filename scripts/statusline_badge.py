"""Claude Code statusline wrapper: put the session's badge and name in front of another statusline.

    "statusLine": {"type": "command",
                   "command": "python3 <repo>/scripts/statusline_badge.py <your statusline command>"}

Claude Code pipes a JSON object to the statusline command on every refresh; its `session_id` says
which session this window is. This prints that session's badge (`agents.session_badge` — the same
shape and colour the agents pane draws, `pixi run agents`) and name (`cecelia-56`), then runs the
wrapped command with the same input and appends its output. So a window and its agents-pane row
can be matched by eye.

Never breaks the statusline it wraps: if the badge can't be worked out, the wrapped output is
printed alone; if the wrapped command fails, the badge is printed alone. Stdlib only, run by any
`python3` — it must not depend on the repo's pixi env being installed.
"""
from __future__ import annotations

import json
import pathlib
import subprocess
import sys

_REPO = pathlib.Path(__file__).resolve().parents[1]
sys.path.insert(0, str(_REPO / "python"))


def badge_for(stdin: bytes) -> str:
    """`● cecelia-56` for the session named in the statusline input; "" when unknown."""
    try:
        from cecelia.effectiveness.agents import session_badge
        from cecelia.effectiveness.claude_sessions import session_name
        sid = json.loads(stdin or b"{}").get("session_id") or ""
    except Exception:   # a broken import or input must not blank the statusline
        return ""
    if not sid:
        return ""
    name = session_name(sid)
    return f"{session_badge(sid)} {name}" if name else session_badge(sid)


def main(argv: list[str]) -> int:
    stdin = sys.stdin.buffer.read()
    badge = badge_for(stdin)
    wrapped = ""
    if argv:
        try:
            wrapped = subprocess.run(argv, input=stdin, capture_output=True, timeout=10,
                                     check=False).stdout.decode("utf-8", "replace")
        except (OSError, subprocess.TimeoutExpired):
            wrapped = ""
    first, nl, rest = wrapped.partition("\n")
    line = "  ".join(p for p in (badge, first) if p)
    # Bytes, UTF-8: the badge shapes don't exist in Windows' default cp1252 stdout encoding.
    sys.stdout.buffer.write((line + nl + rest if nl else line + "\n").encode("utf-8"))
    sys.stdout.flush()
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
