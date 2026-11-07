"""Claude Code PreToolUse hook (Bash): block a `cd` into a worktree another live session is in.

Wire it in `~/.claude/settings.json` (global, so sessions launched from any folder get it):

    "hooks": {"PreToolUse": [{"matcher": "Bash", "hooks": [{"type": "command",
        "command": "f=<repo>/scripts/worktree_cd_guard.py; [ -f \\"$f\\" ] && exec /usr/bin/python3 \\"$f\\" || exit 0"}]}]}

Logic and rationale: `cecelia.effectiveness.worktree_cd_guard`. Stdlib only, run by any `python3`;
fails open if the repo's python can't be imported.
"""
from __future__ import annotations

import pathlib
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parents[1] / "python"))

try:
    from cecelia.effectiveness.worktree_cd_guard import main
except Exception:   # a broken checkout must not block every shell command
    sys.exit(0)

sys.exit(main())
