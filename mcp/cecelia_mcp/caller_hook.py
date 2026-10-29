"""Claude Code PreToolUse hook: tell the Cecelia MCP servers WHICH MODEL is calling them.

Claude Code passes an MCP server neither the model nor the session (measured 2026-10-29; the only
documented env var is CLAUDE_PROJECT_DIR, and a spawn-time value would go stale after `/model`
anyway). A PreToolUse hook does get `session_id` + `transcript_path`, and the transcript records the
exact model on every assistant message — so this hook reads it and adds two fields to the tool call's
arguments (`updatedInput`). The server strips them before the tool sees them (`caller.py`) and sends
them on to the API as headers, where `author_stamp()` records them.

Rules, each one measured:
- NEVER emit `permissionDecision`. `"allow"` would auto-approve every observer call and bypass the
  user's permission prompts; `updatedInput` alone is honoured and leaves permissions as they were.
- A SUBAGENT's call carries `agent_id`, but `transcript_path` is still the PARENT's — reading it would
  stamp the parent's model. The subagent's own transcript is `<session>/subagents/agent-<id>.jsonl`.
  When that can't be read the model is left out: unknown, never a guess.
- Fail open. Any error prints nothing and exits 0, so the call goes through unstamped.

Standalone on purpose (stdlib only, no package imports): the plugin and Kiwi's `--settings` run it as
a script with the install's Python. `caller.py` imports the field names from here.
"""
from __future__ import annotations

import json
import os
import sys

MODEL_FIELD = "cecelia_caller_model"
SESSION_FIELD = "cecelia_caller_session"

_TAIL_BYTES = 512 * 1024


def _model_in(lines) -> str:
    """The `message.model` of the LAST assistant message among `lines` (newest last), or ""."""
    for line in reversed(lines):
        if '"assistant"' not in line:
            continue
        try:
            msg = json.loads(line).get("message")
        except (ValueError, AttributeError):
            continue  # a partial first line from the tail read, or not an object
        if isinstance(msg, dict) and msg.get("role") == "assistant" and msg.get("model"):
            return str(msg["model"])
    return ""


def transcript_model(path: str) -> str:
    """The model of the newest assistant message in a Claude Code transcript (JSONL), or ""."""
    if not path or not os.path.isfile(path):
        return ""
    with open(path, "rb") as f:
        f.seek(0, os.SEEK_END)
        size = f.tell()
        f.seek(max(0, size - _TAIL_BYTES))
        tail = f.read().decode("utf-8", "replace").splitlines()
        model = _model_in(tail)
        if model or size <= _TAIL_BYTES:
            return model
        f.seek(0)  # one assistant message bigger than the tail window: read it all
        return _model_in(f.read().decode("utf-8", "replace").splitlines())


def caller_transcript(event: dict) -> str:
    """The transcript of whoever made THIS call — the subagent's own when `agent_id` is set."""
    path = str(event.get("transcript_path") or "")
    agent = str(event.get("agent_id") or "")
    if not agent:
        return path
    if not path.endswith(".jsonl") or not all(c.isalnum() or c in "-_" for c in agent):
        return ""
    return os.path.join(path[: -len(".jsonl")], "subagents", f"agent-{agent}.jsonl")


def hook_output(event: dict) -> dict | None:
    """The hook's stdout object for one PreToolUse `event`, or None to leave the call untouched."""
    tool_input = event.get("tool_input")
    if not isinstance(tool_input, dict):
        return None
    model = transcript_model(caller_transcript(event))
    session = str(event.get("session_id") or "")
    if not model and not session:
        return None
    updated = dict(tool_input)  # updatedInput REPLACES the input: carry every original field
    if model:
        updated[MODEL_FIELD] = model
    if session:
        updated[SESSION_FIELD] = session
    return {"hookSpecificOutput": {"hookEventName": "PreToolUse", "updatedInput": updated}}


def main() -> None:
    try:
        out = hook_output(json.load(sys.stdin))
        if out is not None:
            sys.stdout.write(json.dumps(out))
    except Exception:
        pass  # fail open: an unstamped call beats a blocked one


if __name__ == "__main__":
    main()
