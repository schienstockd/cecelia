---
name: codrive-record
description: Record this co-driving session (the user steering, Claude driving Cecelia through the observer) as a Blackboard entry. Use when the user says "record the codrive", "record this session", "save this co-drive" or similar, usually at the end of a session.
---

# Record this co-drive

Run, from this session:

```
pixi run codrive-record
```

With no argument it records the Claude Code session it runs under (on any OS). If it says it can't
tell which session this is, ask the user for the session id and pass it. The script writes the record
from the transcript; you never write or edit it yourself (docs/todo/CODRIVE_RECORD_PLAN.md,
Decision 2), and you never summarise the session in its place.

- If the user names a project, add `--project <uid>`; otherwise the script picks the one most calls named.
- If they want only part of the session, pass `--since` / `--until` ISO times from the transcript.
- If they want to see it first, use `--dry-run <scratchpad dir>` and show them `entry.md`.

Then tell them, briefly: the entry's title, how many sections and how many `#fix` corrections it
has, and that they mark each section Good / Bad / Unsure on the Blackboard (each `fix` section
corrects the one before it). If the script says "nothing to record", say so — the session made no
Cecelia call and shared no capture.
