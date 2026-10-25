# Co-drive record — a session where you and Claude work together, as a Blackboard entry you mark

**Status:** P1 built (2026-10-24) — `pixi run codrive-record <transcript>` writes the record from a
Claude Code transcript; `#fix` marks a correction. P2 (your actions in the app) and P3 (corrections
across sessions) not built. Follows the co-driving audit
([`docs/archive/OPUS_AUDIT_CLAUDE_CO_DRIVING_REPORT.md`](../archive/OPUS_AUDIT_CLAUDE_CO_DRIVING_REPORT.md)),
which found no record of any co-driven session. Reuses the run record of
[`AGENT_RUN_REVIEW_PLAN.md`](AGENT_RUN_REVIEW_PLAN.md) (one section per step, a verdict on each).

## Goal

After a session where Claude drove the app and viewer while you steered, you can read back what it
looked at, what it pointed at, what it said and where you corrected it — and mark each step. Over a
handful of sessions the corrections become the list of error modes Cecelia should check in code
(the audit's checkpoint card) and the material for short showcase write-ups. Autonomous runs already
get this ([`AGENT_RUN_REVIEW_PLAN.md`](AGENT_RUN_REVIEW_PLAN.md)); co-driven sessions got nothing.

## What exists

- **Claude's half is already on disk.** Every Claude Code session is a transcript,
  `~/.claude/projects/<cwd>/<sessionId>.jsonl`: your messages, every tool call (MCP included) and its
  result, Claude's text, timestamps. A capture you share arrives in it as a pushed message
  (`[cecelia] shared capture cap-…`), so that much of your side is recorded too.
- **The record format and its review UI.** `scripts/agent_eval/run_record.py` turns a headless
  run's stream-json trace into a Blackboard entry, one section per decision; the Blackboard gives
  any `### sNN · …` section its own good / bad / unsure verdict (`api/src/blackboard_run_review.jl`).
- **Not on disk:** what you did in the app between messages (brushes, gate edits, clicks). The
  observer sees some of it (`poll_observations`) but keeps it in memory only (`mcp/cecelia_mcp/monitor.py`).

## Decisions

1. **The source is the transcript.** No new logging in the MCP server or the app for Claude's half:
   the transcript already has every call with its result and your words between them.
2. **The record is written by the script, never by Claude** — same rule as a run record, so a
   session can't leave out the step that went wrong.
3. **One section per exchange** — your message, then what Claude read, pointed at, wrote and said
   until your next message. Not per action: the observer mostly reads and points, so the unit that
   carries meaning is the exchange. A shared capture is its own section.
4. **Only exchanges that touched Cecelia become sections** — a Cecelia tool call (`cecelia-observer`,
   `cecelia-autonomous`), a shared capture, or a `#fix`. Coding in the same session is counted in the
   header, not shown.
5. **`#fix` marks a correction.** Type `#fix` anywhere in a message (`#fix the gate is far too
   loose`). That section's heading says `fix`, quotes you, and names the section it corrects (the one
   before). It is not auto-marked: you give the verdict, as on any section.
6. **Generic `### sNN` sections, no `agentRun` marker.** `agentRun` means an unattended run on a
   disposable copy (a `bad` needs a guide / platform / agent cause, the list files it under Agent
   runs). A co-drive record is an ordinary entry titled `Co-drive session …`; per-section verdicts work
   on it unchanged.
7. **Pictures:** a PNG a tool returned (`gate_plot`, `gate_cells_view`, …) is uploaded as an
   `agent_run` capture (evidence, kept out of your shared-captures list) and attached to its section; a
   `get_capture` cites the capture id it read; any other image is skipped and the section says so.
8. **Project:** the one most of the session's Cecelia calls name; `--project` overrides. Calls into
   other projects are listed in the header.
9. **A local copy** of the parsed record goes to `~/.cecelia-effectiveness/codrive/<sessionId>.json`
   so P3 can read corrections across sessions without the app.

## Phases

- **P1 — built.** `scripts/agent_eval/codrive_record.py` + `pixi run codrive-record [transcript]
  [--project UID] [--since ISO] [--until ISO] [--dry-run DIR]`. With no transcript it records the
  session it runs under (`claude_sessions.calling_session`), so at the end of a session you tell
  Claude "record this co-drive" and it runs the script (`.claude/skills/codrive-record/SKILL.md`). Verified: unit tests on a synthetic
  transcript (`python/cecelia/tests/test_codrive_record.py`) and a dry run on a real session.
- **P2 — your actions in the app.** Persist the observer's observations per session (a sidecar in
  the project) and fold them into the exchange they fall in by timestamp.
- **P3 — corrections across sessions.** Read every `codrive/*.json`, list the `#fix` sections with
  their verdicts and notes, group them by error mode. Input to the checkpoint card.
