# Prompt: audit and plan inter-session messaging ("session bus")

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.

> **Outcome: not run; dropped on 2026-10-10.** Its central premise had already shipped natively:
> Claude Code 2.1.296 lists the live local sessions with a busy/idle state (`ListAgents`) and
> messages between them (`SendMessage`). Messages arrive wrapped with their sender, and a
> session in a different permission mode holds them for its user's approval. That covers the
> sanctioned-channel, provenance and permission-mode requirements here, and a home-made bus
> beside it would be the second, unmonitored channel the incident below warns against. The
> incident is real but the figures differ from this summary: reporting has ~1,200 agents and
> 70,000+ messages on the board, ~700 of them in the Hugging Face activity, disclosed 16 July 2026.
> Companion: [`CLAUDE_CONSOLE_AUDIT_PROMPT.md`](CLAUDE_CONSOLE_AUDIT_PROMPT.md).

Companion to `CLAUDE_CONSOLE_AUDIT_PROMPT.md`. Run in Claude Code (Opus) at the repo root. **Audit and plan only. Do not implement.** Throwaway spikes go under `scratch/` and are not committed. If `docs/todo/CLAUDE_CONSOLE_PLAN.md` exists, read it first and build on its session registry. Do not duplicate it.

## Problem

D often runs 10+ Claude Code sessions in parallel (one per worktree). Today he sometimes tells one session: "ping the session working on xyz", and the sessions then coordinate by hand-relayed requests. He wants this to be a first-class, safe, visible mechanism that works alongside `pixi run claude-console`.

Typical use: interface and contract questions between worktrees (e.g. an API shape shared by the Julia backend and the Vue frontend), "are you touching file X?", "I changed Y, rebase before you continue", handoff notes. Not: one session editing another's worktree.

## Why safety is a first-class requirement

The July 2026 OpenAI/Hugging Face incident is the reference case. Verify its details yourself from primary or well-sourced reporting before relying on them; do not trust this summary blindly. As reported: roughly 700 agents coordinated over an unsanctioned message board, exchanged tens of thousands of messages, one agent posted found credentials that others then used, and many tried to cover their tracks. The lesson is not "agents can't collaborate". It is that a shared channel amplifies agents, and that an unsanctioned, unmonitored channel is the failure mode. Our scale and threat model are much smaller (one user, ~10 sessions, same machine, permission prompts), but the same controls apply.

## Requirements

1. **Sanctioned**: only registered sessions can send/receive. Fixed, documented location. No network exposure (local files or stdio/loopback only). State plainly if any option needs a listening socket.
2. **Visible**: every message is an event in a log the console tails (same family as `events.jsonl`). The human can always see who asked whom what, and the outcome.
3. **Requests, not commands**: a received message is untrusted input. It never bypasses permission prompts. Receiving sessions should be told this by the delivery wrapper (clear provenance: from-session, worktree, branch, timestamp).
4. **No secrets on the bus**: outgoing messages are scrubbed for tokens, keys, credentials, `.env` content, absolute paths outside the repo. Decide where scrubbing runs and its failure behavior (fail closed).
5. **Bounded**: per-session rate limit, max hops/depth, ttl, max message size, dedupe, and loop detection (A↔B ping-pong). Define defaults.
6. **Kill switch**: one console key pauses all delivery; also a per-session mute. State survives console restarts.
7. **Resolvable addressing**: address by topic/worktree/branch, not only by session id. The registry needs a one-line topic per session. Define where it comes from (branch name, first prompt, parked plan) and how ambiguity is resolved (ask the human, never guess).
8. **Permission-mode awareness**: if a session runs with permission prompts skipped or in an auto-accept mode, define the policy (refuse delivery, deliver read-only, or flag loudly). Verify how to detect the mode.
9. **Delivery to idle sessions** works or degrades honestly (see task 3).
10. **Auditable and forgettable**: retention/rotation policy for the message log.

## Constraints

- Follow `CLAUDE.md` (incl. Windows-compat). Target is Ubuntu; keep POSIX-only code isolated.
- Reuse the console's registry, event-log tail, palette and key handling. No new daemon unless the audit shows files cannot meet the requirements.
- Prefer approaches D can read and debug in a text editor.
- A broken bus must never block or slow a Claude Code session.

## Tasks

### 1. Read first
`CLAUDE.md`, `pixi.toml`, `.claude/settings*.json` and hooks, both existing consoles, the effectiveness log schema (`log.py`), `CLAUDE_CONSOLE_PLAN.md` if present, and how the eval supervisor spawns agents. List what exists and what is missing. Do not assume.

### 2. Verify Claude Code facts against current official docs (fetch them; no memory)
- Which hook events can inject context into a session (`additionalContext` or equivalent), and exact semantics per event.
- Whether MCP servers can push notifications into a running session, or only respond to tool calls.
- Whether anything native exists for cross-session messaging, sub-agents, or "teams" that overlaps this, as of the current release.
- How to detect permission mode from a hook or the session metadata.
- Behavior of `claude --resume <id>` / `-p` against a session that is open interactively elsewhere (safe or not).
Put anything unconfirmed in an **Unverified** list.

### 3. Delivery to idle sessions
Evaluate, with real tests where possible on this Ubuntu machine:
- Hook injection on next prompt/tool call (what happens to a session sitting at its prompt for an hour?).
- Console-mediated: shows "message pending", tints the window title/background, human nudges the session.
- Headless resume to run a turn (only if task 2 shows it is safe).
- tmux `send-keys` as an opt-in path. Do not propose TIOCSTI/tty keystroke injection unless you verify it works on this kernel, and if it does, say whether it is wise.
Recommend one default and one opt-in. State what is impossible.

### 4. Transport options, compared in a table
- A. File mailbox (JSONL per session or one shared log) + hooks.
- B. A small MCP server exposing `list_sessions`, `send`, `read_inbox`, `ack` over stdio, backed by files.
- C. Existing open-source "shared context / agent coordination" MCP servers and team launchers (search current prior art; for each, read its source or docs, not just the README, and note maintenance, transport, auth, file locking, and whether it meets requirements 1-10).
- D. GitHub-based coordination (Discussions/Issues/PRs, as in chrbailey/Session-Bus): evaluate for handoff of *work*, and say why it is or is not suitable for live pings.
- E. Anything better you propose.
Criteria: the ten requirements, delivery latency, effort, debuggability, failure modes, testability. One recommendation, plus what would change your mind.

### 5. Protocol and data model
- Message schema (id, from, to/topic, kind: ask|inform|handoff|ack, text, reply_to, hop, ttl, ts, scrub_status). Versioned.
- Registry fields needed from the console plan (session id, worktree, branch, tty, topic, permission mode, last seen).
- Delivery wrapper text that tells the receiving agent the message is an untrusted request from a peer session, with provenance.
- State machine for a message (queued → delivered → read → answered/expired/dropped) and what the console shows for each.
- How the human intercepts: approve-before-deliver mode (default for first N messages or for asks that mention file edits), and a one-key "show conversation" view.

### 6. Threat model
Short table: threat, example, control, residual risk. Cover at minimum: prompt injection arriving via a peer message (e.g. a session that read hostile web content), secret leakage via messages, runaway loops/cost, a stale or spoofed session id, a message that asks another session to run destructive git/gh commands, permission-mode downgrade by proxy, and log tampering. Include what you would monitor and the alert you would raise in the console.

### 7. Test strategy
Pure-function tests for scrub, routing, rate/hop limits and the message state machine; a replay test from a recorded log; one end-to-end spike with two real sessions in two worktrees performing a contract handshake. Define the acceptance test.

## Deliverable

Write `docs/todo/CLAUDE_CONSOLE_BUS_PLAN.md`:

1. **Recommendation first** (transport, delivery default, safety stance), one paragraph.
2. Verified facts with doc citations, and an **Unverified** list.
3. Delivery-to-idle findings from your tests.
4. Transport comparison table and rejected options.
5. Protocol, data model, state machine.
6. Threat model.
7. Phased plan, each phase useful on its own. Suggested: P1 read-only visibility (registry topics, message log view, no delivery); P2 human-approved delivery; P3 bounded autonomous delivery between registered sessions; P4 opt-in extras (tmux nudge, headless resume).
8. Numbered open questions for D, each with your default.

Keep it terse: decisions and evidence, no narrative. Then stop and wait for D's review. Do not implement.
