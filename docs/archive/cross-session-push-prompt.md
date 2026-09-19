# Real Push Delivery for Share-In — Cecelia

> **ARCHIVED — not authoritative, do not act on this.** The design brief for replacing #1040's
> clipboard/toast stand-in with real cross-session push over Claude Code's `SendMessage`
> machinery. Kept as a record of what was asked; if this work ships, the current design lives in
> `docs/<AREA>.md` or a `docs/todo/*_PLAN.md`, and this file should carry an outcome note here.

## Background

`#1040` (already merged, PR #3 of `docs/todo/BIDIR_CONTEXT_PLAN.md`) shipped
share-in: draw on the viewer, save, and the frame + overlay is written to
`<proj>/captures/<id>/{meta.json,frame.png}`, readable by Claude via two MCP
tools (`get_recent_captures`, `get_capture`). Because there was no way to
alert an already-running Claude Code session that something new landed, the
PR shipped a **clipboard-prefill + toast** as an explicit, named stand-in —
its own PR description calls this "a practical stand-in for MCP not being
able to alert a running Claude session yet. Real push-alerts wait for
Anthropic-side work." That work has since shipped: **Claude Code now has
built-in cross-session messaging** (`SendMessage`/`ListAgents`, delivered
over a per-session Unix domain socket on macOS/Linux or a named pipe on
native Windows, same-machine delivery never touching Anthropic's servers,
requires Claude Code v2.1.224+). A script or hook can post directly into a
running session's inbox socket — the docs have a section written for exactly
this: *"Read this section when... you want a script or hook to post into a
session."* When the target session is idle, delivery **starts a new turn on
its own** — genuine push, not a polling illusion.

**The goal of this task is not full autonomy — it's making share-in feel like
an integrated conversation with Cecelia rather than an Ubuntu-screenshot-and-
drag ritual**, while keeping the person as the approval gate. Since Cecelia's
Julia backend is not a child process of the Claude Code session it's
notifying, its messages won't verify as "own-child" — by the documented
default, an unverified sender's message is **held for a one-time approval
dialog** rather than auto-delivered, unless `crossSessionInbound` is
explicitly set to `accept`. That default is correct and should be preserved
as the shipped default: a brief handshake/approval is fine and matches every
other explicit-trigger decision made across this project's design so far
(the drawing layer, the chain-execution punch list, the "watch over my
shoulder" rejection). Don't design around it or default to `accept` —
respect it, and let the person opt into `accept` themselves if they want it.

## Task

### Part 1 — Audit what exists today

- Read `#1040`'s actual implementation in full: `api/src/captures_api.jl`,
  `mcp/cecelia_mcp/{server,client,guidance}.py`, and the clipboard-prefill +
  `.vw-status-chip` toast logic in `ViewerWindow.vue`/`ViewerPanel.vue`. This
  is what gets replaced or augmented, not rebuilt from scratch.
- Read the full cross-session messaging behavior at
  `https://code.claude.com/docs/en/cross-session-messaging` — in particular
  the **inbox socket** section (`CLAUDE_CODE_MESSAGING_SOCKET`,
  `CLAUDE_CODE_MESSAGING_TOKEN`, the auth line format, the own-child
  verification rules, and what "unverified sender" delivery actually looks
  like by default) and the **`crossSessionInbound`** settings reference.
- Check whether Cecelia's Julia backend already has any mechanism for
  running arbitrary shell/socket I/O safely (existing subprocess-handling
  code, if any) that this should reuse rather than writing raw socket code
  from scratch.

### Part 2 — Design the handshake

Since Cecelia is a long-running desktop app started independently of any
particular Claude Code session — not a child process of one — it has no
automatic way to know which session's socket to post to, especially with
multiple sessions potentially open. This needs an explicit, one-time-per-
session pairing step, not silent auto-discovery:

- Propose a **pairing flow**: e.g., the person runs a Cecelia-provided slash
  command or tells Claude "register this session with Cecelia," which calls
  a new MCP tool (`register_push_target` or similar) with whatever
  Claude Code exposes about its own session (name, and per the docs, its own
  inbox socket path is visible via `/status`'s `Peer address` row — check
  whether that's obtainable programmatically by the session itself, or only
  human-readable, since that changes how automatic this step can be).
- The pairing should be **per-project, not global** — a person may have
  multiple Cecelia projects open against different Claude Code sessions.
  Store the association the same additive way as everything else in this
  design (a small per-project record, not a new database).
- **Re-pairing / expiry**: a paired session ends eventually (closed,
  crashed, restarted). Define what happens when Cecelia tries to push to a
  socket that's gone — fail silently back to the clipboard/toast fallback
  (Part 3), not an error the person has to debug.

### Part 3 — Design the push, with graceful degradation

- **The pushed message is a short, plain-text notification, not the
  image itself** — cross-session messages are plain text only per the docs
  ("Claude sends only plain text across sessions"). The message should name
  the capture id and prompt Claude to call `get_capture`/
  `get_recent_captures` itself — the existing MCP read path stays exactly as
  it is; this only replaces how Claude *learns* a capture exists.
- **Fall back to the existing clipboard-prefill + toast when messaging isn't
  available** — Claude Code version too old, `crossSessionInbound` set to
  `refuse` on the receiving end, no paired session registered, or the socket
  from a prior pairing is gone. This should degrade cleanly, not error out;
  the existing `#1040` mechanism is the correct fallback, not something to
  delete.
- **Respect the default held-for-approval behavior explicitly in the UX** —
  when the person triggers share-in, if delivery requires their approval on
  the Claude Code side, the Cecelia UI should say so plainly (e.g. "sent —
  approve in your Claude session to deliver") rather than implying it
  silently arrived. Don't build UI that assumes `accept` is set.
- **This does not grant Claude any new code or execution access** — it's a
  notification channel, nothing else. Reconfirm this explicitly in the
  report: the MCP-only boundary and the additive-write discipline from the
  rest of this design are unaffected: this task changes how Claude is
  *alerted*, never what it can *do*.

Propose:
1. The pairing mechanism, concretely — what MCP tool(s) it needs, what gets
   stored where, and how re-pairing/expiry is handled.
2. Exactly what the pushed message says (wording, not just structure) and
   how Cecelia's Julia backend opens the socket connection and posts it
   (including the auth-line requirement on native Windows).
3. The fallback logic — the specific conditions that trigger falling back
   to `#1040`'s clipboard/toast path, and how that decision is made (a
   send failure, a timeout, an explicit "not paired" state).
4. UI changes needed in `ViewerPanel.vue`/`ViewerWindow.vue` to reflect
   pairing status and delivery state honestly (paired vs. not, sent vs.
   delivered vs. held-for-approval vs. fell back to clipboard).
5. How this surfaces in `guidance.py` and `docs/MAP.md`/
   `docs/MAINTAINABILITY.md`, same as every other addition in this project.
6. A proposed PR sequence for implementing this — small, reviewable units
   (e.g. pairing mechanism first, push + fallback second, UI polish third),
   matching the cross-cutting PR-phasing discipline already established for
   this project's larger designs. State the sequence explicitly in your
   report.

## Your own verdict

Answer these first, with real conviction:

- Is a person-guided, per-session pairing step actually acceptable friction,
  or does it undercut the "less like dragging a screenshot" goal enough that
  this isn't worth building yet? Give a clear recommendation.
- Does `/status`'s `Peer address` row's socket path have to be manually
  copied by a person, or is there a way for Claude itself (already running
  in that session) to surface it programmatically to Cecelia during
  pairing — which would make the one-time handshake much lighter? Say
  plainly which one Part 1's research actually supports.
- Given the default is held-for-approval, does this genuinely feel
  meaningfully more "integrated" than the current clipboard-prefill flow, or
  is the improvement mostly about removing the manual "go to the terminal"
  step while the actual approval-click replaces it with a different manual
  step? Be honest about how much this actually buys, not just how it's
  architected.

Don't hedge this section to match the tone of the rest of the report — this is
the part meant to be read first.
