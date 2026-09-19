# Real Push Delivery for Share-In — plan

**Status:** PR #1 shipped 2026-09-19 (`#1048`); PR #2 shipped 2026-09-19 (`#1049`, stacked on
#1048); PR #3 shipped 2026-09-19 (`#1051`, stacked on #1049) — all three PRs of the sequence
merged in one afternoon after the wire-protocol probe. Design work archived at
[`docs/archive/cross-session-push-prompt.md`](../archive/cross-session-push-prompt.md); this doc is
the settled version. Sits under `Part 5` of
[`docs/todo/BIDIR_CONTEXT_PLAN.md`](BIDIR_CONTEXT_PLAN.md) as a follow-up to PR #3 (share-in
shipped 2026-09 as `#1040`).

## Goal

Replace the `#1040` clipboard-prefill + `.vw-status-chip` toast stand-in with **real cross-session
push** from Cecelia's Julia backend into the running Claude Code session's inbox socket, so a
freshly shared frame arrives in the paired session without the user leaving the viewer.

This is a *notification channel* only. It does not grant Claude any new code or execution access —
the MCP-only boundary and additive-write discipline from
[`BIDIR_CONTEXT_PLAN.md`](BIDIR_CONTEXT_PLAN.md) are unaffected. What changes is how Claude learns
a capture exists, not what Claude can do with it.

## Cross-cutting constraints (non-negotiable)

- **Held-for-approval is the shipped default.** Cecelia's Julia backend is not a child of Claude
  Code; its messages are unverified sender, so Claude Code holds them for a one-time approval
  dialog by default (`crossSessionInbound: unset` behaviour). Preserve that. The UX assumes
  approval, not `accept`.
- **Plain text only.** Cross-session messages are plain text; the pushed message names the
  captureId and prompts Claude to call `get_capture` / `get_recent_captures`. The existing MCP
  read path stays exactly as it is.
- **Graceful degradation, not errors.** Every failure mode (Claude Code < v2.1.224, `refuse` on
  the receiving end, no paired session, dead socket, timeout) falls back to the existing
  `#1040` clipboard-prefill + toast — never surfaces as an error the user has to debug.
- **Per-project pairing, additive storage.** Pairing state lives at
  `<proj>/settings/push_target.json`, following the shape of the other per-project registries
  (`settings/captures.json`, `settings/notebooks.json`). No new database, no global setting.
- **Additive-write discipline.** New MCP tool + HTTP endpoint follow the `/api/lablog` /
  `/api/notebooks/write` / `/api/viewer/capture` pattern: create/replace-only, recoverable,
  allow-listed, pinned by a test in `mcp/tests/test_server.py::GuidanceTest`.

## Locked decisions

Numbered so code and other docs can cite them (`Decision N`).

1. **Pairing is implicit via MCP middleware — no user command at all** (amended 2026-09-19,
   PR #1048; supersedes the original "one Claude-side command" wording). Verified live on
   Claude Code v2.1.273: `CLAUDE_CODE_MESSAGING_SOCKET` and `CLAUDE_CODE_MESSAGING_TOKEN` are
   both env-var-readable inside the MCP process (which inherits them from the spawning Claude
   Code session at MCP startup). `CeceliaClient._maybe_pair` fires from any MCP tool with a
   `project_uid` on the first call per (project, socket, token) tuple; cached in-memory.
   Zero ceremony: a fresh Claude session pairs the moment it checks a project. If either env
   var is missing (older Claude Code, standalone MCP process), the middleware is silent and the
   fallback path applies as normal.
2. **An explicit `register_push_target(project_uid, session_label?)` MCP tool is kept as a
   debug / manual re-pair path only** (amended 2026-09-19). Auto-pair from Decision 1 is the
   primary entry; this tool exists for the small number of "force a re-pair now" moments — e.g.
   after Cecelia's own backend restarted and the user wants to check pairing without waiting
   for the next natural tool call. Zero-arg on the runtime data — reads the env vars itself,
   POSTs unconditionally, bypasses the cache. Errors with an actionable message if the env
   vars are missing (upgrade Claude Code, don't retry).
3. **Per-project pairing, not global, not per-image.** A user with two Cecelia projects open
   against two Claude sessions pairs each independently. The Julia push writer looks up the
   record by `projectUid`, no ambient state.
4. **Storage shape mirrors the notebooks/captures registries.** `<proj>/settings/push_target.json`
   = `{socketPath, token, sessionLabel?, pairedAt, pairedFromPid?}`. Not encrypted at rest —
   these are same-machine socket paths + short-lived session tokens, no worse than the token on
   disk in `~/.claude/`.
5. **Pushed message wording is fixed and short.** One line, mentions `captureId` and the
   project name, tells Claude to call `get_capture(captureId)`. Format:
   ```
   [cecelia] shared capture cap-a7b8b7fc from project zolIMa (viewer_frame, image
   1SqevM, t=3, z=7). Read it with get_capture("cap-a7b8b7fc").
   ```
   Locked as a helper (`push_writer.jl::format_capture_message`) so all callers speak the same
   sentence — a chat-side grep for `"shared capture"` finds every incident.
6. **Push writer lives in `api/src/push_writer.jl`, one function.** `push_capture_notification(
   project_uid, capture_id, address) → :sent | :fallback`. Wraps: read `push_target.json`, open
   socket, write auth line + message, close, return the outcome. Any exception ⇒ `:fallback`.
   Wired from `captures_api.jl` on successful capture POST. No retry, no queue — a missed push
   is not a crash-severity event; the clipboard/toast is right there.
7. **Auth line is JSON, not `AUTH <token>`** (amended 2026-09-19 after fetching the docs page).
   The Julia writer opens the Unix socket and sends `{"type":"auth","token":"<token>"}\n` as
   the FIRST line — that literal format, JSON not a bare `AUTH`. What comes after the auth
   line (the message frame) is NOT documented — my probe against my own inbox with four
   candidate shapes (`{type:"message",text:...}`, `{type:"user_message",text:...}`,
   `{type:"message",content:...}`, plain text) all had bytes accepted but nothing delivered.
   **PR #2's opening step is to resolve the frame format**; strace-ing a real Claude→Claude
   `SendMessage` on a target with `ptrace_scope=0` is the cheapest path.
8. **Held-for-approval is NOT the universal default** (amended 2026-09-19). The docs' inbound
   rules: with no `crossSessionInbound` set, the receiving session's permission-mode class
   decides. **Receiving session PROMPTS for permissions (the ordinary case) ⇒ the message is
   DELIVERED**, not held — a non-Claude sender like Cecelia has no bypass class to identify
   with, so it's treated as non-bypassing, which delivers. Only when the receiver is in
   `bypassPermissions` mode does the message get held for approval. This is a better UX than
   the plan assumed: for most users PR #2 will feel like real cross-session delivery, not an
   approval ceremony. Cecelia still doesn't recommend the user set `accept` and doesn't offer
   a toggle — the default just doesn't require it.
9. **Fallback is the existing `#1040` clipboard-prefill + `.vw-status-chip` toast, unchanged.**
   No refactor of the fallback in this PR sequence; that path already works. The push writer's
   `:fallback` return signals the frontend to render the existing chip.
10. **UI state has four values, not two.** `ViewerPanel.vue` / `ViewerWindow.vue` capture status
    chip shows: `not paired` / `paired: sent — approve in your Claude session` / `paired:
    delivered` / `paired: fell back to clipboard`. Never `delivered` optimistically — that word
    only appears when the frontend sees a real ACK, otherwise `sent`. Held-for-approval is a
    real state, name it plainly.
11. **ACK path exists via `crossSessionInbound` notices** (amended 2026-09-19 after fetching
    the docs page). When the receiver holds/delivers/denies/expires a message, Claude Code
    sends a notice back to the sender through the same socket transport. For an interactive
    Claude sender the notice appears in the transcript; for a script sender the notice
    arrives on the connection Cecelia opened, so Cecelia CAN observe delivery state without
    a Decision-24-style MCP counter. PR #3 UI polish uses this to render `sent → held →
    delivered/denied/expired` truthfully. PR #2 ships with a simpler `sent` state until the
    ACK reader is written; the socket transport itself doesn't need re-designing.
12. **A stale socket path is invisible until you try to write.** The Unix socket file exists on
    disk after the session closes; `connect()` fails on EPIPE / ECONNREFUSED. Push writer
    treats any connect/write error as `:fallback` and also **clears the stored
    `push_target.json`** (silently) so the next share doesn't re-attempt a known-dead socket.
    The pairing tool has to be re-run; the fallback tells the user "not paired" so the ask is
    obvious.
13. **`register_push_target` records `pairedFromPid` so a debugger can trace the pairing.** Not
    used for security — the socket + token are the only auth. Purely for post-hoc "which
    Claude session did I pair with?" when three are open.
14. **This is orthogonal to `poll_observations`.** The observer arc
    ([`docs/ai-assist/OBSERVER.md`](../ai-assist/OBSERVER.md)) is for pattern surfacing
    (`repeat_attempts`, lab-log echoes); this is a directed notification tied to a user action
    (Share button). Do not add a `capture_added` observation type to `monitor.py` — a
    superficially attractive alternative that would (a) require the observer to be running
    (Cecelia can't push if the user turned it off), (b) collapse the addressed / directed
    nature of a share into a per-session poll, and (c) still not solve the "already-running
    session has to notice on its own" problem this plan exists for.
15. **Guidance addition is small.** One paragraph in `BRIEFING_GUIDANCE`:
    > When a `[cecelia] shared capture cap-…` message arrives, read it with `get_capture(id)`
    > — the address is already in the message. No need to ask "which image".
16. **Test the pairing endpoint like every other additive write.** New row in
    `mcp/tests/test_server.py::GuidanceTest` for `register_push_target`. Additional Python-side
    unit test for the fallback selector logic; Julia-side unit test for `format_capture_message`.

## PR sequence

Independently mergeable in this order. Each ships a working, tested slice; nothing between PRs
leaves the system in a worse state than `#1040`'s stand-in.

1. **Pairing infra (shipped PR #1048, 2026-09-19).** `/api/push/target` HTTP endpoint (POST
   to write, GET for the frontend to read the paired-or-not state); per-project record at
   `<proj>/settings/push_target.json`; MCP client middleware (`_maybe_pair`) auto-registers on
   first tool call per (project, socket, token); explicit `register_push_target` MCP tool for
   manual re-pair; frontend chip beside the Share button. **No push wire-up yet** — the
   frontend can show "paired" but capture POSTs still take the clipboard/toast path. 12 files,
   ~450 lines (5 new client tests + 5 new frontend tests). Design shifted from the plan's
   original "one Claude-side command" model to fully implicit auto-pair — see Decision 1
   amendment.
2. **Push writer + fallback wiring (shipped PR #1049, 2026-09-19).** `api/src/push_writer.jl`
   opens the paired session's inbox socket, sends the auth line + message line, closes; any
   exception ⇒ `:fallback` + stale record cleared. `format_capture_message` locked as the ONE
   wording (Decision 5). `captures_api.jl::api_viewer_capture` calls it after the capture is
   on disk; response body carries `push: "sent" | "fallback" | "not_paired"`.
   `ViewerWindow.vue` reads the outcome — `"sent"` shows a "sent to your Claude session"
   toast and skips the clipboard; anything else keeps the existing `#1040` clipboard/toast
   flow. 4 new `@testset`s cover wording, unpaired, socket round-trip, stale clearing.
   Native Windows named-pipe branch untested (Open Item 2). ~300 lines. Chip stays at the
   two-state "paired ✓ / not paired" for this PR; the four-state chip from Decision 10 lands
   in PR #3.
3. **UI polish + WS signals (shipped PR #1051, 2026-09-19). NO ACK reader** — the plan's
   original Decision 11 amendment sketched reading `crossSessionInbound` notices on the
   sender's connection, but binary inspection (v2.1.278) confirmed those notices go to the
   SENDER'S OWN inbox socket via `sendPeerReceipt`. Cecelia binds no inbox as a raw-socket
   sender, so the notice never arrives. Reading it would require Cecelia to bind an inbox
   and register a from-address, deliberately out of scope. What did ship: (a) WS
   `push_target:changed` broadcast from `push_api.jl` (pair-write) + `push_writer.jl`
   (stale-clear) so the chip flips in real time; (b) WS `push:sent {projectUid, captureId}`
   from a successful push, driving a transient 3 s "sent ✓" chip flash; (c)
   `BRIEFING_GUIDANCE` addition per Decision 15 pinned by `test_server.py`. Chip stays at
   `not paired / paired ✓ / transient sent ✓`, not Decision 10's four-state pitch —
   held/denied/expired are unobservable from our side. ~120 lines. New
   `frontend/src/stores/push.ts` (thin WS dispatcher).

Dependencies: PR #2 depends on PR #1's `push_target.json` reader. PR #3 depends on both. Nothing
else cross-depends.

## Answers to the archived brief's three verdict questions

The archive brief's *Your own verdict* section asks three questions with real conviction. Locked
answers:

1. **Is a person-guided, per-session pairing step actually acceptable friction?** Acceptable
   *because Decision 1 collapses pairing to a single Claude-side command.* The friction is one
   line typed once per session per project. If pairing had required a manual copy from
   `/status` output, the answer would flip to "no, keep the clipboard flow." The 2026-09-19
   verification is what makes the whole plan viable — see Decision 1.
2. **Can Claude programmatically surface its own socket path?** Yes. Verified live 2026-09-19
   on Claude Code v2.1.273: `CLAUDE_CODE_MESSAGING_SOCKET` and `CLAUDE_CODE_MESSAGING_TOKEN`
   are both visible as env vars inside a session (Bash-readable). No manual `/status` inspection
   needed.
3. **Given held-for-approval, does this feel meaningfully more integrated than the current
   clipboard flow?** Yes, but modestly. The improvement is real — no terminal context switch,
   no paste, capture id auto-supplied, notification arrives during whichever turn the user is
   on. It is *not* full autonomy: the approval click replaces the paste. Honest framing is
   "skip the terminal round-trip; don't skip the approval." The design is worth building on
   that basis; anyone pitching this as hands-free is wrong.

## Open items

Not blocking the design — resolve during their PRs.

- **Decision 11's `sent → delivered` counter — build it or not.** Depends on whether
  `paired: sent — approve` reads as anxious or informative in real use. Ship PR #2 without;
  revisit at PR #3.
- **Named-pipe path format on native Windows.** Read from
  `https://code.claude.com/docs/en/cross-session-messaging` at PR #2; verify with a Windows
  test box before shipping. Not blocking Linux/macOS.
- **Multiple concurrent shares from the same project.** A rapid-fire second Share while the
  first is mid-approve — behaviour is "each message queued into the inbox in order," which is
  the Claude Code default; no Cecelia-side change needed but worth eyeballing.

## References

- Archive brief (source, do not act on directly):
  [`docs/archive/cross-session-push-prompt.md`](../archive/cross-session-push-prompt.md)
- Parent plan: [`docs/todo/BIDIR_CONTEXT_PLAN.md`](BIDIR_CONTEXT_PLAN.md) → PR #3 (share-in)
  shipped as `#1040`; this is its follow-up as PR #8
- Cross-session messaging protocol reference:
  `https://code.claude.com/docs/en/cross-session-messaging` (inbox socket, auth line,
  `crossSessionInbound`, own-child verification)
- Share-in HTTP surface: `api/src/captures_api.jl`, `POST /api/viewer/capture`
- Existing MCP surface: `mcp/cecelia_mcp/server.py` (`get_recent_captures`, `get_capture`)
- Existing fallback: `frontend/src/modules/ViewerWindow.vue` /
  `frontend/src/components/ViewerPanel.vue` clipboard-prefill + `.vw-status-chip` toast
