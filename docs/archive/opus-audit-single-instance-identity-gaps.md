# Audit prompt: single-instance enforcement + remaining identity gaps

> **ARCHIVED — one-off audit prompt, not authoritative.** Audit ran 2026-09-24. Outcome folded into
> [`docs/todo/LOGIN_CREDENTIAL_ISOLATION_PLAN.md`](../todo/LOGIN_CREDENTIAL_ISOLATION_PLAN.md) as
> new Decisions 7–11 (single-instance lock, attribution accepted, terminal escape hatch as
> identity-scoped launcher, pre-identity data → reserved `legacy` profile, immutable-name lifecycle)
> and a new phase P5 (single-instance lock). Kept as a second identity plan was rejected — same
> identity story, one home.

## Context

Follow-up to #1201 and the session-identity picker prompt. Two things
surfaced that don't fit either existing doc cleanly.

## Single-instance enforcement

Cecelia runs several components with fixed open ports. Remote access
(SSH/VNC or similar) to the shared machine is possible, so a second
launch attempt — local or remote — is already possible today. But
because ports are fixed, a second launch doesn't get real concurrent
access: it crashes on whichever component's port binds first, with
whatever error that component happens to throw. So the system already
behaves as single-instance in practice — just accidentally and badly,
not deliberately.

**Decision: keep it single-instance, but make it deliberate.** Do not
change ports to support real concurrent instances — that's solving for
genuine concurrent identity-switching, a materially harder design than
anything built so far, for a case that was never actually working
cleanly anyway. Instead: detect an existing instance on launch (lock
file / PID check, or a port-probe against the first component that
binds) and fail with a clear message — "Cecelia is already running on
this machine" — rather than letting an arbitrary component's bind
failure surface as the error. This matters more given remote access:
a local user can glance at the screen and infer what happened; a remote
user launching over SSH/VNC just sees a stack trace with no context.

This also simplifies the identity picker: exactly one Cecelia process
running at a time means exactly one active identity at a time, no
shared-state race between simultaneous sessions to design around.

## Other gaps flagged, not yet resolved in either existing doc

- **Terminal escape hatch bypasses identity isolation entirely.** All
  credential-isolation work (#1201, the picker prompt) is scoped to
  Kiwi's spawned `claude` subprocess. If someone opens a terminal and
  runs `claude` directly, it falls back to whatever's ambient on the
  shared login — potentially reintroducing the exact cross-identity
  contamination problem this plan exists to solve, through a side door.
  If the freeform escape hatch stays available (still an open product
  decision per the shared-MCP audit), it needs the same
  identity-awareness — same `CLAUDE_CONFIG_DIR` scoping based on the
  active session identity — or it quietly undermines the isolation
  work.
- **Migration for pre-identity data.** Captures/threads/outcome-tags
  created before identity ships have no identity attached. Decide:
  bucket as unknown/legacy, leave null, or attempt a backfill. Matters
  for the guardrail-retrieval system, which surfaces whose past
  judgment it's showing.
- **Identity lifecycle** — renaming or removing a session identity.
  Old captures/attribution referencing the old name either break or
  need the old name preserved as history, similar to how `dataRef` was
  designed to stay meaningful even after the thing it points to
  changes. Decide before someone renames themselves and orphans part of
  the log.
- **Decision 3 (attribution) is still unresolved.** Flag again
  explicitly — don't let it become assumed-yes just because the picker
  and config-dir plumbing are moving forward around it. It still needs
  an actual go/no-go from the team.

## Ask

1. Implement single-instance detection with a clear failure message,
   before any port/concurrency work — confirm this removes the need to
   design for simultaneous identity sessions.
2. Decide the terminal escape hatch's identity-awareness question, or
   explicitly flag it back to the team if it's a product call rather
   than a technical one.
3. Propose a migration approach for pre-identity data (bucket / null /
   backfill) with a recommendation.
4. Propose an identity lifecycle policy (rename/remove behavior)
   consistent with how `dataRef` handles referents changing over time.
5. Re-surface Decision 3 (attribution) as still open — do not treat
   downstream work as having implicitly resolved it.
