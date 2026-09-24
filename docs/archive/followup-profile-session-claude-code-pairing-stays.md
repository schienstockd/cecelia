# Follow-up: Claude Code pairing is staying — identity-awareness is now live, not hypothetical

> **ARCHIVED — one-off follow-up prompt, not authoritative.** Received 2026-09-24 confirming the
> Claude Code pairing path stays permanent alongside Kiwi's structured cockpit (PR #1202).
> Identity-awareness for the pairing path is handled by LOGIN_CREDENTIAL_ISOLATION_PLAN P6
> (identity-scoped terminal one-liner) — backend shipped on `feat/kiwi-punchlist`, frontend
> picker + "Open profile terminal" button shipped on `feat/kiwi-picker-ui`. Current design
> lives in [`docs/todo/LOGIN_CREDENTIAL_ISOLATION_PLAN.md`](../todo/LOGIN_CREDENTIAL_ISOLATION_PLAN.md).

## What changed

PR #1202 shipped Kiwi's structured claims-feed cockpit (Phases 4–5).
Decision, confirmed in review: the Claude Code freeform pairing path
stays permanently alongside it — not an escape hatch under debate
anymore. Reasoning: structured claims are good for "here's what's
shown, here's the pointer," but genuinely clunky for open-ended
back-and-forth, which real usage needs sometimes. Both paths are
product, going forward.

This closes an item earlier work on your side had marked open ("should
the freeform escape hatch stay — flag to team, don't assume"). It's
resolved: yes.

## Why that's directly relevant to the profile/identity work

Every credential-isolation and session-identity design so far
(`CLAUDE_CONFIG_DIR` scoping, the picker, attribution) has been scoped
to Kiwi's own spawned `claude` subprocess. The Claude Code pairing path
runs its own separate session against the same shared OS login — and
now that it's confirmed permanent rather than provisional, it needs the
same identity-awareness Kiwi is getting, or it's a live hole: on a
machine multiple people already share, a paired Claude Code session
that isn't scoped to the active session identity can silently
authenticate as whoever's ambient credentials happen to be set, the
exact failure mode the whole isolation plan exists to prevent.

## What to check / do

1. **Confirm whether Claude Code pairing sessions currently read
   `CLAUDE_CONFIG_DIR` at all**, or launch some other way (a different
   spawn path than Kiwi's backend). If it's a separate code path, it
   needs its own wiring to the active session identity, not an
   assumption that Kiwi's plumbing covers it.
2. **The session-identity picker's output (whichever identity is
   currently active) needs to be readable by both consumers** — Kiwi's
   backend spawn and the Claude Code pairing spawn — not just one.
   Worth confirming the picker's design already treats "active
   identity" as global session state rather than something private to
   Kiwi specifically.
3. **Single-instance enforcement (separately proposed) simplifies this
   too** — if only one Cecelia process runs at a time, there's one
   active identity to hand to both consumers, no cross-session race.
   Worth sequencing after single-instance lands if that's not already
   the plan.
4. Flag back if Claude Code pairing turns out to need meaningfully
   different treatment than Kiwi's spawn (e.g. if it's a genuinely
   separate process lifecycle) rather than forcing it into the same
   mechanism if it doesn't fit cleanly.

## Related files, for context

- `docs/todo/LOGIN_CREDENTIAL_ISOLATION_PLAN.md` (from #1201) —
  `CLAUDE_CONFIG_DIR` mechanism, ambient-var risk, confirmed no
  `cecelia_user`/`current_user` exists.
- Session-identity picker prompt — picker design, app-wide config gap,
  project-settings audit, single-instance enforcement.
