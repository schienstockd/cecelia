# Audit prompt: Cecelia login + per-user Claude credential isolation

> **ARCHIVED — one-off audit prompt, not authoritative.** Audit ran 2026-09-23. Outcome parked
> as [`docs/todo/LOGIN_CREDENTIAL_ISOLATION_PLAN.md`](../todo/LOGIN_CREDENTIAL_ISOLATION_PLAN.md).
> `CLAUDE_CONFIG_DIR` isolation and ambient-env override risk both empirically confirmed on the
> currently-installed CLI (2.1.280). One premise in the prompt is wrong: it says
> `KIWI_ASSISTANT_PLAN.md` has an "attribution section" that assumes a `cecelia_user` identity —
> no such section exists in the current plan. Its Decision 2 explicitly says "no per-user
> attribution system — each user's own login is the account". The plan splits the two asks:
> credential isolation is the real requirement (Decision 2 needs it once you accept the
> shared-OS-login constraint); attribution is called out as an optional extension the user must
> confirm because it *reverses* Decision 2, not extends it.

## Context

`KIWI_ASSISTANT_PLAN.md`'s attribution section assumed a `cecelia_user`
identity for token logging and (implicitly) for picking a Claude
credential. That identity doesn't exist — Cecelia currently runs under
one shared OS login, used by everyone in the lab. This breaks two
things the plan depends on:

1. **Attribution** — no way to log `{cecelia_user, tokens, tool_calls}`
   per call without a real per-person identity.
2. **Credential isolation** — each person needs their own Claude login
   (per-seat access, no shared API key — established in the earlier
   architecture decision), but a single OS login has one shared
   `~/.claude` credentials file. Without isolation, the only fallback
   is manual `claude logout` / `claude login` cycling per session,
   which is fragile at more than one or two users: if someone forgets
   to log out, the next person's Kiwi calls silently run under the
   wrong account — no error, just misattributed usage until someone
   notices a claim referencing data they never looked at.

## Decision: Cecelia login yes, config-dir encryption no

**Add a lightweight Cecelia login/profile identity.** Not real auth —
a name/profile selected or entered once, closer to a git author name
than a password gate. Purpose is twofold: (a) attribution key for token
logging, replacing the `cecelia_user` assumption, and (b) selects which
Claude credential profile a given Kiwi session uses.

**Do not encrypt the per-user Claude config directories.** Considered
and rejected: the population that can access those files (anyone with
the shared OS login) is exactly the population that can launch Kiwi and
trigger decryption, since the Kiwi service needs the decrypted
credential available to hand off to the `claude` CLI subprocess for any
legitimate session. Encryption keyed to a Cecelia login only stops
someone with shared-login filesystem access who *isn't* going through
Cecelia at all — narrow, and adds real cost (key management,
encrypt/decrypt session lifecycle, crash handling for a half-decrypted
directory) for a threat that doesn't match who actually has access.

**If credential theft between lab members is a genuine concern** (not
just misattribution, but deliberate use of a colleague's session), the
real fix is separate OS-level accounts, not app-level encryption on a
shared one. Flag this explicitly as a decision for the team — is
trust-based attribution sufficient, or does this need actual OS
separation? Don't default to encryption as a middle ground; it doesn't
provide a real boundary here.

## Mechanism: `CLAUDE_CONFIG_DIR`, keyed to Cecelia login

Claude Code has no officially supported multi-account mode, but
`CLAUDE_CONFIG_DIR` is the standard, stable workaround — several
community tools (`claude-account-switch`, `claude-multiprofile`,
`claude-use`) build on it, and Anthropic's engineers have engaged with
open feature requests for native multi-account support, so the approach
is well known even if undocumented as a first-class feature.

Layout:
```
/shared/kiwi-profiles/<cecelia-login-name>/
```
One-time setup per person: create their profile dir, run
`claude login` with `CLAUDE_CONFIG_DIR` set to it. Thereafter, Kiwi's
backend sets `CLAUDE_CONFIG_DIR` per subprocess invocation based on the
active Cecelia login for that session — reuses the same identity as the
attribution logging, no second identity system.

## Risk to check before rollout

**Ambient credential override.** `ANTHROPIC_API_KEY`,
`ANTHROPIC_AUTH_TOKEN`, `CLAUDE_CODE_OAUTH_TOKEN`, and the
`CLAUDE_CODE_USE_BEDROCK/VERTEX/FOUNDRY` family silently outrank
whatever's set via `CLAUDE_CONFIG_DIR`, on every platform. If any of
these are set in the shared OS login's environment (leftover from
testing, a stray `.bashrc`/`.profile` line), every Kiwi session
silently authenticates as whoever set that variable, not as the
selected Cecelia-login person — and it fails silently, no error. Audit
the shared login's environment for these before rollout, and consider
having the Kiwi service explicitly unset them before invoking `claude`
as a defensive measure regardless of what's currently set.

## Explicitly not being solved here

- Real authentication/passwords for the Cecelia login — it's a label,
  not a security boundary. Anyone can pick anyone else's name; nothing
  stops that on a shared OS login. Acceptable for a lab tool where
  misattribution is a minor annoyance, not acceptable if that
  assumption changes later — don't let logs be treated as tamper-proof
  downstream (e.g. in the outcome-tag/guardrail-retrieval system) if
  they aren't.
- OS-level user separation — out of scope unless the team decides
  trust-based attribution isn't sufficient (see decision above).

## Ask

1. Confirm `CLAUDE_CONFIG_DIR` is respected by the currently-deployed
   Claude Code CLI version and works as described for isolating
   credentials, session state, and settings per profile.
2. Design the Cecelia login/profile picker: first-use creation flow,
   returning-user selection (dropdown/last-used), where profile names
   are stored, and how this feeds both the attribution logging and the
   `CLAUDE_CONFIG_DIR` selection for a given session.
3. Add an explicit pre-rollout check (script or manual checklist) for
   ambient credential env vars in the shared login's environment.
4. Update `KIWI_ASSISTANT_PLAN.md`'s attribution section to replace the
   `cecelia_user` assumption with this actual mechanism.
