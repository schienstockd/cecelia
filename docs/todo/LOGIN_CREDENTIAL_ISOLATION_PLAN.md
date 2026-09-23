# Cecelia login + per-user Claude credential isolation — plan

**Status:** planning (2026-09-23) · branch `audit/login-credential-isolation`. Derived from
[`docs/archive/opus-audit-cecelia-login-credential-isolation.md`](../archive/opus-audit-cecelia-login-credential-isolation.md).
Audit findings below have been empirically verified on this box against the currently-installed
`claude` CLI (2.1.280). Awaiting user go/no-go on **Decision 3** (attribution).

## Goal

Make [`KIWI_ASSISTANT_PLAN.md`](KIWI_ASSISTANT_PLAN.md) Decision 2 — *"each user's own login is
the account"* — actually hold on the shared OS login the lab uses. Today it does not: everyone
shares one `~/.claude/.credentials.json`, so whoever last ran `claude login` is who every Kiwi
turn silently authenticates as, no matter who is sitting at Cecelia.

The audit prompt also proposes adding **per-user attribution** (tokens/tool-calls keyed to a
Cecelia login) on top of the isolation. That reverses Decision 2's "no per-user attribution
system" — see **Decision 3** below. This plan is scoped to credential isolation; attribution is
called out but not designed here.

## What the audit confirmed on this box (2026-09-23)

CLI: `claude 2.1.280 (Claude Code)`, binary at `/home/dominik/.local/bin/claude`.

1. **`CLAUDE_CONFIG_DIR` is a real credential boundary.** Point it at an empty directory and
   `claude -p 'hi' --model haiku` prints `Not logged in · Please run /login` (exit 0) rather than
   using `~/.claude/.credentials.json`. `--version` does not create any files in the pointed-at
   dir; only real invocations do. Isolation for credentials, MCP config, and session state all
   flow from this single env var — Cecelia already relies on it on the read side in
   [`app/src/ai/agent_runner.jl`](../../app/src/ai/agent_runner.jl) `claude_config_path()`
   (line 244).
2. **Ambient credential env vars silently override `CLAUDE_CONFIG_DIR`, as the prompt warned.**
   With an empty `CLAUDE_CONFIG_DIR`:
   - `ANTHROPIC_API_KEY=<fake>` → the CLI attempts a network call using the fake key (hangs until
     timebox); it does **not** say "Not logged in".
   - `ANTHROPIC_AUTH_TOKEN=<fake>` → the CLI returns `Failed to authenticate. API Error: 401
     Invalid bearer token` from Anthropic's API.
   - Fresh env with no ambient key → `Not logged in · Please run /login` (the intended baseline).
3. **The current shell env on this workstation is clean.** No `ANTHROPIC_*` / `CLAUDE_CODE_OAUTH_TOKEN`
   / `CLAUDE_CODE_USE_BEDROCK|VERTEX|FOUNDRY` in the running env, `~/.bashrc`, `~/.profile`, or
   `~/.bash_profile`. `/etc/environment` and `/etc/profile.d/*.sh` were **not** verified (needed
   sudo); the pre-rollout script has to check them.
4. **Cecelia currently has no user identity at all.** A grep across `app/src/` and `api/src/` for
   `cecelia_user` / `current_user` / a session identity finds nothing. `"user"` in the codebase
   only appears as an install scope ("user" vs "system"), a model-directory source tag, and
   message-role strings. Everything user-facing today is machine-scoped via `custom.toml` inside
   `config_dir()` (`app/src/config.jl`). So Decision 2's "each user's own login is the account"
   was aspirational — the mechanism to realise it does not exist yet.

## Decisions

1. **Credential isolation is a real requirement, not an optimisation.** Without it, Decision 2 of
   `KIWI_ASSISTANT_PLAN.md` is not satisfied on the shared-OS-login setup the lab actually uses.
   Manual `claude logout` / `claude login` cycling per session is unmanageable at more than one
   or two users — one missed logout and the next person's Kiwi turns run under the wrong
   account, silently, until someone spots a claim referencing data they never opened.
2. **Mechanism: per-profile `CLAUDE_CONFIG_DIR`, keyed to a Cecelia login/profile name.**
   Layout:
   ```
   <config_dir()>/kiwi-profiles/<profile-name>/     # holds .credentials.json, .claude.json, etc.
   ```
   One-time setup per person: create the dir, run `claude login` with `CLAUDE_CONFIG_DIR` pointed
   at it. Thereafter every claude-CLI invocation Cecelia spawns sets `CLAUDE_CONFIG_DIR` to the
   active profile's dir for that session, via `addenv(cmd, ...)` on the `Cmd` built in
   `agent_runner.jl:_build_claude_cmd` (currently the `Cmd` inherits the process env — this is
   the one line that has to change to make isolation work end-to-end). `claude_config_path()`
   already resolves through `CLAUDE_CONFIG_DIR`, so the MCP-registration read side falls into
   line automatically.
3. **Attribution (per-user token/tool-call logging) is a SEPARATE decision — deferred to the
   user.** The audit prompt bundles this with isolation, but the two have different threat models
   and different reversibility. Isolation is a bug fix (Decision 2 doesn't work without it);
   attribution is a new capability that reverses Decision 2's "no per-user attribution system"
   commitment. Not adopting it in this plan; do not add token-logging plumbing until the user
   confirms. If yes, the same profile name is the natural key — no second identity system needed.
4. **Do not encrypt the per-profile Claude config dirs.** Copied from the audit prompt and agreed:
   the population that can read those files on the shared OS login is the same population that
   can launch Kiwi and trigger decryption, so encryption keyed to a Cecelia login gives no
   boundary against the actual threat and adds real cost (key management, crash handling for a
   half-decrypted dir). If theft between lab members (not just misattribution) is a real concern,
   the fix is OS-level accounts, not app-level encryption — flagged for the team; not this plan.
5. **Cecelia login is a label, not a security boundary.** Anyone can pick anyone else's name;
   nothing stops that on a shared OS login. Acceptable for a lab tool where misattribution is a
   minor annoyance. **Downstream systems must not treat these logs as tamper-proof** — matters if
   the outcome-tag / guardrail-retrieval system in `KIWI_ASSISTANT_PLAN.md` ever keys on the
   profile name for a trust decision.
6. **Ambient credential env vars must be scrubbed from the spawned `claude` process, not just
   audited pre-rollout.** Pre-rollout audit (below) catches what's set today. Defensive scrubbing
   at spawn time catches what's set tomorrow by a stray `.bashrc` edit — the failure is silent, so
   a one-shot audit is not enough. Scrubbing goes in the same `addenv` call that sets
   `CLAUDE_CONFIG_DIR`.

## Open questions for the user

- **Q1 — attribution: yes or no?** (See Decision 3.) If yes, what's logged and where does it go
  (Kiwi turn log entries, a separate audit file, both)? The prompt asks for `{cecelia_user, tokens,
  tool_calls}` per call, but that shape needs sign-off before it's built.
- **Q2 — profile picker UX.** First-use creation, returning-user selection, and where the "which
  profile is active in this session" state lives. Options: browser-tab-scoped (localStorage on
  frontend, per-request header to backend), machine-scoped (single `custom.toml` key, last-used
  wins across tabs), or session-scoped (backend state, requires a real session concept the app
  does not have yet). Recommend browser-tab-scoped — simplest, matches the "label not boundary"
  framing, and doesn't require inventing a session system. Needs confirmation.
- **Q3 — where the profile roster lives.** A `<config_dir()>/kiwi-profiles/` directory listing is
  the natural source of truth (a profile exists iff its dir does). Optionally cached in
  `custom.toml` for a faster picker load. Cheap and self-heals.
- **Q4 — OS-level accounts vs shared login.** Out of scope here per Decision 4, but the plan
  should not build in assumptions that later block it (e.g. don't put `kiwi-profiles/` in a
  location that OS-level accounts would fight over). `<config_dir()>` per-user is fine.

## Phases

### P1 — Pre-rollout audit script (small, standalone)
`scripts/check_claude_env.sh`. Prints any ambient `ANTHROPIC_*` / `CLAUDE_CODE_OAUTH_TOKEN` /
`CLAUDE_CODE_USE_{BEDROCK,VERTEX,FOUNDRY}` in the current shell env, in `~/.bashrc`, `~/.profile`,
`~/.bash_profile`, `~/.zshrc`, `~/.zprofile`, `~/.bash_aliases`, `/etc/environment`, and
`/etc/profile.d/*.sh`. Exit 1 on any hit, 0 on clean. Run it before enabling multi-profile in a
lab install. Ships this phase; landable independently.

### P2 — Spawn-side plumbing (backend, no UX)
Change `_build_claude_cmd` in `app/src/ai/agent_runner.jl` (~line 214) to return a `Cmd` with an
explicit env: `CLAUDE_CONFIG_DIR` set to the resolved profile dir, and the ambient credential vars
from P1 explicitly unset (`ENV` inherited otherwise). Add a config key `[ai].profile` in
`custom.toml` (single-profile default: `"default"`, resolving to `<config_dir()>/kiwi-profiles/default/`)
so the change is behaviourally a no-op for the current single-user setup — just a directory rename
of the credential home. Test: `_build_claude_cmd` unit test asserts env shape.

### P3 — Profile roster + picker (frontend + backend)
List `<config_dir()>/kiwi-profiles/*/` on the backend, expose via a new route; frontend adds a
picker on `KiwiCockpit.vue`. First-use flow spawns `claude login` in a terminal handoff (the same
pattern as the existing "Set up my terminal" button in `agent_runner.jl:217+`), scoped to the new
profile dir. Selection persists per browser tab. **Do not** ship without P1's audit passing on the
target machine.

### P4 — (conditional on Q1) attribution logging
Only build if the user confirms Decision 3 should be reversed. Add `{profile, tokens, tool_calls,
turn_id}` to Kiwi turn logs, keyed by the same profile picker. Amend `KIWI_ASSISTANT_PLAN.md`
Decision 2 in the same PR.

## Interaction with `KIWI_ASSISTANT_PLAN.md`

Do NOT rewrite Decision 2 as part of this plan — the audit prompt claims that plan has an
"attribution section" that assumes a `cecelia_user`; there is no such section. Instead:

- Add a one-line pointer near Decision 2 saying "credential isolation on a shared OS login is
  designed in [`LOGIN_CREDENTIAL_ISOLATION_PLAN.md`](LOGIN_CREDENTIAL_ISOLATION_PLAN.md); required
  before more than one seat uses this box."
- P4 (attribution) *would* rewrite Decision 2, and does so in the same PR as it lands — not
  before.
