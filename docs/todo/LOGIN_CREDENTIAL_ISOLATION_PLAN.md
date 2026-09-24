# Cecelia login + per-user Claude credential isolation — plan

**Status:** in progress (2026-09-24) — P1 shipped #1201, P2 shipped #1204, P5 shipped on
branch `feat/kiwi-single-instance-lock` (this branch). Derived from
[`docs/archive/opus-audit-cecelia-login-credential-isolation.md`](../archive/opus-audit-cecelia-login-credential-isolation.md)
and [`docs/archive/opus-audit-single-instance-identity-gaps.md`](../archive/opus-audit-single-instance-identity-gaps.md).
Audit findings below have been empirically verified on this box against the currently-installed
`claude` CLI (2.1.280). Ready to build.

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
3. **Attribution: SUPERSEDED by Decision 8** (user, 2026-09-24 — "yes"). Originally deferred; now
   accepted with the scope constraint in D8. The same profile name is the key. Kept here for the
   citation trail from earlier PRs.
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
7. **Cecelia stays single-instance by design** (user, 2026-09-24). Today it is *accidentally*
   single-instance — a second launch crashes on whichever fixed-port component binds first, with
   whatever error that component happens to throw; remote SSH/VNC users see a stack trace with no
   context. Detect an existing instance at Julia startup (lock file + PID + liveness check on the
   API port) and fail with a clear "Cecelia is already running on this machine (PID N since T)"
   before any other component binds. Real concurrent instances are explicitly not being solved —
   port ranges, per-instance dev/projects dirs, per-instance log routing, and MCP endpoint
   discovery are weeks of design for a case nobody actually runs today. If one person ever needs
   two at once, that is a future problem for the one person, not everyone's design tax. This also
   collapses the picker's shared-state race: one process ⇒ one active identity ⇒ no cross-tab
   contention to design around.
8. **Attribution logging: yes, but only the write side, and it piggybacks on Kiwi turn logs**
   (user, 2026-09-24). Every Kiwi turn log entry gains `{profile, tokens_in, tokens_out,
   tool_calls, turn_id}`; no new store, no new file, no new schema surface — one field extension
   on an existing record. **Do not build downstream consumers speculatively** — guardrail-retrieval
   keying on the profile, per-user token-spend surface, cross-profile comparison views, none of it,
   until something concrete asks for it and can be costed. Reverses `KIWI_ASSISTANT_PLAN.md`
   Decision 2's "no per-user attribution system" wording; that gets a one-line edit in the same PR
   as the schema extension (D8's PR, not this planning PR).
9. **Terminal escape hatch stays; Cecelia hands out identity-scoped terminals** (user,
   2026-09-24). Kiwi cockpit adds an "Open terminal (profile: X)" button that spawns
   `$SHELL -i` with `CLAUDE_CONFIG_DIR` pre-set and the same ambient-env scrub as D6. Composes
   with the existing "Set up my terminal" wire in [`agent_runner.jl:217+`](../../app/src/ai/agent_runner.jl)
   — same MCP-registration path, one more launcher on top. Someone who bypasses the button and
   opens a raw terminal is opting out of isolation on purpose, and that is consistent with
   Decision 5 (identity is a label, not a boundary). Documented in the picker's onboarding copy;
   not trapped, not warned about every time — treating users as adults.
10. **Pre-identity data → reserved profile `legacy`** (user, 2026-09-24). Name reserved, always
    exists, cannot be selected as a login target (rejects at picker creation). First-run migration
    (idempotent, one pass) flips every pre-identity capture / thread / outcome-tag / Blackboard
    entry / lab-log entry to `profile = "legacy"`. `null` is not used — it propagates through
    query code as a bug source. Backfill by guessing ownership is not attempted — no honest rule
    exists. Users who recognise their own work in `legacy` can duplicate it into their profile;
    the `legacy` original stays labelled honestly. Migration script is a one-shot at the version
    bump that ships D8 — no ongoing "legacy" writes after that.
11. **Identity lifecycle: immutable names, add-new-and-migrate** (user, 2026-09-24). Once created,
    a profile name cannot be renamed. Removal marks the profile `retired` — grayed out in the
    picker, cannot be selected as active, but all past attribution stays under the original name.
    This is the `dataRef` pattern (`docs/DATAMODEL.md`): the log is history; a referent can be
    retired but the pointer's meaning is preserved. Someone who regrets their profile name creates
    a new one — the old one becomes another `legacy`-shaped bucket. Only real cost is the "I
    picked a bad name once" case, which is cheap: one row in the picker's retired list. The
    alternative — a rename map that rewrites past attribution — breaks "log is history" and is not
    worth it for a lab tool.

## Phases

### P1 — Pre-rollout audit script (small, standalone)
`scripts/check_claude_env.sh`. Prints any ambient `ANTHROPIC_*` / `CLAUDE_CODE_OAUTH_TOKEN` /
`CLAUDE_CODE_USE_{BEDROCK,VERTEX,FOUNDRY}` in the current shell env, in `~/.bashrc`, `~/.profile`,
`~/.bash_profile`, `~/.zshrc`, `~/.zprofile`, `~/.bash_aliases`, `/etc/environment`, and
`/etc/profile.d/*.sh`. Exit 1 on any hit, 0 on clean. Run it before enabling multi-profile in a
lab install. Ships this phase; landable independently.

### P2 — Spawn-side plumbing (backend, no UX) — SHIPPED
Landed on branch `feat/kiwi-profile-spawn-env`. `kiwi_profile_name()` reads `custom.toml
[ai].profile` (defaulting to `"default"`); `kiwi_profile_dir(name)` resolves it — `default` maps
to `""` (i.e. keep the CLI's own `~/.claude*` paths, so a single-seat setup needs NO re-login),
and any named profile maps to `<config_dir()>/kiwi-profiles/<name>/`. Every `claude` spawn site
(`claude -p`, `claude mcp add-json`, `claude mcp remove`) is now wrapped in `_apply_claude_env` —
which sets `CLAUDE_CONFIG_DIR` (or omits it for `default`) AND scrubs the ambient credential env
vars via `addenv(cmd, "KEY" => nothing)` (Julia unsets on `nothing`). `claude_config_path` grew
an optional `profile_dir` arg so the "is the terminal already set up?" UI check reflects the
profile the app actually spawns under. Pinned by the `AI observer per-profile credential env (P2
plumbing)` testset in `app/test/suite/observer.jl`. **Deviated from the original wording**: the
plan text said the default profile would resolve to `<config_dir()>/kiwi-profiles/default/`; that
would have orphaned the existing `~/.claude` login (a re-login the user did not sign up for), so
`default` special-cases to `""`. Named profiles land under the plan's directory on first
resolution — the picker (P3) is what triggers that.

### P3 — Profile roster + picker (frontend + backend)
List `<config_dir()>/kiwi-profiles/*/` on the backend (a profile exists iff its dir does — cheap
and self-healing), expose via a new route; frontend adds a picker on `KiwiCockpit.vue`. First-use
flow spawns `claude login` in a terminal handoff (the same pattern as the existing "Set up my
terminal" button in `agent_runner.jl:217+`), scoped to the new profile dir. Selection persists
per browser tab. `legacy` (D10) and any `retired` (D11) profiles are shown but not selectable.
**Do not** ship without P1's audit passing on the target machine, and not before P5 (single-
instance lock removes the cross-tab race the picker would otherwise have to design around).

### P4 — Attribution logging (D8)
Extend the Kiwi turn log record with `{profile, tokens_in, tokens_out, tool_calls, turn_id}` —
one field addition on the existing record, no new store. Amend `KIWI_ASSISTANT_PLAN.md` Decision
2 in the same PR (one line: "no per-user attribution system" → "attribution via the active
profile; see LOGIN_CREDENTIAL_ISOLATION_PLAN D8"). Downstream consumers explicitly out of scope
per D8 — do not add readers speculatively.

### P5 — Single-instance lock (D7) — SHIPPED
Landed on branch `feat/kiwi-single-instance-lock`. Lock file at `<config_dir()>/cecelia.lock`
holds `{pid, startedAt, host, api_port}`; the check runs at the top of `start()` in
`api/src/server.jl` — BEFORE `_BOUND_HOST[]`, before any `_install_log_tee!` / `_start_runner!`
/ `HTTP.listen`, so a remote user sees the friendly one-liner instead of a bind traceback.
Refuses with `Cecelia is already running on this machine (PID N since T on port P). Use
\`pixi run stop\` to release it if you are certain nothing is using it.`; stale locks (dead PID
or unparseable JSON) self-heal silently. `AlreadyRunningError` is a typed exception with a
`showerror` that prints message-only, so `server.jl`'s catch prints the one-liner to stderr and
`exit(1)`s — non-42 exit, so dev.jl treats it as "stop", not "restart".
**Deviated from the plan text on one point**: no port-liveness cross-check. A port bind probe
against the recorded port would falsely refuse a launch on a shared machine where some other
service happens to occupy 8080; the PID-alive check is enough for the single-seat lab tool this
is aimed at, and a wrong-refuse is a worse failure mode than a wrong-reclaim (which fails
loudly at the next bind). Tests: `Single-instance lock` testset in `app/test/suite/config.jl`
(stale-detection + message-building + acquire/release round-trip + idempotence + refuse-on-live-
other-pid + reclaim-on-dead-pid). Manual: `pixi run dev` twice on the same box produces the
friendly error on the second (not run — no live check performed against a running server).

### P6 — Identity-scoped terminal launcher (D9)
"Open terminal (profile: X)" button in `KiwiCockpit.vue` that hits a new route which spawns
`$SHELL -i` (or the platform equivalent — Windows PowerShell / cmd) as a detached child with
`CLAUDE_CONFIG_DIR` set to the active profile's dir and the ambient-credential env vars scrubbed
per D6. Same one-line documentation on the button itself so a user opening a raw terminal
elsewhere knows they are opting out. Composes with the existing "Set up my terminal" MCP-
registration wire — same code path, one more entry point.

### P7 — Pre-identity migration (D10)
One-shot idempotent script that runs at the version bump shipping P4. Reserves the profile name
`legacy`, sets `profile = "legacy"` on every existing capture / thread / outcome-tag / Blackboard
entry / lab-log entry / Kiwi turn log record whose profile field is missing. Records completion
in `custom.toml` (`[ai].legacy_migration_completed = <version>`) so it never re-runs. Test: run
twice on a fixture, assert idempotence and no double-tagging.

## Interaction with `KIWI_ASSISTANT_PLAN.md`

- The pointer near Decision 2 to this plan already landed in #1201 — no change needed here.
- P4 rewrites the "no per-user attribution system" phrase in Decision 2 in the same PR as the
  schema extension. This planning PR does not touch that phrase — the wording change ships with
  the code that makes it true.

## What this replaces from the archived prompts

- `opus-audit-cecelia-login-credential-isolation.md` (2026-09-23) — the credential-isolation
  audit. Its "attribution section assumed a `cecelia_user`" premise was wrong (no such section
  existed); D8 is the honest yes on attribution.
- `opus-audit-single-instance-identity-gaps.md` (2026-09-24) — the follow-up. Its five asks land
  as D7 (single-instance), D8 (attribution re-surface — accepted), D9 (terminal escape hatch),
  D10 (pre-identity data), D11 (lifecycle).

## Not covered here (deliberately)

- OS-level user separation (per D4) — out of scope; noted so a future reader does not derive it
  from the multi-profile design and assume it was rejected.
- Cross-machine identity portability — no; profiles are local to `<config_dir()>` on one box.
- Real authentication on the Cecelia login (per D5) — no; it is a label.
- Downstream attribution consumers (per D8) — no readers built speculatively.
