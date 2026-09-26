# User profile — launch-time identity primitive — plan

**Status:** planning (2026-09-25) · branch `docs/appwide-settings-audit`. Derived from
[`docs/archive/opus-audit-appwide-settings-cleanup.md`](../archive/opus-audit-appwide-settings-cleanup.md).
Reframes that audit's three overlapping asks (per-profile settings field audit, preferences modal,
project ownership/filtering) around a **single primitive** — the launch-time user profile — and
folds in the design decision that the Kiwi picker in `KiwiCockpit.vue` is **dropped** when this
primitive lands, not reconciled with. Builds directly on
[`LOGIN_CREDENTIAL_ISOLATION_PLAN.md`](LOGIN_CREDENTIAL_ISOLATION_PLAN.md) — the per-profile
`<config_dir>/user-profiles/<name>/` machinery already exists and is what this plan promotes from
a Kiwi-scoped mechanism to the app-wide identity anchor.

## Goal

Give Cecelia **one** identity primitive, chosen at launch, that every above-project surface reads
from: per-profile settings, project list filtering, Kiwi `CLAUDE_CONFIG_DIR` + attribution, and any
future above-project state. "App-wide" throughout this plan means **per-profile, persists across
that profile's projects** — NOT one row shared identically by every profile on the shared OS login.
If Ben changes a setting, it must not change anything for other profiles on the same box.

Concretely: launch presents a profile picker (auto-skipped when only one profile exists, matching
the Kiwi picker's design). The chosen profile is the active-profile value for the session; Kiwi,
project filtering, and per-profile settings all read from it. The `KiwiCockpit.vue` profile row
is deleted in the same PR that ships the launch-time picker.

## What already exists (verified 2026-09-25)

- `<config_dir>/user-profiles/<name>/` layout, created by
  [`api/src/kiwi_profile_api.jl`](../../api/src/kiwi_profile_api.jl) (create / list / retire /
  info endpoints).
- Server-side active-profile: `kiwi_profile_name()` reads `[ai].profile` from `custom.toml`;
  `set_kiwi_profile!(name)` writes it and hot-reloads config
  ([`app/src/ai/agent_runner.jl`](../../app/src/ai/agent_runner.jl)).
- Every `claude` spawn already runs under the active profile's `CLAUDE_CONFIG_DIR` with ambient
  `ANTHROPIC_*` / `CLAUDE_CODE_OAUTH_TOKEN` scrubbed at `addenv` time.
- Frontend surface: `utils/profileApi.ts` (renamed from `utils/kiwiProfileApi.ts` in Phase 3),
  `components/profile/CreateProfileDialog.vue` (renamed + moved from
  `components/kiwi/KiwiCreateProfileDialog.vue`). The Kiwi cockpit's profile row was retired
  by Decision 4 below (Phase 6 shipped).
- `SettingsModule.vue` currently mixes one **Project** section (Name + Project ID only) with ten
  install/machine-wide sections (Interface, Software updates, Storage, Custom modules, Plugins,
  Data patches, System, Other MCP connections, Diagnostics, Developer, Debug console). The Task-1
  audit will confirm the balance, but the module already looks predominantly above-project.
- Frontend preferences (`stores/settings.ts`, ~687 lines: `taskListAutoFollow`,
  `autoRefreshOnTask`, `importPyramidAdvisor`, `viewProfile`, …) currently persist to
  localStorage — machine-scoped, not profile-scoped. These are candidates to move.

## Decisions

1. **The user profile is the parent primitive; Kiwi identity follows from it.** Reverses the
   direction implied by the audit prompt (*"reuse Kiwi's profile to filter projects"*). Kiwi is
   one consumer of the active profile, alongside per-profile settings and project filtering.
   Every consumer reads `kiwi_profile_name()` (renamed in Phase 3, see Decision 7); nothing gets
   its own profile store.

2. **The launch-time picker is the profile picker.** No separate "Kiwi login" step. When Cecelia
   starts and more than one profile exists, the first screen after backend-ready is a picker; the
   choice is written via `set_kiwi_profile!`. When only one profile exists, the picker is skipped
   (matching the existing Kiwi picker's auto-skip). No hotkey to change profile mid-session —
   change-of-profile requires reload, same as project switch, because half the app is already
   mounted against the wrong identity by then.

3. **The Kiwi picker in `KiwiCockpit.vue` is deleted, not reconciled.** The Profile row in
   `KiwiCockpit.vue` and the `CreateProfileDialog.vue` invocation from it are removed the same
   PR the launch picker ships. Profile *creation* moves into the launch picker (a "+ new profile"
   affordance) and, once the preferences modal lands (Phase 4), into a Profiles pane there. Kiwi
   itself may still *display* the active profile name (read-only reflection) but does not offer
   change. Rationale: two selectors for one primitive is the bug this whole plan closes; keeping
   Kiwi's dropdown "just for Kiwi" repeats the mistake the audit called out.

4. **"App-wide" = per-profile.** Every per-profile settings key lives under the profile's dir, not
   at `custom.toml` root. `custom.toml` stays as the install-wide store (pool limits, storage
   compressor, TLS, plugin registry, `[ai].profile` itself). New file:
   `<config_dir>/user-profiles/<name>/settings.toml`. Same TOML shape as `custom.toml`; the
   resolver reads the profile file first, falling back to `custom.toml`. Frontend preferences
   currently in localStorage move to this file via a new `/api/profile/settings` round-trip;
   localStorage is retained only for pre-login bootstrap (theme so the picker paints correctly).

5. **Project ownership model: shared, visible-to-a-set.** Rejects strict single-owner. Real lab
   collaboration means multiple people work the same dataset; a strict owner model forces
   re-assignment or workarounds and does not match usage. Model: each project's `ccid.json`
   grows an `owners: [profile-name, …]` field. Empty / missing = visible to all profiles (the
   pre-identity default; see Decision 9). Any listed profile sees the project. There is no
   distinction between "owner" and "collaborator" — presence is the grant.

6. **Load-screen default: filtered to the active profile, with a "Show all" toggle.** Matches the
   picker's own "invisible until more than one profile" principle: on a single-profile install
   nothing changes. On a multi-profile install the load screen shows only projects whose
   `owners` contains the active profile (or is empty). The Show-all toggle stays visible
   regardless of profile count so a person on a single-profile install who later adds a second
   profile does not lose visibility of pre-existing work.

7. **Rename `kiwi_profile_*` code identifiers to `active_profile_*` in Phase 3. On-disk directories
   renamed 2026-09-26 to symmetric names: `<config_dir>/user-profiles/` (identities) and
   `<config_dir>/view-profiles/` (view profiles).** The original decision kept the identity dir at
   `kiwi-profiles/` because `<config_dir>/profiles/` was taken by View Profiles and moving either
   would collide. Amended after neither surface hit prod: with no installs to migrate, both dirs
   were renamed together in one hard cutover — no fallback shim, no read-time compat. Code identifier
   renames stayed as planned: `kiwi_profile_name()` → `active_profile_name()`, `set_kiwi_profile!`
   → `set_active_profile!`, `[ai].profile` → `[profile].active` (with a read-time fallback to
   `[ai].profile`), `utils/kiwiProfileApi.ts` → `utils/profileApi.ts`. Old identifiers retained
   as one-line deprecation shims through Phase 3, removed in Phase 6. The `Kiwi` naming ratchet
   in `frontend/src/components/kiwi/` stays scoped to Kiwi UI.

8. **Migration: pre-identity projects default to `owners: []` (visible to all).** Same category
   of question as the pre-identity-data migration flagged in
   [`LOGIN_CREDENTIAL_ISOLATION_PLAN.md`](LOGIN_CREDENTIAL_ISOLATION_PLAN.md) D9 (pre-identity
   Kiwi turns → reserved `legacy` profile). Resolved consistently here: **no automatic
   assignment**, no first-load prompt. A project without owners is genuinely un-attributed and
   the Show-all toggle plus explicit "Claim project" affordance in project settings is the path
   to attribution. Reasons: (a) any automatic assignment guesses wrong for shared datasets; (b)
   a first-load prompt is a modal that fires once per pre-existing project and would be noise;
   (c) unowned == visible-to-all is the safe default (no one loses access).

9. **The per-profile settings field audit (Task 1 of the archived prompt) is Phase 1 of this plan
   and gates the rest.** It is not skipped and its result may still be *"close this out — nothing
   worth moving."* If the audit comes back with fewer than three candidate fields, Phases 4 and
   5 (preferences modal, migration of frontend settings) collapse into a one-line addition to
   `SettingsModule.vue` and this plan is promoted to closed. Explicit off-ramp — don't
   manufacture scope. Phases 2, 3, 6 still ship regardless because they are about the launch
   picker and Kiwi displacement, not the settings surface.

10. **Preferences-modal design is deferred to Phase 4 — two-pane Firefox/Thunderbird pattern is
    the strong default, not locked.** The prompt suggested it; adopt unless the field audit's
    per-category count makes a single-scroll page obviously right. Decide with the numbers, not
    ahead of them. This modal edits the *active* profile's settings only; it is not a shared
    machine panel and does not replace `SettingsModule.vue` (which stays as the install-scoped
    surface).

11. **Lifecycle verbs: retire + rename + delete, all three, distinct.** Amends
    [`LOGIN_CREDENTIAL_ISOLATION_PLAN.md`](LOGIN_CREDENTIAL_ISOLATION_PLAN.md) D11 (which locked
    "immutable-name lifecycle with retired marker" on the assumption that turn logs pin the
    name). Reality on this ship: a user asking "how do I rename or delete a profile" doesn't
    accept "you can only retire it." Adopt the project-panel convention (rename + delete +
    confirm) alongside retire, each with its own use case:
    - **Retire** — "I'm done for now but keep the record." Data + credentials stay on disk;
      profile becomes non-selectable. Turn logs still resolve. What D11 always meant.
    - **Rename** — "typo / rebrand." Renames the on-disk `user-profiles/<old>/` directory and
      updates `[ai].profile` if the profile was active. Past turn logs still reference the OLD
      name — that's a break from D11's dataRef pattern and is surfaced in the confirm dialog
      copy. Refuse if `default`, if the profile is retired, or if the target name collides.
    - **Delete** — "remove everything." `rm -rf user-profiles/<name>/` — credentials, settings,
      retired marker, all gone. Refuse if the profile is active (user must switch first) or
      `default`. Surfaced through the canonical `ConfirmDeleteButton` (arm→confirm). Past turn
      logs become orphaned — the confirm copy names this out loud.

## Phases

### Phase 1 — Field audit (gate)

Enumerate every field in `SettingsModule.vue`, `stores/settings.ts`, and any other
above-project settings source (data patches parameters excluded — those are project-scoped
by definition). For each, classify:

- **project-scoped** (stays in project settings / `ccid.json`),
- **per-profile** (candidate to move to `profiles/<name>/settings.toml`), or
- **install-wide** (stays in `custom.toml`).

Report back with the count in each bucket and a per-field justification. Report BEFORE any
migration. Land as `docs/audit/user-profile-field-audit.md`. If the per-profile bucket has
fewer than three entries, invoke Decision 9's off-ramp: skip Phases 4 and 5, ship only
Phases 2 / 3 / 6.

### Phase 2 — Launch-time picker

- New Vue route `AppProfilePicker.vue`, mounted from `App.vue` as the first-child-after-backend
  gate. Reuses `components/profile/CreateProfileDialog.vue` (renamed + moved from
  `components/kiwi/KiwiCreateProfileDialog.vue` in this phase).
- Uses existing `utils/kiwiProfileApi.ts` (renamed `utils/profileApi.ts` in Phase 3) — no new
  API surface for the picker itself. Sets `[profile].active` via existing endpoint.
- Auto-skip when `list().profiles.length === 1`.
- Show-all toggle NOT included here (project filter arrives in Phase 5); the picker only sets
  identity.

### Phase 3 — Rename (code identifiers only)

Backend rename `kiwi_profile_name()` → `active_profile_name()`; `kiwi_profile_dir()` →
`active_profile_dir()`; `set_kiwi_profile!` → `set_active_profile!`;
`_DEFAULT_KIWI_PROFILE` → `_DEFAULT_PROFILE`. Frontend `utils/kiwiProfileApi.ts` moves to
`utils/profileApi.ts`; exports rename `kiwiProfile*` → `profile*`. All call sites updated
in the same pass — no code shims, direct rename (per the *no backwards-compat hacks* rule).

**Not renamed in this phase:**
- **`[ai].profile` in `custom.toml` stays put.** A rename to `[profile].active` would need a
  read-fallback and Dominik's dev environment has multiple `cecelia-*` worktrees pointing at
  the same `custom.toml` — an older worktree reading a config written by a newer one would
  lose the active profile. Cosmetic-only; defer.
- **`<config_dir>/user-profiles/` on-disk directory stays put** (Decision 7 — `profiles/` is
  taken by View Profiles).
- **`turn_profile()` in `agent_runner.jl` stays put** — it's about a Kiwi turn record, not the
  identity primitive; still Kiwi-scoped.

Kiwi naming ratchet in `components/kiwi/` stays scoped to that directory. No behaviour change;
no data migration.

### Phase 4 — Per-profile settings store + preferences modal (conditional on Phase 1)

- Server: `<config_dir>/view-profiles/<name>/settings.toml`; TOML reader/writer analogous to
  `custom.toml`. Resolver: profile file first, `custom.toml` fallback.
- API: `GET/PATCH /api/profile/settings` returning the merged view + writing only the profile
  layer.
- Frontend: `stores/settings.ts` grows a profile-backed persistence path for the fields Phase 1
  classifies as per-profile. localStorage retained only for pre-picker bootstrap (theme).
- Preferences modal: `PreferencesModal.vue`. Layout decided from Phase 1's per-category count
  (Decision 10). Entry: settings-page pin + sidebar-footer button, both replacing the deleted
  Kiwi profile row's affordance.

### Phase 5 — Project ownership + load-screen filter (conditional on Phase 4)

- `ccid.json` grows `owners: [string]`. Same versioning discipline as
  other `ccid.json` fields ([`docs/OBJECTMODEL.md`](../OBJECTMODEL.md)). Empty/missing = the
  pre-identity default (Decision 8). A NEWLY created project is stamped with the active profile at
  create-time — the server already knows who's creating it, and the [] default only exists to cover
  the migration case where identity was never known. Without this, every fresh project needed a
  follow-up Claim to appear under Mine, which is what "why isn't my project mine?" surfaced.
- Load screen (`ProjectPicker` / `ImageTable`) filters by
  `owners.length === 0 || owners.includes(activeProfile)`; Show-all toggle overrides.
- Project settings section grows a **Claim** button (adds active profile to `owners`) and a
  **Share** field (add another profile to `owners`). No remove-owner in v1 — an explicit
  ownership audit belongs in the promoted permanent doc, not the first ship.
- No migration; pre-existing projects stay unowned (Decision 8).

### Phase 6 — Delete Kiwi picker; remove deprecation shims

- Delete the Profile row from `KiwiCockpit.vue`. Keep a **read-only** active-profile display
  (name + a "change profile" link that opens the preferences modal's Profiles pane, or the
  settings page's Profiles section on the off-ramp path).
- Delete `kiwi_profile_name` / `set_kiwi_profile!` / `[ai].profile` shims (`custom.toml` on
  upgrade already migrated in Phase 3).
- Promote the durable parts of this plan into `docs/ARCHITECTURE.md` → *Identity & profiles*
  and delete this file (or mark it history at the top).

## References

- Archived prompt: [`docs/archive/opus-audit-appwide-settings-cleanup.md`](../archive/opus-audit-appwide-settings-cleanup.md)
- Credential-isolation prior art: [`LOGIN_CREDENTIAL_ISOLATION_PLAN.md`](LOGIN_CREDENTIAL_ISOLATION_PLAN.md)
- Kiwi surface being retired: [`KIWI_PLAN.md`](KIWI_PLAN.md), [`KIWI_ASSISTANT_PLAN.md`](KIWI_ASSISTANT_PLAN.md)
- Related identity work: [`ONBOARDING_PLAN.md`](ONBOARDING_PLAN.md), [`VIEW_PROFILES_PLAN.md`](VIEW_PROFILES_PLAN.md)
  (note: **view profiles** are a *per-profile setting* — sidebar page curation — not the identity
  primitive here; Phase 1 confirms this is a per-profile field, not machine-wide).
