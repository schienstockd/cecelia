# Persistence audit — where settings and user input live, after user profiles shipped

**Date:** 2026-10-01 · **Branch:** `docs/persistence-audit` · **Follows:**
[`user-profile-field-audit.md`](user-profile-field-audit.md) (2026-09-25, written before the
profile bag existed — it covered `stores/settings.ts`, `SettingsModule.vue` and `custom.toml` only).

This pass covers **every** persistence site: all frontend `localStorage`/`sessionStorage` keys, every
backend writer under `<config_dir>` and every user-originated writer under the project directory.

Four homes, one question each:

| Home | Where | Belongs here when… |
|---|---|---|
| **install** | `<config_dir>/custom.toml`, `view-profiles/`, `modules/`, `models/` | it describes the machine or the install |
| **profile** | `<config_dir>/user-profiles/<name>/` (settings.toml, credentials, `.claude.json`) | it describes how *this person* works |
| **machine (browser)** | `localStorage` | it depends on this GPU / screen / browser |
| **project** | `<project>/…` | it describes the data, and should travel with an export |

---

## Bottom line

1. **The profile bag works, but only `stores/settings.ts` uses it.** ~45 `PROFILE_KEYS` round-trip
   through `settings.toml`. Roughly 40 other `localStorage` key families across ~20 files are
   shared by everyone who uses this browser — including typed input (Kiwi draft/prompt).
2. **Project-level user input carries no author.** Of ~20 writers of user-authored content, only
   project `owners` and Kiwi turns are stamped with the profile. The lab log writes the literal
   `"User"`.
3. **The active profile is install-wide** (`[ai].profile` in `custom.toml`). One switch changes
   identity for every tab on every client. The per-tab `X-Kiwi-Profile` handoff is still deferred.
4. **A few pieces of personal state live in the project, shared by everyone:** `lastOpenedAt`,
   dismissed lab-log entries, push pairing.

---

## 1. Frontend — `stores/settings.ts`

Matches the 09-25 audit except:

| Key | Today | Should be | Why |
|---|---|---|---|
| `hiddenMcpAccounts` | localStorage | **profile** | The 09-25 audit classified it per-profile; it never made it into `PROFILE_KEYS`. Ben's dismissals hide Alice's connectors. |
| `pickZMode`/`pickZWindow` (`cc.pickZScope`) | localStorage | **profile** | New since the audit. A working preference, like `viewerPointZTol`. |
| `tipsEverShown` | localStorage | machine | Intentional, per its comment ("the tour fired once on this box"). OK. |
| `[ai].model` vs `kiwiModel` | — | — | Resolved: no `[ai].model` in `custom.toml` any more; `kiwiModel` is the only home. |

**Mechanism caveat.** `PROFILE_KEYS` are still *also* written to localStorage, and on a profile's
first hydrate any missing key is **bootstrapped from localStorage**. So a newly created profile
inherits whoever last used this browser, not the defaults. The same applies if the
`/api/profile/settings` fetch fails. Fix: bootstrap from `localStorage` only for the `default`
profile (the migration case); for any other profile, missing key → default.

## 2. Frontend — everything outside `stores/settings.ts`

None of these go through the profile bag.

### Should be **profile**

| Key | File | What |
|---|---|---|
| `cc.kiwiDraft`, `cc.kiwiPrompt` | `stores/kiwi.ts:18` | **Typed, unsent user input.** Alice's half-written question appears in Ben's Kiwi box. The strongest case in this list. |
| `cc.guide.<id>.done` | `stores/guide.ts:35` | Completed guides. "Did I finish the tour" is per person. |
| `cc.hint.*` (HintCallout `_key`) | `components/HintCallout.vue:10` | Dismissed first-use hints. Same argument. |
| `cc-fn:<module>` | `tasks/TaskRunner.vue:452`, `composables/useKiwiPoint.ts` | Last-picked task function per module. |
| `cc-filters-open`, `cc-task-open`, `cc-proc-fun`, `cc-proc-mode`, row filters | `components/ModuleLayout.vue` | Module-page layout + filter choices. |
| `cc.movies.starredOnly`, `cc.movies.tags`, `cc.movies.attrFilter`, `cc.movies.filtersOpen`, sort | `modules/MoviesModule.vue:263` | Movie list filters. (The stars/tags themselves are project data, correctly in `settings/movies.json`.) |
| `cc.blackboard.statusFilter`, `.outcomeFilter` | `modules/BlackboardModule.vue:95` | List filters. |
| `cc.vw.gizmo`, `.overview`, `.tilemap`, `.brickmap` | `modules/ViewerWindow.vue` | Viewer overlay toggles — same class as `viewerScaleBar`, which IS in the bag. |
| `cc.sidebar.openGroup`, `cc.consoleGroups`, CollapsibleSection / useSingleOpenSection / PlotOptions / CanvasSidePanel / CleanupModule open-state keys | various | Panel open/closed — same class as `sidebarCollapsed`, which IS in the bag. |
| SelectionTable sort keys | `components/SelectionTable.vue` | Table sort. |

### Should stay **machine**

| Key | File | Why |
|---|---|---|
| `cc.floating.<id>` | `components/FloatingPanel.vue:43` | Panel geometry depends on the screen. |
| `useColumnResize` widths | `composables/useColumnResize.ts` | Screen width. Debatable; leave. |

### Not settings — cross-window channels (leave as they are)

`cc.openProject`, `cc.uiLog`/`cc.uiLogRing`, `cc.viewerFocus`, `cc.viewerOverlaysTick`,
`cc.pickSelectionTick`, `cc.gatingCurrent`, `cc.viewerSelectMode` (also in the bag),
sessionStorage `_JUST_PICKED_KEY`. These use `storage` events as a popout bridge. They don't need a
profile home.

### Should be **project** (already flagged 09-25, still in localStorage)

The per-image/per-set bags in `settings.ts` (`viewerSetPrefs`, `viewerLabelVisibility`,
`viewerTrackVisibility`, `viewerTrackPopHidden`, `viewerBranchVisibility`, `viewerImageVersion`),
plus `cc.gate.lastVn.<popType>.<imageUid>` (`modules/gate/GatingPlots.vue`). Keyed by uid; they
don't survive a browser wipe or an export. The proposed follow-up `PROJECT_VIEWER_STATE_PLAN.md`
still hasn't been created.

**A cheap way to fix §2:** one `useProfilePref(key, default)` composable that wraps the
settings-store bag (an open-keyed sub-bag, e.g. `ui.<key>`). Each call site then changes in one
line, instead of every key being added to the 3 hand-maintained lists in `settings.ts` (the
`PROFILE_KEYS` array, `_profileRefs` and the return). That's ~20 files.

## 3. Backend — install-wide (`<config_dir>`)

| Writer | Path | Verdict |
|---|---|---|
| `config.jl:208` `set_projects_dir!` | `custom.toml [dirs]` | install ✓ |
| `throttle.jl`, `image_format.jl`, `pools.jl`, `tls.jl` | `custom.toml [tasks] [runner] [zarr] [pools] [tls]` | install ✓ |
| `agent_runner.jl:316` `set_active_profile!` | `custom.toml [ai].profile` | install today — **see gap B** |
| `profile_settings.jl:88` | `user-profiles/<name>/settings.toml` | profile ✓ |
| `kiwi_profile_api.jl` | `user-profiles/<name>/` create/rename/retire/delete | profile ✓ |
| `agent_runner.jl:597` `register_observer_mcp` | `<CLAUDE_CONFIG_DIR>/.claude.json` | profile ✓ (via the active profile's dir) |
| `view_profiles.jl:114` | `view-profiles/<id>.json` | install library ✓; the *choice* is per profile ✓ |
| `plugins.jl:567` | `modules/plugins/<name>/` | install ✓ (no installer recorded) |
| denoise / optical-flow vault | `models/*Models/` | install ✓ (no trainer recorded) |
| `observer_api.jl:35`, `runner/server.jl:251`, `single_instance.jl` | `observer-mcp*.json`, `runner.json`, `cecelia.lock` | process state, n/a |

**Stale dirs on the dev box:** `~/cecelia-feijoa/dev/kiwi-profiles/` (pre-Phase-3 name) and
`~/cecelia-feijoa/dev/profiles/` (pre-rename view-profiles) are both still present and both differ
from their successors. No code reads either. Nothing to change in code; delete by hand after
checking.

## 4. Backend — project directory

Verdict column: **data** = correct where it is; **+author** = correct place, but should record the
profile; **personal** = per-person state stored for everyone.

| Writer | Path | What | Verdict |
|---|---|---|---|
| `routes/project.jl:46/403/427` | `project.json owners` | ownership | data ✓ — stamped |
| `kiwi_api.jl:42` | `kiwi/turns.json` | Kiwi turns | data ✓ — stamped `profile` (but see gap D) |
| `routes/project.jl:75` | `project.json lastOpenedAt` | the only "recent projects" signal | **personal** |
| `lab_log.jl:195` | `settings/lab-log-dismissed.json` | hidden lab-log entries | **personal** |
| `push_api.jl:108` | `settings/push_target.json` | Claude-session pairing; one per project | **personal** — a second profile's pairing silently replaces the first's |
| `lab_log.jl:81` | `lab-log.md` | entries headed `## date [Author]` | **+author** — the GUI sends the literal `"User"` (`utils/labLog.ts:57`) |
| `blackboard_api.jl` | `blackboard/<id>/…` | entries, revisions, status, outcome | **+author** |
| `notebooks_api.jl` | `notebooks/*.jl`, `settings/notebooks.json` | notebooks | **+author** |
| `routes/metadata.jl:266` | `ccid.json note/starred/included` | image notes, stars | **+author** for notes |
| `routes/chain.jl`, `chain/persistence.jl` | `settings/chains/` | chain templates + runs | **+author** (doesn't even say GUI vs MCP) |
| `routes/project.jl:165` | `settings/analysisBoards.json` | boards | +author (low priority) |
| `captures_api.jl:289` | `captures/<id>/meta.json` | captures | +author (low priority) |
| `correction_plan.jl:571`, `track_correction.jl:383`, `label_correction.jl:236` | `plan.json`, `corrections/*.json` | cockpit plans + edit journal | **+author** — a correction journal is exactly where "who did this" matters |
| `run_log.jl:99` | `runlog.json` | task provenance | **+author** — who launched it |
| `labarchives.jl:121` | `settings/labarchives.json` | ELN context cache | **+author** — `syncedBy = "claude"` though ELN access is per user |
| gating persistence, `metadata.jl` attrs/channels, `set.jl`, `movies_api.jl`, animations, `ccid.json funParams` | various | gates, attributes, sets, movie stars/tags, last-used params | data ✓ — shared by design |
| `routes/project.jl:340`, `viewer_api.jl:805` | `moduleCanvases.json`, `data/<zarr>.json` | canvas layout, viewer layer props | data ✓ — "how this dataset looks" travels with it |

## Gaps, ranked

- **A. Kiwi draft/prompt in localStorage.** Typed input crosses people. Small fix: two keys.
- **B. Active profile is install-wide.** Fine for one person per machine. With two concurrent
  users (two browsers on a shared workstation, or remote access), whoever switched last wins on
  every tab, and settings PATCHes land in that profile's file. This is the deferred
  `X-Kiwi-Profile` work; it should be prioritised before anything that assumes concurrency.
- **C. No author on project-level user content.** Plumbing `active_profile_name()` into the
  lab-log author is one site. Blackboard/notebook/correction/runlog metadata each take an
  `author` field. Display is a separate step.
- **D. Kiwi follow-up after a switch.** `turns.json` is per project, but each `sessionId` lives in
  the profile's `CLAUDE_CONFIG_DIR`. A follow-up on another profile's turn hits the stale-session
  self-heal and silently starts a fresh conversation. `turn_profile()` exists
  (`agent_runner.jl:294`) but nothing calls it.
- **E. Rename/delete doesn't update references.** Project `owners` and turn `profile` stamps keep
  the old name (`kiwi_profile_api.jl`). Imported projects keep the exporter's owners
  (`project_io.jl:377`).
- **F. New profile bootstraps from the previous user's localStorage** (§1 caveat).
- **G. ~30 per-person UI key families outside the bag** (§2). One composable fixes the class.
- **H. Personal state in the project** — `lastOpenedAt`, lab-log dismissals, push pairing.
  `lastOpenedAt` matters most: `list_projects` "most recent first" is the *install's* most recent,
  not yours. Move to the profile, keyed by project uid; leave project.json's value as a fallback.
- **I. Per-image viewer bags still in localStorage** (§2, carried over from 09-25).

---

## Status — fixed on this branch

| Gap | Fix |
|---|---|
| **A** Kiwi draft/prompt | `stores/kiwi.ts` → `profileStorage` |
| **F** new profile inherits the last user | `adoptProfileBag` (`utils/profileStorage.ts`): the mirror records its owner; a different profile clears it (settings-store mirror included) and reloads, so unset keys start from defaults. A browser with no owner yet keeps its values (migration). |
| **G** per-person keys outside the bag | `utils/profileStorage.ts` — localStorage-shaped, mirrored to `ls:<key>` in settings.toml. Moved: Kiwi draft/prompt, guides, hints, `cc-fn:*`, ModuleLayout filters/panels, Movies + Blackboard filters, `cc.vw.*`, sidebar group, console groups, collapsible/accordion/plot-options/cleanup-plan open state, SelectionTable sort, `hiddenMcpAccounts`, `pickZScope`. Server PATCH is now lock-serialised. |
| **C** (lab log) | `[User · <profile>]` stamped server-side (`lab_log_user_author`); `default` unchanged. |
| **C** (everything else) | `author_stamp()` = `{profile, via}` — `via` is `claude` when the request carries `X-Cecelia-Client: claude` (the MCP client sends it; the router binds `REQUEST_VIA`). Blackboard `createdBy`/`updatedBy`/`outcome.taggedBy`, notebook registry `createdBy`/`updatedBy`, chain templates `createdBy` (from disk, never the body)/`updatedBy`. Task + chain requests carry `by` (set by the asking side, since the runner's active profile can be stale) → `TaskRecord.by` → run log `by`, chain `run.json` `by`, and `_by` beside `_task_id` for the correction journals (which now also get their `runId`). Every edit restamps `updatedBy` (incl. a direct notebook snapshot and a chain rename); a resumed chain records `resumed_by` and its nodes run as the resumer. Shown on the blackboard header, the notebooks table ("By", only once someone is named), the chain palette, the chain run picker and the task detail ("by alice", "by Claude for alice"); the default profile names nobody. Correction journals have no screen — their `runId` points at the run log entry, which carries `by`. |
| **D** cross-profile Kiwi follow-up | 409 *"that conversation was alice's — ask it fresh"* instead of a silent restart. |
| **E** rename/delete/import owners | `rewrite_project_owners!` on rename (carried over) and delete (dropped → visible to all); import keeps only owners that exist here, else the importer (`import_owners`). |
| **H** `lastOpenedAt`, lab-log hides | Per-profile `recent-projects.toml` orders the project list; lab-log dismissals keyed by profile (legacy flat list stays everyone's start). |

**Still open:** push pairing (one per project); `labarchives.json syncedBy` (the MCP caller, not
the active profile, is the honest author — needs the pairing to say who); `plan.json` (a recomputed
recommendation, not authored).

**Won't fix:** B (install-wide active profile) — one person uses a machine at a time, so the active
profile IS the person; per-tab profiles are cost without a user. I (per-image viewer bags) — camera
and layer layout belong to the machine and screen.
