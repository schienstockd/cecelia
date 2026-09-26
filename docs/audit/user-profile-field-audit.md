# User-profile field audit — Phase 1 of USER_PROFILE_PLAN

**Date:** 2026-09-25 · **Branch:** `docs/appwide-settings-audit` · **Gate for:**
[`docs/todo/USER_PROFILE_PLAN.md`](../todo/USER_PROFILE_PLAN.md) Phase 1

Enumerates every above-project setting Cecelia currently exposes and classifies it as
**project-scoped** / **per-profile** / **per-machine** / **per-project-display-state (data)**.

Sources swept:
- `frontend/src/modules/SettingsModule.vue` — the settings page (11 sections, `Project` + 10 others).
- `frontend/src/stores/settings.ts` — the Pinia settings store (~60 keys, all in `localStorage`).
- `app/config.toml` and `custom.toml` — the Julia-side install config
  (`[dirs] [files] [images] [zarr] [tasks] [pools] [ai] [tls]`).

---

## Bottom line

**Off-ramp (Decision 9): NOT invoked.** The per-profile bucket has **~25 candidate fields**, well
above the "fewer than three" threshold. Proceed with Phases 4 and 5 as scoped.

Bucket totals (candidates only, excluding derived / obviously non-settings):

| Bucket | Count | Meaning |
|---|---|---|
| **project-scoped** | 2 | Project Name; Project ID (read-only). Correctly project-scoped today. |
| **per-profile** | ~25 | Working preferences that should follow the person, not the machine. |
| **per-machine** | ~12 | Hardware- or install-dependent (VRAM budget, ports, pool limits, TLS). |
| **per-project-display-state** | ~10 store bags | Per-image / per-set viewer state that should really travel *with the project*, not with the browser. **Not this plan's problem** but flagged — see the note at the bottom. |
| **install-wide, not per-user** | 6 | Compressor, layout, pools, plugins, MCP registry, `[ai].profile` itself. |

The `Project` section on the settings page is already tiny (Name + ID). Most of what looks like
"project settings" today is actually **install-wide** (Storage, Software updates, MCP, Diagnostics,
System, Developer) — the redundancy the archived prompt asked about is UI-mixing, not data
duplication across project rows. That confirms the audit's optimistic branch: the payoff is a
cleaner *settings page*, not a decluttered *projects table*.

---

## SettingsModule.vue — field by field

Sections in template order. Ports/service rows and read-only diagnostics are excluded from the
classification — they are status, not settings.

### Section: Project

| Field | Verdict | Notes |
|---|---|---|
| Project name | **project-scoped** | Correct today. |
| Project ID (uid) | **project-scoped** | Read-only; belongs to the project. |

### Section: Interface

| Field | Verdict | Notes |
|---|---|---|
| `taskListAutoFollow` | **per-profile** | Working style. Follows the person across projects and machines. |
| `autoRefreshOnTask` | **per-profile** | Working style. |
| `importPyramidAdvisor` | **per-profile** | Working style; also `feedback_dont_over_ask_commit` says this class is nudge-not-required. |
| `viewProfile` (sidebar curation) | **per-profile** | **Direct confirmation in the code**: comment at `stores/settings.ts:277-281` says *"PER USER, not per project"*. The profile *definitions* live in files under `<config_dir>/view-profiles/*.json`; only the *choice* lives in localStorage today — move the choice to `profiles/<name>/settings.toml`. |

### Section: Software updates

| Field | Verdict | Notes |
|---|---|---|
| Version (readonly) | — | Diagnostic, not a setting. |
| `preferDevChannel` | **per-profile** | Debatable — one person may prefer dev while another wants stable on the same box. Even so, an update actually flips the install for everyone. **Recommendation: per-profile for the toggle preference, but Apply Update remains an install-wide action** (with a caveat visible on the button when the two profiles disagree). |
| Apply / Revert update | — | Action, not persisted state. |

### Section: Storage

| Field | Verdict | Notes |
|---|---|---|
| Image compressor | **install-wide** | Server-side, applied to every store the backend writes. Machine-wide by definition. Stays in `custom.toml`. |
| Store layout | **install-wide** | Same rationale. |
| Keep previous version on reprocess (VN toggle) | **install-wide** | Writer semantics for every task; not a per-user preference. |
| Scan / Free up space / Prune | — | Actions on the open project's data — project-scoped in effect, not persisted state. |

### Section: Custom modules · Plugins

| Field | Verdict | Notes |
|---|---|---|
| Modules directory | **install-wide** | Machine path. |
| Install plugin / list / registry | **install-wide** | Plugins install into the shared config dir; running unsandboxed and reachable by every profile. |

### Section: Data patches

| Field | Verdict | Notes |
|---|---|---|
| Patch runs | — | Actions on the open project's data. Correctly project-scoped. |

### Section: System (service control)

| Field | Verdict | Notes |
|---|---|---|
| Restart / Quit / Worktree switch | — | Actions, not persisted state. |
| `tls.enabled` (HTTPS + HTTP/2 toggle) | **install-wide** | Binds a socket at startup; affects every user on the machine. |
| Notebooks / Task preview / Task runner service rows | — | Status + actions, not persisted state. |
| `runner.enabled` (Run tasks in a separate process) | **install-wide** | Process topology of the box. |
| Cecelia MCP registration | **install-wide** | Machine registration in `~/.claude.json`. |

### Section: Other MCP connections

| Field | Verdict | Notes |
|---|---|---|
| `hiddenMcpAccounts` (dismissed connector list) | **per-profile** | Currently keyed with a comment claiming *"Machine-wide and permanent, not per project"* — but that's because there was no profile concept. What Ben dismisses shouldn't stay hidden for Alice. Move to per-profile. |

### Section: Diagnostics · Developer · Debug console

| Field | Verdict | Notes |
|---|---|---|
| Refresh / Packages… / GpuDiagnostic | — | Read-only diagnostics. |
| Debug console toggle (`replToggle`) | **per-profile** | Personal working style; the loopback safety gate is independent of who's driving. |

---

## stores/settings.ts — field by field

Grouped by what the field is about. Every key is currently `localStorage`-scoped (machine + browser),
so today every classification below is a *change* from the status quo.

### Working preferences → **per-profile** (~15)

`taskListAutoFollow`, `tasksThisProjectOnly`, `tasksShowHistory`, `autoRefreshOnTask`,
`viewerAutoUpdate`, `preferDevChannel`, `importPyramidAdvisor`, `animationSyncViewer`,
`viewerAutoSaveLayerProps`, `viewerScaleBar`, `viewerTimestamp`, `viewerGrid`,
`viewerLandscape`, `viewerLandscapeLabels`, `viewProfile`.

### Personal overlay sizes → **per-profile** (~6)

`viewerScaleBarPx`, `viewerTimestampPx`, `viewerPointSize`, `viewerPointBorder`, `viewerTailLength`,
`viewerTailWidth`, `viewerLabelOpacity`, `viewerLabelContour`, `viewerPointZTol`, `viewerTrackZTol`,
`viewerGridDensity`. These are "how big do I like markers." Follow the person.

### Movie / animation preferences → **per-profile** (~6)

`moviesPlaybackRate`, `moviesZoom`, `moviesAutoplay`, `moviesEndMode`, `moviesShowDetails`,
`moviesChannelMode`, `animationSyncViewer`. Comment at `settings.ts:192-194` already argues these
are *"a viewing preference, not a project attribute"* — the same argument extends: they're MY
viewing preference, not this machine's.

### Layout state → **per-profile** (~7)

`sidebarCollapsed`, `rightPanelCollapsed`, `viewerWindowSideCollapsed`, `viewerPanelOpen`,
`labLogPanelOpen`, `correctionCockpitOpen`, `correctionCockpitMode`, `correctionCockpitValueName`,
`kiwiOpen`, `viewerSelectMode`. What panels I have open on my last session.

### Kiwi personal preferences → **per-profile** (3)

`kiwiReasoning`, `kiwiModel`, `hiddenMcpAccounts`, `labLogAutoContext`, `labLogShowNames`,
`tipsOnLaunch`, `tipsLastShown`, `captureAttachToKiwi`, `captureSendToPaired`. These
especially — "did I see today's tip" is meaningless shared across users, `kiwiModel` per
profile lets Alice run Sonnet while Ben runs Opus, and Alice's capture-to-Kiwi preference
shouldn't overwrite Ben's when they share a workstation.

### Renderer / hardware knobs → **per-machine** (~9)

`viewerSteps`, `viewerCompress`, `viewerFps`, `viewerLoop`, `viewerCacheFrames`,
`viewerVolumeLevel`, `viewerVolumeProjection`, `viewerAutoContrastPercent`, `viewerPlaneLevel`,
`viewerBricksMode`, `viewerBrickTier`, `viewerCacheMB`. These depend on **this GPU** and **this
network** — Alice's laptop VRAM budget isn't right for Ben's workstation. Stay `localStorage`.
Same argument for `viewerFps`/`viewerLoop`: playback smoothness is bottlenecked by the local
decoder, not by the person.

### Per-image / per-set bags → **per-project-display-state (data)**, NOT this plan

`_labelVisStore`, `_trackVisStore`, `_trackPopHiddenStore`, `_branchVisStore`,
`_imageVersionStore`, `_setPrefs` (colourBy, show3D, showGatedTracks, pointSize, pointBorder,
popVis, trackColorMode, trackSourceColour, colourByOverrides, movie, cropZ, cropT, batchMovie).

These are keyed by `imageUid` / `setUid`. They're not personal preferences — they describe
"how I set up this dataset's view last time." Currently in `localStorage`, which means (a)
they don't survive a browser wipe, (b) they don't travel with an export/import (the `.ccbundle` bundle format,
[`docs/JOBS.md`](../JOBS.md), misses these), and (c) two users on the same
machine share them. **The right home is inside
the project directory** (`<project>/settings/viewer-state.json` or similar), not the profile
dir. **Out of scope for this plan** — flagged so it isn't picked up by mistake when Phase 4
moves the profile-scoped keys. Belongs in a follow-up plan.

### Transient / not persisted → —

`labLogUnseen`, `labLogUnseenKind`, `labLogUnseenLevel`. Session-only; no home change needed.

---

## `custom.toml` sections

| Section | Verdict | Notes |
|---|---|---|
| `[dirs] projects` | **install-wide** | Machine path; onboarding writes it once. |
| `[dirs] bioformats2raw` `python` | **install-wide** | Machine binaries. |
| `[files]` | — | Code invariant. |
| `[images]` `[images.normalise]` | — | Global invariant, not user-facing. |
| `[zarr] imageCompressor` | **install-wide** | See above. |
| `[tasks] concurrentLimit` `workerThreads` | **install-wide** | Machine capacity. |
| `[pools]` cpu / gpu / io / network | **install-wide** | Hardware capacity. |
| `[ai] profile` | **install-wide** | This IS the pointer to the active profile — must stay in `custom.toml` by definition (there's nowhere else to put it before a profile is resolved). |
| `[ai] model` | **per-profile** | Redundant with `kiwiModel` in the store; consolidate on the profile-scoped one in Phase 4. |
| `[ai] agent_bin` | **install-wide** | Path to the `claude` CLI on this machine. |
| `[tls] enabled` | **install-wide** | Socket-binding decision. |

---

## Recommendations

1. **Proceed with Phase 4 as scoped in USER_PROFILE_PLAN.** ~25 fields is well above the off-ramp
   threshold. The two-pane preferences modal makes sense — a single-scroll page for ~25 controls
   grouped into 5+ categories (Interface, Overlays, Movies, Layout, Kiwi) would be a wall of
   toggles. Recommend the modal.
2. **Do NOT move the per-machine renderer knobs.** They stay in `localStorage`. A future
   improvement would key them by a *machine* identifier (`crypto.randomUUID` in `localStorage`)
   rather than by the browser origin, so a person carrying a laptop between browsers keeps their
   tuning — but that's independent and out of scope here.
3. **Do NOT move the per-image / per-set bags into the profile store.** They belong with the
   project data. Track that as a separate follow-up plan
   (`docs/todo/PROJECT_VIEWER_STATE_PLAN.md`, not created yet) — it's the same category of
   question as `PROJECT_EXPORT_IMPORT_PLAN` handles for `ccid.json`.
4. **Consolidate `[ai].model` and `kiwiModel` in Phase 4.** Two homes for the same choice today;
   the profile-scoped one wins.
5. **`SettingsModule.vue` becomes install-scoped after Phase 4.** With the per-profile fields
   moved out, the module page is: Project (name/id) + Storage + Software updates + Custom modules +
   Plugins + Data patches + System + MCP + Diagnostics + Developer. Rename the module page to
   "System" or leave as "Settings" — decide in Phase 4 with the modal wireframe.
6. **Migration path for the moved fields:** on first launch after Phase 4 ships, if
   `profiles/<active>/settings.toml` is missing, read the existing `localStorage` values into it
   as-is (one-shot bootstrap). No prompt — the person's current preferences become their profile's
   starting preferences.

## Flagged for later (not this plan)

- **Per-project viewer state → project directory.** ~10 store bags currently in `localStorage`,
  keyed by uid, that should live in `<project>/settings/`. Independent plan; the fix removes the
  browser-boundary bug where two users on the same machine share these bags. Not blocking any
  phase of USER_PROFILE_PLAN.
- **Rationalise the `preferDevChannel` UX.** If the toggle becomes per-profile but Apply is
  install-wide, the button needs to indicate "this will apply for everyone on this machine."
  Handle in Phase 4 during the modal design, not before.
