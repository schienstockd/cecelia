# GUI clarity audit, P0 step 1: surface census (code-only)

Worktree `cecelia-gui-clarity-audit-archive`, read 2026-11-10. No app run, no screenshots: every number
below is a **static estimate from the templates**. Paths are relative to `frontend/src/` unless noted.

## Method (how to read the numbers)

**Counting script:** `count.mjs` in this scratchpad (it uses `@vue/compiler-sfc` from
`frontend/node_modules`). It walks each SFC's template AST and counts these as controls:
- native `button`, `a`, `RouterLink`, `input` (checkbox/radio = toggle), `select`, `textarea`
- `role="tab"`, `TabbedCanvas`
- any component named `*Button|*Toggle|*Select|*Picker|*Input|*Slider` (CcToggle, ChipSelect,
  SwatchSelect, ConfirmButton, AcqCheckButton, ...), counted as 1 and not recursed into
- native elements that carry an `@click` handler (clickable rows), shown as "cli"

**First paint vs after expansion.** A control counts as "after expansion" when it sits under any of these:
- a `v-if`/`v-show` whose expression looks like a UI toggle (`open|expand|show|advanced|edit|menu|
  popover|dialog|confirm|hover|maximi|running|error|…`, or `=== 'literal'`, which is a tab/mode switch)
- an empty-state branch (`v-if="!x"`, or a `v-else` after a visible branch)
- `BaseModal`, `TeleportPopover`, `FloatingPanel` or `<details>`
- a `CollapsibleSection` that is collapsed by default (`:default-open="false"`, or `accordion` without `default-open`)

Data-presence conditions (`v-if="activeSet"`, `v-if="rows.length"`) count as **visible**. The census
assumes a project is open and a set is selected.

**Where I stopped:**
- **"1-level":** the page plus every child component it imports. The shared shell (`ModuleLayout`,
  `ModulePage`) is treated as transparent, so its own children (SetBar, ImageTable, HintCallout,
  AttrFilterPanel, PanelLaunchers) count as the page's first level. This is the headline number.
- **"2-level":** one more level of children: TaskRunner's ParamRenderer, SummaryCanvas's SummaryPanel,
  GatingPlots' canvas, and so on. Treat it as an **upper bound**. ParamRenderer's per-param-type
  branches are all counted once, even though a given task shows only some of them.
- Overlays (modals and panels) were counted with their own `BaseModal`/`FloatingPanel` root treated as
  open. BaseModal's built-in × button is not counted (add 1 per modal).

**Systematic biases:**
- A `v-for` control counts **once**. The sidebar's 22 nav links count as 1, Preferences' 8 category
  buttons as 1, and ModuleLayout's row filters as 1. Real counts on list-heavy surfaces are higher.
- The number of plot panels, task parameters, chain nodes and board tiles depends on the user's data
  and task JSON, so none of them can be counted statically (see *Could not count*).
- Sections gated by `appCtl.canManageMachine` count as visible. The Settings and Preferences numbers
  are therefore the peanut (`default`) profile's view.

**Depth** means clicks from the welcome page (`/`). `WelcomeModule.vue` is decorative: a lockup and no
controls. So every path starts in the header or the sidebar.
- The sidebar is a **single-open accordion** (`AppSidebar.vue:58-78`, `useSingleOpenSection`). On a
  fresh profile only the first group, **Data**, is open.
- So **a Data page is 1 click, and any other group's page is 2** (heading, then item). The accordion
  persists per profile and follows navigation, so a returning user can be 1 click from any page.
- With **no project open**, add 2 clicks: "Open project" (`AppSidebar` → `ProjectPanel`), then a row.
  The nav items show a lock (`AppSidebar.vue:98-103`).
- Overlay depth = host depth + clicks to open, traced from the mount site.

**Global chrome**, present on every non-bare page and **not** included in the page counts below:
- `AppHeader.vue` has about 13 controls: menu toggle, logo (What's New), compass (Guides), key (icon
  legend), palette (colour legend), megaphone (Call for datasets), GitHub, chat, maturity dot plus
  "Something wrong?", and the profile chip. Two more appear when an update badge shows.
- `AppSidebar.vue` has about 19 on a fresh profile: project ⋯, 5 group headings, 6 Data items, 4 panel
  launchers (`PanelLaunchers.vue`), Settings, Quit and Log out.
- **That is about 32 chrome controls on every page, before the page itself.**

**Entry-point key:**
- **S** sidebar
- **M** guide map tile (`GuideMap.vue:55` `go()`; MAP covers all 22 nav pages, `lib/guides/mapData.ts`)
- **K** Kiwi route action (`utils/kiwiTurn.ts` `TASK_PAGES`/refs, `composables/useKiwiPoint.ts`)
- **R** capture/Blackboard reshow (`utils/moduleRoute.ts` via `composables/useCaptureFocus.ts`)
- **C** Claude MCP navigate. Only `/analysis`: `mcp/cecelia_mcp/server.py:2118` → `viewer:navigate`
  → `stores/ws.ts:422`. `point_at_ui` points at anchors but never navigates.
- **L** an in-app link or button elsewhere (named in the cell)
- No keyboard shortcut navigates anywhere: no keydown handler in App, AppHeader or AppSidebar, and
  the only window keydowns are plot-fullscreen Esc and canvas zoom.

**Maturity:** computed by running `utils/maturity.ts` `pageTier()` over NAV_GROUPS.
- "Badge" means `AppHeader.vue:135` `MaturityIndicator`, which shows a tier dot plus "Something wrong?"
  for every **non-exempt** tier, Beta included.
- Exempt pages, Settings and bare windows show nothing.

## 1. Routes (31 records in `main.ts:36-80`, plus the `/optical-flow` redirect)

| Route (label) | Top-level SFC | Shell | Depth | First / total, 1-level | 2-level total | Entry points | Tier · badge | AI elements on it | Notes |
|---|---|---|---|---|---|---|---|---|---|
| `/` Welcome | `modules/WelcomeModule.vue` | bespoke | 0 | 0 / 0 | 0 | default route | none · no | none | decorative only, no next step |
| `/manage-images` | `modules/ManageImagesModule.vue` | ModuleLayout | 1 | 45 / 112 | 137 | 5: S, M, K, L ImageTable empty-state CTA (`ImageTable.vue:554`), L Setup finish (`SetupModule.vue:59`) | Beta · dot | AddToKiwi in ImageTable ⋯ (ai-gated) | mounts 4 modals itself (FileBrowser, LegacyMigrate, ImmunemapImport, SeriesPicker) and ImageFileActions → Copy and Delete dialogs |
| `/preprocess` | `modules/PreprocessingModule.vue` | ModuleLayout | 1 | 40 / 69 | 89 | 3: S, M, K | Experimental · dot | AddToKiwi in TaskRunner | |
| `/metadata` | `modules/MetadataModule.vue` → `metadata/MetadataPanel.vue` | ModuleLayout | 1 | 36 / 76 | 87 | **2: S, M** | Beta · dot | none | |
| `/cleanup` | `modules/CleanupModule.vue` | ModuleLayout | 1 | 40 / 69 | 89 | 3: S, M, K | Beta · dot | TaskRunner AddToKiwi | |
| `/model-training` | `modules/ModelTrainingModule.vue` | ModuleLayout | 1 | 42 / 71 | 113 | 3: S, M, K (+ `/optical-flow` redirect) | Experimental · dot | "by Claude" in model vault | ModelVault details modals at depth 3–4 |
| `/segment` | `modules/SegmentModule.vue` | ModuleLayout | 1 | 46 / 78 | 142 | 3: S, M, K | Beta · dot | TaskRunner AddToKiwi; SummaryPanel ClaimStrip | |
| `/gate` | `modules/GatingModule.vue` → `gate/GatingPlots.vue` | ModuleLayout | 2 | 34 / 64 | 162 | 3: S, M, L Chain manual-step "open" (`ChainModule.vue:549-562`, only `gate.manual_gate` has `manual.open`) | Beta · dot | proposed-gate ✦ in PopulationManager; plot point-out marks | the 2-level number is mostly the canvas and pop manager |
| `/track` | `modules/TrackingModule.vue` | ModuleLayout | 2 | **54** / 84 | **202** | 3: S, M, K | Beta · dot | TaskRunner AddToKiwi; TrackDiagnostics Claude chip | TaskRunner + GatingPlots on one page |
| `/clust-cells` | `modules/ClusterCellsModule.vue` | ModuleLayout | 2 | 46 / 75 | 163 | 4: S, M, K, R | Beta · dot | ClusterMapView point-out marks | |
| `/clust-tracks` | `modules/ClusterTracksModule.vue` | ModuleLayout | 2 | 46 / 75 | 163 | 4: S, M, K, R | Beta · dot | same | |
| `/regions` | `modules/RegionClusteringModule.vue` | ModuleLayout | 2 | 46 / 75 | 163 | 3: S, M, K | Experimental · dot | same | |
| `/phenotype` | `modules/PhenotypeModule.vue` | ModuleLayout | 2 | 26 / 58 | 102 | 3: S, M, R | Experimental · dot (unguided record) | ClaimStrip | |
| `/behaviour` | `modules/BehaviourModule.vue` | ModuleLayout | 2 | 46 / 79 | 143 | 4: S, M, K, R | Experimental · dot | TaskRunner AddToKiwi; ClaimStrip | |
| `/spatial` | `modules/SpatialAnalysisModule.vue` | ModuleLayout | 2 | 46 / 78 | 142 | 3: S, M, K | Experimental · dot | ClaimStrip | |
| `/analysis` Analysis board | `modules/AnalysisModule.vue` → `canvas/TabbedCanvas.vue` | ModuleLayout | 2 | 21 / 50 | 50 | **6**: S, M, C, K (`useKiwiPoint.ts:118`), R, L "send to board" (`SummaryPanel`, `stores/canvasPanels.ts`, `utils/boardNav.ts`) | Beta · dot | CanvasPanel AddToKiwi; board record modal | board content is user-built (see *Could not count*) |
| `/blackboard` | `modules/BlackboardModule.vue` (1,149 lines) | **bespoke** | 2 | 6 / 46 | 46 | 4: S, M, K (ref + KiwiAsk toast, `KiwiAsk.vue:63`), L ClaimStrip (`ClaimStrip.vue:103`) | exempt · no | ✦ agent runs, "by Claude", SectionVerdict, AddToKiwi | first paint is light; per-entry review controls are gated |
| `/notebooks` | `modules/NotebooksModule.vue` | ModulePage | 2 | 13 / 22 | 22 | **2: S, M** | Experimental · dot | "by Claude" in NotebookTable | |
| `/animation` | `modules/AnimationModule.vue` → `animation/*` | ModuleLayout | 2 | 34 / 64 | 90 | 3: S, M, L Movies "restore" (`utils/movieRestore.ts:48`) | Beta · dot | none | |
| `/batch-movies` | `modules/BatchMoviesModule.vue` → `batchmovies/BatchMoviesPanel.vue` | ModuleLayout | 2 | 27 / 63 | 97 | 3: S, M, L Movies restore | Beta · dot | none | |
| `/movies` Movies & figures | `modules/MoviesModule.vue` (959 lines) | ModulePage + CollapsiblePanel | 2 | 22 / 47 | 50 | **2: S, M** | exempt · no | none | owns an inline BaseModal |
| `/tasks` | `modules/TasksModule.vue` (669 lines) | **bespoke** | 2 | 12 / 22 | 22 | 4: S, M, L TaskList "all tasks" (`TaskList.vue:119`), L task rerun (`utils/taskRerun.ts`) | exempt · no | none | |
| `/chain` Chains | `modules/ChainModule.vue` (2,441 lines) | **bespoke** | 2 | **55** / **101** | 167 | **2: S, M** | Beta · dot | "by Claude" (`ChainModule.vue:647`) | inline BaseModal(s), ChainFromHistoryModal, ParamRenderer, 2 PoolThrottle |
| `/custom/:category` | `modules/CustomModule.vue` | ModuleLayout | 2 (Custom group, only when such a category exists) | 46 / 78 | 142 | 2: S, K | untiered route · no | TaskRunner AddToKiwi | |
| `/settings` | `modules/SettingsModule.vue` (1,763 lines) | **bespoke** | 1 (sidebar footer `AppSidebar.vue:189`) | **70** / 99 | 109 | 1: S footer (guides may point at it) | none · no | Shared workstation (AI machine switch), Other MCP connections | 16 always-open `<section>`s in one scroll, no CollapsibleSection; highest first paint of any surface |
| `/console` (bare) | `modules/ConsoleView.vue` → ErrorConsole | bare | 1–2 (ErrorConsole pop-out, `ErrorConsole.vue:94`) | 9 / 11 | 11 | 1: L | none | none | |
| `/tasks-window` (bare) | `modules/TasksView.vue` → TasksModule | bare | 3 (Tasks page, then pop-out) | 12 / 17 | 22 | 1: L TasksModule pop-out | none | none | |
| `/viewer-window` (bare) | `modules/ViewerWindow.vue` (**7,437 lines**) | bare + CollapsiblePanel | 2 (ImageTable eye on a Data page, or Viewer launcher then pop-out) | 37 / **118** | 118 | **7**: L ImageTable eye (`ImageTable.vue:387`), ViewerPanel pop-out (`:457`), ParamRenderer image picker (`:249`), Movies source (`MoviesModule.vue:483`), ImageStripView zoom-to-source, `showTracksInViewer.ts:56`, `stores/shareTarget.ts` | none · no | AddToKiwi (`:268`), tile marks, freeform marks, capture-to-Claude (CaptureViewSurface/DrawSurface) | highest total of any surface |
| `/board-render` (bare) | `modules/BoardRenderView.vue` | bare | not navigable (agent headless render) | 14 / 19 | 123 | 0 by a user | — | — | exclude from UX ranking |
| `/setup` (bare) | `modules/SetupModule.vue` | bare | 0 (boot guard, first launch only) | 2 / 2 | 2 | 1: automatic | — | none | |
| `/profile-picker` (bare) | `modules/AppProfilePicker.vue` | bare | 0 (launch, when >1 profile) | 2 / 6 | 6 | 2: boot, `stores/appControl.ts` | — | CreateProfileDialog registers the MCP | |

## 2. Modals and dialogs (33 files mount `BaseModal`; no bespoke overlay besides GuideBubble)

The canonical shell is `BaseModal.vue` (`docs/inventory/FRONTEND.md` → *Dialog/panel shells*). Counts
are 1-level with the modal treated as open.

| Modal | File | Opened from (mount site) | Depth | First / total | Entry pts | AI | Notes |
|---|---|---|---|---|---|---|---|
| Project panel | `components/ProjectPanel.vue` | sidebar ⋯ / "Open project" (`AppSidebar`) | 1 | 12 / 37 | 1 | — | gateway when no project is open |
| Preferences | `components/PreferencesModal.vue` | header profile chip, `openPreferences()` (HpcSettings, Kiwi) | 1 (+1 per pane) | 1 / **87** (8 panes, nav v-for counted once) | 3 | Kiwi pane (AI toggle, plugin row) | the largest overlay; tier-less settings in a modal next to a Settings *page* |
| Guides (+ map) | `components/GuidesDialog.vue` + `GuideMap.vue` | header compass, maturity dot (`openMaturityRow`), GuideBubble | 1 | 7 / 9 (+ map 6/9) | 3 | ✦ AI help (ai-gated), ClaudeWaysIn | carries the maturity dots and recipes |
| What's New + tips | `components/WhatsNewDialog.vue` | header logo, Settings, update badge | 1 | 14 / 16 | 3 | tips filtered by `ai.enabled` | |
| Icon legend | `components/IconLegendDialog.vue` | header key | 1 | 1 / 1 | 1 | lists ✦ glyph | |
| Colour legend | `components/ColorLegendDialog.vue` | header palette | 1 | 1 / 1 | 1 | — | |
| Call for datasets | `components/CallForDatasetsModal.vue` | header megaphone, MaturityMap, param figures | 1 | 1 / 3 | 3 | — | |
| AI opt-in | `components/AiOptInDialog.vue` | Kiwi "Turn on…" (`KiwiCockpit.vue:330`), Preferences → Kiwi toggle | 2–3 | 3 / 3 | 2 | is the AI consent | |
| Create profile | `components/profile/CreateProfileDialog.vue` | Preferences → Profiles; AppProfilePicker | 2–3 | 1 / 4 | 2 | MCP registration | |
| Assistant overview (`?`) | `components/ClaudeOverviewDialog.vue` (imported as `AssistantOverviewDialog`) | Kiwi `?` | 2 | 0 / 1 (+ ClaudeWaysIn sections) | 1 | yes | |
| View-profile editor | `components/ViewProfileEditor.vue` | Settings | 2 | 6 / 8 | 1 | — | |
| Packages | `components/PackagesDialog.vue` | Settings | 2 | 2 / 2 | 1 | — | |
| File browser | `components/FileBrowser.vue` | ProjectPanel, ManageImages, ParamRenderer dirPath, HpcSettings | 2–4 | 7 / 10 | 4 | — | |
| Image file actions | `components/ImageFileActions.vue` (embeds its own modal) | ManageImages controls | 2 | 7 / 12 | 1 | — | |
| Copy images | `components/CopyDialog.vue` | ImageFileActions | 2–3 | 5 / 5 | 1 | — | |
| Delete images | `components/DeleteImagesDialog.vue` | ImageFileActions | 2–3 | 8 / 8 | 1 | — | |
| Series picker | `components/SeriesPickerModal.vue` | ManageImages import | 2–3 | 4 / 4 | 1 | — | |
| Immunemap import | `components/ImmunemapImportDialog.vue` | ManageImages | 2 | 10 / 12 | 1 | — | |
| Legacy migrate | `components/LegacyMigrateDialog.vue` | ManageImages | 2 | 5 / 7 | 1 | — | |
| **Image metadata** | `components/ImageMetadataDialog.vue` | ImageTable row ⋯ → item (`ImageTable.vue:770`); AcqCheckDialog | **3** (Data page), 4 elsewhere | 3 / 12 | 2 | — | accordion; Acquisition section collapsed |
| **Acquisition check** | `components/AcqCheckDialog.vue` | `AcqCheckButton` in ModuleLayout action bar; ImageMetadataDialog | 2 (Data) / 3 | 16 / 28 | 2 | — | a check that is 1 click from every ModuleLayout page, but only when a set with images is selected |
| **Acquisition card** | `components/AcquisitionCardDialog.vue` | ImageMetadataDialog → Acquisition section → Card | **5** (Data) / 6 | 2 / 2 | 1 | — | ComingSoonBadge Experimental in its title |
| Physical size | `components/PhysicalSizeDialog.vue` | ImageTable, MetadataPanel | 2–3 | 11 / 11 | 2 | — | |
| Copy function params | `components/CopyFunParamsModal.vue` | TaskRunner (needs a picked task) | 3 (Data) / 4 | 1 / 1 (row list, v-for) | 1 | — | |
| Chain from history | `components/ChainFromHistoryModal.vue` | ChainModule | 3 | 10 / 11 | 1 | — | |
| Chain inline modal(s) | `modules/ChainModule.vue` | ChainModule | 3 | (in page count) | 1 | — | page and modal share a 2,441-line SFC |
| Movies inline modal | `modules/MoviesModule.vue` | MoviesModule | 3 | (in page count) | 1 | — | |
| Board record | `components/canvas/BoardRecordModal.vue` | TabbedCanvas history icon (`TabbedCanvas.vue:286`) | 3 | 5 / 5 | 1 | records agent runs | |
| Gating copy | `modules/gate/GatingCopyDialog.vue` | GatingPlots | 3 | 4 / 6 | 1 | — | |
| Denoise / Flow model details | `modules/modelTraining/{Denoise,Flow}ModelDetails.vue` | ModelVault (ModelPlots manager on, or LayoutCanvas rail) | 3–4 | 0 / 0 (read-only) | 2 | "by Claude" origin | |
| Attachment gallery | `components/blackboard/AttachmentGallery.vue` | Blackboard entry → attachment | 4 | 2 / 2 | 1 | — | |
| Kiwi cockpit (its own BaseModal use) | see panels | | | | | | |

## 3. Floating panels, drawers, popovers

`FloatingPanel.vue` is the canonical viewport window. `CollapsiblePanel.vue` is the only drawer: the
right-hand functions panel, with one shared collapse flag.

| Surface | File | Opened from | Depth | First / total | Entry pts | Tier · badge | AI | Notes |
|---|---|---|---|---|---|---|---|---|
| Viewer panel | `components/ViewerPanel.vue` in FloatingPanel (`App.vue:220`) | sidebar launcher (`PanelLaunchers.vue`), compact launcher in ModuleLayout when plots are maximised, Preferences toggle | 1 | 34 / 50 | 3 | none | 1 Claude literal | pop-out → viewer-window |
| Lab log | `components/LabLogPanel.vue` in FloatingPanel | launcher, Preferences toggle, auto-badge on warn/fail (`stores/labCapture.ts:45`) | 1 | 10 / 12 | 2 | none | `[Claude]` entries, "Claude noted" badge | |
| Correction cockpit | `components/correction/CorrectionCockpit.vue` | launcher, Preferences toggle | 1 | 15 / 17 | 2 | **bespoke WIP hammer** (`PanelLaunchers.vue:46-51`, hex `#d97706`), not the tier system | — | maturity shown outside the tier registry |
| Kiwi cockpit | `components/kiwi/KiwiCockpit.vue` (+ KiwiAsk 16/19) | launcher (hidden by `showKiwiLauncher` when hidden), `stores/kiwi.ts:59,71` auto-open | 1 | 26 / 42 | 2 | `assist-with-kiwi` Experimental, but **no badge on the panel** | the AI hub | |
| ParamFigure | `components/ParamFigure.vue` in FloatingPanel | ParamRenderer param | 3–4 | 1 / 1 | 1 | — | — | |
| Cell card detail | `components/plots/CellCardDetailPanel.vue` in FloatingPanel | CellCardsView card click | ~4–5 (inferred: page → cards plot → card) | 2 / 2 | 1 | — | 1 literal | |
| Canvas side panel | `components/canvas/CanvasSidePanel.vue` (pop manager, series picker, vault) | PopulationManager, SeriesPicker, ModelVault | 2–3 | 6 / 32 | 3 | — | proposed-gate ✦ via PopulationManager | in the canvas layer, not FloatingPanel (by design) |
| Functions drawer | `components/CollapsiblePanel.vue` (ModuleLayout `#right`, Movies) | always on ModuleLayout pages that fill `#right` | 0 extra | 1 | — | — | — | hosts TaskRunner (34 / 40 at 1-level) |
| Guide bubble | `components/GuideBubble.vue` | a running guide | — | 3 / 6 | — | — | — | |
| Pointer bubble | `components/PointerBubble.vue` | Claude `point_at_ui` | — | 0 | C | — | yes | |
| Error console bar | `components/ErrorConsole.vue` (always mounted) | — | 0 | 9 / 11 | — | — | — | |
| Popovers | `TeleportPopover` in **25 files** (ImageTable ⋯, ProjectPanel, TaskRunner, SummaryPanel, PopulationManager, ChainModule, ManageImages, TasksModule, ViewerWindow, ClaimStrip, KiwiAsk, DrawSurface, ColourPicker, MovieOptionsButton, HpcSettings, …) | — | +1 on host | counted inside the host's "after expansion" | — | — | — | not separate rows |

## 4. Shell usage summary

- **ModuleLayout (17 routes):** manage-images, preprocess, metadata, cleanup, model-training,
  segment, gate, track, clust-cells, clust-tracks, regions, phenotype, behaviour, spatial, analysis,
  animation, batch-movies, plus `/custom`.
  - ModuleLayout alone adds about **20 first-paint / 49 total** controls: SetBar 6/9, ImageTable 8/42,
    action-bar filters, AcqCheck and CohortCheck.
  - So **every ModuleLayout page starts at about 20 + 32 chrome ≈ 52 controls before its own content.**
- **ModulePage (2):** notebooks, movies. The inventory confirms it (`FRONTEND.md:34`).
- **Bespoke (5 main-window pages):** Blackboard (1,149 lines), Chain (2,441), Settings (1,763),
  Tasks (669), Welcome. Bare windows are bespoke by nature; ViewerWindow alone is 7,437 lines.
- Only 2 pages use the lighter ModulePage shell. The 5 bespoke main-window pages include the
  three with the most controls apart from ViewerWindow (Chain, Settings) and the AI-heavy Blackboard.

## 5. Rankings

### Top 15 by total controls (1-level, first + expansion; routes and overlays together; chrome excluded; board-render excluded)

| # | Surface | Total | First paint |
|---|---|---|---|
| 1 | `/viewer-window` ViewerWindow.vue | 118 | 37 |
| 2 | `/manage-images` | 112 | 45 |
| 3 | `/chain` | 101 | 55 |
| 4 | `/settings` | 99 | **70** |
| 5 | PreferencesModal | 87 | 1 nav + default pane |
| 6 | `/track` | 84 | 54 |
| 7 | `/behaviour` | 79 | 46 |
| 8= | `/segment`, `/spatial`, `/custom` | 78 | 46 |
| 11 | `/metadata` | 76 | 36 |
| 12= | `/clust-cells`, `/clust-tracks`, `/regions` | 75 | 46 |
| 15 | `/model-training` | 71 | 42 |

By **first paint** alone, the top 5 are:
1. `/settings` 70
2. `/chain` 55
3. `/track` 54
4. the seven TaskRunner + plot pages at 46 each
5. `/manage-images` 45

The **2-level** upper bound reorders the top: `/track` 202, then `/chain` 167, then the cluster pages
and `/gate` (162–163).

### Top 10 by depth (fresh profile, project open)

| # | Surface | Depth | Path |
|---|---|---|---|
| 1 | Acquisition card | 5 (6 off Data) | Data page → row ⋯ → Metadata → Acquisition section → Card |
| 2 | Cell card detail panel | ~4–5 (inferred) | Explore page → cards plot → card |
| 3 | Image metadata dialog | 3 (4 off Data) | page → row ⋯ → Metadata |
| 4 | Attachment gallery | 4 | group → Blackboard → entry → attachment |
| 5 | Copy function params | 3–4 | page → pick function → copy |
| 6 | ParamFigure panel | 3–4 | page → pick function → figure |
| 7 | Model details (denoise/flow) | 3–4 | Model training → vault → details |
| 8 | `/tasks-window` | 3 | group → Tasks → pop-out |
| 9 | Chain-from-history, Board record, Gating copy | 3 | group → page → button |
| 10 | Acquisition check (off Data pages) | 3 | group → page → AcqCheck |

All sidebar pages are at most 2 deep; depth is not where pages get buried. The buried items are
**per-image checks reached through the ImageTable ⋯ menu** and **things inside plot panels**
(ClaimStrip is in every SummaryPanel footer, so claim review costs a plot first).

## 6. AI cross-check: disagreements with `AI_SURFACES_AUDIT.md` §1b only

1. **ClaimStrip starts a Kiwi turn.** `components/canvas/ClaimStrip.vue:4,46,87` runs the model review
   as a background Kiwi turn. It is mounted in every `SummaryPanel` (`SummaryPanel.vue:1387`). §1b says
   only Kiwi Ask and Follow up start a turn, and lists no claim row.
2. **§1b's "Gate today" column predates the toggle build.** 12 components now read
   `observer.ai.enabled`: AddToKiwiButton, DrawSurface, ImageTable ⋯ "Add to Kiwi" (`:776`),
   PopulationManager (`:676`), ClaimStrip, GuideMap, GuidesDialog, WhatsNewDialog, CreateProfileDialog,
   KiwiCockpit, PreferencesModal and AiOptInDialog. §1b lists several of these as "none".
3. **Plot point-out marks are not named in §1b.** These are GateScatterCell (`:27-34`),
   ClusterMapView (`:85-98`), TrackDiagnosticsView's "Claude highlighted N" chip (`:40-43`),
   PlotPointOutMark, and the ViewerWindow `mark_tile` overlay (`:83`). §1b's "Live pointing" row names
   only `viewer:mark` + PointerBubble.
4. **CaptureViewSurface's "share the refined capture with Claude" (`:51`) is not in §1b.** It sits on
   SummaryCanvas, LayoutCanvas and ViewerWindow captures through FrameAnnotator. §1b names only
   DrawSurface's Share dialog.
5. **The correction cockpit's WIP badge is a second, hand-rolled maturity signal** outside
   `utils/maturity.ts`. It isn't AI, but it is noted because Kiwi, the one AI panel with a tier
   (Experimental), shows **no** tier anywhere on its panel.

## 7. Could not count, and why

- **TaskRunner parameter forms** (ParamRenderer) on 12 pages: parameters come from task JSON at run
  time. The 2-level number counts each param-type branch once, which is neither the minimum nor the
  maximum.
- **Plot canvases:** SummaryCanvas, ClusterPlots, GatingPlots, LayoutCanvas. Each user-added plot
  panel adds about a dozen controls (SummaryPanel), and the number of panels is user state. The
  Analysis board (`/analysis`) is almost entirely this.
- **Chain graph nodes, Blackboard entries, the Kiwi feed, ImageTable rows:** lists rendered by
  `v-for`, counted once.
- **ViewerWindow:** counted, but 64 of its own 81 controls sit behind mode/tab conditions in a
  7,437-line file. The heuristic first-paint vs expansion split is least reliable here.
- **Profile-dependent sections** (`canManageMachine`, AI on/off, `showKiwiLauncher`): counted as visible.
- **Depth of Cell card detail** is inferred, not traced click by click.
- **No screenshots** (agents don't start the app; plan open question 1).
