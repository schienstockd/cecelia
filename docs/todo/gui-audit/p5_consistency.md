# P0 consistency pass (Decision 2), from the code only

Worktree: `cecelia-gui-clarity-audit-archive`, read on 2026-11-10. No screenshots. Every count comes from a
script over `frontend/src/**/*.vue` + `style.css` (`lightness.py` / `agg.py` in the same scratchpad).
**[inf]** marks an inference rather than something measured.

## 0. What the code already documents

`docs/ui/PRIMITIVES.md` (lines 225–262) names **three sanctioned page shapes**:

1. **`ModuleLayout`**: an image-table work page. It is SetBar → action bar → `Images` CollapsibleSection →
   `Plots` CollapsibleSection, plus a right-hand `CollapsiblePanel`.
2. **`ModulePage`**: a standalone page with a controls row and content, `layout=flow|scroll|fill`.
3. **The "ChainModule shape"**: a pipeline / writing surface with a toolbar on top and a list+detail split
   below (`SelectionTable` + `usePanelResize({edge:'right'})`). Its members are Chain, Tasks and Blackboard.

So the skeleton problem is not "no skeleton". The skeleton exists, and the outliers are pages that sit
outside those three shapes, or inside one shape but with their own chrome.

**No spacing or chrome tokens exist.** `style.css` defines colour, font-size (`--cc-fs-*`), radius and
layout dimensions. It has no `--cc-space-*`, no bar/panel padding token and no shadow/elevation token.

## 1. Page-by-page skeleton

| Page | Skeleton | Shape | Primary action | Status / progress | Next step |
|---|---|---|---|---|---|
| Manage images `modules/ManageImagesModule.vue` | ModuleLayout | table + right TaskRunner; import buttons in `#actions` | TaskRunner Run (right panel, mid) + action-bar import buttons | TaskList in the bottom half of the right panel (PaneExpandBar) | Only inside `LegacyMigrateDialog` ("now run Migrate…", l.83–90) |
| Preprocessing, Cleanup, Segment, Track, Cluster cells / tracks / regions, Behaviour, Spatial, Custom, Model training | ModuleLayout + `tasks/TaskRunner.vue` | table + Plots canvas + right runner | `.run-btn` (`TaskRunner.vue:1050`, CSS l.1288): full width, under the params | TaskList, bottom half of the right panel | none |
| Metadata `MetadataModule.vue` → `modules/metadata/MetadataPanel.vue` | ModuleLayout | table (editable) + right form | `cc-btn-primary` Apply per section | errors go only to the log rail (`log.error`, l.90–256); no inline feedback | none |
| Gate `GatingModule.vue` | ModuleLayout, **no right panel** | table + full-width Plots canvas (GatingPlots) | none (the gate editor acts in place) | toast + inline (GatingPlots) | none |
| Phenotype `PhenotypeModule.vue` | ModuleLayout, no right panel | table + SummaryCanvas | none | n/a | none |
| Analysis board `AnalysisModule.vue` | ModuleLayout, no right panel | table + `TabbedCanvas` inside the `Plots` section | none (view-only) | n/a | none |
| Animation `animation/AnimationPanel.vue` | ModuleLayout | table + Timeline canvas + right panel | `cc-btn-primary` Render (`AnimationPanel.vue:399`) | TaskList via PaneExpandBar (same as TaskRunner) | none |
| Batch movies `batchmovies/BatchMoviesPanel.vue` | ModuleLayout | table + right panel | `cc-btn-primary` Generate (l.706) | TaskList via PaneExpandBar | none |
| Tasks `TasksModule.vue` | Pipeline shape (bespoke `.tm-*` scaffold) | toolbar + filters + list \| log split | per-row actions | is itself the status surface | none |
| Blackboard `BlackboardModule.vue` | Pipeline shape (bespoke `.bb-*` scaffold) | toolbar + list \| editor split | `cc-btn-primary cc-btn-dense` New / Save revision (l.598, 707), in the toolbar and the pane | inline | none |
| Chains `ChainModule.vue` (2441 lines) | **Own shape** | segmented tabs (Edit/Live) → Edit = **3 panes**: fixed 190px palette \| DAG canvas \| slide-in config. Live = toolbar + canvas | `.run-chain-btn` (l.1836, CSS l.2422) at the **bottom of the left palette**, under a hand-rolled image picker | Live tab (a separate view) | preflight modal only |
| Notebooks `NotebooksModule.vue` | ModulePage `flow` | document: `<h2>` sections, server row, table | `cc-btn-primary` Start server, inline in the first section | inline server status row with hex-coloured icon | none |
| Movies & figures `MoviesModule.vue` | ModulePage `fill` + `CollapsiblePanel` | player stage \| right list panel | per-item actions; `cc-btn-primary` in a dialog | n/a | none |
| Settings `SettingsModule.vue` (1763 lines) | **Bespoke** `.settings-page` | 2-column document grid, `<h2 class="section-title">` with an underline | 17 × `cc-btn-primary`, scattered inline per field | inline per field | none |
| Welcome `WelcomeModule.vue` | none | a 7%-opacity watermark only; no content, no action | none: the project chooser is a **modal** (`components/ProjectPanel.vue` via BaseModal, opened from the sidebar) | n/a | none |
| Setup / Profile picker (`SetupModule.vue`, `AppProfilePicker.vue`) | bare route, centred card | `h1` 1.4rem + card with `padding: 2rem` | `cc-btn-primary` | inline | n/a |

**Run/status/next-step verdict.**
- **Run** sits in **three places**: the right panel (14 ModuleLayout pages), the bottom of a *left* palette
  (Chain), and inline in page flow (Notebooks, Settings).
- **Run** has **three visual forms**: `.run-btn` in TaskRunner (accent-fill, fs-md, radius-md); `.run-chain-btn`
  (green `#10b981` border on `#0c1a0e`, text `#6ee7b7`, fs-sm, radius-sm, the only green primary in the app);
  and `.cc-btn-primary` (Animation, BatchMovies, Notebooks, Settings, Blackboard).
- **Status** is consistent on the TaskRunner family: TaskList, bottom half, PaneExpandBar. Animation and
  BatchMovies reuse that. Elsewhere it is per page.
- **Next step: no page has a slot for it.** The only next-step copy in the app is inside
  `LegacyMigrateDialog`. The guides (`lib/guides`) are opt-in tours, not a resting signpost.

## 2. Families

| Family | Pages | Count |
|---|---|---|
| **F1 Image table + runner** | ModuleLayout + TaskRunner | 12 (Manage images, Preprocessing, Cleanup, Model training, Segment, Track, 3× Cluster, Behaviour, Spatial, Custom) |
| **F1b Image table + bespoke right panel** | ModuleLayout + own panel | 3 (Metadata, Animation, Batch movies) |
| **F1c Image table + canvas only** | ModuleLayout, no right panel | 3 (Gate, Phenotype, Analysis board) |
| **F2 Toolbar + list\|detail** | pipeline shape | 2 that conform (Tasks, Blackboard) + Chain (nominal member, does not conform) |
| **F3 Standalone ModulePage** | ModulePage | 2 (Notebooks, Movies) |
| **F4 Bespoke document** | own scaffold | Settings |
| **F5 Bare onboarding** | centred card | Setup, Profile picker (acceptable: outside the app shell) |

The Plot canvases inside F1/F1c are themselves one family, and a consistent one: GatingPlots,
ClusterPlots, SummaryCanvas, TabbedCanvas and ModelPlots all compose
`FloatingCanvasHost`/`InteractivePanel`/`CanvasPanel`.

### Outliers (pages that would feel like a different app)

1. **Chains.**
   - Its "family" is nominal. It is the doc's exemplar, yet it has three panes, not a list+detail split.
   - Fixed-width hand-rolled side panes: `.wb-palette` 190px (l.2131) and `.wb-config` slide-in (l.2303),
     neither a `CollapsiblePanel`.
   - A **hand-rolled image picker** (`.run-image-list`/`.run-row` with its own check icons, l.1801, CSS
     l.2370) instead of `ImageTable`/`SelectionTable`. This breaks "the table is the picker" (PRIMITIVES l.246).
   - The Run button is on the left and is green.
   - Its closure has the most page-specific chrome: 63 borders, 15 box-shadows, 83 hex (§4).
2. **Settings.**
   - The only page with its own page padding: `2rem 2.5rem` (l.1574), vs ModulePage `1rem 1.25rem`
     (`ModulePage.vue:41`) vs 0 on ModuleLayout.
   - A 2-column grid (l.1579).
   - `<h2 class="section-title">` with an uppercase eyebrow look plus `border-bottom` (l.1600). It duplicates
     `.cc-eyebrow` and adds a divider that no other page has.
   - 17 primary buttons scattered inline.
3. **Notebooks.**
   - On ModulePage, but it brings back an `<h2>` at a raw `1.05rem` (l.255).
   - Uses GitHub-palette hex for status: `#3fb950` running (l.258), `#f0883e` error (l.261), `#58a6ff` note
     (l.267). None are tokens; `--cc-sev-ok`/`--cc-warn`/`--cc-accent` exist.
   - Uses `.nb-error` instead of `.cc-muted-error`.
4. **Animation + Batch movies, right panels only.** They are the only right panels whose sections are
   **bordered boxes**:
   - `.ap-sec` / `.bm-sec`: `border 1px; radius-md; padding 6px 8px; surface-1`, byte-identical rules
     (`AnimationPanel.vue:427`, `BatchMoviesPanel.vue:748`).
   - Their headings are `h4` at fs-md/700.
   - TaskRunner and Metadata use full-width **dividers** with an eyebrow heading instead.
   - Same page skeleton, different feel inside the panel.
5. **Movies, inside the panel.** Three `.cc-card` boxes inside a `CollapsiblePanel` (l.642/689/695), so
   boxes sit inside a bordered panel.
6. **Welcome / project entry.** The landing route is an empty watermark. Opening or creating a project
   happens in a modal, so there is no page skeleton at all for the first thing a user does. **[inf]** The
   page with the least "what do I do now" signal is the very first one.
7. **F1c (Gate / Phenotype / Analysis board):** the right column disappears, so the page reads wider and has
   no status/output area. Acceptable (nothing runs). But the "select an image" empty state is rendered
   **three different ways**:
   - `GatingPlots.vue:749` `.gp-empty cc-muted`
   - `SummaryCanvas.vue:588` `.sc-empty cc-muted`
   - `ClusterPlots.vue:436` `.cp-empty cc-empty-inline`

   Each has its own `padding: 20px`.

## 3. Panel / section chrome

Shared components, and how many files use them:

| Component | Files using it |
|---|---|
| `ModuleLayout` | 18 |
| `ModulePage` | 2 |
| `CollapsibleSection` | 16 |
| `CollapsiblePanel` | 3 (ModuleLayout, Movies, ViewerWindow) |
| `FloatingPanel` | 5 |
| `PaneExpandBar` | 3 |
| `BaseModal` | 33 |

**Right-panel / pane section rules: six hand-rolled versions of one "panel section" idea.**

| Class | File:line | Padding | Separation | Heading |
|---|---|---|---|---|
| `.runner-section` | `tasks/TaskRunner.vue:1146` | 0.75rem | border-bottom | `.cc-eyebrow cc-fs-2xs` h3 |
| `.panel-section` | `metadata/MetadataPanel.vue:601` | 0.85rem 1rem | border-bottom | eyebrow |
| `.ap-sec` | `animation/AnimationPanel.vue:427` | 6px 8px | **boxed** border + radius | h4 fs-md 700 |
| `.bm-sec` | `batchmovies/BatchMoviesPanel.vue:748` | 6px 8px | **boxed** | h4 fs-md 700 |
| `.config-section` | `ChainModule.vue:2329` | 0.65rem 0.75rem | border-bottom | `.cc-eyebrow` label |
| `.settings-section` + `.section-title` | `SettingsModule.vue:1596/1600` | margin 2rem | heading underline | h2 fs-sm 700 uppercase |

**Bar / header rules: 28 hand-rolled `*-head|*-header|*-bar|*-toolbar|*-title` rules with a 1px border,
at 9+ distinct paddings.**

| Rule | File:line | Padding |
|---|---|---|
| SetBar | `components/SetBar.vue:213` | `0.6rem 1rem` |
| action-bar | `ModuleLayout.vue:507` | `0.55rem 0.5rem` |
| tm-toolbar | `TasksModule.vue:511` | `0.35rem 0.75rem` |
| log-header | `TasksModule.vue:630` | `0.55rem 0.9rem` |
| bb-bar | `BlackboardModule.vue:938` | `0.55rem 0.75rem` |
| live-toolbar | `ChainModule.vue:2060` | `0.4rem 0.75rem` |
| chain-bar | `ChainModule.vue:2141` | `0.45rem 0.55rem` |
| config-header | `ChainModule.vue:2319` | `0.6rem 0.75rem` |
| CanvasPanel panel-head | `canvas/CanvasPanel.vue:280` | `5px 8px` |
| csp-header | `canvas/CanvasSidePanel.vue:249` | `6px 8px` |
| fp-header | `FloatingPanel.vue:224` | `0.3rem 0.4rem 0.3rem 0.6rem` |
| tab-bar tab | `canvas/TabbedCanvas.vue:309` | `5px 10px` |
| cs-toggle | `CollapsibleSection.vue:120` | `0.35rem 1rem` |

Two backgrounds are in use for "a bar": `surface-1` and `surface-2`. The action bar sits on `--cc-bg`.

**Scaffolds the pipeline-shape doc says not to hand-roll, still hand-rolled:**
- Tasks `.tm-shell/.tm-toolbar/.tm-body/.tm-divider` (l.503–568)
- Blackboard `.bb-module/.bb-bar/.bb-split/.bb-list/.bb-pane` (l.929–1010)
- Chain `.chain-module/.chain-tabs/.edit-content` (l.2035–2128)

They are consistent with each other in spirit but are three copies, not a component. **[inf]** A
`SplitPage.vue` (toolbar slot + resizable list + detail) would carry all three.

## 4. Empty / loading / error states

- **Empty:**
  - `.cc-empty*` appears in **37** class attributes.
  - **33** bespoke `*-empty` classes carry no `.cc-empty` (mostly `.x-empty cc-muted`). Examples:
    `sel-empty` (`SelectionTable.vue:558`), `sc-empty`, `gp-empty`, `pm-empty` (`PopulationManager.vue:569`),
    `lc-empty` (`LayoutCanvas.vue:679`), `defs-empty` (`TaskRunner.vue:828`), `bb-pane-empty`
    (`BlackboardModule.vue:909`), 4× `channel-empty` (`ParamRenderer.vue`), 4× `kiwi-empty`.
  - Module-level pages that use `.cc-empty`: Tasks, Movies, Chain ×3, Notebooks/Blackboard (`-inline`).
  - **No empty state offers a next action** except TaskRunner's Reload.
- **Loading:**
  - `PlotSpinner` is used in 10 files.
  - There are 111 inline `pi-spin` spinners.
  - Bespoke loading classes: `gm-loading`, `panel-loading`, `pane-loading`, `pp-loading`.
- **Error:** errors take **three routes**.
  - The log rail (`log.error`): Metadata's only feedback.
  - Toasts: GatingPlots, PopulationManager.
  - Inline: `.cc-muted-error` 15 uses vs ~25 bespoke `*-error/*-err` classes (`nb-error`, `pp-error`,
    `pref-error`, `lm-error`, `ll-error`, `kp-err`, `vp-err`, `cfp-err`, `field-error`, `input-error`, …).

## 5. Lightness numbers

Scope: 218 `.vue` files + `style.css`. CSS comments are stripped. Inline `style="…"` is included.

### Totals

| Metric | Value |
|---|---|
| padding/margin/gap declarations | **1678** |
| …with a literal length | **1495 (89%)** |
| …zero/auto | 179 |
| …using a `var(--cc-*)` | **4** (there is no spacing token to use) |
| Distinct literal spacing values | **80** (0.5rem×174, 0.4rem×167, 0.3rem×134, 6px×134, 0.35rem×132, 4px×123, 2px×101, 8px×92, 0.6rem×82, …) |
| Units mixed | rem 1259 / px 663 / em 5 |
| Literals within ±1px of a 2/4/6/8/12/16/24/32px ladder | **88%** (1737/1977 numbers). Biggest off-ladder bucket: **10px** (137) |
| Border declarations (non-none) | **302**, of which single-side dividers 137 |
| Card-like rules (1px border + radius + background in one rule) | **101**, plus 18 `.cc-card` uses |
| box-shadow | **42** declarations, **38 distinct** values (no elevation token) |
| font-size | 458 declarations; **403 tokenised (88%)**; 55 raw across **24 distinct** raw values (1.4rem×7, 8pt×5, 1em×5, 2rem×4, 1rem×4, 0.95rem, 1.05rem, 13pt, 7.5pt, 9pt, …) |
| font-weight | **7 spellings**: 600×84, 700×49, 500×16, 400×4, normal×2, bold×1, 800×1 |
| Hex literals in `.vue` (style + template) | **286** in **70** files; **95 distinct**; **0** exactly equal a token value (all off-palette, e.g. `#fca5a5`×17, `#e836b4`×11, `#fcd34d`×10, `#484f58`×12) |

Hex values inside script (canvas drawing) are not counted.

**Top hex files:**

| File | Hex literals |
|---|---|
| `components/ErrorConsole.vue` | 22 |
| `components/ImageTable.vue` | 16 |
| `components/AcquisitionCardSheet.vue` | 15 |
| `modules/ChainModule.vue` | 13 |
| `components/LabLogPanel.vue` | 12 |
| `components/FigureStyleEditor.vue` | 11 |
| `cluster/ClusterPlots.vue` | 9 |
| `ClusterHmmTransitionsPanel.vue` | 9 |
| `plots/CardsPanelInner.vue` | 9 |
| `components/ProjectPanel.vue` | 9 |

**Files with the most literal spacing:**

| File | Literal spacing values |
|---|---|
| `SettingsModule.vue` | 52 |
| `ViewerWindow.vue` | 46 |
| `ChainModule.vue` | 43 |
| `TaskRunner.vue` | 41 |
| `PreferencesModal.vue` | 40 |
| `BlackboardModule.vue` | 36 |
| `ProjectPanel.vue` | 33 |

### Per page

"Own" = the page's import closure **minus** the shared shell (the `ModuleLayout` + `App.vue` closure, 73
files) **minus** the TaskRunner closure (12 files). So each row is chrome that page alone brings.

| Shared part | Files | Literal spacing | Borders | Card-like | Shadow | Raw fs | Hex |
|---|---|---|---|---|---|---|---|
| Shell (ModuleLayout+App) | 73 | 535 | 110 | 33 | 16 | 23 | 104 |
| TaskRunner closure | 12 | 119 | 19 | 3 | 0 | 0 | 10 |

| Page (own) | Files | Literal spacing | Borders (dividers) | Card-like + `.cc-card` | Shadow | Raw fs | Hex | Accent tokens used |
|---|---|---|---|---|---|---|---|---|
| Analysis board | 57 | 288 | 61 (21) | 23+3 | 11 | 14 | 83 | 6 |
| **Chains** | 53 | 272 | 63 (27) | 17+3 | **15** | 13 | 83 | 6 + own green |
| Gate / Track | 48 | 244 | 56 (22) | 20+2 | 9 | 10 | 66 | 5–7 |
| Cluster ×3 | 50 | 218 | 49 (18) | 19+1 | 10 | 13 | 85 | 7 |
| Segment / Phenotype / Behaviour / Spatial / Custom | 46 | 194 | 41 (14) | 14+3 | 10 | 10 | 61 | 5–7 |
| Model training | 40 | 155 | 30 (12) | 8+1 | 7 | 9 | 46 | 7 |
| Settings | 4 | 77 | 9 (4) | 3 | 0 | 0 | 4 | 5 |
| Blackboard | 4 | 40 | 11 (8) | 2 | 0 | 4 | 1 | 2 (accent, kiwi) |
| Batch movies | 5 | 33 | 8 (1) | 5 | 1 | 0 | 5 | 3 |
| Manage images | 7 | 32 | 3 | 1 | 0 | 2 | 6 | 7 |
| Movies | 4 | 26 | 3 (1) | 1+**3** | 0 | 0 | 12 | 3 |
| Animation | 4 | 25 | 6 (1) | 4 | 1 | 0 | 2 | 5 |
| Notebooks | 3 | 23 | 3 | 1 | 0 | 1 | **8** (3 status hex) | 0 tokens, 3 hex accents |
| Tasks | 1 | 17 | 3 (3) | 0 | 0 | 1 | 2 | 1 |
| Metadata | 2 | 14 | 3 (2) | 1 | 0 | 0 | 2 | 1 |

The canvas-heavy pages (F1/F1c + Chain) all pull in the plot-canvas closure (CanvasPanel,
FloatingCanvasHost, CardsPanel, ClusterMapView, …). That closure is where most of the per-page chrome
lives.

**[inf]** The plot canvas, not the page skeleton, is the heaviest chrome a user sees on most pages.

**Accent colours per page.**
- Every F1 page shows the same set: `accent` violet, `selected` orange, `warn`, `danger`, and
  `sev-warn/fail`, plus `active`.
- Pages that add a colour no other page has:
  - Chain: emerald `#10b981`/`#6ee7b7` Run.
  - Notebooks: GitHub green/orange/blue.
  - Blackboard: kiwi teal.
  - ImageTable: Tailwind amber/orange/red ramp `#fbbf24 #f97316 #fb923c #fca5a5` (shown on every F1 page).

## 6. Recommendations

### Which shared component should carry each outlier

| Outlier | Carrier | What moves |
|---|---|---|
| Chains | `CollapsiblePanel` (palette left, config right); `SelectionTable` for the run-image list (or embed `ImageTable` as the picker); one Run button primitive | `.wb-palette`, `.wb-config`, `.run-image-list`, `.run-chain-btn`, 13 hex |
| Settings | `ModulePage layout="scroll"` + the panel-section primitive below; drop the page padding override | `.settings-page`, `.section-title`, `.settings-section` |
| Notebooks | already ModulePage; heading → panel-section/eyebrow; status hex → `--cc-sev-ok`/`--cc-warn`/`--cc-accent`; `.nb-error` → `.cc-muted-error` | l.255–267 |
| Animation, Batch movies panels | the panel-section primitive (divider + eyebrow) used by TaskRunner, so all right panels read the same | `.ap-sec`, `.bm-sec` |
| Movies list panel | panel sections inside the `CollapsiblePanel` instead of `.cc-card` boxes | l.642/689/695 |
| Tasks / Blackboard / Chain scaffolds | **new** `SplitPage.vue`: toolbar slot + `usePanelResize` list + detail. The doc already describes it, but it is not yet a component | `.tm-*`, `.bb-*`, `.chain-module` |
| "Select an image" in the three plot canvases | `ModuleLayout` (it owns `selectedUids` and the Plots wrapper): one `.cc-empty` with one next action | `.gp-empty`, `.sc-empty`, `.cp-empty` |
| Welcome | **[inf]** a real page: recent projects + one "open/new" action on the page skeleton, instead of a watermark plus a modal | `WelcomeModule.vue`, `ProjectPanel.vue` |
| Run button ×3 | `.cc-btn-primary` + a `cc-btn-block` (full-width) modifier, or a tiny `RunButton.vue`; TaskRunner, Chain, Animation and BatchMovies all use it | `.run-btn`, `.run-chain-btn` |
| Next step (absent everywhere) | a `#next` slot on `ModuleLayout` (under the TaskList in the right panel) and on `ModulePage` (end of the controls row) | new |

### Proposed tokens (small set, in `style.css` `:root`)

Spacing ladder. Measured: 88% of today's literals are already within 1px of it.

```css
--cc-space-3xs: 2px;  --cc-space-2xs: 4px;  --cc-space-xs: 6px;  --cc-space-sm: 8px;
--cc-space-md: 12px;  --cc-space-lg: 16px;  --cc-space-xl: 24px; --cc-space-2xl: 32px;
```

The 10px bucket (137 uses) is the one call to make: round to 8 or 12 per site, or add `--cc-space-sm2: 10px`.

Chrome roles: one value each, replacing the 9+ bar paddings and 6 section rules.

```css
--cc-bar-pad:      var(--cc-space-xs) var(--cc-space-md);   /* every toolbar / panel header / SetBar / action bar */
--cc-bar-bg:       var(--cc-surface-1);
--cc-section-pad:  var(--cc-space-md);                      /* right-panel / pane section */
--cc-page-pad:     var(--cc-space-lg) var(--cc-space-xl);   /* ModulePage; Settings drops its 2rem 2.5rem */
--cc-divider:      1px solid var(--cc-border);              /* the 137 single-side dividers */
--cc-shadow-float: 0 2px 10px rgb(0 0 0 / .28);             /* floating panels / popovers (already in style.css l.842) */
--cc-shadow-modal: 0 24px 48px rgb(0 0 0 / .5);
--cc-ring:         0 0 0 2px var(--cc-accent-soft);         /* focus / active ring */
--cc-fw-normal: 400; --cc-fw-medium: 500; --cc-fw-strong: 600; --cc-fw-bold: 700;
```

Plus one rule worth writing into PRIMITIVES: **inside a panel, sections are separated by dividers, never
boxed.** Today's split is 4 divider styles vs 2 boxed (`ap-sec`/`bm-sec`) plus Movies' cards.

### Suggested P1/P2 slicing (from this pass)

1. Tokens + `--cc-bar-pad`/`--cc-section-pad`, applied to the shared shells only (SetBar, ModuleLayout
   action bar, CollapsibleSection, CollapsiblePanel, FloatingPanel, CanvasPanel, ModulePage). This is
   about 7 files and the highest reach.
2. A panel-section primitive, plus migrating TaskRunner, Metadata, Animation and BatchMovies (one look for
   every right panel).
3. A Run button primitive, plus a `#next` slot on ModuleLayout, plus a ModuleLayout-owned plots empty state.
4. Chains onto CollapsiblePanel / SelectionTable / tokens (the largest outlier, on its own PR).
5. Settings + Notebooks onto ModulePage / section primitive / tokens.
6. `SplitPage.vue` for Tasks/Blackboard (and Chain's Live tab).
7. Hex sweep. The top 10 files hold about 40% of the 286.
