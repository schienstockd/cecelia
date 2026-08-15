# Parked plans

This directory holds **parked plans**: standalone design documents for a feature that is too big to
capture as a `docs/TODO.md` item, but not yet built (or built in phases). Each is a `*_PLAN.md` with
the real design work done up front — decisions, architecture, a phased build sequence — so the
thinking survives a context break and anyone (human or agent) can pick it up cold.

## What belongs here vs. the other trackers

| Doc | Holds | Shape |
|-----|-------|-------|
| `docs/TODO.md` | the backlog | numbered items, one line → one paragraph each |
| `docs/todo/*_PLAN.md` | **parked plans** | full design doc: decisions + phases + architecture |
| `docs/FUTURE.md` | anything **set aside**: known-better alternatives, non-goals, trigger-gated work | what/why-deferred/when-to-revisit |
| `docs/ROADMAP.md` / `docs/MILESTONES.md` | forward phase goals / shipped ledger | high-level |
| `docs/<AREA>.md` | how a **built** subsystem works | permanent reference |

## When to create a parked plan

Create a `*_PLAN.md` here when **any** of these is true:

- The feature needs **multiple locked decisions** and a **phased build sequence** before (or while)
  building — more than a TODO paragraph can hold.
- A topic is being **paused** ("I need a break from this") but the design must be preserved.
- **Code needs a stable pointer** to the rationale — e.g. `# see docs/todo/X_PLAN.md (Decision 5)`.

If it fits in a paragraph and needs no design, it's a `docs/TODO.md` item, not a parked plan.

## Conventions

- **Name**: `<FEATURE>_PLAN.md`, SCREAMING_SNAKE (`CLUSTERING_PLAN.md`, `ANALYSIS_CANVAS_PLAN.md`).
- **Top matter**: a one-line `Status:` (planning / paused / in-progress + branch) and a `## Goal`.
- **Locked decisions**: an explicit dated `## Decisions` (or "Locked decisions") section, numbered so
  code and other docs can cite them (`Decision 5`).
- **Phases**: an independently-shippable build sequence with checkpoints.
- **References**: code/docs that point at the plan use the repo-relative path
  (`docs/todo/<FILE>.md`), not an absolute/`~` path, so the pointer survives a checkout anywhere.
- **Promotion**: once the feature ships, move the durable "how it works" content into a permanent
  `docs/<AREA>.md` and either delete the plan or leave it as history (note which at the top).

## Current parked plans

- `SMOOTHING_PLAN.md` — **BUILT** as `cleanupImages.smooth` (σ=1 gaussian + centred 3-frame
  temporal median, one shared kernel per channel); the `smooth → AF → drift` composite is still open.
  Needed because AF's triangle background
  lands **inside the signal** on resonance-scanner data: the reference channel kept **8.6%** of its
  signal, ~80% after. Effectively the port of R's `slidingWindowCorrect` (whose window was off-by-one and
  off-centre). Deliberately not called "denoise" — that word belongs to the learned restorers.
  Measured on `zolIMa/fXgbTl` (16-bit, the operative case) + `eQRnwU` (8-bit). Negative results worth not
  re-deriving: coastal's `denoise_preserving_ratio` is the *worst* arm here, the Cellpose-3 net was
  **repaired and still dropped** (ties on ratio preservation once normalised through one shared window —
  that objection is retracted — but inflates masks 199 vs 140 px, 2 merges vs 0, at 31x the cost),
  **16-bit does not fix it** (max 522 of 65535 — photon-limited, not
  bit-depth-limited), a **spatial** median is catastrophic (it rejects sparse photon counts as outliers),
  and a "cells are merging" claim was **retracted** — the overlap test found 1 true merge and 0 lost
  objects, the count drop was 74 noise specks. Status: task built and run on a full movie; composite,
  and the drift-on-smoothed / smooth-the-trajectory follow-ups, still open.
- `SEG_QUALITY_PLAN.md` — **make segmentation better, measured objectively** (supersedes "drop the
  cellpose-3 pin so we can move to Cellpose 4" — that assumed v4 is the fix). Decision 1 is the
  yardstick: **QC-gate pass-yield**, so no hand annotation is needed. Ran to a conclusion — v3 is at
  its ceiling (`EaMaVq` 13.4% T / 7.9% B pass, i.e. ~87–92% of labels rejected as over-segmentation),
  and **Cellpose-SAM is categorically worse on intravital** (0/65 objects pass; no out-of-the-box
  config found), so **v4 migration is DROPPED** and the `cellpose==3` pin stays. Phase 3 set the north
  star: coastal as cecelia's own denoise + segmentation engine. Read alongside
  `SEGMENTATION_OPEN_PROBLEM.md`, which narrows Phase 3's flow premise.
- `SEGMENTATION_OPEN_PROBLEM.md` — **negative result, not a plan.** Two sessions failed to segment
  CD169⁺ macrophages on `zolIMa/fXgbTl`; written so the next attempt doesn't re-derive the dead ends.
  The load-bearing finding **challenges `SEG_QUALITY_PLAN.md` Phase 3's premise**: these cells are
  effectively sessile (0.27 µm/min), so *every* velocity field — coastal's Farneback **and**
  OpticalFlow3D's Lucas–Kanade — sits at 0.53–0.61 AUC for cell-vs-background, i.e. chance. Only
  **spatial-structure** fields separate, and a plain 3D structure tensor gets 0.941 with no flow at
  all. coastal's flow segmenter over-segmented **7×** versus a six-line intensity baseline (167 vs 22
  objects). Also ruled out: AF ordering (both directions fail, two different causes), AF competitor
  reconfiguration (5.0 → 5.4%), exponent, scale normalisation. Open: which channels are genuinely
  mutually exclusive, whether dendrites are a deliverable, and **scoring any of this on the QC-gate
  yardstick** (`SEG_QUALITY_PLAN.md` Decision 1) — which this session did not do.
- `CLUSTERING_PLAN.md` — Leiden clustering (cells + tracks), GPU/RAPIDS parked. Cited from
  `pixi.toml`, `clustering_utils.py`, `clustPops`/`clustTracks` `cluster.jl`, `docs/SHIPPING.md`.
- `ANALYSIS_CANVAS_PLAN.md` — multipage tabbed analysis board + gating-strategy plot + PDF export
  (branch `feat/multipage-analysis-canvas`).
- `SERVICE_PANEL_PLAN.md` — Settings control panel: live status + start/stop/restart for backend /
  napari / notebooks, global Quit, and a separate-window "pixi console" (reuses the existing log
  console). Branch `feat/settings-service-panel`; phased (panel → console → backend restart).
- `SPATIAL_ANISOTROPY_PLAN.md` — branching-port audit (A1–A8) + the structure-anisotropy readouts
  behind Figure 4 panels B (SHG quiver + tracks) and D (per-image anisotropy). **Notebook, not app
  plots** — the app computes and stores, `docs/NOTEBOOKS.md` plots. The audit is the substance: the
  anisotropy `uns` contract was *not* ILEE-compatible (the structure tensor's **minor** eigenvector
  is the fibre direction, so the docs invited a silent 90° error), the σ/box defaults gave a
  near-random field, `flattenBranching` dropped the time axis, and the branch labels zarr declared
  the wrong axes so Y inherited the Z step. Worktree `spatial-anisotropy`, branch
  `feat/spatial-anisotropy`. Supersedes `docs/archive/spatial-anisotropy-quiver-prompt.md`.
- `SPATIAL_REGIONS_PLAN.md` — spatial analysis port + region clustering + CytoMAP parity + live
  behaviour-region extension. New `region` poptype (reuses cluster machinery), squidpy re-added,
  per-cell neighbourhoods primary, cross-poptype query in Julia. Status: planning, no branch.
- `PY_PACKAGING_PLAN.md` — make `app/py` an installable package (rename `py` → `cecelia`,
  `pyproject.toml` + extras, editable pixi install) so external consumers (`coastal`) can
  `import cecelia.utils.*` without a `sys.path` hack. Touches `app/src/py_runner.jl` (1 line),
  19 Python imports, `pixi.toml`.
- `WHATS_NEW_PLAN.md` — surface release notes + tips inside the app; reuses the existing
  `/api/update/check` plumbing (only the release-notes `body` field is new). No new notification
  surface, no in-app feedback capture (GitHub issue link instead). Supersedes
  `docs/archive/update-modal-prompt.md`.
- `STATS_ANNOTATIONS_PLAN.md` — server-side hypothesis tests (Mann-Whitney / Kruskal-Wallis
  defaults, t/ANOVA opt-in) rendered as marks inside existing Observable Plot summary charts;
  extends `PlotDataResponse` with `comparisons?` — no new route. Sets the `StatsResult` contract
  reused by `WHATS_NEW_PLAN.md` and `SKETCH_ENGINE_PLAN.md`. Supersedes
  `docs/archive/stats-on-plots-prompt.md`.
- `IMAGE_DELETE_PLAN.md` — **BUILT** (2026-08-04; kept as the rationale record). Collapsed the five image-deletion entry points to **two**: a structured
  delete modal on the Import page (whole images / versions + new active / label sets / all analysis)
  and Settings for automatic whole-project reclaim. Unlists the `importImages.remove` task rather than
  deleting it (the chain suite's real-task workhorse) and scraps the ViewerPanel label delete. New core
  `reset_image_analysis!` (keep-list, not delete-list). Grew out of a since-deleted `docs/TODO.md` item.
- `SKETCH_ENGINE_PLAN.md` — the **feijoa** play repo
  (`github.com/schienstockd/feijoa`, `~/cc-workspace/feijoa`) where sketches are authored for
  cecelia's tip cards. Not yet wired into cecelia — the plan documents the git-dep + conditional
  Vite alias to add at the first commit that swaps a WhatNewCard's grey placeholder for
  `<SketchCanvas>`. Rough.js + animejs; sketches are JSON; R Cecelia logo is the smoke-test port.
  Supersedes `docs/archive/sketch-engine-prompt.md`.
- `MCP_BOARD_AUTHORING_PLAN.md` — **planning** (`work/mcp-boards`). Let Claude ADD an Analysis board
  (one per call, create-only, never modify/delete) — the third artefact after notebooks and chains,
  same design-but-don't-run split. The naive version (allowlist the existing autosave route) is wrong
  three ways: it is a *verbatim whole-document overwrite*, the on-disk `LayoutEntry` is unreadable in a
  preview and unvalidatable, and it races the browser's 800 ms autosave. So: a **semantic spec expanded
  and validated server-side**, a separate create-only route, and a versioned+merged boards document —
  which also fixes the existing bug where **two browser tabs clobber each other's boards**. Phase 0
  (image attributes + board read-back) is a prerequisite with a hard stop: probing `4kS67f` showed the
  metadata is good enough to plot well, but five cluster runs (three junk-named) and no exposed
  attributes mean it would currently miss the comparison that matters.
- `MOVIE_MANAGEMENT_PLAN.md` — **built** (Phases 0–6). Movies become a managed
  collection: `settings/movies.json` keyed by filename (the `notebooks.json` shape), display-name
  rename that never touches the file, free-form tags + a recorder-written `producedBy`, star, delete,
  filters. The audit is the substance: there was **no per-movie record at all**, the generation config
  was **browser-local** (`cc.napariSetPrefs`), movies **overwrite silently**, and the napari protocol
  versions are the wrong shape to reuse (they reject and relaunch; saved config needs migration). Both
  generation configs already round-trip — `seedConfigFromViewState` ↔ `apply-movie-config` — so no new
  capture mechanism was needed. Also flips `napariAutoSaveLayerProps` to true, without which a saved
  look is not reproducible. Phase 5 answered the ONE-TABLE question with counts and then did it: all
  eight surfaces are on `SelectionTable` bar `FileBrowser` (a stated exception — its row click is
  per-row semantics and its `..` row is synthetic). Phase 6 reopens a movie's config on the page that
  authored it, prefilled, with an Undo and a line naming whatever no longer resolves — and fixed three
  gaps the banking phase left: no image reference, an animation banking the render payload rather than
  the editor's model, and a per-image timeline that must be replaced per image rather than wholesale.
- `CANVAS_MANAGER_RAIL_PLAN.md` — **in-progress** (`work/manager-rail`). A plot declares **which
  manager it needs** (`rail` on the interactive/cluster registries) and the Analysis board resolves it,
  instead of hardcoding `activeIsCluster ? PopulationManager : SeriesPicker` — which is why
  `flowProbability` is currently **dead on the board** and `flowTraining` carries a second, bespoke
  model picker. The chrome was already shared (`CanvasSidePanel`); what was missing is the **role
  contract** (`{selected, scope, docked}`), because until the board no host ever held two managers
  behind one variable. Also cleans the half-renamed `pm-` CSS vocabulary the generalisation left
  behind (`CanvasSidePanel` → `csp-`, `SeriesPicker` → `pick-`).
- `TASK_LIST_UNIFICATION_PLAN.md` — **planning** (`refactor/task-list-canonical`). The two task
  surfaces — the module-page sidebar (`tasks/TaskList.vue`, a card stack) and the Task Manager page
  (`modules/TasksModule.vue`, a flat row list) — are **two hand-rolled lists**, neither using
  `SelectionTable`, the canonical list per `docs/UI.md`. The visible symptom is that `/tasks`
  highlights the selected row in **purple** (`--cc-accent`, form-control chrome) using a hand-rolled
  copy of `SelectionTable`'s own left-rule idiom, which is **amber** (`--cc-selected`). Dated as
  drift, not a decision: the rule is from the initial commit, `--cc-selected` arrived 2026-07-14 and
  `SelectionTable` 2026-08-03. Both surfaces move onto `SelectionTable`; the sidebar becomes a real
  table, header and all (Dominik, 2026-08-15). Phase 0 is separable and lands first: **four**
  hand-rolled determinate progress bars (`TaskList`, `TasksModule`, `SettingsModule`, `ProjectPanel`)
  that have drifted to two heights, two radii, two transitions and three different fraction→width
  sums, extracted to `CcProgressBar`.
