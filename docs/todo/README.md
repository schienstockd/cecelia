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

## Index of every plan in here

**Complete as of 2026-08-20 — 57 files.** `docs/todo/` is **excluded from
default search** (see `CLAUDE.md` → *How to read the docs*): 1.2 MB of design docs contributed ~40–50%
of doc grep hits, and 349 citations from code already reach these files **by name**. So this index is
the search surface. Grep *this file*, then open the one plan you need.

**Statuses are copied from each plan's own header**, because this list previously drifted out of sync
with them (five plans were listed "planning" here while their headers said BUILT). If a row and a plan
disagree, the plan's header wins — and fix the row.

### Open — design not fully built. Check here before designing anything nearby.

| Plan | Status (from its own header) | What it covers |
|---|---|---|
| [`CORRECTION_PLAN.md`](CORRECTION_PLAN.md) | P1+P4a built (#590); P2, P3 open | Fix a wrong mask / wrong track in napari and have the correction become the data everything downstream uses |
| [`VALUE_NAME_INPUT_PLAN.md`](VALUE_NAME_INPUT_PLAN.md) | P1–2 built; **P3 open** | One canonical "what does this task write to" input; P3 tracked in `docs/TODO.md` → *Value-name input* |
| [`QC_PLAN.md`](QC_PLAN.md) | Phase 1 landed; **2–3 open** | Objective per-task QC: convention + producers + badges. Durable parts in `docs/ARCHITECTURE.md` |
| [`ZARR_V3_PLAN.md`](ZARR_V3_PLAN.md) | P1–3 built, P4 measured | Read/write/report zarr v3 (NGFF 0.5) + sharding. **Default stays `nested` (NGFF 0.4 / v2)** |
| [`TASK_RUNNER_PLAN.md`](TASK_RUNNER_PLAN.md) | P1–2 built + verified, dev-only | Move task execution out of the API process so a backend restart doesn't lose work in flight |
| [`PLUGINS_PLAN.md`](PLUGINS_PLAN.md) | P1–P4 built on `feat/plugins`; not browser-verified | Drop-in plugin layout, scans, precedence, PYTHONPATH, Settings surface, curated registry |
| [`CENTROID_AXES_PLAN.md`](CENTROID_AXES_PLAN.md) | partly shipped (P0, P4 done) | Explicit centroid axis names from the writer; the one-off converter was retired |
| [`SERVICE_PANEL_PLAN.md`](SERVICE_PANEL_PLAN.md) | in-progress, P1–2 verified live; **P3 open** | Settings control panel: live per-service status + start/stop/restart, global Quit, pixi console |
| [`PROJECT_IO_PLAN.md`](PROJECT_IO_PLAN.md) | in progress (`feat/project-io`) | Project export/import (`.ccbundle`); per-store pack/unpack foundation first, then task + routes + UI |
| [`LEGACY_MIGRATION_PLAN.md`](LEGACY_MIGRATION_PLAN.md) | in progress | Bring an old R-version project into the Julia stack **without recomputing** — images, segmentation, tracking only |
| [`SPATIAL_REGIONS_PLAN.md`](SPATIAL_REGIONS_PLAN.md) | in-progress (`feat/spatial-regions`) | Spatial-analysis port + region clustering; new `region` poptype reusing the cluster machinery |
| [`OBSERVER_INTEGRATION_PLAN.md`](OBSERVER_INTEGRATION_PLAN.md) | **not built** (Phase 1 done elsewhere) | Bring the MCP observer inside Cecelia so the assistant runs from the app |
| [`TRACK_SCHEME_PLAN.md`](TRACK_SCHEME_PLAN.md) | P1–P2 built + P2b (`feat/track-scheme`); **P3–P5 open** | Timeline-first track correction: lanes over frames, so "can these two merge" is answered by the picture rather than by a greyed button. Records why the per-row and XY-only surfaces failed |
| [`SEGMENTATION_OPEN_PROBLEM.md`](SEGMENTATION_OPEN_PROBLEM.md) | active — **negative result, not a plan** | Why CD169⁺ macrophage segmentation keeps failing; written so the next attempt doesn't re-derive the dead ends |
| [`ANALYSIS_CANVAS_PLAN.md`](ANALYSIS_CANVAS_PLAN.md) | planning (`feat/multipage-analysis-canvas`) | Multipage tabbed analysis board + gating-strategy plot + PDF export |
| [`CLUSTERING_PLAN.md`](CLUSTERING_PLAN.md) | UNBLOCKED (2026-06-30) — living plan | Leiden clustering for cells + tracks; GPU/RAPIDS parked. Cited from `pixi.toml`, `cluster.jl`, `clustering_utils.py` |
| [`CLUSTER_POOLING_PLAN.md`](CLUSTER_POOLING_PLAN.md) | design lock / status snapshot | How cluster runs pool across images; rides with the population-summary branch |
| [`POPULATION_SUMMARY_PLAN.md`](POPULATION_SUMMARY_PLAN.md) | design lock / status snapshot | One generalised population-summary surface |
| [`COASTAL_SEGMENTATION_PLAN.md`](COASTAL_SEGMENTATION_PLAN.md) | planning (2026-08-06) | coastal as cecelia's own denoise + segmentation engine. Read with `SEGMENTATION_OPEN_PROBLEM.md` |
| [`OPTICAL_FLOW_MODULE_PLAN.md`](OPTICAL_FLOW_MODULE_PLAN.md) | design (2026-08-06) | Training + inference surface for flow models; companion to `COASTAL_SEGMENTATION_PLAN.md` |
| [`SEG_QUALITY_PLAN.md`](SEG_QUALITY_PLAN.md) | ran to a conclusion — partly reversed | Better segmentation measured by QC-gate pass-yield. Its cpsam-vs-v3 numbers still stand for intravital; the v4 rejection was reversed by `CELLPOSE_V4_PLAN.md` |
| [`WORKFLOW_RECIPES_PLAN.md`](WORKFLOW_RECIPES_PLAN.md) | P0 shipped (#610); P1+P2 built on `feat/workflow-recipes` (2026-08-21), unreviewed in a browser | "What are you trying to do?" → the steps. Recipes composed from the existing guide system, no new runtime. P1 writes the **intravital** one only (needs no new guide); multiplex/interactions go in as request rows asking what people image and for example data. Came out of `CELLPOSE_V4_PLAN.md` |
| [`CELLPOSE_V4_PLAN.md`](CELLPOSE_V4_PLAN.md) | in progress (`feat/cellpose-v4`) | Migration to cellpose 4 (Cellpose-SAM), retirement of `cleanupImages.cellposeCorrect`. Reverses the v3 pin in `pixi.toml` and the verdict in `SEG_QUALITY_PLAN.md` |
| [`SEGMENTATION_QC_PLOT_PLAN.md`](SEGMENTATION_QC_PLOT_PLAN.md) | planned (2026-07-04) | A segmentation-integrity QC plot, per image and per timepoint |
| [`CROP_PANEL_PLAN.md`](CROP_PANEL_PLAN.md) | planned (2026-07-22) | In-app 3D crop UI; supersedes the napari-driven crop, which has a low ceiling |
| [`STATS_ANNOTATIONS_PLAN.md`](STATS_ANNOTATIONS_PLAN.md) | planning · no branch | Server-side hypothesis tests rendered as marks inside existing Observable Plot charts; no new route |
| [`WHATS_NEW_PLAN.md`](WHATS_NEW_PLAN.md) | planning · no branch | In-app What's New modal reusing the `/api/update/check` plumbing |
| [`OBSERVER_DATA_ACCESS_PLAN.md`](OBSERVER_DATA_ACCESS_PLAN.md) | parked (scoped, not started) | Let the observer read actual cell data, not just QC/logs/meta |
| [`QC_OBSERVER_PLAN.md`](QC_OBSERVER_PLAN.md) | parked | Observer-side QC surface; supersedes three exploratory prompts in `docs/ai-assist/` |

### Built — the plan is a record of *why*, not a spec of *what is*

For how these actually work, read the permanent `docs/<AREA>.md`. A built plan describes the design as
*intended*; where it shipped differently, the area doc is right.

| Plan | Status (from its own header) | What it covers |
|---|---|---|
| [`BRANCHING_PLAN.md`](BRANCHING_PLAN.md) | SHIPPED | `segment.branching` — skeletonise a segmentation into a branch/path network and measure each path |
| [`TASK_PREVIEW_PLAN.md`](TASK_PREVIEW_PLAN.md) | BUILT — #437 (2026-08-01) | Run a task's real compute over the visible region so params can be judged before a full run |
| [`GUIDE_SYSTEM_PLAN.md`](GUIDE_SYSTEM_PLAN.md) | BUILT (P1–P4); promoted to `docs/UI.md` → *Guides* | In-app click-through guides behind the compass button |
| [`LABARCHIVES_SYNC_PLAN.md`](LABARCHIVES_SYNC_PLAN.md) | BUILT — all three phases (2026-08-10) | Pull LabArchives ELN context (cohort, protocol, the question) into a cecelia project |
| [`MCP_BOARD_AUTHORING_PLAN.md`](MCP_BOARD_AUTHORING_PLAN.md) | BUILT (2026-08-08); Phase 4 cut | Let Claude ADD an Analysis board — create-only, semantic spec validated server-side |
| [`CANVAS_MANAGER_RAIL_PLAN.md`](CANVAS_MANAGER_RAIL_PLAN.md) | BUILT (2026-08-08); Decision 5 superseded for `GatingPlots` | A plot declares which manager it needs; the host resolves it instead of hardcoding the branch. The Track canvas is the second polymorphic host |
| [`MOVIE_MANAGEMENT_PLAN.md`](MOVIE_MANAGEMENT_PLAN.md) | BUILT — Phases 0–6 | Movies as a managed collection: `settings/movies.json`, rename, tags, star, delete, filters |
| [`MOVIE_COMPARE_PLAN.md`](MOVIE_COMPARE_PLAN.md) | BUILT P1–P6, then generalised (2026-08-08) | Compare image versions and segmentations side by side |
| [`TASK_LIST_UNIFICATION_PLAN.md`](TASK_LIST_UNIFICATION_PLAN.md) | built, phases 0–4 (#576 + follow-ups) | Both task surfaces onto `SelectionTable`; Phase 0 extracted `CcProgressBar` |
| [`IMAGE_DELETE_PLAN.md`](IMAGE_DELETE_PLAN.md) | BUILT (2026-08-04) — kept as the rationale record | Five image-deletion entry points collapsed to two; new `reset_image_analysis!` |
| [`VIEW_PROFILES_PLAN.md`](VIEW_PROFILES_PLAN.md) | BUILT (P1+P2+P2b + welcome page) | A named, ordered subset of sidebar pages, per user. Declutter, not access control |
| [`CUSTOM_MODULES_PLAN.md`](CUSTOM_MODULES_PLAN.md) | P1–P3 BUILT | User adds a task by dropping files into their config dir — no package edit, no rebuild |
| [`SMOOTHING_PLAN.md`](SMOOTHING_PLAN.md) | built as `cleanupImages.smooth` | Gaussian + centred 3-frame temporal median. The `smooth → AF → drift` composite is still open |
| [`PY_PACKAGING_PLAN.md`](PY_PACKAGING_PLAN.md) | DONE, verified in-env | `python/cecelia` as an installable package so coastal can `pip install` it |
| [`ZARR_STREAMING_PLAN.md`](ZARR_STREAMING_PLAN.md) | COMPLETE — #315, #317, #319 | Bounded-memory store writers; Phase 2 measured out, 3.2/3.3 parked |
| [`SPATIAL_GATE_UNITS_PLAN.md`](SPATIAL_GATE_UNITS_PLAN.md) | built, four suites green | Gate on calibrated spatial units; Phase 5 written then deliberately dropped (decision 8) |
| [`SPATIAL_ANISOTROPY_PLAN.md`](SPATIAL_ANISOTROPY_PLAN.md) | BUILT — PR #413 (2026-07-30) | Branching-port audit + structure-anisotropy readouts. **Notebook, not app plots** |
| [`UMAP_COLOUR_FACET_PLAN.md`](UMAP_COLOUR_FACET_PLAN.md) | all phases built (#127 + follow-ups) | Colour the cluster UMAP by cluster / population / attribute |
| [`STORAGE_RECLAIM_PLAN.md`](STORAGE_RECLAIM_PLAN.md) | built (`feat/storage-reclaim`) | Surface reclaimable space instead of telling users to delete originals by hand |
| [`RESOURCE_POOLS_PLAN.md`](RESOURCE_POOLS_PLAN.md) | Slice 1+2 done | Pools per real bottleneck + live per-pool sliders. Durable parts in `docs/SCHEDULER.md` |
| [`SKETCH_ENGINE_PLAN.md`](SKETCH_ENGINE_PLAN.md) | wired | The `feijoa` sketch repo behind tip cards; git-dep + conditional Vite alias |
| [`MOVIE_SEGMENTATION_AUDIT.md`](MOVIE_SEGMENTATION_AUDIT.md) | (a)+(b) BUILT (2026-08-08) | Audit record of what was wrong with movie segmentation overlays |
| [`TASK_DATA_REFRESH_PLAN.md`](TASK_DATA_REFRESH_PLAN.md) | BUILT (confirmed 2026-08-20) | Task-completion refresh without per-plot reload buttons; napari reloads data only. Plot refresh and napari reload stay separate toggles |
| [`plotting-canvas-and-track-df.md`](plotting-canvas-and-track-df.md) | BUILT (confirmed 2026-08-20) | Plotting canvas + track-property gating (gate on track measures, one point per track) |
| [`ONBOARDING_PLAN.md`](ONBOARDING_PLAN.md) | BUILT (confirmed 2026-08-20) | First-launch setup wizard removing the `custom.toml` wall; `config_dir()` is the one resolver it uses |
| [`NOTEBOOK_PLAYGROUND_PLAN.md`](NOTEBOOK_PLAYGROUND_PLAN.md) | BUILT (confirmed 2026-08-20) | Pluto notebooks as the structured home for downstream analysis. As-built: `docs/NOTEBOOKS.md` |
| [`ANIMATION_PLAN.md`](ANIMATION_PLAN.md) | BUILT — A–G + F1/F2 + Phase H all done (2026-07-24) | Publication figures and movies: channel layers, colours, tracks, title cards, on the shared snapshot foundation |

### Trackers and audits — not plans

| Plan | Status (from its own header) | What it covers |
|---|---|---|
| [`UX_PRIMITIVES_PLAN.md`](UX_PRIMITIVES_PLAN.md) | living tracker | Frontend UX-primitive unification status: what a machine check holds, what's open, what's deliberately left. Cited from `docs/ui/PRIMITIVES.md` |
| [`TASKS.md`](TASKS.md) | living checklist | One list, checked only when merged and green |
| [`AF_CORRECTION_AUDIT.md`](AF_CORRECTION_AUDIT.md) | measured 2026-08-06 | What the autofluorescence correction actually does on a real 4-channel 2P spleen movie |
| [`AF_QUANTISATION.md`](AF_QUANTISATION.md) | measured 2026-08-01, revised 08-03 | Output mapping resolved by the mechanism change; input precision still open |

## Keeping this index true

Add a row in the same change that adds a plan, and update the row's status in the same change that
changes the plan's header. A plan absent from this index is invisible, because the directory is not
searched by default.

**Enforced, not advisory.** `python/cecelia/tests/test_doc_index_convention.py` (`pixi run test-py`,
CI on every OS) fails if a plan has no row, a row points at a missing file, a plan is listed twice, an
open/built plan states no status, or a plan's own status outright contradicts the section it is filed
under. It is deliberately one-sided on that last check — a mixed status like "P1+P2 built, P3 open" is
legitimately *Open* and is left alone, because a test that cries wolf gets muted. The check exists
because this index had already failed exactly that way: it listed 20 of 57 plans, five of them
contradicting their own headers, and nothing noticed.
