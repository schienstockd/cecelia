# Changelog

All notable changes to Cecelia Feijoa are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project aims to follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
While the project is pre-1.0, minor versions may contain breaking changes.

Cecelia Feijoa is a ground-up reimplementation of the original R/Shiny
[Cecelia](https://github.com/schienstockd/cecelia-legacy) in a Julia + Python + Vue
stack. Per-tag notes are also on the
[GitHub releases](https://github.com/schienstockd/cecelia/releases) page.

## [Unreleased]

_Changes on `main` that have not yet been tagged in a release._

## [0.1.1] — 2026-08-10

45 pull requests since `v0.1.0`. Still `0.1.x` deliberately — this is the framework's first iteration
and it is still finding its shape, so the minor bump waits for it to settle rather than for the next
substantial change.

### Changed — store format (read this first)

- **Zarr v3 / OME-NGFF 0.5 stores can now be written.** Opt-in and **not** the default: the default
  stays flat-key v2, chosen from a measured storage-vs-access table in Settings → Storage (v3 costs
  ~14% less on disk and reads ~40% slower). Import picks the format and **every derived store inherits
  it**; there is no converter.
- **A store written as v3 cannot be opened by `v0.1.0`.** v3 *reading* landed in this same cycle, so an
  older install has no reader for it. Nothing forces the choice, but it is a one-way door per store.
- **New imports are little-endian** (`<u2` rather than `>u2`), following bioformats2raw 0.12 — which
  retires the byte-order class of bug entirely. Existing stores are untouched and still read correctly.
- **The NGFF version is now stamped** on the stores we write. Anything written before this has none,
  which is why the metadata modal reports it per stored version.
- Chunk-key separator and shard depth are import settings, inherited by derived stores like the format.

### Added

- **Movies are a managed collection, not a directory listing** — a per-project registry under
  `settings/` (so it travels with a `.ccbundle`), an in-app player, star / tags / rename / delete and
  bulk actions on a checked selection, and a sortable, resizable, collapsible list. A movie banks the
  config that produced it, so it can be **reopened for editing on the page that made it**.
- **The movie list shows its source image** — channel and attribute columns behind a Details toggle,
  with the image table's attribute filter (now one shared control, `AttrFilterPanel`).
- **Side-by-side comparison movies** — image versions across the columns × segmentation masks down the
  rows, recorded one pass per cell and composed into one file.
- Movie options throughout: explicit output size, frame range, title cards, mask outline width,
  filename suffix and attribute-based naming, and recording on the task rail with progress + cancel.
- **The Animation page picks its own image**, having become a proper module page.
- **A LabArchives notebook can be linked to a project.** The person analysing the images is often not
  the one who ran the experiment, and the design — cohort, protocol, the question being asked — lives
  in a lab notebook nothing here could see. Cecelia never talks to LabArchives and deliberately never
  learns how: Claude reads it through the user's own authenticated session and hands over a summary,
  which is cached beside the data and carried into the next session's briefing and the GUI.
- **Optical-flow segmentation** — a training task, a model vault page, and a preview backend.
- **The MCP observer can author analysis boards** (add-only; the user keeps them) and **design chain
  templates it cannot run**, and it can see image attributes and existing boards.
- Chain whiteboard: automatic layout for a DAG with no saved positions, plus a Tidy button.
- Gates are positioned in µm, through one pixel scale.

### Fixed

- The Movies page rendered blank (a temporal-dead-zone throw in setup), and the detector gap that let
  it through is closed.
- The analysis board re-rendered itself to Vue's recursion limit; a client reloaded its own write on
  every autosave; the read-back could not tell two boards apart. Boards are now versioned so two tabs
  stop clobbering each other.
- Skeletons vanished from a recording exactly like masks did, and a mask's outline width did not reach
  napari — so recordings came out filled.
- The mp4 writer leaked a staged `.tmp.mp4` on a failed or cancelled render.
- Movie title cards rendered every non-ASCII character as a box.
- A re-import reverted renamed channels; the import chunk-size parameter was wired to nothing.
- **The Metadata panel's "Original path" regex read the wrong path** — it fed the *converted* store
  name (`ccidImage.ome.zarr`), identical on every image, so the option could never do what it exists
  for. It now reads the recorded source location, and the builder gained a `/ folder` separator,
  2nd/3rd-last field positions (an absolute path has a variable number of leading folders, so counting
  from the start is useless), and a `strip extension` that applies only to the last field — applied to
  a `2026.07.16` date folder it had matched nothing at all.
- Column widths in `SelectionTable` were squeezed below what was specified while the frozen-column
  offsets were computed from the specified values, so sticky columns sat misaligned — visible on the
  Movies table once the Details columns ran past the panel.

### Infrastructure

- **Release notes now come from `CHANGELOG.md`.** They used to be GitHub's auto-generated pull-request
  list — which is also what the in-app What's New modal rendered, so the app's answer to "what
  changed" was a list of branch names. This section is what you are reading in both places.
- The notebook table takes the shared resize path (drag-to-resize, persisted widths, a reset).
- One write-behind autosave helper for the three stores that each had their own, `rafCoalesce` for
  paint-rate work, and a written-down coalescing rule with detectors for the two ways it breaks.

## [0.1.0] — 2026-08-05

**The first plain release.** Everything before this was an `-rcN` snapshot; nine of them never
converged, and because Julia sorts an `rc10` prerelease *below* `rc9` as a string, no further rc could
have reached an installed client at all. This tag ends that: it outranks every prerelease, and
`releases/latest` resolves for the first time.

498 commits since `v0.1.0-rc9`. Highlights:

### Added
- **Statistics on summary plots** — between-group hypothesis tests (Mann-Whitney / Kruskal-Wallis by
  default, t/ANOVA opt-in), Prism-parity brackets with p-value labels, Compact Letter Display as
  hoverable HTML overlays that survive PDF/PNG/SVG export, a Compare-groups toggle, which test `auto`
  picked, a sibling `{name}.stats.csv` per plot, and raw+stats zipped into one download.
- **Task preview** — a resident worker (`:7656`) runs a task's *own* compute over the visible region so
  parameters can be judged before a full run. Previewability is a declared task trait; re-previews are
  debounced on view change with a visible state, contrast windows survive moving T/Z, corrected
  channels keep their original's name and colour, and a failure says why somewhere readable.
- **Branching / skeleton analysis** (`segment.branching`) — skeleton extraction, a `branch` pop type
  with per-branch-type filter populations, anisotropy via `skimage.feature.structure_tensor` expressed
  in µm, napari skeleton + branch-type visualisation, and notebook readouts for the orientation field.
- **Spatial analysis + region clustering** — neighbour graphs, neighbourhood-composition region
  clustering with a `region` pop type, cell–cell contact statistics, and a CODEX-style contact heatmap.
- **Structured image delete** — one modal on the Import page with four scopes (whole images, versions
  with the new active picked, label sets with their companions, all analysis), replacing five deletion
  entry points spread across four screens. `reset_image_analysis!` drops derived output while keeping
  the image and never touching gate definitions.
- **Image file operations on the selection** — Copy / Move / Delete in the Import action bar instead of
  hidden at the end of each table row, applied to every checked image at once.
- **Copy an image version into a new image/set** (`editImages.copyImage`) — a re-import shortcut.
- **Custom cellpose checkpoints are drop-in** — `ccia.fluo` (fluorescence, for dendritic/SHG stroma)
  is fetched at install time; any checkpoint dropped into the models dir appears in the picker without
  a restart.
- **What's New + tip of the day** — a release-notes modal on launch, with feijoa sketches on the cards.
- **Observer / MCP** — one-click terminal setup for Claude, shadowed-entry detection and repair.
- **A set reference image**, nominated by the user, plus axis-based task gating (replacing the
  project-wide static/live distinction).
- **QC**: clipped-detector channels flagged on every import; AF's derived ceiling banked.

### Changed
- **The acquired bit depth is kept** — the 16→8-bit import conversion is gone as a non-goal, and the
  store codec is now an explicit, measured decision surfaced in Settings.
- **Every Julia↔Python boundary is versioned**, not just the preview worker.
- **UI unification** — one `.cc-btn` family, one `CcToggle`, one task-status colour map, semantic
  scenario utilities (`.cc-muted` / `.cc-readout` / `.cc-empty` …) with detectors that fail the build
  on a hand-rolled variant, and a tooltip *coverage* ratchet on top of the length one.
- **Storage reporting** — the Settings storage box now accounts for derived analysis, not just images.

### Fixed
- **`rc10` sorted below `rc9`**, so the updater told every client it was up to date. Prerelease digits
  now compare numerically.
- **Staged updates apply on every launch**, not only the first.
- **A worktree's Python env could resolve `cecelia` into a different checkout** — `PYTHONPATH` is
  pinned per checkout, and the shutdown path now stops every resident child.
- **Calibration**: the time scale reaches NGFF metadata and napari; both zarr layouts resolve when
  syncing; OME-XML is written into staged stores instead of being silently skipped.
- **Python text I/O declares UTF-8**, so Windows stops decoding sources as cp1252.
- **A store is identified by structure, not file extension**; the debris sweep finds what is
  incomplete rather than what is named `.partial`.
- Numerous AF-correction, import-window, QC-text, tooltip-layout and task-param fixes.

## [0.1.0-rc9] — 2026-07-21

### Added
- **Spatial analysis + region clustering** — squidpy neighbour graphs, neighbourhood-composition
  region clustering with a new `region` pop type and cross-poptype region queries, cell–cell contact
  statistics (Julia kNN points + trimesh mesh), and a CODEX-style log-odds contact heatmap on a new
  Spatial Analysis page (with napari region colouring and per-timepoint behaviour regions).
- **MCP observer** — read-only Claude access to a running project: session briefing, synthesized
  analysis lineage, population definitions, per-population phenotype/motility summaries, HMM behaviour
  + cluster summaries, spatial stats, cohort QC, whiteboard chains, task-param specs, and board plot
  types; plus notebook tools (create / revise / describe / read) and the REPL data-access surface.
- **In-app AI observer** — Ask/Chat-to-Claude with token readout, sessions, activity log, on-demand
  (non-auto) operation, and a "What Claude can do here" overview.
- **QC system** — objective per-task metrics from segmentation/measure/tracking/clustering/behaviour,
  cohort-consistency checks (median/MAD outliers, flags at n=3) banked per-image, a colour-blind-safe
  severity model, and `[Cecelia]` lab-log digests.
- **Per-project lab log** — auto activity digests, mute-by-category, reactions, and stale
  backend/napari diagnostics.
- **Notebooks (Pluto)** — real versioning (snapshot/restore), create-from-cells, in-place revise, and
  short descriptions.
- **Filter populations** — compound AND-ed conditions with full add/edit/delete in the Population Manager.
- **Custom modules** — user drop-in Julia + Python tasks with no rebuild.
- **Import** — optional 16→8-bit rescale + local staging for large OIRs; per-image metadata dialog;
  whole-filesystem file browser.
- **Analysis board** — unified figure/CSV export; true-vector SVG export for dot plots.

### Changed
- **Gating transforms follow the axis** — spatial/centroid axes (`centroid_x/_y/_z/_t`) default to
  linear and re-derive on axis/channel change; intensity columns select by channel name.
- Storage reclaim frees every non-active version (not just the original).
- Removed Harmony batch integration from clustering.

### Fixed
- **`pixi run dev` GUI shutdown** and **`pixi run stop`** no longer hang on Linux (use `ss`, not `lsof`).
- Revising a notebook preserves cell ids so an open notebook reloads live; revise/describe keep the
  existing description instead of blanking it.
- Self-heal stale `pluto`/`api` manifests on new app deps; macOS CI pin fixes; assorted import and
  scheduler fixes.

## [0.1.0-rc8] — 2026-07-16

### Added
- **Legacy migration**: import original R/Shiny Cecelia projects (images +
  segmentation + tracking).
- **First-launch setup wizard** + onboarding; **system-wide install scope** and
  in-app update surfacing.
- **Per-project lab log** — backend, routes, and Vue panel: auto-generated
  `[Cecelia]` activity digests, mute-by-category, reactions on entries, and
  GUI diagnostics that flag a stale backend or napari bridge.
- **MCP observer server** — read-only Claude access to a running project
  (Phase 1, Slice A).
- **napari 3D crop** — draw a box, preview, save as a new image.
- **Animation module** — keyframe render engine + timeline editor, batch
  movies (one mp4 per image), clean-capture publication stills, vector scale
  bar and timestamp.
- `INVENTORY.md` + a discovery-first rule; raw-datapoint CSV export, duplicate
  boards, and per-image stat units on the analysis board.

### Changed
- Plots aligned with the original R/ggplot look (UMAP, heatmap, palette).
- Consolidated zarr/OME image readers into one path; ImageJ TIFF metadata read
  via `ome_xml_utils`.
- Track/cluster populations render in their own colour under colour-by.

### Fixed
- Installer now expands a leading `~` in `CECELIA_HOME`.
- README config path; PDF/CSV export waits for plots to render (no fixed sleep).

## [0.1.0-rc7] — 2026-07-14

### Changed
- Fetch bioformats2raw at install time instead of bundling it — slims the
  release download.

## [0.1.0-rc6] — 2026-07-13

### Added
- **Population summary** — count / % specs, distribution charts, per-popType
  backbones (gated and tracked populations).
- **Analysis board** view-snapshot atom (zoom-to-source, sidecar board images,
  autosave); per-slot titles for PDF captions.
- Cluster UMAP **faceting**, colour-by population or image attribute, and
  **run-global cluster pops** pooled across co-clustered segmentations.
- Generic `ConfirmDeleteButton` and `TeleportPopover`; napari animation movie
  recorder; extracted `cecelia.utils.napari_utils` layer helpers.
- Frontend typecheck script + CI gate (`vue-tsc -b`).

### Changed
- **Gating overhaul**: 2D raster gating (dropped WebGL), multi-segmentation
  napari pops with autoscale, FlowJo-style dot plot, copy-strategy-to-images.
- Nav sidebar regrouped by pipeline stage; Settings moved to the footer.

### Fixed
- Gates re-project on scale change; freshly-drawn gates show immediately;
  morphology axes auto-linearise.
- Numerous plot/export fixes (square gate dots on PDF, heatmap left margin,
  canvas placement under zoom transforms).
- Dev supervisor: worktree switch relaunches the frontend; free a port by
  killing only the listening process; tear down Vite children on Ctrl-C.

## [0.1.0-rc5] — 2026-07-02

### Added
- **Universal Analysis canvas** (`/analysis`) — tabbed multi-board layout,
  gating-strategy plot, filmstrip, PDF/CSV export.
- One plot-hosting mechanism across surfaces with unified hi-res export; cluster
  UMAP, heatmap, and HMM plots on the board; read-only cluster manager.

### Changed
- Migrated theming to `@primeuix/themes` (`@primevue/themes` deprecated).

### Fixed
- Honour `yMin`/`yMax` on count/proportion charts; keep gate name labels
  on-canvas; CI bumped off the deprecated Node 20 runtime.

## [0.1.0-rc4] — 2026-07-01

### Added
- **Clustering** — Leiden cell + track clustering (`clust` / `trackclust` pop
  types), UMAP endpoint and module pages, cluster population manager, a generic
  interactive-plot canvas (UMAP + heatmap), and cluster HMM behaviour plots.
- **`LICENSE` (GPL-3.0-or-later)** + third-party acknowledgements.
- `run_py` launcher; collapsible left nav + right function/tasks panel; CI +
  Release status badges.

### Changed
- Route all Python spawns through `run_py` (drop inline spawn + `sys.path`);
  standardise Python runner filenames; move the update control to Settings.
- Removed the unused `PythonCall` dependency.

### Fixed
- macOS-arm64 install (cvxopt source build) + full cross-platform CI; installer
  404 while only prereleases exist; restore `ome-types`; several API/napari
  robustness fixes (missing `labelProps`, single-timepoint overlay, name coercion).

## [0.1.0-rc3] — 2026-06-30

### Added
- Bundle **bioformats2raw + Java** for out-of-the-box image import; per-OS README
  guidance for the projects folder / `custom.toml`.

### Fixed
- Use `read_idle_timeout` (`readtimeout` deprecated in HTTP.jl) in the updater.

## [0.1.0-rc2] — 2026-06-30

### Added
- **In-app update** — check + staged-apply-on-restart.

### Fixed
- README links point at the `cecelia-legacy` R/Shiny source.

## [0.1.0-rc1] — 2026-06-30

### Added
- **Initial release** — Cecelia ported to a Julia + Python + Vue stack: the core
  pipeline (image import, segmentation, tracking, gating, population management,
  behavioural track measures), headless-runnable `Cecelia.jl`, and the Vue
  frontend.
- **Bootstrap installer** + release workflow (`release.yml`); CI smoke-test
  workflow; README + docs.

[Unreleased]: https://github.com/schienstockd/cecelia/compare/v0.1.1...HEAD
[0.1.1]: https://github.com/schienstockd/cecelia/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc9...v0.1.0
[0.1.0-rc9]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc8...v0.1.0-rc9
[0.1.0-rc8]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc7...v0.1.0-rc8
[0.1.0-rc7]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc6...v0.1.0-rc7
[0.1.0-rc6]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc5...v0.1.0-rc6
[0.1.0-rc5]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc4...v0.1.0-rc5
[0.1.0-rc4]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc3...v0.1.0-rc4
[0.1.0-rc3]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc2...v0.1.0-rc3
[0.1.0-rc2]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc1...v0.1.0-rc2
[0.1.0-rc1]: https://github.com/schienstockd/cecelia/releases/tag/v0.1.0-rc1
