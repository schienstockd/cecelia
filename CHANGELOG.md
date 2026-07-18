# Changelog

All notable changes to Cecelia Pineapple are documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project aims to follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).
While the project is pre-1.0, minor versions may contain breaking changes.

Cecelia Pineapple is a ground-up reimplementation of the original R/Shiny
[Cecelia](https://github.com/schienstockd/cecelia-legacy) in a Julia + Python + Vue
stack. Per-tag notes are also on the
[GitHub releases](https://github.com/schienstockd/cecelia/releases) page.

## [Unreleased]

_Changes on `main` that have not yet been tagged in a release._

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

[Unreleased]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc8...HEAD
[0.1.0-rc8]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc7...v0.1.0-rc8
[0.1.0-rc7]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc6...v0.1.0-rc7
[0.1.0-rc6]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc5...v0.1.0-rc6
[0.1.0-rc5]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc4...v0.1.0-rc5
[0.1.0-rc4]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc3...v0.1.0-rc4
[0.1.0-rc3]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc2...v0.1.0-rc3
[0.1.0-rc2]: https://github.com/schienstockd/cecelia/compare/v0.1.0-rc1...v0.1.0-rc2
[0.1.0-rc1]: https://github.com/schienstockd/cecelia/releases/tag/v0.1.0-rc1
