# Cecelia Feijoa — Milestones

The durable ledger of what actually landed and how it was packaged. **Append-only** — never edit or
delete a past milestone; add a new one. This is the counterpart to the temporary
[`docs/ROADMAP.md`](ROADMAP.md) (forward goals) and the finer-grained [`docs/TODO.md`](TODO.md).

A **milestone** is a coherent, shippable state of the package. Each entry records:

- **ID / date** — `M<n>`, ISO date.
- **Version** — the `Project.toml` version at this milestone (`0.0.0` = pre-release dev).
- **Packaging** — how this state was packaged/distributed (git tag, GitHub release artifact,
  installer). `none (dev)` until Phase 3+ of the roadmap.
- **Landed** — the capabilities that became real at this milestone.

From the v1.0 freeze onward, cutting a milestone is one action: write the entry, bump the version,
git-tag, and attach the built artifacts to a GitHub release (see ROADMAP Phases 3–5).

---

## M2 — Distribution + analysis breadth (2026-08-05)

- **Version:** `0.1.0` — the first **plain** release. Every prior tag was an `-rcN`; nine of them never
  converged, and since Julia sorts an `rc10` prerelease *below* `rc9` as a string, no further rc could
  have reached an installed client. This tag outranks every prerelease and makes `releases/latest`
  resolve for the first time.
- **Packaging:** git tag `v0.1.0` → `release.yml` publishes a ~6 MB portable bundle (prebuilt frontend,
  `VERSION`, `pixi.toml`/`pixi.lock`) + a SHA-256. `install.sh` / `install.ps1` provision Pixi + Julia
  and fetch bioformats2raw (~190 MB) and the `ceceliaModels` cellpose checkpoints (~26 MB) at install
  time. In-app update check + staged apply on launch.
- **Landed** since M1 (498 commits):
  - **Distribution end-to-end** — installers, desktop launcher, `pixi run app`, checksum-verified
    self-update. ROADMAP Phases 4 and 5.
  - **Clustering** — cells, tracks and spatial regions as first-class populations in the one
    population manager (ROADMAP Phase 2), with heatmap + UMAP canvas plots.
  - **Spatial analysis** — neighbour graphs, region clustering, cell–cell contact statistics.
  - **Statistics on plots** — hypothesis tests, Prism-parity brackets, Compact Letter Display, CSV
    export; all surviving PDF/PNG/SVG export.
  - **Branching / skeleton analysis** with anisotropy in µm, and the custom-checkpoint path
    (`ccia.fluo`) that makes its real fibrous-tissue workflow reachable on a fresh install.
  - **Task preview** — a resident worker running a task's own compute over the visible region.
  - **Notebooks** (Pluto) and the **MCP observer** for read-only assistant access.
  - **Import correctness** — the acquired bit depth is kept, the store codec is an explicit decision,
    detector clipping is flagged, and calibration reaches NGFF + OME-XML + ccid consistently.
  - **UI unification with enforcement** — one button/toggle/status vocabulary, semantic scenario
    utilities, and build-failing detectors for copy length, copy style and tooltip coverage.
- **Known limitations at this milestone:** written almost entirely by AI and not yet independently
  tested by another user; native constructor/menuinst installers not built (the shell installers cover
  all three OSes); system-scope install unverified on a second machine.

## M1 — Analysis spine (2026-06-27)

- **Version:** `0.0.0` (pre-release dev)
- **Packaging:** none (dev) — runs from source via `pixi run dev`; see `docs/INSTALL.md`.
- **Landed:** the full processing + analysis spine in the Julia/Python/Vue stack, ported from the
  R/Shiny `cecelia`:
  - Import (OME-TIFF/Zarr, bf2raw, MIP); Cellpose segmentation.
  - Bayesian tracking (btrack) → `track_id` + lineage; **track measures** (celltrackR port:
    per-cell `live.cell.*`, per-track `live.track.*` → companion `{vn}__tracks.h5ad`).
  - Julia-native gating + population manager: hierarchical pops with indentation, cell **and**
    track populations, one-point-per-track gating, napari linked-brushing, derived `_tracked` pops.
  - Observable Plot summary canvas: box/violin/beeswarm/bar/histogram/frequency, cell + per-track
    views, facets, dark theme, population-consistent colours, CSV/PNG/SVG export, fully persisted
    view state (`useViewState`).
  - Headless-testable package boundary enforced; test suite green.
- **Not yet:** behaviour/HMM, clustering (ROADMAP Phases 1–2); versioning/packaging/self-update
  (Phases 3–5).

<!-- Next milestone M2 added when the next coherent shippable state lands (e.g. behaviour + clustering,
     or the v1.0 freeze). Do not edit M1 above. -->
