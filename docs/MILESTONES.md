# Cecelia Pineapple — Milestones

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
