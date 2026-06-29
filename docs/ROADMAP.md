# Cecelia Pineapple — Roadmap

**Status: temporary goal-setting.** This is a lightweight direction sketch, not a contract — it
gets rewritten as priorities shift. The *durable* record of what actually landed (and was packaged)
lives in [`docs/MILESTONES.md`](MILESTONES.md). Fine-grained backlog is [`docs/TODO.md`](TODO.md);
deliberately-deferred alternatives are [`docs/FUTURE.md`](FUTURE.md).

Guiding constraints (do not violate): the **language boundary** (Vue = UI, Julia = orchestration +
stats + gating + API, Python = image I/O + ML; no fourth language) and the **layer boundary**
(analysis logic in the headless package, never the API/GUI). See `docs/ARCHITECTURE.md`.

---

## Where we are (baseline)

The processing + analysis spine is working end-to-end in the new stack:

- **Import** (OME-TIFF/Zarr, bf2raw, MIP) → **Segment** (Cellpose) → **Track** (btrack: `track_id`
  + lineage) → **Track measures** (celltrackR port in `tasks/tracking/track_measures.jl`:
  per-cell `live.cell.*` and per-track `live.track.*` aggregates → companion `{vn}__tracks.h5ad`).
- **Gating + population manager** — Julia-native engine, hierarchy with indentation, cell **and**
  track populations, one-point-per-track gating (`pop_type="track"`), napari linked-brushing,
  derived `_tracked` pops.
- **Summary plots** — Observable Plot canvas (box/violin/beeswarm/bar/histogram/frequency), cell
  and `granularity=:track` views, facets, dark theme, population-consistent colours, CSV/PNG/SVG,
  persisted view state. regl-scatterplot for gating/UMAP.

So the gap to the old R `cecelia` is now the **analysis layer on top** (behaviour, clustering),
then hardening and shipping.

---

## Phase 1 — Behaviour / HMM module — **largely landed**

Port the old `behaviourAnalysis` module. HMM maps 1:1 to Julia's ecosystem (`docs/ARCHITECTURE.md`)
— **Julia-native, no Python** for the fit. The track measures it needs already exist.

**Done:** `behaviour.hmm_states` + `behaviour.hmm_transitions` (set-scope — fitted jointly across the
selected images) + the `behaviour.hmm` composite (states→transitions one click). Core in
`app/src/behaviour/hmm.jl` (`HiddenMarkovModels.jl` + custom diagonal-Gaussian emission; Viterbi
decode; preprocessing: drop-NA-at-track-starts, noise filter, normalise, scale, post-filter
smoothing). States written as **numeric integer codes** (fast to gate/filter, auto-detected
categorical); transitions as **categorical strings**. Set-scope wiring (`task_scope`,
`run_task(imgs)`, `task:run` `imageUids`, `BehaviourModule` TaskRunner + `labelPropsColsSelection`
widget) landed too and is reused by Phase 2. **Dropped** (no-ops): `skipTimesteps`/`subtrackOverlap`
+ `seed` → TODO #00047. **Not yet:** `hmmHybrid` standalone (transitions already does cross-model
hybrid), `createPseudotime` (stretch), interpretive state relabelling (deferred to clustering).

Original design notes (kept for reference):

- **`behaviour.hmmStates`** — Gaussian-emission HMM over selected `live.cell.*` / `live.track.*`
  measures → discrete behavioural state per cell. R `depmixS4` → a Julia HMM package (e.g.
  `HiddenMarkovModels.jl`); confirm fit-stability on a known track set first. Params ported:
  `modelMeasurements`, `numStates`, `skipTimesteps`, `subtrackOverlap`, scale/norm,
  `postFiltering`/`postIterations`. Output: an HMM-state categorical measure → behaves like any
  population (gateable, plottable, colour-mapped).
- **`behaviour.hmmTransitions`** — state→state transition matrix + stats (feeds heatmaps).
- **`behaviour.hmmHybrid`** — combine models into hybrid states.
- *(stretch)* **`behaviour.createPseudotime`** — diffusion-map ordering; flag if heavier than
  expected.

Module page `BehaviourModule.vue` follows `GatingModule.vue` (ModuleLayout + TaskRunner +
useTaskDefs + persisted view state); states surface in the existing summary-plot canvas.

## Phase 2 — Cell & track clustering (akin to gating)

"Akin to gating" = clusters become **populations in the same population manager**, not a parallel
UI.

- **Cell clustering — `clustPopulations.leiden`** (Python/scanpy; port `leidenClustering.R`):
  normalise → optional batch correction → Leiden → UMAP/PCA. Clusters land as populations; UMAP in
  the existing regl-scatterplot. `+ clustPopulations.integrate` to merge across images.
- **Track clustering — `clustTracks.kmeans`** (Julia-native `Clustering.jl`; port
  `clusterTracks.R`): k-means/hierarchical on the per-track table — one point per track, cluster
  members expand to their cells. Track clusters become track populations.

## Phase 3 — Freeze v1.0

Hardening only. Version bump `0.0.0 → 1.0.0`, `CHANGELOG.md`, per-task smoke/validation tests,
docs pass, root `LICENSE`. Record the freeze as a milestone (below). Scope-freeze the post-v1
backlog.

## Phase 4 — Packaging & distribution (Linux/macOS/Windows)

Ship Cecelia as a **primary Julia package that carries a GUI and bootstraps its own Python env** —
the same model as the old R `cecelia` (a package that provisions its heavy Python deps). Frontend
built once to static (`npm run build`) and served by the Julia API; napari is already its own Qt
window.
Detailed plan + current state: **`docs/SHIPPING.md`**.
- **Python env** — DONE: committed `pixi.toml`/`pixi.lock` (Pixi), relocated out of `napari/`, GPU
  variant platform-gated (CUDA/MPS/CPU). Don't freeze napari+Qt+torch into one binary.
- **Julia** via `juliaup` + precompiled Manifest (or `PackageCompiler.create_app`); baked into the
  installer's conda env at build time.
- **GUI shell — DECIDED: system browser, no new runtime.** The Julia server serves the built frontend
  at same-origin `:8080` (DONE) and a small `app.py` launcher opens the default browser. **No Tauri**
  (rejected: Rust + webkit build pain), **no Electron** (rejected: bundled Chromium). Native installer
  + desktop icon come from conda `constructor` + menuinst (Phase 2, per-OS/CI build).
- **CI**: GitHub Actions matrix building per-platform release artifacts.

## Phase 5 — Self-update via GitHub

Releases tagged with semver; app checks the GitHub Releases API, pulls a newer release, re-syncs
the Julia Manifest + Python lockfile, uses the prebuilt frontend asset. Update-on-launch with
confirmation, not silent. (Adapts the old `updateCecelia.R` intent to a GitHub-release source.)

---

## Milestone system

The roadmap is throwaway; **milestones are the durable ledger**. A *milestone* = a coherent,
shippable state of the package: what landed, a version tag, and how it was packaged. Recorded in
[`docs/MILESTONES.md`](MILESTONES.md), append-only. Each freeze/release (Phases 3+) writes a
milestone entry and a matching git tag + GitHub release artifact, so "record what we've done and
package it" is one action. CHANGELOG (per-release detail) hangs off this.

---

## Backlog (post-v1, not scheduled)

From the old-R inventory, captured so nothing is lost — pull into a phase only on instruction:
more segmentation (StarDist/Mesmer/donblo/watershed/blob/iLEE/branching), more cleanup
(drift/AF/N2V/stripe/registration/filters), spatial analysis (neighbours/contacts/regions/meshes/
distances), classifiers (object + pixel), signal peaks, flow (FCS) import, model training (N2V),
region clustering, infra (HPC offload, mflux backup, launchpad), more import (Xenium/cellToLocation,
normalise/rescale/crop/split). See `docs/TODO.md` #00036–#00038 and `docs/FUTURE.md`.
