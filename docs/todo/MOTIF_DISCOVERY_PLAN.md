# Sub-behavior motif discovery — plan

> **Status: parked (2026-09-15).** Blocked on the maintainability audit
> (`cecelia-comment-audit/`, `docs/archive/comment-audit-prompt.md`) finishing —
> `MAP.md` and `MAINTAINABILITY.md` need to be fleshed out beyond the current
> skeletons before this plan spawns new-file work. Design + feasibility already
> done — see the audit report on `docs/archive/motif-discovery-audit-prompt.md`
> and the full report at `~/Downloads/prompts/motif-discovery-audit-report.md`
> (kept outside the tree per the audit-queue rule).
>
> Once unblocked, work resumes on a fresh branch off `origin/main` in a
> dedicated worktree (`cecelia-motif-discovery`). Phase 1 (POC) is ~1–2
> focused sessions.

## Goal

Add **sub-track motif discovery** to Cecelia as a third representation of cell
behaviour, alongside:
1. per-timepoint **HMM states** (`live.cell.hmm.state.{col}`), and
2. whole-track **Leiden clusters** (`clusters.{suffix}` / `live.track.*`).

The gap: neither representation captures a *shared temporal pattern shorter
than a full track and not equivalent to a single HMM state* — e.g.
"approach-to-DC" vs "approach-to-tumor", each a multi-step composite of state
transitions ⊕ spatial context. Motif discovery finds and clusters those
sub-track patterns unsupervised.

Feature space: kinematics (`speed`, `angle`) ⊕ HMM state (as ordinal channel
OR discrete alphabet) ⊕ optionally distance-to-target
(`live.cell.min_distance_t#<target>`, see Decision 6).

Not a new page, not a new sidebar item — the tasks land on the existing
behaviour module page (`frontend/src/modules/BehaviourModule.vue`).

## Method — anchored, not to be re-litigated

**Detection = multivariate matrix profile via STUMPY `mstump` through
`run_py`** (Decision 3). Fixed window, motifs emerge unsupervised. This is the
Berman/MotionMapper spirit — let stereotyped sub-behaviours surface first, no
template required.

**Cluster time = pairwise subsequence DTW via `DynamicAxisWarping.jl` in
Julia on the top-K survivors** (Decision 4), then feed the K×K distance
matrix into the existing Leiden path (`find_populations` in
`python/cecelia/utils/clustering_utils.py`, `use_rep="X_dtw"` with a
precomputed obsp).

Not to be re-proposed: DTW for detection with a hand-crafted template
(there is no ground truth to hand-craft against); PythonCall.jl for the
STUMPY leg (violates the "no resident Python in the Julia server" layer
contract in `docs/ARCHITECTURE.md`); matrix profile in Julia
(`MatrixProfile.jl` is not in `app/Project.toml` today, and its multivariate
maturity vs. STUMPY was not verified — treat STUMPY as the default).

## Locked decisions

Numbered so code and other docs can cite them (`# see MOTIF_DISCOVERY_PLAN
Decision N`).

1. **New pop_type `motifs`** (mirror `clust`/`trackclust`/`region`).
   *Not* an annotation on `trackclust` — collapsing two independent
   groupings (a track can have a whole-track cluster class AND a sequence
   of motifs from a different clustering run). *Not* a variant of `clust`
   — different granularity semantic (a `clust` cell = member of a
   track-level cluster; a `motifs` cell = inside a motif-instance span).
   Confirm with Dominik before implementation.
2. **Set-scope task, always** — same rule as clustering (see
   `docs/todo/CLUSTERING_PLAN.md` Decision 6). Pooling via
   `pop_df(imgs, uids, "live", pops; granularity=:cell)`; per-image
   write-back mirrors `hmm_states.jl:44`. No per-image variant ships.
3. **Detection method = STUMPY `mstump`** through `run_py`. See "Method"
   above. Adds one Python dep (`stumpy`) — bank via `pixi.toml`.
4. **Cluster-time metric = subsequence DTW via `DynamicAxisWarping.jl`.**
   Adds one Julia dep to `app/`. Triggers the three-manifest
   `Pkg.resolve()` dance (`docs/ARCHITECTURE.md` — the trap where `Pkg.add`
   in `app/` breaks `api/` and `pluto/` precompile). Verify all three envs
   precompile before landing.
5. **Discrete hard clusters as default; mandatory confidence
   (`distance_to_centroid`) per instance.** Every motif instance carries
   its hard label AND the DTW distance from the instance to its class
   medoid, side by side in the cell obs (`motif.class.{suffix}` +
   `motif.distance.{suffix}`). Continuum representation (arXiv 2506.15190
   basis-function motif weights) deferred to v2 as an *additive* obsm
   layer that doesn't retire the hard label. Reserving the obsm slot
   `M_weights.{suffix}` upfront is an open question (Decision 3 in §Open).
6. **Distance-to-target uses a new per-t column, not the existing
   `min_distance#{target}`.** `cellContacts` today builds one KDTree over
   the whole A/B frame regardless of `centroid_t` (verified at
   `app/src/tasks/spatialAnalysis/cellContacts.jl:73-85`), so its output
   collapses across timepoints — wrong for motif use where a fast-moving
   B cell three frames away must not appear as a spurious match. Add a
   sibling task `spatialAnalysis.cellContactsPerT` writing
   `live.cell.min_distance_t#{target}`, sharing a small
   `per_timepoint_min_distance(a_df, b_df, targets)` helper in
   `python/cecelia/utils/spatial_utils.py`. ~30 LOC. Motif runner reads
   the new column; the old one keeps working for image-summary use.
   *(Design proposal — see Where I had to guess in the report; if per-t
   distance is already banked somewhere unfound, this becomes unneeded.
   Grep on `min_distance` returned only `cellContacts.jl` +
   `cell_contacts_mesh_run.py`.)*
7. **Motif class does NOT feed back into whole-track clustering in v1.**
   Same stance as the HMM state: keep it downstream, mirror the
   `clustTracks` position (see `docs/todo/CLUSTERING_PLAN.md` Decision 9).
   The scientific point of a motif *is* the sub-track dynamic that
   whole-track clustering averages away; feeding motif frequencies back
   re-averages them at exactly the wrong layer. `track_props.jl`
   compute-on-read stays trivially extensible for a v2 experiment.
8. **Resolution locked before comparison — sidecar receipt, advisory
   first.** Task JSON declares `resolutionLocked: bool` (default `false`).
   The `{props}.motiffeatures.json` sidecar (Decision 12) records
   `resolutionLockedAt: <iso-timestamp>` the first time a run is used in
   a group comparison (`partOf > 1` and a board authored comparing across
   `attr:*` groups). Enforcement point (task-level `ParamValidationError`
   vs board-side warn vs advisory only) stays advisory in v1; hard-gate
   is a follow-up decision. Each run emits `qc_finding("info",
   "motif.resolution_choice", …)` with `resolution`, `numClasses`,
   `medianConfidence`, `boundaryFraction`.
9. **Overlapping-instance handling on the per-cell column: keep the
   highest-confidence instance covering each cell.** Matrix profile
   discovers overlapping motifs by construction. Options considered:
   (a) highest-confidence-wins (chosen), (b) comma-join into a composite
   label, (c) top-N ranked columns `motif.class.{suffix}.{rank}`.
   (a) matches how gating handles membership, keeps `pop_df` filters
   simple. Instance-level detail (all overlapping instances with their
   spans and confidences) lives in the `.motiffeatures.json` sidecar for
   anyone who needs it.
10. **Motif class naming: auto-numbered `Motif 1..K` in v1.**
    Auto-labelling from dominant HMM-state pattern
    (`"HighSpeed→LowSpeed→HighSpeed"`) reads the interpretation off the
    class centroid — a nicety, not the default. Ship auto-number; users
    rename via the pop manager.
11. **Motif tasks land on the existing behaviour module page.**
    `frontend/src/modules/BehaviourModule.vue` is already
    `ModuleLayout + TaskRunner + SummaryCanvas('behaviourAnalysis')`.
    New JSON task category (`Motif discovery`) appears in the existing
    TaskRunner category dropdown; new `plotDefinitions/*.json` entries
    with `module: "behaviourAnalysis"` get picked up automatically by
    `SummaryCanvas`. No new page, no new sidebar item.
12. **Storage layout.** Sidecars mirror the clustering machinery
    (see `docs/todo/CLUSTERING_PLAN.md` + `docs/todo/CLUSTER_POOLING_PLAN.md`):

    ```
    {proj}/1/{uid}/gating/{vn}__motifs.json             # new POP_MAP_SUFFIX, mirror __clust.json
    {proj}/1/{uid}/labelProps/{props}.motiffeatures.json # mirror {props}.clustfeatures.json
                                                        # keyed {suffix} → { params, medoids,
                                                        # centroidVectors, partOf,
                                                        # resolutionLockedAt? }
    ```

    Per-cell obs (in `{vn}.h5ad`):

    - `motif.class.{suffix}` — categorical string, span-broadcast over
      the motif instance's `(t_start, t_end)`; overlap resolved per
      Decision 9.
    - `motif.distance.{suffix}` — Float32 confidence (DTW distance from
      the instance to its class medoid), span-broadcast.
    - `motif.instance_id.{suffix}` — Int (per-run instance UID) so
      "which instance did this cell belong to?" is answerable.

    Per-track obs (in `{vn}__tracks.h5ad`):

    - `motif.sequence.{suffix}` — categorical string, `"A_B_A_C"`, the
      ordered classes of every motif whose span contains ≥1 cell of the
      track. This is the "track annotated as a sequence of sub-behaviours"
      surface for downstream classifier / KM-survival-style analysis.

13. **On-disk units follow the standing invariant.** Centroids on disk
    are pixels; `centroid_t` is a frame index (`docs/DATAMODEL.md`, also
    restated at `app/src/tasks/tracking/track_measures.jl:216`). µm goes
    through `scale_centroids!` in `app/src/label_props.jl` (equivalently
    `pop_df(...; centroids=:physical)`). Frame → seconds happens at the
    UX layer via `frontend/src/utils/timeAxis.ts`. Motion dimensionality
    reuses `detect_motion_dims` (`track_measures.jl:381`) — don't
    re-derive.

## Phases

Independently-shippable. P1 is the smallest slice that validates the
approach on real data; P2 wires the full v1 with pop_type + UI; P3
adds the deferred pieces.

### P1 — POC (single image, one method, one visualisation)

**Goal**: prove the discovered classes are visible as sub-track
structure on a real dataset before we commit to the full Vue/canvas
surface. ~1–2 focused sessions.

- **Dataset**: `zolIMa/obWDNS/fXgbTl` — 267 tracks, 3259 tracked cells,
  3 HMM states already fitted on `live.cell.hmm.state.movement`,
  physical 0.331 µm/px XY × 2 µm Z (verified via
  `cecelia-observer` MCP).
- **Feature set** (one, not many): 3-channel per-cell stream
  `[live.cell.speed, live.cell.angle, live.cell.hmm.state.movement]`,
  all read via `pop_df(imgs, uids, "live", pops; granularity=:cell,
  centroids=:physical)`. No distance-to-target — that requires the
  Decision-6 work.
- **Detection** (one, not both): STUMPY `mstump` at `windowSize=8`,
  `top_k=100`.
- **Clustering**: Euclidean k-NN → Leiden on the top-100
  feature-vector matrix (skip DTW). `find_populations` from
  `clustering_utils.py` in precomputed-X mode. Target 3 classes at
  `resolution=0.5`; do not tune.
- **Visualisation**: motif ribbon rendered in a *Pluto notebook* via
  `AlgebraOfGraphics` — one lane per track, coloured by discovered
  class. No new InteractiveView, no new Vue component.
- **Files added**:
  - `app/src/tasks/behaviour/motif_discovery.jl` (~200 LOC — mirror
    `hmm_states.jl:44`).
  - `app/src/tasks/behaviour/motif_discovery.json` (~50 LOC).
  - `app/src/tasks/behaviour/motif_discovery_run.py` (~150 LOC —
    reads params, runs STUMPY, JSON out).
  - Task registration in `app/src/tasks/task_registry.jl` (2 lines).
  - `pluto/notebooks/motif_ribbon.jl` reading the banked columns.
- **Validation bar** (real-data visual check per `CLAUDE.md`): on
  fXgbTl the ribbon should show stretches of HMM state 1 (49% of
  cells) broken up into visually-distinct classes if there is
  sub-behavior structure. If every ribbon class maps 1-to-1 to an
  HMM state, matrix profile added no signal on this data — a real
  answer worth banking. If it reveals a class that is "state 2
  preceded by state 3", concept validated. The
  `2 track pair(s) look like one cell` QC finding on fXgbTl is a
  known duplicate-tracks artefact — flag but don't gate.

### P2 — v1 (set-scope, pop_type, cards, cohort-ready)

Only after P1 shows sub-behavior structure.

- **Distance-to-target channel** — Decision 6 lands (new
  `spatialAnalysis.cellContactsPerT` task + shared
  `per_timepoint_min_distance` helper in `spatial_utils.py`).
- **DTW at cluster time** — Decision 4 lands. K×K subsequence DTW via
  `DynamicAxisWarping.jl` on top-K survivors, then Leiden on the
  k-NN graph (`use_rep="X_dtw"` precomputed obsp).
- **Cohort-scale run** — same code, more images. Set at 10–20 images
  runs in minutes on CPU (STUMPY documents linear scaling in n;
  multivariate is O(n·m·d)); DTW is bounded by `top_k` per Decision 4.
- **`motifs` pop_type** — Decision 1 wired end-to-end:
  - `pop_df` / `pop_df_multi` / `pop_namespace` / `resolve_pop_type`
    routing (`docs/POPULATION.md`).
  - Tick-cluster-into-pop UX (mirror
    `docs/todo/CLUSTERING_PLAN.md` Decision 10: `pop/add` with
    `filter: {measure: "motif.class.{suffix}", fun: "in", values:
    ["Approach"]}`).
  - napari colour-by via `napari.show_populations` — cluster pops
    today colour cells by `clusters.{suffix}`; motif pops colour
    by `motif.class.{suffix}`, same code path.
- **SummaryCanvas plots** — two new `plotDefinitions/*.json`:
  - `motifClassFrequency` — 100% stacked bar per image or per
    attribute group; `_matrix_agg` category mode.
  - `motifTransitionMatrix` — from→to grid of motif classes;
    reuses the HMM transitions plot builder.
- **`motifCards` InteractiveView** — mirror `cellCards`
  (`docs/todo/CELL_CARDS_PLAN.md` Decisions 0/1/3/4). One card per
  motif class; medoid = motif instance closest to its class centroid
  in DTW space; filmstrip cropped to `(t_start, t_end)` via
  `render_view_frame` (`api/src/image_render.jl`). Rides the Cell
  Cards Phase-2 StripCell extraction rather than growing its own.
- **`motifRibbon` InteractiveView** — per-track colour-bar over
  time, one lane per (image × track); grouped by `track_plot_groups`
  (`app/src/tracking/track_cohort.jl`). regl-scatterplot rect mode
  OR a purpose-built canvas layer; domain reasoning from
  `plots/trackPaths.ts`'s `pathDomain`.
- **Resolution-lock discipline** — Decision 8 wires the
  `resolutionLocked` param, sidecar receipt, and per-run
  `qc_finding`. Advisory only in v1.

### P3 — deferred / experimental

Only when a real ask lands or a v2 dataset exposes the need. Each is
independently gated.

- **Continuum motif weights** (arXiv 2506.15190) — add `M_weights.{suffix}`
  obsm alongside the hard label. New view type consumes it; hard label
  and pop_type stay unchanged. Decision 3 in §Open governs whether the
  slot is *reserved* in v1's sidecar shape.
- **Discrete-alphabet motif search over HMM state sequences** — the
  prompt's third method. Direct substring / edit-distance search on
  `live.cell.hmm.transitions.movement` (already a `"prev_cur"`
  categorical, 9 symbols on fXgbTl). Additive to matrix-profile
  motifs, not a replacement — a different query shape.
- **Motif frequencies as a `clustTracks` input feature** —
  reverse Decision 7. Only under a specific research question, and
  only after deciding what "motif frequency vs sequence-order" means
  as a feature.
- **Auto-labelled class names** — Decision 10 reversal, once a
  reliable centroid-description scheme is written.
- **Per-t neighbourhood composition as a channel** — derivable from
  `spatialGraph/{suffix}.h5ad` (node identity `(valueName, label)`
  is per-cell-per-frame for a live image, so per-t neighbours are
  structurally there — verify by opening the h5ad; no shipping
  helper does it today). ~50 LOC composition helper. Cost: shipping
  a whole feature nobody asked for yet.
- **Validation baselines** — Berman fly-motion, Luxem VAME, Vissat
  relative-motion, Gurarie BCPA, Garriga EMbC (all cited in the
  prompt). None ships in Cecelia. Half-session comparison via a
  Pluto notebook running one (e.g. EMbC in R) on a handful of
  tracks and checking correlation with matrix-profile output —
  worth doing once the classes are stable; not a blocker.

## Open questions (Dominik's call before implementation starts)

Copy of the report's §6, kept here so the plan is self-contained.

1. **Confirm Decision 1** — `motifs` as its own pop_type vs.
   annotation on `trackclust`. Recommendation: new pop_type. This
   choice ripples through §Storage and §UX.
2. **Confirm Decision 7** — motif class NOT feeding back into
   `clustTracks` in v1. Symmetric with the HMM stance. Reversal is
   cheap later (one-line addition to `track_props.jl` on-read
   aggregation); the question is whether we should pre-commit to it.
3. **Continuum obsm slot reservation.** Should v1's storage layout
   pre-reserve `M_weights.{suffix}` as an obsm slot so a v2 continuum
   layer is a pure additive migration? Cheaper to reserve than to
   migrate later; the cost is a small amount of unused schema surface.
4. **Resolution-locking enforcement point.** Decision 8 lands
   advisory. Escalation options: task-level `ParamValidationError`
   when a locked run is re-run at a different resolution; board-side
   warn only; auto-lock the first time a run is used in a cross-group
   comparison. The failure mode is scientific integrity of a
   comparison, not data corruption — an advisory + a sidecar receipt
   is arguably the right ratchet. Confirm.
5. **DTW dependency addition.** Verify `DynamicAxisWarping.jl`
   precompiles in `app/`, `api/`, and `pluto/` before the P2 task
   lands. The three-manifest trap (`docs/ARCHITECTURE.md`) has bitten
   before; a `Pkg.add` here without the resolve dance breaks the
   other two envs.

## Files touched (once unblocked)

Grouped by phase for grep-by-file-path.

**P1**:
- `app/src/tasks/behaviour/motif_discovery.jl` (new, ~200 LOC).
- `app/src/tasks/behaviour/motif_discovery.json` (new, ~50 LOC).
- `app/src/tasks/behaviour/motif_discovery_run.py` (new, ~150 LOC).
- `app/src/tasks/task_registry.jl` (+2 LOC).
- `pluto/notebooks/motif_ribbon.jl` (new).
- `pixi.toml` (+1 dep: `stumpy`).

**P2**:
- `app/src/tasks/spatialAnalysis/cellContactsPerT.jl` (new; per Decision 6).
- `app/src/tasks/spatialAnalysis/cellContactsPerT.json` (new).
- `app/src/tasks/spatialAnalysis/cell_contacts_per_t_run.py` (new).
- `python/cecelia/utils/spatial_utils.py` (+ `per_timepoint_min_distance`).
- `app/Project.toml` (+`DynamicAxisWarping`, three-manifest resolve).
- `app/src/tasks/behaviour/motif_discovery.jl` (add DTW cluster step).
- `python/cecelia/utils/clustering_utils.py` (small path for `use_rep="X_dtw"` + precomputed obsp — verify already supported by the existing `find_populations`).
- `app/src/pops/pop_namespace.jl` and siblings — new `motifs` pop_type.
- `app/src/pops/pop_df.jl` and siblings — routing.
- `frontend/src/registries/interactiveViews.ts` — new `motifCards`, `motifRibbon`.
- `frontend/src/plotDefinitions/motifClassFrequency.json` (new).
- `frontend/src/plotDefinitions/motifTransitionMatrix.json` (new).
- No new module page. `BehaviourModule.vue` is untouched.

**P3**: only under an ask. Files enumerated at that point.

## References

Repo-relative paths so the plan survives a checkout anywhere.

- `docs/archive/motif-discovery-audit-prompt.md` — the audit prompt this plan
  answers.
- `docs/ARCHITECTURE.md` — layer boundary, `run_py` contract, three-manifest
  `Pkg.resolve()` trap.
- `docs/DATAMODEL.md` — `centroid_t` is a frame index; centroids on disk are
  pixels.
- `docs/OBJECTMODEL.md` — versioned-field convention (`{value_name → ...,
  "_active": ...}`); `.{suffix}` naming rides this.
- `docs/POPULATION.md` — `pop_df` / `pop_df_multi` / `pop_namespace` /
  `resolve_pop_type`; how a new pop_type is wired.
- `docs/MODULES.md` — task registration pattern (`.jl` + `.json` + optional
  `_run.py`).
- `docs/CUSTOM_MODULES.md` — same, for user drop-in.
- `docs/PLOTS.md` — the plot registry every summary panel goes through.
- `docs/NOTEBOOKS.md` — Pluto notebook surface + AlgebraOfGraphics.
- `docs/inventory/DATA_ACCESS.md` — H5AD readers/writers, `pop_df` accessor,
  `scale_centroids!`, staged store rules.
- `docs/inventory/JULIA_APP.md` — spatial substrate (one graph),
  `pooled_track_frame`, `track_plot_groups`, cluster/region run manifest.
- `docs/inventory/PYTHON.md` — `clustering_utils.py find_populations` /
  `split_back_and_write`, `spatial_utils.py`.
- `docs/inventory/FRONTEND.md` — `ModuleLayout` / `TaskRunner` shell,
  `InteractiveView` registry, image-version advisory pattern.
- `docs/todo/CLUSTERING_PLAN.md` — Decisions 6 (set-scope), 8 (sub-clustering
  via suffix + restricted parent pop), 9 (track feature matrix = celltrackR
  + HMM + transitions), 10 (`pop/add` + `filter`).
- `docs/todo/CLUSTER_POOLING_PLAN.md` — pool across co-clustered value_names,
  `co_clustered_value_names`, `{props}.clustfeatures.json` sidecar shape
  (mirror for `{props}.motiffeatures.json`).
- `docs/todo/CELL_CARDS_PLAN.md` — Phase 1 shipped 2026-09-08; Decisions 0
  (pooled always), 1 (`cellCards` InteractiveView, rail `'clusterPops'`), 3
  (medoid), 4 (image + trace + stats footer). `motifCards` is the sibling.
- `docs/todo/PLOTTING_CANVAS_AND_TRACK_DF_PLAN.md` — `{vn}__tracks.h5ad`
  layout; `motif.sequence.{suffix}` rides this.
- `docs/todo/MULTI_POP_TRACKING_PLAN.md` — per-pop `has_tracks` flag +
  provenance-aware `_write_back` (touches if motif pops need track-
  annotation variants).
- `docs/todo/TRACK_SCHEME_PLAN.md` — timeline metaphor; direct inspiration
  for the sub-behavior ribbon.

Specific file:line anchors used by the audit:

- `app/src/behaviour/hmm.jl:205-274` — `hmm_fit_states` grouping + Viterbi
  decode.
- `app/src/behaviour/hmm.jl:296-358` — `hmm_transitions` produces
  `"prev_cur"` categorical.
- `app/src/tasks/behaviour/hmm_states.jl:44-130` — set-scope task pattern
  (copy for `motif_discovery.jl`); pooling via `pop_df(imgs, uids, "live",
  pops; granularity=:cell)`.
- `app/src/tasks/behaviour/hmm_states.jl:96,107` — writes integer state
  codes as Float64.
- `app/src/tasks/behaviour/hmm_states.json` — task-JSON shape (params,
  `scope: "set"`, `requires: {"axes": ["T"]}`).
- `app/src/tasks/tracking/track_measures.jl:216` — `centroid_t` frame-index
  invariant.
- `app/src/tasks/tracking/track_measures.jl:235-246` — the ten
  `_TRACK_AGG_COLS`.
- `app/src/tasks/tracking/track_measures.jl:381` — `detect_motion_dims`.
- `app/src/tasks/tracking/track_measures.jl:465-475` —
  `live.cell.speed`/`live.cell.angle` write.
- `app/src/tasks/spatialAnalysis/cellContacts.jl:73-85` — the whole-frame
  KDTree that Decision 6 works around.
- `app/src/tasks/spatialAnalysis/contactsMeshes.jl:1-9` — per-timepoint
  mesh surface distance (alternative distance channel).
- `app/src/tasks/spatialAnalysis/cellNeighbours.jl:5` — ONE pop-agnostic
  spatial graph per (vn, suffix).
- `app/src/label_props.jl` — Julia h5ad chain; `scale_centroids!`.
- `app/src/model/image.jl` — `img_track_props_path`, `img_label_props_path`,
  `img_physical_sizes`, `resolve_value_name`.
- `app/src/tracking/track_cohort.jl` — `track_plot_groups`,
  `track_group_paths`, `track_group_diagnostics`, `track_group_frame`.
- `app/src/tracking/track_diagnostics.jl::pooled_track_frame` — pooling
  primitive that offsets `track_id` and tags `source`.
- `app/src/py_runner.jl` — `run_py`, the ONE Python-subprocess launcher.
- `python/cecelia/utils/clustering_utils.py` — shared Leiden engine
  (`find_populations` + `split_back_and_write`).
- `python/cecelia/utils/label_props_utils.py` — `LabelPropsView` chain.
- `python/cecelia/utils/spatial_utils.py` — home for
  `per_timepoint_min_distance` (Decision 6).
- `frontend/src/modules/BehaviourModule.vue` — the module page motif work
  lands on.
- `frontend/src/utils/timeAxis.ts` — the ONE frames → seconds converter.

External method references (see the audit report §8 for the full list
with DOIs / URLs):

- Berman et al. 2014, *Mapping the stereotyped behaviour of freely moving
  fruit flies*, J. R. Soc. Interface — closest published analogue for
  unsupervised motif emergence.
- Luxem et al. 2022, VAME (Nature Comm. Biol.) — closest existing tool for
  the motif → sub-behavior-class clustering step.
- Vissat et al. 2021, relative-motion classification — the "approach-to-X"
  relative-motion analogue.
- Gurarie et al. 2009, BCPA; Garriga et al., EMbC — standard
  movement-ecology segmentation baselines.
- UCR Matrix Profile page + STUMPY (`mstump`) — the detection method
  chosen in Decision 3.
- arXiv 2506.15190 (*Learning Task-Agnostic Motifs to Capture the
  Continuous Nature of Animal Behavior*) — the continuum-motifs reference
  for the discrete-vs-continuum decision.

## Live-probe grounding

Numbers used in this plan (extracted via `cecelia-observer` MCP during the
audit, project `zolIMa/MERTK`, set `obWDNS/MERTK crop`, image `fXgbTl`):

- 267 tracks, 3259 tracked cells.
- `live.cell.hmm.state.movement` — 3 states, fractions 0.48 / 0.34 / 0.18.
- `live.cell.hmm.transitions.movement` — 9 distinct symbols
  `{1_1, 2_2, 3_3, 2_1, 1_2, 3_2, 2_3, 1_3, 3_1}`.
- Physical scale: 0.331 µm/px XY, 2 µm Z.
- Motion dimensionality flagged uncertain (3D) in QC — respected in the
  turning-angle channel choice.
- Existing QC finding: `2 track pair(s) look like one cell` — flag but
  don't gate on POC validation.
