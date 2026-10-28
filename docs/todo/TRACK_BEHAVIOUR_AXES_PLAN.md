# Track behaviour axes: path position + diffusion map beside the UMAP

Status: P1 built (2026-10-28, PR #1821); R (renames) built on `rename-cluster-map`; P2 open. Evidence:
[`docs/archive/DIFFMAP_TRACK_COMPARISON_RESULTS.md`](../archive/DIFFMAP_TRACK_COMPARISON_RESULTS.md) (set XcPcu8, project 4kS67f).

## Goal

Users find it hard to name clusters that are just blobs on a UMAP. A track-cluster run should also
produce:

- **path position**: one number per track that places it along the behaviour continuum, named by
  its two ends (e.g. "scanning → directed").
- **two named axes** (DC1 × DC2) as an alternative to the UMAP layout.

The named clusters stay. On this path they are contiguous zones, so "more tracks in the scanning zone"
still works.

## Evidence (summary)

- On the clustering feature set (motility + HMM states + transitions), the tracks form a single path:
  HMM state 1 → 2 → 3. Clusters are contiguous segments of it, not islands.
- DC1 = +speed/+HMM state 3 and DC2 = +HMM state 2, with the same labels in 6/6 runs (n_neighbors
  10/15/30 and three 80 % subsamples). DC3 and later are unstable.
- Path position (`sc.tl.dpt`, automatic root) orders Scanning < Meandering < Directed in 6/6 runs,
  ρ 0.91 with speed.
- On motility features alone, only DC1 (straightness) is stable. The HMM features are what make the
  axes nameable.
- Dropping the self-transitions (1_1, 2_2, 3_3) changes neither the axis names nor the path order.
- No sensitivity gain over cluster proportions could be shown, but the dataset can't test it (4 mice,
  no treatment conditions). Re-check on the first dataset that has conditions.

## Decisions (2026-10-28)

1. **Tracks only.** Cell clustering (phenotype) has shown no continuum, so it is out of scope until a
   dataset asks for it.
2. **Root = Auto, showing what it picked, and reversible.** Auto = the low end of the sign-aligned
   DC1. The run records the cluster that holds the root (shown in P2 as e.g. "Auto (Scanning)").
   The override is *Reverse path* (start at the high end), **not** a cluster pick. Cluster codes don't
   exist until the run assigns them, so a task param can't name one, and a path has only two ends to
   start from. (Revised from "pick a cluster" while building P1.)
3. **Axes are named by the run itself, never by hand.** Each DC is flipped so its strongest feature
   correlates positively, and carries its top 3 features by Spearman ρ (labels show 2). Only DC1 and DC2 are
   written and shown.
4. **Stored the same way as the UMAP.** In each `{vn}__tracks.h5ad`:
   - `obsm['X_diffmap.{suffix}']` (DC1, DC2) sits next to `obsm['X_umap.{suffix}']`.
   - `obs['path.{suffix}']` is a per-track column, so it is an ordinary track measure that existing
     plots, boards, gating and per-mouse stats pick up.
   - The axis names, root end and resolved root cluster go in the run's `clustfeatures.json` entry
     as `behaviourAxes`.
5. **Computed from the neighbour graph Leiden already builds.** No second kNN and no new dependency
   (`sc.tl.diffmap` / `sc.tl.dpt` are in scanpy). Always computed for track runs; no toggle.
6. **The feature set stays the user's choice.** Self-transitions stay. `overallAngle` (track direction
   in the image frame) is not a default; it was picked for the XcPcu8 run, and it is what makes the
   motility-only DC2 unstable. That is guidance for whoever picks features, not a code change.
7. **Rename the two names the plot outgrows, before P2** (one PR). The cluster tasks' `mergeUmap`
   param becomes `createUmap` (it never merged anything, and it's what the Python side already calls
   it). The `umap` plot/view family becomes `clusterMap`, shown as "Cluster map", because it will
   show two layouts. Saved data keeps working: parsers and the board reader accept the old key and
   write only the new one (the `analysis_boards.jl` read-both pattern). `X_umap.{suffix}` and
   `sc.tl.umap` keep their names, since those really are UMAP.

## Phases

### P1: compute + store (backend, small)

- `python/cecelia/utils/clustering_utils.py`: after Leiden, `diffmap` + `dpt` on the same graph;
  sign-align and auto-label DC1/2; resolve the root (Decision 2).
- `split_back_and_write`: write `X_diffmap.{suffix}` (via `add_obsm`) and `path.{suffix}` (via
  `add_obs`), alongside the UMAP.
- `app/src/tasks/clustTracks/cluster_run.py` + `cluster.json`: a *Reverse path* bool (Advanced).
  Write the axis names and root into `clustfeatures.json`.
- `app/src/analysis_runs.jl`: deleting a run must also drop `X_diffmap.{suffix}` and `path.{suffix}`
  (`_clust_run_keys`, unit-tested).
- Tests: `test_clustering_utils.py` (path follows a synthetic slow → fast continuum, sign alignment,
  reverse, raw-feature naming, skip on tiny/no-graph input, `extra_obsm` write-back);
  `analysis_runs.jl` (run key ownership); `typed_params.jl` (`reversePath`).
- Done 2026-10-28. Checked read-only on XcPcu8: same axis names as the exploration, root in Scanning,
  path ρ 0.91 with speed, axes 0.02 s.
- Checkpoint: `path.{suffix}` plots as a track measure on an existing board with no frontend change.

### R: renames (`mergeUmap` → `createUmap`, `umap` → `clusterMap`), Decision 7

- Task params: the three cluster tasks' `.json` + parsers (old key as fallback), and the task form
  reading a saved `mergeUmap` so an off setting doesn't reappear as on.
- Plot family: frontend view + registry + export + linked selection + cluster module, the board spec
  (`analysis_board_spec.jl`) and board reader (old `umap` view key read, `clusterMap` written), the
  plot route, the MCP plot family + guidance, docs.
- Done 2026-10-28. Param renames now have a generic home: a spec param's `formerKeys` (read by the form
  and `_apply_spec_defaults`; docs/MODULES.md). View key `clusterMap` (`VIEW_ALIASES` /
  `BOARD_VIEW_ALIASES`, saved boards migrated on read), plot family `cluster-map` (marks accept `umap`),
  component `ClusterMapView`, route `/api/plots/cluster-map`. The picker says "Cluster map"; the panel
  title still says "UMAP (suffix)" until P2 adds the layout switch.

### P2: diffusion axes as a cluster-view layout (medium)

- `api/src/gating_api.jl` `api_plots_cluster_map`: add a `basis=umap|diffmap` param in place of the hard-coded
  `X_umap.{suffix}`, and return the axis labels.
- `frontend/src/components/plots/ClusterMapView.vue`: a layout picker; axis titles from the run labels in
  place of the fixed `UMAP1`/`UMAP2`; an optional colouring by path position.
- `app/src/analysis_board_spec.jl` `"umap"` view: persist `basis` in the view state.
- Checkpoint: on XcPcu8, a domain expert confirms the DC1 × DC2 picture is easier to name than the
  UMAP (a visual check, per CLAUDE.md, documented in the PR).

### Later (trigger: a dataset with real conditions)

- Repeat the sensitivity comparison (cluster % vs path position, replicate-level). Record it here.

## Not planned

Windowed track segments in diffusion space (loses the HMM's states and transition matrix), PHATE
(licence), a second kNN, cell clustering.
