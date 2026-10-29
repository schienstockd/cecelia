# Diffmap vs Leiden for track behaviour clustering: comparison task

> **ARCHIVED — not authoritative, do not act on this.** A frozen record of what was asked at the
> time. It is not a description of how the code works now, and not instructions to re-run. Current
> design lives in `docs/<AREA>.md` and `docs/todo/*_PLAN.md`.
>
> **Outcome:** run on branch `diffmap-track-comparison` (2026-10-28) on set XcPcu8 (project 4kS67f,
> B vs T, 4 mice; no treatment conditions, no Aggregating cluster). One continuous S1→S2→S3 path with
> contiguous clusters; DC1/DC2 nameable and stable only with the HMM features included; sensitivity
> untestable at n=4. Plan: path position (dpt) as a track measure first, diffmap layout second.
> Write-up: [`DIFFMAP_TRACK_COMPARISON_RESULTS.md`](DIFFMAP_TRACK_COMPARISON_RESULTS.md); plan:
> [`docs/todo/TRACK_BEHAVIOUR_AXES_PLAN.md`](../todo/TRACK_BEHAVIOUR_AXES_PLAN.md).

Work in a separate worktree. This is an **exploratory comparison in a throwaway notebook/script**. Do not change Cecelia plumbing (`X_umap.{suffix}`, Julia, API, UmapView) in this task.

## Background

Current track behaviour pipeline (published, Nat Commun 2025, fig. 4):
whole-track measures + HMM states + HMM transitions -> `sc.pp.neighbors` -> Leiden -> UMAP (visualisation only).
Clusters are named by hand (Scanning, Meandering, Directed, Aggregating) so users can say "scanning increased under condition X".

Observations motivating this:
- "Tracks alone" UMAP is one round blob cut into 3 Leiden pieces, which looks like a continuum.
- Clusters are mixtures of HMM states, so they look like discretised HMM occupancy.
- Hard clusters make condition effects lossy: a partial shift (scanning -> meandering) shows up as two cluster-% changes.
- Aggregating is probably a separate branch (interaction-driven, not intrinsic motility).

## Hypothesis

Motility behaviour (scanning -> meandering -> directed) is largely continuous. A diffusion-map representation with named axes could (a) show this, (b) detect condition effects more sensitively than cluster proportions, (c) still allow plain-language reporting.

## Constraints

- Licenses: only BSD/MIT/Apache/LGPL-compatible deps (project is GPL-3-or-later). `scanpy` `sc.tl.diffmap`, `sc.tl.paga`, `sc.tl.draw_graph` are fine.
- PHATE: **local reference only**, never added as a dependency, never committed to requirements/env files. Note: its repo license is reportedly Yale non-commercial; verify the LICENSE file yourself rather than trusting this note.
- No new heavy dependencies. Reuse the existing feature matrix and AnnData if available.

## Dataset

- Path / project: `<FILL IN: dataset already analysed with the current approach>`
- Conditions: `<FILL IN: e.g. naive / 1 dpi / 2 dpi>`
- Replicate unit (mouse/animal/movie): `<FILL IN>`
- Existing cluster labels and names: load them from the saved analysis, do not recompute them.

First, find where the feature matrix, HMM outputs, cluster labels and neighbour graph are stored for this dataset. Report what you found before building anything.

## Tasks

1. **Reproduce baseline.** Load the existing features and Leiden labels. Confirm cluster counts and per-condition cluster proportions match the saved analysis.
2. **Diffmap.** On the same features and neighbour graph settings: `sc.tl.diffmap` (record n_comps, eigenvalues). Also compute PAGA on the Leiden clusters.
3. **Does it look continuous?** Plot DC1 vs DC2 (and DC1 vs DC3) coloured by: existing Leiden cluster, each HMM state occupancy fraction, key raw features (speed, straightness, displacementRatio, meanTurningAngle), condition, cell type if present. Include the UMAP side by side.
4. **Name the axes.** For each of the first 3-5 DCs, report Spearman correlation with every input feature and every HMM occupancy fraction. Propose a plain-language meaning per axis (e.g. "DC1 = persistence/speed") and name the endpoints using exemplar tracks (show a few track trajectories at each end).
5. **Cluster landmarks.** Plot the named clusters as centroids on the diffmap. Are they contiguous slices along an axis, or disjoint islands? Quantify (e.g. silhouette in DC space, overlap of DC1 distributions between adjacent clusters).
6. **Condition effects, two ways.**
   - Baseline: per-replicate cluster proportions per condition (as in the paper).
   - Continuous: per-replicate **median** position on each named axis per condition.
   Test both at the replicate level (no pooling tracks across replicates, to avoid pseudoreplication). Compare: which detects the condition effects more sensitively, and do they agree with the published findings?
7. **Aggregating check.** Does Aggregating sit on its own branch (PAGA edge weights, DC separation) or on the motility continuum? If separate, say so and propose handling it as a discrete class.
8. **Stability.** Re-run with different n_neighbors (e.g. 10/15/30), subsampled tracks (80%), and different seeds. Report whether the axes and their feature correlations stay the same. If possible, repeat on a second dataset to check that DC1 means the same thing.
9. **Optional, reference only:** `sc.tl.draw_graph` (ForceAtlas2) and local PHATE, only to compare how the continuum looks. Skip if it costs much time.

## Also assess (short, no implementation)

- Version A: per-track diffusion coordinates replacing the Leiden step.
- Version B: windowed track segments embedded in diffusion space, so tracks become paths (a possible alternative to the HMM). Note what would be lost relative to the HMM's interpretable states and transition matrices.
- Binning an axis into named zones (low/mid/high), with boundaries set by cluster landmarks, so users can still say "more tracks in the scanning zone".

## Output

A notebook or script plus a short `RESULTS.md` containing:
- Figures from tasks 3, 4, 5, 6.
- A table of correlations of DCs with features (task 4).
- Condition-effect comparison table: Leiden proportions vs axis shifts, with replicate-level p-values.
- Stability findings (task 8).
- A **decision**: one of
  - *Continuous axes justified* for motility clusters (state which axes, proposed names, and how aggregating is handled),
  - *Partially justified* (continuous view as an add-on to named clusters),
  - *Not justified* (clusters are disjoint islands, or axes unstable): keep Leiden.
- If justified: an estimate of the plumbing work to support a second embedding, given the hard-coded `X_umap.{suffix}` name (do not implement).

## Decision rules (apply honestly, do not tune to get a positive result)

- Axes count as meaningful only if the feature correlations are clean and stable across n_neighbors/subsamples (and a second dataset if available).
- Continuous analysis is worth building only if it detects at least one condition effect that cluster proportions miss, or explains the partial shifts more clearly, without contradicting the published results.
- If clusters appear as disjoint islands in diffusion space, say so and stop.

## Reporting

Be concise. Flag anything you could not verify. Do not claim a result without a figure or number backing it.
