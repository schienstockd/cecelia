> **ARCHIVED — not authoritative, do not act on this.** A frozen record of an exploratory comparison
> (2026-10-28). It is not a description of how the code works now. The design it led to lives in
> [`docs/todo/TRACK_BEHAVIOUR_AXES_PLAN.md`](../todo/TRACK_BEHAVIOUR_AXES_PLAN.md); the brief is
> [`DIFFMAP_TRACK_COMPARISON.md`](DIFFMAP_TRACK_COMPARISON.md). The scripts and figures were kept
> outside the repo (`~/Downloads/TMP/diffmap/` on the author's machine): they read one dev project by
> absolute path.

# Diffmap vs Leiden on track behaviour: results

Brief: [`DIFFMAP_TRACK_COMPARISON.md`](DIFFMAP_TRACK_COMPARISON.md).
Script: `diffmap_compare.py` (read-only on the project). Figures and CSVs are in
`~/Downloads/TMP/diffmap/`, not committed. Every number below is from `log.txt` in that directory.

## Dataset (what the brief left as FILL IN)

- Project `4kS67f` "unimelb 3P spleen", set `XcPcu8` "B and T". Track cluster run `movement`, joint over
  `B/qc/_tracked` + `T/qc/_tracked`, resolution 0.3, minTracklength 5, 21 features (10 motility, 3 HMM
  state fractions, 8 HMM transition fractions). Recorded params: `XcPcu8/ccid.json` → funParams.
- Names (`gating/B__trackclust.json`): Scanning = Leiden 0, Meandering = 1+2, Directed = 3.
  **No Aggregating cluster exists here.**
- 7 images, 416 tracks (B 144, T 272), 4 mice. The run's `partOf` includes EaMaVq (M4c), which has
  since left the set's `image_uids`; I loaded the run's list, not the set's.
- **No treatment conditions.** The only comparison is B vs T, which I used as the "condition", with the
  mouse as the replicate (n = 4, paired). This dataset has no published result. The reference is the
  open Blackboard entry "best measure to distinguish B and T": T moves faster and further than B in 6 of
  7 images, and M2b (mouse 2) is reversed.
- Where things live: the per-track motility values are in `{vn}__tracks.h5ad` X. The HMM fractions are
  computed when read (`track_props.jl`). I recomputed them from the cell `obs` using the same rule
  (frequency over all of the track's cells). Labels are in `clusters.movement`, the UMAP in
  `obsm['X_umap.movement']`. The neighbour graph is not saved.

## 1. Baseline reproduced

Saved sizes {0:146, 1:114, 2:78, 3:78} match the observer profile exactly. Re-running Leiden on the
rebuilt matrix gives ARI 0.969 against the saved labels. So the feature matrix is right.

## 2–3. Diffmap: is it continuous? Yes, along a single path

`fig3_dc12.png`, `fig3_dc13.png`, `fig3_umap.png`, `fig5_landmarks.png`. Eigenvalues DC1..5:
0.987, 0.975, 0.927, 0.913, 0.909. The kNN graph is one connected component.

DC1 vs DC2 is a **∧-shaped path with no islands**. One arm runs from the Scanning end (HMM state 1 ≈ 1)
up to the apex (state 2 ≈ 1, the Meandering core). The other runs down to the Directed end
(state 3 ≈ 1). Tracks are mixtures of states 1+2 or states 2+3, almost never 1+3. The three named
clusters are **contiguous segments of that one path**.

The geometry is the HMM-occupancy simplex: 11 of the 21 features are HMM fractions, and the transition
fractions duplicate the occupancy fractions. Post-hoc check: Spearman(DC1, the track's mean HMM state)
= **0.978**. **DC1 is essentially "mean HMM state",** and that can be computed without a diffusion map.

## 4. Axis names (Spearman, `dc_feature_spearman.csv`)

| | strongest correlations | proposed name |
|---|---|---|
| DC1 | speed 0.92, hmmS3 0.86, hmmT3_3 0.85, hmmS1 −0.84, displacement 0.77, turning −0.62 | slow/scanning → fast/directed |
| DC2 | hmmS2 0.92, hmmT2_2 0.89, hmmS3 −0.49 | "how pure state 2 is" (the apex of the ∧) |
| DC3 | duration −0.50, num_cells −0.51, straightness 0.50, T3_2 −0.56 | track length/duration vs straightness; mixed |
| DC4–5 | no correlation above \|0.52\| | not nameable |

Exemplars (`fig4_exemplars.png`): the low end of DC1 is short, tangled tracks at 1–2.5 µm/min. The high
end is long, near-straight T tracks at 10–19 µm/min. DC3's low end is long, wandering Meandering
tracks. DC1 and DC2 are clean; DC3 is mixed.

## 5. Landmarks: contiguous slices, not islands

Silhouette (named clusters): DC1–5 0.34, raw features 0.32, UMAP 0.50. On DC1 the three named clusters
don't overlap at all (AUC 0.00, histogram overlap ≤ 0.02). That is close to built in: Leiden and the
diffusion map are both built from the same kNN graph, so Leiden cuts along the graph's slowest mode.
PAGA (Leiden 0..3) is a chain: 0–1 0.08, 1–2 0.26, 2–3 0.25, 0–3 0.004.

## 6. B vs T at mouse level (n = 4 paired; `condition_effects.csv`, `fig6_condition.png`)

| readout | B | T | d_z | p (paired t) | same sign |
|---|---|---|---|---|---|
| %Scanning | 0.48 | 0.29 | −0.35 | 0.54 | 3/4 |
| %Meandering | 0.47 | 0.49 | 0.06 | 0.92 | 2/4 |
| **%Directed** | 0.05 | 0.21 | **1.46** | **0.06** | 3/4 |
| median DC1 | −0.025 | 0.001 | 0.35 | 0.54 | 3/4 |
| mean DC1 (post-hoc) | −0.018 | 0.005 | 0.43 | 0.45 | 3/4 |
| mean HMM state (post-hoc) | 1.69 | 2.05 | 0.52 | 0.38 | 3/4 |
| median DC2 / DC3 | — | — | ≈0 | 0.99 / 0.76 | 2/4 |
| median speed (reference) | 3.9 | 6.0 | 0.68 | 0.27 | 3/4 |

Every readout points the same way, and the same mouse (2, image M2b) reverses all of them, as the
Blackboard entry already noted. **None reaches p < 0.05 with 4 mice.** The continuous axis does **not**
detect more than the cluster proportions do. %Directed has the largest effect, and DC1 is weaker by
d_z. A median is also a poor summary of DC1 because DC1 has several peaks: a mouse's median sits in
whichever arm holds the majority (fig 6), and the mean only partly fixes that. Nothing contradicts the
earlier B/T observations.

## 7. Aggregating

The brief's check can't be run here: there is no Aggregating cluster. As a stand-in I used the
per-track fraction of aggregate cells (`flow.cell.is.aggregate`, which is set only on T). Only 9 tracks
have ≥ 50 % aggregate cells, and they don't separate on any DC (AUC 0.39–0.58). They sit on the
motility path; there is no branch. This doesn't tell us how an interaction-driven Aggregating cluster
from another dataset would behave.

## 8. Stability (`stability.csv`)

I matched each DC to the reference by the correlation between their feature-correlation profiles.

- n_neighbors 10/15/30: DC1 and DC2 1.00. DC3 1.00 / 1.00 / 0.94. **Changing the seed does nothing:**
  the diffusion map is deterministic once the neighbour graph is fixed, so the seed rows are trivially
  the same.
- 80 % subsamples ×5: DC1 and DC2 1.00 every time. DC3 0.81–0.99, and in one subsample it swapped to DC4.
- Second "dataset" (no other annotated dataset was at hand, so I embedded B only and T only): DC1
  0.89 / 0.98, DC2 0.91 / 0.99, DC3 0.74 / 0.99.

**DC1 and DC2 are stable. DC3 is not.**

## 9. Reference layouts

`fig9_drawgraph.png` uses igraph's FR layout, because fa2 isn't installed. PHATE is not installed and I
did not install it.

## Assessments (no implementation)

- **Version A (per-track DCs instead of Leiden):** on this feature set, DC1 ≈ mean HMM state
  (ρ 0.98). The same continuous readout is available today as a per-track mean of a column we already
  have, with no new embedding and no plumbing.
- **Version B (windowed segments in diffusion space):** each track would become a path, but you lose
  what the HMM gives you: named discrete states, a transition matrix you can compare between
  conditions, and dwell times. The diffusion map here mostly *recovers* the HMM states, so Version B
  would likely rebuild the HMM less interpretably. Not worth it on this evidence.
- **Binned zones:** cutting DC1 at the cluster landmarks gives back the clusters themselves (zero
  overlap, §5). Cutting mean HMM state at 1.5 / 2.5 is the same idea with no embedding.

## Decision (revised): justified for interpretation; sensitivity untested

The first verdict ("partially justified, a view, not a replacement") weighed the brief's sensitivity
rule, which this dataset can't test (4 mice, no conditions, mouse 2 reversed). On the question that
matters (can users name what they see?), the answer is yes: one ∧ path, contiguous clusters, two axes
that can be named and are stable. Sensitivity is still open and needs a dataset with real conditions.

## Plan checks (`plan_checks.py`, `plan_checks_*.csv`, `fig_plan_motility_dc12.png`)

Only what the build plan depends on. Each check ran on 6 variants: n_neighbors 10/15/30 and three
80 % subsamples.

1. **The axis names need the HMM features.** On motility features alone, DC1 = +straightness (0.95–0.96)
   in all 6 variants, but DC2 changes from variant to variant: overallAngle, trackLength, displacement
   or speed. The DC1–DC2 plot is an arch with the named clusters mixed together, and the path position
   only reaches ρ 0.49–0.56 with speed. With the full feature set: DC1 = +speed/+hmmS3 and DC2 =
   +hmmS2/+hmmT2_2 in all 6 variants. **So a continuous view is only nameable on the HMM-inclusive set
   (the set clustering already uses).**
   Side finding: `overallAngle` is the direction a track points in the image frame. It describes
   orientation, not behaviour, and it drives the unstable motility-only DC2. That's worth a look for
   anyone choosing cluster features. I'm flagging it, not removing it.
2. **Automatic naming works for DC1 and DC2, not DC3.** Each axis is flipped so its strongest feature
   correlates positively, then labelled with its top 2 features. DC1 and DC2 get the same label in 6/6
   variants. DC3 changes between hmmT3_2, straightness and similar. Show DC1 and DC2 only.
3. **Path position works with an automatic root.** `sc.tl.dpt` rooted at the low end of the sign-aligned
   DC1 orders Scanning < Meandering < Directed in 6/6 variants. Median path position by cluster is
   0.18–0.20 / 0.70–0.77 / 0.91–0.95, and ρ with speed is 0.90–0.91. This gives one number per track,
   named by its ends, and the clusters become zones along it.

## Concrete plan

- **P1: path position as a track measure (small).** In the track-cluster runner, compute
  `sc.tl.diffmap` + `sc.tl.dpt` on the neighbour graph Leiden already builds. Write
  `obs['path.{suffix}']` and `obsm['X_diffmap.{suffix}']`, plus the automatic DC1/DC2 labels and the
  root rule, to the run's metadata. Because it's an ordinary per-track measure, the existing
  track-measure plots, boards and per-mouse stats pick it up with no new plot code. The write-back
  already supports this (`split_back_and_write` → `LabelPropsView.add_obs/add_obsm`). Touchpoints:
  `clustering_utils.py`, `clustTracks/cluster_run.py`, and run deletion in `analysis_runs.jl`, which
  must drop the new keys too. Plus a test.
- **P2: DC1 × DC2 as a second layout in the cluster view (medium).** This replaces the hard-coded
  `X_umap.{suffix}` with a basis choice: the gating API scatter (`gating_api.jl`), `label_props`
  helpers, and the frontend cluster view picker. The axis labels come from P1's metadata.
- **Decision for you:** the root rule. Automatic (low end of DC1, which worked here) or "start at
  cluster X" (the user picks e.g. Scanning, which is more explicit). Also: should cell clustering get
  the same treatment, or only tracks?
- **Not planned:** windowed segments (Version B), PHATE, new dependencies.

## Unverified / caveats

- The HMM fractions were recomputed in Python, not read through `track_props`. ARI 0.969 says they are
  right, but they are not bit-exact.
- `funParams` stores only the *last* clustering run's params (`there`). I'm assuming `movement` used the
  same params, because all four runs have identical sizes.
- The "post-hoc" rows (mean DC1, mean state) were added after I looked at fig 3. Treat them as
  hypothesis-generating.
