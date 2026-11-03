# Spatial pattern of one population, compared between groups

Status: in-progress (2026-11-03) · branch `feat/spatial-pattern` · P1 built + validated; P2 built (panel unseen); P3 open

## Goal

Answer "is population X randomly distributed, and is its pattern the same in condition A vs B?"
inside the app, with the image as the statistical unit. The trigger was a reviewer asking that about
TRITC+ cells in PBS vs DTx lymph nodes, answered in the old R framework with spatstat (Clark–Evans per
image, a t-test across images, then a studentised permutation test on the L-function). The legacy
dataset will not be migrated. This plan builds the general readout in the new framework, validated
on new-framework data.

What the R analysis taught, which shapes the decisions below:

1. **Which cells count as the tissue decides the answer.** On the same 12 images, Clark–Evans R moved
   by up to 0.2 depending on whether the window was the hull of the target cells (the legacy `ppp`
   default) or the hull of all segmented cells. A target-hull window cannot see a population confined
   to one part of the tissue, which is the usual biological answer.
2. **"Random" is rarely the claim that survives.** CSR was rejected in 10/12 images. The defensible
   claim is "the same pattern in both groups", which needs a group test on per-image values, not a
   per-image CSR test.
3. **Batches can dominate.** Two acquisition batches differed more than the two treatments did. The
   group comparison has to show which images are which, not just a pooled p.

## What already exists (audit, 2026-11-03)

| Need | Exists | Gap |
|---|---|---|
| Cell positions in µm, 2D or 3D | `spatialGraph/{suffix}.h5ad` → `obsm['spatial']` + node identity (`SPATIAL_REGIONS_PLAN` Decision 17) | none |
| Target membership composed with gating | `pop_df_multi` + `_basis_segments` (`app/src/spatial.jl`) | none |
| A label-shuffle null, within frame | `pairwise_contact_logodds` (`spatial_utils.py`, Decision 18) | it scores population PAIRS on graph edges, not one population's own spacing |
| Group test, image as the unit, split by image attribute | `plot_summary_data` `stat_unit=:image` + `attr_map` + `stats.jl` | only for per-cell/per-track measures from `pop_df`; per-image numbers from a sidecar cannot reach it |
| Nearest-neighbour ratio, pair-count curve, whole-curve group test | — | new |

## Decisions (2026-11-03)

### Decision 1 — the reference set is the neighbour graph's nodes

The task LOADS a named neighbour graph and treats its nodes as "the tissue": the cells the target is
compared against. It takes no reference-population picker of its own. Which cells make up the tissue
is chosen once, when the graph is built (`spatialAnalysis.cellNeighbours` → *Cells*), the same way a
neighbourhood is defined once for interactions and regions (Decision 17 of `SPATIAL_REGIONS_PLAN.md`).
Only node coordinates and identity are used; the graph's edges are not.

Consequence to state in the UI tip and docs: if the graph was built over labelled T cells and the
target only, "random" means "spread like those cells", not "uniform over the node".

### Decision 2 — the null is random labelling over the reference, within frame

Target membership is shuffled across ALL graph nodes (target counts per frame fixed), and per frame
when the graph is per-timepoint. This is the random-labelling null of point-pattern theory (Baddeley,
Rubak & Turner, *Spatial Point Patterns*, CRC 2015, DOI 10.1201/b19708, ch. 14). It needs no window
and no edge correction, because target and reference share the same positions and the same edges.
It reuses the within-frame grouping of `pairwise_contact_logodds`, extracted to one helper
(`permutation_groups`) so there is one shuffle scheme in the codebase.

Differs from `neighbourStats` on purpose: that test shuffles only basis cells among themselves,
because it asks how basis types are arranged relative to each other. Here the question is where the
target sits among all cells, so non-target nodes take part in the shuffle.

### Decision 3 — three readouts per target population

1. **Nearest-neighbour ratio** (headline scalar): mean target→target NN distance divided by its mean
   under the shuffle. 1 = random relative to the reference, <1 = clustered, >1 = dispersed. Two-sided
   empirical p with the +1 correction, as in Decision 18.
2. **Pair ratio curve** g(r): observed target–target pairs within distance r divided by the
   random-labelling expectation, `pairs_ref(r) · nT(nT−1) / (nR(nR−1))`, summed over frames, plus a
   2.5–97.5% envelope from the shuffles. This is K_TT(r)/K_RR(r). 1 = random at that scale.
3. **Clark–Evans R** against CSR in the reference's convex hull: descriptive only, no edge correction
   (Clark & Evans, *Ecology* 35:445–453, 1954, DOI 10.2307/1931034; 3D expectation
   Γ(4/3)·(3/(4πλ))^{1/3} from Chandrasekhar, *Rev Mod Phys* 15:1–89, 1943, DOI
   10.1103/RevModPhys.15.1). Kept because it is the number a reviewer asks for by name. It is not the
   tested quantity: no edge correction, and a hull window has the problem lesson 1 describes. Frames
   with fewer than 20 reference cells are not scored (NaN when none qualify): the hull of a handful
   of points has almost no area, and R then reads 2–5 for an ordinary pattern.

### Decision 4 — coordinates as the graph stores them; always scored within a frame

2D stays 2D and 3D stays 3D, in µm. No projection. An axis with no spread (a single plane stored as
3D) is dropped before computing hull size, so a flat stack doesn't report zero volume.

A time-lapse is always scored frame by frame, even when its graph pooled the frames
(`perTimepoint=false`, no `_t` on the nodes). There, the runner reads each node's frame from its
segmentation's cell table (`spatial_utils.graph_node_times`). Without that, a cell's nearest node is
itself one frame later. Found on the first real run: 4kS67f's `immune` graph pools 44 frames with
~4 B cells each, and the pooled NN ratio read B as far more clustered than it is. Repaired, not
warned: no pooled reading of a per-cell distance is meaningful for live data.

### Decision 5 — its own sidecar: `spatialPattern/{suffix}.json`

Not `spatialStats/`. That directory's readers (`_collect_contact_stats`, the `stats` run namespace,
the interaction-matrix run picker) list every `.json` in it as an interaction run. A flat record per
target population (Decision 9 of `SPATIAL_REGIONS_PLAN.md`): `pop`, `nTarget`, `nReference`,
`nFrames`, `nnObserved`, `nnExpected`, `nnRatio`, `zScore`, `pValue`, `ceR`, `hullSize`, and the
curve as `r`, `pairRatio`, `envLo`, `envHi`. The observer's `spatial_summary` carries it as
`patternStats`.

### Decision 6 — each target population is tested on its own

`pops` is a multi-select; each population is scored separately against the whole graph. A cell in
two selected populations counts in both. No pairing between targets, since that is `neighbourStats`.

### Decision 7 — QC

Pure helper, advisory, like `_neighbour_stats_findings`:
- no target cells in the graph → warn;
- some target cells missing from the graph (coverage < 90%) → warn, rebuild the graph over these
  segmentations;
- fewer than 20 target cells → warn, too few to judge;
- target is ≥ 95% of the graph → warn, the shuffle has nothing to compare against; build the graph
  over more cells.

Metrics: `nTargetMin`, `targetFractionMax`, `nSignificant`. Cohort metrics: `nTargetMin`,
`nSignificant`.

### Decision 8 — group comparison reads per-image numbers through ONE source (Phase 2)

A per-image scalar stored in a sidecar (this task's `nnRatio`/`ceR`, `neighbourStats`' per-pair
log-odds, the anisotropy scalar) reaches the summary plots through one new precomputed source in
`plot_summary_data`, not one route per readout. Each image is one point, series are image-attribute
groups (`attr_map`), and `stats.jl` tests them with the image as the unit, as `stat_unit=:image`
already does for cell measures. This is the rule of three: three readouts already produce per-image
numbers no plot can group by condition.

### Decision 9 — the whole-curve test is Hahn's studentised permutation test (Phase 3)

Compare groups of per-image g(r) curves with the studentised permutation test (Hahn, *JASA*
107:754–764, 2012, DOI 10.1080/01621459.2012.688463), the method behind spatstat's
`studpermu.test` used in the R analysis. Implemented ourselves (no Python package ships it), with
golden values from spatstat on a fixed input. Exact enumeration when the number of distinct
relabellings is ≤ the permutation count, since small groups make random permutations repeat.

## Phases

### P1 — `spatialAnalysis.spatialPattern` task (per image)

- `spatial_pattern_utils.py`: `pattern_test` (NN ratio + pair-ratio curve) and `clark_evans_r`;
  `spatial_utils.py`: `permutation_groups` (extracted, `pairwise_contact_logodds` switched to it) and
  `graph_node_times`.
- `spatial_pattern_run.py` + `spatialPattern.jl` + `spatialPattern.json`; registry; path helpers
  `img_pattern_*`; run kind in `analysis_runs.jl`; `patternSuffixes` in the image payload + frontend
  namespace; cohort metrics; `patternStats` in `spatial_summary`.
- Tests: Python (null calibration on uniform labels; a clustered target reads < 1 with small p; the
  analytic pair expectation equals the shuffle mean; CE ≈ 1 on a Poisson process in 2D and 3D;
  within-frame shuffle never moves a label across frames). Julia (dispatch, bad param, QC helper).
- Real-data check (done 2026-11-03):
  - **FtGoJO (4rNbMp), still, 49 067 cells in 3D.** Reference = all cells. The top 10% of every
    channel's mean intensity read clustered (NN ratio 0.62–0.80, p at the floor; g(52 µm) 1.2–3.1
    against an envelope of 0.98–1.02). A random 10% read NN ratio 1.004 (p 0.51), g 1.00. On a dense
    still image the descriptive Clark–Evans R tracks the NN ratio (0.63 vs 0.62 …). Done on the
    statistic directly: the image has no gates, and none were written into the project.
  - **4kS67f, live 3P spleen, end-to-end through `run_task` on 7 images.** Exposed Decision 4's
    pooled-frame bug and the sparse-frame CE guard. With ~10 QC'd cells per frame these movies are
    too sparse to say much about B/T placement; they validated the plumbing, not the biology.

### P2 — per-image numbers in summary plots, by attribute

Built 2026-11-03. `IMAGE_METRIC_SOURCES` + `image_metric_frame`/`image_metric_summary`
(`app/src/plotting/image_metrics.jl`): a run's sidecars → one row per (image, population) → the shared
`_summary_agg`. Sources `spatialPattern` (`nnRatio`, `ceR`, `zScore`, `nnObserved`) and
`neighbourStats` (per pair: `logOdds`, `zScore`); the anisotropy scalar is the next entry. Request
field `imageMetric` on `/api/plot_data` (+ `suffix`, `pop`). Specs `spatial_pattern_summary.json`
("Spatial pattern by group") and `spatial_interactions_by_group.json`; `isPrecomputedSpec` covers
`dataSource.imageMetric`; the panel plots ONE of the run's populations at a time (header picker), so
the test compares attribute groups, not populations. Checked headless on 4kS67f grouped by Mouse;
the rendered panel has not been looked at.

### P3 — curve plot + whole-curve test

Per-image g(r) lines coloured by group with the Hahn test p in the response; golden values from
spatstat.

## Out of scope

- Inhomogeneous K / intensity-weighted nulls, tissue masks as windows, mark correlation.
- CSR envelopes in a hull window (the descriptive CE R is enough; see Decision 3).
- Equivalence testing ("show the groups are the same"): report effect + CI; say so in docs.
