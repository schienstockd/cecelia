# Spatial waves and behaviour maps: plan

**Status:** parked (2026-11-28). Nothing built. Waits on `regionEdge` (uncommitted on `feat/tissue-edge`)
reaching main. Source prompt: [`docs/archive/SPATIAL_WAVES_PROMPT.md`](../archive/SPATIAL_WAVES_PROMPT.md).
Reviewed once (2026-11-28) against the code. The decisions below are proposals that need the
maintainer's sign-off. None are locked.

## Goal

Two questions, one shared bands-and-profile core:

1. **Static waves.** Along a signed distance to a region edge, which populations peak where, in what
   order, and which pairs co-occur beyond a null that keeps the shared gradient? First case: image
   2dR91B (PhenoCycler, 2D, ~171k cells, follicle-centric; project mjphFQ, set xxHqjQ "HuMAP tissues").
2. **Behaviour maps as an environment sensor (live).** Intravital movies rarely show the tissue around the cells
   (sometimes a little SHG). Hypothesis: behaviour reflects that local environment. Test: build a behaviour
   region map from each co-imaged population on its own, then ask whether the maps agree place by place. If they do, the
   cells are reading a shared environment that can't be seen. First case: project RFtn9f, three naive CD8 TCR
   transgenic lines (OT-I, gBT-I, P14). **All recipients are naive (no antigen)**, so the lines are near-replicate
   probes and agreement is the expected result. In a later infected cohort, the cognate line breaking away from the
   bystanders' map is the readout.

The aim is to make static spatial methods work on live data in the same framework. Novelty is not the aim.

## What already exists (checked 2026-11-28)

| Piece | Where | Relevance |
|---|---|---|
| Distance to region edge | `spatialAnalysis.regionEdge` (`feat/tissue-edge`): `edge_dist#<name>` (µm, unsigned) + `inside#<name>` (0/1, absent for the all-cells root); shape from centroids closed with a radius (default 75 µm, grid step = radius/15); frame never counts as edge; `fillHoles` auto = filled for the tissue, kept for a region (user can override); **shape recomputed per frame**; QC = maximum over frames. Writes contour lines only, **not the mask** | The axis for question 1 |
| Proximity profile | `proximityProfile` + `proximity_bands.jl`: distance to the nearest structure cell, bands from 0 (`d ≤ width` → band 1), structure cells not counted, log2(share in band / share in image) | Extended by Decision 2. Cannot take a signed axis as it stands |
| Stratified label shuffle | `spatial_utils.permutation_groups` (shuffle within groups, today per frame) + `_permutation_expected`, shared by the contact test and Spatial pattern | Decision 4 is a new grouping key, not a new null |
| Behaviour regions | `SPATIAL_REGIONS_PLAN.md` Phase 8b (#2380): `clustRegions` on a "Fixed windows" graph (S = 50 µm, 25 µm tiles), `pooling = "movie"` gives one region per tile for the whole movie, and every cell takes its tile's region. The cells-per-window density is a clustering feature. `includeOther` (default on) counts non-basis cells | The behaviour region is already built. Which pooling RFtn9f's `regions.A` used is unchecked (the default is `frame`) |
| Censored dwell of a per-cell flag | `MOTILITY_METRICS_PLAN.md` P3 (Decisions 6–7) | `inside#<name>` is such a flag. P3 below feeds it rather than rebuilding it |
| Time crop | `editImages.cropImage` box has `t0`/`t1` | K1 can split a movie, but the halves then need the pipeline rerun |

RFtn9f, from the record: 16 movies with a 3-state HMM on `speed` + `angle`, roughly 45–100 tracked T cells
per frame (via get_behaviour_summary). 180 frames × 10 s, 8 planes × 5 µm, field ~266 µm; channels AF,
P14-ubiTom, gBT-uGFP, OTI-CTV (via get_image_info). 5 movies are flagged "Whole field is drifting".

## Proposed decisions

### Decision 1: signed distance is derived
`signed = inside ? -edge_dist : edge_dist` (negative inside, QuPath/Enable convention). A tissue run
(no `inside` column) counts every cell as inside, so its signed distance is `-edge_dist`.

### Decision 2: waves extend Proximity profile
- **Distance source:** nearest structure cell (today) or a `regionEdge` run, signed.
- **Bands below zero:** bands run symmetrically on both sides of the edge. With an edge source, the cells that
  make the region are counted, because the B cells of a follicle belong in its profile.
- **Peaks and order come from density per tissue area, not share.** A share is a closed composition: in a
  dense B core everything else reads depleted. Share stays as the second view.
- **Tissue area per band** needs `regionEdge` to persist its mask (a small raster beside the contour JSON)
  with the grid step. The tissue and region runs can differ in radius, so the step differs too; resample the area to the band edges.
- **Statistics:** per population peak, centroid, FWHM, onset (10% cumulative), order rank. One tidy table
  `(image, region, piece, population, bin, n, area, density, share)`, with the run parameters.

### Decision 3: a piece ID per cell
`regionEdge` returns the `ndi.label` piece per cell (the QC already labels). Multi-part regions are
handled per piece, then aggregated.

### Decision 4: the interaction null is stratified by distance bin
Pass a frame × bin key to `permutation_groups`. Both Interaction matrix and Spatial pattern get a
within-bin null for free. Under strata the expected value must be the permutation mean (as the
conditional null already does), not the global frequency. This reopens ground
`SPATIAL_PATTERN_PLAN.md` excluded (tissue masks, inhomogeneous nulls). Say so there when it lands.

### Decision 5: circularity is broken by population, not by feature
A region built from line A's states is circular only for line A. Lines B and C did not draw it, so their
behaviour, HMM state and speed included, is a legitimate readout. One map per line:
`clustRegions` with `basisPops` = that line's HMM states, **`includeOther` off**, a window graph over **that line's
cells only** (otherwise the other lines enter as "other" and through the cells-per-window density column),
`pooling = "movie"`. That is three runs per movie, and nothing new to build.

### Decision 6: the test is map agreement, not rings
- **Statistic:** per tile, each line's behaviour summary (state fractions, or fraction of time arrested).
  Agreement = correlation across tiles for each line pair (3 pairs), plus agreement of the region labels.
  This starts as a notebook. Promote it to a task only if it earns its place.
- **Null:** toroidal shift or rotation of one line's map against the other's. This keeps each map's spatial
  structure and breaks the alignment. Do not permute states across cells: that gives noise maps that are
  too easy to beat.
- **Unit:** per movie, then summarised per mouse (check the number of mice with `get_experiment_design`
  before any test).
- **Rings** (distance transform of the tile raster, 25 µm steps) are optional. They answer only "is there a
  gradient around a zone". A pooled-frames option on `regionEdge` is not needed: its 75 µm default radius
  would fill a 266 µm field, and pooled centroids are weighted by dwell.

### Decision 7: what could fake agreement, and the control for each
- **Optics, not tissue.** A deep, dim or scattering patch gives jittery tracks and false arrest in every line.
  Control: per-tile depth, signal and track-quality covariates. The lines carry different labels (CTV, GFP,
  tomato), so a purely optical artefact should hit them unequally.
- **Cells influencing each other.** B slowing where A sits can be crowding or a shared DC, not the place.
  Control: A's map from the first half of the movie, B's readout from the second half, with a gap or with tracks spanning both halves dropped. A place
  keeps its property without A present at the same time; an interaction does not. Check contact rates
  (Cell contacts) too.
- **Drift.** It misaligns tiles over the movie. Drift-correct first or leave the movie out (5 are flagged).
- **Sparse sampling.** At ~15–35 cells per line per frame, many tiles are thinly visited. Set a minimum visit count per tile,
  and drop tiles below it in any line.
- **SHG, where present,** is the one visible check: does the shared behaviour map line up with structure?
  This is the same per-box approach as the SHG × HMM notebook in project 4kS67f.

## Phases

**P0: prerequisites.** `regionEdge` on main; Decisions 1 and 3; the persisted mask (Decision 2). Small.

**P1: static waves on 2dR91B.** Decision 2 + Decision 4.
- **Calibration first:** simulate a pure gradient (populations placed by distance alone) and confirm the
  stratified null reports no pairwise signal. Then a planted interaction and confirm it is found.
- **Kill or simplify if:**
  - (b) a peak moves by more than half the coarser bin width between 1× and 2× cell diameter bins, or
    between closing radii 50/75/100 µm;
  - (d) a second image or region gives a different order. This is the independent test, because the
    expected order in (a) was read off 2dR91B itself.

  On a fail: keep the binned profile, drop the peak and order summary.
- Pairwise signal that survives the within-bin null is the finding, not a failure.
- **"(AF)" populations:** a second clustering on `segment.afCells`-corrected markers (#2485). Run both and treat
  agreement as a robustness check.

**P2: behaviour maps on RFtn9f.** Independent of P1, so it can run any time. No code to build: three `clustRegions`
runs per movie (Decision 5) plus a notebook (Decision 6).
- **K1 does each map persist?** Compare a line's map from the first half with its map from the second half
  (`cropImage` `t0`/`t1`, then rerun the pipeline on each half, which costs time). If a line doesn't agree with itself, it
  can't agree with another line, so stop.
- **K2 do the lines agree?** Pairwise tile agreement beats the shifted null in most movies, holds under
  the cross-time split (Decision 7), and survives the region resolution and window size (S = 50 vs 75 µm).
- **Kill:** no agreement beyond the null, or agreement that the depth/signal covariates fully explain.
  Then report it as a negative result: in these movies behaviour is not a readable environment sensor at
  this scale.
- **Pass:** keep the protocol for the infected cohort, where the cognate line diverging from the
  bystanders' map is the question.

**P3: edge dynamics. Dropped as a separate phase.** Radial velocity (step on the distance gradient) is
the only new readout here. Dwell, entries and exits come from `MOTILITY_METRICS_PLAN.md` P3 fed with `inside#<name>`.
Revisit only if P2 passes on an infected cohort.

## Excluded

- Per-region shape statistics and normalised depth: no question needs them yet.
- Cross-K over level sets, sDD space, SASHIMI: Spatial pattern and Interaction matrix cover this ground.
- Class-probability weighting: cluster labels carry no probabilities.
- Distance to the follicle *core*: it needs a new anchor. Add it only if the edge axis blurs the order.
- New dependencies.

## Open questions for the maintainer

1. P2 needs no new code and doesn't depend on P1. Run it first, as the cheaper test of the live idea?
2. The behaviour summary per tile: HMM state fractions (the existing 3-state `movement_3`), or a simpler
   fraction of time arrested?
3. Unverified citations in the prompt (PMC13620163, arXiv 2512.06116, 2609.27755) must be checked
   before anything cites them.
