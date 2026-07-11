# Parked plan — Population summary plot + open UX queue

Status snapshot for a session compaction. See `docs/todo/README.md` for the convention.

## A. Population summary plot — IN FLIGHT on branch `feat/phenotype-population-summary` (NOT merged)

**Goal.** One generalised "population summary" plot — cell/track **counts / proportion per
population, across images** — hosted per popType on the module pages + the analysis board. Port of
the old R `popsSummary` (boxplot / `geom_quasirandom` / jitter over `pop.n` / `pop.freq`).

### Locked decisions
- **One backbone, split by module page** (each spec carries ONE popType, so a page's summary canvas —
  and the board's picker — is popType-homogeneous). The board then follows the ACTIVE slot's popType.
- Two views from the same backbone:
  - `count` → one bar per `(pop, image)`; `normalize` (Proportion toggle) → fraction.
  - `boxplot`/`violin`/`strip`/`bar` with **no cell measure** → each **image** is a data point (its
    pop count/proportion), grouped by pop → within-pop variability + between-pop comparison.
- **Proportion denominator = `(uID, value_name)`** — clusters within B and within T each normalise to
  their OWN tracked population per image (NOT pooled B+T). ("split by pop".)
- **Keep the legend** (user reconsidered — needed when split by pop).

### Done (committed on the branch)
- Backend `app/src/plotting/plot_data.jl`: `_population_metric_frame` (per-`(value_name,pop,uID)`
  count/fraction) + `_summary_agg` detects `measure===nothing` + distribution chart → runs the normal
  distribution builders over per-image rows; `count` branch honours `normalize`; denominator keyed by
  `(uID, value_name)`. Tests in `app/test/runtests.jl` ("plot count ... population summary").
- Specs `app/src/plotDefinitions/population_summary{,_tracks,_clust,_trackclust}.json` — modules
  `phenotype`(flow) / `behaviourAnalysis`(live,track) / `clustPops`(clust) / `clustTracks`(trackclust);
  chartTypes `[boxplot,strip,violin,bar,count]`; `normalize` param; all `whiteboardCompatible`.
- Frontend: new **Phenotype** page (`PhenotypeModule.vue`, route `/phenotype`, sidebar after Gate);
  cluster pages host a collapsible `SummaryCanvas` below the cluster canvas; `SummaryPanel` hides the
  measure picker + sends no measure for a measure-less (population) spec, and shows the Proportion
  toggle for these; boxplot/strip raw points get a themed `stroke` so whitish colours read on the
  white PDF/light ground.
- Board: `useSummaryData` takes `activeSpecId` → popType/granularity follow the active slot's spec
  (LayoutCanvas passes it). Prune-vanished-pops watches are **popType-aware** (only prune the current
  popType's keys) so activating a trackclust slot no longer wipes the live/track plots' selections.
- Board rail (`.lc-rail`) got right padding.
- Docs: `docs/PLOTS.md` (population summary section).

### OPEN / to verify after reload
1. **Cluster pages missing the section?** User reported the population summary missing on Cluster
   cells / Cluster tracks. Wiring looks correct (specs valid, module strings match, `#below-table`
   renders); likely below-the-fold under the tall cluster canvas OR needs a page reload to fetch the
   new spec JSONs. CONFIRM: is the "Population summary" collapsible present (scroll below the cluster
   canvas)? If absent → render/hosting bug; if empty dropdown → backend not serving the clust specs.
2. **Dot contrast** shipped as a themed stroke (outlines all points). If the user wants a true BLACK
   FILL only for whitish series (not an outline on everything), that needs a per-series darkened
   point colour — Plot shares one color scale per plot, so it needs a second identity color channel
   or precomputed per-point fill. Refinement, not done.
3. **Board mixed-popType colour degradation** (known limitation, not a reset): while a plot of popType
   X is active, `popColors` only holds X's colours, so other-popType plots keep their selections but
   may show pops in the fallback grey. Clean fix = per-slot pop data on the board (bigger refactor).
4. **Verify end-to-end** on project `XcPcu8` (board: pop summary track clusters + behaviour track
   measures/HMM) and `4kS67f`/img `3w4IY5` (B/T trackclust): (a) selecting the pop-summary slot no
   longer resets other track plots; (b) proportions split by B/T; (c) boxplots render.
- After merge: clean up `main`, delete the branch.

## B. Pending UX queue (separate branches; from the running task list)

- **#32 Image strip: channel colour legend** — optional legend on the image strip pulling each
  channel's colour from the napari image layers + channel names (coloured caption like the reference).
- **#36 Image strip: screengrab provenance + zoom-to-source** — store which image/timepoint/napari
  viewer state a screengrab came from (napari-animation serialises viewer state), add a zoom button
  next to re-grab to reopen that exact state.
- **#37 Napari: movie/recorder toggle** — a "show recorder" button that pops up napari-animation to
  record an animation. (#32/#36/#37 are the image-strip/napari batch — need napari running to verify.)

## C. Notes
- Backend `plot_data.jl` is Revise-tracked (reload picks it up); plot-definition JSONs are read from
  disk per request (no restart). `api/src/*.jl` need a restart.
- Task tracker drift: `#48` (module-page drag) is DONE (merged PR #121); `#39` done (Phenotype).
- Every population-summary change must keep `pixi run test-pkg` (currently 868 pass, 1 pre-existing
  broken) and `frontend` typecheck + 85 vitest green.
