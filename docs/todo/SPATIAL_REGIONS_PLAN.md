# Spatial Analysis + Region Clustering — Parked Plan

**Status:** in-progress on branch `feat/spatial-regions` (Opus pass 2026-07-20; decisions confirmed
by Dominik same day). Source prompt: `cecelia-pineapple/docs/prompts/spatial-region-clustering-prompt.md`.

## Goal

Port the legacy Cecelia spatial-analysis suite and region clustering into new Cecelia
(Julia backend + Python analysis env + Vue frontend), with two additional ambitions the prompt sets:

1. **CytoMAP parity** — produce CytoMAP's analytical readouts *natively* so Gerner-lab users can
   transition off MATLAB without exporting to it.
2. **A genuinely new contribution** — extend region analysis into 4D (x,y,z,t) via **behaviour
   regions** for live imaging (regions defined on the spatial distribution of HMM behaviour clusters).

Two first-class module pages result: **Spatial Analysis** and **Region Clustering**. A new `region`
population type joins `flow`/`clust`/`trackclust`/`live`/`labels`. The headline feature is the
**cross-poptype containment query** ("which regions do my gated CD8+ cells end up in?").

---

## Findings that reshape the prompt's premises

The prompt was written before reading the legacy source, the current env, and the CytoMAP paper.
Three of its assumptions are wrong and the plan corrects them:

1. **"squidpy is already in the env."** It is **not** — `squidpy`/`minisom` are absent from
   `pixi.toml`/`pixi.lock`/`pyproject.toml`. BUT the *legacy* R version used squidpy
   (`cell_neighbours.py` → `sq.gr.spatial_neighbors`), so it is a *returning* dependency. Its
   **incremental weight is negligible**: the heavy transitive tree (numba, llvmlite, scikit-learn,
   statsmodels, networkx, umap-learn, pynndescent, dask) is **already present** — scanpy pulls all of
   it. Squidpy adds essentially itself + `xarray`. **Caveat (found during Phase 0):** adding squidpy
   from **conda-forge** bumps numpy to 2.4.x, which breaks the hard `cellpose==3.1.1.2` pin
   (numpy<2.1). It must be added via **`pixi add --pypi squidpy`** so the PyPI resolver honours the
   numpy ceiling (backtracking to a numpy<2.1-compatible squidpy).

2. **"CytoMAP HMRT/SOM clustering pipeline" + "permutation testing for spatial associations."**
   Neither exists in CytoMAP:
   - CytoMAP's region step is a **1-D SOM chain of NR neurons** (`selforgmap([NR 1])`), NR auto-chosen
     by minimizing the **Davies-Bouldin index** — each neuron *is* a region. There is **no** secondary
     hierarchical ("HMRT") node-clustering step.
   - CytoMAP has **no permutation test and no p-values**. Its "RDP" (Random Distributed Points) is a
     *reference population*, not a statistical test. The only formal statistic in the whole toolbox is
     Pearson's r (Student-t) for the co-localization heatmap.
   → We do **not** port RDP-as-a-test. We use **squidpy's** nulls (`nhood_enrichment` permutation
     z-scores, `co_occurrence`, `ripley`, `spatial_autocorr`/Moran's I), which are more rigorous than
     anything in CytoMAP. An optional uniform-in-mask reference population can be offered for visual
     parity, but it is not the significance mechanism.

3. **"Regions are per-cell."** Legacy Cecelia *and* the cross-poptype query are per-cell (region label
   keyed on segmentation `label`). CytoMAP is **per-window** (raster grid of overlapping circular
   windows, spacing R/2). These are different paradigms — see Decision 2.

Also confirmed: **region clustering *was* implemented** in legacy (`clustRegions/kmeansClust.R`, base-R
`kmeans`, k=`numClusters`) — not SOM. Legacy statistics/significance came from **SPIAT** (R;
`entropy_gradient_aggregated` via a `SpatialExperiment` bridge), which new Cecelia has no equivalent
for → replaced by squidpy.

---

## Locked decisions (2026-07-20)

Numbered so code and other docs can cite them (`Decision N`).

### Decision 1 — squidpy stays; no Julia spatial port

Add **squidpy** (`pixi add squidpy`, heavy tier) as the spatial-graph + spatial-statistics engine.
Rationale: it already backed the legacy version, it is built on AnnData (our dual-native format,
[[project_python_boundary_h5ad]]), all clustering/spatial-stats already live in Python (there is **no**
Julia clustering/NN/Distances dep in `app/Project.toml`), and its statistical layer is strictly better
than CytoMAP's. A Julia port buys nothing and cuts against the architecture. `squidpy` owns: spatial
graph (Delaunay / kNN / radius), `nhood_enrichment`, `co_occurrence`, `ripley`, `spatial_autocorr`.
**Julia owns** only what needs `pop_df`/gating membership: composition-vector assembly and the
cross-poptype query (Decision 6).

### Decision 2 — per-cell neighbourhoods are primary; raster-window is a committed parity mode

The **region label is assigned per cell** (keyed on `label`), because that is what makes the
cross-poptype containment query work (join on `label`, Decision 6) and matches legacy. Per-cell
neighbourhoods come from the squidpy graph (radius / kNN / Delaunay — user-selectable, mirroring
legacy `cellNeighbours` modes).

CytoMAP's **raster-window** paradigm is a **committed mode** (Phase 6; Dominik confirmed 2026-07-20),
sequenced after the per-cell path works end-to-end so it can't block static-image shipping. The two
paradigms differ in *what a neighbourhood is*:
- **Per-cell:** each cell centres its own neighbourhood (its graph neighbours); every cell gets a region
  label directly. Can't represent empty space (no cell to label there).
- **Raster-window (CytoMAP):** lay a regular grid of *overlapping* circular windows over the tissue
  (square grid, spacing = R/2 → ~50% overlap, deliberate smoothing). Each **window** — not each cell —
  is a sample; compute its composition, cluster the window×celltype matrix into regions. Windows are a
  fixed physical size, so region definitions are comparable across samples of different cell density,
  and empty/sparse windows are a real region type ("capsule/empty").

Reconciliation that keeps the per-cell contract: after clustering windows, **assign each cell the region
of the window it falls in** → the cell still carries a `regions.{suffix}` label keyed on `label`, so the
cross-poptype query is unaffected. Raster-window is therefore an added *composition source* on the same
region machinery, not a parallel system. `perTimepoint` (Decision 7) applies to windows too.

### Decision 3 — the composition vector

Per cell, the composition vector = **normalized frequency of each population/cluster among its graph
neighbours** (`freq = n_type / n_neighbours`), matching legacy `cellRegionsStats.R` (`freq = n/sum(n)`)
and CytoMAP's "composition" encoding. Numeric neighbour features use the neighbour **mean**. Also
offer CytoMAP's **z-scored counts** encoding (`(count − mean)/SD` across cells) as an option.
- **Population basis is user-configurable** — all pops, or a subset, across any poptype(s).
- `excludeSelfConnections` and `addSelf` options carried over from legacy.
- Temporal (Decision 7) decides whether composition is per-timepoint or aggregated.

### Decision 4 — region clustering method: Leiden default, k-means, SOM optional

- **Leiden** (default): reuse the existing `clustering_utils.find_populations` engine verbatim
  (`sc.pp.neighbors` on the composition matrix → `sc.tl.leiden` → `sc.tl.umap`). **Zero new deps**,
  consistent with cell/track clustering ([[project_clustering_design]], [[feedback_use_existing_framework]]).
- **k-means** (legacy parity): `sklearn.cluster.KMeans`, k=`numClusters`. sklearn arrives with squidpy.
- **SOM** (CytoMAP parity, opt-in): `minisom` (`pixi add --pypi minisom`) — 1-D chain of NR neurons,
  NR via Davies-Bouldin (`sklearn.metrics.davies_bouldin_score`). Only added if CytoMAP faithfulness is
  wanted; flagged, not default.

**No Julia SOM** (`SOM.jl` not needed — clustering stays in Python per Decision 1).

### Decision 5 — region labels reuse the cluster-pop machinery, in the labelProps h5ad

**This is not a divergence from legacy — it is faithful to it.** Legacy region was AnnData-backed
exactly like `clust` (via `adataUtils`), a first-class poptype distinguished *only* by its backing file
variable (`imRegionsFilepath` vs `imAnndataFilepath`), with region→pop mapping done by clicking a
region-id×population grid (`filterMeasure="regions"`, `fun="eq"`). A cell label already carried both a
cluster label *and* a region label simultaneously — which is the entire point. New Cecelia keeps that
mechanism.

The **only** change from legacy is *file location*: region labels are written as a
**`regions.{suffix}` obs column in the segmentation's existing labelProps h5ad** (via the canonical
`LabelPropsView.add_obs`/`add_obsm`) with a UMAP `X_umap.{suffix}` in obsm — instead of a separate
`{vn}.region.h5ad`. And that change is not a divergence either: it *aligns* region storage with the
decision already made for cell/track clustering in new Cecelia ([[project_clustering_design]]: unified
`clusters.{suffix}` column, no separate `clust.h5ad`). So region uses the identical write path as
clustering (`clustering_utils.split_back_and_write`). A `regions.*` categorical name-rule mirrors the
`clusters.*` rule (`track_props.jl`) so region and cluster labels coexist on the same cell without
colliding. Region **populations** are filter pops (`filterMeasure="regions.{suffix}"`, `fun="in"`),
resolved at query time by the gating engine — no rows written by the task. This reuses the whole cluster
sidecar + `co_clustered_value_names` auto-share machinery, and inherits per-run scoping
([[project_cluster_pop_scoping]]): a region plot MUST resolve its value_name from the run suffix, not
the active segmentation.

### Decision 6 — the cross-poptype query is a Julia `pop_df` join on `label`

`region_membership` / `region_enrichment` are **Julia** functions built on the existing `pop_df`
accessor. Because region labels live as `regions.{suffix}` obs on the same label-keyed table, "which
cells of poptype X are in region Y?" is a `pop_df(pop_type=X)` ⟕ region-column join on
`(uID, value_name, label)` — the "universal join contract" every legacy vignette used
(`spatialDT ⟕ popDT`, stamp `pop.from`/`pop.to`, group-by). This is why membership resolution is Julia,
not Python: it must compose with gating filters across poptypes. Signatures:

```julia
region_membership(img; query_pop=("flow","cd8+cd29+"), regions=("region", ["r1","r2"]))
region_enrichment(img; region=("region","arrest_zone"),
                  query_pops=[("clust","Arrested"),("flow","cd4+"),("live","tcells/tracked")])
```

Both return **flat, MCP-friendly tables** (Decision 9), not matrices.

### Decision 7 — temporal handling is a user parameter (`perTimepoint`)

Carried from legacy `cellRegions.json`: `perTimepoint` (default off). Off → aggregate all timepoints
into one graph (region defined across the timecourse). On → build a graph per timepoint, composition
per (cell, t). Behaviour regions (Phase 7) default to **aggregated-across-time** composition of the
behaviour-cluster distribution, since a "zone" is a persistent territory; per-timepoint offered for
transition analysis. `centroid_t` is excluded from the distance space either way.

### Decision 8 — storage of the four data objects

1. **Neighbour graph** → squidpy's native `obsp` (`spatial_connectivities`/`spatial_distances`) in a
   **new** per-image/value_name graph file `{vn}.spatial.h5ad` (file *creation* is the sanctioned
   Python-task exception; the graph is not appended to the cell table). Path registered like the
   legacy `imNeighboursFilepath`.
2. **Composition vectors** → **not persisted by default** (recomputed; cheap Julia assembly). Optionally
   cached in the spatial file's `obsm` when the user wants reuse.
3. **Region labels** → `regions.{suffix}` obs in the segmentation labelProps h5ad (Decision 5).
4. **Contact / distance / aggregate measures** → obs columns on the cell table, reproducing the legacy
   naming schema exactly: `<popType>.cell.contact#<targetPop>` (bool), `.min_distance#<targetPop>`,
   `.contact_id#<targetPop>`, and `<popType>.cell.is.aggregate` (Decision 10).

### Decision 9 — every readout is MCP-accessible as a flat table

New `GET /api/analysis/spatial` (backed by `app/src/ai/spatial.jl`) + MCP tool `get_spatial_stats`
(mirror `get_cluster_summary`: route → `client.py` `ALLOWED_ROUTES` + method → `@mcp.tool()`). Output
format is **flat, interpretable rows** — `population_pair → mean_distance → z_score → p_value`,
`population × region → frequency`, `population_pair → pearson_r` — never raw matrices. This is a
from-the-start design constraint on every analysis function's stored output, not a retrofit.

### Decision 10 — "aggregate" terminology, not "cluster"

Spatially-grouped same-type cells are **aggregates**, to avoid collision with the `clust` poptype.
Functions `detect_aggregates` / `aggregate_size`; obs `<popType>.cell.is.aggregate`; pop names
`"aggregated"`/`"non-aggregated"`; module labels, QC and lab-log entries all use "aggregate."
Detection: DBSCAN on centroids (`sklearn.cluster.DBSCAN`, legacy `cellClusters`) + a mesh-proximity
variant (trimesh surface distance → connected components) for large objects (legacy `cellClustersMeshes`).

### Decision 11 — mesh contact keeps trimesh; generate on the fly; pre-filter with the neighbour graph

Mesh contact/containment (legacy `cell_contacts_mesh.py`, `cell_clusters_mesh.py`) stays on **trimesh**
— consistency with the existing 3D object-measurement step (same mesh objects), and geometry stays in
Python (the architecture boundary). The legacy's two pain points (slow collision sweep, required saving
meshes) are addressed by *how* it's used, not by switching engines (Dominik raised this 2026-07-20):

1. **Do not persist meshes at segmentation.** Generate on the fly. Two tiers:
   - *Fast/default:* bbox / convex-hull approx meshes from the measure step's existing `bbox_*` columns
     (`morpho_utils.df_to_meshes`) — cheap, no disk.
   - *Accurate/opt-in:* true surface meshes (marching-cubes on the label submask) built **only for the
     candidate cells**, cached in the task-run dir (never the permanent object) if reused.
   Drop the legacy `saveMeshes` path — persisted per-cell surfaces are storage-heavy and go stale on
   re-segmentation.
2. **Candidate pre-filter is the real speedup.** Contacts need exact mesh distance only for pairs that
   are already spatial neighbours. Reuse the Phase 2 neighbour graph (or a centroid kNN) to get candidate
   pairs → O(edges), not O(N²) collision. This is orthogonal to trimesh-vs-Julia and does most of the work.
3. **Parallelise across timepoints** for live imaging (each frame independent; trimesh/fcl releases the
   GIL) — the second lever if the pre-filter isn't enough.

**ImplicitBVH.jl stays parked** (→ `docs/FUTURE.md`): a new Julia geometry dep against the
Python-owns-geometry boundary, justified only if — *after* the pre-filter — mesh distance is still the
measured bottleneck on large PhenoCycler volumes. Measure first.

### Decision 13 — cohort batch integration via Harmony (2026-07-20)

Region IDs must be comparable across a cohort, not confounded by per-sample batch effects — the
cohort-integration idea from NicheCompass (Birk et al., *Nat Genet* 2025), applied to **composition
vectors**, not their GNN (wrong modality for protein imaging + heavy deps; see the dismissal note
below). Implemented **lightweight**: an optional `batch_key` in the shared `find_populations` engine
runs Harmony (`harmonypy`, PCA→`run_harmony`) on the feature matrix before Leiden, clustering over the
integrated embedding. `clustRegions` exposes it as `integrateBatch` (batch = `uID`); it also benefits
cell/track clustering for free. Skipped (with a log) when <3 features (a ≤2-population composition is
~1-D — integration is meaningless). NB `harmonypy` pinned `<1`: scanpy's `harmony_integrate` wrapper
transposes `Z_corr` for the old (d,N) layout and breaks on harmonypy ≥0.2's (N,d) — so we call
`run_harmony` directly and orient the output ourselves.

**NicheCompass dismissed for direct import (2026-07-20):** its niche model is *signalling*-based
(ligand-receptor gene programs, a graph-attention VAE) and needs spatial *transcriptomics*; not
demonstrated on protein/antibody imaging (CODEX/IMC), which is Cecelia's core. Heavy deps (torch-geometric
+ gene-program DBs) cut against the Python-boundary stance. It benchmarks *against* the composition-based
camp (CellCharter/GraphST) — which is what Cecelia does, and the modality-appropriate choice here. Only
the cohort-integration idea was worth taking (this Decision). Revisit NicheCompass/CellCharter only if
Cecelia invests in spatial-transcriptomics niches (the `stomics` vignette).

### Decision 14 — clean population selection: `accepts` allow-list + grouped UI + aggregated pop (2026-07-20)

The `popSelection` widget muddles all poptypes into one flat chip strip (and discards the `popType` the
backend already sends). With flow/live/clust/trackclust/region now all live, this is confusing — and
region-clustering basis must accept BOTH cells and tracks (gated + clustered + region). Redesign
(Dominik's calls):

1. **A function declares an explicit poptype allow-list** — `popSelection` params carry
   `accepts: [...]` (e.g. `["flow","live","clust","region"]`; region basis adds `"track","trackclust"`).
   The function states exactly what it takes; supersedes the coarse `popScope` (kept back-compatible).
   Matches the R per-widget popType / `showAll`.
2. **Grouped, labelled selection (universal)** — each population option gains `granularity` (cell/track)
   + `category` (gated / clustered / region / tracked / aggregated), derived backend-side from
   `pop_type`+`is_track_pop`. The chip UI groups under headers (*Cells · Gated*, *Cells · Clustered*,
   *Cells · Regions*, *Tracks · Gated*, *Tracks · Clustered*). Applies to every popSelection.
3. **Aggregated cells → a reusable population** — `detectAggregates`/`aggregatesMeshes` write the
   `<popType>.cell.is.aggregate` column AND auto-define an "aggregated" filtered pop (filter on that
   column, `> 0`) under the input pop, so it flows into any downstream `popSelection` — the R
   lazy-predicate model (`filterMeasure`/`filterFun`/`filterValues`, resolved at read by `pop_df`),
   auto-created from the cutoff rather than hand-defined.

Build: backend enumeration by explicit poptype list + category/granularity tags + API `accepts` param
(testable) → aggregated-pop auto-creation (testable) → frontend grouped renderer + `accepts` (typecheck)
→ migrate task JSONs to `accepts`. `popScope` stays working through the transition.

**BUILT** (commit 5297a75): `pop_category`, `population_accept_groups` (+ `population_scope_groups` shim),
`ensure_filter_pop!`, `AGGREGATED_POP_NAME`; detectAggregates/aggregatesMeshes auto-create `_aggregated`;
`/api/plots/populations?accepts=` + granularity/category on every path; frontend `popGroups.ts` grouped
picker. Julia 1464, vitest 255. `clustRegions` accepts cells only for now — the track basis lands with
behaviour regions (Phase 8 backend, when a track basis is verified).

### Decision 15 — filter populations live in the existing PopulationManager (no separate manager)

Dominik's call after weighing a separate "Filter Manager": **one manager, not two.** If storage is
unified (a filter pop is just a `Population` in the per-poptype sidecar, resolved by `pop_df` like a
gate — `ensure_filter_pop!` is the programmatic primitive), a second manager is only a filtered *view*
behind an artificial boundary. So: add a **"New filter population"** affordance to the existing
per-poptype `PopulationManager` (reused CRUD: `pop/add`·`update`·`delete`), and make that manager
**embeddable** on other module pages for reach. Muddling (gates vs filters mixed for flow/track) is a
**presentation** concern — badge/group filters distinctly in the list — not an architecture one.
- **Compound filter** (the one model change): `Population` filter gains an optional AND-ed
  `conditions: [{measure, fun, values}]` (back-compatible with single measure/fun/values); `pop_df`
  evaluates all. Reproduces the old `populationUI.R` multi-condition-in-one-pop (up to 12 rows).
- **Colour + all pop attributes apply** (a filter pop is a Population): user-picked colour (palette
  default, not the old random), `show`, label overrides — nothing special-cased.
- Filter on ANY obs measure (cluster/region/aggregate/intensity/speed/HMM…) for ANY poptype — the R
  `filterMeasure/filterFun/filterValues` lazy-predicate model. Dominik: "old app filtered TRUE/FALSE —
  reference not gospel"; kept measure-agnostic (`> 0` is just the flag case).
LAST priority (task #10). See CD8→aggregate→CD69 workflow: chain composes parent∩child, no muddle.

### Decision 16 — Spatial Analysis plots: CytoMAP parity for the Gerner lab

Dominik: "take CytoMAP as inspiration; provide at least similar plots for Gerner people." The Spatial
Analysis + Region Clustering pages need a plot canvas (currently none). Plan, reusing the existing
registry + `SummaryCanvas`/cluster-heatmap framework (docs/PLOTS.md — NOT a bespoke panel):
1. **Region composition heatmap** (CytoMAP signature): region × basis-population mean composition —
   "what each region is made of." ENABLER: save the composition vectors as obs columns in clustRegions
   so the EXISTING cluster-heatmap plot (cluster/region × measures) renders it directly. (verifiable)
2. **Cell-type contact heatmap** (CODEX log-odds, `spatialStats/{suffix}.json`): pop × pop association/
   avoidance. Needs a small data route reading the sidecar → a matrix plot. This is the lean, clustered-
   adjacency form of the "round network connection plot" (CODEX Fig 6); a chord/network view is a
   deferred second option (propose).
3. **Region abundance + per-cell spatial measures** (composition, `is.aggregate`, region) via the
   existing summary family once the composition obs columns exist.
4. **Region UMAP** already produced (`obsm['X_umap.{suffix}']`) — reuse the cluster UMAP canvas.
Mount `SummaryCanvas` on both module pages. Task #6.

**BUILT — Phase 7 (napari region colouring, d4c8ab9):** region toggle in the viewer overlay row;
`resolve_pops`/`show_populations` were already generic over pop_type.
**BUILT — Phase 8 (behaviour regions, e4cc6da):** `clustRegions` `perTimepoint` → per-frame
block-diagonal neighbourhood graph (`build_block_diagonal_graph`); regions vary over time. test-py 97.

### Decision 12 — two module pages, both REPL-runnable

**Spatial Analysis** and **Region Clustering** are separate first-class pages (like segment/track/gate),
each = `ModuleLayout` + `ImageTable` + `TaskRunner(useTaskDefs)` + registry-driven plot canvas. All
task `_run_task` functions run headless from the REPL/tests with no API/Vue (sink-agnostic callbacks).
Spatial Analysis population selectors show **all poptypes** (`flow`/`clust`/`live`/`region`/`branch`).

---

## CytoMAP parity assessment (per readout)

| CytoMAP readout | squidpy? | Plan |
|---|---|---|
| Per-cell / cell-centered neighbourhood | `sq.gr.spatial_neighbors` (radius/kNN/Delaunay) | **squidpy** (Phase 2) |
| Raster-window composition matrix | none | **custom** grid-bin + count (Phase 6, optional) |
| Neighbourhood composition vector | none | **Julia** assembly on squidpy edges (Phase 3, Decision 3) |
| SOM region definition (1-D chain, DB-index NR) | none | **minisom** opt-in; Leiden/k-means default (Phase 4, Decision 4) |
| Pearson-r density co-localization heatmap | no (different stat) | **custom** `np.corrcoef` over composition (Phase 5) |
| Spatial association / null | **stronger** (`nhood_enrichment`, `co_occurrence`, `ripley`, Moran's I) | **squidpy** (Phase 5); RDP not ported as a test |
| Positional remapping (region-colored dots) | `sq.pl.spatial_scatter` (trivial) | interactive **PositionalScatterView** + napari points (Phase 5/8) |
| Region adjacency (% shared-border force graph) | `interaction_matrix` approximates | **custom** border-% metric + network overlay (Phase 8) |
| Per-region population frequency | none (one-line groupby) | **summary plot**, `popType:"region"` (Phase 5) |
| UMAP of composition colored by region | (scanpy umap) | reuse `X_umap.{suffix}` + `UmapView` (Phase 4) |

---

## Canonical questions the module must answer natively (from vignettes)

Every one reduces to the **join contract** (squidpy edges ⟕ `pop_df`, group-by) + a stored measure:

1. Pairwise population/cluster spatial-interaction frequency ("who neighbours whom") — join + group-by.
2. Neighbourhood composition around a specific population ("what surrounds cell X") — per-source group-by.
3. Contact fraction between two pops with distance threshold — `cellContacts`/`cellContactsMeshes`.
4. Distance to a structure/region/border — leading-edge (spatstat→shapely/scikit-image), branch kNN, min-distance.
5. Region detection = neighbourhood-composition clustering ("i-niches") — the Region Clustering page.
6. Region membership ("what fraction of pop X lives in region R") — cross-poptype query (Decision 6).
7. Spatial-association significance ("≥1 B neighbour vs chance") — squidpy `nhood_enrichment` + binary test.
8. Spatial entropy/mixing gradient at increasing radii — squidpy `co_occurrence`/`ripley` (replaces SPIAT).
9. Population density per area/volume — hull area (scikit-image/shapely) → cells per volume.

Contact-column naming schema to reproduce: `<popType>.cell.contact#<pop>`, `.min_distance#<pop>`,
`.contact_id#<pop>`. `branch` (structures) is a first-class cross-query poptype alongside flow/clust/live/region.

---

## `region` poptype — Julia touchpoints (from the integration map)

Add `"region"` at (all in `app/src/gating/population_manager.jl` unless noted):
1. `POP_MAP_SUFFIX` → `"region" => "__region"`.
2. `_is_cluster_pop_type` → include `"region"` (reuse cluster expansion / borrow / auto-share).
3. `is_track_pop` → `false` for region (per-cell), unless a track-scoped behaviour region.
4. `scope_pop_types` → add to the `"cells"` branch (feeds the module popSelection picker).
5. `pop_df` → handled by the cluster-filter branch once (2) applies; add a bespoke branch only if a
   distinct data source is needed.
6. `app/src/ai/observer_summary.jl` `_OBSERVER_POP_TYPES`, `ai/lineage.jl`, `ai/measures.jl` — add for MCP/summary coverage.
7. `lab_log_context.jl` `_category_of_pop_type` — categorize under "Spatial".
- **Frontend:** `popType` is a plain string (no strict union) — runtime keys work; extend the two local
  literals in `cluster/ClusterPlots.vue` + `clusterHeatmapBody.ts`, and `utils/overlayLayers.ts` points list.
- **API** (`api/src/gating_api.jl`, not Revise-tracked — restart): region resolves like `clust`, no change
  beyond confirming default fallbacks.

---

## Build order (phased, independently shippable)

**Phase 0 — Attribution + deps. ✅ DONE (2026-07-20).** THIRD_PARTY.md CytoMAP entry (MIT; Stoltzfus
et al. 2020, DOI 10.1016/j.celrep.2020.107523) + squidpy row added. `squidpy 1.8.3` added via
`pixi add --pypi squidpy` (numpy held at 2.0.2; conda route breaks the cellpose pin). sklearn/numba/
statsmodels/networkx confirmed already present via scanpy.

**Phase 1 — `region` poptype plumbing. ✅ DONE (2026-07-20).** Generalised the cluster-pop machinery so
`region` rides it with a `regions.{suffix}` prefix — **no duplicated logic** (new `_cluster_measure_prefix`
is the single prefix decision). Touched: `population_manager.jl` (`POP_MAP_SUFFIX`, `_is_cluster_pop_type`,
`_referenced_cluster_suffixes`, `_expand_cluster_pops`, `scope_pop_types`), `track_props.jl`
(`_is_categorical_col` name-rule), `plot_data.jl` (`_cluster_matrix_suffix`), `ai/observer_summary.jl`
(`_OBSERVER_POP_TYPES`), `lab_log_context.jl` (`_category_of_pop_type`). `lineage.jl`/`measures.jl` needed
**no** change (cluster-family already excluded / already generic — deliberately not adding region where
clust isn't). Tests added (region pop membership, round-trip, referenced-suffixes, categorical name-rule,
matrix-suffix, auto-share + expand across co-clustered vns); full `test-pkg` suite green (1268 pass).
Frontend literals (`ClusterPlots.vue`, `clusterHeatmapBody.ts`, `overlayLayers.ts`) deferred to the phases
that render region plots/napari (4/5/7). No central pop-type enum needed extending; gating engine filter
membership is already column-generic.

**Phase 2 — Neighbour graph (shared substrate). ✅ DONE backend (2026-07-20); Python runner unverified
end-to-end.** `spatialAnalysis.cellNeighbours` (image-scope): Julia handler `cellNeighbours.jl` (resolves
value_name + optional pop subset via `pop_df` + physical sizes) + `cell_neighbours_run.py` via `run_py`
(squidpy `spatial_neighbors` delaunay/knn/radius; scales pixel centroids → µm; writes `{vn}.spatial.h5ad`)
+ `cellNeighbours.json`. Registered (`Cecelia.jl` include, `task_registry.jl` `_spec_path`+`_fun_name_map`,
exported). QC: pure `_neighbours_qc_findings` (no-cells / no-edges / >50%-isolated) + metrics
(nCells/nEdges/meanDegree) + `COHORT_METRICS`. Reuses `_str_list` from clustPops (module-visible, not
re-defined). Tests: param validation (range enforcement — note `int` enforces min/max, `integer` does
NOT) + QC-helper unit tests; suite green (1278 pass). **Not yet run against real image data** (needs a
live squidpy run + fixture); frontend module page is Phase 5.

**Phase 3+4 — Composition vector + region clustering. ✅ DONE backend, validated end-to-end
(2026-07-20).** Unified into the `clustRegions.cluster` task (set-scope, like clustPops). The graph
obsp is Python-native, so composition is computed Python-side, but **membership stays Julia**: the
handler resolves the basis via `pop_df` and assigns each pooled cell a **(value_name, pop) basis code**
(keying on the pair, so B/qc vs T/qc are distinct across segmentations — found during validation).
`cluster_run.py` pools each image's basis cells across segmentations into **one combined graph** (a B
cell and a nearby T cell are neighbours across segmentations), computes composition via shared
`spatial_utils.neighbourhood_composition` (`freq = n/sum(n)`), pools across the set, clusters (Leiden
via `find_populations` with `axis="NONE"`, or k-means), and writes `regions.{suffix}` + `X_umap.{suffix}`
via `split_back_and_write(col_prefix="regions")` — **generalised, not forked** (Dominik's ask). Shared
`spatial_utils.build_spatial_graph` used by both cellNeighbours and this (one squidpy entry point).
Registered + QC + COHORT_METRICS. Tests: Julia param/dispatch, Python composition unit tests (pure).
**Validated on XcPcu8** (project 4kS67f, images LUkCpP+k3Tx90, B/T basis): 1966 cells pooled →
`regions.btregions` written to all 4 segments, readable as categorical, UMAP present.
- ⚠️ **Over-segmentation with a tiny basis:** 2 basis pops → ~1-D composition → Leiden at res 1.0 made
  49 regions. Real usage has many cell types; small-basis runs want k-means (fixed k) or low resolution.
  Consider a QC finding for "regions ≫ basis dimensionality". *(Follow-up, not blocking.)*
- **Still deferred:** SOM method (minisom), `perTimepoint` temporal composition (Decision 7 — Phase 8),
  the Region Clustering **module page** (Vue) + route/nav (Phase 5 with the spatial page), region plots.

**Phase 5 — Spatial Analysis functions + stats + page.** Contacts (`cellContacts` kNN, `cellContactsMeshes`
trimesh), aggregate detection (Decision 10), distance-to-structure/leading-edge, squidpy stats
(`nhood_enrichment`/`co_occurrence`/`ripley`/Moran's I), Pearson-r co-localization. Spatial Analysis
module page (all poptypes as inputs). Summary/interactive/cluster-panel plots via registries
([[feedback_use_existing_framework]]): per-region frequency bar, composition heatmap, region UMAP,
positional scatter, co-localization heatmap.

**Phase 6 — Cross-poptype query + MCP.** `region_membership`/`region_enrichment` (Julia, flat tables).
`/api/analysis/spatial` + `ai/spatial.jl` + MCP `get_spatial_stats`. (Optional: raster-window parity mode,
Decision 2.)

**Phase 7 — Napari region visualization.** Region-colored points (reuse `show_populations`/`resolve_pops`,
scale+units), region-fill via `colour_labels` on `regions.{suffix}`, region-adjacency network overlay
(new; scope carefully). [[feedback_napari_layer_units]] applies.

**Phase 8 — Live-imaging behaviour regions.** Composition basis = spatial distribution of HMM behaviour
clusters (`clust`); aggregated-across-time default (Decision 7). Scoped separately so it never delays
static-image support (prompt constraint).

---

## Constraints (from the prompt) — carried

- `region` is a dedicated poptype, shown in the population manager (Phase 1).
- All module functions REPL-runnable without API/Vue (Decision 12).
- squidpy evaluation done (Decision 1) before any spatial code.
- CytoMAP attribution on every derived function (Phase 0).
- Region labels in the population map, same mechanism as clusters (Decision 5).
- Cross-poptype query is first-class (Decision 6), not an afterthought.
- Live-imaging extension scoped separately (Phase 8).

## Resolved (Dominik, 2026-07-20)

- **squidpy dep weight** — non-issue; incremental cost is negligible (heavy tree already present via
  scanpy). Added via `pixi add --pypi squidpy` (v1.8.3; conda route breaks the cellpose numpy pin).
  Happy to move CytoMAP functions onto squidpy/scanpy. Not behind an optional extra.
- **Raster-window parity mode** — build it (Decision 2, Phase 6), sequenced after per-cell.
- **`regions.{suffix}` namespace** — confirmed; matches legacy behaviour (a cell label carried both a
  cluster and a region assignment).
