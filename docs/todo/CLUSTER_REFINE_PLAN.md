# Plan — refine clusters: gating with clustering

Status: **P1–P3 built** (2026-11-25, branch `feat/cluster-refine`); P4 (the check on xxHqjQ) open. How it
works now lives in `docs/POPULATION.md` → *Refine runs*. Supersedes the sketch in
[`CLUSTERING_PLAN.md`](CLUSTERING_PLAN.md) Decision 8 (`keepPops` → sub-clustering).

## Goal

Build a population tree from clustering the way a gating tree is built from gates. A coarse,
low-resolution run gives the first level ("Lymphoid, mixed"). A higher-resolution run on **one cluster
population** gives that population's children, and so on down. Each level is its own run with its own
resolution and features, so refining lymphoid cells does not also shatter epithelium or stroma. The
user must be able to **see** the hierarchy: which run refines which population, and at what resolution.

Why not just raise the resolution on everything: a refine run rebuilds the neighbour graph and the
normalisation from the parent's cells alone, so differences inside the parent no longer compete with
the differences between the parent and everything else.

Motivating case: PhenoCycler set `xxHqjQ` (project `mjphFQ`), run `tissues`. "Lymphoid, mixed" =
clusters 0/32/34 (~61k cells); cluster 0 (59,854 cells, the largest of the run) is high for CD20, CD45,
CD8 and gdTCR at once.

The old R `keepPops` tried this and overwrote the previous clusters (`merge_new_adata`). Not ported.

## What already works (verified 2026-11-24)

- **Restricted run.** `clustPops.cluster` `popsToCluster` accepts `clust` pops
  (`app/src/tasks/clustPops/cluster.json`); `pop_df_multi` pools only their cells.
- **No overwrite.** The run writes a new `clusters.{suffix}` obs column. `split_back_and_write`
  (`python/cecelia/utils/clustering_utils.py`) writes only the run's labels; `LabelPropsView.add_obs`
  fills every other cell with NaN, and a `fun: "in"` filter never matches NaN.
- **Child membership.** A pop's cells are its filter ∩ its parent. A `clust` pop under
  `/Lymphoid, mixed` filtering `clusters.lymphoid` computes correctly today; `pop/add` takes `parent`.
- **Own UMAP per run.** `obsm['X_umap.{suffix}']`, computed on the run's cells only; `add_obsm` pads
  rows outside the run with NaN; `api_plots_umap` drops NaN rows (`api/src/gating_api.jl`).
- **Per-run QC, stamp and delete.** QC under `{labelSet}.{suffix}`; run stamp `at` in the
  clustfeatures sidecar marks cluster pops stale; `app/src/analysis_runs.jl` deletes one run and lists
  the runs it `invalidates`.

## Gaps

1. The run does not record what it refines. Its sidecar entry (`_write_clust_features!`,
   `cluster.jl`) holds `features`, `partOf`, `family`, `labels`, `at` — no source pop, and **no
   resolution or other params**, so nothing can show "res 2.0" next to a run or a population.
2. Cluster pops are always created at root: `addClusterPop` (`frontend/src/stores/gating.ts`) and
   `add-cluster-pops` (`api_gating_pop_add_cluster_pops`) send no parent.
3. The pop manager shows one run at a time (`visiblePops` filters to the active `clusters.{suffix}`), so
   a parent from `tissues` with children from `lymphoid` cannot be seen as one tree.
4. The suffix dropdown is a flat list of runs.
5. Nothing starts a refine from a population; the user has to fill the task form by hand.

## Decisions (locked 2026-11-25 with the recommendations below, when built)

1. **A refine is an ordinary clustering run with a recorded source.** No new task, pop type or column
   family. A run whose `popsToCluster` is exactly one `clust` pop records
   `refines: {pop: "<path>", run: "<source suffix>"}` in its sidecar entry. Any other selection (several
   pops, a gate, a region) is a plain restricted run with no lineage.
2. **Lineage is data, not the suffix name.** Suffixes stay flat (`lymphoid`, not `tissues.lymphoid`);
   the tree is built from `refines`. Renaming a suffix never breaks the tree.
3. **Record the run's params.** The sidecar entry also stores `params` (resolution, normalisation,
   transformation, UMAP/PAGA flags) so the GUI and Claude can show what made each level. Older runs read
   back without it (the label shows no resolution).
4. **A refine run's populations default to children of the source pop** — from the pop manager and from
   `add_cluster_pops`, decided server-side from `refines` so the two cannot disagree. The
   one-population-per-cluster rule stays per run.
5. **A cluster pop can only parent cluster pops.** There is one pop tree per (segmentation, pop type)
   sidecar, so a `clust` child cannot sit under a `flow` gate. Restricting a run to a gate stays a plain
   restricted run (Decision 1). See open question 2.
6. **Each refine run keeps its own UMAP** (`X_umap.{suffix}`), a zoomed-in embedding of the parent. No
   shifting of child coordinates into the parent's embedding (the old R left this as a commented-out
   hack). On the parent run's UMAP, the children are visible by colouring by population
   ([`UMAP_COLOUR_FACET_PLAN.md`](UMAP_COLOUR_FACET_PLAN.md)), which already works.
7. **The heatmap of a refine run is z-scored across that run's clusters only.** This already happens
   (profiles are per run); stated so nobody "fixes" it to the whole set.
8. **Staleness flows down.** Re-running the source run, or changing the source pop's clusters, after a
   refine ran makes the refine run stale, and with it its populations. Deleting the source run makes
   it stale too ("run X no longer exists"). Refine runs are NOT listed in the delete's `invalidates`:
   the delete dialog shows that list as "also deleted", and a refine run is not deleted — its column
   stays valid for the cells it clustered. Population definitions stay (user work, IMAGE_DELETE
   Decision 13) — but nested under a source population whose column is gone, they read empty until the
   user moves them (⋯ → Move under…); the run's stale banner says why. Accepted: the tree is the point.
9. **"Refine" starts from the population.** A button on a cluster pop in the pop manager opens Cluster
   cells with `popsToCluster` = that pop, the source run's features and normalisation, resolution one
   step above the source run's, and a suffix from the pop name. Usually only the resolution changes.
   It fills the form; it never starts the run.

## Storage

| What | Where | Refine run adds |
|---|---|---|
| Cluster ids | `labelProps/{vn}.h5ad` obs `clusters.{suffix}`, integer codes, NaN outside the run | one obs column |
| UMAP | same file, `obsm['X_umap.{suffix}']`, NaN-padded to every cell | one 2-column obsm |
| Features, members, stamp | `{props}.clustfeatures.json`, key per (suffix, family) | **+ `params`, + `refines`** |
| QC | `qc/clustPops.cluster/{labelSet}.{suffix}.json` | one entry |
| Populations | `gating/{vn}__clust.json` | children under the source pop |

Each run adds an obs column and a full-length NaN-padded obsm to every segment's h5ad. **Measure the
size and write time on xxHqjQ (~650k cells) in P1 before deciding whether a refine run's UMAP should be
stored only for its own rows.** Every UMAP reader already drops NaN rows, so the padded layout costs
nothing on the read side; only change it if the measurement says so.

A resolution sweep on a refine gives N sibling refine runs of the same pop; the tree shows them side by
side and the user keeps one (delete the rest with the per-run delete).

## Open questions

1. **Cross-run pop-manager view.** Built narrower than recommended: the active run's populations plus
   their ancestors (read-only context rows with run + resolution, click = that run); a refined population
   links to its refine runs. Showing every run at once was noisy with resolution sweeps.
2. **Refine from a gate** (gate → cluster inside it). Recommendation: out of scope; restricted run
   without lineage. Revisit if it is asked for.
3. **Unassigned clusters per level.** Not built: the existing per-pop "n cl" counts and the cluster
   strip already show which ids no population holds. Revisit if it is missed.
4. **Track and region clustering.** Same mechanism (`trackclust`, `region`); build for cells first,
   then check whether the other two want it.

## Phases

- **P1 — backend. BUILT.** Manifest `params` + `refines` (clustPops only; track/region later);
  source-pop default parent in `pop/add` and `add-cluster-pops`; staleness from the source pop and run
  (incl. a deleted run — Decision 8, no `invalidates`); channels `clusterRuns`. Test:
  `api/test/suite/gating_cluster_refine.jl`. **Storage not measured yet** — moved to P4, it needs a real
  refine run on xxHqjQ.
- **P2 — frontend. BUILT.** Pop-manager ⋯ → Refine fills the form via `paramHandoff`; run dropdown as a
  tree with resolution + stale ⚠; context rows; stale banner. Logic in `utils/clusterRefine.ts` (unit
  tested); no browser test (not a fix branch; the prefill values are the unit-tested part). Unassigned
  count not built (open question 3).
- **P3 — Claude. BUILT.** `get_cluster_summary` → `runs`; `add_cluster_pops` places a refine run's groups
  under the source pop; both tool docs say so. No new tool or param, so tools.json is unchanged.
- **P4 — check on xxHqjQ.** Refine "Lymphoid, mixed" at a higher resolution; confirm cluster 0 splits
  (or say why not: bleed-through or segmentation in dense lymphoid areas is not a clustering problem).
  Measure the h5ad size + write time a refine run adds (Storage).
