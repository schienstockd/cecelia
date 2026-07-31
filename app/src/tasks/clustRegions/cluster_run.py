"""
clustRegions.cluster (set-scope) — Python runner.

Spatial region clustering by neighbourhood composition (docs/todo/SPATIAL_REGIONS_PLAN.md, Phase 3+4).
For each image it LOADS the shared neighbour graph (`spatialGraph/{suffix}.h5ad`, built once by
`spatialAnalysis.cellNeighbours`), attaches the basis labelling to its nodes, computes each cell's
neighbourhood composition vector (fraction of neighbours in each basis population — the CytoMAP /
cecelia "i-niche" feature), then clusters the pooled composition vectors across the whole set (Leiden or
k-means) so region IDs are comparable across images, and writes `regions.{suffix}` back per segmentation.

Nothing here builds a graph: the same graph backs the interaction statistics, so a region's
neighbourhood and a measured contact mean the same thing. Legacy parity — old-R
`clustRegions/kmeansClust.R:45` also read the persisted neighbour table and joined populations onto it.

Membership + basis-code assignment happen in Julia (clustRegions/cluster.jl); this script gets, per
segment, the member labels and their basis-population codes.

Params: suffix, segments [{uID, valueName, propsPath, labels, popCodes}], basis (population names ↔
code index), graphPaths {uID: path}, graphSuffix, compCols (composition column names, aligned to basis),
otherCol, includeOther, clusterMethod ("leiden"|"kmeans"), numClusters, resolution, createUmap,
randomState, qcOutPath.
"""
from collections import defaultdict

import numpy as np
import pandas as pd

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.script_utils as script_utils
import cecelia.utils.spatial_utils as spatial_utils
import cecelia.utils.clustering_utils as clustering_utils
from cecelia.utils.atomic_io import write_json_atomic


def run(params):
    log = script_utils.get_logfile_utils(params)

    suffix     = script_utils.get_param(params, "suffix", default="default")
    segments   = script_utils.get_param(params, "segments", default=[])
    basis      = script_utils.get_param(params, "basis", default=[])
    graph_pths = script_utils.get_param(params, "graphPaths", default={})
    graph_sfx  = script_utils.get_param(params, "graphSuffix", default="default")
    inc_other  = bool(script_utils.get_param(params, "includeOther", default=True))
    n_basis    = len(basis)

    if len(segments) == 0 or n_basis < 2:
        log.log("[ERROR] clustRegions: no segments or <2 basis populations")
        return

    import anndata as ad

    # ── per image: LOAD the shared graph → attach this basis → composition ──
    by_uid = defaultdict(list)
    for seg in segments:
        by_uid[seg["uID"]].append(seg)

    comp_blocks, obs_blocks, coverages = [], [], []
    for uid, segs in by_uid.items():
        gp = graph_pths.get(uid)
        if gp is None:
            log.log(f"[ERROR] clustRegions: no graph path for {uid}")
            return
        a, obs_all = spatial_utils.load_graph(gp)
        # attach the basis labelling to the graph's nodes (the (valueName, label) join)
        codes_all, coverage = spatial_utils.pop_codes_for(obs_all, segs, n_basis)
        if coverage == 0.0:
            log.log(f"[ERROR] clustRegions: none of {uid}'s basis cells are in graph '{graph_sfx}' — "
                    "was the graph built over these segmentations?")
            return
        obs_all = obs_all.copy()
        obs_all.insert(0, "uID", uid)
        # Composition is computed over the WHOLE graph — every neighbour counts, including cells outside
        # the basis (the "other" bin) — but only BASIS cells are then clustered and labelled. The graph
        # now spans more cells than the analysis (it is pop-agnostic), and a non-basis cell must not pick
        # up a `regions.{suffix}` value for a basis the user did not put it in.
        comp = spatial_utils.neighbourhood_composition(
            a.obsp["spatial_connectivities"], codes_all, n_basis, include_other=inc_other)
        keep = codes_all >= 0
        comp = comp[keep]
        obs_all = obs_all.loc[keep].reset_index(drop=True)
        m = spatial_utils.graph_metrics(a)
        log.log(f">> {uid}: graph '{graph_sfx}' {m['nCells']} cells, {m['nEdges']} edges, "
                f"mean degree {m['meanDegree']:.1f} → {int(keep.sum())} basis cells "
                f"({coverage * 100:.1f}%)")
        comp_blocks.append(comp)
        obs_blocks.append(obs_all)
        coverages.append(coverage)

    if not comp_blocks:
        log.log("[ERROR] clustRegions: no cells pooled")
        return

    # column names of the composition matrix: the basis, plus the trailing "other" bin when included.
    # Julia supplies both (one namer) — `compCols` aligned to `basis`, then `otherCol`.
    comp_cols = list(script_utils.get_param(params, "compCols", default=[]))
    other_col = script_utils.get_param(params, "otherCol", default=None)
    if len(comp_cols) != n_basis:
        log.log(f"[ERROR] clustRegions: compCols ({len(comp_cols)}) does not match basis ({n_basis})")
        return
    if inc_other:
        if other_col is None:
            log.log("[ERROR] clustRegions: includeOther is set but no otherCol was given")
            return
        comp_cols = comp_cols + [other_col]

    X = np.vstack(comp_blocks).astype(np.float32)
    obs = pd.concat(obs_blocks, ignore_index=True)
    if X.shape[1] != len(comp_cols):
        log.log(f"[ERROR] clustRegions: composition has {X.shape[1]} columns, expected {len(comp_cols)}")
        return
    log.log(f">> {X.shape[0]} cells pooled; composition over {n_basis} populations"
            + (" + other" if inc_other else ""))

    adata = ad.AnnData(X)
    adata.var_names = [str(c) for c in comp_cols]
    adata.obs_names = [str(i) for i in range(X.shape[0])]
    adata.obs["uID"]       = obs["uID"].to_numpy()
    adata.obs["valueName"] = obs["valueName"].to_numpy()
    adata.obs["label"]     = obs["label"].to_numpy()

    # ── cluster the pooled composition vectors (comparable region IDs across the set) ──
    create_umap = bool(script_utils.get_param(params, "createUmap", default=True))
    rs = int(script_utils.get_param(params, "randomState", default=0))
    method_c = script_utils.get_param(params, "clusterMethod", default="leiden")

    if method_c == "kmeans":
        import scanpy as sc
        from sklearn.cluster import KMeans
        n_clusters = int(script_utils.get_param(params, "numClusters", default=5))
        km = KMeans(n_clusters=n_clusters, random_state=rs, n_init=10).fit(X)
        adata.obs["clusters"] = pd.Categorical([str(c) for c in km.labels_])
        log.log(f">> k-means: {n_clusters} regions")
        if create_umap:
            sc.pp.neighbors(adata, use_rep="X")
            sc.tl.umap(adata, random_state=rs)   # obsm['X_umap']
    else:
        # Leiden on the composition graph — no normalisation (vectors are already fractions in [0,1]).
        clustering_utils.find_populations(
            adata,
            resolution=float(script_utils.get_param(params, "resolution", default=1.0)),
            axis="NONE", transformation="NONE",
            create_umap=create_umap, backend="auto", random_state=rs, log=log.log)

    # ── persist the composition vectors as continuous per-cell measures so the region-composition
    # heatmap reuses the cluster-heatmap (region × measures) — no new plot family (SPATIAL_REGIONS_PLAN
    # Decision 16). Names were resolved above from Julia's `compCols`/`otherCol`, which are ALSO what the
    # clustfeatures sidecar records for the heatmap — one namer, so the two cannot drift. ──
    comp_obs = {comp_cols[j]: X[:, j] for j in range(len(comp_cols))}

    # DON'T persist an all-zero "other" column. It is all-zero exactly when the graph contains nothing
    # outside the basis — i.e. the graph was built over the basis populations themselves — and then it is
    # not a measurement, just a flat row in the composition heatmap that invites "where does this come
    # from?". Dropping it here (and telling Julia, so the clustfeatures sidecar doesn't advertise it)
    # keeps the option on by default without producing a dead measure for a basis-only graph.
    other_all_zero = False
    if inc_other and other_col in comp_obs:
        if not np.any(comp_obs[other_col] > 0):
            other_all_zero = True
            del comp_obs[other_col]
            log.log(">> every graph neighbour is in the basis — skipping the empty 'other' column "
                    "(build the graph over the segmentations' all-cells roots to populate it)")

    # ── write regions.{suffix} back per segmentation (shared writer, region column family) ──
    qc = clustering_utils.split_back_and_write(adata, segments, suffix, log=log.log,
                                               col_prefix="regions", extra_obs=comp_obs)
    qc["otherAllZero"] = other_all_zero
    qc["coverage"] = float(min(coverages)) if coverages else 0.0

    qc_out_path = params.get("qcOutPath")
    if qc_out_path is not None:
        write_json_atomic(qc_out_path, qc)
        log.log(f">> saved region QC: {qc['nClusters']} regions over {len(qc['perSegment'])} segment(s)")

    log.log(">> clustRegions done")


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] no --params file", flush=True)
        return
    run(params)


if __name__ == "__main__":
    main()
