"""
clustRegions.cluster (set-scope) — Python runner.

Spatial region clustering by neighbourhood composition (docs/todo/SPATIAL_REGIONS_PLAN.md, Phase 3+4).
For each image, pools the basis cells across its segmentations into ONE combined spatial graph (so a B
cell and a nearby T cell are neighbours across segmentations), computes each cell's neighbourhood
composition vector (fraction of neighbours in each basis population — the CytoMAP / cecelia "i-niche"
feature), then clusters the pooled composition vectors across the whole set (Leiden or k-means) so
region IDs are comparable across images, and writes `regions.{suffix}` back per segmentation.

Membership + basis-code assignment happen in Julia (clustRegions/cluster.jl); this script gets, per
segment, the member labels and their basis-population codes, and the physical pixel sizes per image.

Params: suffix, segments [{uID, valueName, propsPath, labels, popCodes}], basis (population names ↔
code index), physicalSizes {uID: [sz,sy,sx]}, neighbourMethod, neighbourRadius, nNeighbours,
clusterMethod ("leiden"|"kmeans"), numClusters, resolution, createUmap, randomState, qcOutPath.
"""
import json
from collections import defaultdict

import numpy as np
import pandas as pd

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl::run_py).
import cecelia.utils.script_utils as script_utils
import cecelia.utils.spatial_utils as spatial_utils
import cecelia.utils.clustering_utils as clustering_utils


def run(params):
    log = script_utils.get_logfile_utils(params)

    suffix   = script_utils.get_param(params, "suffix", default="default")
    segments = script_utils.get_param(params, "segments", default=[])
    basis    = script_utils.get_param(params, "basis", default=[])
    phys     = script_utils.get_param(params, "physicalSizes", default={})
    method   = script_utils.get_param(params, "neighbourMethod", default="delaunay")
    radius   = float(script_utils.get_param(params, "neighbourRadius", default=30.0))
    k        = int(script_utils.get_param(params, "nNeighbours", default=6))
    per_t    = bool(script_utils.get_param(params, "perTimepoint", default=False))
    n_basis  = len(basis)

    if len(segments) == 0 or n_basis < 2:
        log.log("[ERROR] clustRegions: no segments or <2 basis populations")
        return

    import anndata as ad

    # ── per image: pool the basis cells across segmentations → one combined graph → composition ──
    by_uid = defaultdict(list)
    for seg in segments:
        by_uid[seg["uID"]].append(seg)

    comp_blocks, obs_blocks = [], []
    for uid, segs in by_uid.items():
        a, codes_all, obs_all = spatial_utils.build_pooled_image_graph(
            segs, phys.get(uid, [1.0, 1.0, 1.0]), method=method, radius=radius, n_neighs=k,
            per_timepoint=per_t)
        if a is None:
            continue
        obs_all.insert(0, "uID", uid)
        comp = spatial_utils.neighbourhood_composition(
            a.obsp["spatial_connectivities"], codes_all, n_basis)
        m = spatial_utils.graph_metrics(a)
        log.log(f">> {uid}: {obs_all.shape[0]} cells, {m['nEdges']} edges, mean degree {m['meanDegree']:.1f}")
        comp_blocks.append(comp)
        obs_blocks.append(obs_all)

    if not comp_blocks:
        log.log("[ERROR] clustRegions: no cells pooled")
        return

    X = np.vstack(comp_blocks).astype(np.float32)
    obs = pd.concat(obs_blocks, ignore_index=True)
    log.log(f">> {X.shape[0]} cells pooled; composition over {n_basis} populations")

    adata = ad.AnnData(X)
    adata.var_names = [str(b) for b in basis]
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

    # ── persist the composition vectors as continuous per-cell measures (spatial.comp.{basis}.{suffix})
    # so the region-composition heatmap reuses the cluster-heatmap (region × measures) — no new plot
    # family (SPATIAL_REGIONS_PLAN Decision 16). "/" in a basis name → "_" for a clean column/label. ──
    def _san(b):
        return str(b).replace("/", "_").replace(" ", "_")
    comp_obs = {f"spatial.comp.{_san(basis[j])}.{suffix}": X[:, j] for j in range(n_basis)}

    # ── write regions.{suffix} back per segmentation (shared writer, region column family) ──
    qc = clustering_utils.split_back_and_write(adata, segments, suffix, log=log.log,
                                               col_prefix="regions", extra_obs=comp_obs)

    qc_out_path = params.get("qcOutPath")
    if qc_out_path is not None:
        with open(qc_out_path, "w") as f:
            json.dump(qc, f)
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
