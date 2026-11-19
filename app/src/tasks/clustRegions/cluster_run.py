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
otherCol, densityCol, includeOther, pooling ("frame"|"track"|"movie", spatial_utils.POOLINGS), clusterMethod ("leiden"|"kmeans"), numClusters, resolution, createUmap,
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
    # "track": sum each track's per-frame neighbour counts → one composition, one region per track
    pooling    = script_utils.get_param(params, "pooling", default="frame")
    n_basis    = len(basis)

    if len(segments) == 0 or n_basis < 2:
        log.log("[ERROR] clustRegions: no segments or <2 basis populations")
        return


    # ── per image: LOAD the shared graph → attach this basis → composition ──
    by_uid = defaultdict(list)
    for seg in segments:
        by_uid[seg["uID"]].append(seg)

    comp_blocks, obs_blocks, coverages = [], [], []
    # per-image metrics of the graph actually loaded — banked in the run's QC so Julia can flag a graph
    # too sparse for composition vectors (the same check + threshold as the graph task, cellNeighbours.jl)
    graph_metrics = {}
    kinds = set()     # per image: the window grid's axes, or None for cell-to-cell — must agree
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
        # Composition counts EVERY neighbour, including cells outside the basis (the "other" bin), but
        # only BASIS cells are clustered and labelled: the graph is pop-agnostic and spans more cells
        # than the analysis, and a non-basis cell must not pick up a region for a basis it isn't in.
        counts, own, keep, problem = spatial_utils.image_region_rows(
            a, obs_all, codes_all, n_basis, pooling, inc_other,
            {s["valueName"]: s["propsPath"] for s in segs})
        if problem:
            log.log(f"[ERROR] clustRegions: {uid}, graph '{graph_sfx}': {problem}")
            return
        n_dropped = int((codes_all >= 0).sum() - keep.sum())
        if n_dropped:
            log.log(f">> {uid}: {n_dropped} basis cells are not on a track — they get no region")
        obs_all = obs_all.loc[keep].reset_index(drop=True)
        win_grid = spatial_utils.graph_window_grid(a)
        m = spatial_utils.window_metrics(a) if win_grid else spatial_utils.graph_metrics(a)
        graph_metrics[uid] = m
        kinds.add(tuple(win_grid["gridAxes"]) if win_grid else None)
        if len(kinds) > 1:
            log.log(f"[ERROR] clustRegions: graph '{graph_sfx}' differs across images (window vs cell graph, "
                    "or windows through Z on some only) — rebuild it the same way on every image")
            return
        log.log(f">> {uid}: graph '{graph_sfx}' {m['nCells']} cells, "
                + (f"{m['tile']:g} µm tiles" if win_grid else f"mean degree {m['meanDegree']:.1f}")
                + f" → {int(keep.sum())} basis cells ({coverage * 100:.1f}%), {len(counts)} rows")
        comp_blocks.append((counts, own))
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

    counts, inverse = spatial_utils.pool_region_rows(comp_blocks)
    obs = pd.concat(obs_blocks, ignore_index=True)
    Xc = spatial_utils.normalise_counts(counts)      # what is clustered: cells, tracks or windows
    X = Xc[inverse]                                    # each cell carries its row's composition
    windows = None not in kinds
    log.log(f">> {X.shape[0]} cells → {len(Xc)} {'window' if windows else 'cell/track'} rows clustered "
            f"(pooling: {pooling})")
    density_col = script_utils.get_param(params, "densityCol", default="spatial.comp.cells_per_window")
    if windows:
        # a window's cell count joins its composition, so dense, sparse and near-empty places separate
        density = counts.sum(axis=1)
        Xc = np.hstack([Xc, (density / max(density.max(), 1.0))[:, None]]).astype(np.float32)
    if X.shape[1] != len(comp_cols):
        log.log(f"[ERROR] clustRegions: composition has {X.shape[1]} columns, expected {len(comp_cols)}")
        return

    # ── cluster the pooled composition vectors (comparable region IDs across the set) ──
    run_env = {}   # → run log `env` (Leiden names the device that ran; Julia adds the seed)
    clustered = spatial_utils.cluster_composition(
        Xc, comp_cols + ([density_col] if windows else []), method=script_utils.get_param(params, "clusterMethod", default="leiden"),
        num_clusters=int(script_utils.get_param(params, "numClusters", default=5)),
        resolution=float(script_utils.get_param(params, "resolution", default=1.0)),
        create_umap=bool(script_utils.get_param(params, "createUmap", default=True)),
        random_state=int(script_utils.get_param(params, "randomState", default=0)),
        log=log.log, run_env=run_env)
    # back to one row per CELL for the write-back (per track: every cell takes its track's region)
    adata = spatial_utils.broadcast_clusters(clustered, X, obs, inverse)

    # ── persist the composition vectors as continuous per-cell measures so the region-composition
    # heatmap reuses the cluster-heatmap (region × measures) — no new plot family (SPATIAL_REGIONS_PLAN
    # Decision 16). Names were resolved above from Julia's `compCols`/`otherCol`, which are ALSO what the
    # clustfeatures sidecar records for the heatmap — one namer, so the two cannot drift. ──
    comp_obs = {comp_cols[j]: X[:, j] for j in range(len(comp_cols))}
    if windows:
        comp_obs[density_col] = density[inverse]       # cells in its window per frame (edge-corrected)

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
    qc["windowGraph"] = windows
    qc["coverage"] = float(min(coverages)) if coverages else 0.0
    qc["graphMetrics"] = graph_metrics
    qc["runEnv"] = run_env

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
