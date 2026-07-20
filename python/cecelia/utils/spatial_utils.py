"""
Shared spatial helpers — the ONE place the squidpy neighbour graph is built and the ONE place a
neighbourhood composition vector is computed (docs/todo/SPATIAL_REGIONS_PLAN.md). Used by both the
standalone neighbour-graph task (`spatialAnalysis/cell_neighbours_run.py`) and region clustering
(`clustRegions/cluster_run.py`), so the graph modes + composition formula are not implemented twice.

squidpy is a bundled dependency (THIRD_PARTY.md); these wrap its `spatial_neighbors`. The composition
formula (per-cell normalised frequency of each population among its graph neighbours) is the CytoMAP /
legacy-cecelia "i-niche" feature — cited in the region-clustering runner.
"""
import numpy as np
import scipy.sparse as sp


def build_spatial_graph(adata, method="delaunay", radius=30.0, n_neighs=6):
    """Populate `adata.obsp['spatial_connectivities' / 'spatial_distances']` from `adata.obsm['spatial']`
    (physical coordinates). `method`: "delaunay" (radius-pruned), "knn", or "radius". Mutates `adata`,
    returns it. One squidpy entry point for the whole codebase.

    NB: pinned to squidpy <1.9 — `sq.gr.spatial_neighbors` is removed in 1.9 (see pixi.toml). Migrate
    to the `spatial_neighbors_{delaunay,knn,radius}` builders when that pin is lifted."""
    import squidpy as sq

    if method == "delaunay":
        sq.gr.spatial_neighbors(adata, coord_type="generic", delaunay=True)
        # prune edges longer than the radius (legacy parity). Operate on the sparse `.data` (only the
        # nonzeros) rather than boolean-indexing the matrix — the latter densifies to n×n and warns
        # (SparseEfficiencyWarning). Rebuild connectivities (binary) from the pruned distances so both
        # matrices stay consistent regardless of squidpy's internal ordering.
        dist = adata.obsp["spatial_distances"].copy()
        dist.data[dist.data > radius] = 0.0
        dist.eliminate_zeros()
        conn = dist.copy()
        conn.data[:] = 1.0
        adata.obsp["spatial_distances"] = dist
        adata.obsp["spatial_connectivities"] = conn
    elif method == "knn":
        sq.gr.spatial_neighbors(adata, coord_type="generic", n_neighs=int(n_neighs))
    else:  # radius
        sq.gr.spatial_neighbors(adata, coord_type="generic", radius=float(radius))
    return adata


def graph_metrics(adata):
    """Objective graph metrics (undirected: each edge counted once) for QC."""
    conn = adata.obsp["spatial_connectivities"]
    deg = np.asarray((conn > 0).sum(axis=1)).ravel()
    n = adata.n_obs
    return {
        "nCells": int(n),
        "nEdges": int(conn.nnz // 2),
        "meanDegree": float(deg.mean()) if n else 0.0,
        "isolatedFrac": float((deg == 0).sum() / n) if n else 0.0,
    }


def neighbourhood_composition(conn, pop_codes, n_pops):
    """Per-cell neighbourhood composition vector: row i = the normalised frequency of each population
    (code 0..n_pops-1) among cell i's graph neighbours (CytoMAP / cecelia "i-niche" feature).

    `conn` is the (n_cells, n_cells) sparse connectivity matrix; `pop_codes` is a length-n_cells int
    array (each cell's population index). Returns a dense (n_cells, n_pops) float32 matrix; a cell with
    no neighbours gets an all-zero row. Formula matches legacy cellRegionsStats (`freq = n / sum(n)`).
    """
    pop_codes = np.asarray(pop_codes, dtype=np.int64)
    n_cells = conn.shape[0]
    # one-hot (n_cells, n_pops): neighbour j contributes to its own population column
    onehot = sp.csr_matrix(
        (np.ones(n_cells, dtype=np.float32), (np.arange(n_cells), pop_codes)),
        shape=(n_cells, n_pops))
    # counts[i, p] = number of cell i's neighbours in population p
    counts = np.asarray((conn > 0).astype(np.float32) @ onehot.toarray())
    totals = counts.sum(axis=1, keepdims=True)
    totals[totals == 0] = 1.0                       # avoid /0; isolated cells stay all-zero
    return (counts / totals).astype(np.float32)
