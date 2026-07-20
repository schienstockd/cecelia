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


def build_pooled_image_graph(segs, phys_uid, method="delaunay", radius=30.0, n_neighs=6):
    """Pool ONE image's basis cells across its segmentations into a single spatial graph — so a cell in
    segmentation B and a nearby cell in segmentation T are neighbours (cross-segmentation). Shared by
    region clustering and neighbourhood statistics so the pooling is written once.

    `segs`: list of {valueName, propsPath, labels, popCodes} for this image; `phys_uid`: [sz,sy,sx]
    µm/pixel (skimage order). Reads centroids through LabelPropsView (the sanctioned reader), scales to
    physical units, and builds the graph via `build_spatial_graph`. Returns (adata, codes, obs_df) where
    adata carries obsp graph + obsm['spatial'], codes is the per-cell basis code, and obs_df has
    valueName + label per row (row order matches adata / codes). Returns (None, None, None) if empty."""
    import anndata as ad
    import pandas as pd
    from cecelia.utils.label_props_utils import LabelPropsView

    phys = np.asarray(phys_uid, dtype=float)
    coords_list, code_list, obs_list = [], [], []
    for seg in segs:
        view = LabelPropsView(seg["propsPath"]).only_centroid_cols().filter_by_label(seg["labels"])
        d = view.as_df(); ccols = view.centroid_columns(); view.close()
        if d.shape[0] == 0:
            continue
        code_map = {int(l): int(c) for l, c in zip(seg["labels"], seg["popCodes"])}
        codes = np.array([code_map[int(l)] for l in d["label"]], dtype=np.int64)
        coords = d[ccols].to_numpy(dtype=np.float64) * phys[-len(ccols):].reshape(1, -1)
        coords_list.append(coords)
        code_list.append(codes)
        obs_list.append(pd.DataFrame({"valueName": seg["valueName"], "label": d["label"].to_numpy()}))
    if not coords_list:
        return None, None, None

    coords_all = np.vstack(coords_list)
    codes_all = np.concatenate(code_list)
    obs_all = pd.concat(obs_list, ignore_index=True)
    a = ad.AnnData(coords_all.astype(np.float32))
    a.obsm["spatial"] = coords_all
    build_spatial_graph(a, method=method, radius=radius, n_neighs=n_neighs)
    return a, codes_all, obs_all


def pairwise_contact_logodds(conn, pop_codes, n_pops, pseudocount=0.5):
    """Pairwise cell-type contact specificity: the log-odds ratio of OBSERVED vs EXPECTED contacts
    between every pair of populations in the neighbour graph — the canonical CODEX statistic
    (Goltsev et al., *Cell* 174(4):968-981, 2018, DOI 10.1016/j.cell.2018.07.010; Delaunay graph
    per Gabriel & Sokal 1969). Positive = selective association, negative = avoidance (e.g. the
    T/B-cell avoidance reflecting PALS vs follicle segregation).

    Each undirected edge is counted once. Expected counts assume random labelling given the node
    frequencies f: expected(a,b) = n_edges · f_a·f_b·(1 if a==b else 2). A symmetric `pseudocount`
    is added to observed AND expected before the log so an unobserved pair is a finite strong-negative,
    not -inf. Returns (observed, expected, log_odds) as (n_pops, n_pops) symmetric matrices."""
    pop_codes = np.asarray(pop_codes, dtype=np.int64)
    n = len(pop_codes)
    freq = np.bincount(pop_codes, minlength=n_pops).astype(float) / max(n, 1)

    coo = sp.triu(conn, k=1).tocoo()               # each undirected edge once; no self-loops
    a = pop_codes[coo.row]
    b = pop_codes[coo.col]
    lo = np.minimum(a, b); hi = np.maximum(a, b)
    n_edges = len(a)
    observed = np.zeros((n_pops, n_pops))
    np.add.at(observed, (lo, hi), 1.0)
    observed = observed + observed.T - np.diag(np.diag(observed))   # symmetrise: observed[a,b] = # a–b edges

    # expected # of a–b edges under random labelling of nodes with frequencies f: an edge is an
    # unordered pair of endpoints, so P(edge is {a,b}) = 2·f_a·f_b (a≠b) or f_a² (a==b).
    ff = np.outer(freq, freq)
    expected = n_edges * (2.0 * ff)
    np.fill_diagonal(expected, n_edges * np.diag(ff))

    with np.errstate(divide="ignore", invalid="ignore"):
        log_odds = np.log((observed + pseudocount) / (expected + pseudocount))
    return observed, expected, log_odds


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
