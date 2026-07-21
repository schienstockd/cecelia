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


def build_block_diagonal_graph(coords, times, method="delaunay", radius=30.0, n_neighs=6):
    """Per-timepoint spatial graph for LIVE imaging: a cell is linked ONLY to cells in the SAME frame
    (a cell at t=0 must not neighbour a spatially-close cell at t=50). Builds one graph per unique
    `times` value and assembles them block-diagonally, returning a csr `spatial_connectivities` matrix
    (n×n) in the SAME row order as `coords`/`times` (callers keep codes/obs aligned — no reordering).
    A frame with too few cells for the method yields an empty (isolated) block, which
    `neighbourhood_composition` renders as a zero row. Behaviour regions — SPATIAL_REGIONS_PLAN Phase 8."""
    import anndata as ad
    import pandas as pd

    n = coords.shape[0]
    times = np.asarray(times).ravel()
    blocks, order = [], []
    for t in pd.unique(times):                       # first-appearance order
        idx = np.where(times == t)[0]
        order.append(idx)
        if idx.size < 3:                             # too few for a triangulation → isolated block
            blocks.append(sp.csr_matrix((idx.size, idx.size)))
            continue
        sub = ad.AnnData(coords[idx].astype(np.float32)); sub.obsm["spatial"] = coords[idx]
        try:
            build_spatial_graph(sub, method=method, radius=radius, n_neighs=n_neighs)
            blocks.append(sub.obsp["spatial_connectivities"].tocsr())
        except Exception:
            blocks.append(sp.csr_matrix((idx.size, idx.size)))
    if not order:
        return sp.csr_matrix((n, n))
    perm = np.concatenate(order)                     # block_diag rows follow `order` (= perm)
    bd = sp.block_diag(blocks, format="csr")
    inv = np.empty(n, dtype=np.int64); inv[perm] = np.arange(n)   # scatter back to original order
    return bd[inv][:, inv]


def build_pooled_image_graph(segs, phys_uid, method="delaunay", radius=30.0, n_neighs=6,
                             per_timepoint=False):
    """Pool ONE image's basis cells across its segmentations into a single spatial graph — so a cell in
    segmentation B and a nearby cell in segmentation T are neighbours (cross-segmentation). Shared by
    region clustering and neighbourhood statistics so the pooling is written once.

    `segs`: list of {valueName, propsPath, labels, popCodes} for this image; `phys_uid`: [sz,sy,sx]
    µm/pixel (skimage order). Reads centroids through LabelPropsView (the sanctioned reader), scales to
    physical units, and builds the graph via `build_spatial_graph`. Returns (adata, codes, obs_df) where
    adata carries obsp graph + obsm['spatial'], codes is the per-cell basis code, and obs_df has
    valueName + label per row (row order matches adata / codes). Returns (None, None, None) if empty.

    `per_timepoint=True` (LIVE imaging): build the graph FRAME BY FRAME (block-diagonal over the
    temporal column) so neighbourhoods — and hence regions — are per-timepoint and can change over time
    (behaviour regions). Falls back to a single pooled graph if no temporal column is present."""
    import anndata as ad
    import pandas as pd
    from cecelia.utils.label_props_utils import LabelPropsView, axis_of, physical_size_for_axis

    phys = np.asarray(phys_uid, dtype=float)
    coords_list, code_list, obs_list, time_list = [], [], [], []
    for seg in segs:
        view = LabelPropsView(seg["propsPath"]).only_centroid_cols().filter_by_label(seg["labels"])
        d = view.as_df(); ccols = view.centroid_columns(); tcols = view.temporal_columns(); view.close()
        if d.shape[0] == 0:
            continue
        code_map = {int(l): int(c) for l, c in zip(seg["labels"], seg["popCodes"])}
        codes = np.array([code_map[int(l)] for l in d["label"]], dtype=np.int64)
        # each centroid column scaled by ITS OWN axis resolution (by name, never by position) — 2D-safe
        scale = np.array([physical_size_for_axis(phys, axis_of(c)) for c in ccols])
        coords = d[ccols].to_numpy(dtype=np.float64) * scale.reshape(1, -1)
        coords_list.append(coords)
        code_list.append(codes)
        obs_list.append(pd.DataFrame({"valueName": seg["valueName"], "label": d["label"].to_numpy()}))
        if per_timepoint and tcols and tcols[0] in d.columns:
            time_list.append(d[tcols[0]].to_numpy())
    if not coords_list:
        return None, None, None

    coords_all = np.vstack(coords_list)
    codes_all = np.concatenate(code_list)
    obs_all = pd.concat(obs_list, ignore_index=True)
    a = ad.AnnData(coords_all.astype(np.float32))
    a.obsm["spatial"] = coords_all
    if per_timepoint and len(time_list) == len(coords_list):   # every segment had a temporal column
        a.obsp["spatial_connectivities"] = build_block_diagonal_graph(
            coords_all, np.concatenate(time_list), method=method, radius=radius, n_neighs=n_neighs)
    else:
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
