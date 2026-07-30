"""
Shared spatial helpers — the ONE place the squidpy neighbour graph is built, persisted, read back and
labelled, and the ONE place neighbourhood composition + pairwise contact statistics are computed
(docs/todo/SPATIAL_REGIONS_PLAN.md).

THE GRAPH IS POP-AGNOSTIC. `spatialAnalysis.cellNeighbours` builds it once per image and persists it to
`spatialGraph/{suffix}.h5ad`, storing only each node's IDENTITY (`valueName`, `label`) plus the
coordinates and squidpy's `obsp` connectivities. Every downstream analysis — pairwise cell interactions
(`neighbourStats`) and neighbourhood-composition region clustering (`clustRegions`) — LOADS that graph
and attaches its own population labelling at analysis time via `pop_codes_for`. Nothing rebuilds a
graph, so a neighbourhood means the same thing in every readout, interactions need no regions, and the
graph parameters live in exactly one place.

This restores the legacy architecture: old-R `clustRegions/kmeansClust.R` read `cciaObj$spatialDT(...)`
— the persisted `cellNeighbours` edge table (`from`/`to`/`dist`, with `neighbour_value_name` /
`neighbour_label` identity per edge) — and joined `popDT` onto it. The interim new-Cecelia code had each
task rebuild its own basis-restricted graph in-process, which is the divergence this removes.

squidpy is a bundled dependency (THIRD_PARTY.md); the graph modes wrap its `spatial_neighbors`. The
composition formula (per-cell normalised frequency of each population among its graph neighbours) is the
CytoMAP / legacy-cecelia "i-niche" feature. The contact log-odds + label-permutation null are cited on
`pairwise_contact_logodds`.
"""
import numpy as np
import scipy.sparse as sp

# obs columns that identify a graph node. The graph carries NO population codes — see the module note.
GRAPH_ID_COLS = ("valueName", "label")
UNASSIGNED = -1        # a graph node outside the analysis's population basis (the "other" bin)


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
    """Pool ONE image's cells across its segmentations into a single spatial graph — so a cell in
    segmentation B and a nearby cell in segmentation T are neighbours (cross-segmentation). THE graph
    builder: only `spatialAnalysis.cellNeighbours` calls it, and everything else loads what it persisted.

    `segs`: list of {valueName, propsPath, labels} for this image (`labels` = the node set; a
    whole-segmentation graph passes every label); `phys_uid`: [sz,sy,sx] µm/pixel (skimage order). Reads
    centroids through LabelPropsView (the sanctioned reader), scales to physical units, and builds the
    graph via `build_spatial_graph`.

    Returns (adata, obs_df): adata carries obsp connectivities/distances + obsm['spatial'] (µm), and
    obs_df has `valueName` + `label` per row in adata's row order. NO population codes — a labelling is
    attached later by `pop_codes_for`. Returns (None, None) if no cells were found.

    `per_timepoint=True` (LIVE imaging): build the graph FRAME BY FRAME (block-diagonal over the
    temporal column) so neighbourhoods — and hence regions — are per-timepoint and can change over time
    (behaviour regions). Falls back to a single pooled graph if no temporal column is present; when it
    applies, the frame index is kept as obs `_t` so downstream nulls can shuffle WITHIN a frame."""
    import anndata as ad
    import pandas as pd
    from cecelia.utils.label_props_utils import LabelPropsView, axis_of, physical_size_for_axis

    phys = np.asarray(phys_uid, dtype=float)
    coords_list, obs_list, time_list = [], [], []
    for seg in segs:
        view = LabelPropsView(seg["propsPath"]).only_centroid_cols()
        labels = seg.get("labels")
        if labels is not None:
            view = view.filter_by_label(labels)
        d = view.as_df(); ccols = view.centroid_columns(); tcols = view.temporal_columns(); view.close()
        if d.shape[0] == 0:
            continue
        # each centroid column scaled by ITS OWN axis resolution (by name, never by position) — 2D-safe
        scale = np.array([physical_size_for_axis(phys, axis_of(c)) for c in ccols])
        coords = d[ccols].to_numpy(dtype=np.float64) * scale.reshape(1, -1)
        coords_list.append(coords)
        obs_list.append(pd.DataFrame({"valueName": seg["valueName"], "label": d["label"].to_numpy()}))
        if per_timepoint and tcols and tcols[0] in d.columns:
            time_list.append(d[tcols[0]].to_numpy())
    if not coords_list:
        return None, None

    coords_all = np.vstack(coords_list)
    obs_all = pd.concat(obs_list, ignore_index=True)
    a = ad.AnnData(coords_all.astype(np.float32))
    a.obsm["spatial"] = coords_all
    if per_timepoint and len(time_list) == len(coords_list):   # every segment had a temporal column
        times = np.concatenate(time_list)
        a.obsp["spatial_connectivities"] = build_block_diagonal_graph(
            coords_all, times, method=method, radius=radius, n_neighs=n_neighs)
        obs_all["_t"] = times
    else:
        build_spatial_graph(a, method=method, radius=radius, n_neighs=n_neighs)
    return a, obs_all


def save_graph(adata, obs_df, path, meta=None):
    """Persist a built graph to `spatialGraph/{suffix}.h5ad` — node identity (`valueName`, `label`,
    optional `_t`), coordinates, and squidpy's obsp matrices. `meta` (the graph parameters) is written to
    `uns` so a consumer can report how the graph it is using was built.

    Creating a NEW .h5ad is the sanctioned producing-task exception to the LabelPropsView rule (see
    CLAUDE.md): this is a graph sidecar, not a cell table, and the view wraps existing cell tables."""
    import os

    adata.obs = obs_df.reset_index(drop=True)
    adata.obs_names = [str(i) for i in range(adata.n_obs)]
    adata.obs["label"] = adata.obs["label"].astype(np.int64)
    adata.obs["valueName"] = adata.obs["valueName"].astype(str)
    if meta:
        for k, v in meta.items():
            adata.uns[k] = v
    os.makedirs(os.path.dirname(path), exist_ok=True)
    adata.write_h5ad(path)
    return path


def load_graph(path):
    """Read a persisted graph back → (adata, obs_df). Raises FileNotFoundError if it was never built, so
    a consumer fails loudly rather than silently falling back to a graph of its own."""
    import anndata as ad
    import os

    if not os.path.isfile(path):
        raise FileNotFoundError(f"no neighbour graph at {path} — run 'Neighbour graph' for this image first")
    a = ad.read_h5ad(path)
    return a, a.obs.reset_index(drop=True)


def graph_meta(adata):
    """The graph's recorded build parameters (`uns`), as plain Python — for logging + QC provenance."""
    out = {}
    for k, v in (adata.uns or {}).items():
        out[k] = v.tolist() if isinstance(v, np.ndarray) else v
    return out


def pop_codes_for(obs_df, segs, n_pops):
    """Attach an analysis's population labelling to a LOADED graph: map each graph node to its 0-based
    basis code via `(valueName, label)`, the join key legacy used (`spatialDT[popDT, on=.(uID, label)]`).

    `segs`: the Julia-resolved segments ({valueName, labels, popCodes}) — membership and code assignment
    stay in Julia so they compose with gating across poptypes (Decision 6). Nodes not in any basis
    population get `UNASSIGNED` (-1): they remain real neighbours in the graph but form the explicit
    "other" bin rather than being silently dropped.

    Returns (codes, coverage) where `coverage` is the fraction of graph nodes that got a code — the
    number to warn on when a graph was built over a narrower cell set than the analysis asks for."""
    code_by_key = {}
    for seg in segs:
        vn = str(seg["valueName"])
        for l, c in zip(seg["labels"], seg["popCodes"]):
            code_by_key[(vn, int(l))] = int(c)
    vns = obs_df["valueName"].astype(str).to_numpy()
    labs = obs_df["label"].astype(np.int64).to_numpy()
    codes = np.full(len(labs), UNASSIGNED, dtype=np.int64)
    for i in range(len(labs)):
        c = code_by_key.get((vns[i], int(labs[i])))
        if c is not None and 0 <= c < n_pops:
            codes[i] = c
    coverage = float((codes >= 0).sum() / len(codes)) if len(codes) else 0.0
    return codes, coverage


def _contact_counts(conn_row, conn_col, pop_codes, n_pops):
    """Symmetric (n_pops, n_pops) count of a–b edges for one labelling. `conn_row`/`conn_col` are the
    upper-triangle edge endpoints (each undirected edge once), precomputed so a permutation loop does not
    re-extract them. Edges touching an UNASSIGNED node contribute nothing."""
    a = pop_codes[conn_row]
    b = pop_codes[conn_col]
    keep = (a >= 0) & (b >= 0)
    a = a[keep]; b = b[keep]
    lo = np.minimum(a, b); hi = np.maximum(a, b)
    m = np.zeros((n_pops, n_pops))
    np.add.at(m, (lo, hi), 1.0)
    return m + m.T - np.diag(np.diag(m))


def pairwise_contact_logodds(conn, pop_codes, n_pops, pseudocount=0.5,
                             n_permutations=1000, random_state=0, times=None):
    """Pairwise cell-type contact specificity, with a permutation test.

    EFFECT SIZE — the log-odds ratio of OBSERVED vs EXPECTED contacts between every pair of populations
    in the neighbour graph, the canonical CODEX statistic (Goltsev et al., *Cell* 174(4):968-981, 2018,
    DOI 10.1016/j.cell.2018.07.010; Delaunay graph per Gabriel & Sokal 1969). Positive = selective
    association, negative = avoidance (e.g. T/B-cell avoidance reflecting PALS vs follicle segregation).
    Expected counts assume random labelling given the node frequencies f: expected(a,b) =
    n_edges · f_a·f_b·(1 if a==b else 2). A symmetric `pseudocount` is added to observed AND expected
    before the log, so an unobserved pair is a finite strong-negative rather than -inf.

    SIGNIFICANCE — `n_permutations` random relabellings of the graph's nodes (the neighbourhood-enrichment
    permutation scheme of squidpy's `nhood_enrichment`; Palla et al., *Nat Methods* 19:171-178, 2022,
    DOI 10.1038/s41592-021-01358-2). The GRAPH and the label COUNTS are held fixed and only the
    assignment of labels to nodes is shuffled, so the null asks exactly "would I see this many A–B
    contacts if these cell types were arranged at random?". Yields a z-score against the null and a
    two-sided EMPIRICAL p-value ((#|null−µ| ≥ |obs−µ| + 1)/(n+1), so p is never 0). `n_permutations=0`
    skips the test and returns NaN z/p — the fast path.

    Only ASSIGNED nodes (code ≥ 0) are shuffled, among themselves: which nodes fall outside the basis is
    a property of the segmentation, not of the biology under test, so it is conditioned on rather than
    randomised. `times` (a per-node frame index) restricts each shuffle to WITHIN a timepoint — required
    for a per-timepoint (block-diagonal) graph, where a global shuffle would migrate labels across frames
    and destroy the per-frame composition the null is supposed to preserve.

    CAVEAT: this is a complete-spatial-randomness-of-labels null. It preserves the graph and the label
    counts but not larger-scale tissue structure, so in strongly zoned tissue it can call weak
    associations significant. squidpy's `nhood_enrichment` shares this limitation.

    Returns (observed, expected, log_odds, zscore, pvalue) as (n_pops, n_pops) symmetric matrices."""
    pop_codes = np.asarray(pop_codes, dtype=np.int64)
    n = len(pop_codes)
    assigned = pop_codes >= 0
    n_assigned = int(assigned.sum())
    freq = np.bincount(pop_codes[assigned], minlength=n_pops).astype(float) / max(n_assigned, 1)

    coo = sp.triu(conn, k=1).tocoo()               # each undirected edge once; no self-loops
    row, col = coo.row, coo.col
    observed = _contact_counts(row, col, pop_codes, n_pops)
    n_edges = float(observed.sum() + np.trace(observed)) / 2.0   # pop–pop edges (diagonal counted once)

    # expected # of a–b edges under random labelling of nodes with frequencies f: an edge is an
    # unordered pair of endpoints, so P(edge is {a,b}) = 2·f_a·f_b (a≠b) or f_a² (a==b).
    ff = np.outer(freq, freq)
    expected = n_edges * (2.0 * ff)
    np.fill_diagonal(expected, n_edges * np.diag(ff))

    with np.errstate(divide="ignore", invalid="ignore"):
        log_odds = np.log((observed + pseudocount) / (expected + pseudocount))

    n_perm = int(n_permutations)
    if n_perm <= 0 or n_assigned == 0:
        nan = np.full((n_pops, n_pops), np.nan)
        return observed, expected, log_odds, nan, nan

    rng = np.random.default_rng(int(random_state))
    idx_assigned = np.where(assigned)[0]
    # shuffle groups: one group per timepoint (per-timepoint graph) or a single global group
    if times is None:
        groups = [idx_assigned]
    else:
        t = np.asarray(times).ravel()
        groups = [idx_assigned[t[idx_assigned] == tv] for tv in np.unique(t[idx_assigned])]

    null = np.empty((n_perm, n_pops, n_pops))
    shuffled = pop_codes.copy()
    for i in range(n_perm):
        for g in groups:
            shuffled[g] = rng.permutation(pop_codes[g])
        null[i] = _contact_counts(row, col, shuffled, n_pops)

    mu = null.mean(axis=0)
    sd = null.std(axis=0)
    with np.errstate(divide="ignore", invalid="ignore"):
        zscore = np.where(sd > 0, (observed - mu) / sd, np.nan)
    # two-sided empirical p with the +1 correction (Davison & Hinkley 1997) → p ∈ [1/(n+1), 1]
    extreme = (np.abs(null - mu) >= np.abs(observed - mu)).sum(axis=0)
    pvalue = (extreme + 1.0) / (n_perm + 1.0)
    return observed, expected, log_odds, zscore, pvalue


def neighbourhood_composition(conn, pop_codes, n_pops, include_other=False):
    """Per-cell neighbourhood composition vector: row i = the normalised frequency of each population
    (code 0..n_pops-1) among cell i's graph neighbours (CytoMAP / cecelia "i-niche" feature).

    `conn` is the (n_cells, n_cells) sparse connectivity matrix; `pop_codes` is a length-n_cells int
    array (each cell's population index, or UNASSIGNED). Returns a dense float32 matrix; a cell with no
    neighbours gets an all-zero row. Formula matches legacy cellRegionsStats (`freq = n / sum(n)`).

    `include_other=True` adds ONE extra trailing column counting UNASSIGNED neighbours — cells present in
    the graph but outside the basis. This matters for correctness, not just completeness: with it, the
    row is the true composition of the neighbourhood and sums to 1; without it, the fractions are
    renormalised over basis neighbours only, so a cell surrounded mostly by unlabelled cells looks
    identical to one surrounded entirely by basis cells. CytoMAP keeps the same "other" bin.
    """
    pop_codes = np.asarray(pop_codes, dtype=np.int64)
    n_cells = conn.shape[0]
    n_cols = n_pops + 1 if include_other else n_pops
    # map codes to columns; UNASSIGNED → the trailing "other" column, or dropped when not included
    cols = np.where(pop_codes >= 0, pop_codes, n_pops if include_other else 0)
    keep = (pop_codes >= 0) | include_other
    rows = np.arange(n_cells)[keep]
    # one-hot (n_cells, n_cols): neighbour j contributes to its own population column
    onehot = sp.csr_matrix(
        (np.ones(len(rows), dtype=np.float32), (rows, cols[keep])),
        shape=(n_cells, n_cols))
    # counts[i, p] = number of cell i's neighbours in population p
    counts = np.asarray((conn > 0).astype(np.float32) @ onehot.toarray())
    totals = counts.sum(axis=1, keepdims=True)
    totals[totals == 0] = 1.0                       # avoid /0; isolated cells stay all-zero
    return (counts / totals).astype(np.float32)
