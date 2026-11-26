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
`pairwise_contact_logodds`. One population's own spacing among the graph's nodes (nearest-neighbour
ratio, pair-ratio curve) lives in `spatial_pattern_utils`, which shares `permutation_groups`.
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


def undirected_edges(conn):
    """Each edge of a neighbour graph once, as (row, col) with row < col: a pair linked in EITHER
    direction. Delaunay and radius graphs store every edge both ways; a kNN graph is directed (a
    cell's k nearest need not list it back), so its upper triangle alone misses the edges stored only
    below the diagonal. Self-loops are not edges. The viewer's reader (`spatial_graph_edges`,
    app/src/label_props.jl) counts edges the same way."""
    sym = sp.triu((conn != 0).astype(np.int8) + (conn != 0).astype(np.int8).T, k=1).tocoo()
    return sym.row, sym.col


def graph_metrics(adata):
    """Objective graph metrics (undirected: each edge counted once, `undirected_edges`) for QC."""
    conn = adata.obsp["spatial_connectivities"]
    row, col = undirected_edges(conn)
    n = adata.n_obs
    deg = np.bincount(np.concatenate([row, col]), minlength=n)
    return {
        "nCells": int(n),
        "nEdges": int(len(row)),
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
                             per_timepoint=True, window_size=50.0):
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

    `per_timepoint=True` (the default; LIVE imaging): build the graph FRAME BY FRAME (block-diagonal over
    the temporal column) so no edge links two frames, and neighbourhoods — hence regions — can change over
    time (behaviour regions). `False` pools every frame into one graph (opt-in). Falls back to a single pooled graph if no temporal column is present; when it
    applies, the frame index is kept as obs `_t` so downstream nulls can shuffle WITHIN a frame."""
    import anndata as ad
    import pandas as pd
    from cecelia.utils.label_props_utils import LabelPropsView, scale_centroids

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
        # pixels → µm via the ONE shared conversion (per axis, by name — 2D-safe). Same helper the Julia
        # side uses, so a graph's edge lengths cannot disagree with a task's µm threshold.
        coords = scale_centroids(d, phys)[ccols].to_numpy(dtype=np.float64)
        coords_list.append(coords)
        obs_list.append(pd.DataFrame({"valueName": seg["valueName"], "label": d["label"].to_numpy()}))
        if (per_timepoint or method == "windows") and tcols and tcols[0] in d.columns:
            time_list.append(d[tcols[0]].to_numpy())
    if not coords_list:
        return None, None

    coords_all = np.vstack(coords_list)
    obs_all = pd.concat(obs_list, ignore_index=True)
    a = ad.AnnData(coords_all.astype(np.float32))
    a.obsm["spatial"] = coords_all
    if method == "windows":
        # FIXED WINDOWS: no cell-to-cell edges — each node belongs to a tile of a grid laid over the
        # field (`window_grid`), and a consumer counts cells per window. The frame is kept whatever
        # `per_timepoint` says: whether windows count per frame or over the movie is the consumer's call.
        a.obsp["spatial_connectivities"] = sp.csr_matrix((a.n_obs, a.n_obs), dtype=np.float32)
        a.uns.update(window_grid(coords_all, [c.split("_")[-1] for c in ccols], window_size))
        if len(time_list) == len(coords_list):
            obs_all["_t"] = np.concatenate(time_list)
        return a, obs_all
    if per_timepoint and len(time_list) == len(coords_list):   # every segment had a temporal column
        times = np.concatenate(time_list)
        a.obsp["spatial_connectivities"] = build_block_diagonal_graph(
            coords_all, times, method=method, radius=radius, n_neighs=n_neighs)
        obs_all["_t"] = times
    else:
        build_spatial_graph(a, method=method, radius=radius, n_neighs=n_neighs)
    return a, obs_all


# ── fixed windows (CytoMAP raster scan) ─────────────────────────────────────────────────────────────
# Schienstock & Mueller 2021 (Immunol Rev, doi:10.1111/imr.13038 §4.5) after CytoMAP (Stoltzfus et al.
# 2020, Cell Rep, doi:10.1016/j.celrep.2020.107523): the image "tiled into overlapping squares". A window
# is a square (cube in 3D) of side S; windows sit on a grid of step S/2, so neighbours overlap by half.
# The grid's cells — TILES, S/2 wide — are what a cell is assigned to: a cell's own window is the one
# centred on its tile (= the nearest window centre). Every cell lies in exactly two windows per axis.

def window_grid(coords, axes, window_size):
    """The grid for `coords` (n, d) µm with axis names `axes` (e.g. ['z', 'y', 'x']): a dict for the graph
    file's `uns` — `windowSize`, `tile` (= S/2), `gridAxes` (the axes windows span) and `gridShape`
    (tiles per axis). The grid starts at 0 µm and covers the cells' extent. Z is left out (windows are
    columns through the stack) unless the cells span at least two windows in Z: a shallow intravital
    stack — 55 µm with 50 µm windows — is one layer of columns, not a single slab of cubes whose depth
    would then hinge on each image's Z spread."""
    coords = np.asarray(coords, dtype=float)
    S = float(window_size)
    tile = S / 2.0
    keep = [i for i, ax in enumerate(axes)
            if not (ax == "z" and np.ptp(coords[:, i]) < 2 * S)] if len(coords) else list(range(len(axes)))
    shape = [int(np.floor(max(coords[:, i].max(), 0.0) / tile)) + 1 if len(coords) else 1 for i in keep]
    return {"method": "windows", "windowSize": S, "tile": tile,
            "gridAxes": [str(axes[i]) for i in keep], "gridAxisIdx": [int(i) for i in keep],
            "gridShape": shape}


def window_tiles(coords, grid):
    """Each node's tile as a flat index into `grid['gridShape']` (C order over `gridAxes`)."""
    c = np.asarray(coords, dtype=float)[:, grid["gridAxisIdx"]]
    shape = np.asarray(grid["gridShape"], dtype=np.int64)
    idx = np.clip(np.floor(c / grid["tile"]).astype(np.int64), 0, shape - 1)
    return np.ravel_multi_index(tuple(idx.T), tuple(shape))


def window_metrics(adata):
    """A window graph's QC metrics: `nCells`, the grid, and `cellsPerTile` — cell-frames per occupied
    tile, how much each window has to go on. `method: "windows"` tells a reader there are no edges."""
    grid = {k: _plain(adata.uns[k]) for k in ("windowSize", "tile", "gridAxes", "gridAxisIdx", "gridShape")}
    tiles = window_tiles(adata.obsm["spatial"], grid) if adata.n_obs else np.zeros(0, dtype=np.int64)
    occupied = len(np.unique(tiles))
    return {"method": "windows", "nCells": int(adata.n_obs), "nEdges": 0, **grid,
            "nTiles": int(np.prod(grid["gridShape"])), "occupiedTiles": int(occupied),
            "cellsPerTile": float(adata.n_obs / occupied) if occupied else 0.0}


def _plain(v):
    """An `uns` value as plain Python (h5ad round-trips lists as numpy arrays)."""
    if isinstance(v, np.ndarray):
        return v.tolist()
    return v.item() if isinstance(v, np.generic) else v


def graph_window_grid(adata):
    """The window grid a graph was built with, or None for a cell-to-cell graph."""
    uns = adata.uns or {}
    if _plain(uns.get("method")) != "windows":
        return None
    return {k: _plain(uns[k]) for k in ("windowSize", "tile", "gridAxes", "gridAxisIdx", "gridShape")}


def window_counts(coords, pop_codes, n_pops, grid, groups=None, include_other=False):
    """Cells of each population inside each WINDOW: `(counts, n_windows)`, `counts` shaped
    (n_groups × n_windows, n_cols) with row `g * n_windows + w`. `groups` (length n, ints 0..G-1)
    separates frames for per-frame windows; None counts over the whole movie (G = 1). A cell lies in
    two windows per axis — those centred on its tile and on the tile next to it on the side of the
    tile's centre it sits on — so it is counted in 2^d windows. UNASSIGNED cells count in the trailing
    "other" column with `include_other`, else not at all (as in `neighbourhood_counts`)."""
    import itertools
    c = np.asarray(coords, dtype=float)[:, grid["gridAxisIdx"]]
    shape = np.asarray(grid["gridShape"], dtype=np.int64)
    n_win = int(np.prod(shape))
    codes = np.asarray(pop_codes, dtype=np.int64)
    g = np.zeros(len(codes), dtype=np.int64) if groups is None else np.asarray(groups, dtype=np.int64)
    n_groups = int(g.max()) + 1 if len(g) else 1
    n_cols = n_pops + 1 if include_other else n_pops
    col = np.where(codes >= 0, codes, n_pops if include_other else -1)
    use = col >= 0
    # the window centred on tile k spans [(k - 0.5)·tile, (k + 1.5)·tile) → lower window index
    lo = np.floor(c / grid["tile"] - 0.5).astype(np.int64)
    counts = np.zeros((n_groups * n_win, n_cols), dtype=np.float64)
    for offs in itertools.product((0, 1), repeat=c.shape[1]):
        w = lo + np.asarray(offs, dtype=np.int64)
        ok = use & np.all((w >= 0) & (w < shape), axis=1)
        flat = np.ravel_multi_index(tuple(w[ok].T), tuple(shape))
        np.add.at(counts, (g[ok] * n_win + flat, col[ok]), 1.0)
    return counts, n_win


def window_coverage(grid):
    """The fraction of each window (flat index, as in `window_counts`) that lies inside the field —
    the grid's extent, [0, gridShape·tile) per axis. A window centred on an edge tile reaches half a
    tile past the field (3/4 inside per axis), so its raw count understates the tissue there; dividing
    by this puts every window on the same footing."""
    shape = np.asarray(grid["gridShape"], dtype=np.int64)
    per_axis = []
    for n in shape:
        k = np.arange(n, dtype=float)                  # the window centred on tile k: [k-0.5, k+1.5) tiles
        inside = np.minimum(k + 1.5, n) - np.maximum(k - 0.5, 0.0)
        per_axis.append(inside / 2.0)
    cov = per_axis[0]
    for p in per_axis[1:]:
        cov = np.multiply.outer(cov, p)
    return np.asarray(cov, dtype=float).ravel()


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
    from cecelia.utils.atomic_io import write_h5ad_atomic

    os.makedirs(os.path.dirname(path), exist_ok=True)
    write_h5ad_atomic(adata, path)
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


def graph_node_times(obs_df, props_paths):
    """Each graph node's frame, read from its segmentation's cell table — for a graph built with frames
    POOLED (`perTimepoint=False`, so no `_t` obs). Returns an array parallel to `obs_df`, or None when
    no node's table has a temporal column (a still image).

    Why a consumer needs it: on a pooled graph of a time-lapse, the nearest node to a cell is usually
    the same cell one frame later, so any distance statistic over the nodes measures the tracks, not
    the placement. `props_paths` maps valueName → labelProps path; reads go through LabelPropsView."""
    import pandas as pd
    from cecelia.utils.label_props_utils import LabelPropsView

    times = np.full(len(obs_df), np.nan)
    vns = obs_df["valueName"].astype(str).to_numpy()
    labs = obs_df["label"].astype(np.int64).to_numpy()
    found = False
    for vn in pd.unique(vns):
        path = props_paths.get(vn)
        if path is None:
            continue
        idx = np.where(vns == vn)[0]
        view = LabelPropsView(path).only_centroid_cols().filter_by_label(labs[idx].tolist())
        tcols = view.temporal_columns(); d = view.as_df(); view.close()
        if not tcols or tcols[0] not in d.columns:
            continue
        t_of = dict(zip(d["label"].astype(np.int64), d[tcols[0]].to_numpy()))
        times[idx] = [t_of.get(int(l), np.nan) for l in labs[idx]]
        found = True
    if not found or np.isnan(times).all():
        return None
    return times


def node_track_ids(obs_df, props_paths):
    """Each graph node's `track_id`, read from its segmentation's cell table — what pools a cell's
    per-frame neighbourhoods into one per TRACK (`clustRegions`, pooling "track"). Array parallel to
    `obs_df` (float, NaN = an untracked cell), or None when no table carries `track_id`.
    `props_paths` maps valueName → labelProps path; reads go through LabelPropsView."""
    import pandas as pd
    from cecelia.utils.label_props_utils import LabelPropsView

    ids = np.full(len(obs_df), np.nan)
    vns = obs_df["valueName"].astype(str).to_numpy()
    labs = obs_df["label"].astype(np.int64).to_numpy()
    found = False
    for vn in pd.unique(vns):
        path = props_paths.get(vn)
        if path is None:
            continue
        idx = np.where(vns == vn)[0]
        view = LabelPropsView(path).view_cols(["track_id"]).filter_by_label(labs[idx].tolist())
        d = view.as_df(); view.close()
        if "track_id" not in d.columns:
            continue
        tid = pd.to_numeric(d["track_id"], errors="coerce").to_numpy(dtype=float)
        t_of = dict(zip(d["label"].astype(np.int64), tid))
        ids[idx] = [t_of.get(int(l), np.nan) for l in labs[idx]]
        found = True
    return ids if found else None


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


def permutation_groups(indices, times=None):
    """The node groups a label-shuffle null permutes WITHIN: one group per timepoint when `times` (a
    per-node frame index over the WHOLE graph) is given, else one global group. `indices` are the nodes
    taking part in the shuffle. A per-timepoint (block-diagonal) graph must shuffle within a frame — a
    global shuffle migrates labels across frames and destroys the per-frame composition the null
    preserves. The one shuffle-grouping rule, shared by the contact test and the spatial-pattern test
    (`spatial_pattern_utils`)."""
    idx = np.asarray(indices, dtype=np.int64)
    if times is None:
        return [idx]
    t = np.asarray(times).ravel()
    return [idx[t[idx] == tv] for tv in np.unique(t[idx])]


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


def _permutation_expected(conn_row, conn_col, pop_codes, n_pops, groups):
    """EXACT mean of `_contact_counts` over all relabellings that permute `pop_codes` within each index
    array in `groups` (nodes outside every group keep their label). Each node gets a label distribution
    — one-hot if fixed, the group's composition if shuffled — and an edge's expected pair matrix is the
    outer product of its endpoints' distributions, EXCEPT when both endpoints share a group: they are
    drawn without replacement, so P(a,b) = n_a·n_b / (N(N−1)) and P(a,a) = n_a(n_a−1) / (N(N−1)).
    Centring the two-sided permutation p on this rather than the sampled mean matters for discrete
    nulls: a symmetric null centred on a noisy mean of 0.99 instead of 1 loses one whole tail."""
    n = len(pop_codes)
    dist = np.zeros((n, n_pops))
    fixed = pop_codes >= 0
    gid = np.full(n, -1, dtype=np.int64)
    for k, g in enumerate(groups):
        gid[g] = k
        fixed[g] = False
    dist[np.where(fixed)[0], pop_codes[fixed]] = 1.0
    for g in groups:
        if len(g):
            dist[g] = np.bincount(pop_codes[g], minlength=n_pops) / len(g)
    m = dist[conn_row].T @ dist[conn_col]                    # ordered (row-end, col-end) pair mass
    same = (gid[conn_row] >= 0) & (gid[conn_row] == gid[conn_col])
    for k, n_k in zip(*np.unique(gid[conn_row][same], return_counts=True)):
        g = groups[k]
        cnt = np.bincount(pop_codes[g], minlength=n_pops).astype(float)
        big_n = float(len(g))
        without = (np.outer(cnt, cnt) - np.diag(cnt)) / (big_n * (big_n - 1.0))
        m += n_k * (without - np.outer(cnt, cnt) / big_n ** 2)
    return m + m.T - np.diag(np.diag(m))


def pairwise_contact_logodds(conn, pop_codes, n_pops, pseudocount=0.5,
                             n_permutations=1000, random_state=0, times=None, shuffle_codes=None):
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
    two-sided EMPIRICAL p-value ((#|null−µ| ≥ |obs−µ| + 1)/(n+1), so p is never 0), µ being the exact
    permutation mean (`_permutation_expected`) rather than the sampled one. `n_permutations=0`
    skips the test and returns NaN z/p — the fast path.

    Only ASSIGNED nodes (code ≥ 0) are shuffled, among themselves: which nodes fall outside the basis is
    a property of the segmentation, not of the biology under test, so it is conditioned on rather than
    randomised. `times` (a per-node frame index) restricts each shuffle to WITHIN a timepoint — required
    for a per-timepoint (block-diagonal) graph, where a global shuffle would migrate labels across frames
    and destroy the per-frame composition the null is supposed to preserve.

    CAVEAT: this is a complete-spatial-randomness-of-labels null. It preserves the graph and the label
    counts but not larger-scale tissue structure, so in strongly zoned tissue it can call weak
    associations significant. squidpy's `nhood_enrichment` shares this limitation.

    CONDITIONAL NULL — `shuffle_codes` (a subset of population codes) shuffles labels only among the
    nodes of THOSE populations; every other node keeps its label. This is the random-labelling test of
    spatial point-pattern analysis (Diggle, *Statistical Analysis of Spatial and Spatio-Temporal Point
    Patterns*, 3rd ed., CRC 2013, §5.4; Baddeley, Rubak & Turner, *Spatial Point Patterns*, CRC 2015,
    §14.7): e.g. shuffling CD69+ vs CD69− among T cells with the target cells fixed asks "do CD69+ T
    cells sit next to the targets more than T cells in general do?". Because both compared groups share
    the same positions, tissue zoning and the graph's edges cancel — the caveat above no longer applies
    to the shuffled pairs. The CODEX expected count (global frequencies) does not describe this null,
    so `expected` becomes the EXACT mean of the permutation null (`_permutation_expected`, no sampling —
    so `n_permutations=0` still gives the effect size) and the log-odds is observed vs that mean. Pairs
    with no shuffled member have a fixed count → z NaN, p = 1.

    Returns (observed, expected, log_odds, zscore, pvalue) as (n_pops, n_pops) symmetric matrices."""
    pop_codes = np.asarray(pop_codes, dtype=np.int64)
    n = len(pop_codes)
    assigned = pop_codes >= 0
    n_assigned = int(assigned.sum())
    freq = np.bincount(pop_codes[assigned], minlength=n_pops).astype(float) / max(n_assigned, 1)

    row, col = undirected_edges(conn)               # each edge once, either direction; no self-loops
    observed = _contact_counts(row, col, pop_codes, n_pops)
    n_edges = float(observed.sum() + np.trace(observed)) / 2.0   # pop–pop edges (diagonal counted once)

    # expected # of a–b edges under random labelling of nodes with frequencies f: an edge is an
    # unordered pair of endpoints, so P(edge is {a,b}) = 2·f_a·f_b (a≠b) or f_a² (a==b).
    ff = np.outer(freq, freq)
    expected = n_edges * (2.0 * ff)
    np.fill_diagonal(expected, n_edges * np.diag(ff))

    shuffled_nodes = assigned if shuffle_codes is None else np.isin(pop_codes, np.asarray(shuffle_codes))
    idx_shuffled = np.where(shuffled_nodes)[0]
    groups = permutation_groups(idx_shuffled, times)   # within frame on a per-timepoint graph
    # the conditional null's expected count is its exact permutation mean (no frequency formula fits it)
    if shuffle_codes is not None:
        expected = _permutation_expected(row, col, pop_codes, n_pops, groups)

    with np.errstate(divide="ignore", invalid="ignore"):
        log_odds = np.log((observed + pseudocount) / (expected + pseudocount))

    n_perm = int(n_permutations)
    if n_perm <= 0 or n_assigned == 0:
        nan = np.full((n_pops, n_pops), np.nan)
        return observed, expected, log_odds, nan, nan

    rng = np.random.default_rng(int(random_state))
    null = np.empty((n_perm, n_pops, n_pops))
    shuffled = pop_codes.copy()
    for i in range(n_perm):
        for g in groups:
            shuffled[g] = rng.permutation(pop_codes[g])
        null[i] = _contact_counts(row, col, shuffled, n_pops)

    mu = expected if shuffle_codes is not None else _permutation_expected(row, col, pop_codes, n_pops, groups)
    sd = null.std(axis=0)
    with np.errstate(divide="ignore", invalid="ignore"):
        zscore = np.where(sd > 0, (observed - mu) / sd, np.nan)
    # two-sided empirical p with the +1 correction (Davison & Hinkley 1997) → p ∈ [1/(n+1), 1]; the
    # tolerance keeps exactly-equidistant integer counts tied despite float error in `mu`
    extreme = (np.abs(null - mu) >= np.abs(observed - mu) - 1e-9).sum(axis=0)
    pvalue = (extreme + 1.0) / (n_perm + 1.0)
    return observed, expected, log_odds, zscore, pvalue


def neighbourhood_composition(conn, pop_codes, n_pops, include_other=False):
    """Per-cell neighbourhood composition vector: row i = the normalised frequency of each population
    (code 0..n_pops-1) among cell i's graph neighbours (CytoMAP / cecelia "i-niche" feature).

    `conn` is the (n_cells, n_cells) sparse connectivity matrix; `pop_codes` is a length-n_cells int
    array (each cell's population index, or UNASSIGNED). Returns a dense float32 matrix; a cell with no
    neighbours gets an all-zero row. Formula matches legacy cellRegionsStats (`freq = n / sum(n)`).
    Row-wise on purpose: on a directed kNN graph a cell's neighbourhood is its OWN k nearest (the usual
    kNN niche), not every cell that lists it — unlike edge counts, which use `undirected_edges`.

    `include_other=True` adds ONE extra trailing column counting UNASSIGNED neighbours — cells present in
    the graph but outside the basis. This matters for correctness, not just completeness: with it, the
    row is the true composition of the neighbourhood and sums to 1; without it, the fractions are
    renormalised over basis neighbours only, so a cell surrounded mostly by unlabelled cells looks
    identical to one surrounded entirely by basis cells. CytoMAP keeps the same "other" bin.
    """
    return normalise_counts(neighbourhood_counts(conn, pop_codes, n_pops, include_other=include_other))


def neighbourhood_counts(conn, pop_codes, n_pops, include_other=False):
    """The COUNTS behind `neighbourhood_composition`: row i = how many of cell i's graph neighbours are
    in each population (+ the trailing "other" column with `include_other`). Kept separate so counts can
    be summed over a group of rows (a track's frames, `sum_rows_by_group`) BEFORE normalising — summing
    fractions instead would weigh a frame with one neighbour as much as a frame with twenty."""
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
    return np.asarray((conn > 0).astype(np.float32) @ onehot.toarray())


def normalise_counts(counts):
    """Row-normalise a count matrix to fractions (`freq = n / sum(n)`, legacy cellRegionsStats). A row
    with no counts — an isolated cell, a track that never had a neighbour — stays all-zero."""
    counts = np.asarray(counts, dtype=np.float32)
    totals = counts.sum(axis=1, keepdims=True)
    totals[totals == 0] = 1.0                       # avoid /0; empty rows stay all-zero
    return (counts / totals).astype(np.float32)


def effective_types(comp):
    """Local diversity of a neighbourhood: the effective number of cell types around each row of a
    composition matrix `comp` (n, k) — the Hill number of order 1, exp of the Shannon entropy
    (Hill 1973, Ecology 54:427, DOI 10.2307/1934352; Jost 2006, Oikos 113:363,
    DOI 10.1111/j.2006.0030-1299.14714.x). k types in equal shares give k; one type gives 1.

    Pass the BASIS columns only, never the "other" bin: "other" lumps every unlabelled cell into one
    column, so it is not a type. Rows are renormalised over what is passed. A row with no typed
    neighbours gets 0, not NaN — it is a real place (an isolated cell, or one ringed by "other"), and a
    NaN would drop it from gates, plots and the HMM."""
    p = normalise_counts(comp).astype(np.float64)
    with np.errstate(divide="ignore", invalid="ignore"):
        h = -np.where(p > 0, p * np.log(p), 0.0).sum(axis=1)
    out = np.exp(h)
    out[p.sum(axis=1) == 0] = 0.0
    return out.astype(np.float32)


def sum_rows_by_group(rows, keys):
    """Sum the rows of `rows` (n, k) that share a key. `keys` is a length-n sequence of hashables (e.g.
    (uID, valueName, track_id) tuples). Returns `(sums, inverse, uniques)`: `sums[g]` is group g's summed
    row, `inverse[i]` is row i's group, `uniques[g]` its key — so a per-group result broadcasts back to
    the rows as `result[inverse]`. Groups are numbered in first-appearance order."""
    import pandas as pd
    rows = np.asarray(rows, dtype=np.float64)
    inverse, uniques = pd.factorize(pd.Series(list(keys), dtype=object), sort=False)
    sums = np.zeros((len(uniques), rows.shape[1]), dtype=np.float64)
    np.add.at(sums, inverse, rows)
    return sums, inverse, list(uniques)


# ── region clustering: from a loaded graph to the rows that get clustered ─────────────────────────
# Three ways to pool (`clustRegions` `pooling`), one shape out: per image, a count matrix of the rows
# that will be clustered and, per basis cell, the row it takes its region from.
#   "frame" — a cell-to-cell graph: one row per cell-frame (its neighbours' counts); on a WINDOW graph:
#             one row per window per frame.
#   "track" — a cell-to-cell graph: a track's frames summed into one row.
#   "movie" — a WINDOW graph: one row per window, cells counted over every frame, per frame on average.
POOLINGS = {"cells": ("frame", "track"), "windows": ("frame", "movie")}


def image_region_rows(a, obs_all, codes_all, n_pops, pooling, include_other, props_paths):
    """One image's contribution: `(counts, own, keep, problem)`. `counts` are the image's rows to
    cluster; `own[i]` is the row basis cell `i` (of the nodes kept by `keep`) takes its region from;
    `problem` is a user-facing reason the pooling can't run on this graph (else None). Only basis cells
    are kept — composition counts every neighbour, but a non-basis cell gets no region."""
    grid = graph_window_grid(a)
    kind = "windows" if grid is not None else "cells"
    if pooling not in POOLINGS[kind]:
        return None, None, None, (
            f"pooling \"{pooling}\" needs a {'cell-to-cell' if kind == 'windows' else 'fixed-window'} graph "
            f"— this graph is {kind.replace('cells', 'cell-to-cell')}; use one of {', '.join(POOLINGS[kind])}")
    keep = np.asarray(codes_all) >= 0
    has_t = "_t" in obs_all.columns
    if grid is not None:
        frames = pd_codes(obs_all["_t"]) if has_t else None
        groups = frames if pooling == "frame" else None
        counts, n_win = window_counts(a.obsm["spatial"], codes_all, n_pops, grid, groups=groups,
                                      include_other=include_other)
        # per window per frame, as if the whole window lay in the field: the cell count beside the
        # composition then means the same at the image edge, and in a short movie as in a long one
        n_frames = 1 if (groups is not None or frames is None) else int(frames.max()) + 1
        counts = counts / (np.tile(window_coverage(grid), len(counts) // n_win)[:, None] * n_frames)
        own = window_tiles(a.obsm["spatial"], grid) + (0 if groups is None else groups * n_win)
        return counts, own[keep], keep, None
    counts = neighbourhood_counts(a.obsp["spatial_connectivities"], codes_all, n_pops,
                                  include_other=include_other)
    if pooling == "frame":
        idx = np.flatnonzero(keep)
        return counts[keep], np.arange(len(idx)), keep, None
    # "track": a graph pooled over frames would count a cell's own other frames as its neighbours
    if not has_t:
        return None, None, None, ("the graph was built over all frames together — pooling per track "
                                  "needs a per-timepoint graph (Neighbour graph → Per timepoint)")
    tids = node_track_ids(obs_all, props_paths)
    if tids is None:
        return None, None, None, "no tracks — pooling per track needs tracked cells"
    keep = keep & ~np.isnan(tids)
    keys = list(zip(obs_all["valueName"].astype(str).to_numpy()[keep], tids[keep].astype(np.int64)))
    sums, inverse, _ = sum_rows_by_group(counts[keep], keys)
    return sums, inverse, keep, None


def pd_codes(values):
    """Dense 0-based integer codes for `values` (frame indices → 0..F-1)."""
    import pandas as pd
    return pd.factorize(pd.Series(np.asarray(values)), sort=True)[0].astype(np.int64)


def pool_region_rows(blocks):
    """Stack the images' `(counts, own)` blocks (from `image_region_rows`) into one matrix, dropping
    rows nobody counted (an empty window): `(counts, inverse)`, `inverse[i]` = pooled cell i's row."""
    counts = np.vstack([b[0] for b in blocks]) if blocks else np.zeros((0, 0))
    offsets = np.cumsum([0] + [len(b[0]) for b in blocks[:-1]])
    own = np.concatenate([np.asarray(b[1], dtype=np.int64) + o for b, o in zip(blocks, offsets)]) \
        if blocks else np.zeros(0, dtype=np.int64)
    used = counts.sum(axis=1) > 0
    used[own] = True                       # a row a cell takes its region from stays, even if empty
    remap = np.full(len(counts), -1, dtype=np.int64)
    remap[used] = np.arange(int(used.sum()))
    return counts[used], remap[own]


# ── region clustering: composition rows → clusters, then back to one row per cell ────────────────
def cluster_composition(X, var_names, method="leiden", num_clusters=5, resolution=1.0,
                        create_umap=True, random_state=0, log=None, run_env=None):
    """Cluster neighbourhood-composition rows (`clustRegions`): one row per cell, or per track when a
    run pools per track. k-means (fixed count, sklearn) or Leiden through `find_populations_on_distinct_rows` with no
    transform/normalisation — the rows are already fractions in [0, 1]. Returns an AnnData whose obs
    `clusters` holds string codes and, with `create_umap`, `obsm['X_umap']`."""
    import anndata as ad
    import pandas as pd
    _log = log if callable(log) else (lambda _m: None)
    adata = ad.AnnData(np.asarray(X, dtype=np.float32))
    adata.var_names = [str(c) for c in var_names]
    adata.obs_names = [str(i) for i in range(adata.n_obs)]
    if method == "kmeans":
        import scanpy as sc
        from sklearn.cluster import KMeans
        km = KMeans(n_clusters=int(num_clusters), random_state=random_state, n_init=10).fit(adata.X)
        adata.obs["clusters"] = pd.Categorical([str(c) for c in km.labels_])
        _log(f">> k-means: {num_clusters} regions")
        if create_umap:
            sc.pp.neighbors(adata, use_rep="X")
            sc.tl.umap(adata, random_state=random_state)
    else:
        # compositions repeat exactly (k/6 steps on a Delaunay graph), which splits Leiden's kNN graph
        # into one island per repeated vector — cluster the distinct vectors once, then map back
        from cecelia.utils.clustering_utils import find_populations_on_distinct_rows
        find_populations_on_distinct_rows(adata, resolution=resolution, axis="NONE", transformation="NONE",
                                          create_umap=create_umap, backend="auto", random_state=random_state,
                                          log=log, run_env=run_env if run_env is not None else {})
    return adata


def drop_empty_other(comp_obs, other_col):
    """Drop the "other" composition column from `comp_obs` (column name → per-cell values) when it is
    all zero, and say whether it was. It is all zero exactly when the graph holds nothing outside the
    basis (built over the basis populations themselves): then it is no measurement, only a flat heatmap
    row that invites "where does this come from?". The region run tells Julia, so the clustfeatures
    sidecar doesn't advertise it, which keeps the option on by default without a dead measure."""
    if other_col in comp_obs and not np.any(np.asarray(comp_obs[other_col]) > 0):
        del comp_obs[other_col]
        return True
    return False


def broadcast_clusters(clustered, X, obs, inverse=None):
    """The per-CELL AnnData `split_back_and_write` expects, from a clustering of cells or of groups.
    `X` (n_cells, k) and `obs` (uID, valueName, label) are per cell; `inverse` maps each cell to its row
    in `clustered` (None = `clustered` IS per cell). Per group, the UMAP point goes on each group's first
    cell only, the rest NaN — the cluster map drops non-finite rows, so a track shows as ONE point."""
    import anndata as ad
    import pandas as pd
    n = X.shape[0]
    cells = ad.AnnData(np.asarray(X, dtype=np.float32))
    cells.obs_names = [str(i) for i in range(n)]
    for c in ("uID", "valueName", "label"):
        cells.obs[c] = np.asarray(obs[c])
    rows = np.arange(n) if inverse is None else np.asarray(inverse)
    cells.obs["clusters"] = pd.Categorical(clustered.obs["clusters"].astype(str).to_numpy()[rows])
    if "X_umap" in clustered.obsm:
        emb = np.asarray(clustered.obsm["X_umap"])
        first = np.zeros(n, dtype=bool)
        first[np.unique(rows, return_index=True)[1]] = True
        um = np.full((n, emb.shape[1]), np.nan)
        um[first] = emb[rows[first]]
        cells.obsm["X_umap"] = um
    return cells
