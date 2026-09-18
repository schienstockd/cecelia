"""
behaviour.motif_discovery (set-scope) — Python runner. P2 (DTW cluster-time metric).

Reads a pooled per-cell frame from the Julia side (columns as JSON arrays: uID, valueName,
label, track_id, t, speed, angle, hmm_state), runs multivariate matrix profile per track via
STUMPY `stumpy.mstump`, keeps the top-K positions by lowest matrix-profile distance across
the set, extracts each position's (window x 3) time-series window, computes pairwise
subsequence DTW between windows via `dtaidistance.dtw_ndim` (K×K distance matrix), clusters
those windows with a DTW-precomputed k-NN graph + Leiden (scanpy), and assigns each cell
that falls within any instance's window its class + medoid-DTW-distance + instance id
(overlap = highest-confidence wins per MOTIF_DISCOVERY_PLAN Decision 9). Writes a results
JSON the Julia side reads.

The DTW step lives here (not in Julia) — MOTIF_DISCOVERY_PLAN Decision 4 was flipped
2026-09-18 so the whole pipeline is one runner; the plan's original Julia-DTW leg forced a
Python→Julia→Python sandwich without an architectural benefit.

Citations
---------
- STUMPY:  Law, S. M. (2019). "STUMPY: A Powerful and Scalable Python Library for Time Series
  Data Mining." Journal of Open Source Software, 4(39), 1504.
  https://doi.org/10.21105/joss.01504 - https://github.com/TDAmeritrade/stumpy
- Matrix profile / STOMP: Yeh, C.-C. M. et al. (2016). "Matrix Profile I".
  https://www.cs.ucr.edu/~eamonn/MatrixProfile.html
- Berman, G. J. et al. (2014). "Mapping the stereotyped behaviour of freely moving fruit
  flies." J. R. Soc. Interface 11(99): 20140672.
  https://doi.org/10.1098/rsif.2014.0672  -- closest published analogue for unsupervised
  behavioural motif emergence.
- dtaidistance: Meert, W. et al. (2020). "wannesm/dtaidistance." Zenodo.
  https://doi.org/10.5281/zenodo.7158824 - https://github.com/wannesm/dtaidistance
"""

import os

import numpy as np
import pandas as pd

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher (app/src/py_runner.jl).
import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import write_json_atomic

# STUMPY is bundled in the default pixi env. Import lazily so a missing install fails loudly here
# rather than at module load, keeping the "params-only" test path in Julia usable.


def _to_float(arr, dtype=np.float32):
    """JSON -> numpy: None -> NaN. Preserves length."""
    out = np.full(len(arr), np.nan, dtype=dtype)
    for i, v in enumerate(arr):
        if v is not None:
            try:
                out[i] = float(v)
            except (TypeError, ValueError):
                out[i] = np.nan
    return out


def _to_int(arr, missing=-1):
    out = np.full(len(arr), missing, dtype=np.int64)
    for i, v in enumerate(arr):
        if v is None:
            continue
        try:
            fv = float(v)
            if not np.isnan(fv):
                out[i] = int(fv)
        except (TypeError, ValueError):
            pass
    return out


def _per_track_mp(track_rows, window, log):
    """Run mstump on ONE track's [speed, angle, hmm_state] stream (row-order == time-order).

    Returns (mp, positions_start_row) where mp is a 1-D array (best matrix profile per position,
    across all channels -- the last row of stumpy.mstump's mp matrix which is the "all-dim" MP),
    and positions_start_row are the ORIGINAL pooled-frame row indices that each MP position maps
    back to (position i starts at row track_rows[i]).
    """
    import stumpy
    n = len(track_rows)
    if n < window + 1:
        return None, None
    T = np.stack([track_rows["speed"].to_numpy(dtype=np.float64),
                  track_rows["angle"].to_numpy(dtype=np.float64),
                  track_rows["hmm"].to_numpy(dtype=np.float64)], axis=0)  # (3, n)
    # NaN safety: stumpy tolerates NaN by treating those subsequences as invalid.
    try:
        mp, _idx = stumpy.mstump(T, m=window)
    except Exception as e:
        log(f"[WARN] mstump failed on a track (n={n}): {e}")
        return None, None
    # mp shape (d, n-m+1). "All-dim" profile = last row (uses all channels).
    all_dim_mp = np.asarray(mp[-1], dtype=np.float64)
    pos_row_index = track_rows.index.to_numpy()[: all_dim_mp.size]
    return all_dim_mp, pos_row_index


def _extract_window(pooled, start_row, window):
    """(window, 3) time-series slice [speed, angle, hmm]. NaN-containing → None."""
    speed = pooled["speed"].to_numpy()[start_row: start_row + window]
    angle = pooled["angle"].to_numpy()[start_row: start_row + window]
    hmm = pooled["hmm"].to_numpy()[start_row: start_row + window]
    if speed.size < window or np.any(np.isnan(speed)) or np.any(np.isnan(angle)) or np.any(np.isnan(hmm)):
        return None
    return np.stack([speed, angle, hmm], axis=1).astype(np.float64)   # (W, 3)


def _pairwise_dtw(windows, log):
    """K×K symmetric multivariate DTW distance matrix over (K, W, 3) windows.

    `dtw_ndim.distance_matrix_fast` takes list of (W, D)-shaped series and returns an upper-tri
    K×K matrix (lower is inf); mirror it into symmetric. Multivariate DTW is O(K² · W · D)
    which at K=100, W=8, D=3 is ≈ 240k cell ops — trivial. The `use_c=True` path is the C
    backend (compiled at install time); it falls back to Python if the C extension isn't
    available.
    """
    from dtaidistance import dtw_ndim
    K = len(windows)
    log(f">> pairwise DTW (K={K}, W={windows[0].shape[0]}, D={windows[0].shape[1]})")
    # dtaidistance's fast path wants a list of contiguous (W, D) arrays.
    series = [np.ascontiguousarray(w) for w in windows]
    mat = dtw_ndim.distance_matrix_fast(series)
    # Fold upper→symmetric (lower is inf, diagonal is 0).
    for i in range(K):
        for j in range(i):
            mat[i, j] = mat[j, i]
    np.fill_diagonal(mat, 0.0)
    return mat.astype(np.float64)


def _leiden_cluster(dist_matrix, resolution, random_state, log):
    """k-NN + Leiden on a K×K precomputed distance matrix. Returns integer class codes.

    Passes `metric='precomputed'` so `sc.pp.neighbors` treats `adata.X` (== the K×K distance
    matrix) as the pairwise distances rather than a feature matrix — this is the entry point
    for feeding DTW into the same Leiden path the other clustering tasks use, without pulling
    `find_populations`'s transform/normalise/UMAP surface (irrelevant at K≈100).
    """
    import anndata as ad
    import scanpy as sc
    K = dist_matrix.shape[0]
    n_neighbors = int(min(15, max(3, K // 4)))
    adata = ad.AnnData(dist_matrix.astype(np.float32))
    adata.obs_names = [str(i) for i in range(K)]
    log(f">> k-NN Leiden (precomputed DTW): K={K}, n_neighbors={n_neighbors}, resolution={resolution}")
    sc.pp.neighbors(adata, use_rep="X", n_neighbors=n_neighbors, metric="precomputed")
    sc.tl.leiden(adata, resolution=resolution, key_added="motif_class",
                 flavor="leidenalg", random_state=random_state)
    codes = adata.obs["motif_class"].astype(int).to_numpy()
    return codes


def _medoid_distances(dist_matrix, codes):
    """For each row: DTW distance to its class' medoid.

    Medoid = class member minimising the sum of pairwise DTW distances to the rest of its class
    (row-sum argmin over the precomputed matrix — no re-computation). Returns
    (distances shape (K,), medoid_row_by_class dict[int,int]).
    """
    K = dist_matrix.shape[0]
    dists = np.zeros(K, dtype=np.float32)
    medoid_by_class = {}
    for cls in np.unique(codes):
        idx = np.where(codes == cls)[0]
        if idx.size == 1:
            medoid_by_class[int(cls)] = int(idx[0])
            dists[idx] = 0.0
            continue
        sub = dist_matrix[np.ix_(idx, idx)]           # (m, m) DTW sub-matrix
        sums = sub.sum(axis=1)
        medoid_local = int(np.argmin(sums))
        medoid_row = int(idx[medoid_local])
        medoid_by_class[int(cls)] = medoid_row
        dists[idx] = dist_matrix[idx, medoid_row].astype(np.float32)
    return dists, medoid_by_class


def run(params):
    log = script_utils.get_logfile_utils(params)

    suffix       = script_utils.get_param(params, "suffix", default="default")
    window       = int(script_utils.get_param(params, "windowSize", default=8))
    top_k        = int(script_utils.get_param(params, "topK", default=100))
    num_classes  = int(script_utils.get_param(params, "numClasses", default=3))
    resolution   = float(script_utils.get_param(params, "resolution", default=0.5))
    random_state = int(script_utils.get_param(params, "randomState", default=0))
    results_path = script_utils.get_param(params, "resultsOutPath", default=None)
    if results_path is None:
        log.log("[ERROR] motif_discovery: resultsOutPath missing"); return

    uids       = script_utils.get_param(params, "uIDs", default=[]) or []
    vns        = script_utils.get_param(params, "valueNames", default=[]) or []
    labels     = script_utils.get_param(params, "labels", default=[]) or []
    track_ids  = script_utils.get_param(params, "trackIds", default=[]) or []
    ts         = script_utils.get_param(params, "ts", default=[]) or []
    speed      = script_utils.get_param(params, "speed", default=[]) or []
    angle      = script_utils.get_param(params, "angle", default=[]) or []
    hmm_state  = script_utils.get_param(params, "hmmState", default=[]) or []
    n = len(labels)
    if n == 0:
        log.log("[ERROR] motif_discovery: no pooled rows"); return

    log.log(f">> motif_discovery: pooled={n}, window={window}, topK={top_k}, "
            f"numClasses={num_classes}, resolution={resolution}")
    log.progress(1, 6)

    pooled = pd.DataFrame({
        "uID":      [str(u) for u in uids],
        "vn":       [str(v) for v in vns],
        "label":    _to_int(labels),
        "track_id": _to_int(track_ids, missing=-1),
        "t":        _to_float(ts, dtype=np.float64),
        "speed":    _to_float(speed),
        "angle":    _to_float(angle),
        "hmm":      _to_float(hmm_state),
    })

    # Group each (uID, vn, track_id) into a time-ordered contiguous stream; run mstump per track.
    valid = pooled[(pooled["track_id"] > 0) & pooled["t"].notna()].copy()
    valid.sort_values(["uID", "vn", "track_id", "t"], inplace=True)
    log.log(f">> {valid.shape[0]} tracked cells across "
            f"{valid.groupby(['uID','vn','track_id']).ngroups} track(s)")
    log.progress(2, 6)

    all_positions = []   # list of (mp_value, pooled_row_index, uID, vn, track_id)
    n_track = 0
    for _key, g in valid.groupby(["uID", "vn", "track_id"], sort=False):
        n_track += 1
        mp, pos_rows = _per_track_mp(g, window, log.log)
        if mp is None:
            continue
        for i, d in enumerate(mp):
            if np.isnan(d) or np.isinf(d):
                continue
            all_positions.append((float(d), int(pos_rows[i]),
                                  g["uID"].iat[0], g["vn"].iat[0], int(g["track_id"].iat[0])))
    log.log(f">> {len(all_positions)} candidate positions from {n_track} track(s)")
    log.progress(3, 6)

    if len(all_positions) < 2:
        log.log("[ERROR] motif_discovery: not enough candidate positions to cluster"); return

    # Sort ascending by MP value; keep the top K -- these are the strongest motif matches.
    all_positions.sort(key=lambda p: p[0])
    survivors = all_positions[: min(top_k, len(all_positions))]
    log.log(f">> keeping top {len(survivors)} positions (MP min={survivors[0][0]:.4f}, "
            f"max={survivors[-1][0]:.4f})")

    # Build list of (W, 3) windows; drop any that contain NaN.
    windows = []
    kept_meta = []
    for mp_val, start_row, uid, vn, tid in survivors:
        w = _extract_window(pooled, start_row, window)
        if w is None:
            continue
        windows.append(w)
        kept_meta.append((mp_val, start_row, uid, vn, tid))
    if len(windows) < 2:
        log.log("[ERROR] motif_discovery: no usable windows after NaN filter"); return
    log.progress(4, 6)

    dtw_matrix = _pairwise_dtw(windows, log.log)
    codes = _leiden_cluster(dtw_matrix, resolution=resolution,
                            random_state=random_state, log=log.log)
    n_classes_found = int(len(np.unique(codes)))
    log.log(f">> Leiden produced {n_classes_found} class(es) (target hint: {num_classes})")

    dists, medoid_by_class = _medoid_distances(dtw_matrix, codes)
    class_names = [f"Motif {i + 1}" for i in range(n_classes_found)]
    class_id_to_name = {cls: class_names[i] for i, cls in enumerate(sorted(medoid_by_class))}
    log.progress(5, 6)

    # Span-broadcast each instance's (class, distance, id) over its window; overlap = keep the
    # instance with the LOWEST distance (highest confidence -- Decision 9).
    n = pooled.shape[0]
    best_dist = np.full(n, np.inf, dtype=np.float32)
    class_by_cell = [None] * n
    dist_by_cell = [None] * n
    instance_by_cell = [None] * n
    for inst_id, (feat_row, (mp_val, start_row, _uid, _vn, _tid)) in enumerate(zip(range(len(kept_meta)), kept_meta)):
        cls = int(codes[feat_row])
        cls_name = class_id_to_name[cls]
        d = float(dists[feat_row])
        for k in range(window):
            row = start_row + k
            if row >= n:
                break
            if d < best_dist[row]:
                best_dist[row] = d
                class_by_cell[row] = cls_name
                dist_by_cell[row] = d
                instance_by_cell[row] = int(inst_id)

    run_stats = {
        "nCellsPooled":       int(n),
        "nTracks":             int(n_track),
        "nCandidatePositions": int(len(all_positions)),
        "nSurvivors":          int(len(kept_meta)),
        "nClasses":            int(n_classes_found),
        "window":              int(window),
        "topK":                int(top_k),
        "resolution":          float(resolution),
        "mpMin":               float(kept_meta[0][0]) if kept_meta else None,
        "mpMax":               float(kept_meta[-1][0]) if kept_meta else None,
    }
    payload = {
        "class_by_cell":       class_by_cell,
        "distance_by_cell":    dist_by_cell,
        "instance_id_by_cell": instance_by_cell,
        "class_names":         class_names,
        "run_stats":           run_stats,
    }
    os.makedirs(os.path.dirname(results_path), exist_ok=True)
    write_json_atomic(results_path, payload)
    log.log(f">> wrote results: {results_path} "
            f"({sum(1 for c in class_by_cell if c is not None)} cells assigned)")
    log.progress(6, 6)


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] no --params file", flush=True)
        return
    run(params)


if __name__ == "__main__":
    main()
