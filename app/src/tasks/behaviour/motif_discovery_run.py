"""
behaviour.motif_discovery (set-scope) — Python runner. P1 POC.

Reads a pooled per-cell frame from the Julia side (columns as JSON arrays: uID, valueName,
label, track_id, t, speed, angle, hmm_state), runs multivariate matrix profile per track via
STUMPY `stumpy.mstump`, keeps the top-K positions by lowest matrix-profile distance across the
set, extracts each position's (window x 3) feature vector, clusters those vectors with a
Euclidean k-NN graph + Leiden (scanpy), assigns each cell that falls within any instance's
window its class + medoid-distance + instance id (overlap = highest-confidence wins per
MOTIF_DISCOVERY_PLAN Decision 9), and writes a results JSON the Julia side reads.

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


def _extract_feature_vector(pooled, start_row, window):
    """Concatenated [speed, angle, hmm] window flattened to length 3*window.

    Windows containing NaN (across the flattened vector) are rejected -- returns None.
    """
    speed = pooled["speed"].to_numpy()[start_row: start_row + window]
    angle = pooled["angle"].to_numpy()[start_row: start_row + window]
    hmm = pooled["hmm"].to_numpy()[start_row: start_row + window]
    if speed.size < window or np.any(np.isnan(speed)) or np.any(np.isnan(angle)) or np.any(np.isnan(hmm)):
        return None
    return np.concatenate([speed, angle, hmm]).astype(np.float32)


def _leiden_cluster(feature_matrix, resolution, random_state, log):
    """k-NN + Leiden on a (K, F) feature matrix. Returns integer class codes (0..C-1)."""
    import anndata as ad
    import scanpy as sc
    K, _F = feature_matrix.shape
    n_neighbors = int(min(15, max(3, K // 4)))
    adata = ad.AnnData(feature_matrix.astype(np.float32))
    adata.obs_names = [str(i) for i in range(K)]
    log(f">> k-NN Leiden: K={K}, n_neighbors={n_neighbors}, resolution={resolution}")
    sc.pp.neighbors(adata, use_rep="X", n_neighbors=n_neighbors)
    sc.tl.leiden(adata, resolution=resolution, key_added="motif_class",
                 flavor="leidenalg", random_state=random_state)
    codes = adata.obs["motif_class"].astype(int).to_numpy()
    return codes


def _medoid_distances(feature_matrix, codes):
    """For each row: Euclidean distance to its class' medoid.

    Medoid = the class member minimising the sum of pairwise Euclidean distances to the other
    class members. Returns (distances shape (K,), medoid_row_by_class dict[int,int]).
    """
    K = feature_matrix.shape[0]
    dists = np.zeros(K, dtype=np.float32)
    medoid_by_class = {}
    for cls in np.unique(codes):
        idx = np.where(codes == cls)[0]
        if idx.size == 1:
            medoid_by_class[int(cls)] = int(idx[0])
            dists[idx] = 0.0
            continue
        sub = feature_matrix[idx].astype(np.float64)
        # pairwise squared distances
        d2 = np.sum((sub[:, None, :] - sub[None, :, :]) ** 2, axis=2)
        sums = np.sqrt(d2).sum(axis=1)
        medoid_local = int(np.argmin(sums))
        medoid_by_class[int(cls)] = int(idx[medoid_local])
        centroid_vec = sub[medoid_local]
        dists[idx] = np.linalg.norm(sub - centroid_vec, axis=1).astype(np.float32)
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

    # Build (K, 3*window) feature matrix; drop any window that contains NaN.
    feats = []
    kept_meta = []
    for mp_val, start_row, uid, vn, tid in survivors:
        v = _extract_feature_vector(pooled, start_row, window)
        if v is None:
            continue
        feats.append(v)
        kept_meta.append((mp_val, start_row, uid, vn, tid))
    if len(feats) < 2:
        log.log("[ERROR] motif_discovery: no usable feature vectors after NaN filter"); return
    feature_matrix = np.stack(feats, axis=0)
    log.progress(4, 6)

    codes = _leiden_cluster(feature_matrix, resolution=resolution,
                            random_state=random_state, log=log.log)
    n_classes_found = int(len(np.unique(codes)))
    log.log(f">> Leiden produced {n_classes_found} class(es) (target hint: {num_classes})")

    dists, medoid_by_class = _medoid_distances(feature_matrix, codes)
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
