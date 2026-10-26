"""ROI auto-discovery — the pure core (docs/todo/ROI_TRIAGE_PLAN.md).

Per-object cues → one score → spatio-temporal clusters of high-scoring objects → ranked ROIs.
Nothing here reads a store or a label table: the runner
(`app/src/tasks/spatialAnalysis/roi_discovery_run.py`) does the I/O and hands arrays in, so every
step is unit-testable on synthetic points (`tests/test_roi_utils.py`).

Units: positions are µm (the runner scales pixel centroids by the voxel size), time is in frames.
No tracking — every cue is computed per frame, and "change" cues compare a position's neighbourhood
across t±1, never an object's identity.
"""

import hashlib

import numpy as np

# The cue names, in the order the score / type use them. A cue is a per-object value in [0, 1] after
# `rank_normalise`; its weight comes from the task params (the user's "feedback knobs").
CUES = ("contact", "density", "turnover", "naked", "partner")


def rank_normalise(values):
    """Per-image percentile of each value in [0, 1]; values <= 0 (or NaN) stay 0.

    Ranks are taken among the POSITIVE values only, so a cue that is zero for most objects (contact
    with a sparse macrophage mask) maps zeros to 0 rather than to the ~40th percentile a plain rank
    would give them — a zero cue must contribute nothing to the score. Ties share their mean rank.
    Rank-normalising makes weights compare like with like across cues and across movies whose
    intensities differ (Decision 3).
    """
    v = np.asarray(values, dtype=float)
    out = np.zeros_like(v)
    pos = np.isfinite(v) & (v > 0)
    n = int(pos.sum())
    if n == 0:
        return out
    from scipy.stats import rankdata   # lazy: keeps `import roi_utils` light
    out[pos] = rankdata(v[pos], method="average") / n
    return out


def _frames(t):
    t = np.asarray(t).astype(int)
    return {int(f): np.flatnonzero(t == f) for f in np.unique(t)}


def local_density(xyz, t, radius):
    """Neighbour counts per object: `(n_now, n_prev, n_next)`.

    `n_now` = other objects within `radius` (µm) in the same frame. `n_prev` / `n_next` = objects
    within `radius` OF THIS OBJECT'S POSITION in frame t-1 / t+1 (NaN when that frame has no objects
    at all or doesn't exist) — the identity-free way to see a neighbourhood fill up or empty out.
    """
    from scipy.spatial import cKDTree
    xyz = np.asarray(xyz, dtype=float).reshape(-1, 3)
    n = len(xyz)
    n_now = np.zeros(n)
    n_prev = np.full(n, np.nan)
    n_next = np.full(n, np.nan)
    frames = _frames(t)
    trees = {f: cKDTree(xyz[idx]) for f, idx in frames.items()}
    for f, idx in frames.items():
        pts = xyz[idx]
        n_now[idx] = np.array([len(r) - 1 for r in trees[f].query_ball_point(pts, radius)])
        for df, out in ((-1, n_prev), (1, n_next)):
            tree = trees.get(f + df)
            if tree is not None:
                out[idx] = np.array([len(r) for r in tree.query_ball_point(pts, radius)])
    return n_now, n_prev, n_next


def turnover(n_now, n_prev, n_next):
    """|n_now − mean of the available neighbours in t±1| — 0 where neither side exists."""
    side = np.vstack([n_prev, n_next])
    with np.errstate(invalid="ignore"):
        ref = np.nanmean(np.where(np.isfinite(side), side, np.nan), axis=0)
    out = np.abs(np.asarray(n_now, dtype=float) - ref)
    out[~np.isfinite(out)] = 0.0
    return out


def nearest_distance(xyz, t, target_xyz, target_t, exclude_ids=None, target_ids=None):
    """Distance (µm) from each object to the nearest target in the same frame; inf when none.

    `exclude_ids`/`target_ids` drop self-matches when a source is also a target (a big fragment is
    both a candidate and a partner).
    """
    from scipy.spatial import cKDTree
    xyz = np.asarray(xyz, dtype=float).reshape(-1, 3)
    target_xyz = np.asarray(target_xyz, dtype=float).reshape(-1, 3)
    out = np.full(len(xyz), np.inf)
    if len(target_xyz) == 0:
        return out
    tf = _frames(target_t)
    for f, idx in _frames(t).items():
        tidx = tf.get(f)
        if tidx is None:
            continue
        tree = cKDTree(target_xyz[tidx])
        k = min(2, len(tidx))
        d, j = tree.query(xyz[idx], k=k)
        d = d.reshape(len(idx), k)
        j = j.reshape(len(idx), k)
        if exclude_ids is not None and target_ids is not None:
            src = np.asarray(exclude_ids)[idx]
            hit = np.asarray(target_ids)[tidx][j]
            d = np.where(hit == src[:, None], np.inf, d)
        out[idx] = d.min(axis=1)
    return out


def score_objects(cues, weights):
    """Weighted mean of the rank-normalised cues → per-object score in [0, 1].

    `cues` maps cue name → raw per-object values (normalised here); `weights` maps cue name → ≥ 0.
    Returns `(score, normalised)` — the normalised cues are kept for the ROI's cue summary.
    """
    names = [c for c in CUES if c in cues]
    if not names:
        raise ValueError("no cues")
    n = len(next(iter(cues.values())))
    normed = {c: rank_normalise(cues[c]) for c in names}
    w = {c: max(0.0, float(weights.get(c, 0.0))) for c in names}
    total = sum(w.values())
    if total <= 0:
        raise ValueError("all cue weights are zero")
    score = np.zeros(n)
    for c in names:
        score += w[c] * normed[c]
    return score / total, normed


def candidate_mask(score, quantile):
    """Objects at or above the `quantile` of the positive scores (all-zero scores → none)."""
    score = np.asarray(score, dtype=float)
    pos = score > 0
    if not pos.any():
        return np.zeros(len(score), dtype=bool)
    thr = np.quantile(score[pos], float(quantile))
    return pos & (score >= thr)


def cluster_4d(xyz, t, eps, time_scale, min_samples):
    """DBSCAN over (x, y, z µm, t·time_scale µm): cluster id per object, -1 = noise.

    `time_scale` converts one frame into µm, so `eps` reaches `eps / time_scale` frames — the knob
    that decides how far apart in time two hits may be and still count as one event.
    """
    xyz = np.asarray(xyz, dtype=float).reshape(-1, 3)
    if len(xyz) == 0:
        return np.zeros(0, dtype=int)
    from sklearn.cluster import DBSCAN   # lazy: sklearn import is ~1 s
    pts = np.column_stack([xyz, np.asarray(t, dtype=float) * float(time_scale)])
    return DBSCAN(eps=float(eps), min_samples=int(min_samples)).fit_predict(pts)


def split_windows(cluster_ids, t, window, min_samples):
    """Cut each cluster into consecutive time windows of at most `window` frames.

    A sessile macrophage keeps scoring for the whole movie, so DBSCAN chains its hits into one
    cluster spanning every frame — true, but not reviewable. A window keeps an ROI to what a person
    (or one montage) can read. Pieces with fewer than `min_samples` objects become noise (-1).
    `window <= 0` leaves the clusters as they are.
    """
    cluster_ids = np.asarray(cluster_ids).astype(int)
    if window <= 0 or len(cluster_ids) == 0:
        return cluster_ids
    t = np.asarray(t).astype(int)
    out = np.full(len(cluster_ids), -1)
    nxt = 0
    for cid in sorted(set(int(c) for c in cluster_ids if c >= 0)):
        idx = np.flatnonzero(cluster_ids == cid)
        start = t[idx].min()
        piece = (t[idx] - start) // int(window)
        for p in np.unique(piece):
            sub = idx[piece == p]
            if len(sub) >= int(min_samples):
                out[sub] = nxt
                nxt += 1
    return out


def proximity(dist, scale, reach=3.0):
    """exp(-dist/scale) within `reach`·scale, else 0 — a distance cue that is ZERO for far objects.

    Without the cutoff almost every object gets a tiny positive value, and `rank_normalise` (which
    ranks positives) would turn a 30 µm distance into a mid-ranked cue.
    """
    d = np.asarray(dist, dtype=float)
    out = np.where(np.isfinite(d) & (d <= reach * scale), np.exp(-d / scale), 0.0)
    return out


def roi_id(t0, t1, box):
    """Stable id from the time window + box (Decision 5): an identical re-run yields the same ids, so
    reviews keyed by id survive it."""
    key = f"{int(t0)}:{int(t1)}:" + ":".join(str(int(box[k])) for k in ("x0", "x1", "y0", "y1", "z0", "z1"))
    return "r" + hashlib.sha1(key.encode("utf-8")).hexdigest()[:8]


def dominant_cue(cue_means, weights):
    """The cue contributing most to an ROI's score (Decision 4: the ROI type is a cue, not biology)."""
    best, best_v = None, -1.0
    for c in CUES:
        if c not in cue_means:
            continue
        v = float(cue_means[c]) * max(0.0, float(weights.get(c, 0.0)))
        if v > best_v:
            best, best_v = c, v
    return best


def build_rois(cluster_ids, px, t, score, normed, weights, shape, pad_px, objects=None, max_rois=50):
    """Group clustered objects into ROI dicts, ranked by summed score.

    px: (n, 3) pixel centroids in (x, y, z) order. shape: dict with t/z/y/x extents (the STORE's
    shape — never ccid meta, which can disagree; plan §1a). pad_px: (x, y, z) padding in pixels.
    objects: optional list of per-object dicts (same order) embedded in each ROI's object table.
    """
    px = np.asarray(px, dtype=float).reshape(-1, 3)
    t = np.asarray(t).astype(int)
    rois = []
    for cid in sorted(set(int(c) for c in cluster_ids if c >= 0)):
        idx = np.flatnonzero(np.asarray(cluster_ids) == cid)
        lo = px[idx].min(axis=0)
        hi = px[idx].max(axis=0)
        box = {}
        for k, (axis, lim) in enumerate((("x", shape["x"]), ("y", shape["y"]), ("z", shape["z"]))):
            box[f"{axis}0"] = int(max(0, np.floor(lo[k] - pad_px[k])))
            box[f"{axis}1"] = int(min(lim - 1, np.ceil(hi[k] + pad_px[k])))
        t0 = int(max(0, t[idx].min() - 1))
        t1 = int(min(shape["t"] - 1, t[idx].max() + 1))
        cue_means = {c: float(np.mean(normed[c][idx])) for c in normed}
        order = idx[np.argsort(-np.asarray(score)[idx])]
        rois.append({
            "id": roi_id(t0, t1, box),
            "score": float(np.sum(np.asarray(score)[idx])),
            "type": dominant_cue(cue_means, weights),
            "t0": t0, "t1": t1, "box": box,
            "centroid": {"t": float(t[idx].mean()), "z": float(px[idx, 2].mean()),
                         "y": float(px[idx, 1].mean()), "x": float(px[idx, 0].mean())},
            "nObjects": int(len(idx)),
            "cues": cue_means,
            "objects": [objects[i] for i in order] if objects is not None else [],
        })
    rois.sort(key=lambda r: -r["score"])
    rois = rois[:int(max_rois)]
    for i, r in enumerate(rois):
        r["rank"] = i + 1
    assign_sites(rois)
    return rois


def box_iou(a, b):
    """Volume IoU of two inclusive pixel boxes."""
    inter, va, vb = 1.0, 1.0, 1.0
    for ax in ("x", "y", "z"):
        lo, hi = max(a[f"{ax}0"], b[f"{ax}0"]), min(a[f"{ax}1"], b[f"{ax}1"])
        inter *= max(0, hi - lo + 1)
        va *= a[f"{ax}1"] - a[f"{ax}0"] + 1
        vb *= b[f"{ax}1"] - b[f"{ax}0"] + 1
    u = va + vb - inter
    return inter / u if u > 0 else 0.0


def assign_sites(rois, min_iou=0.3):
    """Group ROIs at the same PLACE into sites: `site` = rank of the site's best ROI, in place.

    A sessile macrophage scores in every time window, so one site fills the top of the queue with
    windows of the same event. Overlapping boxes (IoU ≥ `min_iou`, any time) are one site —
    transitively — so a reviewer can take one per site.
    """
    parent = list(range(len(rois)))

    def find(i):
        while parent[i] != i:
            parent[i] = parent[parent[i]]
            i = parent[i]
        return i

    for i in range(len(rois)):
        for j in range(i + 1, len(rois)):
            if box_iou(rois[i]["box"], rois[j]["box"]) >= min_iou:
                parent[find(j)] = find(i)
    best = {}
    for i, r in enumerate(rois):       # rois are rank-ordered, so the first seen is the best
        best.setdefault(find(i), r["rank"])
    for i, r in enumerate(rois):
        r["site"] = best[find(i)]
    return rois


# ── Macrophage mask (Decision 1: a per-frame mask, never macrophage objects) ────────────────────


def channel_mask(stack_zyx, threshold, sigma_px=(0.5, 1.5, 1.5), min_voxels=500):
    """Threshold a smoothed (z, y, x) channel into a filled binary mask.

    - `threshold <= 0` → Otsu of the smoothed stack (plan §1b: a knob, because the bright and the dim
      TBMs of one movie need different values).
    - Holes are filled PER z-PLANE: an engulfed fragment displaces the CD169 signal, so without the
      fill every engulfed fragment would read as outside its macrophage.
    - Components below `min_voxels` are dropped (B-cell bleed-through specks).
    Returns `(mask, threshold_used)`.
    """
    from scipy import ndimage as ndi
    sm = ndi.gaussian_filter(np.asarray(stack_zyx, dtype=np.float32), sigma_px)
    thr = float(threshold)
    if thr <= 0:
        # Otsu over the non-zero voxels: a sparse channel is mostly exactly 0, and Otsu over every
        # voxel lands at ~0 (`background_threshold`'s `ignore_zero` exists for exactly this)
        from cecelia.utils.intensity_utils import background_threshold
        hist = np.bincount(np.clip(np.rint(sm), 0, None).astype(np.int64).ravel())
        thr = background_threshold(hist, "otsu", ignore_zero=True) if hist[1:].any() else np.inf
    mask = sm > thr
    for z in range(mask.shape[0]):
        mask[z] = ndi.binary_fill_holes(mask[z])
    if min_voxels > 0 and mask.any():
        lab, n = ndi.label(mask)
        sizes = np.bincount(lab.ravel())
        keep = sizes >= int(min_voxels)
        keep[0] = False
        mask = keep[lab]
    return mask, thr


def mask_cues(labels_zyx, mask, label_ids, centroids_zyx, sampling):
    """Per-object `(frac_inside, dist_um)` against a binary mask.

    frac_inside = fraction of the object's voxels in the mask (0 for a label absent from the frame).
    dist_um = 3D Euclidean distance (anisotropic `sampling`, z/y/x µm per voxel) from the object's
    centroid voxel to the nearest mask voxel — 0 inside, inf when the frame has no mask at all.
    """
    from scipy import ndimage as ndi
    labels_zyx = np.asarray(labels_zyx)
    label_ids = np.asarray(label_ids, dtype=np.int64)
    n = len(label_ids)
    frac = np.zeros(n)
    dist = np.full(n, np.inf)
    if n == 0:
        return frac, dist
    flat = labels_zyx.ravel().astype(np.int64)
    top = int(max(flat.max(initial=0), label_ids.max(initial=0))) + 1
    tot = np.bincount(flat, minlength=top)
    ins = np.bincount(flat[mask.ravel()], minlength=top)
    with np.errstate(invalid="ignore", divide="ignore"):
        f = np.where(tot[label_ids] > 0, ins[label_ids] / np.maximum(tot[label_ids], 1), 0.0)
    frac[:] = f
    if mask.any():
        edt = ndi.distance_transform_edt(~mask, sampling=sampling)
        c = np.rint(np.asarray(centroids_zyx, dtype=float)).astype(int)
        for k, lim in enumerate(mask.shape):
            c[:, k] = np.clip(c[:, k], 0, lim - 1)
        dist[:] = edt[c[:, 0], c[:, 1], c[:, 2]]
    return frac, dist
