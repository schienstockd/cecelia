"""ROI discovery — the pure core (docs/todo/ROI_TRIAGE_PLAN.md).

Where do the objects of ONE population gather? The question lives in the population (segmentation +
gating, filter populations on any measure); this module only finds the places:

1. the movie is cut into fixed time windows (`WINDOW_FRAMES`); each window's objects are POOLED,
2. groups in the pooled positions (HDBSCAN — no radius to set); a place objects keep returning to
   piles up over the window while a passing crowd smears out, so persistence is found without
   tracking or linking frames,
3. each group is one ROI, ranked by its EXCESS: the summed object weight (1 each, or the percentile
   of the measures picked to rank by) minus what the window's average density would put in a box
   that size. A big loose group at background density scores ~0; a compact one scores high —
   ranking by plain count favoured the first, and no radius is needed to tell them apart,
4. ROIs at the same place in different windows are grouped into sites.

Nothing here reads a store or a label table: the runner
(`app/src/tasks/spatialAnalysis/roi_discovery_run.py`) does the I/O and hands arrays in, so every
step is unit-testable on synthetic points (`tests/test_roi_utils.py`). Positions are µm for grouping
and pixels for boxes; time is in frames. No tracking — identity is never used.
"""

import hashlib

import numpy as np

# Frames pooled per window, so the longest an ROI lasts. Long enough for a place objects keep
# returning to to pile up over a passing crowd; short enough that one ROI is reviewable at a glance.
WINDOW_FRAMES = 12

# HDBSCAN membership strength below which an object is not part of a group's core (`window_groups`).
CORE_MEMBERSHIP = 0.5


def percentile_rank(values):
    """Each value's percentile among the finite values, in (0, 1]; NaN / inf → 0. Ties share their
    mean rank. Makes measures in different units comparable, so several can be averaged."""
    v = np.asarray(values, dtype=float)
    out = np.zeros_like(v)
    ok = np.isfinite(v)
    n = int(ok.sum())
    if n == 0:
        return out
    from scipy.stats import rankdata   # lazy: keeps `import roi_utils` light
    out[ok] = rankdata(v[ok], method="average") / n
    return out


def object_weights(measures, n):
    """Per-object weight in [0, 1]: the mean percentile of the picked measures, or 1 when none."""
    if not measures:
        return np.ones(n)
    return np.mean([percentile_rank(m) for m in measures], axis=0)


def window_groups(xyz, t, min_objects, window=None):
    """Group id per object (-1 = in no group): HDBSCAN on each time window's POOLED positions.

    HDBSCAN finds groups at the density they actually occur, so the only setting is the smallest
    group (`min_objects`, a count over the window) — no radius, which would have to match the cell
    size. `leaf` selection takes the finest dense groups: the default (`eom`) keeps the most
    persistent cluster, which in tissue is often the whole region the cells live in (a follicle
    against empty space). Only each group's core is kept (membership ≥ `CORE_MEMBERSHIP`): HDBSCAN
    hands every point to some group, so a group would also collect the sparse objects around it.
    Ids are unique across windows. A window holding one group and nothing else finds none — HDBSCAN
    needs a background to stand out from.

    HDBSCAN: Campello, Moulavi & Sander, "Density-Based Clustering Based on Hierarchical Density
    Estimates", PAKDD 2013, doi:10.1007/978-3-642-37456-2_14 (scikit-learn's implementation).
    """
    window = WINDOW_FRAMES if window is None else int(window)
    xyz = np.asarray(xyz, dtype=float).reshape(-1, 3)
    t = np.asarray(t).astype(int)
    out = np.full(len(xyz), -1)
    if len(xyz) == 0:
        return out
    from sklearn.cluster import HDBSCAN   # lazy: sklearn import is ~1 s
    m = max(2, int(min_objects))
    win = t // window if window > 0 else np.zeros(len(t), dtype=int)
    nxt = 0
    for w in np.unique(win):
        idx = np.flatnonzero(win == w)
        if len(idx) < m:
            continue
        h = HDBSCAN(min_cluster_size=m, cluster_selection_method="leaf", copy=True).fit(xyz[idx])
        lab = np.where(h.probabilities_ >= CORE_MEMBERSHIP, h.labels_, -1)
        for g in np.unique(lab[lab >= 0]):
            sel = lab == g
            if sel.sum() >= m:
                out[idx[sel]] = nxt
                nxt += 1
    return out


def roi_id(t0, t1, box):
    """Stable id from the time window + box (Decision 5): an identical re-run yields the same ids, so
    reviews keyed by id survive it."""
    key = f"{int(t0)}:{int(t1)}:" + ":".join(str(int(box[k])) for k in ("x0", "x1", "y0", "y1", "z0", "z1"))
    return "r" + hashlib.sha1(key.encode("utf-8")).hexdigest()[:8]


def _volume(p, voxel_um):
    """Bounding-box volume (µm³) of points `p`, each axis at least one voxel thick."""
    return float(np.prod(np.maximum(np.ptp(p, axis=0), voxel_um))) if len(p) else 0.0


def background_expected(group_ids, xyz, t, weight, voxel_um, window=None):
    """`{group: expected weight}` — what the group's window would hold in a box the group's size at
    that window's AVERAGE density (summed weight over the window's occupied bounding box)."""
    window = WINDOW_FRAMES if window is None else int(window)
    group_ids = np.asarray(group_ids).astype(int)
    xyz = np.asarray(xyz, dtype=float).reshape(-1, 3)
    t = np.asarray(t).astype(int)
    weight = np.asarray(weight, dtype=float)
    voxel_um = np.asarray(voxel_um, dtype=float)
    win = t // window if window > 0 else np.zeros(len(t), dtype=int)
    rho = {}
    for w in np.unique(win):
        sel = win == w
        v = _volume(xyz[sel], voxel_um)
        rho[int(w)] = weight[sel].sum() / v if v > 0 else 0.0
    out = {}
    for g in sorted(set(int(x) for x in group_ids if x >= 0)):
        sel = group_ids == g
        out[g] = rho[int(win[sel][0])] * _volume(xyz[sel], voxel_um)
    return out


def build_rois(cluster_ids, px, t, weight, shape, objects=None, max_rois=50, pad_frac=0.2,
               expected=None):
    """One ROI dict per group, ranked by score = summed weight − `expected[group]` (0 when absent).

    Groups scoring ≤ 0 (no denser than the background) are dropped. px: (n, 3) pixel centroids in
    (x, y, z) order. shape: dict with t/z/y/x extents (the STORE's shape — never ccid meta, which can
    disagree; plan §1a). Each box is padded by `pad_frac` of its own extent (at least 2 px in x/y, 1
    in z) — relative, so it suits any cell size. objects: optional list of per-object dicts (same
    order) embedded in each ROI's object table.
    """
    px = np.asarray(px, dtype=float).reshape(-1, 3)
    t = np.asarray(t).astype(int)
    weight = np.asarray(weight, dtype=float)
    expected = expected or {}
    rois = []
    for cid in sorted(set(int(c) for c in cluster_ids if c >= 0)):
        idx = np.flatnonzero(np.asarray(cluster_ids) == cid)
        exp = float(expected.get(cid, 0.0))
        score = float(weight[idx].sum()) - exp
        if score <= 0:
            continue
        lo, hi = px[idx].min(axis=0), px[idx].max(axis=0)
        box = {}
        for k, (axis, lim, floor_px) in enumerate((("x", shape["x"], 2), ("y", shape["y"], 2), ("z", shape["z"], 1))):
            pad = max(floor_px, pad_frac * (hi[k] - lo[k]))
            box[f"{axis}0"] = int(max(0, np.floor(lo[k] - pad)))
            box[f"{axis}1"] = int(min(lim - 1, np.ceil(hi[k] + pad)))
        t0 = int(max(0, t[idx].min() - 1))
        t1 = int(min(shape["t"] - 1, t[idx].max() + 1))
        order = idx[np.argsort(-weight[idx])]
        rois.append({
            "id": roi_id(t0, t1, box),
            "score": score,
            "expected": exp,
            "t0": t0, "t1": t1, "box": box,
            "centroid": {"t": float(t[idx].mean()), "z": float(px[idx, 2].mean()),
                         "y": float(px[idx, 1].mean()), "x": float(px[idx, 0].mean())},
            "nObjects": int(len(idx)),
            "nFrames": int(len(np.unique(t[idx]))),
            "meanWeight": float(weight[idx].mean()),
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

    A sessile hotspot scores in every time window, so one site fills the top of the queue with
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


def find_rois(xyz_um, px, t, shape, min_objects, voxel_um=(1.0, 1.0, 1.0), weight=None, objects=None,
              max_rois=50, window=None):
    """The whole pipeline: groups per pooled window → ROIs ranked by excess over the background →
    sites. `voxel_um` is (x, y, z) µm per pixel, the floor of a box's thickness. Returns
    `(rois, group_ids)`."""
    n = len(np.asarray(t))
    weight = np.ones(n) if weight is None else np.asarray(weight, dtype=float)
    groups = window_groups(xyz_um, t, min_objects, window)
    expected = background_expected(groups, xyz_um, t, weight, voxel_um, window)
    return build_rois(groups, px, t, weight, shape, objects=objects, max_rois=max_rois,
                      expected=expected), groups
