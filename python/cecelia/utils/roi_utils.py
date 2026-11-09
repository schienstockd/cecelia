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
               expected=None, score_by="sum"):
    """One ROI dict per group, ranked by score = summed weight − `expected[group]` (0 when absent), or
    with `score_by="mean"` its mean weight (given groups ranked by a measure — `find_rois`).

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
        score = float(weight[idx].mean()) if score_by == "mean" else float(weight[idx].sum()) - exp
        if score <= 0:
            continue
        box = group_box(px[idx], shape, pad_frac)
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


def group_box(px, shape, pad_frac=0.2):
    """The inclusive pixel box of a group's (n, 3) x/y/z centroids, padded by `pad_frac` of its own
    extent (at least 2 px in x/y, 1 in z) and kept inside `shape`."""
    lo, hi = px.min(axis=0), px.max(axis=0)
    box = {}
    for k, (axis, lim, floor_px) in enumerate((("x", shape["x"], 2), ("y", shape["y"], 2), ("z", shape["z"], 1))):
        pad = max(floor_px, pad_frac * (hi[k] - lo[k]))
        box[f"{axis}0"] = int(max(0, np.floor(lo[k] - pad)))
        box[f"{axis}1"] = int(min(lim - 1, np.ceil(hi[k] + pad)))
    return box


# Two groups are the same SPOT when their x/y boxes overlap by at least this share of the smaller one
# (and their planes touch) — `merge_groups`. Of the smaller, so a clump inside a bigger one still merges.
MERGE_OVERLAP = 0.5


def same_spot(a, b, min_overlap=MERGE_OVERLAP):
    """Do two pixel boxes cover the same spot: x/y overlap ≥ `min_overlap` of the smaller box, z touching."""
    if min(a["z1"], b["z1"]) < max(a["z0"], b["z0"]):
        return False
    ix = min(a["x1"], b["x1"]) - max(a["x0"], b["x0"]) + 1
    iy = min(a["y1"], b["y1"]) - max(a["y0"], b["y0"]) + 1
    if ix <= 0 or iy <= 0:
        return False
    small = min((a["x1"] - a["x0"] + 1) * (a["y1"] - a["y0"] + 1), (b["x1"] - b["x0"] + 1) * (b["y1"] - b["y0"] + 1))
    return ix * iy >= min_overlap * small


def merge_groups(groups, px, t, shape, max_gap, min_overlap=MERGE_OVERLAP):
    """Union groups at the same spot (`same_spot`) whose frames overlap or have at most `max_gap` frames
    between them (0 = back to back) — transitively, so a chain of short events at one place becomes one ROI. Returns the new group
    id per object (-1 stays -1) and `{old id: new id}`.

    This runs BEFORE the ROIs are built, ranked and capped: one event split into several (a linking
    break, consecutive windows) is then one ROI scored on all its objects, not several near-copies
    filling the list."""
    groups = np.asarray(groups)
    px = np.asarray(px, dtype=float).reshape(-1, 3)
    t = np.asarray(t).astype(int)
    ids = sorted(set(int(g) for g in groups if g >= 0))
    info = []
    for g in ids:
        idx = np.flatnonzero(groups == g)
        info.append((group_box(px[idx], shape), int(t[idx].min()), int(t[idx].max())))
    def joined(i, j):
        (bi, ti0, ti1), (bj, tj0, tj1) = info[i], info[j]
        between = max(ti0, tj0) - min(ti1, tj1) - 1        # frames strictly between; < 0 = overlapping
        return between <= max_gap and same_spot(bi, bj, min_overlap)

    root = union_components(len(ids), joined)
    mapping = {g: ids[root[i]] for i, g in enumerate(ids)}
    out = np.array([mapping.get(int(g), -1) if g >= 0 else -1 for g in groups], dtype=int)
    return out, mapping


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


def union_components(n, joined):
    """Component root per item 0..n-1, joining every pair (i, j), i < j, for which `joined(i, j)` is
    true — transitively. The one union-find behind sites and same-spot merging."""
    parent = list(range(n))

    def find(i):
        while parent[i] != i:
            parent[i] = parent[parent[i]]
            i = parent[i]
        return i

    for i in range(n):
        for j in range(i + 1, n):
            if joined(i, j):
                parent[find(j)] = find(i)
    return [find(i) for i in range(n)]


def assign_sites(rois, min_iou=0.3):
    """Group ROIs at the same PLACE into sites: `site` = rank of the site's best ROI, in place.

    A sessile hotspot scores in every time window, so one site fills the top of the queue with
    windows of the same event. Overlapping boxes (IoU ≥ `min_iou`, any time) are one site —
    transitively — so a reviewer can take one per site.
    """
    root = union_components(len(rois), lambda i, j: box_iou(rois[i]["box"], rois[j]["box"]) >= min_iou)
    best = {}
    for i, r in enumerate(rois):       # rois are rank-ordered, so the first seen is the best
        best.setdefault(root[i], r["rank"])
    for i, r in enumerate(rois):
        r["site"] = best[root[i]]
    return rois


def given_groups(ids, min_objects):
    """Group id per object from an id the population already carries (`Group by`, e.g. an aggregate
    event or a track): objects sharing an id are one group, whatever frames it spans. A missing or
    non-positive id is in no group (-1), and a group of fewer than `min_objects` objects is dropped."""
    ids = np.asarray(ids, dtype=float)
    out = np.full(len(ids), -1, dtype=int)
    ok = np.isfinite(ids) & (ids > 0)
    if not ok.any():
        return out
    vals, inv, counts = np.unique(ids[ok], return_inverse=True, return_counts=True)
    keep = counts >= max(1, int(min_objects))
    out[np.flatnonzero(ok)] = np.where(keep[inv], inv, -1)
    return out


def find_rois(xyz_um, px, t, shape, min_objects, voxel_um=(1.0, 1.0, 1.0), weight=None, objects=None,
              max_rois=50, window=None, group_ids=None, merge_gap=None):
    """The whole pipeline: groups per pooled window → ROIs ranked by excess over the background →
    sites. `voxel_um` is (x, y, z) µm per pixel, the floor of a box's thickness. Returns
    `(rois, group_ids)`.

    With `group_ids` (one id per object — `Group by`) the groups are GIVEN, not found: each id is one
    ROI over the frames it spans, ranked by its summed weight. No window and no background, because a
    given group is not a pile-up to be told from a passing crowd — it is already one event.

    With `merge_gap` (frames; None = off) groups at the same spot at most that far apart in time are
    merged first (`merge_groups`) — a windowed group's background is the sum of its parts'."""
    n = len(np.asarray(t))
    weight = np.ones(n) if weight is None else np.asarray(weight, dtype=float)
    if group_ids is not None:
        groups = given_groups(group_ids, min_objects)
        if merge_gap is not None:
            groups, _ = merge_groups(groups, px, t, shape, merge_gap)
        # Ranked by a measure, a given group scores its MEAN weight: summed, a bigger event outranks a
        # smaller one whatever its measure (percentile weights are never 0), so "rank by preceded" would
        # rank by size. With no measure every weight is 1 and the sum is the object count, as before.
        by = "sum" if np.allclose(weight, 1.0) else "mean"
        return build_rois(groups, px, t, weight, shape, objects=objects, max_rois=max_rois,
                          score_by=by), groups
    groups = window_groups(xyz_um, t, min_objects, window)
    expected = background_expected(groups, xyz_um, t, weight, voxel_um, window)
    if merge_gap is not None:
        groups, mapping = merge_groups(groups, px, t, shape, merge_gap)
        merged = {}
        for old, new in mapping.items():
            merged[new] = merged.get(new, 0.0) + float(expected.get(old, 0.0))
        expected = merged
    return build_rois(groups, px, t, weight, shape, objects=objects, max_rois=max_rois,
                      expected=expected), groups
