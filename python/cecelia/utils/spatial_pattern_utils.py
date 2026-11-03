"""
One population's spatial pattern among a neighbour graph's nodes — `spatialAnalysis.spatialPattern`
(docs/todo/SPATIAL_PATTERN_PLAN.md).

The REFERENCE is every node of a persisted neighbour graph (`spatial_utils.load_graph`), i.e. the cells
the graph was built over (Decision 1). The TARGET is a boolean mask over those nodes. The null is RANDOM
LABELLING: the target's membership is shuffled across all nodes, within a frame for a per-timepoint
graph (Decision 2; Baddeley, Rubak & Turner, *Spatial Point Patterns: Methodology and Applications with
R*, CRC 2015, DOI 10.1201/b19708, ch. 14). Target and reference share positions, so the test needs no
window and no edge correction. Only coordinates are used; the graph's edges are not.

Readouts (Decision 3):
- nearest-neighbour ratio: observed mean target→target NN distance / its mean under the shuffle
  (1 = random relative to the reference, <1 clustered, >1 dispersed), with a two-sided empirical p;
- pair-ratio curve g(r): target–target pairs within r / the random-labelling expectation
  pairs_ref(r)·nT(nT−1)/(nR(nR−1)) — i.e. K_TT(r)/K_RR(r) — with a 2.5–97.5 % shuffle envelope;
- Clark–Evans R against CSR in the reference's convex hull: descriptive, no edge correction.
"""
import math

import numpy as np

from cecelia.utils.spatial_utils import permutation_groups


def _frame_stats(frames, coords, mask, radii):
    """Pooled NN-distance sum + count and per-radius target–target pair counts for one labelling."""
    from scipy.spatial import cKDTree

    nn_sum, nn_n = 0.0, 0
    pairs = np.zeros(len(radii)) if radii is not None else None
    for idx in frames:
        pts = coords[idx[mask[idx]]]
        k = pts.shape[0]
        if k < 2:
            continue
        tree = cKDTree(pts)
        d, _ = tree.query(pts, k=2)
        nn_sum += float(d[:, 1].sum())
        nn_n += k
        if pairs is not None:
            # count_neighbors counts ORDERED pairs with d ≤ r, self-pairs included → distinct unordered
            pairs += (tree.count_neighbors(tree, radii) - k) / 2.0
    return nn_sum, nn_n, pairs


def _expected_pairs(frames, coords, mask, radii):
    """Random-labelling expectation of target–target pairs within each r: per frame, every reference
    pair is a target pair with probability k(k−1)/(n(n−1)) (hypergeometric), so the expectation is the
    reference pair count scaled by that — exact, not simulated."""
    from scipy.spatial import cKDTree

    out = np.zeros(len(radii))
    for idx in frames:
        n = idx.size; k = int(mask[idx].sum())
        if n < 2 or k < 2:
            continue
        tree = cKDTree(coords[idx])
        ref_pairs = (tree.count_neighbors(tree, radii) - n) / 2.0
        out += ref_pairs * (k * (k - 1)) / (n * (n - 1))
    return out


def _unit_ball(d):
    return math.pi ** (d / 2.0) / math.gamma(d / 2.0 + 1.0)


# a convex hull of a handful of points has almost no area, so λ = k/area explodes and R reads 3–5
# for an ordinary pattern; frames with fewer reference cells than this are not scored
CE_MIN_REFERENCE = 20


def clark_evans_r(coords, mask, frames, min_reference=CE_MIN_REFERENCE):
    """Clark–Evans aggregation index R (Clark & Evans, *Ecology* 35:445–453, 1954, DOI 10.2307/1931034)
    against CSR in the convex hull of each frame's REFERENCE nodes. The CSR mean NN distance in d
    dimensions is Γ(1+1/d)/(λ·V_d)^{1/d} (V_d = unit-ball volume; 0.5/√λ in 2D, the 3D form is
    Chandrasekhar, *Rev Mod Phys* 15:1–89, 1943, DOI 10.1103/RevModPhys.15.1). An axis with no spread is
    dropped first, so a single plane stored as 3D is scored in 2D. Pooled over frames as
    Σ observed / Σ expected over target cells, over frames with ≥ `min_reference` reference cells.
    No edge correction — descriptive only (Decision 3).
    Returns (R, mean hull size in µm^d, d) or (nan, nan, 0) when no frame can be scored."""
    from scipy.spatial import ConvexHull, cKDTree

    obs_sum = exp_sum = 0.0
    sizes, dims_used = [], 0
    for idx in frames:
        allpts = coords[idx]
        dims = np.where(np.ptp(allpts, axis=0) > 1e-9)[0] if allpts.shape[0] else np.array([], int)
        d = int(dims.size)
        tpts = coords[idx[mask[idx]]][:, dims]
        k = tpts.shape[0]
        if d == 0 or k < 2 or allpts.shape[0] < max(d + 1, int(min_reference)):
            continue
        try:
            size = float(np.ptp(allpts[:, dims[0]])) if d == 1 else float(ConvexHull(allpts[:, dims]).volume)
        except Exception:                       # degenerate hull (collinear / coplanar reference)
            continue
        if size <= 0:
            continue
        lam = k / size
        expected = math.gamma(1.0 + 1.0 / d) / (lam * _unit_ball(d)) ** (1.0 / d)
        dist, _ = cKDTree(tpts).query(tpts, k=2)
        obs_sum += float(dist[:, 1].sum())
        exp_sum += k * expected
        sizes.append(size); dims_used = d
    if exp_sum == 0:
        return float("nan"), float("nan"), 0
    return obs_sum / exp_sum, float(np.mean(sizes)), dims_used


def pattern_test(coords, mask, times=None, radii=None, n_permutations=999, random_state=0,
                 on_permutation=None):
    """Score one target population's spacing among all reference nodes (module docstring).

    `coords` (n, d) µm; `mask` (n,) bool target membership; `times` optional per-node frame index;
    `radii` the r grid for the pair-ratio curve (None skips it). `n_permutations=0` skips the shuffle:
    the NN ratio, z, p and the envelope are then NaN. `on_permutation(i)` is called after each shuffle
(the slow loop) so a runner can report progress. Returns a flat dict (floats may be NaN)."""
    coords = np.asarray(coords, dtype=np.float64)
    mask = np.asarray(mask, dtype=bool)
    n = mask.size
    frames = permutation_groups(np.arange(n), times)
    radii = None if radii is None else np.asarray(radii, dtype=np.float64)

    nn_sum, nn_n, obs_pairs = _frame_stats(frames, coords, mask, radii)
    nn_obs = nn_sum / nn_n if nn_n else float("nan")
    nan = float("nan")
    out = {"nTarget": int(mask.sum()), "nReference": int(n), "nFrames": len(frames),
           "targetFraction": float(mask.sum() / n) if n else 0.0,
           "nnObserved": nn_obs, "nnExpected": nan, "nnRatio": nan, "zScore": nan, "pValue": nan}

    n_perm = int(n_permutations)
    null_nn = null_pairs = None
    if n_perm > 0 and nn_n:
        rng = np.random.default_rng(int(random_state))
        null_nn = np.empty(n_perm)
        null_pairs = np.empty((n_perm, len(radii))) if radii is not None else None
        shuffled = mask.copy()
        for i in range(n_perm):
            for g in frames:
                shuffled[g] = rng.permutation(mask[g])
            s, c, p = _frame_stats(frames, coords, shuffled, radii)
            null_nn[i] = s / c if c else nan
            if null_pairs is not None:
                null_pairs[i] = p
            if on_permutation is not None:
                on_permutation(i + 1)
        mu, sd = float(np.nanmean(null_nn)), float(np.nanstd(null_nn))
        out["nnExpected"] = mu
        out["nnRatio"] = nn_obs / mu if mu > 0 else nan
        out["zScore"] = (nn_obs - mu) / sd if sd > 0 else nan
        # two-sided empirical p with the +1 correction (Davison & Hinkley 1997) — same as the contact test
        extreme = int((np.abs(null_nn - mu) >= abs(nn_obs - mu)).sum())
        out["pValue"] = (extreme + 1.0) / (n_perm + 1.0)

    if radii is not None:
        exp_pairs = _expected_pairs(frames, coords, mask, radii)
        with np.errstate(divide="ignore", invalid="ignore"):
            ratio = np.where(exp_pairs > 0, obs_pairs / exp_pairs, np.nan)
            if null_pairs is not None:
                null_ratio = np.where(exp_pairs > 0, null_pairs / exp_pairs, np.nan)
                lo, hi = np.nanpercentile(null_ratio, [2.5, 97.5], axis=0)
            else:
                lo = hi = np.full(len(radii), np.nan)
        out.update({"r": radii.tolist(), "pairRatio": ratio.tolist(),
                    "envLo": np.asarray(lo).tolist(), "envHi": np.asarray(hi).tolist()})

    ce, size, d = clark_evans_r(coords, mask, frames)
    out.update({"ceR": ce, "hullSize": size, "nDims": d})
    return out
