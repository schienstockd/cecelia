"""Local structure anisotropy: the fibre-orientation field behind the quiver plot.

Two estimators live here. They answer the same question — *which way does the fibrous structure
run in this neighbourhood?* — from different inputs, and **their conventions are opposite**, which
is the single most important thing to know before touching this file:

| estimator                     | input                 | fibre direction is the … |
|-------------------------------|-----------------------|--------------------------|
| `structure_tensor_field`      | an image (intensity / mask / skeleton) | **minor** eigenvector |
| `tangent_tensor_field`        | a skeleton (topology) | **major** eigenvector |

The structure tensor measures *intensity gradients*, which are largest ACROSS a fibre, so its
dominant eigenvector points perpendicular to the structure. The tangent tensor accumulates skeleton
edge directions, so its dominant eigenvector points along it. Verified on synthetic fibre fields at
0/30/60/90° — each estimator recovers the true angle to within ~1° via its own convention, and to
within ~89° via the other one's.

**Never index the eigenvectors by hand — call `fibre_orientation` (or `tangent_orientation`).**
A 90° error here is silent: the arrows still look like a plausible vector field, they just point
the wrong way. See `docs/todo/SPATIAL_ANISOTROPY_PLAN.md` finding A1 for how this got
mis-documented once already.

`structure_tensor_field` is the production estimator (`segment.branching` → `calcAnisotropy`).
`tangent_tensor_field` is the **reference implementation** of what the old R version computed
(ILEE_CSK's `analyze_anisotropy_2d`), kept so any claim about "legacy vs now" is a number anyone
can re-run rather than an assertion — the tests validate the production path against it, and
`compare_fields` is the one-call form of that comparison.

Algorithmic ancestry: Li et al., *Plant Cell* 35:371 (2023), doi:10.1093/plcell/koac290
(ILEE_CSK, <https://github.com/phylars/ILEE_CSK>). The tangent tensor below is a direct
reimplementation of `functions.py:757-840` of that package; the structure tensor is
`skimage.feature.structure_tensor` plus box aggregation.
"""

import numpy as np
import skimage.feature


# ── box grid ──────────────────────────────────────────────────────────────────

def pool_by_box(x: np.ndarray, box: int) -> np.ndarray:
    """Mean-pool a (H, W) or (D, H, W) array into a box grid.

    Trailing pixels that don't fill a whole box are trimmed, so the output grid is
    `tuple(s // box for s in x.shape)` — the same trim `box_centres` assumes. Vectorised
    (reshape + mean); no loops.
    """
    trimmed = tuple(int((s // box) * box) for s in x.shape)
    y = x[tuple(slice(0, t) for t in trimmed)]
    new_shape = []
    for s in y.shape:
        new_shape.extend([s // box, box])
    y = y.reshape(new_shape)
    return y.mean(axis=tuple(range(1, y.ndim, 2)))


def box_centres(shape: tuple, box: int) -> np.ndarray:
    """Grid of box-centre coordinates in image PIXEL units, matching `pool_by_box`'s trim.

    shape=(H, W) → (H_boxes, W_boxes, 2) ordered (y, x); 3D → (…, 3) ordered (z, y, x).
    Pixels, not µm — the caller records `scale_um_per_px` in `aniso_meta` alongside.
    """
    axes = [np.arange(s // box) * box + box / 2 for s in shape]
    return np.stack(np.meshgrid(*axes, indexing="ij"), axis=-1).astype(np.float32)


# ── the one place eigenvectors get interpreted ────────────────────────────────

def fibre_orientation(eigval: np.ndarray, eigvec: np.ndarray):
    """Fibre direction + coherence from a **structure tensor** eigendecomposition.

    Expects this module's layout: `eigval` ascending along the last axis, `eigvec[..., i, :]`
    the unit eigenvector for `eigval[..., i]` (see `_eigen_decompose`).

    Returns `(direction, coherence)`:
      - `direction` — the **minor** eigenvector, i.e. the axis of least intensity variation,
        i.e. ALONG the fibre. Shape `(..., ndim)`, unit length, sign arbitrary (an orientation
        has no head or tail — fold to 0–90° when comparing angles, never 0–180°).
      - `coherence` — `(λmax − λmin) / (λmax + λmin)` ∈ [0, 1]. 0 = isotropic, 1 = perfectly
        aligned. Degenerate (zero-trace) boxes give 0.

    Coherence is a *shape* descriptor, not a quality gate: a real intravital SHG field sits
    around 0.1–0.4 (the published Figure 4 axis) and reads perfectly well as a quiver, because
    arrows are length-normalised for display. Do not threshold on it.
    """
    direction = np.asarray(eigvec)[..., 0, :]
    return direction, _coherence(np.asarray(eigval))


def tangent_orientation(eigval: np.ndarray, eigvec: np.ndarray):
    """Fibre direction + coherence from a **tangent tensor** eigendecomposition.

    Same contract as `fibre_orientation`, opposite convention: the tangent tensor's **major**
    eigenvector is the fibre direction. Separate function rather than a flag, so a call site
    cannot silently pick the wrong one by passing the wrong boolean.
    """
    direction = np.asarray(eigvec)[..., -1, :]
    return direction, _coherence(np.asarray(eigval))


def _coherence(eigval: np.ndarray) -> np.ndarray:
    tr = eigval.sum(axis=-1)
    with np.errstate(divide="ignore", invalid="ignore"):
        return np.where(tr > 0, (eigval[..., -1] - eigval[..., 0]) / tr, 0.0).astype(np.float32)


def _eigen_decompose(tensor: np.ndarray):
    """`eigh` per box on a symmetric N×N tensor field → (eigval ascending, eigvec as ROWS).

    numpy returns eigenvectors as COLUMNS (`v[:, i]` ↔ `w[i]`); we transpose so
    `eigvec[..., i, :]` is the i-th vector. That is this module's layout, and it is NOT the
    layout ILEE_CSK stored (it kept numpy's columns, and sorted descending) — see the module
    docstring and SPATIAL_ANISOTROPY_PLAN A1.
    """
    eigval, eigvec = np.linalg.eigh(tensor)
    return eigval.astype(np.float32), np.swapaxes(eigvec, -1, -2).astype(np.float32)


# ── production estimator ──────────────────────────────────────────────────────

def structure_tensor_field(image: np.ndarray, sigma: float, box: int):
    """Local structure tensor of `image` at scale `sigma`, aggregated onto a `box` grid.

    Works for 2D and 3D. `skimage.feature.structure_tensor` returns the upper-triangular
    elements in `combinations_with_replacement` order — `[Arr, Arc, Acc]` in 2D,
    `[Azz, Azy, Azx, Ayy, Ayx, Axx]` in 3D — which is what `_from_upper_triangular` rebuilds.

    Returns `(coor, eigval, eigvec, coherence)`; feed `eigval`/`eigvec` to `fibre_orientation`.
    """
    elems = skimage.feature.structure_tensor(
        np.asarray(image, dtype=np.float32), sigma=sigma, mode="reflect"
    )
    pooled = [pool_by_box(a, box) for a in elems]
    tensor = _from_upper_triangular(pooled, image.ndim)
    eigval, eigvec = _eigen_decompose(tensor)
    return box_centres(image.shape, box), eigval, eigvec, _coherence(eigval)


def _from_upper_triangular(elems, ndim: int) -> np.ndarray:
    """[A00, A01, …, A11, …] (upper triangle, row-major) → a full symmetric (..., n, n) field."""
    rows = []
    k_of = {}
    k = 0
    for i in range(ndim):
        for j in range(i, ndim):
            k_of[(i, j)] = k_of[(j, i)] = k
            k += 1
    for i in range(ndim):
        rows.append(np.stack([elems[k_of[(i, j)]] for j in range(ndim)], axis=-1))
    return np.stack(rows, axis=-2)


def box_lengths(skeleton_bool: np.ndarray, box: int) -> np.ndarray:
    """Skeleton pixel count per box — the weight for `weighted_anisotropy`."""
    n_per_box = box ** skeleton_bool.ndim
    return (pool_by_box(np.asarray(skeleton_bool, dtype=np.float32), box) * n_per_box).astype(np.float32)


def weighted_anisotropy(coherence: np.ndarray, box_length: np.ndarray) -> float:
    """The ONE per-image anisotropy scalar — a **length-weighted** mean of per-box coherence.

    This is the number on the published Figure 4 panel D x-axis ("Anisotropy (1 = non-uniform)",
    range ≈ 0.1–0.4) and it matches ILEE_CSK's `weighting_method='by_length'`
    (`sum(box_anisotropy) / sum(box_total_length)`, which is length-weighted because each of its
    box terms is already divided by that box's length).

    Weighting matters: an unweighted `coherence.mean()` counts empty background boxes equally with
    boxes full of fibre, so it drifts with how much blank field an image happens to contain. On
    EaMaVq the two differ by ~0.05–0.06 (see SPATIAL_ANISOTROPY_PLAN A5).
    """
    total = float(np.sum(box_length))
    if total <= 0:
        return 0.0
    return float(np.sum(np.asarray(coherence) * np.asarray(box_length)) / total)


# ── legacy reference estimator (validation, not production) ───────────────────

def tangent_tensor_field(skeleton_bool: np.ndarray, box: int, radius: float):
    """REFERENCE: the anisotropy the old R version computed. Not the production path.

    Reimplements ILEE_CSK `anisotropy_2d_internal` / `anisotropy_3d_internal`
    (`functions.py:757-840`, `:867-948`): for each grid point, take every skeleton edge whose
    source node lies within `radius`, accumulate `outer(t̂, t̂) · edge_length`, eigendecompose.
    Legacy cecelia defaults were `radius = anisoRadius = 50`, `box = floor(radius / 2) = 25`.

    Kept in the shipped package (not the test tree) so "how does this differ from the old
    version?" is answerable with a function call from a notebook or the REPL, not an argument.
    `compare_fields` below is the packaged form of that answer.

    `skeleton_bool` MUST be a THIN (1px) skeleton, not a mask. `skan.Skeleton` walks pixel
    adjacency, so a multi-pixel-wide structure reads as a thicket of spurious junctions and its
    edge directions scatter — the same trap that corrupted branch topology in PR #396. Passing a
    2px-wide stripe field here misses a known 30° orientation by 59°. Skeletonise first.

    Returns `(coor, eigval, eigvec, coherence)` in THIS module's layout (ascending eigenvalues,
    eigenvectors as rows) — deliberately not ILEE's own layout, so the two estimators are
    directly comparable. Read the direction with `tangent_orientation`, NOT `fibre_orientation`.

    `skan` is imported lazily: it is an app dependency (`pixi.toml`), not part of the light IO
    tier an external `pip install cecelia` pulls (`python/pyproject.toml`).
    """
    import skan

    sk = skan.Skeleton(np.asarray(skeleton_bool, dtype=bool))
    pts = sk.coordinates
    graph = sk.nbgraph
    ndim = skeleton_bool.ndim
    grid = tuple(s // box for s in skeleton_bool.shape)

    tensors = np.zeros(grid + (ndim, ndim), dtype=np.float64)
    r2 = float(radius) ** 2

    for idx in np.ndindex(*grid):
        centre = np.array([i * box + box / 2 for i in idx])
        near = np.nonzero(((pts - centre) ** 2).sum(axis=1) < r2)[0]
        acc = np.zeros((ndim, ndim))
        for n in near:
            for m in graph.neighbors(n):
                if m <= n:
                    continue          # upper triangle only — don't count an edge twice
                d = pts[m] - pts[n]
                norm = np.linalg.norm(d)
                if norm == 0:
                    continue
                d = d / norm
                acc += np.outer(d, d) * graph.edge(n, m)
        tensors[idx] = acc

    eigval, eigvec = _eigen_decompose(tensors)
    return box_centres(skeleton_bool.shape, box), eigval, eigvec, _coherence(eigval)


# ── comparison ────────────────────────────────────────────────────────────────

def acute_angle(a: np.ndarray, b: np.ndarray) -> np.ndarray:
    """Acute angle in DEGREES between two (fields of) direction vectors, folded to 0–90°.

    An orientation has no head or tail, so 170° and 10° describe the same alignment. Folding is
    the reason this returns 0–90 and not the legacy vignette's `180 - matlib::angle(...)`, which
    split one physical alignment across both ends of its scale.
    """
    a = np.asarray(a, dtype=np.float64)
    b = np.asarray(b, dtype=np.float64)
    dot = np.abs(np.sum(a * b, axis=-1))
    na = np.linalg.norm(a, axis=-1)
    nb = np.linalg.norm(b, axis=-1)
    with np.errstate(divide="ignore", invalid="ignore"):
        cos = np.where((na * nb) > 0, dot / (na * nb), 0.0)
    return np.degrees(np.arccos(np.clip(cos, 0.0, 1.0)))


def neighbour_consistency(direction: np.ndarray, valid: np.ndarray = None) -> float:
    """Mean acute angle between each box's direction and its axis-forward neighbours, in degrees.

    The quality metric for a direction field: a smooth, plottable field scores low (the legacy
    reference scores ~22° on real SHG); pure noise scores 45°. This — NOT coherence — is what
    tuning `sigma`/`box` should be scored on (SPATIAL_ANISOTROPY_PLAN Decision 4).

    `valid` is an optional boolean mask over the grid (e.g. boxes that contain skeleton).
    """
    direction = np.asarray(direction)
    grid = direction.shape[:-1]
    if valid is None:
        valid = np.ones(grid, dtype=bool)
    angles = []
    for axis in range(len(grid)):
        lo = [slice(None)] * len(grid)
        hi = [slice(None)] * len(grid)
        lo[axis] = slice(0, -1)
        hi[axis] = slice(1, None)
        lo, hi = tuple(lo), tuple(hi)
        both = valid[lo] & valid[hi]
        if not both.any():
            continue
        angles.append(acute_angle(direction[lo][both], direction[hi][both]))
    if not angles:
        return float("nan")
    return float(np.mean(np.concatenate(angles)))


def direction_contrast(direction: np.ndarray, valid: np.ndarray = None, far_lag: int = 4) -> dict:
    """Quality of a direction field: do NEARBY boxes agree while DISTANT ones don't?

    Returns `{"near_deg", "far_deg", "contrast_deg"}` where `contrast = far - near`. Maximise it.

    **Use this, not `neighbour_consistency` alone, to tune sigma/box.** Neighbour agreement on its
    own is monotonically improved by blurring — measured on EaMaVq SHG, raising sigma from 2 to 25
    drove it from 43.5° to 17.2° while coherence collapsed from 0.32 to 0.13. The "best" score was
    an oversmoothed field that had stopped describing local structure. Contrast can't be gamed that
    way: over-smoothing drives near AND far to 0 together, noise drives both to 45°, and only a
    field with real spatial structure separates them.

    `far_lag` is the grid separation (in boxes) at which directions should be uncorrelated.
    """
    direction = np.asarray(direction)
    grid = direction.shape[:-1]
    if valid is None:
        valid = np.ones(grid, dtype=bool)
    near, far = [], []
    for axis in range(len(grid)):
        for lag, sink in ((1, near), (far_lag, far)):
            if grid[axis] <= lag:
                continue
            lo = [slice(None)] * len(grid); hi = [slice(None)] * len(grid)
            lo[axis] = slice(0, -lag); hi[axis] = slice(lag, None)
            lo, hi = tuple(lo), tuple(hi)
            both = valid[lo] & valid[hi]
            if both.any():
                sink.append(acute_angle(direction[lo][both], direction[hi][both]))
    near_deg = float(np.mean(np.concatenate(near))) if near else float("nan")
    far_deg = float(np.mean(np.concatenate(far))) if far else float("nan")
    return {"near_deg": near_deg, "far_deg": far_deg, "contrast_deg": far_deg - near_deg}


def compare_fields(image: np.ndarray, skeleton_bool: np.ndarray, sigma: float, box: int,
                   radius: float = None) -> dict:
    """Run both estimators on the same data and report how they differ. Validation, not pipeline.

    Returns a dict with, for each estimator, its `neighbour_consistency` and its per-image
    `weighted_anisotropy`, plus `mean_angle_between` — the mean acute angle between the two
    fibre-direction fields over boxes that contain skeleton.

    This is the packaged answer to "is the new implementation doing what the old one did?".
    Reference numbers on `EaMaVq` SHG (T=0, Z-MIP, skeleton source, sigma=12, box=25):
    tangent ≈ 22° consistency, structure ≈ 26°, ~23° between them.
    """
    radius = float(2 * box) if radius is None else float(radius)
    _, s_val, s_vec, _ = structure_tensor_field(image, sigma=sigma, box=box)
    _, t_val, t_vec, _ = tangent_tensor_field(skeleton_bool, box=box, radius=radius)
    s_dir, s_coh = fibre_orientation(s_val, s_vec)
    t_dir, t_coh = tangent_orientation(t_val, t_vec)
    blen = box_lengths(skeleton_bool, box)
    occupied = blen > 0
    return {
        "structure": {
            "neighbour_consistency_deg": neighbour_consistency(s_dir, occupied),
            "anisotropy": weighted_anisotropy(s_coh, blen),
        },
        "tangent_reference": {
            "neighbour_consistency_deg": neighbour_consistency(t_dir, occupied),
            "anisotropy": weighted_anisotropy(t_coh, blen),
        },
        "mean_angle_between_deg": float(np.mean(acute_angle(s_dir[occupied], t_dir[occupied])))
                                  if occupied.any() else float("nan"),
        "n_boxes_with_skeleton": int(occupied.sum()),
    }
