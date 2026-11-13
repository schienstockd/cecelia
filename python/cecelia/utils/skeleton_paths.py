"""A skeleton's branches as simplified polylines — what the viewer and the movie renderer draw.

`segment.branching` builds each frame's branch network with `skan.Skeleton`. Every path there is an
ordered run of skeleton voxels; this keeps it as a polyline, thinned by Ramer–Douglas–Peucker so a
straight run costs two points, not one per voxel. The polylines ride in the branch table
(`{vn}__branch.h5ad`) as a ragged array, one per row, in row order:

    uns["paths"] = {"offsets": int64[N + 1], "coords": float32[M, D]}
    uns["path_axes"] = ["z", "y", "x"]  (or ["y", "x"])

Row i's points are `coords[offsets[i]:offsets[i + 1]]`, in voxel index of the skeleton's frame, axes
as named. The frame (`centroid_t`) is the row's own.

Ramer–Douglas–Peucker: U. Ramer, Comput. Graph. Image Process. 1, 244–256 (1972),
doi:10.1016/S0146-664X(72)80017-0; D. Douglas & T. Peucker, Cartographica 10, 112–122 (1973),
doi:10.3138/FM57-6770-U75U-7727.
"""

from __future__ import annotations

import numpy as np

PATHS_KEY = "paths"
PATH_AXES_KEY = "path_axes"


def _point_segment_distance(pts: np.ndarray, a: np.ndarray, b: np.ndarray) -> np.ndarray:
    """Distance from each of `pts` to the segment a–b (not the infinite line), any dimension."""
    ab = b - a
    denom = float(ab @ ab)
    if denom == 0.0:
        return np.linalg.norm(pts - a, axis=1)
    s = np.clip((pts - a) @ ab / denom, 0.0, 1.0)
    return np.linalg.norm(pts - (a + s[:, None] * ab), axis=1)


def simplify_polyline(coords: np.ndarray, tolerance: float) -> np.ndarray:
    """Ramer–Douglas–Peucker: the subset of `coords` (K×D, in order) whose polyline stays within
    `tolerance` of every dropped point. Endpoints always kept; `tolerance <= 0` keeps every point.
    Iterative (an explicit stack), so a long branch cannot hit Python's recursion limit."""
    pts = np.asarray(coords, dtype=np.float64)
    n = len(pts)
    if n <= 2 or tolerance <= 0:
        return pts.copy()
    keep = np.zeros(n, dtype=bool)
    keep[0] = keep[-1] = True
    stack = [(0, n - 1)]
    while stack:
        i, j = stack.pop()
        if j - i < 2:
            continue
        d = _point_segment_distance(pts[i + 1:j], pts[i], pts[j])
        k = int(np.argmax(d))
        if d[k] > tolerance:
            m = i + 1 + k
            keep[m] = True
            stack.append((i, m))
            stack.append((m, j))
    return pts[keep]


def skeleton_polylines(skeleton, tolerance: float) -> list[np.ndarray]:
    """Every path of a `skan.Skeleton`, in path-id order (the order `skan.summarize` rows come in),
    simplified to `tolerance` voxels."""
    return [simplify_polyline(skeleton.path_coordinates(i), tolerance) for i in range(skeleton.n_paths)]


def pack_polylines(polylines: list[np.ndarray], n_dims: int) -> dict:
    """The ragged `uns["paths"]` form of `polylines` (one per table row, in row order)."""
    counts = np.array([len(p) for p in polylines], dtype=np.int64)
    offsets = np.concatenate([[0], np.cumsum(counts)]).astype(np.int64)
    coords = (np.concatenate([np.asarray(p, dtype=np.float32).reshape(-1, n_dims) for p in polylines])
              if polylines else np.zeros((0, n_dims), dtype=np.float32))
    return {"offsets": offsets, "coords": coords.astype(np.float32)}


def unpack_polylines(paths: dict) -> list[np.ndarray]:
    """`pack_polylines`' inverse."""
    off = np.asarray(paths["offsets"], dtype=np.int64)
    coords = np.asarray(paths["coords"], dtype=np.float32)
    return [coords[off[i]:off[i + 1]] for i in range(len(off) - 1)]
