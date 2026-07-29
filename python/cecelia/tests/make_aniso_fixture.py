"""Regenerate `test-data/projects/testpr/1/KDIeEm/labelProps/aniso__branch.h5ad`.

A tiny SYNTHETIC branch sidecar — NOT a real run. Run it with `pixi run python` after any change
to the anisotropy `uns` layout: `pixi run python python/cecelia/tests/make_aniso_fixture.py`.

Not a TestCase (the discovery pattern is `test_*.py`, so unittest ignores it) — it is a generator
for the fixture that `runtests.jl` reads.

Every value encodes its own coordinate so a transposed/axis-swapped read FAILS instead of passing
on symmetry (HDF5 is C-order, Julia is column-major → numpy (T,y,x,comp) arrives fully reversed,
including the two equal-length box axes).

  orientation_coords[t,y,x,0] = 100t + 10y + x        (the y component)
  orientation_coords[t,y,x,1] = 1000 + 100t + 10y + x (the x component)

The grid is 3 frames x 4x4 boxes; the table is 6 branches over 3 timepoints with real endpoint
columns, a per-branch `anisotropy` obs column, and an `orientation_summary` frame (one row per t).
"""
import os

import numpy as np
import pandas as pd
import anndata as ad

# <workspace-root>/test-data/… — the fixtures dir is a SIBLING of this repo (see test-data/README.md).
# Override with CECELIA_TEST_DATA, the same env var runtests.jl honours.
_ROOT = os.environ.get("CECELIA_TEST_DATA") or os.path.normpath(
    os.path.join(os.path.dirname(__file__), "..", "..", "..", "..", "test-data", "projects"))
OUT = os.path.join(_ROOT, "testpr", "1", "KDIeEm", "labelProps", "aniso__branch.h5ad")
T, NY, NX = 3, 4, 4

coor = np.zeros((T, NY, NX, 2), dtype=np.float32)
for t in range(T):
    for y in range(NY):
        for x in range(NX):
            coor[t, y, x, 0] = 100 * t + 10 * y + x
            coor[t, y, x, 1] = 1000 + 100 * t + 10 * y + x

# eigenvalues ASCENDING [λmin, λmax]; eigvec rows, eigvec[..., i, :] ↔ eigval[..., i].
# Row 0 (the MINOR / fibre direction) is a pure +x unit vector, row 1 is +y — so a reader that
# takes the wrong row gets a 90° rotation the test can see.
eigval = np.zeros((T, NY, NX, 2), dtype=np.float32)
eigval[..., 0] = 1.0
eigval[..., 1] = 3.0
eigvec = np.zeros((T, NY, NX, 2, 2), dtype=np.float32)
eigvec[..., 0, 1] = 1.0        # minor → (y=0, x=1)
eigvec[..., 1, 0] = 1.0        # major → (y=1, x=0)
box_len = np.full((T, NY, NX), 5.0, dtype=np.float32)
box_aniso = np.full((T, NY, NX), 0.5, dtype=np.float32)
# one box per frame is empty, so a `length == 0` filter has something to bite on
box_len[:, 0, 0] = 0.0
box_aniso[:, 0, 0] = 0.0

summary = pd.DataFrame({
    "occupancy":      np.array([0.10, 0.20, 0.30], dtype=np.float32),
    "linear_density": np.array([0.01, 0.02, 0.03], dtype=np.float32),
    "skewness":       np.array([0.50, 0.60, 0.70], dtype=np.float32),
    "cv":             np.array([1.10, 1.20, 1.30], dtype=np.float32),
    "MF_full_length": np.array([100.0, 200.0, 300.0], dtype=np.float32),
    "branching_act":  np.array([0.05, 0.06, 0.07], dtype=np.float32),
    "anisotropy":     np.array([0.21, 0.32, 0.43], dtype=np.float32),
})

n = 6
paths = pd.DataFrame({
    "label": np.arange(1, n + 1),
    "branch-distance":    np.arange(n, dtype=np.float32) * 3.0,
    "branch-type":        np.array([0, 1, 2, 3, 1, 2], dtype=np.float32),
    "euclidean-distance": np.arange(n, dtype=np.float32) * 2.0,
    # 2D: axis 0 = y, axis 1 = x
    "image-coord-src-0":  np.array([0, 10, 20, 30, 40, 50], dtype=np.float32),
    "image-coord-src-1":  np.array([1, 11, 21, 31, 41, 51], dtype=np.float32),
    "image-coord-dst-0":  np.array([4, 14, 24, 34, 44, 54], dtype=np.float32),
    "image-coord-dst-1":  np.array([5, 15, 25, 35, 45, 55], dtype=np.float32),
    "centroid_t":         np.array([0, 0, 1, 1, 2, 2], dtype=np.float32),
})

feature_cols = [c for c in paths.columns if c not in ("label", "centroid_t")]
obs = pd.DataFrame({"anisotropy": np.array([0.1, 0.2, 0.3, 0.4, 0.5, 0.6], dtype=np.float32)},
                   index=paths["label"].astype(str).values)
adata = ad.AnnData(X=paths[feature_cols].to_numpy(dtype=np.float32), obs=obs,
                   var=pd.DataFrame(index=feature_cols))
src = paths[["image-coord-src-0", "image-coord-src-1"]].to_numpy(dtype=np.float32)
dst = paths[["image-coord-dst-0", "image-coord-dst-1"]].to_numpy(dtype=np.float32)
adata.obsm["spatial"] = np.median(np.stack([src, dst], axis=0), axis=0)
adata.uns["spatial_cols"] = ["centroid_y", "centroid_x"]
adata.obsm["temporal"] = paths["centroid_t"].to_numpy(dtype=np.float32).reshape(-1, 1)
adata.uns["temporal_cols"] = ["centroid_t"]
adata.uns["orientation_coords"] = coor
adata.uns["orientation_eigval"] = eigval
adata.uns["orientation_eigvec"] = eigvec
adata.uns["orientation_box_length"] = box_len
adata.uns["orientation_box_coherence"] = box_aniso
adata.uns["orientation_summary"] = summary
adata.uns["orientation_meta"] = {
    "box_size_px": 15, "sigma_px": 12.0, "source": "skeleton",
    "scale_um_per_px": np.array([0.5, 0.5], dtype=np.float32),
    "flattened": True, "t_index": np.array([0, 1, 2], dtype=np.int64),
    "eigvec_layout": "vec_major", "eigval_order": "ascending", "fibre_direction": "minor",
}
adata.write_h5ad(OUT)
print("wrote", OUT)
