"""
Normalise a label-props AnnData to the explicit centroid-axis convention.

ONE home for converting any prior centroid layout to the current one (`obsm['spatial']` labelled
`centroid_x`/`_y`/`_z` in `uns['spatial_cols']`, `obsm['temporal']` labelled `centroid_t`). Two prior
shapes are handled — see `docs/todo/CENTROID_AXES_PLAN.md`:

  A. Pineapple with skimage's positional labels: `obsm['spatial']` present but `uns['spatial_cols']`
     is `centroid-0..N` (and `uns['temporal_cols']` is `['t']`). Relabel only — the matrices are
     unchanged (skimage z,y,x order → `centroid_z/_y/_x`).
  B. Old R (or any pre-obsm) file with flat `centroid_x/_y/_z/_t` columns in `var`/`X`: lift them into
     `obsm` with the explicit `uns` labels and drop them from `X` (same as `legacy_migrate`).

Idempotent: a file already in the explicit-obsm form reports no changes. This is the remedy the strict
read-time guard (`label_props_utils._check_centroid_names`) points at.
"""
import re

import numpy as np

from cecelia.utils.label_props_utils import skimage_centroid_axis_names

_EXPLICIT_SPATIAL = re.compile(r"^centroid_[xyz]$")
# flat R var names, in skimage z,y,x order so the lifted matrix column order matches the labels
_FLAT_SPATIAL = ("centroid_z", "centroid_y", "centroid_x")
_FLAT_TEMPORAL = ("centroid_t", "t")


def normalise_centroids(adata):
    """Mutate `adata` in place to the explicit-obsm centroid convention. Returns a list of human-readable
    change descriptions (empty ⇒ already normalised). Returns the (possibly new) adata via the caller's
    reference for case B, so callers use the returned object: ``adata, changes = normalise_centroids(a)``."""
    changes = []

    if "spatial" in adata.obsm:
        # Case A — relabel positional/legacy names; the obsm matrix is untouched.
        sc = [str(x) for x in adata.uns.get("spatial_cols", [])]
        if sc and not all(_EXPLICIT_SPATIAL.match(x) for x in sc):
            new = skimage_centroid_axis_names(len(sc))
            adata.uns["spatial_cols"] = np.array(new, dtype=object)
            changes.append(f"spatial_cols {sc} -> {new}")
        if "temporal" in adata.obsm:
            tc = [str(x) for x in adata.uns.get("temporal_cols", [])]
            if tc and tc != ["centroid_t"]:
                adata.uns["temporal_cols"] = np.array(["centroid_t"], dtype=object)
                changes.append(f"temporal_cols {tc} -> ['centroid_t']")
        return adata, changes

    # Case B — flat centroid columns in var → lift into obsm (mirror of legacy_migrate's lift).
    var_names = [str(v) for v in adata.var_names]
    spatial_src = [c for c in _FLAT_SPATIAL if c in var_names]
    temporal_src = [c for c in _FLAT_TEMPORAL if c in var_names]
    if spatial_src:
        X = adata.to_df()
        adata.obsm["spatial"] = X[spatial_src].to_numpy(dtype=np.float32)
        adata.uns["spatial_cols"] = np.array(spatial_src, dtype=object)
        if temporal_src:
            adata.obsm["temporal"] = X[temporal_src[:1]].to_numpy(dtype=np.float32)
            adata.uns["temporal_cols"] = np.array(["centroid_t"], dtype=object)
        keep = [c for c in var_names if c not in spatial_src + temporal_src]
        adata = adata[:, keep].copy()
        changes.append(f"lifted flat centroids {spatial_src + temporal_src} -> obsm")

    return adata, changes
