"""
Branching (skeleton) analysis task entry point.

Called by the Julia `segment.branching` handler as a subprocess. Skeletonises a segmentation
into a branch/path network via `skan.Skeleton` + `skan.summarize`, writes a labels zarr for the
skeleton, and a per-branch labelProps sidecar `{value_name}__branch.h5ad` (one row per skeleton
path). See `docs/todo/BRANCHING_PLAN.md` (Decisions 1, 6, 7, 8).

Phase 1 scope: skeletonisation + per-branch table. Anisotropy (`calcAnisotropy` / structure_tensor
+ eigendecomp; Decision 4) is Phase 3 and lives behind a param that this runner does not yet accept.

Parameter contract (JSON written by Julia):
  imPath              - source image path (for OME/scale metadata; not read pixel-wise)
  labelsPath          - input segmentation labels zarr
  branchLabelsOutPath - output skeleton labels zarr (written here)
  branchPropsOutPath  - output {vn}__branch.h5ad path (written here)
  qcOutPath           - JSON path for QC counters (nBranches, nSkeletons, meanBranchLength)
  labelIds            - list[int] restricting the input mask to these label IDs; [] = whole seg
  preDilationSize     - int; binary closing structuring-element radius before skeletonise
  postDilationSize    - int; dilation of the skeleton output (for napari visibility)
  useBorders          - bool; skeletonise label BOUNDARIES instead of interiors
  flattenBranching    - bool; Z-MIP the labels before skeletonising (3D only)
"""

import json
import os

import anndata as ad
import numpy as np
import pandas as pd
import skan
import skimage.morphology
import skimage.segmentation

import cecelia.utils.script_utils as script_utils
import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.label_props_utils import skimage_centroid_axis_names
import cecelia.utils.ome_xml_utils as ome_xml_utils


# skan's `separator` kwarg is pinned to '-' against the scheduled upstream flip to '_'.
# Column names our Julia + downstream code depend on: `branch-type`, `node-id-src/dst`,
# `branch-distance`, `image-coord-src-N`, `image-coord-dst-N`, `euclidean-distance`.
# See docs/todo/BRANCHING_PLAN.md → skan smoke test (Phase 0).
SKAN_SEPARATOR = "-"


def _binary_mask(labels: np.ndarray, label_ids, use_borders: bool) -> np.ndarray:
    """Mask → optional refPops filter → optional label-boundary conversion → bool array."""
    if label_ids:
        mask = np.isin(labels, np.asarray(list(label_ids), dtype=labels.dtype))
        labels = np.where(mask, labels, 0)
    if use_borders:
        return skimage.segmentation.find_boundaries(labels)
    return labels > 0


def _skeletonise(bin_im: np.ndarray, pre: int, post: int, is_3d: bool) -> np.ndarray:
    """Optional pre-closing → skeletonise → optional post-dilation for visibility."""
    if pre > 0:
        selem = skimage.morphology.ball(pre) if is_3d else skimage.morphology.disk(pre)
        bin_im = skimage.morphology.binary_closing(bin_im.astype(np.uint8), selem)
    sk = skimage.morphology.skeletonize(bin_im)
    if post > 0:
        selem = skimage.morphology.ball(post) if is_3d else skimage.morphology.disk(post)
        sk = skimage.morphology.dilation(sk.astype(np.uint8), selem).astype(bool)
    return sk


def _summarise_paths(skeleton_bool: np.ndarray, t_index):
    """`skan.Skeleton` + `skan.summarize(separator='-')` → paths DataFrame + label column.

    Adds `label` (1..N, unique across timepoints via caller-tracked offset), `path-id`, and
    (when `t_index` is not None) `centroid_t`. Returns (df, skeleton_array). The skeleton array
    (`np.asarray(skeleton)`) is the per-pixel path label — write it into the labels zarr.
    """
    sk = skan.Skeleton(skeleton_bool)
    df = skan.summarize(sk, separator=SKAN_SEPARATOR)
    df["path-id"] = np.arange(sk.n_paths)
    if t_index is not None:
        df["centroid_t"] = int(t_index)
    return df, np.asarray(sk)


def _iterate_timepoints(labels_arr: np.ndarray, dim_utils: DimUtils):
    """Yield (t_index, 2D-or-3D-labels-slice) per timepoint. t_index is None for static images."""
    if not dim_utils.is_timeseries():
        yield None, labels_arr
        return
    t_idx = dim_utils.dim_idx("T", ignore_channel=True)
    n_t = labels_arr.shape[t_idx]
    for t in range(n_t):
        sl = [slice(None)] * labels_arr.ndim
        sl[t_idx] = t
        yield t, np.squeeze(labels_arr[tuple(sl)])


def _spatial_from_endpoints(df: pd.DataFrame, n_spatial: int) -> np.ndarray:
    """Branch centroid = median of the `image-coord-src-N` / `image-coord-dst-N` endpoints.
    Preserves the old convention (`create_branching.py:368-375`) while writing through the
    current obsm['spatial'] contract (Decision 8). Column order matches
    `skimage_centroid_axis_names(n_spatial)`: z,y,x (3D) or y,x (2D)."""
    src_cols = [f"image-coord-src-{i}" for i in range(n_spatial)]
    dst_cols = [f"image-coord-dst-{i}" for i in range(n_spatial)]
    src = df[src_cols].to_numpy(dtype=np.float32)
    dst = df[dst_cols].to_numpy(dtype=np.float32)
    return np.median(np.stack([src, dst], axis=0), axis=0)


def _write_branch_h5ad(paths_df: pd.DataFrame, is_3d: bool, has_time: bool, out_path: str):
    """Create the {vn}__branch.h5ad sidecar. Producing-task exception — direct anndata write,
    same shape as `measure_utils._to_anndata` (docs/DATAMODEL.md 'Reading .h5ad' + CLAUDE.md)."""
    n_spatial = 3 if is_3d else 2
    obsm_spatial = _spatial_from_endpoints(paths_df, n_spatial)
    obsm_temporal = (
        paths_df["centroid_t"].to_numpy(dtype=np.float32).reshape(-1, 1)
        if has_time and "centroid_t" in paths_df.columns else None
    )

    # X carries everything except the spatial/temporal columns that we lift into obsm. `label` is
    # the row key (as index); the endpoint columns stay in X so downstream consumers can rebuild
    # the branch geometry (the R vignettes read image-coord-src-*/-dst-* for quivers).
    obs_index_col = "label"
    reserved = set([obs_index_col, "centroid_t"])
    feature_cols = [c for c in paths_df.columns if c not in reserved]
    X = paths_df[feature_cols].to_numpy(dtype=np.float32)

    adata = ad.AnnData(
        X=X,
        obs=pd.DataFrame(index=paths_df[obs_index_col].astype(str).values),
        var=pd.DataFrame(index=feature_cols),
    )
    adata.obsm["spatial"] = obsm_spatial
    adata.uns["spatial_cols"] = skimage_centroid_axis_names(n_spatial)
    if obsm_temporal is not None:
        adata.obsm["temporal"] = obsm_temporal
        adata.uns["temporal_cols"] = ["centroid_t"]

    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    adata.write_h5ad(out_path)


def run(params: dict):
    log = script_utils.get_logfile_utils(params)

    im_path              = script_utils.get_param(params, "imPath")
    labels_path          = script_utils.get_param(params, "labelsPath")
    branch_labels_out    = script_utils.get_param(params, "branchLabelsOutPath")
    branch_props_out     = script_utils.get_param(params, "branchPropsOutPath")
    qc_out_path          = script_utils.get_param(params, "qcOutPath")
    label_ids            = script_utils.get_param(params, "labelIds", default=[]) or []
    pre_dilation_size    = int(script_utils.get_param(params, "preDilationSize", default=2))
    post_dilation_size   = int(script_utils.get_param(params, "postDilationSize", default=2))
    use_borders          = bool(script_utils.get_param(params, "useBorders", default=False))
    flatten_branching    = bool(script_utils.get_param(params, "flattenBranching", default=False))

    log.log(f">> open labels {labels_path}")

    # Canonical readers — never hand-rolled (CLAUDE.md → Image / OME-ZARR access).
    labels_list, _ = zarr_utils.open_as_zarr(labels_path)
    labels_data = zarr_utils.fortify(labels_list[0])

    omexml = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml)
    dim_utils.calc_image_dimensions(labels_data.shape)

    is_3d = dim_utils.is_3D()
    has_time = dim_utils.is_timeseries()

    # Optional 3D → 2D flatten before skeletonise. Do the Z-MIP up-front and treat as 2D from here.
    if flatten_branching and is_3d:
        z_idx = dim_utils.dim_idx("Z", ignore_channel=True, ignore_time=not has_time)
        labels_data = np.max(labels_data, axis=z_idx)
        is_3d = False
        log.log(f"> flattened Z → shape {labels_data.shape}")

    paths_tables = []
    skeleton_frames = []      # one np.ndarray per timepoint (or one for a static image)
    n_skeletons_total = 0
    label_offset = 0

    for t_index, labels_slice in _iterate_timepoints(labels_data, dim_utils):
        log.log(f"> skeletonise{'' if t_index is None else f' T={t_index}'}")
        bin_im = _binary_mask(labels_slice, label_ids, use_borders)
        skeleton_bool = _skeletonise(bin_im, pre_dilation_size, post_dilation_size, is_3d)
        df, skeleton_arr = _summarise_paths(skeleton_bool, t_index)

        if df.empty:
            skeleton_frames.append(skeleton_arr.astype(np.uint32))
            continue

        # Uniqueify `label` across timepoints so the h5ad row key is globally unique.
        df["label"] = np.arange(len(df)) + 1 + label_offset
        label_offset = int(df["label"].max())
        paths_tables.append(df)

        # `skeleton_arr` starts at 1 per timepoint — shift by label_offset - n_local so labels
        # in the labels zarr match the h5ad label column.
        n_local = int(df["path-id"].max()) + 1 if len(df) else 0
        arr = skeleton_arr.astype(np.uint32)
        arr[arr > 0] += np.uint32(label_offset - n_local)
        skeleton_frames.append(arr)
        n_skeletons_total += int(df["skeleton-id"].nunique())

    if paths_tables:
        paths_df = pd.concat(paths_tables, axis=0, ignore_index=True)
    else:
        paths_df = pd.DataFrame(
            columns=["label", "path-id", "skeleton-id", "node-id-src", "node-id-dst",
                     "branch-distance", "branch-type", "euclidean-distance"]
        )

    log.log(f"> {len(paths_df)} branch(es) across {n_skeletons_total} skeleton(s)")

    # Write the labels zarr — a fresh multiscales store (canonical create path).
    if flatten_branching or not has_time:
        stacked = skeleton_frames[0] if len(skeleton_frames) == 1 else np.stack(skeleton_frames, axis=0)
    else:
        t_idx = dim_utils.dim_idx("T", ignore_channel=True)
        # Re-insert time axis at the original position
        stacked = np.stack(skeleton_frames, axis=t_idx)

    log.log(f"> write labels zarr {branch_labels_out}")
    os.makedirs(os.path.dirname(branch_labels_out), exist_ok=True)
    zarr_utils.create_multiscales(
        stacked, branch_labels_out,
        dim_utils=dim_utils,
        nscales=1,
        keyword="labels",
        ignore_channel=True,
        squeeze=False,
    )

    log.log(f"> write branch props {branch_props_out}")
    _write_branch_h5ad(paths_df, is_3d, has_time and not flatten_branching, branch_props_out)

    mean_branch_length = (
        float(paths_df["branch-distance"].mean()) if "branch-distance" in paths_df.columns and len(paths_df)
        else 0.0
    )
    with open(qc_out_path, "w") as f:
        json.dump({
            "nBranches": int(len(paths_df)),
            "nSkeletons": int(n_skeletons_total),
            "meanBranchLength": mean_branch_length,
        }, f)


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] No params file provided (--params missing or not found)", flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == "__main__":
    main()
