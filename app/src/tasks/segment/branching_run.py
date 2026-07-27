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
import skimage.feature
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


def _globalise_labels(df: pd.DataFrame, skeleton_arr: np.ndarray, offset: int):
    """Shift a timepoint's per-frame skeleton labels (1..N) to globally-unique labels
    starting at `offset + 1`, keeping the labels zarr array aligned with the h5ad `label`
    column. Returns `(df_with_label_col, arr_shifted, new_offset)`.

    Invariant tested in `test_branching_anisotropy.py`: the set of nonzero values in
    `arr_shifted` equals the set of `df.label` values. Empty df is a no-op — the frame passes
    through with zeros preserved.
    """
    if len(df) == 0:
        return df, skeleton_arr.astype(np.uint32), offset
    n_local = int(df["path-id"].max()) + 1
    df["label"] = np.arange(len(df)) + 1 + offset
    new_offset = int(df["label"].max())
    arr = skeleton_arr.astype(np.uint32)
    arr[arr > 0] += np.uint32(new_offset - n_local)
    return df, arr, new_offset


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


def _extract_fibre_image(im_data, dim_utils: DimUtils, fibre_channels, t_index):
    """Max-merge the selected fibre channels for one timepoint, return as a 2D (or 3D) array.
    `im_data` is the full multiscale level-0 array; `fibre_channels` are 0-based channel indices.
    Mirrors the old create_branching per-timepoint channel merge (`np.maximum` accumulation)."""
    c_idx = dim_utils.dim_idx("C")
    # Assemble a slice tuple that (a) picks a single T if timeseries, (b) keeps Z/Y/X, and (c) will
    # be indexed per-channel below.
    sl = [slice(None)] * im_data.ndim
    if dim_utils.is_timeseries() and t_index is not None:
        sl[dim_utils.dim_idx("T")] = int(t_index)
    merged = None
    for c in fibre_channels:
        sl[c_idx] = int(c)
        band = np.squeeze(np.asarray(im_data[tuple(sl)]))
        merged = band if merged is None else np.maximum(merged, band)
    return merged.astype(np.float32) if merged is not None else None


# ── Anisotropy (BRANCHING_PLAN Decision 4) ────────────────────────────────────
# Algorithmic ancestry: Li et al., *Plant Cell* 35, 371 (2023), doi:10.1093/plcell/koac290
# (ILEE_CSK — the local structure-tensor formulation for cytoskeleton anisotropy). This
# reimplementation replaces the vendored ILEE_CSK with skimage.feature.structure_tensor +
# numpy.linalg.eigh, dropping ~2000 LOC of unmaintained code (upstream last commit 2024-04-22,
# `imp` unimportable on py3.12, live /3 bug in the 2D path).
#
# The output shape is deliberately compatible with the old `uns` layout so R notebooks that
# indexed `x$ilee_coor_list[1,,,1]` etc. still read post-port outputs.
#
# Per timepoint (2D):
#   coor_list        (H_boxes, W_boxes, 2)     — box centre (y, x)
#   eigval           (H_boxes, W_boxes, 2)     — sorted ascending [λ₁, λ₂]
#   eigvec           (H_boxes, W_boxes, 2, 2)  — eigenvectors as ROWS: eigvec[..., i, :] ↔ eigval[..., i]
#   box_total_length (H_boxes, W_boxes)        — skeleton pixel count per box
#   box_anisotropy   (H_boxes, W_boxes)        — (λ₂ - λ₁) / (λ₁ + λ₂), 0 if degenerate
# 3D is the same shape family with an extra axis.

def _pool_by_box(x: np.ndarray, box: int) -> np.ndarray:
    """Mean-pool a (H, W[, D]) array into a box-grid by trimming trailing pixels and averaging
    over box × box[× box] windows. Vectorised (reshape + mean); no numba, no loops."""
    shp = np.asarray(x.shape)
    trimmed = tuple(int((s // box) * box) for s in shp)
    sl = tuple(slice(0, t) for t in trimmed)
    y = x[sl]
    new_shape = []
    for s in y.shape:
        new_shape.extend([s // box, box])
    y = y.reshape(new_shape)
    axes_to_mean = tuple(range(1, y.ndim, 2))
    return y.mean(axis=axes_to_mean)


def _box_centres(shape: tuple, box: int) -> np.ndarray:
    """Grid of box-centre coordinates in image pixel units. shape=(H,W) → (H_boxes,W_boxes,2)."""
    ns = tuple(s // box for s in shape)
    axes = [np.arange(n) * box + box / 2 for n in ns]
    grids = np.meshgrid(*axes, indexing="ij")
    return np.stack(grids, axis=-1).astype(np.float32)


def _anisotropy_from_tensor(box_tensor: np.ndarray):
    """`eigh` per box on a symmetric N×N tensor field.
    box_tensor shape (H_boxes, W_boxes[, D_boxes], N, N) → (eigval, eigvec, anisotropy)."""
    eigval, eigvec = np.linalg.eigh(box_tensor)         # eigval sorted ascending
    # Move numpy's eigvec layout (columns) to rows so eigvec[..., i, :] ↔ eigval[..., i] —
    # matches the old ILEE convention the R vignettes index into.
    eigvec = np.swapaxes(eigvec, -1, -2)
    tr = eigval.sum(axis=-1)
    with np.errstate(divide="ignore", invalid="ignore"):
        aniso = np.where(tr > 0, (eigval[..., -1] - eigval[..., 0]) / tr, 0.0)
    return eigval.astype(np.float32), eigvec.astype(np.float32), aniso.astype(np.float32)


def _anisotropy_2d(fibre_im: np.ndarray, skeleton_bool: np.ndarray, sigma: float, box: int):
    """Local structure tensor at scale `sigma`, mean-pooled over `box × box` windows, then
    eigendecomposed per box. Returns the 5-tuple that mirrors ILEE_CSK's `return_box_data`."""
    Arr, Arc, Acc = skimage.feature.structure_tensor(
        fibre_im.astype(np.float32), sigma=sigma, mode="reflect"
    )
    Trr = _pool_by_box(Arr, box)
    Trc = _pool_by_box(Arc, box)
    Tcc = _pool_by_box(Acc, box)
    box_tensor = np.stack(
        [np.stack([Trr, Trc], axis=-1),
         np.stack([Trc, Tcc], axis=-1)], axis=-2
    )   # shape (H_b, W_b, 2, 2)
    eigval, eigvec, aniso = _anisotropy_from_tensor(box_tensor)
    coor_list = _box_centres(fibre_im.shape, box)
    box_len = _pool_by_box(skeleton_bool.astype(np.float32), box) * (box * box)
    return coor_list, eigval, eigvec, box_len.astype(np.float32), aniso


def _anisotropy_3d(fibre_im: np.ndarray, skeleton_bool: np.ndarray, sigma: float, box: int):
    """3D counterpart. skimage returns 6 upper-triangular elements ordered
    `[Azz, Azy, Azx, Ayy, Ayx, Axx]`."""
    A = skimage.feature.structure_tensor(
        fibre_im.astype(np.float32), sigma=sigma, mode="reflect"
    )
    Azz, Azy, Azx, Ayy, Ayx, Axx = [_pool_by_box(a, box) for a in A]
    box_tensor = np.stack([
        np.stack([Azz, Azy, Azx], axis=-1),
        np.stack([Azy, Ayy, Ayx], axis=-1),
        np.stack([Azx, Ayx, Axx], axis=-1),
    ], axis=-2)   # (D_b, H_b, W_b, 3, 3)
    eigval, eigvec, aniso = _anisotropy_from_tensor(box_tensor)
    coor_list = _box_centres(fibre_im.shape, box)
    box_len = _pool_by_box(skeleton_bool.astype(np.float32), box) * (box ** 3)
    return coor_list, eigval, eigvec, box_len.astype(np.float32), aniso


def _scalar_summary(fibre_im: np.ndarray, skeleton_bool: np.ndarray,
                    pixel_size: float, aniso_scalar: float) -> pd.DataFrame:
    """The scalar per-image summary that used to come from `analyze_actin_{2d,3d}_standard`'s
    first return: occupancy, cv, skewness, MF_full_length, linear_density, branching_act, aniso.
    No `/3` bug (nothing is oversampled 3×). `Diameter_tdt`/`Diameter_sdt` are dropped; they
    aren't consumed by any surviving vignette."""
    binary = skeleton_bool.astype(bool)
    on = fibre_im[binary]
    occupancy = float(binary.mean())
    if on.size > 1 and on.std() > 0:
        mean_on = float(on.mean()); std_on = float(on.std())
        cv = std_on / max(mean_on, 1e-12)
        skewness = float(np.mean(((on - mean_on) / std_on) ** 3))
    else:
        cv = 0.0
        skewness = 0.0
    mf_full_length = float(binary.sum()) * pixel_size
    area_or_volume = float(np.prod(fibre_im.shape)) * pixel_size ** fibre_im.ndim
    linear_density = mf_full_length / area_or_volume if area_or_volume > 0 else 0.0
    # branching_act: nodes with degree > 2 per unit skeleton length. Same idea as the old code.
    if binary.any():
        sk_obj = skan.Skeleton(binary)
        nodes = np.concatenate([sk_obj.paths.indices, np.array([], dtype=np.int64)])
        _, counts = np.unique(nodes, return_counts=True)
        branching = float(np.sum(counts[counts > 2] - 2)) / mf_full_length if mf_full_length else 0.0
    else:
        branching = 0.0
    return pd.DataFrame([{
        "occupancy": occupancy,
        "linear_density": linear_density,
        "skewness": skewness,
        "cv": cv,
        "MF_full_length": mf_full_length,
        "branching_act": branching,
        "anisotropy": float(aniso_scalar),
    }])


def _write_branch_h5ad(paths_df: pd.DataFrame, is_3d: bool, has_time: bool, out_path: str,
                       aniso_uns: dict = None):
    """Create the {vn}__branch.h5ad sidecar. Producing-task exception — direct anndata write,
    same shape as `measure_utils._to_anndata` (docs/DATAMODEL.md 'Reading .h5ad' + CLAUDE.md).
    Optional `aniso_uns` merges the anisotropy outputs into `uns` under the ilee_* keys the old
    R vignettes still index into (Decision 4)."""
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
    if aniso_uns:
        for k, v in aniso_uns.items():
            adata.uns[k] = v

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
    calc_anisotropy      = bool(script_utils.get_param(params, "calcAnisotropy", default=False))
    calc_flattened       = bool(script_utils.get_param(params, "calcFlattened", default=False))
    fibre_channels       = script_utils.get_param(params, "fibreChannels", default=[]) or []
    st_sigma             = float(script_utils.get_param(params, "structureTensorSigma", default=2.0))
    aniso_box_size       = int(script_utils.get_param(params, "anisotropyBoxSize", default=45))

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

    # Anisotropy needs the source image; only open it if the user asked. Merged fibre-channel image
    # is materialised per timepoint below (max-projection over the selected channels, following the
    # old create_branching convention).
    im_list = None
    if calc_anisotropy and fibre_channels:
        im_list, _ = zarr_utils.open_as_zarr(im_path)
        im_data = im_list[0]     # highest-res level

    paths_tables = []
    skeleton_frames = []      # one np.ndarray per timepoint (or one for a static image)
    n_skeletons_total = 0
    label_offset = 0
    aniso_coor, aniso_eigval, aniso_eigvec = [], [], []
    aniso_box_len, aniso_box_aniso, aniso_summary = [], [], []

    for t_index, labels_slice in _iterate_timepoints(labels_data, dim_utils):
        log.log(f"> skeletonise{'' if t_index is None else f' T={t_index}'}")
        bin_im = _binary_mask(labels_slice, label_ids, use_borders)
        skeleton_bool = _skeletonise(bin_im, pre_dilation_size, post_dilation_size, is_3d)
        df, skeleton_arr = _summarise_paths(skeleton_bool, t_index)

        df, arr, label_offset = _globalise_labels(df, skeleton_arr, label_offset)
        if df.empty:
            skeleton_frames.append(arr)
            continue
        paths_tables.append(df)
        skeleton_frames.append(arr)
        n_skeletons_total += int(df["skeleton-id"].nunique())

        if calc_anisotropy and im_list is not None:
            fibre_im = _extract_fibre_image(im_data, dim_utils, fibre_channels, t_index)
            fibre_2d = fibre_im
            sk_bool = skeleton_bool
            if calc_flattened and fibre_im.ndim == 3:
                z_axis = 0     # after squeeze/extract the leading axis is the remaining Z (see _extract)
                fibre_2d = np.max(fibre_im, axis=z_axis)
                sk_bool = np.max(sk_bool, axis=z_axis) if sk_bool.ndim == 3 else sk_bool
            do_3d = fibre_2d.ndim == 3
            coor, ev, evec, blen, ban = (
                _anisotropy_3d(fibre_2d, sk_bool, st_sigma, aniso_box_size) if do_3d else
                _anisotropy_2d(fibre_2d, sk_bool, st_sigma, aniso_box_size)
            )
            aniso_coor.append(coor); aniso_eigval.append(ev); aniso_eigvec.append(evec)
            aniso_box_len.append(blen); aniso_box_aniso.append(ban)
            aniso_scalar = float(ban.mean()) if ban.size else 0.0
            pxsz = float(dim_utils.im_physical_size("x")) if hasattr(dim_utils, "im_physical_size") else 1.0
            aniso_summary.append(_scalar_summary(fibre_2d, sk_bool, pxsz, aniso_scalar))

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

    aniso_uns = None
    if calc_anisotropy and aniso_coor:
        # Stack per-timepoint arrays along a leading T axis so the R vignettes' [T, ...]
        # indexing works whether the image was a timeseries or a single frame.
        aniso_uns = {
            "ilee_coor_list":        np.stack(aniso_coor, axis=0),
            "ilee_eigval":           np.stack(aniso_eigval, axis=0),
            "ilee_eigvec":           np.stack(aniso_eigvec, axis=0),
            "ilee_box_total_length": np.stack(aniso_box_len, axis=0),
            "ilee_box_anisotropy":   np.stack(aniso_box_aniso, axis=0),
            "ilee_summary":          pd.concat(aniso_summary, axis=0, ignore_index=True)
                                          .astype(np.float32),
        }

    log.log(f"> write branch props {branch_props_out}")
    _write_branch_h5ad(paths_df, is_3d, has_time and not flatten_branching, branch_props_out,
                       aniso_uns=aniso_uns)

    mean_branch_length = (
        float(paths_df["branch-distance"].mean()) if "branch-distance" in paths_df.columns and len(paths_df)
        else 0.0
    )
    # Unique branch-type codes present in the output → Julia auto-creates one filter pop per code
    # via ensure_filter_pop! (BRANCHING_PLAN Decision 3). Sort so pop-map order is stable across runs.
    branch_types = sorted({int(v) for v in paths_df["branch-type"].unique()}) \
        if "branch-type" in paths_df.columns and len(paths_df) else []

    with open(qc_out_path, "w") as f:
        json.dump({
            "nBranches": int(len(paths_df)),
            "nSkeletons": int(n_skeletons_total),
            "meanBranchLength": mean_branch_length,
            "branchTypes": branch_types,
        }, f)


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] No params file provided (--params missing or not found)", flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == "__main__":
    main()
