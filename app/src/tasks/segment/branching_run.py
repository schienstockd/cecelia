"""
Branching (skeleton) analysis task entry point.

Called by the Julia `segment.branching` handler as a subprocess. Skeletonises a segmentation
into a branch/path network via `skan.Skeleton` + `skan.summarize`, writes a labels zarr for the
skeleton, and a per-branch labelProps sidecar `{value_name}__branch.h5ad` (one row per skeleton
path). See `docs/todo/BRANCHING_PLAN.md` (Decisions 1, 6, 7, 8).

The anisotropy pass (`calcAnisotropy`) computes the fibre-orientation field that feeds the quiver
plot and the per-image anisotropy readout. All of its maths lives in
`cecelia.utils.anisotropy_utils` — this runner only chooses the input array and stacks the
per-timepoint results. See `docs/todo/SPATIAL_ANISOTROPY_PLAN.md`.

Label frames are read from the store ONE TIMEPOINT AT A TIME (`_read_labels_frame` →
`zarr_utils.read_timepoint`), per `docs/todo/ZARR_STREAMING_PLAN.md` Decision 1. Never load a whole
label level: on a 201x20x544x548 uint32 set that is 4.8 GB resident before any work starts.

Parameter contract (JSON written by Julia):
  imPath              - source image path (for OME/scale metadata, and the fibre channels)
  labelsPath          - input segmentation labels zarr
  branchLabelsOutPath - output skeleton labels zarr (written here)
  branchPropsOutPath  - output {vn}__branch.h5ad path (written here)
  qcOutPath           - JSON path for QC counters (nBranches, nSkeletons, meanBranchLength,
                        anisotropy)
  labelIds            - list[int] restricting the input mask to these label IDs; [] = whole seg
  preDilationSize     - int; binary closing structuring-element radius before skeletonise
  postDilationSize    - int; dilation of the skeleton output (for napari visibility)
  useBorders          - bool; skeletonise label BOUNDARIES instead of interiors
  flattenBranching    - bool; Z-MIP the labels before skeletonising (3D only)
  integrateTime       - bool; collapse T to ONE network for the whole movie
  integrateTimeMode   - "max" | "avg"; applies to the raw channel only (labels always merge by max)
  calcAnisotropy      - bool; run the orientation-field pass
  calcFlattened       - bool; run the anisotropy pass on a Z-MIP even when the labels stayed 3D
  anisotropySource    - "skeleton" | "mask" | "channel"; which array the structure tensor reads
  fibreChannels       - list[int]; 0-based channel indices, only used by source="channel"
  structureTensorSigma- float; Gaussian integration scale of the structure tensor, PIXELS
  anisotropyBoxSize   - int; side of the aggregation box, PIXELS
  structureTensorSigmaUm / anisotropyBoxUm / umPerPx
                      - what the USER asked for, in um, and the factor Julia converted with. Recorded
                        in orientation_meta for provenance; the compute uses the pixel values above,
                        because array space is the only space these arrays have.
"""

import os

import anndata as ad
import numpy as np
import pandas as pd
import skan
import skimage.morphology
import skimage.segmentation
from cecelia.utils.atomic_io import write_h5ad_atomic, write_json_atomic

import cecelia.utils.anisotropy_utils as aniso
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


def _skeletonise(bin_im: np.ndarray, pre: int, is_3d: bool) -> np.ndarray:
    """Optional pre-closing → skeletonise. Returns the THIN (1px-wide) boolean skeleton.

    Deliberately does NOT dilate here. `skan.Skeleton` (built on this next, in
    `_summarise_paths`) walks pixel adjacency to construct the topological graph — degree-2
    pixels are a path, degree>=3 pixels are junctions. Feeding it an already-dilated (multi-pixel-
    wide) mask makes every pixel along the width have several neighbours, so skan reads a thick
    line as riddled with spurious junctions/short paths instead of one clean edge. Post-dilation is
    for napari visibility only and must happen AFTER skan has analysed the thin skeleton — see
    `_dilate_label_image`, applied to the labelled output, mirroring the old
    `create_branching.py` order (dilate `np.asarray(skeleton)`, never the mask skan consumes)."""
    if pre > 0:
        selem = skimage.morphology.ball(pre) if is_3d else skimage.morphology.disk(pre)
        # `closing` on a boolean input is the deprecation-safe successor to `binary_closing`
        # (skimage 0.26+ deprecates the binary_* helpers). Same operation.
        bin_im = skimage.morphology.closing(bin_im.astype(bool), selem)
    return skimage.morphology.skeletonize(bin_im)


def _summarise_paths(skeleton_bool: np.ndarray, t_index):
    """`skan.Skeleton` + `skan.summarize(separator='-')` → paths DataFrame + label column.

    Adds `label` (1..N, unique across timepoints via caller-tracked offset), `path-id`, and
    (when `t_index` is not None) `centroid_t`. Returns (df, skeleton_array). The skeleton array
    (`np.asarray(skeleton)`) is the per-pixel path label — write it into the labels zarr.
    `skeleton_bool` MUST be the thin, unmodified skeletonize() output (see `_skeletonise`).
    """
    sk = skan.Skeleton(skeleton_bool)
    df = skan.summarize(sk, separator=SKAN_SEPARATOR)
    df["path-id"] = np.arange(sk.n_paths)
    if t_index is not None:
        df["centroid_t"] = int(t_index)
    return df, np.asarray(sk)


def _dilate_label_image(label_arr: np.ndarray, post: int, is_3d: bool) -> np.ndarray:
    """Post-dilation of the already-PATH-LABELLED array, for napari visibility only — grows each
    path's footprint outward without touching skan's (already-finished) topology/branch-type
    read. Never apply this to the boolean mask before `skan.Skeleton` sees it (see
    `_skeletonise`)."""
    if post <= 0:
        return label_arr
    selem = skimage.morphology.ball(post) if is_3d else skimage.morphology.disk(post)
    return skimage.morphology.dilation(label_arr, selem)


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


def _iterate_timepoints(labels_arr: np.ndarray, dim_utils: DimUtils, has_time: bool = None):
    """Yield (t_index, 2D-or-3D-labels-slice) per timepoint of an ALREADY-IN-RAM label array.

    `has_time` overrides `dim_utils.is_timeseries()` — needed once `integrateTime` has collapsed
    the stack, since dim_utils still describes the SOURCE image.

    Only used for an array that is already one frame's worth (the `integrateTime` accumulator and
    the static-image case). The timeseries path reads from the STORE via `_read_labels_frame` —
    never materialise a whole label level (docs/todo/ZARR_STREAMING_PLAN.md Decision 1).
    """
    if has_time is None:
        has_time = dim_utils.is_timeseries()
    if not has_time:
        yield None, labels_arr
        return
    t_idx = dim_utils.dim_idx("T", ignore_channel=True)
    n_t = labels_arr.shape[t_idx]
    for t in range(n_t):
        sl = [slice(None)] * labels_arr.ndim
        sl[t_idx] = t
        yield t, np.squeeze(labels_arr[tuple(sl)])


def _read_labels_frame(level, dim_utils: DimUtils, t, flatten_z: bool, src_is_3d: bool):
    """ONE timepoint of the labels store, in RAM, already Z-MIPed if `flatten_z`.

    The streaming read (ZARR_STREAMING_PLAN Decision 1: "granularity = one timepoint, in RAM").
    This task used to `fortify(labels_list[0])` — the whole level — which on a 201×20×544×548 uint32
    label set is **4.8 GB resident** before a single frame is skeletonised, and scales with the movie.
    That is the pattern the streaming migration retired for the correction/segmentation tasks
    (PRs #315/#317/#319); branching landed afterwards and reintroduced it. Per frame instead: ~1 MB.

    `ignore_channel=True` is load-bearing — a label level has no C axis while `dim_utils` describes
    the source image, which does.

    The Z-MIP moves here from a whole-stack `np.max`: it is a per-frame reduction, so doing it on
    read keeps peak memory at one frame rather than one movie.
    """
    frame = zarr_utils.read_timepoint(level, dim_utils, t, drop_time=True, ignore_channel=True)
    if flatten_z and src_is_3d:
        # after drop_time the frame carries the label axes minus T, so Z is resolved with time out
        frame = np.max(frame, axis=dim_utils.dim_idx("Z", ignore_channel=True, ignore_time=True))
    return frame


def _collapse_time(arr: np.ndarray, axis: int, mode: str) -> np.ndarray:
    """Collapse a stack over `axis`. `mode` is "max" or "avg" (legacy `integrateTimeMode`)."""
    return np.mean(arr, axis=axis) if mode == "avg" else np.max(arr, axis=axis)


def _per_branch_anisotropy(paths_df: pd.DataFrame, coherence_frames, t_index, box: int,
                           n_spatial: int) -> np.ndarray:
    """Give every branch the coherence of the grid box it sits in → one value per branch row.

    This is what makes anisotropy a **branch measurement** rather than a whole-image number: it
    lands as an obs column on `{vn}__branch.h5ad`, so it reads through `pop_df(img, "branch", …)`
    like any other per-branch measure and can be compared ACROSS branch populations (is
    junction-to-junction structure more aligned than endpoint-to-endpoint?). That is the point of
    the `branch` pop type — see docs/todo/SPATIAL_ANISOTROPY_PLAN.md.

    The box index is integer division on the branch's own centroid (`_spatial_from_endpoints`), so
    no nearest-neighbour search is needed — the grid is regular. Branches outside the grid (the
    trailing pixels `pool_by_box` trims) get NaN rather than a clamped neighbour's value.
    """
    n = len(paths_df)
    out = np.full(n, np.nan, dtype=np.float32)
    if n == 0 or not len(coherence_frames):
        return out
    centroids = _spatial_from_endpoints(paths_df, n_spatial)        # (n, n_spatial) as z?,y,x
    # frame lookup: a T-collapsed run stores a single frame under t_index [-1]
    slot_of = {int(t): i for i, t in enumerate(t_index)}
    if "centroid_t" in paths_df.columns and len(t_index) > 1:
        slots = [slot_of.get(int(t), -1) for t in paths_df["centroid_t"].to_numpy()]
    else:
        slots = [0] * n
    for i in range(n):
        s = slots[i]
        if s < 0:
            continue
        grid = coherence_frames[s]
        # the grid's axes are the LAST `grid.ndim` spatial axes of the centroid (a 2D grid over a
        # 3D branch table means the anisotropy was computed on a Z-MIP — index by y, x only)
        idx = tuple(int(centroids[i, n_spatial - grid.ndim + k] // box) for k in range(grid.ndim))
        if all(0 <= idx[k] < grid.shape[k] for k in range(grid.ndim)):
            out[i] = grid[idx]
    return out


def _store_axes(dim_order, has_time: bool, is_3d: bool):
    """Axis letters of the LABEL store, which is not the source image's shape (finding A8).

    Labels never carry C; T is gone once `integrateTime` collapsed it; Z is gone once
    `flattenBranching` Z-MIPped it. Deriving the store's axes (and its per-axis scale) from the
    image instead tagged a 3-axis (T, Y, X) array as `t,c,z,y,x` with scale
    [1, 1, 3.0, 0.596, 0.596] — so a positional reader handed Y the Z step, a 5× stretch.
    """
    return [ax for ax in dim_order
            if ax != "C"
            and not (ax == "T" and not has_time)
            and not (ax == "Z" and not is_3d)]


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


def _spatial_scale(dim_utils: DimUtils, n_spatial: int):
    """Physical size per pixel for the anisotropy grid's axes: (y, x) in 2D, (z, y, x) in 3D.

    Banked into `orientation_meta` so a consumer can put the quiver in µm without re-reading OME-XML —
    `coor_list` itself is in pixels of the (possibly Z-MIPed) array.
    """
    axes = ["z", "y", "x"][-n_spatial:]
    return [float(dim_utils.im_physical_size(a)) for a in axes]


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


# ── Anisotropy ────────────────────────────────────────────────────────────────
# The maths lives in `cecelia.utils.anisotropy_utils` (structure tensor + eigendecomposition, and
# the legacy tangent-tensor reference the tests validate against). This runner's only jobs are
# picking the input array and keeping the per-timepoint stack aligned with `t`.
#
# CONVENTION, and the one thing to get right: the fibre direction is the structure tensor's
# **MINOR** eigenvector, not its major one. Read it via `aniso.fibre_orientation` — never index
# `orientation_eigvec` by hand. The old ILEE tangent tensor used the opposite convention, so the `uns`
# arrays below are shape-alike but NOT index-compatible with the old R vignettes; `orientation_meta`
# records the layout so a reader never has to guess. See docs/todo/SPATIAL_ANISOTROPY_PLAN.md A1.
#
# Per timepoint (2D):
#   coor_list        (H_boxes, W_boxes, 2)     — box centre (y, x), in PIXELS
#   eigval           (H_boxes, W_boxes, 2)     — ASCENDING [λmin, λmax]
#   eigvec           (H_boxes, W_boxes, 2, 2)  — as ROWS: eigvec[..., i, :] ↔ eigval[..., i]
#   box_total_length (H_boxes, W_boxes)        — skeleton pixel count per box
#   box_anisotropy   (H_boxes, W_boxes)        — coherence (λmax − λmin)/(λmax + λmin)
# 3D is the same shape family with an extra leading axis.

ANISOTROPY_SOURCES = ("skeleton", "mask", "channel")


def _anisotropy_input(source: str, fibre_im, labels_slice: np.ndarray,
                      skeleton_bool: np.ndarray) -> np.ndarray:
    """The array the structure tensor reads, per `anisotropySource`.

    `skeleton` (default) and `mask` are segmentation-derived and therefore denoised — they measure
    closest to the legacy skeleton-only estimator. `channel` reads the raw fibre channels and is
    the only source that survives a bad segmentation or describes structure nobody segmented.
    """
    if source == "skeleton":
        return skeleton_bool.astype(np.float32)
    if source == "mask":
        return (labels_slice > 0).astype(np.float32)
    if fibre_im is None:
        raise ValueError("anisotropySource='channel' needs at least one fibreChannel")
    return np.asarray(fibre_im, dtype=np.float32)


def _match_rank(arr: np.ndarray, ndim: int) -> np.ndarray:
    """Z-MIP `arr` down to `ndim` axes.

    Needed because the LABELS and the fibre CHANNEL can end up with different ranks: with
    `flattenBranching` the labels are Z-MIPed (so the skeleton is 2D) while `_extract_fibre_image`
    still returns the full 3D stack. Left unreconciled, the 3D branch produced a `box_total_length`
    grid of a different rank to its four sibling arrays — silently, since nothing raised
    (SPATIAL_ANISOTROPY_PLAN A3).

    Axis 0 is the remaining Z: `_extract_fibre_image` squeezes out T and C, and in every layout
    cecelia imports Z precedes Y/X.
    """
    while arr.ndim > ndim:
        arr = np.max(arr, axis=0)
    return arr


def _scalar_summary(aniso_im: np.ndarray, skeleton_bool: np.ndarray,
                    pixel_size: float, aniso_scalar: float) -> pd.DataFrame:
    """The scalar per-image summary that used to come from `analyze_actin_{2d,3d}_standard`'s
    first return: occupancy, cv, skewness, MF_full_length, linear_density, branching_act, aniso.
    No `/3` bug (nothing is oversampled 3×). `Diameter_tdt`/`Diameter_sdt` are dropped; they
    aren't consumed by any surviving vignette.

    `anisotropy` is the LENGTH-WEIGHTED mean coherence (`aniso.weighted_anisotropy`), matching
    ILEE's `weighting_method='by_length'` — an unweighted mean counts empty background boxes
    equally and drifts with how much blank field the image contains (A5)."""
    binary = skeleton_bool.astype(bool)
    on = aniso_im[binary]
    occupancy = float(binary.mean())
    if on.size > 1 and on.std() > 0:
        mean_on = float(on.mean()); std_on = float(on.std())
        cv = std_on / max(mean_on, 1e-12)
        skewness = float(np.mean(((on - mean_on) / std_on) ** 3))
    else:
        cv = 0.0
        skewness = 0.0
    mf_full_length = float(binary.sum()) * pixel_size
    area_or_volume = float(np.prod(aniso_im.shape)) * pixel_size ** aniso_im.ndim
    linear_density = mf_full_length / area_or_volume if area_or_volume > 0 else 0.0
    # branching_act: nodes with degree > 2 per unit skeleton length. Same idea as the old code.
    if binary.any():
        _, counts = np.unique(skan.Skeleton(binary).paths.indices, return_counts=True)
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
                       aniso_uns: dict = None, obs_cols: dict = None):
    """Create the {vn}__branch.h5ad sidecar. Producing-task exception — direct anndata write,
    same shape as `measure_utils._to_anndata` (docs/DATAMODEL.md 'Reading .h5ad' + CLAUDE.md).
    Optional `aniso_uns` merges the anisotropy outputs into `uns` under the `aniso_*` keys
    (docs/SEGMENTATION.md)."""
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
    reserved = set([obs_index_col, "centroid_t"]) | set((obs_cols or {}).keys())
    feature_cols = [c for c in paths_df.columns if c not in reserved]
    X = paths_df[feature_cols].to_numpy(dtype=np.float32)

    # Per-branch derived measures (e.g. `anisotropy`) go in obs, so `pop_df(img, "branch", …)`
    # surfaces them as measures on branch populations — the whole reason the branch pop type exists.
    obs = pd.DataFrame(index=paths_df[obs_index_col].astype(str).values)
    for k, v in (obs_cols or {}).items():
        obs[k] = np.asarray(v, dtype=np.float32)
    adata = ad.AnnData(X=X, obs=obs, var=pd.DataFrame(index=feature_cols))
    adata.obsm["spatial"] = obsm_spatial
    adata.uns["spatial_cols"] = skimage_centroid_axis_names(n_spatial)
    if obsm_temporal is not None:
        adata.obsm["temporal"] = obsm_temporal
        adata.uns["temporal_cols"] = ["centroid_t"]
    if aniso_uns:
        for k, v in aniso_uns.items():
            adata.uns[k] = v

    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    write_h5ad_atomic(adata, out_path)


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
    st_sigma             = float(script_utils.get_param(params, "structureTensorSigma", default=12.0))
    aniso_box_size       = int(script_utils.get_param(params, "anisotropyBoxSize", default=15))
    sigma_um             = script_utils.get_param(params, "structureTensorSigmaUm", default=None)
    box_um               = script_utils.get_param(params, "anisotropyBoxUm", default=None)
    um_per_px            = script_utils.get_param(params, "umPerPx", default=None)
    aniso_source         = str(script_utils.get_param(params, "anisotropySource", default="skeleton"))
    integrate_time       = bool(script_utils.get_param(params, "integrateTime", default=False))
    integrate_time_mode  = str(script_utils.get_param(params, "integrateTimeMode", default="max"))
    if aniso_source not in ANISOTROPY_SOURCES:
        raise ValueError(f"anisotropySource must be one of {ANISOTROPY_SOURCES}, got {aniso_source!r}")

    log.log(f">> open labels {labels_path}")

    # Canonical readers — never hand-rolled (CLAUDE.md → Image / OME-ZARR access). The level stays
    # LAZY: frames are read one at a time by `_read_labels_frame` (ZARR_STREAMING_PLAN Decision 1).
    labels_list, _ = zarr_utils.open_as_zarr(labels_path)
    labels_level = labels_list[0]

    # DimUtils is built against the FULL IMAGE shape (with channel), matching every other task
    # in the codebase (measure_labels, cellpose, drift_correct, …). Labels have no C axis but
    # DimUtils' `ignore_channel=True` on dim_idx handles that at the label-slicing sites below
    # (_read_labels_frame, _iterate_timepoints). Passing the labels' shape here previously mismatched
    # the OME-XML dim count (C=4 vs labels' rank) and threw `ValueError: 4 is not in list`.
    omexml = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    im_list, _ = zarr_utils.open_as_zarr(im_path, as_dask=True)   # metadata-only, cheap
    dim_utils.calc_image_dimensions(im_list[0].shape)

    is_3d = dim_utils.is_3D()
    has_time = dim_utils.is_timeseries()
    src_is_3d = is_3d          # frames are Z-MIPed on READ, so the reader needs the SOURCE rank
    flatten_z = flatten_branching and is_3d
    if flatten_z:
        is_3d = False          # everything downstream sees 2D frames
        log.log("> flattening Z per frame")

    n_t = labels_level.shape[dim_utils.dim_idx("T", ignore_channel=True)] if has_time else 1

    # Optional T-collapse: one network for the whole movie. This is the one genuinely cross-frame
    # reduction, done as a RUNNING max so peak memory stays at one frame + the accumulator instead
    # of the whole movie. The LABEL stack always collapses by MAX (a union of where structure
    # existed) — "avg" of a label image is meaningless; `integrateTimeMode` applies to the raw
    # channel, which is the only source where an average means something. Both modes are equally
    # valid scientifically and neither is the default (SPATIAL_ANISOTROPY_PLAN Decision 9).
    collapsed = None
    if integrate_time and has_time:
        for t in range(n_t):
            fr = _read_labels_frame(labels_level, dim_utils, t, flatten_z, src_is_3d)
            collapsed = fr if collapsed is None else np.maximum(collapsed, fr)
        has_time = False
        log.log(f"> integrated time ({integrate_time_mode}) → labels shape {collapsed.shape}")

    # One frame at a time, from the store. `collapsed`/static images are already a single frame.
    def _frame_source():
        if not has_time:
            yield from _iterate_timepoints(
                collapsed if collapsed is not None else
                _read_labels_frame(labels_level, dim_utils, None, flatten_z, src_is_3d),
                dim_utils, has_time=False)
        else:
            for t in range(n_t):
                yield t, _read_labels_frame(labels_level, dim_utils, t, flatten_z, src_is_3d)

    # Anisotropy pass gets the raw pixels off the SAME im_list (single open above); no extra read.
    # Only source="channel" reads pixels at all — the other two work off the labels we already have.
    im_data = im_list[0] if (calc_anisotropy and aniso_source == "channel" and fibre_channels) else None

    paths_tables = []
    skeleton_frames = []      # one np.ndarray per timepoint (or one for a static image)
    n_skeletons_total = 0
    label_offset = 0
    aniso_coor, ev_frames, evec_frames = [], [], []
    aniso_box_len, aniso_box_aniso, summary_frames = [], [], []
    aniso_t_index = []        # the ACTUAL t of each stacked frame — see A4 below

    # Progress ticks per timepoint — the dominant cost is skeletonise + skan.summarize per frame
    # (139k branches over 7T on real data), so per-T is the right unit. Static images = 1 tick.
    # Prime the meter at 0/total before the loop so the UI shows a bar before the first frame lands.
    total_ticks = n_t if has_time else 1
    log.progress(0, total_ticks)

    for t_index, labels_slice in _frame_source():
        log.log(f"> skeletonise{'' if t_index is None else f' T={t_index}'}")
        bin_im = _binary_mask(labels_slice, label_ids, use_borders)
        skeleton_bool = _skeletonise(bin_im, pre_dilation_size, is_3d)
        df, skeleton_arr = _summarise_paths(skeleton_bool, t_index)
        skeleton_arr = _dilate_label_image(skeleton_arr, post_dilation_size, is_3d)

        df, arr, label_offset = _globalise_labels(df, skeleton_arr, label_offset)
        skeleton_frames.append(arr)
        if not df.empty:
            paths_tables.append(df)
            n_skeletons_total += int(df["skeleton-id"].nunique())

        # A4: run the anisotropy pass for EVERY timepoint, including ones whose skeleton came back
        # empty. Skipping empties (as this used to, by sitting after a `continue`) made the stacked
        # leading axis "index among non-empty frames" rather than `t`, so any consumer indexing
        # orientation_eigvec[t] silently read the wrong frame. An empty frame yields a zero field, which
        # is the correct answer for it, and `aniso_t_index` records the mapping either way.
        if calc_anisotropy:
            fibre_im = (_extract_fibre_image(im_data, dim_utils, fibre_channels, t_index)
                        if im_data is not None else None)
            if fibre_im is not None and integrate_time and dim_utils.is_timeseries():
                # t_index is None here (the stack was collapsed), so _extract_fibre_image returned
                # every timepoint — reduce it the way the user asked.
                fibre_im = _collapse_time(fibre_im, 0, integrate_time_mode)
            aniso_im = _anisotropy_input(aniso_source, fibre_im, labels_slice, skeleton_bool)
            sk_bool = skeleton_bool
            if calc_flattened and sk_bool.ndim == 3:
                sk_bool = np.max(sk_bool, axis=0)
            # A3: reconcile ranks — the fibre channel can still be 3D when the labels were Z-MIPed.
            aniso_im = _match_rank(aniso_im, sk_bool.ndim)

            coor, ev, evec, coh = aniso.structure_tensor_field(aniso_im, st_sigma, aniso_box_size)
            blen = aniso.box_lengths(sk_bool, aniso_box_size)
            aniso_coor.append(coor); ev_frames.append(ev); evec_frames.append(evec)
            aniso_box_len.append(blen); aniso_box_aniso.append(coh)
            # -1 marks a T-collapsed frame, so a reader can tell it from real frame 0.
            aniso_t_index.append(int(t_index) if t_index is not None else (-1 if integrate_time else 0))
            pxsz = float(dim_utils.im_physical_size("x"))
            summary_frames.append(_scalar_summary(
                aniso_im, sk_bool, pxsz, aniso.weighted_anisotropy(coh, blen)))

        # tick at the END of the timepoint so anisotropy's cost (when on) rolls into this frame's
        # tick, not the next one. For a static image t_index is None → single 1/1 tick.
        log.progress((t_index if t_index is not None else 0) + 1, total_ticks)

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
    # `create_multiscales`' numpy branch needs `im_chunks` — otherwise `create_zarr_from_ndarray`
    # calls `chunks(None)` and TypeErrors. Chunk against the IMAGE shape (with C) because
    # `ignore_channel=True` pops the C entry INSIDE create_zarr_from_ndarray — passing label-sized
    # chunks would then pop the wrong axis. plane_chunks: 1 along non-spatial, 512-capped on Y/X.
    # A8: describe the store by the axes it ACTUALLY has. The label array never has C, and may have
    # lost T (integrateTime) or Z (flattenBranching). Deriving both the axes and the chunks from the
    # source image instead wrote `t,c,z,y,x` with scale [1, 1, 3.0, 0.596, 0.596] over a 3-axis
    # (T, Y, X) array — a positional reader gave Y the Z step, a 5× stretch — and the chunk vector
    # was rescued only by `create_zarr_from_ndarray`'s `pop(0)` rank fallback, which drops a LEADING
    # entry and so misassigns the rest. Compute both here, exactly, and let neither be guessed:
    # `ignore_channel=False` because these chunks already have no C to pop.
    store_axes = _store_axes(dim_utils.im_dim_order, has_time, is_3d)
    store_chunks = zarr_utils.plane_chunks(stacked.shape)   # store_axes always end …Y, X
    # Staged: the store lands on its final path only once complete, so cancelling this task can't
    # leave a registered branch-label set truncated.
    # See docs/SEGMENTATION.md → *Stores are written staged, never in place*.
    with zarr_utils.staged_store(branch_labels_out) as staging:
        zarr_utils.create_multiscales(
            stacked, staging,
            dim_utils=dim_utils,
            axes=store_axes,
            im_chunks=store_chunks,
            nscales=1,
            # `datasets` (the default) is what `zarr_data_to_list` reads. `keyword='labels'` was for
            # a legacy R store layout only; using it here writes a store no cecelia reader can open.
            ignore_channel=False,
            squeeze=False,
        )

    aniso_uns = None
    image_anisotropy = 0.0
    anisotropy_series = []
    if calc_anisotropy and aniso_coor:
        # Stack per-timepoint arrays along a leading T axis. `orientation_meta["t_index"]` says which
        # timepoint each slot IS — don't infer it from the position (A4).
        aniso_uns = {
            "orientation_coords":        np.stack(aniso_coor, axis=0),
            "orientation_eigval":        np.stack(ev_frames, axis=0),
            "orientation_eigvec":        np.stack(evec_frames, axis=0),
            "orientation_box_length":    np.stack(aniso_box_len, axis=0),
            "orientation_box_coherence": np.stack(aniso_box_aniso, axis=0),
            "orientation_summary":       pd.concat(summary_frames, axis=0, ignore_index=True)
                                             .astype(np.float32),
            # Self-describing block: everything a reader needs to interpret the arrays above
            # without reading this file. The eigen layout keys exist because the arrays are
            # shape-alike but NOT index-compatible with the old ILEE outputs (A1).
            "orientation_meta": {
                "box_size_px":     int(aniso_box_size),
                "sigma_px":        float(st_sigma),
                # what the user actually set, and the factor it was converted with. A reader
                # comparing images across a cohort wants the um, not this image's pixels.
                "box_size_um":     float(box_um) if box_um is not None else float("nan"),
                "sigma_um":        float(sigma_um) if sigma_um is not None else float("nan"),
                "um_per_px":       float(um_per_px) if um_per_px is not None else float("nan"),
                "source":          aniso_source,
                "flattened":       bool(calc_flattened or flatten_branching),
                "t_index":         np.asarray(aniso_t_index, dtype=np.int32),
                "scale_um_per_px": np.asarray(_spatial_scale(dim_utils, aniso_coor[0].shape[-1]),
                                              dtype=np.float32),
                "eigvec_layout":   "vec_major",   # eigvec[..., i, :] is the i-th eigenvector
                "eigval_order":    "ascending",
                "fibre_direction": "minor",       # cecelia.utils.anisotropy_utils.fibre_orientation
            },
        }
        # The per-image readout: median over frames of the length-weighted per-frame anisotropy.
        # Median, not mean — one bad frame shouldn't move the image's number.
        per_frame = pd.concat(summary_frames, axis=0, ignore_index=True)["anisotropy"].to_numpy()
        image_anisotropy = float(np.median(per_frame))
        # …and the whole series, so the plot layer can show a real distribution for a single image
        # rather than a box drawn around one number (SPATIAL_ANISOTROPY_PLAN Decision 8).
        anisotropy_series = [float(v) for v in per_frame]

    # Per-branch anisotropy: each branch takes the coherence of its grid box (see
    # `_per_branch_anisotropy`). Only when the pass ran AND the grid matches this run's frames.
    branch_obs = {}
    if calc_anisotropy and aniso_box_aniso:
        branch_obs["anisotropy"] = _per_branch_anisotropy(
            paths_df, aniso_box_aniso, aniso_t_index, aniso_box_size, 3 if is_3d else 2)
        n_ok = int(np.isfinite(branch_obs["anisotropy"]).sum())
        log.log(f"> per-branch anisotropy on {n_ok}/{len(paths_df)} branch(es)")

    log.log(f"> write branch props {branch_props_out}")
    # `has_time`, NOT `has_time and not flatten_branching`. `flattenBranching` is a **Z** operation
    # — it does not collapse time, and `_iterate_timepoints` still produced one skeleton per frame.
    # Conflating the two silently dropped obsm['temporal'] from every Z-flattened timeseries, so
    # 66k branches over 201 frames arrived with no way to tell which frame each came from — which
    # is exactly the standard intravital case (SPATIAL_ANISOTROPY_PLAN A7). When `integrateTime`
    # lands it is THAT flag, not this one, that suppresses the temporal axis.
    _write_branch_h5ad(paths_df, is_3d, has_time, branch_props_out, aniso_uns=aniso_uns,
                       obs_cols=branch_obs)

    mean_branch_length = (
        float(paths_df["branch-distance"].mean()) if "branch-distance" in paths_df.columns and len(paths_df)
        else 0.0
    )
    # Unique branch-type codes present in the output → Julia auto-creates one filter pop per code
    # via ensure_filter_pop! (BRANCHING_PLAN Decision 3). Sort so pop-map order is stable across runs.
    branch_types = sorted({int(v) for v in paths_df["branch-type"].unique()}) \
        if "branch-type" in paths_df.columns and len(paths_df) else []

    # one write idiom per file: the per-run QC handoff uses the same helper as the .h5ad above
    write_json_atomic(qc_out_path, {
        "nBranches": int(len(paths_df)),
        "nSkeletons": int(n_skeletons_total),
        "meanBranchLength": mean_branch_length,
        "branchTypes": branch_types,
        # The per-image structure readout. 0.0 when the anisotropy pass didn't run — Julia only
        # banks the metric when calcAnisotropy was on, so a zero never reaches the cohort stats.
        "anisotropy": image_anisotropy,
        # One value per timepoint, for the per-frame view of the same measure.
        "anisotropySeries": anisotropy_series,
        # The grid the run ACTUALLY produced, so Julia can report what the chosen spacing cost
        # in stored bytes and warn when it dominates the sidecar. Reported, not estimated:
        # clamping and integer box rounding both move the real number away from the request.
        "anisoBoxes": int(np.prod(aniso_coor[0].shape[:-1])) if aniso_coor else 0,
        "anisoFrames": len(aniso_coor),
    })


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] No params file provided (--params missing or not found)", flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == "__main__":
    main()
