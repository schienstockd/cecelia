"""
Ridge segmentation runner.

Wraps `skimage.filters.{meijering, sato, frangi}` — three Hessian-based ridge filters —
followed by a threshold and connected-component labelling. Runs on ONE channel of one image
version, ONE T frame at a time: `zarr_utils.read_timepoint` for the read,
`zarr_utils.open_multiscales_for_writing` for the write. Peak RAM stays at one frame + its
label output rather than the whole T×Z×Y×X label movie (`docs/todo/ZARR_STREAMING_PLAN.md`
Decision 1). Never hand-roll `level[slice]` or accumulate frames in a list here.

Filter rankings measured on Unimelb 3P SHG (EaMaVq Z=9 t=10):
  meijering  d=3.87  ratio=6.39   ← default: best fibre/background per pixel
  sato       d=4.03  ratio=5.22   ← ties meijering; slightly more background haze
  frangi     d=2.06  ratio=13.9   ← sparse but very clean; misses side branches

Not learned (no coastal, no cellpose): coastal's flow-warp supervision is a noise-driven signal
on SHG, so a classical Hessian filter is the honest choice here.

Parameter contract (JSON written by Julia):
  imPath          - source image path
  labelsOutPath   - output labels zarr path (staged; final path lands atomically at end)
  qcOutPath       - JSON path for QC counters (nLabels)
  channelIndex    - 0-based channel index (Julia resolved the name)
  filter          - "meijering" | "sato" | "frangi"
  sigmaMin        - int; smallest sigma passed to the filter (px)
  sigmaMax        - int; largest sigma (px), inclusive
  threshold       - float in [0, 1]; ridge-response cutoff. 0 = Otsu (auto)
  darkRidges      - bool; True for dark-line-on-bright, False for bright-line-on-dark
  perZ            - bool; True runs the 2D filter per Z, False runs on a Z-MIP first
  minSizePx       - int; drop connected components smaller than this (2D area). 0 = keep all
  outputValueName - label set name (registry key in ccid.json)
"""
from __future__ import annotations

import os

import numpy as np
from skimage.filters import frangi, meijering, sato, threshold_otsu
from skimage.measure import label as cc_label
from skimage.morphology import remove_small_objects

from cecelia.utils.atomic_io import write_json_atomic
import cecelia.utils.ome_xml_utils as ome_xml_utils
import cecelia.utils.script_utils as script_utils
import cecelia.utils.zarr_utils as zarr_utils
from cecelia.utils.dim_utils import DimUtils


_FILTERS = {"meijering": meijering, "sato": sato, "frangi": frangi}


def _apply_filter(image_2d: np.ndarray, filt_name: str,
                  sigmas: range, dark_ridges: bool) -> np.ndarray:
    """Run one of the three skimage Hessian ridge filters. Returns a float32 response in
    the filter's native range (0..1 for meijering/frangi; unnormalised for sato)."""
    fn = _FILTERS[filt_name]
    return fn(image_2d.astype(np.float32), sigmas=sigmas, black_ridges=dark_ridges).astype(np.float32)


def _binarise(response: np.ndarray, threshold: float) -> np.ndarray:
    """threshold=0 → Otsu on the non-zero response; else absolute cutoff."""
    if threshold > 0:
        return response > threshold
    nz = response[response > 0]
    if nz.size < 32:                     # frame with no signal: Otsu is ill-defined
        return np.zeros_like(response, dtype=bool)
    return response > threshold_otsu(nz)


def _postprocess_labels(mask_2d: np.ndarray, min_size_px: int) -> np.ndarray:
    """Drop small components → label. 2D CC because ridges are 2D structures per plane."""
    if min_size_px > 0:
        mask_2d = remove_small_objects(mask_2d, min_size=int(min_size_px))
    lab, _ = cc_label(mask_2d, connectivity=2, return_num=True)
    return lab.astype(np.uint32)


def _binarise_stack_per_z(stack_3d: np.ndarray, filt_name: str, sigmas: range,
                          dark_ridges: bool, threshold: float) -> np.ndarray:
    """Apply the 2D ridge filter to each Z slice independently and stack the boolean masks.
    2D per-Z (not 3D directly) because the three skimage filters are configured against fibre
    thickness in the XY plane and Z spacing is usually much coarser (3 µm/px vs 0.6 µm/px in
    the reference data), so a 3D sigma would answer a different question."""
    masks = np.zeros(stack_3d.shape, dtype=bool)
    for z in range(stack_3d.shape[0]):
        resp = _apply_filter(stack_3d[z], filt_name, sigmas, dark_ridges)
        masks[z] = _binarise(resp, threshold)
    return masks


def _label_volume_3d(mask_3d: np.ndarray, min_size_px: int,
                     offset: int) -> tuple[np.ndarray, int]:
    """3D connected components with face-adjacent connectivity — a single collagen fibre spanning
    several Z planes ends up with ONE label, matching what SegmentationUtils' per-Z + IoU stitching
    produces for other segmenters. connectivity=1 (6-neighbour in 3D) rather than 3 (26-neighbour)
    because ridges are thin and 26-connectivity chains distinct fibres that merely touch at a
    corner. `remove_small_objects` in 3D drops components below `min_size_px` VOXELS."""
    if min_size_px > 0:
        mask_3d = remove_small_objects(mask_3d, min_size=int(min_size_px))
    lab, _ = cc_label(mask_3d, connectivity=1, return_num=True)
    lab = lab.astype(np.uint32)
    if offset > 0:
        lab = np.where(lab > 0, lab + offset, 0).astype(np.uint32)
    n_here = int(lab.max()) - offset if lab.size else 0
    return lab, offset + max(0, n_here)


def _label_frame_2d(frame_2d: np.ndarray, filt_name: str, sigmas: range,
                    dark_ridges: bool, threshold: float, min_size_px: int,
                    label_offset: int) -> tuple[np.ndarray, int]:
    """2D case: filter → threshold → CC → per-frame offset for cross-T uniqueness."""
    resp = _apply_filter(frame_2d, filt_name, sigmas, dark_ridges)
    mask = _binarise(resp, threshold)
    lab  = _postprocess_labels(mask, min_size_px)
    if label_offset > 0:
        lab = np.where(lab > 0, lab + label_offset, 0).astype(np.uint32)
    n_here = int(lab.max()) - label_offset if lab.size else 0
    return lab, label_offset + max(0, n_here)


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path         = params["imPath"]
    labels_out_path = params["labelsOutPath"]
    qc_out_path     = params["qcOutPath"]
    # channel_index handles the list-shape a `channelSelection` widget stores, plus a bare int and
    # a `None` default — the convention every runner is enforced against by test_script_utils.
    ch              = script_utils.channel_index(params.get("channelIndex"),
                                                 what="channelIndex",
                                                 translator="preview_params / _run_task (ridges.jl)")
    filt_name       = str(params["filter"])
    sig_min         = int(params["sigmaMin"])
    sig_max         = int(params["sigmaMax"])
    threshold       = float(params["threshold"])
    dark_ridges     = bool(params["darkRidges"])
    per_z           = bool(params["perZ"])
    min_size_px     = int(params["minSizePx"])

    if filt_name not in _FILTERS:
        raise ValueError(f"unknown filter {filt_name!r}; expected one of {sorted(_FILTERS)}")
    if sig_min <= 0 or sig_max < sig_min:
        raise ValueError(f"invalid sigma range: min={sig_min} max={sig_max}")
    sigmas = range(sig_min, sig_max + 1)

    log.log(f'> open image {im_path}')
    # Plain zarr, not dask: reads are per-timepoint slices, and dask's per-slice graph rebuild is
    # exactly what branching_run.py and every cleanup runner already found not worth paying.
    im_list, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
    level0 = im_list[0]
    omexml = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(level0.shape)

    # Axis PRESENCE, not just size>1 — dim_utils.is_3D() returns False for size_z=1, but the array
    # keeps the Z axis in `im_dim_order`, so reading has to squeeze it whether or not is_3D() says
    # so. Same for time. `n_z==1 && per_z==True` behaves the same as `per_z==False` because both
    # collapse to one 2D filter call, so the output store also drops Z.
    order = list(dim_utils.im_dim_order)
    has_time = "T" in order
    z_axis_present = "Z" in order
    n_t = int(dim_utils.dim_val("T")) if has_time else 1
    n_z = int(dim_utils.dim_val("Z")) if z_axis_present else 1
    has_z = z_axis_present and n_z > 1
    is_3d_output = has_z and per_z

    # Output label axes: drop C always, drop Z when per_z is off OR the image has only one Z slice
    # (collapsing a size-1 axis is a no-op that lets create_multiscales pick the right axis metadata).
    kept_axes = [ax for ax in order if ax != "C" and (ax != "Z" or is_3d_output)]
    ax_to_dim = {ax: idx for idx, ax in enumerate(order)}
    src_shape = level0.shape
    out_shape = tuple(src_shape[ax_to_dim[ax]] for ax in kept_axes)

    log.log(f'> filter {filt_name}   sigmas {sig_min}..{sig_max}   '
            f'{"per-Z" if is_3d_output else "Z-MIP" if has_z else "2D"}   ch={ch}')
    log.log(f'> output shape {out_shape} axes {"".join(kept_axes)}')

    label_offset = 0
    total_frames = n_t if has_time else 1
    log.progress(0, total_frames)

    # Positions inside a frame returned by `read_timepoint` (T squeezed out on the way back). The
    # kept image axes are C + (optional) Z + Y + X in their source order; C sits at the same index
    # as in `order`, or one less if T came before it.
    def _frame_pos(ax):
        idx = ax_to_dim[ax]
        return idx - 1 if (has_time and idx > ax_to_dim["T"]) else idx
    c_pos = _frame_pos("C")
    z_pos_in_frame = _frame_pos("Z") if z_axis_present else None
    t_out_pos = kept_axes.index("T") if has_time else None

    os.makedirs(os.path.dirname(labels_out_path), exist_ok=True)
    with zarr_utils.staged_store(labels_out_path) as staging:
        # Streaming label writer: allocate the empty store up front and fill one timepoint at a
        # time. Passing `axes=kept_axes` overrides the source's `im_dim_order` so the metadata
        # matches the LABEL rank (no C, no Z when Z-MIPped) — same discipline as `create_multiscales`
        # already has (finding A8: a positional scale reader gave Y the Z step). `kind='labels'`
        # picks the label compressor per CLAUDE.md.
        group, level0_out, _ = zarr_utils.open_multiscales_for_writing(
            staging, out_shape, np.uint32, dim_utils,
            axes=kept_axes,
            nscales=1,
            reference_zarr=im_path,   # inherit source zarr format (ZARR_V3_PLAN D9)
            kind='labels',
        )

        for t in range(n_t):
            # One read per frame, through the canonical helper (ZARR_STREAMING_PLAN Decision 1).
            # T is squeezed; the remaining layout keeps the source axis order (C, Z?, Y, X).
            frame = zarr_utils.read_timepoint(level0, dim_utils, t, drop_time=True)
            ch_sel = [slice(None)] * frame.ndim
            ch_sel[c_pos] = ch
            raw = frame[tuple(ch_sel)]
            if z_pos_in_frame is not None:
                # `ch_sel` collapsed the C axis, shifting later axes down by one.
                z_after_ch = z_pos_in_frame - 1 if z_pos_in_frame > c_pos else z_pos_in_frame
                raw = np.moveaxis(raw, z_after_ch, 0)          # → (Z, Y, X)
                if not has_z:                                   # Z present but size 1
                    raw = raw[0]                                # → (Y, X)

            if has_z and is_3d_output:
                # 2D ridge filter per Z, then ONE 3D connected-components pass: a fibre spanning
                # multiple Z-planes ends up with a single label, matching the effect of
                # SegmentationUtils' per-Z + IoU stitching without needing the IoU step.
                masks = _binarise_stack_per_z(raw, filt_name, sigmas, dark_ridges, threshold)
                lab, label_offset = _label_volume_3d(masks, min_size_px, label_offset)
            elif has_z:
                # perZ=False: MIP over Z first, then a single 2D filter+CC pass.
                lab, label_offset = _label_frame_2d(
                    raw.max(axis=0), filt_name, sigmas, dark_ridges, threshold,
                    min_size_px, label_offset)
            else:
                lab, label_offset = _label_frame_2d(
                    raw, filt_name, sigmas, dark_ridges, threshold,
                    min_size_px, label_offset)

            if has_time:
                wr = [slice(None)] * len(out_shape)
                wr[t_out_pos] = slice(t, t + 1)
                level0_out[tuple(wr)] = np.expand_dims(lab, axis=t_out_pos)
            else:
                level0_out[:] = lab
            log.progress(t + 1, total_frames)

        # nscales=1, so no pyramid to build — the level-0 store above is complete.

        # Ridges is per-pixel-per-frame — it doesn't move pixels, only picks the ridge ones — so
        # the source's valid box (if any) applies unchanged. `carry_valid_box` refuses when the
        # boxed axes don't match (per-Z=False collapses Z), which is the right behaviour.
        # Enforced by test_valid_box_propagation.
        if zarr_utils.carry_valid_box(im_path, staging):
            log.log('   carried the image valid box onto the ridge labels')

    log.log(f'> {label_offset} label component(s) total')
    write_json_atomic(qc_out_path, {"nLabels": int(label_offset)})


def main():
    params = script_utils.script_params()
    if params is None:
        print("[ERROR] No params file provided (--params missing or not found)", flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == "__main__":
    main()
