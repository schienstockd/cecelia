"""
Flow-based per-pixel registration task.

For each z-plane, compute dense Farneback optical flow between the reference
channel of consecutive timepoints, then warp EVERY channel at that (z, t) by
the same flow — cross-channel-consistent per-pixel deformation correction.

Sits between `stackAlign` (per-plane rigid) and `smooth` (temporal denoise) in
the cleanup pipeline. Fixes non-rigid within-frame deformation (a moving
sample being captured row-by-row during a resonant/galvo scan) that neither
rigid frame alignment nor smoothing can undo.

Ref-channel flow is applied to all channels at the same (z, t) — matches
stackAlign's "reference channel drives, others follow" pattern. Runs at
~0.15s per (t, z) so a typical (126 T × 6 Z) volume completes in ~2 minutes.

Parameter contract (JSON written by Julia):
  imPath          - absolute path to input .ome.zarr
  imOutPath       - absolute path to write flow-registered .ome.zarr
  registerChannel - int, 0-based channel index used to estimate flow
  referenceMode   - "previous" | "first"
  winsize         - int, Farneback averaging window
  pyrLevels       - int, Farneback pyramid levels
  maxShiftPx      - float, per-pixel flow-magnitude clamp
"""

import cv2
import numpy as np

# `cecelia.*` resolves via PYTHONPATH=python/, set by the Julia launcher.
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import write_json_atomic


_FARNEBACK_FIXED = dict(pyr_scale=0.5, iterations=3, poly_n=5, poly_sigma=1.2,
                        flags=cv2.OPTFLOW_FARNEBACK_GAUSSIAN)


def _flow(prev, curr, winsize, pyr_levels):
    return cv2.calcOpticalFlowFarneback(
        np.asarray(prev, dtype=np.float32),
        np.asarray(curr, dtype=np.float32),
        None, _FARNEBACK_FIXED['pyr_scale'], pyr_levels, winsize,
        _FARNEBACK_FIXED['iterations'], _FARNEBACK_FIXED['poly_n'],
        _FARNEBACK_FIXED['poly_sigma'], _FARNEBACK_FIXED['flags'],
    )


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path         = params['imPath']
    im_out_path     = params['imOutPath']
    register_channel = script_utils.channel_index(
        params.get('registerChannel'), 'registerChannel', 'flow_register.jl')
    reference_mode  = params.get('referenceMode', 'previous')
    winsize         = int(params.get('winsize', 17))
    pyr_levels      = int(params.get('pyrLevels', 5))
    max_shift_px    = float(params.get('maxShiftPx', 16.0))

    if reference_mode not in ('previous', 'first'):
        raise ValueError(
            f'referenceMode must be "previous" or "first", got {reference_mode!r}')

    log.log(f'>> open image: {im_path}')
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(im_dat[0].shape)

    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')
    log.log(f'>> reference channel: {register_channel}, mode: {reference_mode}, '
            f'winsize={winsize}, pyr_levels={pyr_levels}, max_shift={max_shift_px} px')

    n_t = dim_utils.dim_val('T')
    n_c = dim_utils.dim_val('C')
    n_z = dim_utils.dim_val('Z')
    H   = dim_utils.dim_val('Y')
    W   = dim_utils.dim_val('X')

    # Progress: n_t frames + 1 metadata step.
    total = n_t + 1
    log.progress(0, total)

    out_shape = im_dat[0].shape
    out_dtype = im_dat[0].dtype

    # Per-frame flow diagnostics (for the QC sidecar).
    flow_max  = np.zeros(n_t, dtype=np.float32)
    flow_mean = np.zeros(n_t, dtype=np.float32)

    yy, xx = np.mgrid[0:H, 0:W].astype(np.float32)

    # Dim indices to slice into (T, C, Z, Y, X) arrays generically. All the
    # tasks in this dir have used TCZYX ordering; guard so we notice if it
    # ever differs.
    if ''.join(dim_utils.im_dim_order) != 'TCZYX':
        raise ValueError(
            f'flow_register expects TCZYX; got {"".join(dim_utils.im_dim_order)}')

    with zarr_utils.staged_store(im_out_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, out_shape, out_dtype, dim_utils, nscales=len(im_dat),
            reference_zarr=im_path)

        # First frame is identity by construction.
        level0[0, ...] = im_dat[0][0, ...]
        log.progress(1, total)

        for t in range(1, n_t):
            frame_max = 0.0
            frame_mean_accum = 0.0
            for z in range(n_z):
                if reference_mode == 'first':
                    ref_frame = np.asarray(
                        im_dat[0][0, register_channel, z], dtype=np.float32)
                else:
                    ref_frame = np.asarray(
                        im_dat[0][t - 1, register_channel, z], dtype=np.float32)
                mov_frame = np.asarray(
                    im_dat[0][t, register_channel, z], dtype=np.float32)

                flow = _flow(ref_frame, mov_frame, winsize, pyr_levels)
                mag = np.sqrt(flow[..., 0] ** 2 + flow[..., 1] ** 2)
                frame_max = max(frame_max, float(mag.max()))
                frame_mean_accum += float(mag.mean())

                map_x = xx + flow[..., 0]
                map_y = yy + flow[..., 1]
                over = mag > max_shift_px

                for c in range(n_c):
                    mov = np.asarray(im_dat[0][t, c, z], dtype=np.float32)
                    warped = cv2.remap(
                        mov, map_x, map_y,
                        interpolation=cv2.INTER_LINEAR,
                        borderMode=cv2.BORDER_REPLICATE,
                    )
                    if over.any():
                        warped = np.where(over, mov, warped)
                    level0[t, c, z] = warped.astype(out_dtype, copy=False)

            flow_max[t]  = frame_max
            flow_mean[t] = frame_mean_accum / n_z
            log.progress(1 + t, total)

        log.log(f'>> build pyramid + save: {im_out_path}')
        zarr_utils.write_multiscale_pyramid(group, level0, dim_utils, len(im_dat), list(pchunks))

        log.log('>> save OME-XML metadata')
        ome_xml_utils.save_meta_in_zarr(
            staging, im_path,
            changed_shape=out_shape,
            dim_utils=dim_utils,
        )
        zarr_utils.write_calibration(staging, dim_utils)

        # Pass through the source's valid box: flow-register pulls pixels
        # laterally by at most `maxShiftPx` (default 16 px), negligible against
        # a 512+ px frame and much smaller than the drift-canvas expansion the
        # upstream `driftCorrect` recomputes anyway.
        if zarr_utils.carry_valid_box(im_path, staging):
            log.log(f'>> carried valid box from {im_path}')

    # QC sidecar: per-frame flow diagnostics + params echo.
    qc_out_path = params.get('qcOutPath')
    if qc_out_path:
        doc = {
            'dimOrder':      ''.join(dim_utils.im_dim_order),
            'sourceShape':   [int(x) for x in im_dat[0].shape],
            'referenceMode': reference_mode,
            'winsize':       winsize,
            'pyrLevels':     pyr_levels,
            'maxShiftPx':    max_shift_px,
            'flowMax':       [float(x) for x in flow_max],
            'flowMean':      [float(x) for x in flow_mean],
        }
        write_json_atomic(qc_out_path, doc)
        log.log(f'>> saved flow-register QC: {qc_out_path}')

    log.progress(total, total)
    log.log('>> done')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
