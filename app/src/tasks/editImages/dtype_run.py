"""
Bit-depth (dtype) conversion task.

Reads an OME-ZARR image and writes it back at a chosen target dtype (uint8 / uint16 / float32),
optionally with per-channel min–max rescaling into the target's range, streaming one timepoint at a
time. Dims are preserved; the Julia handler registers the output as a NEW VERSION on the same image.

Parameter contract (JSON written by Julia):
  imPath   - absolute path to the source .ome.zarr
  imOutPath- absolute path to write the converted .ome.zarr
  dtype    - one of 'uint8' | 'uint16' | 'float32'
  rescale  - 'auto' (per-channel min-max stretched to the target's full range)
             or 'none' (direct cast; may saturate)
"""

import numpy as np

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils


_TARGETS = {
    'uint8':   (np.uint8,   0, 255),
    'uint16':  (np.uint16,  0, 65535),
    'float32': (np.float32, 0.0, 1.0),   # convention: auto-rescale to [0, 1] for float32
}


def _per_channel_extents(level_in, dim_utils, t_idx, c_idx, n_t, on_tick):
    """Per-channel (lo, hi) over the whole image, computed in one pass by folding per-timepoint
    reductions into a running accumulator. `c_idx` is the source-layout channel index; the axis
    position INSIDE a frame (T squeezed) is derived here. `on_tick(t)` bumps progress per frame."""
    n_c = level_in.shape[c_idx]
    lo = np.full(n_c, np.inf,  dtype=np.float64)
    hi = np.full(n_c, -np.inf, dtype=np.float64)
    c_pos = c_idx - 1 if (t_idx is not None and c_idx > t_idx) else c_idx
    for t in range(n_t):
        frame = zarr_utils.read_timepoint(level_in, dim_utils, t)   # T squeezed
        # Reduce every axis EXCEPT the channel axis, per timepoint. That gives n_c mins and maxes.
        reduce_axes = tuple(i for i in range(frame.ndim) if i != c_pos)
        fmin = frame.min(axis=reduce_axes)   # shape (n_c,)
        fmax = frame.max(axis=reduce_axes)
        np.minimum(lo, fmin, out=lo)
        np.maximum(hi, fmax, out=hi)
        on_tick(t)
    return lo, hi


def _rescale_frame(frame, c_pos, lo, hi, np_dtype, out_min, out_max):
    """Per-channel linear stretch of `frame` from each channel's [lo, hi] to [out_min, out_max],
    cast to np_dtype and clipped. Broadcast via `reshape(-1)` at `c_pos`, so the same code path
    handles float and integer targets."""
    span = np.where(hi > lo, hi - lo, 1.0).astype(np.float64)
    scale = (out_max - out_min) / span                        # (n_c,)
    # Reshape lo/scale so they broadcast against the channel axis.
    view_shape = [1] * frame.ndim
    view_shape[c_pos] = frame.shape[c_pos]
    lo_b    = lo.reshape(view_shape)
    scale_b = scale.reshape(view_shape)
    scaled  = (frame.astype(np.float64) - lo_b) * scale_b + out_min
    np.clip(scaled, out_min, out_max, out=scaled)
    return scaled.astype(np_dtype, copy=False)


def _cast_frame(frame, np_dtype, out_min, out_max):
    """`rescale='none'`: clip then cast. For a float target the clip range is [0, 1] by convention
    (matching auto), so `none` on a float32 target casts + clips to [0, 1] — a legible fallback."""
    if np.issubdtype(np_dtype, np.floating):
        clipped = np.clip(frame.astype(np.float32), out_min, out_max)
    else:
        clipped = np.clip(frame, out_min, out_max)
    return clipped.astype(np_dtype, copy=False)


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    im_out_path = params['imOutPath']
    dtype_key   = params['dtype']
    rescale     = params['rescale']
    if dtype_key not in _TARGETS:
        raise ValueError(f'unknown dtype: {dtype_key} (want one of {sorted(_TARGETS)})')
    if rescale not in ('auto', 'none'):
        raise ValueError(f'unknown rescale mode: {rescale} (want "auto" or "none")')
    np_dtype, out_min, out_max = _TARGETS[dtype_key]

    log.log(f'>> open image: {im_path}')
    # Plain zarr, not dask — the transform is per-frame; the only cross-frame state is the auto
    # per-channel (min, max) reduction, which is a one-pass streaming fold. See
    # docs/todo/ZARR_STREAMING_PLAN.md decision 2.
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
    level_in  = im_dat[0]

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(level_in.shape)
    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')

    c_idx = dim_utils.dim_idx('C')
    t_idx = dim_utils.dim_idx('T') if 'T' in dim_utils.im_dim_order else None
    shape = tuple(level_in.shape)
    n_t   = shape[t_idx] if t_idx is not None else 1

    # Two passes when auto-rescaling (extents + write), one pass when not.
    pre_ticks   = n_t if rescale == 'auto' else 0
    total       = pre_ticks + n_t + 1
    done        = 0
    log.progress(done, total)

    lo = hi = None
    if rescale == 'auto':
        log.log('>> pass 1: per-channel extents')
        if c_idx is not None:
            def _tick(_t):
                nonlocal done
                done += 1
                log.progress(done, total)
            lo, hi = _per_channel_extents(level_in, dim_utils, t_idx, c_idx, n_t, _tick)
        else:
            # no channel axis — one scalar min/max over the whole array, still streamed per frame
            lo_s, hi_s = np.inf, -np.inf
            for t in range(n_t):
                frame = zarr_utils.read_timepoint(level_in, dim_utils, t)
                lo_s = min(lo_s, float(frame.min())); hi_s = max(hi_s, float(frame.max()))
                done += 1
                log.progress(done, total)
            lo = np.array([lo_s], dtype=np.float64)
            hi = np.array([hi_s], dtype=np.float64)

    log.log(f'>> convert dtype={dtype_key} rescale={rescale}: {level_in.dtype} -> {np_dtype}')
    log.log(f'>> write converted image: {im_out_path}')

    with zarr_utils.staged_store(im_out_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, shape, np_dtype, dim_utils, nscales=len(im_dat),
            reference_zarr=im_path)   # inherit source zarr format (ZARR_V3_PLAN D9)

        c_pos = None
        if c_idx is not None:
            c_pos = c_idx - 1 if (t_idx is not None and c_idx > t_idx) else c_idx

        for t in range(n_t):
            frame = zarr_utils.read_timepoint(level_in, dim_utils, t)   # T squeezed
            if rescale == 'auto':
                if c_idx is not None:
                    out_frame = _rescale_frame(frame, c_pos, lo, hi, np_dtype, out_min, out_max)
                else:
                    # single-channel-less rescale — expand the scalar extents to broadcast trivially
                    out_frame = _rescale_frame(frame[np.newaxis, ...], 0, lo, hi,
                                               np_dtype, out_min, out_max)[0]
            else:
                out_frame = _cast_frame(frame, np_dtype, out_min, out_max)

            out_sl = [slice(None)] * len(shape)
            if t_idx is not None:
                out_sl[t_idx] = slice(t, t + 1)
                level0[tuple(out_sl)] = np.expand_dims(out_frame, axis=t_idx)
            else:
                level0[tuple(out_sl)] = out_frame
            done += 1
            log.progress(done, total)

        log.log('>> build pyramid + save')
        zarr_utils.write_multiscale_pyramid(group, level0, dim_utils, len(im_dat), list(pchunks))
        # shape unchanged — no changed_shape override needed
        ome_xml_utils.save_meta_in_zarr(staging, im_path, dim_utils=dim_utils)
        zarr_utils.write_calibration(staging, dim_utils)

        # dtype is a per-pixel value transform — pixel COORDINATES are unchanged, so the source's
        # valid box still describes the same data region and carries verbatim.
        if zarr_utils.carry_valid_box(im_path, staging):
            log.log('>> carried valid box from source')

    done += 1
    log.progress(done, total)
    log.log('>> done')


def main():
    params = script_utils.script_params()
    if params is None:
        print('[ERROR] No params file provided (--params missing or not found)', flush=True)
        raise SystemExit(1)
    run(params)


if __name__ == '__main__':
    main()
