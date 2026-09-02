"""
T-projection task.

Reads an OME-ZARR image, collapses the T axis with an ImageJ-style statistic (max/mean/median/sum/
min/std) — all channels and Z-planes kept, one frame out — and writes the result as a NEW OME-ZARR
multiscale store with SizeT = 1. The Julia handler registers the output as a new image in the set.

Parameter contract (JSON written by Julia):
  imPath   - absolute path to the source .ome.zarr
  imOutPath- absolute path to write the projected .ome.zarr
  op       - reduction across T: max | mean | median | sum | min | std
"""

import numpy as np

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils


_OPS = ('max', 'mean', 'median', 'min', 'sum', 'std')

# Ops that can be built with a running accumulator, ONE frame in memory at a time. `median` is not
# associative and needs every frame at once — handled separately by iterating over (c, z) planes.
_RUNNING_OPS = ('max', 'mean', 'min', 'sum', 'std')


def _accumulate(op, running, frame_f64, n):
    """Fold `frame_f64` into a running accumulator for op. `n` is the count so far AFTER this frame.
    `running` is a dict; the update is in-place. `frame_f64` is the source frame cast to float64 so
    a sum/mean over many integer frames cannot overflow. Same shape rules as ImageJ Z-Project's
    T-axis defaults — the caller casts back to source dtype at the end."""
    if op == 'max':
        running['acc'] = frame_f64 if running.get('acc') is None else np.maximum(running['acc'], frame_f64)
    elif op == 'min':
        running['acc'] = frame_f64 if running.get('acc') is None else np.minimum(running['acc'], frame_f64)
    elif op == 'sum':
        running['acc'] = frame_f64 if running.get('acc') is None else (running['acc'] + frame_f64)
    elif op == 'mean':
        running['acc'] = frame_f64 if running.get('acc') is None else (running['acc'] + frame_f64)
    elif op == 'std':
        # Welford's online variance — keeps running mean + M2 (sum of squared deltas from the mean),
        # so std at the end is sqrt(M2/(n-1)). Numerically stable and single-pass, unlike
        # `sqrt(mean(x^2) - mean(x)^2)` which loses precision when the variance is small vs the mean.
        if running.get('mean') is None:
            running['mean'] = np.zeros_like(frame_f64)
            running['m2']   = np.zeros_like(frame_f64)
        delta = frame_f64 - running['mean']
        running['mean'] += delta / n
        running['m2']   += delta * (frame_f64 - running['mean'])


def _finalise(op, running, n_t, out_dtype):
    if op == 'mean':
        return (running['acc'] / n_t).astype(out_dtype, copy=False)
    if op == 'std':
        # ddof=1 (sample std) matches numpy's default for consistency with the pre-streaming version
        # (`da.std`, which delegates to numpy). Requires n>=2; a single-frame source raises before us.
        var = running['m2'] / max(1, n_t - 1)
        return np.sqrt(var).astype(out_dtype, copy=False)
    return running['acc'].astype(out_dtype, copy=False)


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path     = params['imPath']
    im_out_path = params['imOutPath']
    op          = params['op']
    if op not in _OPS:
        raise ValueError(f'unknown op: {op} (want one of {sorted(_OPS)})')

    log.log(f'>> open image: {im_path}')
    # Plain zarr, not dask (docs/todo/ZARR_STREAMING_PLAN.md decision 2). Reading proceeds per
    # timepoint for the associative ops and per (c, z) plane for median — memory is bounded either
    # way, without any dask graph.
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
    level_in  = im_dat[0]

    omexml    = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(level_in.shape)
    log.log(f'>> image dims: {dim_utils.im_dim_order} {dim_utils.im_dim}')

    t_idx = dim_utils.dim_idx('T')
    if t_idx is None:
        # Task JSON `requires: {axes: ["T"]}` blocks this at the module-page gate, but a REPL or
        # chain caller may still reach the runner. Refuse rather than write a copy identical to
        # the source under a "t-<op>" name.
        raise ValueError('image has no T axis to project')

    src_shape = tuple(level_in.shape)
    n_t = src_shape[t_idx]
    out_shape = list(src_shape); out_shape[t_idx] = 1
    out_shape = tuple(out_shape)
    dim_utils.im_dim[t_idx] = 1

    log.log(f'>> project op={op} axis=T(idx={t_idx}) {src_shape} -> {out_shape}')
    log.log(f'>> write projected image: {im_out_path}')

    total = n_t + 1
    done = 0
    log.progress(done, total)

    # Position of Z in a squeezed frame (T dropped); used only by the median path.
    z_idx = dim_utils.dim_idx('Z') if 'Z' in dim_utils.im_dim_order else None
    c_idx = dim_utils.dim_idx('C') if 'C' in dim_utils.im_dim_order else None

    with zarr_utils.staged_store(im_out_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, out_shape, level_in.dtype, dim_utils, nscales=len(im_dat),
            reference_zarr=im_path)   # inherit source zarr format (ZARR_V3_PLAN D9)

        if op in _RUNNING_OPS:
            # Running accumulator, ONE frame in memory at a time. sum/mean/std run in float64 so a
            # long integer time-series can't overflow; cast back at the end.
            running = {}
            for t in range(n_t):
                frame = zarr_utils.read_timepoint(level_in, dim_utils, t)   # T squeezed
                _accumulate(op, running, frame.astype(np.float64, copy=False), t + 1)
                done += 1
                log.progress(done, total)
            projected = _finalise(op, running, n_t, level_in.dtype)
            level0[...] = np.expand_dims(projected, axis=t_idx)
        else:
            # median: not associative, so no running accumulator. Iterate over (c, z) planes and
            # buffer only that plane's T×Y×X into RAM at once — the SAME per-plane peak the running
            # ops have, so median is not a memory outlier.
            frame_shape = list(src_shape)
            # frame axes minus T
            frame_axes = [i for i in range(len(src_shape)) if i != t_idx]
            def _read_ct_z_all_t(c, z):
                """Assemble the (T, Y, X)-with-C-and-Z-squeezed stack for one (c, z) plane."""
                sl = [slice(None)] * len(src_shape)
                if c_idx is not None and c is not None:
                    sl[c_idx] = c
                if z_idx is not None and z is not None:
                    sl[z_idx] = z
                # Bring T to axis 0 by keeping T's slice(None) and moving that axis first via np.moveaxis
                arr = np.asarray(level_in[tuple(sl)])
                # `sl` used integer indexers for c/z (dropping those axes) and slice(None) for T/Y/X:
                # arr shape is (…, T=n_t, Y, X, …) minus the C/Z axes, with the T axis at the original
                # position adjusted for the dropped axes.
                dropped_before_t = sum(1 for i in (c_idx, z_idx) if i is not None and i < t_idx)
                arr_t_axis = t_idx - dropped_before_t
                return np.moveaxis(arr, arr_t_axis, 0)   # (T, remaining...)

            nc = src_shape[c_idx] if c_idx is not None else 1
            nz = src_shape[z_idx] if z_idx is not None else 1
            # Reuse `total` for the same progress feel — bump per plane instead of per t.
            total = nc * nz + 1
            done = 0
            log.progress(done, total)
            # Prepare an output frame with the source layout minus T (matches level0's slice-at-T=0).
            out_frame_shape = tuple(s for i, s in enumerate(src_shape) if i != t_idx)
            out_frame = np.empty(out_frame_shape, dtype=level_in.dtype)
            for c in range(nc):
                for z in range(nz):
                    stack = _read_ct_z_all_t(c if c_idx is not None else None,
                                             z if z_idx is not None else None)
                    med = np.median(stack, axis=0).astype(level_in.dtype, copy=False)
                    # Place `med` back into out_frame at the right (c, z) slot.
                    out_sl = [slice(None)] * len(out_frame_shape)
                    # rebuild the frame-axis position of C and Z after dropping T
                    if c_idx is not None:
                        c_pos = c_idx - (1 if t_idx < c_idx else 0)
                        out_sl[c_pos] = c
                    if z_idx is not None:
                        z_pos = z_idx - (1 if t_idx < z_idx else 0)
                        out_sl[z_pos] = z
                    out_frame[tuple(out_sl)] = med
                    done += 1
                    log.progress(done, total)
            level0[...] = np.expand_dims(out_frame, axis=t_idx)

        log.log('>> build pyramid + save')
        zarr_utils.write_multiscale_pyramid(group, level0, dim_utils, len(im_dat), list(pchunks))
        ome_xml_utils.save_meta_in_zarr(
            staging, im_path,
            changed_shape=out_shape,   # SizeT → 1; SizeX/Y/Z + PhysicalSize* carry over
            dim_utils=dim_utils,
        )
        zarr_utils.write_calibration(staging, dim_utils)

        # VALID-BOX-EXEMPT: T is COLLAPSED to a single frame. A per-frame box is keyed by frame
        # index and would have no frames to key on; the frame-wise union would over-report validity.
        # No box means "all valid" — a consumer skips nothing and is merely slower, never wrong.

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
