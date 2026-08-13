"""Smoothing task.

Applies `coastal.smooth`'s model-free smoothing — an xy Gaussian, then a running statistic over
time — to selected channels, streaming z-plane by z-plane so peak memory is the temporal window and
not the movie. Output shape equals input shape; unselected channels are copied through.

Why it exists: on photon-limited resonance data (86-95% of voxels exactly zero) the AF task's
triangle threshold lands *inside* the signal, because the histogram has no background population to
find — measured on `zolIMa/fXgbTl`, the reference channel kept 8.6% of its signal past background
subtraction, 80% after smoothing. So this runs BETWEEN drift correction and AF. Full record:
`docs/todo/SMOOTHING_PLAN.md`, and coastal's `docs/SMOOTHING.md`.

Two invariants, both load-bearing and both measured (they live in `coastal.smooth`; this file only
has to not break them):

  * **spatial BEFORE temporal.** A temporal statistic alone keeps 8.5% of the reference channel's
    signal — worse than doing nothing (15.4%) — because at single-digit photon counts a median over
    3 mostly-zero samples is zero. The Gaussian has to fill the counts first. The rolling window
    below therefore caches *spatially smoothed* frames and takes the temporal statistic across them.
  * **one shared kernel for every channel.** The consumer is a cross-channel ratio (AF's weight is
    `b_t^p / sum_i b_i^p`), so a per-channel transform corrupts it. That extends to the dynamic-range
    gain below: ONE gain for all smoothed channels, never per-channel.

Parameter contract (JSON written by Julia):
  imPath           - absolute path to input .ome.zarr
  imOutputPath     - absolute path to write the smoothed .ome.zarr
  channels         - list of 0-based channel indices to smooth ([] = all)
  spatialSigma     - xy Gaussian sigma in px (0 disables)
  temporalFrames   - full centred window, forced odd by coastal (1 disables)
  temporalStat     - "median" | "mean"
  restoreGain      - bool, rescale so an integer store keeps usable precision
  qcOutPath        - where to persist stats for the Julia QC step
"""

import numpy as np

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import write_json_atomic

# coastal owns the smoothing engine (array-only, imports nothing from cecelia). Declared as a git
# dep in pixi.toml — see the note there for why it is no longer an editable sibling path.
from coastal.smooth import spatial_smooth, temporal_smooth

#: How many (t, z) planes to sample when estimating the dynamic-range gain. The gain only needs the
#: right order of magnitude, and a sample keeps this from being a second full pass over the store.
GAIN_SAMPLE_PLANES = 24


def _axis_len(dim_utils, letter, shape):
    idx = dim_utils.dim_idx(letter)
    return (idx, shape[idx]) if idx is not None else (None, 1)


def _plane_slice(ndim, t_idx, t, c_idx, c, z_idx, z):
    """Slice tuple selecting one (timepoint, channel, z) plane, leaving the spatial axes whole."""
    sl = [slice(None)] * ndim
    sl[t_idx] = t
    if c_idx is not None:
        sl[c_idx] = c
    if z_idx is not None:
        sl[z_idx] = z
    return tuple(sl)


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path      = params['imPath']
    out_path     = params['imOutputPath']
    channels     = [int(c) for c in (params.get('channels') or [])]
    sigma        = float(params.get('spatialSigma', 1.0))
    frames       = int(params.get('temporalFrames', 3))
    stat         = str(params.get('temporalStat', 'median'))
    restore_gain = bool(params.get('restoreGain', True))

    log.log(f'>> open image: {im_path}')
    # Plain zarr, not dask: every read is one chunk-aligned plane, so a dask graph only adds
    # overhead. Same reasoning as drift_correct_run.py (docs/todo/ZARR_STREAMING_PLAN.md).
    im_dat, _ = zarr_utils.open_as_zarr(im_path, as_dask=False)
    level_in = im_dat[0]

    omexml = ome_xml_utils.parse_meta(im_path)
    dim_utils = DimUtils(omexml, use_channel_axis=True)
    dim_utils.calc_image_dimensions(level_in.shape)

    shape = tuple(level_in.shape)
    t_idx, nt = _axis_len(dim_utils, 'T', shape)
    c_idx, nc = _axis_len(dim_utils, 'C', shape)
    z_idx, nz = _axis_len(dim_utils, 'Z', shape)
    if t_idx is None:
        log.log('[ERROR] image has no time axis — the temporal statistic needs one')
        raise SystemExit(1)

    sel = channels if channels else list(range(nc))
    others = [c for c in range(nc) if c not in sel]
    log.log(f'>> dims {dim_utils.im_dim_order} {shape}')
    log.log(f'>> smoothing channels {sel} (passing through {others}); '
            f'sigma={sigma}, frames={frames}, stat={stat}')

    half = max(0, (frames - 1) // 2) if frames and frames > 1 else 0

    # One progress scale across the whole run rather than a 4-step one, same as drift_correct_run.py:
    # the streaming loop below is the minutes-long part and the old scale stood still through all of
    # it (25% → 50% → done). One tick per z-plane — the same beat as the log's `z i/nz` lines — plus
    # the gain estimate and the pyramid. Deliberately NOT per (z, timepoint): that is `nz * nt` ticks
    # (~6.7k on a 37 z × 181 t movie), a progress line per plane for a bar that only has so many pixels.
    total = nz + 2
    done = 0
    log.progress(done, total)

    def read_plane(t, c, z):
        return np.asarray(level_in[_plane_slice(len(shape), t_idx, t, c_idx, c, z_idx, z)],
                          dtype=np.float32)

    # ── the gain ───────────────────────────────────────────────────────────────────────────────
    # Averaging lowers the maximum, so writing the result back at the input dtype throws away the
    # precision the AF background estimate needs: measured on fXgbTl, smoothed nuc-GFP has p99=15
    # and max 59, i.e. ~59 integer levels for the whole channel, and the background sits at 2.6 —
    # one integer step is 38% of it. ONE gain across all smoothed channels restores the range
    # without touching cross-channel ratios.
    gain = 1.0
    dtype_max = np.iinfo(zarr_utils.native_dtype(level_in.dtype)).max \
        if np.issubdtype(level_in.dtype, np.integer) else None
    if restore_gain and dtype_max is not None:
        log.log('>> estimate dynamic-range gain')
        rng = np.random.default_rng(0)
        picks = [(int(rng.integers(0, nt)), int(rng.integers(0, nz)))
                 for _ in range(min(GAIN_SAMPLE_PLANES, nt * nz))]
        hi_in, hi_sm = [], []
        for t, z in picks:
            for c in sel:
                raw = read_plane(t, c, z)
                sm = spatial_smooth(raw, sigma)
                hi_in.append(np.percentile(raw, 99.99))
                hi_sm.append(np.percentile(sm, 99.99))
        hi_in, hi_sm = float(np.mean(hi_in)), float(np.mean(hi_sm))
        if hi_sm > 0:
            gain = max(1.0, hi_in / hi_sm)
        log.log(f'   input p99.99 {hi_in:.1f}, smoothed {hi_sm:.1f} -> gain {gain:.2f}')

    # Counted whether or not it ran, so the scale means the same thing with restoreGain off.
    done = 1
    log.progress(done, total)

    # ── stream ─────────────────────────────────────────────────────────────────────────────────
    log.log(f'>> smooth + write (streaming per z-plane): {out_path}')
    stats = {'zeroFracIn': {}, 'zeroFracOut': {}, 'clippedVoxels': 0, 'gain': gain}
    zin = {c: [] for c in sel}
    zout = {c: [] for c in sel}
    clipped = 0

    with zarr_utils.staged_store(out_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, shape, level_in.dtype, dim_utils, nscales=len(im_dat),
            reference_zarr=im_path)   # inherit the source's zarr format (ZARR_V3_PLAN D9)

        for z in range(nz):
            # Rolling cache of SPATIALLY smoothed planes for this z, keyed by timepoint. Bounded by
            # the window, so memory is `frames` planes per channel regardless of T.
            cache = {}

            def spatial_at(t, c):
                t = min(max(t, 0), nt - 1)          # clamp = scipy's mode='nearest'
                key = (t, c)
                if key not in cache:
                    cache[key] = spatial_smooth(read_plane(t, c, z), sigma)
                return cache[key]

            for t in range(nt):
                for c in sel:
                    if half == 0:
                        out = spatial_at(t, c)
                    else:
                        # spatial FIRST (cached), then the temporal statistic across the window
                        win = np.stack([spatial_at(tt, c) for tt in range(t - half, t + half + 1)])
                        out = temporal_smooth(win, frames, stat, time_axis=0)[half]
                    raw = read_plane(t, c, z)
                    zin[c].append(float((raw == 0).mean()))

                    out = out * gain
                    if dtype_max is not None:
                        clipped += int((out > dtype_max).sum())
                        out = np.clip(np.rint(out), 0, dtype_max)
                    zout[c].append(float((out == 0).mean()))
                    level0[_plane_slice(len(shape), t_idx, t, c_idx, c, z_idx, z)] = \
                        out.astype(level0.dtype)

                for c in others:
                    level0[_plane_slice(len(shape), t_idx, t, c_idx, c, z_idx, z)] = \
                        level_in[_plane_slice(len(shape), t_idx, t, c_idx, c, z_idx, z)]

                # drop cache entries that can no longer be reached by a later window
                for key in [k for k in cache if k[0] < t - half]:
                    del cache[key]

            done += 1
            log.progress(done, total)
            if nz > 1:
                log.log(f'   z {z + 1}/{nz}')

        log.log('>> build pyramid + save')
        zarr_utils.write_multiscale_pyramid(group, level0, dim_utils, len(im_dat), list(pchunks))
        ome_xml_utils.save_meta_in_zarr(staging, im_path, changed_shape=shape, dim_utils=dim_utils)
        zarr_utils.write_calibration(staging, dim_utils)

        # Carry the source's valid box forward. Smoothing does not move pixels, but it is normally
        # run on a DRIFT-CORRECTED store whose canvas is mostly padding — losing the box here would
        # make every downstream consumer re-derive geometry this store already knows.
        #
        # Via `carry_valid_box`, NOT read+write: `read_valid_box(path)` on a per-frame box returns the
        # UNION over frames, and this used to write that back. For a window that drifts across the
        # canvas the union is nearly the whole canvas, so the box survived in name while losing
        # exactly the information that makes it useful — segmentation on a smoothed store then had
        # nothing to skip.
        if zarr_utils.carry_valid_box(im_path, staging):
            log.log('   carried the source valid box forward (per timepoint)')

    stats['zeroFracIn'] = {str(c): float(np.mean(v)) for c, v in zin.items() if v}
    stats['zeroFracOut'] = {str(c): float(np.mean(v)) for c, v in zout.items() if v}
    stats['clippedVoxels'] = clipped
    stats['channels'] = sel
    stats['spatialSigma'] = sigma
    stats['temporalFrames'] = frames
    stats['temporalStat'] = stat
    stats['shape'] = [int(x) for x in shape]
    for c in sel:
        log.log(f'   ch{c}: zero voxels {100*stats["zeroFracIn"][str(c)]:.1f}% -> '
                f'{100*stats["zeroFracOut"][str(c)]:.1f}%')
    if clipped:
        log.log(f'[WARN] gain clipped {clipped} voxels at the dtype maximum')

    qc_out_path = params.get('qcOutPath')
    if qc_out_path:
        write_json_atomic(qc_out_path, stats)
        log.log(f'>> saved QC stats: {qc_out_path}')

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
