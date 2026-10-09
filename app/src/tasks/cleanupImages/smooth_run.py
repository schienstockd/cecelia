"""Smoothing task.

Applies `coastal.smooth`'s model-free smoothing — an xy Gaussian, then a running statistic over
time — to selected channels, streaming z-plane by z-plane so peak memory is the temporal window and
not the movie. Output shape equals input shape; unselected channels are copied through.

Why it exists: on photon-limited resonance data (86-95% of voxels exactly zero) the AF task's
triangle threshold lands *inside* the signal, because the histogram has no background population to
find — measured on `zolIMa/fXgbTl`, the reference channel kept 8.6% of its signal past background
subtraction, 80% after smoothing. So this runs BETWEEN drift correction and AF. Full record:
`docs/todo/SMOOTHING_PLAN.md`, and `../coastal/docs/SMOOTHING.md` in the sibling repo.

Two invariants, both load-bearing and both measured (they live in `coastal.smooth`; this file only
has to not break them):

  * **spatial BEFORE temporal.** A temporal statistic alone keeps 8.5% of the reference channel's
    signal — worse than doing nothing (15.4%) — because at single-digit photon counts a median over
    3 mostly-zero samples is zero. The Gaussian has to fill the counts first. The rolling window
    below therefore caches *spatially smoothed* frames and takes the temporal statistic across them.
  * **one shared kernel for every channel.** The consumer is a cross-channel ratio (AF's weight is
    `b_t^p / sum_i b_i^p`), so a per-channel transform corrupts it. That extends to the dynamic-range
    gain below: ONE gain for all smoothed channels, never per-channel. For `stat="gated"`, whose
    kernel is ADAPTIVE, "shared" has to mean shared WEIGHTS: the match and the gate are derived once
    from the summed channels of the window and applied to each — see `smooth_utils.smooth_timepoint`.

Parameter contract (JSON written by Julia):
  imPath           - absolute path to input .ome.zarr
  imOutputPath     - absolute path to write the smoothed .ome.zarr
  channels         - list of 0-based channel indices to smooth ([] = all)
  spatialMethod    - "gaussian" | "bilateral_vst"
  spatialSigma     - xy Gaussian sigma in px, only used for spatialMethod=gaussian
  bilateralColor   - Anscombe-space color tolerance, only used for spatialMethod=bilateral_vst
  bilateralReach   - Spatial sigma in px, only used for spatialMethod=bilateral_vst
  temporalFrames   - full centred window, forced odd by coastal (1 disables)
  temporalStat     - "median" | "mean"
  restoreGain      - bool, rescale so an integer store keeps usable precision
  qcOutPath        - where to persist stats for the Julia QC step

Bilateral (VST) branch (docs/todo/SMOOTHING_PLAN.md → *Alternative spatial engine*):
  Anscombe VST → cv2.bilateralFilter in stabilised space → unbiased inverse (Mäkitalo & Foi 2011).
  Kept behind one shared kernel per channel, applied per plane. The dynamic-range gain path runs
  the same spatial function on the sampled planes so the gain estimate matches the engine that
  actually runs. **Local implementation** — will move into `coastal.smooth` once the design lands.
"""

import threading
from concurrent.futures import ThreadPoolExecutor

import cv2
import numpy as np

# STREAMING-READ-EXEMPT: temporal smoothing reads a t-window (half…half around each frame)
# with per-plane caching keyed on (t, c, z). `read_timepoint` returns ONE full frame — folding
# to it would need a window cache built on top; the current per-(t, c, z) plane reads are
# already chunk-aligned. Refactor when the window cache becomes reusable.
import cecelia.utils.cpu_utils as cpu_utils
import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.ome_xml_utils as ome_xml_utils
from cecelia.utils.dim_utils import DimUtils
import cecelia.utils.script_utils as script_utils
from cecelia.utils.atomic_io import write_json_atomic

# The per-frame compute (spatial engines, gain, gate scale, the temporal statistic) lives in
# `smooth_utils`, shared with the preview worker so a preview runs this file's arithmetic. coastal owns
# the engine underneath it (array-only, a git dep in pixi.toml).
import cecelia.utils.smooth_utils as smooth_utils


def _axis_len(dim_utils, letter, shape):
    idx = dim_utils.dim_idx(letter)
    return (idx, shape[idx]) if idx is not None else (None, 1)


def _plane_slice(ndim, t_idx, t, c_idx, c, z_idx, z):
    """Slice tuple selecting one (timepoint, channel, z) plane, leaving the spatial axes whole."""
    sl = [slice(None)] * ndim
    # `t_idx` is None on a static image (no T axis) — the store simply has no T dimension to index,
    # and the caller's `t=0` is the only value it ever passes for that case anyway.
    if t_idx is not None:
        sl[t_idx] = t
    if c_idx is not None:
        sl[c_idx] = c
    if z_idx is not None:
        sl[z_idx] = z
    return tuple(sl)


def run(params):
    log = script_utils.get_logfile_utils(params)

    im_path          = params['imPath']
    out_path         = params['imOutputPath']
    channels         = [int(c) for c in (params.get('channels') or [])]
    method           = str(params.get('spatialMethod', 'gaussian'))
    sigma            = float(params.get('spatialSigma', 1.0))
    bilateral_color  = float(params.get('bilateralColor', 10.0))
    bilateral_reach  = float(params.get('bilateralReach', 3.0))
    bilateral_polish = float(params.get('bilateralPolish', 0.6))
    frames           = int(params.get('temporalFrames', 3))
    stat             = str(params.get('temporalStat', 'median'))
    farneback_clamp  = float(params.get('farnebackMaxShiftPx', 8.0))
    restore_gain     = bool(params.get('restoreGain', True))

    # ONE spatial function for the estimators AND the streaming loop — see `_build_spatial_fn`.
    # A separate closure per path would let the gain estimate drift from the loop that uses it.
    spatial_fn = smooth_utils.build_spatial_fn(method, sigma, bilateral_color, bilateral_reach,
                                               bilateral_polish)

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
    # A static image is fine: `temporalFrames` and `temporalStat` carry `requires.axes: ["T"]` in
    # the spec, so `_apply_param_requires` drops them for a static input and the fallbacks kick in
    # (`frames=1`, gated=False, half=0). The streaming loop's `if half == 0` branch is then one
    # spatial pass per plane — which is precisely what the spatial-only engines (gaussian and
    # bilateral_vst) mean by themselves. The task no longer needs a T axis; the temporal step does.

    sel = channels if channels else list(range(nc))
    others = [c for c in range(nc) if c not in sel]
    log.log(f'>> dims {dim_utils.im_dim_order} {shape}')
    if method == 'bilateral_vst':
        log.log(f'>> smoothing channels {sel} (passing through {others}); '
                f'spatial=bilateral_vst color={bilateral_color} reach={bilateral_reach} '
                f'polish={bilateral_polish}, frames={frames}, stat={stat}')
    else:
        log.log(f'>> smoothing channels {sel} (passing through {others}); '
                f'spatial=gaussian sigma={sigma}, frames={frames}, stat={stat}')

    half = max(0, (frames - 1) // 2) if frames and frames > 1 else 0
    gated = stat == 'gated' and half > 0
    farneback = stat == 'farneback' and half > 0
    if farneback:
        log.log(f'   farneback clamp: {farneback_clamp} px')

    # ── the gate's noise scale ─────────────────────────────────────────────────────────────────
    # Estimated ONCE, from a sample, and handed to every frame. `gated_frames` would otherwise
    # estimate it per window — a 3-9 frame sample — so the gate's strictness would drift between
    # z-planes and timepoints for no physical reason. The noise level is a property of the
    # acquisition, not of the window we happen to be holding. (coastal pins the two forms equal
    # given one sigma.)
    gate_sigma = None

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

    # ── the gain ── ONE across all smoothed channels (`smooth_utils.estimate_gain` says why)
    gain = 1.0
    dtype_max = np.iinfo(zarr_utils.native_dtype(level_in.dtype)).max \
        if np.issubdtype(level_in.dtype, np.integer) else None
    if restore_gain and dtype_max is not None:
        log.log('>> estimate dynamic-range gain')
        gain, hi_in, hi_sm = smooth_utils.estimate_gain(read_plane, spatial_fn, sel, nt, nz)
        log.log(f'   input p99.99 {hi_in:.1f}, smoothed {hi_sm:.1f} -> gain {gain:.2f}')

    if gated:
        log.log('>> estimate the gate noise scale')
        gate_sigma, n_samples = smooth_utils.estimate_gate_sigma(read_plane, spatial_fn, sel, nt, nz)
        log.log(f'   gate sigma {gate_sigma:.2f} (median of {n_samples} z-planes)')

        # A gate with no noise scale is not a weak gate, it is NO gate: `_scale_from` clamps to 1e-12,
        # so every weight becomes exp(-d/1e-12) = 0 for any mismatch at all and the output is the
        # input. It happens for one reason — `spatialSigma=0` on photon-limited data, where the
        # temporal difference is a majority of exact zeros and its MAD is exactly 0. Measured on
        # `zolIMa/fXgbTl` at sigma 0: amplitude kept 1.00, background noise kept 1.00, all four
        # channels. Refused rather than run, because the alternative is minutes a channel spent
        # writing a copy of the input that LOOKS like a successful smoothing run — and the AF task
        # downstream would then be handed the same untouched counts it could not threshold before.
        # Not repaired by raising sigma here: the value the user set is the one they will read back
        # off the QC, and a run that silently used a different one is the worse surprise.
        if gate_sigma <= 0:
            log.log('[ERROR] the gate has no noise scale to work with, so every frame would be '
                    'returned unchanged. Set a spatial sigma above 0 (the Gaussian has to fill the '
                    'counts before a gate can match on them), or use the median statistic')
            raise SystemExit(1)

    # Counted whether or not it ran, so the scale means the same thing with restoreGain off.
    done = 1
    log.progress(done, total)

    # ── stream ─────────────────────────────────────────────────────────────────────────────────
    # Per z-plane parallelism: each z owns its rolling cache and writes to non-overlapping chunks,
    # so the loop is embarrassingly parallel below the gain/gate estimates. Threads (not processes)
    # because the compute paths — cv2, scipy, numpy — release the GIL, and zarr writes are I/O
    # bound. The worker count comes from `cpu_utils.task_workers()`, which respects the pool
    # budget the scheduler set via `run_py`. `cv2.setNumThreads` gets the *remaining* CPU share
    # after z-parallelism — on a static image (nz=1) that's the full budget, so cv2 still uses
    # every core; on a stack with many z-planes the pool covers the parallelism and cv2 stays
    # single-threaded per worker rather than fighting the pool for cores.
    z_workers = max(1, min(cpu_utils.task_workers(), nz))
    cv2.setNumThreads(max(1, cpu_utils.task_workers() // z_workers))
    log.log(f'>> smooth + write (per-z workers: {z_workers}, cv2 threads: {cv2.getNumThreads()}) '
            f'→ {out_path}')
    stats = {'zeroFracIn': {}, 'zeroFracOut': {}, 'clippedVoxels': 0, 'gain': gain}
    zin = {c: [] for c in sel}
    zout = {c: [] for c in sel}
    clipped = 0

    with zarr_utils.staged_store(out_path) as staging:
        group, level0, pchunks = zarr_utils.open_multiscales_for_writing(
            staging, shape, level_in.dtype, dim_utils, nscales=len(im_dat),
            reference_zarr=im_path)   # inherit the source's zarr format (ZARR_V3_PLAN D9)

        # accumulators are updated from worker threads — one lock for both the fraction lists AND
        # the clipped counter, so the (zin, zout, clipped) triple is either all-updated or none.
        stats_lock = threading.Lock()

        def _process_z(z):
            """Process one z-plane. Returns (zin_local, zout_local, clipped_local)."""
            # Rolling cache of SPATIALLY smoothed planes for THIS z only, keyed by timepoint.
            # Local to the worker — no cross-thread sharing.
            cache = {}

            def spatial_at(t, c):
                t = min(max(t, 0), nt - 1)          # clamp = scipy's mode='nearest'
                key = (t, c)
                if key not in cache:
                    cache[key] = spatial_fn(read_plane(t, c, z))
                return cache[key]

            local_zin = {c: [] for c in sel}
            local_zout = {c: [] for c in sel}
            local_clipped = 0

            for t in range(nt):
                outs = smooth_utils.smooth_timepoint(spatial_at, sel, t, half, frames, stat,
                                                     gate_sigma, farneback_clamp)
                for c in sel:
                    raw = read_plane(t, c, z)
                    local_zin[c].append(float((raw == 0).mean()))
                    out, n_clip = smooth_utils.apply_gain(outs[c], gain, dtype_max)
                    local_clipped += n_clip
                    local_zout[c].append(float((out == 0).mean()))
                    # Zarr writes to non-overlapping chunks are safe from concurrent threads (each
                    # z is its own chunk row in the default layout). Verified by convention: coastal
                    # writes the same way (`coastal_utils` ThreadPoolExecutor over z).
                    level0[_plane_slice(len(shape), t_idx, t, c_idx, c, z_idx, z)] = \
                        out.astype(level0.dtype)

                for c in others:
                    level0[_plane_slice(len(shape), t_idx, t, c_idx, c, z_idx, z)] = \
                        level_in[_plane_slice(len(shape), t_idx, t, c_idx, c, z_idx, z)]

                # drop cache entries that can no longer be reached by a later window
                for key in [k for k in cache if k[0] < t - half]:
                    del cache[key]

            return z, local_zin, local_zout, local_clipped

        # `ThreadPoolExecutor` gives determinism at the write layer for free — every z's writes are
        # independent — and pays no fork-startup cost. Serial when nz == 1 (a static image) — no
        # point paying the executor tax for a one-item queue.
        if z_workers == 1 or nz <= 1:
            iterator = (_process_z(z) for z in range(nz))
        else:
            executor = ThreadPoolExecutor(max_workers=z_workers)
            iterator = executor.map(_process_z, range(nz))
        try:
            for z, lz_in, lz_out, lc in iterator:
                with stats_lock:
                    for c in sel:
                        zin[c].extend(lz_in[c])
                        zout[c].extend(lz_out[c])
                    clipped += lc
                done += 1
                log.progress(done, total)
                if nz > 1:
                    log.log(f'   z {z + 1}/{nz}')
        finally:
            if z_workers > 1 and nz > 1:
                executor.shutdown(wait=True)

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
    stats['spatialMethod'] = method
    stats['spatialSigma'] = sigma
    stats['bilateralColor']  = bilateral_color
    stats['bilateralReach']  = bilateral_reach
    stats['bilateralPolish'] = bilateral_polish
    stats['temporalFrames']  = frames
    stats['temporalStat'] = stat
    stats['farnebackMaxShiftPx'] = farneback_clamp
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
