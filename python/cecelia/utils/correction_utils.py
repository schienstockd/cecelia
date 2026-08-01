"""
Image correction utilities: autofluorescence and drift correction.

Ported from the original R/Python cecelia package. Uses scipy and skimage
instead of dask-image and pyclesperanto (neither of which is in the venv).
All channel-level operations materialise to numpy internally; output is
returned as a dask array so create_multiscales can use it directly.
"""

import collections
from copy import copy
import numpy as np
import shutil

import dask.array as da
import scipy.ndimage
import skimage.restoration
import skimage.morphology
import skimage.filters
from skimage.registration import phase_cross_correlation

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.slice_utils as slice_utils
import cecelia.utils.intensity_utils as intensity_utils


# ── Drift correction ──────────────────────────────────────────────────────────

def drift_correction_shifts(
        image_array, phase_shift_channel, dim_utils,
        timepoints=None, upsample_factor=100,
        normalisation=None, time_idx=None, channel_idx=None):
    shifts = []
    if channel_idx is None:
        channel_idx = dim_utils.dim_idx('C')
    if time_idx is None:
        time_idx = dim_utils.dim_idx('T')

    slices = [slice(None)] * len(image_array.shape)
    if channel_idx is not None:
        slices[channel_idx] = slice(phase_shift_channel, phase_shift_channel + 1, 1)

    if timepoints is None:
        timepoints = range(1, dim_utils.dim_val('T'))

    for x in timepoints:
        if x % 10 == 0:
            print(x)
        slices_a = slices.copy()
        slices_b = slices.copy()
        slices_a[time_idx] = slice(x - 1, x, 1)
        slices_b[time_idx] = slice(x, x + 1, 1)
        shift, error, diffphase = phase_cross_correlation(
            np.squeeze(zarr_utils.fortify(image_array[tuple(slices_a)])),
            np.squeeze(zarr_utils.fortify(image_array[tuple(slices_b)])),
            upsample_factor=upsample_factor,
            normalization=normalisation,
        )
        shifts.append(shift)
    return np.vstack(shifts)


def shifts_summary(shifts, cumulative=True, is_3D=True):
    shift_size = 3 if is_3D else 2
    max_shifts = np.zeros(shift_size)
    min_shifts = np.zeros(shift_size)
    cur_shifts = np.zeros(shift_size)
    for x in shifts:
        cur_shifts = cur_shifts + x if cumulative else x
        max_shifts = np.maximum(cur_shifts, max_shifts)
        min_shifts = np.minimum(cur_shifts, min_shifts)
    min_shifts = abs(min_shifts)
    return {'max': max_shifts, 'min': min_shifts, 'sum': max_shifts + min_shifts}


def _as_shape(array_or_shape):
    """Accept either an array or a plain shape tuple. Lets the geometry helpers below be used
    post-hoc — from a QC sidecar, say — without opening the store they describe."""
    return tuple(getattr(array_or_shape, 'shape', array_or_shape))


def correction_im_shape(image_array, dim_utils, shifts_sum):
    new_shape = list(_as_shape(image_array))
    if dim_utils.is_3D():
        new_shape[dim_utils.dim_idx('Z')] += abs(shifts_sum['sum'][0])
        new_shape[dim_utils.dim_idx('Y')] += abs(shifts_sum['sum'][1])
        new_shape[dim_utils.dim_idx('X')] += abs(shifts_sum['sum'][2])
    else:
        new_shape[dim_utils.dim_idx('Y')] += abs(shifts_sum['sum'][0])
        new_shape[dim_utils.dim_idx('X')] += abs(shifts_sum['sum'][1])
    new_shape_round = tuple(round(x) for x in new_shape)
    return new_shape, new_shape_round


def correction_first_im_pos(drift_im_shape, dim_utils, shifts_sum):
    if dim_utils.is_3D():
        new_pos = np.take(
            drift_im_shape,
            [dim_utils.dim_idx('Z'), dim_utils.dim_idx('Y'), dim_utils.dim_idx('X')])
        shift_size = 3
    else:
        new_pos = np.take(
            drift_im_shape,
            [dim_utils.dim_idx('Y'), dim_utils.dim_idx('X')])
        shift_size = 2
    return tuple(
        slice(shifts_sum['min'][i], new_pos[i] - shifts_sum['max'][i], 1)
        for i in range(shift_size)
    )


def drift_correct_shape(input_array, dim_utils, shifts):
    """Output canvas shape (rounded) and first-frame position for a drift correction with the
    given per-frame ``shifts``. Split out so a caller can create the on-disk output store BEFORE
    filling it (``drift_correct_im`` streams each timepoint straight into that store)."""
    shifts_sum = shifts_summary(shifts, is_3D=dim_utils.is_3D())
    drift_im_shape, drift_im_shape_round = correction_im_shape(
        input_array, dim_utils, shifts_sum)
    first_im_pos = correction_first_im_pos(drift_im_shape, dim_utils, shifts_sum)
    return drift_im_shape_round, first_im_pos


def drift_frame_slices(input_array, dim_utils, shifts, timepoints=None):
    """Where each timepoint's pixels LAND in the expanded canvas: ``{t: tuple(slice per axis)}``.

    Drift correction writes every frame into a ZEROED canvas at a per-frame offset, so these
    slices are also the answer to "which part of a corrected store is data and which is padding".
    That matters because the expansion is not small — a 30-minute 8-plane movie here came out
    22 planes deep, i.e. 64% zeros — and a downstream consumer that does not know the box pays
    for all of it.

    Pure shape arithmetic; touches no pixels, so it can be replayed later from nothing but the
    source shape and the shifts a run recorded. `drift_correct_im` places its frames with this,
    and the QC sidecar persists its output — one implementation, so the padding a consumer skips
    is exactly the padding the writer left.

    ``input_array`` may be an array or a plain shape tuple. ``timepoints`` must start at 0 and be
    consecutive: the offset ACCUMULATES across frames (``shifts`` are per-frame deltas), so a
    subset would silently mis-place everything after the first gap — the same constraint the
    writer's loop has always had.
    """
    shifts = np.asarray(shifts)
    canvas_shape, first_im_pos = drift_correct_shape(input_array, dim_utils, shifts)
    src_shape = list(_as_shape(input_array))
    src_shape[dim_utils.dim_idx('T')] = 1
    tp_shape = list(canvas_shape)
    tp_shape[dim_utils.dim_idx('T')] = 1
    if timepoints is None:
        timepoints = range(dim_utils.dim_val('T'))

    out = {}
    slices = list(first_im_pos)
    for i in timepoints:
        if i > 0:
            slices = [slice(y.start + shifts[i - 1, j], y.stop + shifts[i - 1, j], 1)
                      for j, y in enumerate(slices)]

        new_slices = [slice(None)] * len(canvas_shape)
        for j, y in enumerate(dim_utils.spatial_axis()):
            new_slices[dim_utils.dim_idx(y)] = slice(
                round(slices[j].start), round(slices[j].stop), 1)

        # Clamp exactly as the writer does: rounding can leave the destination window a pixel off
        # the source, and a frame at the very start/end of the drift can run past the canvas edge.
        # `slice.indices` is what numpy itself applies, so this measures the real destination size.
        dest = [len(range(*sl.indices(tp_shape[k]))) for k, sl in enumerate(new_slices)]
        if dest != src_shape:
            adj = list(new_slices)
            for j, y in enumerate([d - s for d, s in zip(dest, src_shape)]):
                if y > 0:
                    adj[j] = slice(adj[j].start + y, adj[j].stop, 1)
                elif y < 0:
                    if adj[j].start - y >= 0:
                        adj[j] = slice(adj[j].start + y, adj[j].stop, 1)
                    elif adj[j].stop + y < canvas_shape[j]:
                        adj[j] = slice(adj[j].start, adj[j].stop + y, 1)
            new_slices = adj
        out[i] = tuple(new_slices)
    return out


def drift_frame_origins(input_array, dim_utils, shifts, timepoints=None):
    """`drift_frame_slices` reduced to what a consumer usually wants: per timepoint, the
    ``{axis: [start, stop]}`` of the occupied box on each SPATIAL axis, as plain ints.

    JSON-friendly, so a run can record it (see the drift QC sidecar) and anything reading that
    store later — coastal, a viewer — can skip the padding without re-deriving the geometry or
    reading a single voxel."""
    axes = list(dim_utils.spatial_axis())
    return {
        t: {ax: [int(sl[dim_utils.dim_idx(ax)].start), int(sl[dim_utils.dim_idx(ax)].stop)]
            for ax in axes}
        for t, sl in drift_frame_slices(input_array, dim_utils, shifts, timepoints).items()
    }


def drift_correct_im(
        input_array, dim_utils, phase_shift_channel,
        timepoints=None, drift_corrected_path=None,
        upsample_factor=100, shifts=None, chunk_size=None, out=None):
    if timepoints is None:
        timepoints = range(dim_utils.dim_val('T'))

    if shifts is None:
        shifts = drift_correction_shifts(
            input_array, phase_shift_channel, dim_utils,
            timepoints=range(1, dim_utils.dim_val('T')),
            upsample_factor=upsample_factor,
        )

    drift_im_shape_round, _ = drift_correct_shape(input_array, dim_utils, shifts)

    # The writer owns byte order (zarr_utils.native_dtype): when `out` is a pre-created native store
    # we match it; the out=None numpy path returns source-order and create_multiscales makes it native.
    result_dtype = out.dtype if out is not None else input_array.dtype

    # `out`, when given, is a pre-created on-disk zarr of `drift_im_shape_round` — each timepoint is
    # written straight to disk so the whole (expanded) corrected image never lives in RAM. When None
    # we allocate it in memory (legacy / small-image path). Either way the loop below is unchanged:
    # it fills one timepoint at a time, so the streaming and in-RAM results are byte-identical.
    result = out if out is not None else np.zeros(drift_im_shape_round, dtype=result_dtype)

    tp_shape = list(drift_im_shape_round)
    tp_shape[dim_utils.dim_idx('T')] = 1
    tp_shape = tuple(tp_shape)

    # Where each frame lands. Shape arithmetic only, so it is computed up front and shared with
    # every other consumer of this store (the QC sidecar records it) instead of living inline here
    # — see drift_frame_slices.
    frame_slices = drift_frame_slices(input_array, dim_utils, shifts, timepoints)

    for i in timepoints:
        new_slices = frame_slices[i]
        im_slices = [slice(None)] * len(drift_im_shape_round)
        im_slices[dim_utils.dim_idx('T')] = slice(i, i + 1, 1)
        im_slices = tuple(im_slices)

        if i % 10 == 0:
            print(i)

        src = zarr_utils.fortify(input_array[im_slices])
        new_image = np.zeros(tp_shape, dtype=result_dtype)
        new_image[new_slices] = src
        result[im_slices] = new_image

    return result


# ── Autofluorescence correction ───────────────────────────────────────────────

# Per-FRAME spatial primitives (operate on one channel-frame slab: T=1, C=1, spatial). AF streams
# per timepoint, so these replace the old whole-channel helpers that iterated dim_utils' global T
# (which broke on a single-frame slab). Each squeezes to the spatial frame, applies the op, and
# reshapes back to the slab shape.

def non_zero_edges(im):
    true_points = np.argwhere(im)
    return {'tl': true_points.min(axis=0), 'br': true_points.max(axis=0)}





def _af_slab(data, dim_utils, channel_idx, t):
    """One channel + one timepoint as numpy, full axis layout (T=1, C=1, spatial). The unit of AF
    processing — bounded to a single frame regardless of movie length."""
    sl = list(dim_utils.create_channel_slices(channel_idx))
    if dim_utils.is_timeseries():
        sl[dim_utils.dim_idx('T')] = slice(t, t + 1, 1)
    return zarr_utils.fortify(data[tuple(sl)])


def _af_write_slab(out, dim_utils, channel_idx, t, slab):
    """Write one processed channel-frame into the on-disk (or numpy) output at (channel_idx, t)."""
    sl = list(dim_utils.create_channel_slices(channel_idx))
    if dim_utils.is_timeseries():
        sl[dim_utils.dim_idx('T')] = slice(t, t + 1, 1)
    out[tuple(sl)] = slab


def _af_correction_slab(data, dim_utils, correction_channel_idx, t, summary_mode, summary_percentile):
    """Summary correction image (max, or percentile, across the division channels) for one frame."""
    stack = np.stack([_af_slab(data, dim_utils, x, t) for x in correction_channel_idx], axis=0)
    if summary_mode == 'percentile':
        return np.percentile(stack, summary_percentile, axis=0)
    return np.max(stack, axis=0)


def _af_subtract(slab, subtract_val):
    """Background subtraction with a GLOBAL subtract value; returns float. Same op as the old
    subtract_background, but the percentile is computed once over the whole channel (streamed
    histogram) and applied per frame. ``subtract_val`` None → no subtraction."""
    f = slab.astype(np.float64)
    if subtract_val is not None:
        f[f < subtract_val] = subtract_val
        f -= subtract_val
    return f


#: The GLOBAL scalars divide-mode AF needs before it can correct a single pixel. Whole-image by
#: definition, which is why they are a separate, cacheable step — see `af_division_stats`.
AfDivisionStats = collections.namedtuple('AfDivisionStats', 'val1 val2 c_max nbins rescale')

#: How many voxels a value must reach before the ceiling will sit at it. Measured on a real 181-frame
#: movie: the top six occupied ratio bins held ONE voxel each, and this threshold gave an identical
#: ceiling under every spatial stride tried (z::2, xy::4, both), where the true max did not. Lower
#: values track the data more closely but sample worse — 100 drifted ~10% under striding, 1 000 by 2%,
#: 10 000 not at all. See docs/todo/TASK_PREVIEW_PLAN.md.
AF_CEILING_MIN_COUNT = 10_000

#: Fraction of the ratio's own range the derived ceiling may never fall below. Guards the trap in
#: `robust_hist_max`: the background bin dominates the histogram, so a min-count larger than the
#: signal population would return a ceiling inside the background and collapse the rescale.
AF_CEILING_FLOOR_FRAC = 0.02


def af_division_stats(
        data, dim_utils, channel_idx, correction_channel_idx,
        background_method='triangle', summary_mode='maximum', summary_percentile=75,
        timepoints=None, spatial_stride=(1, 1), ceiling_min_count=None):
    """Everything divide-mode AF needs that a single region cannot supply: the two background levels
    and the output ceiling. All three **derived**, none dialled in.

    * ``val1`` — background of the target channel
    * ``val2`` — the level above which the AF reference counts as autofluorescence
    * ``c_max`` — the ratio that maps to full scale, as an outlier-rejected maximum

    These replace `channelPercentile`, `correctionPercentile`, `correctionMin` and `correctionMax`,
    four numbers with no defensible value that were fitted per dataset and never revisited. The
    background pair now comes from `intensity_utils.background_threshold` (Zack's triangle by default)
    and the ceiling from `intensity_utils.robust_hist_max`.

    Split out from the streaming writer so the **task preview** can pay for them once and cache them,
    then correct just the region on screen with `af_correct_frame` (the analogue of cellpose's
    `norm_params` → `predict_slice`). They are global by definition: computing them from a crop would
    subtract a different background in the previewed region than in the run, i.e. lie about the one
    thing being tuned.

    Two passes, because the ratio cannot be formed until the backgrounds are known. Both are
    subsamplable and the defaults are measured, not guessed:

    * ``timepoints`` — which frames to gather from. ``None`` = all.
    * ``spatial_stride`` — ``(z, xy)`` stride WITHIN each frame. Prefer this over dropping frames: it
      keeps every timepoint represented, and the count-thresholded ceiling is invariant to it
      (identical answer at ``(2, 4)``, which cost 24 s against 148 s for the full read).

    A note on why the ceiling is derived here and not handed in: it is a property of the corrected
    ratio, so it cannot be known before the backgrounds are, and it must be identical for the run and
    the preview or the preview's brightness is a lie.
    """
    T = dim_utils.dim_val('T') if dim_utils.is_timeseries() else 1
    ts = range(T) if timepoints is None else list(timepoints)
    zst, xyst = (int(spatial_stride[0]), int(spatial_stride[1]))
    strided = zst > 1 or xyst > 1

    dt = data.dtype
    integer = np.issubdtype(dt, np.integer)
    rescale = float(np.iinfo(dt).max) if integer else 255.0
    nbins = (int(np.iinfo(dt).max) + 1) if integer else 256
    hi = float(nbins)                     # the ratio's ceiling: (maxval+1)/1

    def _stride(a):
        return a[..., ::zst, ::xyst, ::xyst] if (strided and a.ndim >= 3) else a

    # ── pass 1: the two background levels ───────────────────────────────────
    H_ch, H_corr = np.zeros(nbins, np.int64), np.zeros(nbins, np.int64)
    for t in ts:
        ch = _stride(_af_slab(data, dim_utils, channel_idx, t))
        H_ch += np.bincount(np.clip(ch, 0, nbins - 1).astype(np.int64).ravel(), minlength=nbins)[:nbins]
        ci = _stride(_af_correction_slab(data, dim_utils, correction_channel_idx, t,
                                         summary_mode, summary_percentile))
        H_corr += np.bincount(np.clip(np.rint(ci), 0, nbins - 1).astype(np.int64).ravel(),
                              minlength=nbins)[:nbins]
    val1 = float(intensity_utils.background_threshold(H_ch, background_method))
    val2 = float(intensity_utils.background_threshold(H_corr, background_method))

    # ── pass 2: the ceiling, from the corrected-ratio distribution ──────────
    H_ratio = np.zeros(nbins, np.int64)
    for t in ts:
        img = _af_subtract(_stride(_af_slab(data, dim_utils, channel_idx, t)), val1)
        corr = _af_subtract(_stride(_af_correction_slab(data, dim_utils, correction_channel_idx, t,
                                                       summary_mode, summary_percentile)), val2)
        ratio = (img + 1.0) / (corr + 1.0)
        H_ratio += np.bincount(np.clip(ratio / hi * (nbins - 1), 0, nbins - 1).astype(np.int64).ravel(),
                               minlength=nbins)[:nbins]

    # Scale the count threshold by the sampling fraction, so a strided pass asks for proportionally
    # fewer voxels and lands on the same ceiling as a full one.
    if ceiling_min_count is None:
        frac = 1.0 / (zst * xyst * xyst) * (len(ts) / max(1, T))
        ceiling_min_count = max(1, int(round(AF_CEILING_MIN_COUNT * frac)))
    c_max = intensity_utils.robust_hist_max(H_ratio, ceiling_min_count) / (nbins - 1) * hi
    # the trap documented on `robust_hist_max`: the background bin dominates, so an over-large count
    # can return a ceiling inside it. Never let the window collapse.
    c_max = max(float(c_max), hi * AF_CEILING_FLOOR_FRAC)

    return AfDivisionStats(val1=val1, val2=val2, c_max=c_max, nbins=nbins, rescale=rescale)


def af_correct_frame(img_slab, corr_slab, stats, out_dtype):
    """Divide-mode AF for ONE frame: subtract both backgrounds, divide, map to the stored dtype.

    Takes the two raw slabs already read — the target channel and the summarised AF reference — so it
    is pure compute with no data access. That is what lets the preview hand it a CROP while the run
    hands it a whole frame, and it is why the preview cannot drift from the run.

    Four lines of arithmetic, and deliberately nothing else. It used to carry a median filter, a
    gaussian, a rolling-ball and a top-hat, none of which are autofluorescence correction — they were
    a small image-processing toolbox that accreted in this task while fitting individual datasets.

    The gaussian is the one worth explaining, because it was not cosmetic: dividing by a small noisy
    denominator amplifies noise, and the blur hid that. **Noise suppression is the denoise step's job**
    (pre-cellpose, now in coastal), so keeping a second, weaker version of it here meant every
    corrected channel was silently blurred whether or not it was going to be denoised anyway. It also
    matters less than it did: the derived AF background sits higher than the hand-tuned percentile it
    replaced, so more of the reference channel is zeroed, the denominator is 1 more often, and there is
    simply less division to amplify anything.

    ``stats.c_max`` is the ratio that maps to full scale, derived in `af_division_stats` as an
    outlier-rejected maximum. Every value in this function comes from the data.
    """
    img = _af_subtract(img_slab, stats.val1)
    corr = _af_subtract(corr_slab, stats.val2)
    ratio = (img + 1.0) / (corr + 1.0)
    denom = stats.c_max if stats.c_max > 0 else 1.0
    return np.clip(ratio / denom * stats.rescale, 0, stats.rescale).astype(out_dtype)




def _stream_division_channel(data, out, dim_utils, channel_idx, out_ch, correction_channel_idx,
                             background_method='triangle', summary_mode='maximum',
                             summary_percentile=75, stats=None, logfile_utils=None):
    """Divide-mode AF for one channel, streamed one timepoint at a time into ``out`` (peak memory =
    one frame, not the whole channel).

    Everything global comes from `af_division_stats`, everything per-voxel from `af_correct_frame`.
    Pass ``stats`` to reuse an already-computed set — that is how the preview and the run stay
    identical. Returns `af_output_stats` for the corrected channel so the caller can bank QC.
    """
    T = dim_utils.dim_val('T') if dim_utils.is_timeseries() else 1
    if stats is None:
        stats = af_division_stats(data, dim_utils, channel_idx, correction_channel_idx,
                                  background_method=background_method, summary_mode=summary_mode,
                                  summary_percentile=summary_percentile)
    if logfile_utils is not None:
        logfile_utils.log(f'>> ch{channel_idx}: background {stats.val1:.0f} / AF {stats.val2:.0f}, '
                          f'ceiling {stats.c_max:.1f} ({background_method})')

    # Output histogram, accumulated as we go — the objective signal the QC exemption in af_correct.jl
    # said was missing. Free: one bincount per frame we already hold.
    H_out = np.zeros(stats.nbins, np.int64)

    for t in range(T):
        corrected = af_correct_frame(
            _af_slab(data, dim_utils, channel_idx, t),
            _af_correction_slab(data, dim_utils, correction_channel_idx, t,
                                summary_mode, summary_percentile),
            stats, out.dtype)
        H_out += np.bincount(np.clip(corrected, 0, stats.nbins - 1).astype(np.int64).ravel(),
                             minlength=stats.nbins)[:stats.nbins]
        _af_write_slab(out, dim_utils, out_ch, t, corrected)

    return af_output_stats(H_out, stats)


def af_output_stats(hist, stats):
    """Did the derived ceiling land well? The numbers a user (or QC) acts on.

    * ``clippedFrac`` — fraction pushed to the dtype ceiling. Should be near zero; if it isn't, the
      ceiling was derived too low and real signal is being flattened.
    * ``levelsUsed`` / ``levelsAvailable`` — how much of the output range the data occupies. Low means
      the ceiling is too high and quantisation is being thrown away: under the percentile window this
      replaced, 99% of a real image landed in ~13 of 255 levels.

    Reported by the run (QC) and by the preview (readout), from one helper so the two agree.
    """
    rescale = stats.rescale
    hi = int(rescale)
    s = intensity_utils.clip_stats(hist, 0, hi)
    nz = np.nonzero(hist)[0]
    return {
        'clippedFrac': float(s.get('clipHighFrac', 0.0)),
        'levelsUsed': int(nz.size),
        'levelsAvailable': hi + 1,
        'trueMax': int(s.get('trueMax', 0)),
        'p999': int(s.get('p999', 0)),
    }


def af_correction_output_shape(input_array, dim_utils, af_combinations=None):
    """Shape of the AF-corrected output — now identical to the input.

    It used to widen the channel axis by one per combination requesting `generateInverse`. That option
    is gone with the rest of the per-combination bag, so the output has the same channels as the input:
    corrected where a combination covers them, carried through unchanged where it doesn't. Kept as a
    function (rather than inlining `input_array.shape`) because every writer sizes its store through it
    and a future channel-adding option would land here.
    """
    return tuple(input_array.shape)


def _copy_channel(input_image, out, i, dim_utils):
    """Carry a channel no combination covers through unchanged, one timepoint at a time.

    It used to denoise, gaussian-blur, rolling-ball and top-hat these channels — by DEFAULT, since
    `applyGaussianToOthers` was true, so the AF task silently filtered channels it wasn't correcting.
    A correction task has no business modifying a channel nobody asked it to touch.
    """
    T = dim_utils.dim_val('T') if dim_utils.is_timeseries() else 1
    for t in range(T):
        _af_write_slab(out, dim_utils, i, t,
                       _af_slab(input_image, dim_utils, i, t).astype(out.dtype))


def af_correct_image(input_image, af_combinations, dim_utils, logfile_utils,
                     background_method='triangle', out=None, output_stats=None):
    """Correct autofluorescence for all channels, streamed ONE TIMEPOINT AT A TIME per channel.

    A **channel combination is now just channels**: which channel to correct, and which to correct it
    against. Everything that was a number in the UI — two background percentiles, a rescale window, a
    median filter, a gaussian, a rolling ball, a top hat, a denoiser, an inverse channel — is either
    derived (`af_division_stats`) or gone. Those parameters accreted while fitting individual datasets
    and were a bag nobody revisited; a correction task should correct, not carry a filter toolbox.

    ``background_method`` is the one remaining choice, global to every combination — how the two
    background levels are derived (`intensity_utils.BACKGROUND_METHODS`).

    Peak memory is a single channel-frame — casting a whole channel-timecourse to float64 (~47 GB on
    a large movie) was the OOM. When ``out`` is None a numpy array is allocated and returned (legacy /
    small-image path); production passes the on-disk zarr from ``open_multiscales_for_writing``.
    Pass a dict as ``output_stats`` to receive per-corrected-channel `af_output_stats`, keyed by
    channel index as a string — an out-parameter rather than a second return value so callers that use
    the returned array keep working.
    """
    n_channels = dim_utils.dim_val('C')
    af_combinations = {int(i): x for i, x in af_combinations.items()}

    if out is None:   # legacy/small: allocate the full output (compute still streams per frame)
        out = np.zeros(af_correction_output_shape(input_image, dim_utils, af_combinations),
                       dtype=zarr_utils.native_dtype(input_image.dtype))

    output_stats = {} if output_stats is None else output_stats
    for i in range(n_channels):
        x = af_combinations.get(i)
        div_channels = x.get('divisionChannels', []) if x is not None else []
        if div_channels:
            output_stats[str(i)] = _stream_division_channel(
                input_image, out, dim_utils, channel_idx=i, out_ch=i,
                correction_channel_idx=div_channels,
                background_method=background_method,
                logfile_utils=logfile_utils)
        else:
            _copy_channel(input_image, out, i, dim_utils)
    return out
