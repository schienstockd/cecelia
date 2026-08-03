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
import cecelia.utils.script_utils as script_utils


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


#: The Julia function that turns this task's channel NAMES into indices. Named in the error a channel
#: name raises, so the message points at the thing that should have run — see `script_utils.channel_indices`.
_AF_TRANSLATOR = 'af_combinations_for_python (af_correct.jl)'


def _af_slabs(data, dim_utils, channels, t):
    """One frame of EVERY channel taking part in a correction, keyed by channel index.

    Replaces a `max`-across-the-references summary. The competing channels have to stay separate: each
    one contributes its own term to the weight's denominator, so collapsing them to a single reference
    image (which is what divide-mode AF did) throws away exactly the information the weight needs.
    """
    return {int(ch): _af_slab(data, dim_utils, int(ch), t) for ch in channels}


def _af_subtract(slab, subtract_val):
    """Background subtraction with a GLOBAL subtract value; returns float. Same op as the old
    subtract_background, but the percentile is computed once over the whole channel (streamed
    histogram) and applied per frame. ``subtract_val`` None → no subtraction."""
    f = slab.astype(np.float64)
    if subtract_val is not None:
        f[f < subtract_val] = subtract_val
        f -= subtract_val
    return f


#: The GLOBAL values the power weight needs before it can correct a single voxel: one background level
#: per participating channel. Whole-image by definition, which is why they are a separate, cacheable
#: step — see `af_weight_stats`. ``saturated`` rides along because the same pass already has the
#: histograms, and input saturation is the one thing about this correction worth warning about.
AfWeightStats = collections.namedtuple('AfWeightStats', 'backgrounds saturated exponent nbins')

#: How sharply a channel must dominate a voxel to keep it: ``weight = b_t^p / Σ b_i^p``.
#:
#: **Deliberately not a user parameter.** This task previously carried `channelPercentile`,
#: `correctionPercentile`, `correctionMin` and `correctionMax` — four numbers with no defensible value,
#: fitted per dataset and never revisited — and deleting them was the point of the current design. A
#: user-facing sharpness dial is that same thing coming back. p=2 was compared against p=1 and p=8 on
#: real overlapping reporter cells (kSUFux/Or1L8a): p=1 leaves too much of the losing channel, p=8
#: approaches a hard cut without gaining separation. If a dataset ever genuinely needs a different
#: value, that measurement is the trigger to expose it — not a guess in advance.
AF_WEIGHT_EXPONENT = 2


def af_weight_stats(
        data, dim_utils, channels, background_method='triangle',
        timepoints=None, spatial_stride=(1, 1), exponent=AF_WEIGHT_EXPONENT):
    """The one thing the power weight needs that a single region cannot supply: a background level per
    participating channel. Derived, not dialled in
    (`intensity_utils.background_threshold`, Zack's triangle by default).

    Background subtraction is **not optional** here, and that is worth stating because
    `BACKGROUND_METHODS` still offers `'none'` for other callers. The weight is a ratio of channel
    intensities, so an unsubtracted pedestal makes background voxels split evenly between the channels
    and survive. Measured on kSUFux/Or1L8a: **92.1%** of background voxels come out non-zero and
    cell-to-background contrast collapses from effectively unbounded to **6.8x**. The task JSON
    therefore does not offer `'none'`.

    Split out from the streaming writer so the **task preview** can pay for it once and cache it, then
    correct just the region on screen with `af_correct_frame` (the analogue of cellpose's
    `norm_params` → `predict_slice`). Global by definition: derived from a crop it would subtract a
    different background in the previewed region than in the run, i.e. lie about the thing being judged.

    ONE pass, where divide-mode AF needed two — it had to build the ratio distribution to find an
    output ceiling, and there is no ceiling any more (the output is in input counts). Measured on the
    kSUFux movies that halves this step, ~39 s → ~20 s per channel pair.

    Both subsampling knobs are kept for the preview's benefit:

    * ``timepoints`` — which frames to gather from. ``None`` = all.
    * ``spatial_stride`` — ``(z, xy)`` stride WITHIN each frame. Prefer it over dropping frames: it
      keeps every timepoint represented.

    **Subsampling is exact only because the channel has a background population to find.** The preview
    reads strided while the run reads every voxel, and they must agree to the count or the preview
    subtracts a different background than the run. They do, on real data and on any image with a
    background peak. On structureless noise there is no such population and the triangle threshold
    swings under subsampling (measured on uniform `rng.integers(0, 4000)`: 3178 → 544 → 196 at strides
    (1,1) → (1,2) → (2,4)). A fluorescence channel is mostly background so this is a degenerate rather
    than a realistic input, but it is the assumption the cheap pass rests on. Pinned by
    `test_a_spatial_stride_gives_the_same_backgrounds`.

    ``saturated`` is the fraction of each channel's voxels sitting at the dtype maximum, i.e. clipped
    on acquisition. It is free here (the histograms are already built) and it is the honest QC signal
    for this task: a clipped voxel's true value is gone, and no correction recovers it.
    """
    T = dim_utils.dim_val('T') if dim_utils.is_timeseries() else 1
    ts = range(T) if timepoints is None else list(timepoints)
    zst, xyst = (int(spatial_stride[0]), int(spatial_stride[1]))
    strided = zst > 1 or xyst > 1

    dt = data.dtype
    integer = np.issubdtype(dt, np.integer)
    nbins = (int(np.iinfo(dt).max) + 1) if integer else 256

    # dict.fromkeys dedupes while keeping order: a channel named twice must not be counted twice, and
    # a target listed inside its own competitors would otherwise square that term into the denominator
    # a second time (the Julia side rejects that case outright — see `af_combinations_for_python`).
    channels = list(dict.fromkeys(
        script_utils.channel_indices(channels, 'channels', _AF_TRANSLATOR)))

    def _stride(a):
        return a[..., ::zst, ::xyst, ::xyst] if (strided and a.ndim >= 3) else a

    hists = {ch: np.zeros(nbins, np.int64) for ch in channels}
    for t in ts:
        for ch in channels:
            a = _stride(_af_slab(data, dim_utils, ch, t))
            hists[ch] += np.bincount(np.clip(a, 0, nbins - 1).astype(np.int64).ravel(),
                                     minlength=nbins)[:nbins]

    backgrounds = {ch: float(intensity_utils.background_threshold(hists[ch], background_method))
                   for ch in channels}
    saturated = {ch: float(hists[ch][nbins - 1]) / max(1.0, float(hists[ch].sum()))
                 for ch in channels}
    return AfWeightStats(backgrounds=backgrounds, saturated=saturated,
                         exponent=int(exponent), nbins=nbins)


def af_correct_frame(slabs, target, stats, out_dtype):
    """Correct ONE frame of ONE channel: keep the share of each voxel this channel dominates.

        b_i    = max(raw_i - background_i, 0)      for the target and every competing channel
        out_t  = b_t * b_t^p / Σ_i b_i^p

    Takes the raw slabs already read, keyed by channel index, so it is pure compute with no data
    access. That is what lets the preview hand it a CROP while the run hands it a whole frame, and it
    is why the preview cannot drift from the run.

    **The output is in input counts.** Where the target is the only channel present the weight is 1 and
    the voxel passes through untouched; where a competitor is brighter it is suppressed towards zero.
    Nothing is rescaled, so there is no ceiling to derive and no window to get wrong — and the output
    can never exceed the input, so there is nothing to clip either.

    This replaces a mutual ratio, whose problem was structural rather than a matter of tuning. Its final
    form (#448) anchored the neutral ratio at zero::

        ratio = (b_t + 1) / (b_c + 1)
        out   = clip((ratio - 1) / (ceiling - 1), 0, 1) * rescale

    which makes the flaw explicit: **every voxel with ``ratio <= 1`` maps to zero.** So a cell carrying
    BOTH reporters was hollowed into a dim rim — its centre, where both channels are bright and the ratio
    sits at 1, went to zero. Anchoring at the neutral point was a real fix for a different bug (a
    ``rescale / c_max`` pedestal on every background voxel) and it did not touch this one; it could not.
    Measured on real overlapping reporter cells with overall gain cancelled (kSUFux/Or1L8a and bNnmQL,
    one plane each), the co-positive cell came out at **3-7%** of a clean single-reporter cell's
    brightness under the ratio, against **149-358%** here. A hollow cell is not a cosmetic problem:
    segmentation runs next, and a ring with a zero centre either fails to be detected or splits.

    Two properties the ratio did not have, both falling out of the form rather than added machinery: it
    is symmetric (no channel wins territory for being brighter overall), and it takes any number of
    competitors in one expression instead of chaining pairwise ratios.

    Deliberately nothing else here. This function used to carry a median filter, a gaussian, a
    rolling-ball and a top-hat, none of which are autofluorescence correction. The gaussian is the one
    worth explaining, because it was not cosmetic: dividing by a small noisy denominator amplifies
    noise, and the blur hid that. **Noise suppression is the denoise step's job** (pre-cellpose, now in
    coastal), so keeping a second, weaker version here silently blurred every corrected channel.

    ``p`` is `AF_WEIGHT_EXPONENT` — see that constant for why it is not a user parameter.

    What this still cannot fix is the INPUT's precision: on 8-bit data with ~30 usable counts above
    background, no arithmetic here invents levels. It no longer magnifies that coarseness either (the
    ratio stretched one input count into ~17 output counts), but the remaining questions — correcting the
    16-bit source before the 8-bit import, and whether a reference that is above its own background for
    1.45% of voxels can serve at all — are in docs/todo/AF_QUANTISATION.md.
    """
    p = int(stats.exponent)
    target = int(target)
    channels = [int(ch) for ch in slabs]
    if target not in channels:
        raise ValueError(f'target channel {target} is not among the slabs given ({sorted(channels)})')
    # A channel with no derived background would silently skip subtraction, so its pedestal would enter
    # the denominator and over-suppress the target. Refuse instead: it means the stats were derived for a
    # different channel set than the one being corrected, which no caller can want.
    missing = [ch for ch in channels if ch not in stats.backgrounds]
    if missing:
        raise ValueError(f'no derived background for channel(s) {missing}; '
                         f'stats cover {sorted(stats.backgrounds)}')

    b = {int(ch): _af_subtract(slab, stats.backgrounds[int(ch)]) for ch, slab in slabs.items()}

    num = b[target] ** p
    den = num.copy()
    for ch, v in b.items():
        if ch != target:
            den += v ** p
    # den == 0 exactly where every channel sits at or below its own background — nothing to attribute.
    # `where` leaves those voxels at the zeros `out=` already holds rather than dividing by zero.
    weight = np.divide(num, den, out=np.zeros_like(num), where=den > 0)
    out = b[target] * weight

    # ROUND to the integer output, don't truncate. Output is in input counts now, so the values are
    # often single digits and a bare `astype` biases every one of them down by ~half a count. Measured
    # on one plane of kSUFux/Or1L8a (85,526 non-zero voxels): truncating shifts the mean by -0.072
    # counts and forces 4.9% of real output to zero, against +0.002 and 4.0% when rounding. It mattered
    # less under the ratio, whose output was multiplied up by `rescale / ceiling` first.
    if np.issubdtype(np.dtype(out_dtype), np.integer):
        out = np.rint(out)
    return np.clip(out, 0, stats.nbins - 1).astype(out_dtype)


def _stream_corrected_channel(data, out, dim_utils, channel_idx, out_ch, competing_channel_idx,
                              background_method='triangle', stats=None, logfile_utils=None):
    """Correct one channel, streamed one timepoint at a time into ``out`` (peak memory = one frame,
    not the whole channel).

    Everything global comes from `af_weight_stats`, everything per-voxel from `af_correct_frame`.
    Pass ``stats`` to reuse an already-computed set — that is how the preview and the run stay
    identical. Returns `af_output_stats` for the corrected channel so the caller can bank QC.

    Reads the target plus every competing channel per frame, which is the same number of slabs the
    divide-mode path read to build its `max`-collapsed reference — so no extra IO, it just keeps them
    separate.
    """
    T = dim_utils.dim_val('T') if dim_utils.is_timeseries() else 1
    channels = script_utils.channel_indices([channel_idx], 'the target channel', _AF_TRANSLATOR) + \
        script_utils.channel_indices(
            competing_channel_idx, f'competingChannels for channel {channel_idx}', _AF_TRANSLATOR)
    if stats is None:
        stats = af_weight_stats(data, dim_utils, channels, background_method=background_method)
    if logfile_utils is not None:
        others = ', '.join(f'ch{c} {stats.backgrounds.get(int(c), 0.0):.0f}'
                           for c in competing_channel_idx)
        logfile_utils.log(
            f'>> ch{channel_idx}: background {stats.backgrounds.get(int(channel_idx), 0.0):.0f} '
            f'({background_method}), competing against {others}, p={stats.exponent}')

    # Output histogram, accumulated as we go — the objective signal the QC exemption in af_correct.jl
    # said was missing. Free: one bincount per frame we already hold.
    H_out = np.zeros(stats.nbins, np.int64)

    for t in range(T):
        corrected = af_correct_frame(_af_slabs(data, dim_utils, channels, t),
                                     channel_idx, stats, out.dtype)
        H_out += np.bincount(np.clip(corrected, 0, stats.nbins - 1).astype(np.int64).ravel(),
                             minlength=stats.nbins)[:stats.nbins]
        _af_write_slab(out, dim_utils, out_ch, t, corrected)

    return af_output_stats(H_out, stats, channel_idx)


def af_derived_values(stats, target):
    """The values this correction derives instead of asking for, as a plain JSON-friendly dict.

    One helper because two callers report them and they must not drift: the run banks them in QC
    (`af_output_stats`) and the preview shows them as its readout. They were briefly written out by
    hand in both places, which is how the two would have started disagreeing about a key name.

    There is no ``ceiling`` any more — the output is in input counts, so nothing is rescaled and there
    is no derived full-scale value to report or to compare across a set.
    """
    target = int(target)
    return {
        'background': float(stats.backgrounds.get(target, 0.0)),
        'competingBackgrounds': {str(ch): float(v) for ch, v in sorted(stats.backgrounds.items())
                                 if ch != target},
        'saturatedFrac': float(stats.saturated.get(target, 0.0)),
        'exponent': int(stats.exponent),
    }


def af_output_stats(hist, stats, target):
    """What the correction did to this channel — the numbers a user (or QC) acts on.

    * ``saturatedFrac`` — fraction of the channel's INPUT voxels sitting at the dtype maximum, i.e.
      clipped on acquisition. This is the honest warning for this task: a clipped voxel's true value is
      gone before we see it, so no correction recovers it and the right fix is at the microscope.
    * ``levelsUsed`` / ``levelsAvailable`` — how much of the output range the data occupies. Low means
      the channel is quantised coarsely. Under the hand-tuned percentile window that preceded all of
      this, 99% of a real image landed in ~13 of 255 levels and nothing flagged it.

    There is deliberately no ``clippedFrac``. The output is ``b_t * weight`` with ``weight <= 1``, so it
    can never exceed ``raw - background`` and therefore never reaches the dtype ceiling — the metric
    would be structurally ~0 and say nothing. Under the ratio it was the signal that the derived
    ceiling had landed too low; there is no ceiling now.

    Reported by the run (QC) and by the preview (readout), from one helper so the two agree.
    """
    hi = int(stats.nbins) - 1
    s = intensity_utils.clip_stats(hist, 0, hi)
    nz = np.nonzero(hist)[0]
    return {
        'levelsUsed': int(nz.size),
        'levelsAvailable': hi + 1,
        'trueMax': int(s.get('trueMax', 0)),
        'p999': int(s.get('p999', 0)),
        **af_derived_values(stats, target),
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

    A **channel combination is now just channels**: which channel to correct, and which channels
    compete with it. Everything that was a number in the UI — two background percentiles, a rescale
    window, a median filter, a gaussian, a rolling ball, a top hat, a denoiser, an inverse channel — is
    either derived (`af_weight_stats`) or gone. Those parameters accreted while fitting individual
    datasets and were a bag nobody revisited; a correction task should correct, not carry a filter
    toolbox.

    ``background_method`` is the one remaining choice, global to every combination — how each channel's
    background level is derived (`intensity_utils.BACKGROUND_METHODS`, minus ``'none'``: see
    `af_weight_stats`).

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
        competing = x.get('competingChannels', []) if x is not None else []
        if competing:
            output_stats[str(i)] = _stream_corrected_channel(
                input_image, out, dim_utils, channel_idx=i, out_ch=i,
                competing_channel_idx=competing,
                background_method=background_method,
                logfile_utils=logfile_utils)
        else:
            _copy_channel(input_image, out, i, dim_utils)
    return out
