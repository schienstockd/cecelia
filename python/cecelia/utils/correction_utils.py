"""
Image correction utilities: autofluorescence and drift correction.

Ported from the original R/Python cecelia package. Uses scipy and skimage
instead of dask-image and pyclesperanto (neither of which is in the venv).
All channel-level operations materialise to numpy internally; output is
returned as a dask array so create_multiscales can use it directly.
"""

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


def correction_im_shape(image_array, dim_utils, shifts_sum):
    new_shape = list(image_array.shape)
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

    drift_im_shape_round, first_im_pos = drift_correct_shape(input_array, dim_utils, shifts)

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

    slices = list(first_im_pos)

    for i in timepoints:
        if i > 0:
            new_slices = []
            for j, y in enumerate(slices):
                new_slices.append(slice(
                    y.start + shifts[i - 1, j],
                    y.stop + shifts[i - 1, j],
                    1,
                ))
            slices = new_slices

        new_slices = [slice(None)] * len(drift_im_shape_round)
        im_slices = [slice(None)] * len(drift_im_shape_round)

        for j, y in enumerate(dim_utils.spatial_axis()):
            new_slices[dim_utils.dim_idx(y)] = slice(
                round(slices[j].start), round(slices[j].stop), 1)

        im_slices[dim_utils.dim_idx('T')] = slice(i, i + 1, 1)
        new_slices = tuple(new_slices)
        im_slices = tuple(im_slices)

        if i % 10 == 0:
            print(i)

        src = zarr_utils.fortify(input_array[im_slices])
        new_image = np.zeros(tp_shape, dtype=result_dtype)

        if new_image[new_slices].shape != src.shape:
            dif_dim = [x - y for x, y in zip(new_image[new_slices].shape, src.shape)]
            adj = list(new_slices)
            for j, y in enumerate(dif_dim):
                if y > 0:
                    adj[j] = slice(adj[j].start + y, adj[j].stop, 1)
                elif y < 0:
                    if adj[j].start - y >= 0:
                        adj[j] = slice(adj[j].start + y, adj[j].stop, 1)
                    elif adj[j].stop + y < result.shape[j]:
                        adj[j] = slice(adj[j].start, adj[j].stop + y, 1)
            new_slices = tuple(adj)

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


def _af_denoise_frame(slab, dtype, denoise_fun, denoise_params):
    """Denoise one frame (tv / wavelet), rescaled to the dtype's max. Returns slab-shaped float."""
    chunk = np.squeeze(slab)
    if denoise_fun == 'tv':
        r = skimage.restoration.denoise_tv_chambolle(
            chunk, weight=denoise_params.get('weight', 0.1), channel_axis=None) * np.iinfo(dtype).max
    elif denoise_fun == 'wavelet':
        r = skimage.restoration.denoise_wavelet(
            chunk, channel_axis=None, convert2ycbcr=False,
            method=denoise_params.get('method', 'BayesShrink'),
            mode=denoise_params.get('mode', 'soft'), rescale_sigma=True) * np.iinfo(dtype).max
    else:
        return slab
    return r.reshape(slab.shape)


def _af_rolling_ball_frame(slab, is_3d, radius, padding):
    """Rolling-ball background subtraction on one frame (2D disk / 3D ellipsoid), cropped to the
    non-zero interior with `padding` (zeros outside). Returns slab-shaped."""
    im = np.squeeze(slab)
    edges = non_zero_edges(im)
    out = np.zeros_like(im)
    if is_3d:
        crop = (slice(None),
                slice(edges['tl'][1] + padding, edges['br'][1] - padding, 1),
                slice(edges['tl'][2] + padding, edges['br'][2] - padding, 1))
        out[crop] = im[crop] - skimage.restoration.rolling_ball(
            im[crop], kernel=skimage.restoration.ellipsoid_kernel((1, radius, radius), 0.1))
    else:
        crop = (slice(edges['tl'][0] + padding, edges['br'][0] - padding, 1),
                slice(edges['tl'][1] + padding, edges['br'][1] - padding, 1))
        out[crop] = im[crop] - skimage.restoration.rolling_ball(im[crop], radius=radius)
    return out.reshape(slab.shape)


def _af_top_hat_frame(slab, radius):
    """White top-hat (background suppression) on one frame. Returns slab-shaped."""
    im = np.squeeze(slab)
    footprint = (np.ones((3, 2 * radius + 1, 2 * radius + 1)) if im.ndim >= 3
                 else skimage.morphology.disk(radius))
    return skimage.morphology.white_tophat(im, footprint=footprint).reshape(slab.shape)


# ── Per-timepoint slab helpers (bounded AF — a whole channel-timecourse in float64 is what OOM'd) ──

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


def _stream_division_channel(
        data, out, dim_utils, channel_idx, out_ch, inv_ch, correction_channel_idx,
        channel_percentile, correction_percentile, correction_mode, summary_mode, summary_percentile,
        correction_range, median_filter, generate_inverse, gaussian_sigma, apply_gaussian,
        rolling_ball_radius, rolling_ball_padding, top_hat_radius, logfile_utils):
    """Divide-mode AF for one channel, streamed one timepoint at a time into ``out`` (peak memory =
    one frame, not the whole channel). The three global-per-channel percentiles — channel background,
    correction background, and the final rescale window — are gathered from streamed histograms; every
    other op (subtract / divide / rescale / inverse / median / gaussian / rolling-ball / top-hat) is
    frame-local. Result matches the old whole-channel path to within the histogram bin resolution.
    See docs/todo/ZARR_STREAMING_PLAN.md (AF rework)."""
    T = dim_utils.dim_val('T') if dim_utils.is_timeseries() else 1
    dt = data.dtype
    integer = np.issubdtype(dt, np.integer)
    rescale = float(np.iinfo(dt).max) if integer else 255.0
    nbins = (int(np.iinfo(dt).max) + 1) if integer else 256
    hi = float(nbins)                     # corrected ratio is bounded by (0, maxval+1] = (0, nbins]

    sigma = [0] * len(data.shape)
    for d in dim_utils.spatial_axis():
        sigma[dim_utils.dim_idx(d)] = gaussian_sigma
    fp = None
    if median_filter > 0:
        fp = (skimage.morphology.ball(median_filter) if dim_utils.is_3D()
              else skimage.morphology.disk(median_filter)).astype(bool)

    do_ch_sub, do_corr_sub = channel_percentile > 0, correction_percentile > 0

    # Pass 1 — global background percentiles (integer histograms; exact bins for integer data)
    H_ch, H_corr = np.zeros(nbins, np.int64), np.zeros(nbins, np.int64)
    for t in range(T):
        if do_ch_sub:
            ch = _af_slab(data, dim_utils, channel_idx, t)
            H_ch += np.bincount(np.clip(ch, 0, nbins - 1).astype(np.int64).ravel(), minlength=nbins)[:nbins]
        if do_corr_sub:
            ci = _af_correction_slab(data, dim_utils, correction_channel_idx, t, summary_mode, summary_percentile)
            H_corr += np.bincount(np.clip(np.rint(ci), 0, nbins - 1).astype(np.int64).ravel(), minlength=nbins)[:nbins]
    val1 = float(intensity_utils.hist_percentile(H_ch, channel_percentile)) if do_ch_sub else None
    val2 = float(intensity_utils.hist_percentile(H_corr, correction_percentile)) if do_corr_sub else None

    def _raw(t):
        img = _af_subtract(_af_slab(data, dim_utils, channel_idx, t), val1)
        corr = _af_subtract(_af_correction_slab(data, dim_utils, correction_channel_idx, t,
                                                summary_mode, summary_percentile), val2)
        return img, (img + 1.0) / (corr + 1.0)

    # Pass 2 — rescale window from the corrected-ratio distribution (divide mode only)
    c_min, c_max = 0.0, 1.0
    if correction_mode == 'divide':
        Hc = np.zeros(nbins, np.int64)
        for t in range(T):
            _, corrected = _raw(t)
            idx = np.clip(corrected / hi * (nbins - 1), 0, nbins - 1).astype(np.int64)
            Hc += np.bincount(idx.ravel(), minlength=nbins)[:nbins]
        c_min = intensity_utils.hist_percentile(Hc, correction_range[0]) / (nbins - 1) * hi
        c_max = intensity_utils.hist_percentile(Hc, correction_range[1]) / (nbins - 1) * hi

    # Pass 3 — apply + write per timepoint
    for t in range(T):
        img, corrected = _raw(t)
        if correction_mode == 'divide':
            denom = (c_max - c_min) if c_max > c_min else 1.0
            corrected = np.clip((corrected - c_min) / denom * rescale, 0, rescale)
        else:
            corrected = img
        inverse = (img + 1.0) / (corrected + 1.0) if (generate_inverse and inv_ch is not None) else None

        if fp is not None:
            f = fp
            for _ in range(corrected.ndim - f.ndim):
                f = np.expand_dims(f, axis=0)
            corrected = scipy.ndimage.median_filter(corrected, footprint=f)
            if inverse is not None:
                inverse = scipy.ndimage.median_filter(inverse, footprint=f)
        if apply_gaussian:
            corrected = scipy.ndimage.gaussian_filter(corrected, sigma=sigma)
            if inverse is not None:
                inverse = scipy.ndimage.gaussian_filter(inverse, sigma=sigma)

        # cast to the stored dtype BEFORE rolling ball / top hat (matches the old per-channel order,
        # which cast the corrected channel to int before those spatial ops)
        corrected = corrected.astype(out.dtype)
        if inverse is not None:
            inverse = inverse.astype(out.dtype)
        is_3d = dim_utils.is_3D()
        if rolling_ball_radius > 0:           # corrected only, per the old order
            corrected = _af_rolling_ball_frame(corrected, is_3d, rolling_ball_radius, rolling_ball_padding)
        if top_hat_radius > 0:                # both corrected and inverse
            corrected = _af_top_hat_frame(corrected, top_hat_radius)
            if inverse is not None:
                inverse = _af_top_hat_frame(inverse, top_hat_radius)

        _af_write_slab(out, dim_utils, out_ch, t, corrected)
        if inverse is not None:
            _af_write_slab(out, dim_utils, inv_ch, t, inverse)


def _af_inverse_channels(af_combinations, dim_utils):
    """Channel indices (ascending) that produce an appended inverse channel — a combination with
    both ``generateInverse`` and at least one division channel. Shared by the output-shape helper
    and the streaming writer so both agree on the appended-channel layout."""
    combos = {int(i): x for i, x in af_combinations.items()}
    return sorted(i for i, x in combos.items()
                  if x.get('generateInverse', False) and len(x.get('divisionChannels', [])) > 0)


def af_correction_output_shape(input_array, dim_utils, af_combinations):
    """Shape of the AF-corrected output: same as input but with the channel axis widened by one
    channel per combination that requests an inverse (inverses are appended after the C corrected
    channels). Lets a caller size the on-disk output store before ``af_correct_image`` fills it."""
    n_inverse = len(_af_inverse_channels(af_combinations, dim_utils))
    shape = list(input_array.shape)
    shape[dim_utils.dim_idx('C')] = dim_utils.dim_val('C') + n_inverse
    return tuple(shape)


def _stream_simple_channel(input_image, out, i, dim_utils, recipe, sigma,
                           apply_gaussian_to_others, logfile_utils):
    """Non-division output channel, streamed one timepoint at a time: optional denoise (per frame),
    optional gaussian-to-others, then rolling ball / top hat — all frame-local. ``recipe`` is the
    channel's af_combination entry, or None for a channel not covered by any combination."""
    T = dim_utils.dim_val('T') if dim_utils.is_timeseries() else 1
    dt = input_image.dtype
    x = recipe or {}
    denoise_fun = x.get('denoiseFun', 'NONE') if recipe is not None else 'NONE'
    r = x.get('rollingBallRadius', 0) if recipe is not None else 0
    th = x.get('topHatRadius', 0) if recipe is not None else 0
    if denoise_fun == 'wavelet':
        dp = {'method': x.get('waveletMethod', 'BayesShrink'), 'mode': x.get('waveletMode', 'soft')}
    elif denoise_fun == 'tv':
        dp = {'weight': x.get('tvWeight', 0.1)}
    else:
        dp = {}

    is_3d = dim_utils.is_3D()
    for t in range(T):
        base = _af_slab(input_image, dim_utils, i, t)
        if denoise_fun != 'NONE':
            base = _af_denoise_frame(base, dt, denoise_fun, dp).astype(dt)
        if apply_gaussian_to_others:
            base = scipy.ndimage.gaussian_filter(base.astype(np.float64), sigma=sigma).astype(dt)
        if r > 0:
            base = _af_rolling_ball_frame(base, is_3d, r, x.get('rollingBallPadding', 4))
        if th > 0:
            base = _af_top_hat_frame(base, th)
        _af_write_slab(out, dim_utils, i, t, base.astype(out.dtype))


def af_correct_image(input_image, af_combinations, dim_utils, logfile_utils,
                     gaussian_sigma=1, use_dask=False,
                     apply_gaussian=True, apply_gaussian_to_others=True, out=None):
    """Correct autofluorescence for all channels, streamed ONE TIMEPOINT AT A TIME per channel.

    Peak memory is a single channel-frame — casting a whole channel-timecourse to float64 (~47 GB on
    a large movie) was the OOM. Division channels gather their three global-per-channel percentiles
    (channel bg, correction bg, rescale window) from streamed histograms; every other op is
    frame-local. When ``out`` is None a numpy array of the output shape is allocated and returned
    (legacy / small-image path); production passes the on-disk zarr from
    ``open_multiscales_for_writing`` (sized by ``af_correction_output_shape``). Output matches the
    old whole-channel path to within the histogram bin resolution."""
    n_channels = dim_utils.dim_val('C')
    af_combinations = {int(i): x for i, x in af_combinations.items()}

    sigma = [0] * len(input_image.shape)
    for d in dim_utils.spatial_axis():
        sigma[dim_utils.dim_idx(d)] = gaussian_sigma

    # inverse channels are appended after the C corrected channels, in ascending channel order
    inverse_channels = _af_inverse_channels(af_combinations, dim_utils)
    inv_slot = {ch: n_channels + k for k, ch in enumerate(inverse_channels)}

    if out is None:   # legacy/small: allocate the full output (compute still streams per frame)
        out = np.zeros(af_correction_output_shape(input_image, dim_utils, af_combinations),
                       dtype=zarr_utils.native_dtype(input_image.dtype))

    for i in range(n_channels):
        x = af_combinations.get(i)
        div_channels = x.get('divisionChannels', []) if x is not None else []
        if x is not None and len(div_channels) > 0:
            _stream_division_channel(
                input_image, out, dim_utils, channel_idx=i, out_ch=i, inv_ch=inv_slot.get(i),
                correction_channel_idx = div_channels,
                channel_percentile     = x.get('channelPercentile',    80),
                correction_percentile  = x.get('correctionPercentile', 40),
                correction_mode        = x.get('correctionMode',       'divide'),
                summary_mode           = x.get('summaryMode',          'maximum'),
                summary_percentile     = x.get('summaryPercentile',    75),
                correction_range       = (x.get('correctionMin', 1), x.get('correctionMax', 99)),
                median_filter          = x.get('medianFilter', 0),
                generate_inverse       = x.get('generateInverse', False),
                gaussian_sigma         = gaussian_sigma, apply_gaussian = apply_gaussian,
                rolling_ball_radius    = x.get('rollingBallRadius', 0),
                rolling_ball_padding   = x.get('rollingBallPadding', 4),
                top_hat_radius         = x.get('topHatRadius', 0),
                logfile_utils          = logfile_utils)
        else:
            _stream_simple_channel(input_image, out, i, dim_utils, x, sigma,
                                   apply_gaussian_to_others, logfile_utils)
    return out
