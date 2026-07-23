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
from tqdm import tqdm

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.slice_utils as slice_utils


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

    # Use native byte order — big-endian source data (e.g. >u2 from bioformats2raw)
    # is not rendered correctly by napari/OpenGL on little-endian systems.
    result_dtype = input_array.dtype.newbyteorder('=')

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

def subtract_background(array, percentile_min=80):
    if 0 not in array.shape:
        subtract_val = np.percentile(array, percentile_min)
        array[array < subtract_val] = subtract_val
        array = array - subtract_val
    return array


def percentile(a, q=80, axis=0):
    return np.percentile(a, q, axis=axis)


def apply_denoise(data, dim_utils, denoise_fun='wavelet',
                  denoise_params=None):
    if denoise_params is None:
        denoise_params = {'method': 'BayesShrink', 'mode': 'soft'}
    slices = slice_utils.create_slices(data.shape, dim_utils)
    for x in tqdm(slices):
        chunk = np.squeeze(zarr_utils.fortify(data[x]))
        if denoise_fun == 'tv':
            result = skimage.restoration.denoise_tv_chambolle(
                chunk,
                weight=denoise_params.get('weight', 0.1),
                channel_axis=None,
            ) * np.iinfo(data.dtype).max
        elif denoise_fun == 'wavelet':
            result = skimage.restoration.denoise_wavelet(
                chunk,
                channel_axis=None,
                convert2ycbcr=False,
                method=denoise_params.get('method', 'BayesShrink'),
                mode=denoise_params.get('mode', 'soft'),
                rescale_sigma=True,
            ) * np.iinfo(data.dtype).max
        else:
            continue

        dims_diff = len(data[x].shape) - len(result.shape)
        if dims_diff > 0:
            result = np.expand_dims(result, axis=dim_utils.dim_idx('T'))
        if dims_diff > 1:
            result = np.expand_dims(result, axis=dim_utils.dim_idx('C'))
        data[x] = result
    return data


def non_zero_edges(im):
    true_points = np.argwhere(im)
    return {'tl': true_points.min(axis=0), 'br': true_points.max(axis=0)}


def apply_2D_rolling_ball(im, slices, dim_utils, logfile_utils, radius=40, padding=4):
    im_to_process = np.squeeze(zarr_utils.fortify(im[slices]))
    edges = non_zero_edges(im_to_process)
    crop_slices = (
        slice(edges['tl'][0] + padding, edges['br'][0] - padding, 1),
        slice(edges['tl'][1] + padding, edges['br'][1] - padding, 1),
    )
    im[slices] = np.zeros_like(im[slices])
    slices = list(slices)
    slices[dim_utils.dim_idx('Y')] = crop_slices[0]
    slices[dim_utils.dim_idx('X')] = crop_slices[1]
    slices = tuple(slices)
    im[slices] = (im_to_process[crop_slices]
                  - skimage.restoration.rolling_ball(im_to_process[crop_slices], radius=radius))


def apply_3D_rolling_ball(im, slices, dim_utils, logfile_utils, radius=40, padding=4):
    im_to_process = np.squeeze(zarr_utils.fortify(im[slices]))
    edges = non_zero_edges(im_to_process)
    crop_slices = (
        slice(None),
        slice(edges['tl'][1] + padding, edges['br'][1] - padding, 1),
        slice(edges['tl'][2] + padding, edges['br'][2] - padding, 1),
    )
    im[slices] = np.zeros_like(im[slices])
    slices = list(slices)
    slices[dim_utils.dim_idx('Y')] = crop_slices[1]
    slices[dim_utils.dim_idx('X')] = crop_slices[2]
    slices = tuple(slices)
    im[slices] = (im_to_process[crop_slices]
                  - skimage.restoration.rolling_ball(
                      im_to_process[crop_slices],
                      kernel=skimage.restoration.ellipsoid_kernel((1, radius, radius), 0.1)))


def apply_rolling_ball(data, dim_utils, logfile_utils, radius=40, padding=4):
    slices = slice_utils.create_slices(data.shape, dim_utils)
    logfile_utils.log('>> Rolling ball subtraction')
    for cur_slices in slices:
        logfile_utils.log(f'> Slice: {cur_slices}')
        if dim_utils.is_3D():
            apply_3D_rolling_ball(data, cur_slices, dim_utils, logfile_utils, radius, padding)
        else:
            apply_2D_rolling_ball(data, cur_slices, dim_utils, logfile_utils, radius, padding)
    return data


def apply_top_hat(data, dim_utils, radius=40):
    """White top-hat (background suppression) per spatial slice."""
    slices = slice_utils.create_slices(data.shape, dim_utils)
    for x in tqdm(slices):
        data_slice = np.squeeze(zarr_utils.fortify(data[x]))
        if data_slice.ndim >= 3:
            footprint = np.ones((3, 2 * radius + 1, 2 * radius + 1))
        else:
            footprint = skimage.morphology.disk(radius)
        top_hat_result = skimage.morphology.white_tophat(data_slice, footprint=footprint)

        dims_diff = len(data[x].shape) - len(top_hat_result.shape)
        if dims_diff > 0:
            top_hat_result = np.expand_dims(top_hat_result, axis=dim_utils.dim_idx('T'))
        if dims_diff > 1:
            top_hat_result = np.expand_dims(top_hat_result, axis=dim_utils.dim_idx('C'))
        data[x] = top_hat_result
    return data


def af_correct_channel(
        data, channel_idx, correction_channel_idx, dim_utils,
        channel_percentile=80, correction_percentile=40,
        gaussian_sigma=1, apply_gaussian=True, use_dask=False,
        correction_mode='divide', median_filter=0, generate_inverse=False,
        summary_mode='maximum', summary_percentile=75,
        correction_range=(1, 99)):
    """Correct autofluorescence in one channel by division against reference channels."""
    slices = dim_utils.create_channel_slices(channel_idx)

    # Materialise to numpy
    channel_data = zarr_utils.fortify(data[slices])

    # Build summary correction image from division channels
    correction_parts = [
        zarr_utils.fortify(data[dim_utils.create_channel_slices(x)])
        for x in correction_channel_idx
    ]
    correction_stack = np.stack(correction_parts, axis=0)
    if summary_mode == 'percentile':
        correction_im = np.percentile(correction_stack, summary_percentile, axis=0)
    else:
        correction_im = np.max(correction_stack, axis=0)

    # Background subtraction
    cleaned_image      = subtract_background(channel_data.copy(),  percentile_min=channel_percentile) \
                         if channel_percentile > 0 else channel_data.copy()
    cleaned_correction = subtract_background(correction_im.copy(), percentile_min=correction_percentile) \
                         if correction_percentile > 0 else correction_im.copy()

    # AF correction
    if correction_mode == 'divide':
        corrected = (cleaned_image.astype(np.float64) + 1) / (cleaned_correction.astype(np.float64) + 1)
        rescale   = np.iinfo(data.dtype).max if np.issubdtype(data.dtype, np.integer) else 255.0
        c_min     = np.percentile(corrected.ravel(), correction_range[0])
        c_max     = np.percentile(corrected.ravel(), correction_range[1])
        corrected = ((corrected - c_min) / (c_max - c_min)) * rescale
        corrected = np.clip(corrected, 0, rescale)
    else:
        corrected = cleaned_image.astype(np.float64)

    inverse = None
    if generate_inverse:
        inverse = (cleaned_image.astype(np.float64) + 1) / (corrected + 1)

    # Median filter with isotropic footprint (ball in 3D, disk in 2D)
    if median_filter > 0:
        if dim_utils.is_3D():
            fp = skimage.morphology.ball(median_filter).astype(bool)
        else:
            fp = skimage.morphology.disk(median_filter).astype(bool)
        # Expand leading non-spatial dims (T, C) to match array ndim
        for _ in range(len(corrected.shape) - fp.ndim):
            fp = np.expand_dims(fp, axis=0)
        corrected = scipy.ndimage.median_filter(corrected, footprint=fp)
        if inverse is not None:
            inverse = scipy.ndimage.median_filter(inverse, footprint=fp)

    # Gaussian filter
    if apply_gaussian:
        sigma = [0] * len(corrected.shape)
        for d in dim_utils.spatial_axis():
            sigma[dim_utils.dim_idx(d)] = gaussian_sigma
        corrected = scipy.ndimage.gaussian_filter(corrected, sigma=sigma)
        if inverse is not None:
            inverse = scipy.ndimage.gaussian_filter(inverse, sigma=sigma)

    dt = data.dtype
    corrected = corrected.astype(dt)
    if inverse is not None:
        inverse = inverse.astype(dt)

    return corrected, inverse


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


def _af_process_channel(input_image, i, af_combinations, dim_utils, logfile_utils,
                        sigma, gaussian_sigma, apply_gaussian, apply_gaussian_to_others,
                        use_dask=False):
    """Produce the corrected data (and optional inverse) for output channel ``i``, replicating the
    exact operation order of the original whole-image path. Channels are independent — AF division
    always reads the ORIGINAL ``input_image``, never another output channel — so each can be
    computed and written on its own, which is what lets the caller stream channel-by-channel.
    Returns ``(corrected_np, inverse_np_or_None)``."""
    x = af_combinations.get(i)
    ch_slice = dim_utils.create_channel_slices(i)

    if x is None:
        # channel not covered by any combination
        if apply_gaussian_to_others:
            return scipy.ndimage.gaussian_filter(
                zarr_utils.fortify(input_image[ch_slice]).astype(np.float64), sigma=sigma
            ).astype(input_image.dtype), None
        return zarr_utils.fortify(input_image[ch_slice]), None

    inverse = None

    # Denoise the channel's own data. NOTE: when division channels are present this result is
    # overwritten below — af_correct_channel recomputes from the ORIGINAL input, not the denoised
    # channel. Preserved verbatim from the original path (denoise only takes effect without division).
    base = zarr_utils.fortify(input_image[ch_slice])
    if x.get('denoiseFun', 'NONE') != 'NONE':
        fn = x['denoiseFun']
        if fn == 'wavelet':
            dp = {'method': x.get('waveletMethod', 'BayesShrink'),
                  'mode':   x.get('waveletMode',   'soft')}
        elif fn == 'tv':
            dp = {'weight': x.get('tvWeight', 0.1)}
        else:
            dp = {}
        base = apply_denoise(base, dim_utils, fn, dp)

    div_channels = x.get('divisionChannels', [])
    if len(div_channels) > 0:
        base, inverse = af_correct_channel(
            input_image, i, div_channels, dim_utils=dim_utils,
            channel_percentile    = x.get('channelPercentile',    80),
            correction_percentile = x.get('correctionPercentile', 40),
            correction_mode       = x.get('correctionMode',       'divide'),
            summary_mode          = x.get('summaryMode',          'maximum'),
            summary_percentile    = x.get('summaryPercentile',    75),
            correction_range      = (x.get('correctionMin', 1),
                                     x.get('correctionMax', 99)),
            median_filter         = x.get('medianFilter', 0),
            generate_inverse      = x.get('generateInverse', False),
            gaussian_sigma        = gaussian_sigma,
            apply_gaussian        = apply_gaussian,
            use_dask              = use_dask,
        )
    elif apply_gaussian_to_others:
        base = scipy.ndimage.gaussian_filter(
            base.astype(np.float64), sigma=sigma).astype(input_image.dtype)

    # Rolling ball (applied after correction, division or not)
    r = x.get('rollingBallRadius', 0)
    if r > 0:
        base = apply_rolling_ball(base, dim_utils, logfile_utils, r, x.get('rollingBallPadding', 4))

    # Top hat
    th = x.get('topHatRadius', 0)
    if th > 0:
        base = apply_top_hat(base, dim_utils, th)
        if inverse is not None:
            inverse = apply_top_hat(inverse, dim_utils, th)

    return base, inverse


def af_correct_image(input_image, af_combinations, dim_utils, logfile_utils,
                     gaussian_sigma=1, use_dask=False,
                     apply_gaussian=True, apply_gaussian_to_others=True, out=None):
    """Correct autofluorescence for all channels.

    Processes ONE CHANNEL AT A TIME (channels are independent — see ``_af_process_channel``). When
    ``out`` is given (a pre-created on-disk zarr sized by ``af_correction_output_shape``), each
    corrected/inverse channel is written straight to disk and freed, so the whole T×C×Z×Y×X image
    never lives in RAM. When ``out`` is None the channels are concatenated and returned as a dask
    array (legacy / small-image path). Both paths are byte-identical."""
    n_channels = dim_utils.dim_val('C')
    af_combinations = {int(i): x for i, x in af_combinations.items()}

    sigma = [0] * len(input_image.shape)
    for d in dim_utils.spatial_axis():
        sigma[dim_utils.dim_idx(d)] = gaussian_sigma

    # native byte order — big-endian source (e.g. >u2 from bioformats2raw) mis-renders in napari
    out_dtype = input_image.dtype.newbyteorder('=')

    # inverse channels are appended after the C corrected channels, in ascending channel order
    inverse_channels = _af_inverse_channels(af_combinations, dim_utils)
    inv_slot = {ch: n_channels + k for k, ch in enumerate(inverse_channels)}

    output_parts  = []   # (legacy path) corrected channels, in channel order
    inverse_parts = []   # (legacy path) inverse channels, in ascending channel order

    for i in range(n_channels):
        corrected, inverse = _af_process_channel(
            input_image, i, af_combinations, dim_utils, logfile_utils,
            sigma, gaussian_sigma, apply_gaussian, apply_gaussian_to_others,
            use_dask=use_dask)

        if out is not None:
            out[dim_utils.create_channel_slices(i)] = zarr_utils.fortify(corrected).astype(out_dtype)
            if inverse is not None:
                out[dim_utils.create_channel_slices(inv_slot[i])] = \
                    zarr_utils.fortify(inverse).astype(out_dtype)
            corrected = inverse = None   # free before the next channel
        else:
            output_parts.append(zarr_utils.fortify(corrected))
            if inverse is not None:
                inverse_parts.append(zarr_utils.fortify(inverse))

    if out is not None:
        return out

    output_np = np.concatenate(output_parts + inverse_parts, axis=dim_utils.dim_idx('C'))
    output_np = output_np.astype(out_dtype)
    return da.from_array(output_np, chunks='auto')
