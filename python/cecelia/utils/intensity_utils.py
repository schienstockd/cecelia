"""
Intensity-range utilities for 16→8-bit conversion on import.

The manual Fiji workflow for large low-dynamic-range acquisitions was: rescale the histogram, then
save as 8-bit. This module is the automated core of that: per-channel intensity windows computed
over the WHOLE stack, and a lazy uint8 rescale.

Why a per-channel histogram (not `np.percentile`): the input is 16-bit integer data that can be tens
of GB, so we cannot materialise a channel to sort it. A single streamed `bincount` per channel
(65 536 bins ≈ 256 KB) is exact for integer data and gives min/max, any percentile, and the clip
stats QC needs — all from one pass. The rescale itself is returned as a lazy dask array so
`create_multiscales` writes it chunk-by-chunk with bounded memory.

Default window is the channel's TRUE [min, max] (lo=0 / hi=100 percentile): nothing is clipped, and
the (narrow) signal fills the full 0–255 range instead of collapsing into the first few codes the
way a blind cast from [0, 65535] would. See docs/todo/IMPORT_RESCALE_PLAN.md.
"""
import numpy as np

try:
    import dask.array as da
    _HAS_DASK = True
except ImportError:  # dask is an IO-tier dep; guard so pure-numpy callers still work
    _HAS_DASK = False


def _is_dask(a):
    return _HAS_DASK and isinstance(a, da.Array)


def _take_channel(arr, channel_axis, c):
    """One channel as an array of the same kind (numpy→numpy, dask→dask)."""
    if channel_axis is None:
        return arr
    idx = [slice(None)] * arr.ndim
    idx[channel_axis] = c
    return arr[tuple(idx)]


def _n_channels(arr, channel_axis):
    return 1 if channel_axis is None else int(arr.shape[channel_axis])


def channel_histograms(arr, channel_axis):
    """
    One integer histogram per channel over the whole stack (all axes except `channel_axis`).

    Integer dtype only — the histogram is indexed by pixel value in [0, iinfo(dtype).max]. Returns a
    list of 1-D numpy arrays (length = max value + 1), one per channel. Streams over dask chunks.
    """
    if not np.issubdtype(arr.dtype, np.integer):
        raise ValueError(f"channel_histograms requires an integer dtype, got {arr.dtype}")
    nbins = int(np.iinfo(arr.dtype).max) + 1
    hists = []
    for c in range(_n_channels(arr, channel_axis)):
        flat = _take_channel(arr, channel_axis, c).ravel()
        if _is_dask(flat):
            hist = da.bincount(flat, minlength=nbins).compute()
        else:
            hist = np.bincount(np.asarray(flat), minlength=nbins)
        hists.append(np.asarray(hist))
    return hists


def hist_percentile(hist, pct):
    """Value at percentile `pct` (0–100) from an integer-value histogram (0 if empty)."""
    total = int(hist.sum())
    if total == 0:
        return 0
    cdf = np.cumsum(hist)
    target = (pct / 100.0) * total
    return int(np.searchsorted(cdf, target))


def range_from_hist(hist, lo_pct=0.0, hi_pct=100.0):
    """
    `(vmin, vmax)` window from a channel histogram.

    lo_pct<=0 → true min (first non-empty value); hi_pct>=100 → true max (last non-empty value) —
    the "just take the highest value" default. Otherwise the respective percentile.
    """
    nz = np.nonzero(hist)[0]
    if nz.size == 0:
        return 0.0, 0.0
    vmin = int(nz[0]) if lo_pct <= 0.0 else hist_percentile(hist, lo_pct)
    vmax = int(nz[-1]) if hi_pct >= 100.0 else hist_percentile(hist, hi_pct)
    return float(vmin), float(vmax)


def clip_stats(hist, vmin, vmax):
    """
    QC stats for a channel's rescale, from its histogram + chosen window. Pure/JSON-friendly.

    - clipLowFrac / clipHighFrac: fraction of pixels strictly outside [vmin, vmax] (→ saturated
      to 0 / 255). Zero for the true-min/max default; non-zero only when a percentile trims the tail.
    - trueMax / p999: to spot a hot pixel pinning the max — trueMax >> p999 means the true-max window
      squashes the real signal, so the user should lower the high percentile.
    - rangeSpan: vmax - vmin (0 ⇒ flat channel ⇒ blank output).
    """
    total = int(hist.sum())
    nz = np.nonzero(hist)[0]
    true_max = int(nz[-1]) if nz.size else 0
    if total == 0:
        return {"total": 0, "clipLowFrac": 0.0, "clipHighFrac": 0.0,
                "p999": 0, "trueMax": true_max, "rangeSpan": float(vmax - vmin)}
    lo = int(round(vmin)); hi = int(round(vmax))
    clip_low = int(hist[:lo].sum()) if lo > 0 else 0
    clip_high = int(hist[hi + 1:].sum()) if hi + 1 < len(hist) else 0
    return {
        "total": total,
        "clipLowFrac": clip_low / total,
        "clipHighFrac": clip_high / total,
        "p999": hist_percentile(hist, 99.9),
        "trueMax": true_max,
        "rangeSpan": float(vmax - vmin),
    }


def _rescale_block(sub, vmin, vmax, xp):
    denom = (vmax - vmin) if vmax > vmin else 1.0
    scaled = (sub.astype("float32") - vmin) / denom * 255.0
    return xp.clip(scaled, 0, 255).astype("uint8")


def rescale_stack_to_uint8(arr, channel_axis, ranges):
    """
    Rescale each channel by its `(vmin, vmax)` window and cast to uint8.

    Returns a dask array when `arr` is dask (lazy — evaluated chunk-by-chunk on write), else numpy.
    `ranges` is a list of `(vmin, vmax)` aligned with the channel axis (length 1 when no C axis).
    """
    xp = da if _is_dask(arr) else np
    if channel_axis is None:
        vmin, vmax = ranges[0]
        return _rescale_block(arr, vmin, vmax, xp)
    parts = []
    for c in range(_n_channels(arr, channel_axis)):
        vmin, vmax = ranges[c]
        sub = _rescale_block(_take_channel(arr, channel_axis, c), vmin, vmax, xp)
        parts.append(xp.expand_dims(sub, channel_axis))
    return xp.concatenate(parts, axis=channel_axis)
