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


def channel_histograms(arr, channel_axis, channels=None):
    """
    One integer histogram per channel over the whole stack (all axes except `channel_axis`).

    Integer dtype only — the histogram is indexed by pixel value in [0, iinfo(dtype).max]. Returns a
    list of 1-D numpy arrays (length = max value + 1). Streams over dask chunks (bounded memory).

    `channels`: optional list of channel indices to histogram (default: all). The returned list is
    aligned with `channels` when given — so callers that only need a subset (e.g. segmentation
    normalising just its cell/nuc channels) don't pay to scan every channel.
    """
    if not np.issubdtype(arr.dtype, np.integer):
        raise ValueError(f"channel_histograms requires an integer dtype, got {arr.dtype}")
    nbins = int(np.iinfo(arr.dtype).max) + 1
    chans = range(_n_channels(arr, channel_axis)) if channels is None else channels
    hists = []
    for c in chans:
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


def robust_hist_max(hist, min_count):
    """Highest value that at least ``min_count`` voxels attain — an **outlier-rejected maximum**.

    The true max (``nonzero(hist)[-1]``) is decided by a single voxel, which makes it useless as a
    rescale ceiling: measured on a real 181-frame movie, the top six occupied bins held **exactly one
    voxel each**, so one hot pixel in 5.88 billion set the output scale of the whole image. A
    percentile is no better here, only differently wrong — the signal in a fluorescence ratio lives in
    the extreme tail (p99.99 was 52 against a max of 256), so any percentile near 100 cuts into real
    structure.

    What actually separates a hot pixel from a bright cell is not its value but how many voxels share
    it: one versus thousands. Hence a count threshold. It also **survives subsampling**, which a max
    cannot — a structure of 1000 voxels still has ~60 after a 16× stride, so a strided pass gives the
    same answer, while hunting a single voxel fails as soon as you skip it. Measured: identical
    ceilings under z::2 / xy::4 / both at ``min_count`` 10 000.

    Returns 0 for an empty histogram, and falls back to the true max if NO value reaches
    ``min_count`` (a tiny or nearly-empty image — better a wide ceiling than a zero one).

    **Trap:** the background bin usually holds far more voxels than anything else, so a ``min_count``
    larger than the signal population returns the *background* level rather than nothing — a ceiling
    at or below the peak, which makes a rescale window degenerate. The caller owns that check, because
    only it knows which bins are background: see `correction_utils.af_division_stats`, which floors the
    ceiling above the derived background.
    """
    h = np.asarray(hist)
    ok = np.nonzero(h >= int(min_count))[0]
    if ok.size:
        return int(ok[-1])
    nz = np.nonzero(h)[0]
    return int(nz[-1]) if nz.size else 0


def triangle_threshold(hist):
    """Zack's triangle threshold on a histogram — the bin furthest from the line joining the
    histogram's peak to its last occupied bin.

    Zack GW, Rogers WE, Latt SA (1977) *Automatic measurement of sister chromatid exchange frequency*,
    J Histochem Cytochem 25(7):741-753. doi:10.1177/25.7.70454

    Implemented here rather than via `skimage.filters.threshold_triangle`, which takes an image and
    not a histogram — we already have the histogram, and the images are too large to hand over whole.
    Chosen as the default over Otsu because it is designed for exactly this shape: one dominant
    background peak with a long signal tail. Otsu assumes two comparable classes and collapses to the
    background bin when the histogram is 95% zeros, which fluorescence channels routinely are.
    """
    h = np.asarray(hist, dtype=np.float64)
    nz = np.nonzero(h)[0]
    if nz.size < 2:
        return float(nz[0]) if nz.size else 0.0
    peak = int(nz[int(np.argmax(h[nz]))])
    end = int(nz[-1])
    if end <= peak:
        return float(peak)
    x = np.arange(peak, end + 1, dtype=np.float64)
    y = h[peak:end + 1]
    x0, y0, x1, y1 = float(peak), float(h[peak]), float(end), float(h[end])
    # perpendicular distance to the peak->end line, up to the constant 1/|(x1-x0, y1-y0)|
    dist = np.abs((y1 - y0) * (x - x0) - (x1 - x0) * (y - y0))
    return float(x[int(np.argmax(dist))])


#: Methods `background_threshold` accepts. `"none"` disables subtraction (threshold 0).
BACKGROUND_METHODS = ('triangle', 'otsu', 'none')


def background_threshold(hist, method='triangle', ignore_zero=True):
    """The level at or below which a channel is background, derived rather than dialled in.

    Replaces two hand-tuned percentiles in the AF task (`channelPercentile`, `correctionPercentile`)
    which had no defensible value — they were fitted per dataset and never revisited.

    ``ignore_zero`` drops the zero bin first, and it is load-bearing: measured on real channels, 91-95%
    of voxels are **exactly zero** (already background-subtracted upstream), so every threshold
    computed over the full histogram collapses to 0 and the correction then divides by sensor noise.
    Excluding it makes the estimate describe the population that actually carries signal.
    """
    if method not in BACKGROUND_METHODS:
        raise ValueError(f'unknown background method {method!r}; expected one of {BACKGROUND_METHODS}')
    if method == 'none':
        return 0.0
    h = np.asarray(hist).astype(np.int64).copy()
    if ignore_zero and h.size:
        h[0] = 0
    if int(h.sum()) == 0:
        return 0.0
    if method == 'triangle':
        return triangle_threshold(h)
    from skimage.filters import threshold_otsu       # lazy: skimage import is not free
    return float(threshold_otsu(hist=(h, np.arange(h.size))))


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
