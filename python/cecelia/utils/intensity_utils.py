"""
Whole-stack intensity statistics, computed from streamed per-channel histograms.

Used by AF correction (`correction_utils.af_weight_stats` — the derived background level;
`af_output_stats` — clip stats) and by segmentation normalisation (`segmentation_utils` — percentile
bounds per channel).

Why a histogram (not `np.percentile`): the input is 16-bit integer data that can be tens of GB, so we
cannot materialise a channel to sort it. A single streamed `bincount` per channel (65 536 bins
≈ 256 KB) is exact for integer data and gives min/max, any percentile, a threshold and the clip stats
— all from one pass over the stack.
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


def clip_stats(hist, vmin, vmax):
    """
    How much of a channel falls outside an intensity window, from its histogram. Pure/JSON-friendly.

    Sole caller today is `correction_utils.af_output_stats`, which reports what the corrected
    channel clips.

    - clipLowFrac / clipHighFrac: fraction of pixels strictly outside [vmin, vmax] — i.e. what the
      window would saturate at each end.
    - trueMax / p999: to spot a hot pixel pinning the max — trueMax >> p999 means a true-max window
      would squash the real signal into the bottom of the range.
    - rangeSpan: vmax - vmin (0 ⇒ flat channel ⇒ nothing to scale).
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
