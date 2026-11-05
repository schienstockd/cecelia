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
import itertools

import numpy as np


def _n_channels(arr, channel_axis):
    return 1 if channel_axis is None else int(arr.shape[channel_axis])


def channel_histograms(arr, channel_axis, channels=None, steps=None, on_plane=None):
    """
    One integer histogram per channel over the whole stack (all axes except `channel_axis`).

    Integer dtype only — the histogram is indexed by pixel value in [0, iinfo(dtype).max]. Returns a
    list of 1-D numpy arrays (length = max value + 1).

    `arr` is a zarr level or a numpy array, with Y/X as its last two axes. It is read ONE PLANE AT A
    TIME and the planes' `np.bincount`s are summed, so memory stays at one plane plus the histogram.
    Not dask: see docs/todo/DASK_NARROW_PLAN.md Decision 7 for the measurement.

    `channels`: optional list of channel indices to histogram (default: all). The returned list is
    aligned with `channels` when given — so callers that only need a subset (e.g. segmentation
    normalising just its cell/nuc channels) don't pay to scan every channel.

    `steps`: optional `{axis: stride}` over the leading (non-Y/X) axes — e.g. `{t_idx: 4}` reads every
    fourth timepoint. Striding here, rather than slicing `arr` first, keeps a zarr level unread.

    `on_plane`: optional `on_plane(c, ix, plane_hist)`, called with each plane's own histogram before it
    is summed — `ix` indexes the leading non-channel axes in order. Per-plane numbers (`PlaneProfile`)
    come out of the same pass at no extra read.
    """
    if not np.issubdtype(arr.dtype, np.integer):
        raise ValueError(f"channel_histograms requires an integer dtype, got {arr.dtype}")
    nbins = int(np.iinfo(arr.dtype).max) + 1
    chans = range(_n_channels(arr, channel_axis)) if channels is None else channels
    steps = steps or {}
    lead = [i for i in range(arr.ndim - 2) if i != channel_axis]
    hists = []
    for c in chans:
        hist = np.zeros(nbins, np.int64)
        for ix in itertools.product(*(range(0, arr.shape[i], steps.get(i, 1)) for i in lead)):
            sl = [slice(None)] * arr.ndim
            if channel_axis is not None:
                sl[channel_axis] = c
            for i, v in zip(lead, ix):
                sl[i] = v
            plane = np.bincount(np.asarray(arr[tuple(sl)]).ravel(), minlength=nbins)[:nbins]
            hist += plane
            if on_plane is not None:
                on_plane(c, ix, plane)
        hists.append(hist)
    return hists


def stack_axis_indices(ngff_axes, ndim, fallback):
    """`{'C': i|None, 'T': i|None, 'Z': i|None}` for a store's level array.

    From the store's own NGFF axis names (`zarr_utils.read_axes`) when they cover every dimension —
    they describe the array as written. `fallback` (a dict of the same shape, e.g. from `DimUtils` over
    the OME-XML) is used only when the store declares no usable axes: an OME-XML `DimensionOrder`
    other than XYZCT does not describe a bioformats2raw TCZYX store, and trusting it files frames
    under depths.
    """
    if ngff_axes and len(ngff_axes) == ndim:
        low = [str(a).lower() for a in ngff_axes]
        return {d: (low.index(d.lower()) if d.lower() in low else None) for d in ('C', 'T', 'Z')}
    return dict(fallback)


class PlaneProfile:
    """Per-plane intensity numbers, collected through `channel_histograms(on_plane=...)` and summarised
    by depth (over time) and by time (over depth).

    Per plane: the 50th and 99th percentile, the zero fraction and the voxels at the plane's own top
    value. `summary(c, top_value)` then counts, per depth / per frame, the voxels at the CHANNEL's
    ceiling (`saturation_stats`' `topValue`) — where in the stack clipping happened, not just whether.
    No verdicts: these are the numbers a reader (or a later finding) reasons with.
    """

    def __init__(self, lead_axes, t_axis=None, z_axis=None):
        self._t = lead_axes.index(t_axis) if t_axis in lead_axes else None
        self._z = lead_axes.index(z_axis) if z_axis in lead_axes else None
        self._rows = {}                       # channel -> list of (t, z, p50, p99, zeroFrac, top, nTop)

    def __call__(self, c, ix, h):
        total = int(h.sum())
        nz = np.flatnonzero(h)
        top = int(nz[-1]) if nz.size else 0
        occupied = h[:top + 1]          # same percentiles, without summing the empty bins above `top`
        self._rows.setdefault(c, []).append((
            ix[self._t] if self._t is not None else 0,
            ix[self._z] if self._z is not None else 0,
            hist_percentile(occupied, 50), hist_percentile(occupied, 99), (int(h[0]) / total) if total else 0.0, top, int(h[top]) if nz.size else 0))

    def drop_t(self, t):
        """Forget every plane of timepoint `t` — one that is not acquisition data (a stopped
        acquisition's zero-filled last frame, `partial_last_timepoint`)."""
        if self._t is None:
            return
        for c, rows in self._rows.items():
            self._rows[c] = [r for r in rows if r[0] != t]

    def summary(self, c, top_value):
        """`{'byZ': {...}, 'byT': {...}}` — per depth / per frame: median p50 and p99, mean zero
        fraction, and the count of voxels at the channel ceiling `top_value` (0 where the plane's top is
        below it, and everywhere when `top_value` is None — pass `profile_ceiling(stats)`, which is
        None unless the channel clipped). Lists are indexed by z / t position."""
        rows = self._rows.get(c, [])
        def group(key):
            out = {}
            for r in rows:
                out.setdefault(r[key], []).append(r)
            keys = sorted(out)
            g = [out[k] for k in keys]
            return {
                'p50':      [float(np.median([r[2] for r in rs])) for rs in g],
                'p99':      [float(np.median([r[3] for r in rs])) for rs in g],
                'zeroFrac': [round(float(np.mean([r[4] for r in rs])), 4) for rs in g],
                'clipped':  [int(sum(r[6] for r in rs if top_value and r[5] == top_value)) for rs in g],
            }
        return {'byZ': group(1), 'byT': group(0)}


def profile_ceiling(stats):
    """The value `PlaneProfile.summary` should count as clipped: the channel's `topValue` when
    `saturation_stats` judged it clipped, else None. A channel's brightest value is only a ceiling
    when voxels piled up there; otherwise it is just the brightest signal."""
    return stats['topValue'] if stats.get('saturated') else None


def hist_percentile(hist, pct):
    """Value at percentile `pct` (0–100) from an integer-value histogram (0 if empty)."""
    total = int(hist.sum())
    if total == 0:
        return 0
    cdf = np.cumsum(hist)
    target = (pct / 100.0) * total
    return int(np.searchsorted(cdf, target))


#: Floor and fraction for the `is_saturated` count threshold: a bin must hold at least
#: ``max(floor, total * frac)`` voxels to count as a population rather than a hot pixel. Scale-free (a
#: fraction of the image) with a floor so a small or synthetic histogram still behaves sensibly.
SATURATION_MIN_FRAC = 1e-6
SATURATION_MIN_FLOOR = 64


def is_saturated(hist, min_count=None):
    """Whether a channel CLIPPED AT ACQUISITION — its brightest occupied bin is shared by enough
    voxels to be real signal rather than a hot pixel.

    **Structural, so it needs no knowledge of the detector's bit depth**, and that is the whole point.
    A fluorescence tail DECAYS — each brighter bin holds fewer voxels than the one below — so the top
    occupied bin is the sparsest. Clipping inverts that: every value the detector could not represent
    is accumulated into the top bin, which then holds MORE voxels than its neighbours. That pile-up is
    the signature, wherever the ceiling happens to sit.

    Both conditions are required. The count floor alone would flag any channel whose brightest bin
    happens to be well populated; the pile-up alone would flag a two-voxel spike as saturation.

    **Why not "fraction of voxels at the dtype maximum"** — the obvious one-liner, and what
    `correction_utils.af_weight_stats` computes for its own `saturatedFrac`: it only works when the
    data fills its dtype. Measured on the nine `kSUFux` movies, which are 12-bit sensor data stored in
    16-bit words: every channel clips at **4095** while the dtype maximum is 65535, so the dtype-max
    fraction reports **0.00000 for every channel of every movie** — including one with 105 voxels piled
    at 4095 against a median of 1 in the bins below it, which this test flags.

    That is an artifact of a 12-bit sensor in a 16-bit container, and the container DECLARES it —
    those files carry ``SignificantBits="12"`` next to ``Type="uint16"``. So a dtype-max test is not
    unfixable, it just has to read the ceiling from `SignificantBits`. The reason this one is
    structural anyway: it needs no metadata, so it still holds when the declaration is absent or
    wrong (`4kS67f` declares 16 significant bits while its data never exceeds 7311).

    **Use a FULL histogram, not a strided one.** The threshold is a voxel count, so subsampling scales
    the pile-up down with it and a marginal channel changes verdict: `kSUFux/RbC99T` ch2 holds 60 at
    the top bin under a ``::6`` stride (below the floor of 64, so not flagged) and ~360 unstrided.

    **The threshold, measured.** Over all 36 channels of the nine `kSUFux` movies (one session,
    identical settings) it flags **4, in 4 different movies** — independently reproducing the count the
    earlier rescale work arrived at. The separation is real but not wide: flagged channels hold
    1.10-1.42e-6 of their voxels at the ceiling, while the highest unflagged one holds 5.7e-7. Note
    the ABSOLUTE counts are small (415-534 voxels of ~377 M), so this reports a true clipping event of
    modest extent rather than a ruined channel — which is why the QC finding carries the voxel count
    and leaves the judgement to the reader.
    """
    h = np.asarray(hist)
    nz = np.nonzero(h)[0]
    if nz.size < 2:
        return False
    if min_count is None:
        min_count = max(int(SATURATION_MIN_FLOOR), int(int(h.sum()) * float(SATURATION_MIN_FRAC)))
    top = int(nz[-1])
    if int(h[top]) < int(min_count):
        return False
    # compare against the run of occupied bins just below, not a single neighbour, so one ragged bin
    # in a sparse tail doesn't read as a pile-up
    below = h[nz[max(0, nz.size - 11):nz.size - 1]]
    return bool(int(h[top]) > float(np.median(below)))


def saturation_stats(hist, background_method='triangle'):
    """`is_saturated` plus the numbers behind the verdict, JSON-friendly for QC.

    `topValue` is where the pile-up sits — the detector's effective ceiling, which is worth reporting
    because it is NOT the dtype maximum on sub-16-bit sensor data.

    **Two denominators, and `clippedSignalFrac` is the one to reason with.** `topFrac` divides by every
    voxel in the channel, and these images are ~95% background, so it is diluted by empty space: on the
    nine `kSUFux` movies the clipped channels come out at 1.1-1.4e-6, a number no threshold can be
    argued about because it says as much about how much blank frame there is as about the clipping.
    `clippedSignalFrac` divides by the voxels ABOVE the derived background (`background_threshold`), so
    it answers "how much of what this channel actually recorded got truncated" — 3.9-7.2e-5 for those
    same channels. Both are kept: `topFrac` because the cohort metric already banks it.

    Measured caveat, so nobody reads more into the change than it earns: the better denominator did NOT
    separate the flagged channels more cleanly (a 1.5x gap to the highest unflagged one, against 1.9x
    for `topFrac`). It is a more meaningful quantity, not a sharper discriminator.
    """
    h = np.asarray(hist)
    total = int(h.sum())
    nz = np.nonzero(h)[0]
    top = int(nz[-1]) if nz.size else 0
    n_top = int(h[top]) if nz.size else 0
    n_zero = int(h[0]) if h.size else 0
    signal = 0
    if nz.size > 1:
        bg = background_threshold(h, method=background_method)
        lo = int(np.ceil(bg))
        signal = int(h[lo:].sum()) if lo < h.size else 0
    return {
        'saturated': bool(is_saturated(h)),
        'topValue': top,
        'topCount': n_top,
        'topFrac': (n_top / total) if total else 0.0,
        'signalVoxels': signal,
        'clippedSignalFrac': (n_top / signal) if signal else 0.0,
        # Sparsity fields (CORRECTION_QC_PLAN.md Q-M4). Free — same histogram pass, one extra sum.
        # Shown as-is in `get_image_info` (`img_cleanup_facts`, CLEANUP_FACTS_PLAN D1), no verdict.
        'zeroFrac': (n_zero / total) if total else 0.0,
        'signalFrac': (signal / total) if total else 0.0,
    }


def partial_last_timepoint(occupied):
    """The z-planes an acquisition stopped before writing, or None. `occupied` is a bool `[T, Z]` array:
    does the (t, z) plane hold any non-zero voxel in ANY channel.

    A recording stopped mid-volume leaves its last timepoint with the deepest z-planes never written.
    Bio-Formats keeps that timepoint and fills the missing planes with zeros (OIRReader only drops a
    timepoint that has no complete plane at all), so it arrives as black planes in the last frame.

    Only planes that hold data in EVERY earlier timepoint can be missing: a stopped acquisition loses
    planes it recorded throughout. That excludes planes the source left blank throughout (a crop done
    before import). A plane blank in SOME earlier timepoints but not others is registration padding
    (a z-drift correction shifts each frame and pads the deep end), and then nothing is reported at all:
    with steady drift the shift can step up on the last frame, leaving one more plane blank there than
    in any frame before — a complete timepoint that looks stopped. Acquired data never blanks a whole
    plane in every channel now and then. A miss only means no trim, so the rule errs that way; the one
    case it cannot tell apart is a drift whose ONLY step is on the last frame. The missing planes must
    also be the tail of the recorded planes. Empty planes anywhere else (mid-stack, or mid-series) are a
    different problem and are not reported here.

    Returns `{"t": T-1, "emptyZ": [z, ...], "sizeZ": Z}`, or None when the last timepoint is complete,
    the image has fewer than two timepoints, or the pattern is not a trailing run.
    """
    occ = np.asarray(occupied, dtype=bool)
    if occ.ndim != 2 or occ.shape[0] < 2:
        return None
    n_t, n_z = occ.shape
    earlier = occ[:-1]
    if (earlier.any(axis=0) & ~earlier.all(axis=0)).any():   # blank only sometimes: drift padding
        return None
    recorded = earlier.all(axis=0)                # data in every earlier timepoint
    missing = ~occ[-1] & recorded
    if not missing.any():
        return None
    rec_z = np.flatnonzero(recorded)
    miss_z = np.flatnonzero(missing)
    # a stopped acquisition loses a TAIL: the missing planes are the last len(miss_z) recorded ones
    if not np.array_equal(miss_z, rec_z[len(rec_z) - len(miss_z):]):
        return None
    return {"t": int(n_t - 1), "emptyZ": [int(z) for z in miss_z], "sizeZ": int(n_z)}


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


#: Bins of the compact histogram the data-path panel draws: enough to show a shape, small enough to
#: keep in ccid `meta` per channel.
COMPACT_HIST_BINS = 32

#: Above this zero fraction the median of the background is itself 0, so its spread cannot be measured
#: from what is left (`floor_stats` says what the zeros are).
NOISE_MAX_ZERO_FRAC = 0.5

#: Normal-distribution quantile one standard deviation above the median: Φ(1).
UPPER_SIGMA_Q = 0.8413


def _wquantile(h, lo, hi, q):
    """Quantile `q` (0–1) of the values in bins [lo, hi] of integer histogram `h`, or None when empty."""
    seg = h[lo:hi + 1]
    return float(lo + hist_percentile(seg, q * 100)) if int(seg.sum()) else None


#: Floor classification (`floor_stats`). A photon-counting detector writes 0 for "no photon" and about
#: one fixed step per photon, with a discriminator below the first step, so its histogram has an EMPTY
#: run above 0 and then a peak. A background clipped at 0 (offset, or subtracted upstream) is a
#: continuous distribution cut at 0: a spike at 0, then counts that run on from 1 with no gap.
#: The legal `floor_stats` kinds — the one list; the TS union `ChannelFloor.kind`
#: (frontend/src/utils/acquisitionLightPath.ts) and `ome_qc.jl` mirror it.
FLOOR_KINDS = ('photon-counted', 'clipped', 'indeterminate', 'none')
#: Minimum empty values between 0 and the first occupied value to call a gap.
FLOOR_MIN_GAP = 3
#: The first peak must stand this far above the first occupied value: a hard threshold on analog data
#: (values below T set to 0) also leaves a gap, but its counts fall from T instead of rising to a peak.
FLOOR_PEAK_RISE = 1.5
#: An occupied value needs this share of the non-zero voxels to count (hot pixels and rounding).
FLOOR_MIN_SHARE = 1e-5
#: The empty run must end at a CUT: the first occupied value holds at least this share of the first
#: peak. A discriminator cuts the one-photon peak's lower side (4–15 % of the peak on a real FV4000
#: store); an analog distribution's tail fades out to single voxels (< 0.1 %).
FLOOR_EDGE_MIN = 0.01
#: A clipped background piles everything at or below 0 into the 0 bin: a spike at least this many times
#: the counts at 1–5, with no gap after it. An unclipped distribution that merely reaches 0 has a 0 bin
#: like its neighbours.
FLOOR_SPIKE = 3
#: Checked before the spike: counts that fall from 1 like a Poisson ladder (h[2]/h[1] below this, each
#: next ratio smaller) are raw photon counts (step 1) OR a background clipped well below 0 with a narrow
#: spread — a log-concave tail falls the same way. The histogram cannot tell them apart, so neither is
#: claimed (`indeterminate`). A clipped background with a wider spread runs on from 1.
FLOOR_LADDER_FALL = 0.6
#: Photon counting also puts a peak at two photons: a local maximum in [1.6, 2.4] × the step that stands
#: this far above the trough before it. Padding zeros plus an analog background above 0 (a stitched or
#: drift-corrected canvas) also give a gap and a peak, but no second peak at twice its level.
FLOOR_COMB_WINDOW = (1.6, 2.4)
FLOOR_COMB_RISE = 1.5


def _first_peak(h, start):
    """The first local maximum of the (5-wide smoothed) histogram at or after `start`, refined to the
    largest raw bin within 2 of it. The first, not the tallest: at a few photons per voxel the
    two-photon peak can be the taller one, and the discriminator can sit anywhere below the first."""
    sm = np.convolve(h.astype(np.float64), np.ones(5) / 5, mode='same')
    m = start
    while m + 1 < h.size and sm[m + 1] >= sm[m]:
        m += 1
    lo, hi = max(start, m - 2), min(h.size, m + 3)
    return lo + int(np.argmax(h[lo:hi]))


def _has_second_photon_peak(h, step):
    """Whether the (3-wide smoothed) histogram has a local maximum in `FLOOR_COMB_WINDOW` × `step`
    standing `FLOOR_COMB_RISE` above the lowest point between it and the first peak."""
    lo, hi = int(round(FLOOR_COMB_WINDOW[0] * step)), int(round(FLOOR_COMB_WINDOW[1] * step))
    if hi + 1 >= h.size:
        return False
    sm = np.convolve(h.astype(np.float64), np.ones(3) / 3, mode='same')
    m2 = lo + int(np.argmax(sm[lo:hi + 1]))
    if m2 in (lo, hi):                         # an edge of the window: a slope, not a peak
        return False
    trough = float(sm[step:m2 + 1].min())
    return sm[m2] > 0 and sm[m2] >= FLOOR_COMB_RISE * max(trough, 1.0 / 3)


def floor_stats(hist):
    """What the exact zeros of a channel are, read from the low end of its histogram.

    * ``kind`` — ``'photon-counted'`` (empty run above 0 ending at a cut, then a peak, and another at
      twice its value: zeros are voxels where no photon was detected), ``'clipped'`` (a spike at 0
      over a background that runs on from 1: a continuous background cut at 0 by the offset or by
      subtraction upstream), ``'indeterminate'``, or ``'none'`` (no zeros). Shape only: a histogram cannot see
      where the zeros sit, so padding plus a peaked analog channel can still pass — hence "likely".
    * ``firstValue`` — the smallest occupied non-zero value.
    * ``photonStep`` — value of the first peak above the cut (`_first_peak`), taken as the output per
      detected photon (photon-counted only).
    * ``photons`` — p50 and p99.9 of the non-zero voxels divided by ``photonStep``.
    * ``evidence`` — one line saying what the call rests on.
    """
    h = np.asarray(hist).astype(np.int64)
    out = {'kind': 'indeterminate', 'firstValue': None, 'photonStep': None, 'photons': None, 'evidence': None}
    nzt = int(h[1:].sum()) if h.size > 1 else 0
    if h.size == 0 or int(h[0]) == 0:
        out['kind'] = 'none'
        return out
    if nzt == 0:
        out['evidence'] = 'every voxel is 0'
        return out
    occ = np.nonzero(h[1:] >= max(1, FLOOR_MIN_SHARE * nzt))[0]
    if not occ.size:
        return out
    v1 = int(occ[0]) + 1
    out['firstValue'] = v1
    mode = _first_peak(h, v1)
    if (v1 - 1 >= FLOOR_MIN_GAP and h[mode] >= FLOOR_PEAK_RISE * h[v1] and h[v1] >= FLOOR_EDGE_MIN * h[mode]
            and _has_second_photon_peak(h, mode)):
        hn = h.copy()
        hn[0] = 0
        out.update(kind='photon-counted', photonStep=mode,
                   photons={'p50': round(hist_percentile(hn, 50) / mode, 2),
                            'p999': round(hist_percentile(hn, 99.9) / mode, 2)},
                   evidence=f'nothing from 1 to {v1 - 1}, then a peak at {mode}')
    elif v1 == 1 and h.size > 3 and h[2] < FLOOR_LADDER_FALL * h[1] and h[3] * h[1] < h[2] * h[2]:
        out['evidence'] = 'counts fall steeply from 1: raw photon counts or a narrow clipped background'
    elif v1 == 1 and h[0] >= FLOOR_SPIKE * h[1:min(6, h.size)].max(initial=0):
        out.update(kind='clipped', evidence='a spike at 0 over a background that runs on from 1')
    else:
        out['evidence'] = (f'nothing from 1 to {v1 - 1}, but no photon peaks above it' if v1 > 1
                           else 'no spike at 0')
    return out


def range_stats(hist):
    """How much of the range a channel uses: p1, p50 and p99.9 over NON-ZERO voxels, the max, and a
    `COMPACT_HIST_BINS`-bin histogram over [0, max] (all voxels). Non-zero because most channels are
    mostly exact zeros (`zeroFrac` counts them, `floor_stats` says what they are), which
    would pin every low percentile at 0 and say nothing about the signal."""
    h = np.asarray(hist).astype(np.int64)
    nz = np.nonzero(h)[0]
    top = int(nz[-1]) if nz.size else 0
    hn = h.copy()
    if hn.size:
        hn[0] = 0
    pct = {k: (hist_percentile(hn, p) if int(hn.sum()) else None)
           for k, p in (('p1', 1), ('p50', 50), ('p999', 99.9))}
    edges = np.linspace(0, top + 1, COMPACT_HIST_BINS + 1)
    counts = [int(h[int(np.ceil(a)):int(np.ceil(b))].sum()) for a, b in zip(edges[:-1], edges[1:])]
    return {**pct, 'max': top, 'hist': {'binWidth': float((top + 1) / COMPACT_HIST_BINS), 'counts': counts}}


def noise_stats(hist, background_method='triangle', floor=None):
    """Noise floor and a typical-signal SNR, from the histogram alone.

    * ``bgLevel`` — `background_threshold` (zeros ignored when FINDING the level).
    * ``bgMedian`` — median of the voxels at or below ``bgLevel``, zeros included.
    * ``bgSpread`` — the ``UPPER_SIGMA_Q`` quantile of those voxels minus their median: one standard
      deviation for a normal background, read from the upper side only. A background clipped at 0
      loses its LOWER tail; while fewer than half its voxels are 0 the median and everything above it
      are untouched, so this stays right where a MAD would shrink (the zeros' deviations are cut short).
    * ``snr`` — (median of the voxels above ``bgLevel`` − ``bgMedian``) / ``bgSpread``.

    Limits: a histogram does not know where a voxel sits, so dim structure inside the background widens
    the spread (SNR reads low), and Poisson noise grows with signal, so one background spread
    understates the noise on bright structure. When more than ``NOISE_MAX_ZERO_FRAC`` of voxels are
    exactly zero the median itself is 0, and when the spread is 0 there is nothing to divide by:
    ``bgSpread`` and ``snr`` are None and ``untestable`` says why — naming the cause from ``floor``
    (`floor_stats`) when given. A photon-counted channel has no background distribution to measure;
    its counts are the measure (``floor['photons']``).
    """
    h = np.asarray(hist).astype(np.int64)
    total = int(h.sum())
    out = {'bgLevel': None, 'bgMedian': None, 'bgSpread': None, 'snr': None, 'untestable': None,
           'estimator': 'triangle background; spread = p84.13 - median of the background (zeros kept)'}
    if total == 0 or int(h[1:].sum()) == 0:
        out['untestable'] = 'no non-zero voxels'
        return out
    bg = background_threshold(h, method=background_method)
    lo = int(np.floor(bg))
    out['bgLevel'] = float(bg)
    if int(h[0]) / total > NOISE_MAX_ZERO_FRAC:
        kind = (floor or {}).get('kind')
        out['untestable'] = ('photon-counted: zeros are voxels with no photon' if kind == 'photon-counted'
                             else 'background clipped at 0' if kind == 'clipped'
                             else 'most voxels are 0')
        return out
    med = _wquantile(h, 0, lo, 0.5)
    if med is None:
        out['untestable'] = 'no voxels in the background'
        return out
    out['bgMedian'] = med
    spread = _wquantile(h, 0, lo, UPPER_SIGMA_Q) - med
    sig = _wquantile(h, lo + 1, h.size - 1, 0.5) if lo + 1 < h.size else None
    if spread <= 0 or sig is None:
        out['untestable'] = 'background spread is 0' if spread <= 0 else 'no voxels above background'
        return out
    out['bgSpread'] = float(spread)
    out['snr'] = float((sig - med) / spread)
    return out
