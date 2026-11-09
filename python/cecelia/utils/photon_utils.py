"""
How many photons a channel's grey levels stand for, from the image itself — the test-acquisition
checks for noise, dynamic range on the objects and shared structure between channels
(`docs/todo/ACQUISITION_TEST_CHECK_PLAN.md`).

Why not `intensity_utils.noise_stats`: it divides by the spread of the BACKGROUND, which understates the
noise wherever there is signal (Poisson noise grows with it), and on a high-gain detector counting few
photons the background threshold sits at the edge of the zero-photon peak, so single-photon spikes count
as "signal". Measured on an LSM 510/710 two-photon movie that read SNR 39 there: one photon is worth
~600 grey levels and the 12-bit range holds ~6 photons.

These need where a voxel sits (local mean and variance, smoothing), so unlike `intensity_utils` they
work on planes, not histograms. Callers feed a SAMPLE of planes (`data_path.measure_signal`): none of
these numbers is a voxel count, so a sample estimates them without bias, unlike clipping.
"""
import numpy as np

#: Side of the square window for the local mean / local noise variance (`photon_transfer`).
PT_WINDOW = 5
#: Mean-intensity bins the window statistics are grouped into before the line fit.
PT_BINS = 30
#: Windows a bin needs before its median variance is trusted.
PT_MIN_PER_BIN = 200
#: Fewest bins a line is fitted through.
PT_MIN_BINS = 5
#: Unit-variance high-pass: a 4-neighbour Laplacian scaled so i.i.d. noise of variance s^2 comes out
#: with variance s^2 (sum of squared weights = 16 + 4 = 20). Smooth signal cancels to first order.
_LAPLACE = np.array([[0, -1, 0], [-1, 4, -1], [0, -1, 0]], dtype=np.float64) / np.sqrt(20.0)

#: Gaussian sigma (pixels, in-plane) used to turn a photon-starved plane into a local mean flux before
#: masking objects and correlating channels: a voxel holding 0 or 1 photon says nothing on its own.
SMOOTH_SIGMA_PX = 1.5


def window_stats(plane, ceiling=None, win=PT_WINDOW, drop_zeros=True):
    """Local mean and local high-pass variance on a non-overlapping grid of `win`-wide windows of one
    2-D plane. Windows touching the ceiling (clipped, variance cut short) are dropped, and with
    ``drop_zeros`` so are windows touching an exact 0 (a background clipped at 0 loses its lower tail,
    same problem). Pass ``drop_zeros=False`` for a photon-counted channel, whose zeros are voxels that
    caught no photon — data, and most of it (`intensity_utils.floor_stats`)."""
    from scipy import ndimage as ndi
    p = np.asarray(plane, dtype=np.float64)
    if p.ndim != 2 or min(p.shape) < 3 * win:
        return np.zeros(0), np.zeros(0)
    hp = ndi.convolve(p, _LAPLACE, mode='reflect')
    mu = ndi.uniform_filter(p, win, mode='reflect')
    var = ndi.uniform_filter(hp * hp, win, mode='reflect')
    bad = (p <= 0) if drop_zeros else np.zeros(p.shape, dtype=bool)
    if ceiling is not None:
        bad |= p >= ceiling
    # the high-pass reaches one pixel past the window, so the guard does too
    bad = ndi.maximum_filter(bad.astype(np.uint8), win + 2, mode='constant') > 0
    edge = win // 2 + 1
    grid = (slice(edge, p.shape[0] - edge, win), slice(edge, p.shape[1] - edge, win))
    keep = ~bad[grid]
    return mu[grid][keep], var[grid][keep]


def photon_transfer(planes, ceiling=None, offset=None):
    """`fit_photon_transfer` over the `window_stats` of every plane in ``planes`` (2-D arrays)."""
    mus, vs = [], []
    for p in planes:
        m, v = window_stats(p, ceiling)
        mus.append(m)
        vs.append(v)
    return fit_photon_transfer(np.concatenate(mus) if mus else np.zeros(0),
                               np.concatenate(vs) if vs else np.zeros(0), offset=offset)


def fit_photon_transfer(mu, var, offset=None):
    """Grey levels per photon from the variance-against-mean relation of the image's own noise.

    For a photon-counting signal read through a gain, the noise variance rises in a straight line with
    the mean: ``var = g * (mean - offset) + read_var``, where ``g`` is grey levels per (effective)
    photon. The windows of each plane (`window_stats`, pooled by the caller into ``mu`` / ``var``) give
    local means and local noise variances; they are grouped into up to `PT_BINS` bins by mean (fewer when the sample is small), each bin's MEDIAN variance is taken (robust to windows that
    straddle an edge, whose variance carries structure), and a line is fitted through the bins weighted
    by their counts. Single-image noise-model fitting after Foi A, Trimeche M, Katkovnik V, Egiazarian K
    (2008) *Practical Poissonian-Gaussian noise modeling and fitting for single-image raw-data*, IEEE
    Trans Image Process 17(10):1737-1754, doi:10.1109/TIP.2008.2001399 — simplified: a Laplacian
    high-pass and window medians stand in for its wavelet detail coefficients and smooth-region
    selection. Golden-tested on synthetic Poisson-Gaussian images of known gain
    (`test_photon_utils.py`).

    "Effective" photon: an analogue PMT adds pulse-height variance (excess noise), so ``g`` is the gain
    times that factor and the photon numbers are what the noise is WORTH, which is what SNR needs.
    Structure that survives the high-pass biases ``g`` up, i.e. photon numbers down.

    ``offset`` (the zero-photon level, e.g. ``noise_stats``' ``bgMedian``) turns a value into photons
    (`photons`); without it the fit's own zero-variance point stands in, which also absorbs the read
    noise. Returns ``{greyPerPhoton, offset, r2, points: [[mean, var], ...], untestable}``; on too few
    bins or a non-positive slope the numbers are None and ``untestable`` says why.
    """
    mu = np.asarray(mu, dtype=np.float64)
    var = np.asarray(var, dtype=np.float64)
    out = {'greyPerPhoton': None, 'offset': None, 'r2': None, 'points': [], 'untestable': None,
           'estimator': 'variance against mean of a Laplacian high-pass in 5-px windows (after Foi 2008)'}
    if mu.size < PT_MIN_PER_BIN * PT_MIN_BINS:
        out['untestable'] = 'too few unclipped windows'
        return out
    # fewer, fuller bins on a small sample rather than PT_BINS starved ones
    n_bins = min(PT_BINS, mu.size // PT_MIN_PER_BIN)
    edges = np.unique(np.percentile(mu, np.linspace(0, 100, n_bins + 1)))
    pts = []
    for i, (lo, hi) in enumerate(zip(edges[:-1], edges[1:])):
        sel = (mu >= lo) & ((mu < hi) if i < len(edges) - 2 else (mu <= hi))
        n = int(sel.sum())
        if n >= PT_MIN_PER_BIN:
            pts.append((float(np.median(mu[sel])), float(np.median(var[sel])), n))
    if len(pts) < PT_MIN_BINS:
        out['untestable'] = 'too few intensity levels to fit'
        return out
    x, y, n = (np.array(c, dtype=np.float64) for c in zip(*pts))
    w = np.sqrt(n)
    slope, icept = np.linalg.lstsq(np.vstack([x, np.ones_like(x)]).T * w[:, None], y * w, rcond=None)[0]
    out['points'] = [[round(a, 2), round(b, 2)] for a, b in zip(x, y)]
    if not np.isfinite(slope) or slope <= 0:
        out['untestable'] = 'noise does not grow with signal'
        return out
    pred = slope * x + icept
    tot = float(((y - np.average(y, weights=n)) ** 2 * n).sum())
    out['r2'] = float(1 - ((y - pred) ** 2 * n).sum() / tot) if tot > 0 else None
    out['greyPerPhoton'] = float(slope)
    out['offset'] = float(offset) if offset is not None else float(-icept / slope)
    return out


def photons(value, fit):
    """`value` (grey level, scalar or array) in effective photons above the zero-photon level, per a
    `photon_transfer` result; None when the fit is untestable."""
    if not fit or fit.get('greyPerPhoton') is None:
        return None
    return (np.asarray(value, dtype=np.float64) - fit['offset']) / fit['greyPerPhoton']


#: Gaussian sigma (pixels) of the background taken off a plane before channels are correlated
#: (`structure_plane`): well above a cell (~16 px), well below the field, so vignetting and uneven
#: illumination go and cells stay.
STRUCTURE_BG_SIGMA_PX = 25.0


def structure_plane(plane, sigma=SMOOTH_SIGMA_PX, bg_sigma=STRUCTURE_BG_SIGMA_PX):
    """A plane band-passed to cell-scale structure: `smooth_plane` minus a `STRUCTURE_BG_SIGMA_PX`
    background. Two channels share the optics, so they share its slow gradients (vignetting, uneven
    illumination, dimming with depth); correlated raw, those alone read as shared structure (r 0.97
    for unrelated objects under a common vignette). What `shared_structure` is fed."""
    from scipy import ndimage as ndi
    p = np.asarray(plane, dtype=np.float32)
    return ndi.gaussian_filter(p, sigma) - ndi.gaussian_filter(p, bg_sigma)


def smooth_plane(plane, sigma=SMOOTH_SIGMA_PX):
    """A plane as local mean flux: Gaussian in-plane smoothing (`SMOOTH_SIGMA_PX`)."""
    from scipy import ndimage as ndi
    return ndi.gaussian_filter(np.asarray(plane, dtype=np.float32), sigma)


def object_photons(smoothed, fit, ceiling=None, raw=None):
    """Photons on the OBJECTS of a channel: what its dynamic range and noise look like where it carries
    signal, not over the (mostly empty) volume.

    ``smoothed`` is a 1-D sample of `smooth_plane` values. Objects are the sample above the triangle
    level of its own histogram (`intensity_utils.background_threshold`, the derived level used
    everywhere else); ``raw`` (same positions) drops voxels at ``ceiling``, whose value is a floor.
    Returns ``{objectFrac, p50Photons, p99Photons, ceilingPhotons}`` (``ceilingPhotons`` only when a
    ceiling is known) — or None when ``fit`` is untestable. SNR on an object is ~sqrt(photons) at best.
    """
    from cecelia.utils import intensity_utils
    if not fit or fit.get('greyPerPhoton') is None:
        return None
    s = np.asarray(smoothed, dtype=np.float64).ravel()
    if s.size == 0:
        return None
    hist = np.bincount(np.clip(np.round(s), 0, None).astype(np.int64))
    level = intensity_utils.background_threshold(hist)
    obj = s > level
    if raw is not None and ceiling is not None:
        obj &= np.asarray(raw).ravel() < ceiling
    out = {'objectFrac': float(obj.mean()), 'objectLevel': float(level),
           'p50Photons': None, 'p99Photons': None,
           'ceilingPhotons': (float(photons(ceiling, fit)) if ceiling is not None else None)}
    if obj.any():
        p50, p99 = np.percentile(s[obj], [50, 99])
        out['p50Photons'] = float(photons(p50, fit))
        out['p99Photons'] = float(photons(p99, fit))
    return out


def shared_structure(samples):
    """``[{a, b, r}]`` — how strongly every channel pair shows the same structure, over the same smoothed
    sample positions.

    ``samples`` maps a channel to a LIST of 1-D arrays, one per sampled plane, same positions in every
    channel — `structure_plane` values, so slow gradients the channels share through the optics are
    already gone. Each plane is also centred on its own mean before the planes are pooled (the
    within-plane correlation): pooled raw, two independent channels that both dim with depth or fade
    over time correlate through that shared trend alone (r 0.99 against ~0 within each plane). A bare
    array is taken as one plane.

    A leak, broadband autofluorescence and a real co-label all raise it, and from one image the three
    cannot be told apart (both leak directions fit equally well under one excitation line). So this is
    reported as "these channels show the same structure", for a person to read against what each
    channel should contain; a no-stain control is what separates them."""
    from cecelia.utils import coloc_utils
    def centred(v):
        planes = v if isinstance(v, (list, tuple)) else [v]
        return np.concatenate([np.asarray(p, dtype=np.float64).ravel() - np.mean(p) for p in planes])
    cs = {c: centred(v) for c, v in samples.items()}
    chans = sorted(cs)
    out = []
    for i, a in enumerate(chans):
        for b in chans[i + 1:]:
            r = coloc_utils.pearson(cs[a], cs[b])
            if r is not None:
                out.append({'a': int(a), 'b': int(b), 'r': round(float(r), 3)})
    return out
