"""Pixel colocalisation statistics for a pair of channels — the primitives behind "are these two
channels related, and how".

Written for one caller (`correction_utils.af_bleedthrough_alphas`, which needs the slope of the
proportional part of a channel pair) but kept separate and general, because these are the standard
coloc measures and the one place they should ever be implemented. Imaris' Coloc module reports the
same set; a second hand-rolled Pearson somewhere else is the bug this file exists to prevent.

Everything here is PURE and takes paired samples — two flat arrays of the same length, one value per
voxel per channel. Gathering them (which frames, which stride) is the caller's problem, so these can be
unit-tested with a handful of numbers.

**Citations.** The automatic threshold is Costes et al. 2004, *Biophys J* 86(6):3993-4003,
doi:10.1529/biophysj.103.038422 — the paper Imaris, Fiji's Coloc 2 and JACoP all implement. The
orthogonal-regression step is theirs too: an ordinary least-squares fit is asymmetric (regressing y on
x and x on y give different lines), and a threshold derived from an asymmetric fit depends on which
channel you happened to call the source.
"""
import numpy as np


def tls_slope(x, y):
    """Total-least-squares (orthogonal) fit ``y = a*x + b``, as ``(a, b)``.

    Not ordinary least squares, and the difference is the point: OLS minimises error in ``y`` alone, so
    it is asymmetric in the two channels and biased towards zero by noise in ``x``. Costes' threshold
    walks along this line, so an asymmetric fit would make the answer depend on which channel was named
    first. The closed form below is the principal axis of the 2x2 covariance matrix.

    Returns ``(nan, nan)`` when the covariance is zero — two channels with no linear relationship at all
    have no principal axis to report, and a caller must not read a slope out of that.
    """
    x = np.asarray(x, dtype=np.float64).ravel()
    y = np.asarray(y, dtype=np.float64).ravel()
    if x.size < 2 or x.size != y.size:
        return float('nan'), float('nan')
    mx, my = x.mean(), y.mean()
    vx, vy = x.var(), y.var()
    cxy = float(((x - mx) * (y - my)).mean())
    if abs(cxy) < 1e-12:
        return float('nan'), float('nan')
    a = (vy - vx + np.sqrt((vy - vx) ** 2 + 4.0 * cxy ** 2)) / (2.0 * cxy)
    return float(a), float(my - a * mx)


def pearson(x, y):
    """Pearson's r, or None when either input is constant (r is undefined, not zero)."""
    x = np.asarray(x, dtype=np.float64).ravel()
    y = np.asarray(y, dtype=np.float64).ravel()
    if x.size < 2 or x.size != y.size or x.std() < 1e-12 or y.std() < 1e-12:
        return None
    return float(np.corrcoef(x, y)[0, 1])


#: Steps taken walking the threshold down. 200 over the source's range is finer than the noise on the
#: correlation it is testing, and the crossing is a broad zero-crossing rather than a sharp one.
COSTES_STEPS = 200

#: Below this many voxels the correlation being tested is noise, so the walk skips the step rather than
#: reading a crossing off it. Costes' own implementations use a similar floor.
COSTES_MIN_VOXELS = 100


def costes_threshold(x, y, steps=COSTES_STEPS, min_voxels=COSTES_MIN_VOXELS):
    """Costes' automatic threshold for a channel pair. Returns a dict, never raises.

    The method: fit the orthogonal regression line, then walk a threshold ``T`` down the source axis,
    and at each step measure Pearson's r over ONLY the voxels below ``T`` (and below the line's value
    at ``T``). The threshold is where that correlation reaches zero — i.e. the level below which the
    two channels no longer explain one another. Everything above it is the colocalised population.

    **The convergence is as informative as the threshold.** ``converged`` False means r never reached
    zero: the two channels stay positively correlated all the way down to their darkest voxels, which
    is what a global proportional relationship (spillover, or a shared broadband source) looks like and
    what discrete object-level colocalisation does not. A caller deciding whether to subtract a
    proportional term should look at this before trusting a slope.

    Caveat worth knowing before reading a result: this is sensitive to SMOOTHING. A spatial filter
    applied to both channels correlates neighbouring voxels and can stop the walk converging at all —
    measured on `WIaUjL/p6t4mC`, r stalled at +0.34..+0.65 on the smoothed store while the same planes
    of the unsmoothed store converged to |r| < 0.02. Run it on unfiltered pixels.

    Keys: ``slope``, ``intercept``, ``threshold`` (source), ``targetThreshold``, ``r`` (at the
    crossing), ``converged``, ``minR`` (the lowest r the walk reached, for the non-converged case).
    """
    x = np.asarray(x, dtype=np.float64).ravel()
    y = np.asarray(y, dtype=np.float64).ravel()
    a, b = tls_slope(x, y)
    out = {'slope': a, 'intercept': b, 'threshold': None, 'targetThreshold': None,
           'r': None, 'converged': False, 'minR': None}
    if not np.isfinite(a) or x.size < min_voxels:
        return out

    min_r = None
    for t in np.linspace(float(x.max()), float(x.min()), int(steps)):
        below = (x <= t) & (y <= a * t + b)
        if int(below.sum()) < min_voxels:
            continue
        r = pearson(x[below], y[below])
        if r is None:
            continue
        min_r = r if min_r is None else min(min_r, r)
        if r <= 0.0:
            out.update(threshold=float(t), targetThreshold=float(a * t + b),
                       r=r, converged=True, minR=min_r)
            return out
    out['minR'] = min_r
    return out


#: Quantile taken as "the floor" of the target within each source bin — see `envelope_slope`. Costes'
#: own papers use a low percentile for the same job. 5 rather than 0 because the true minimum of a bin
#: is one voxel of read noise.
ENVELOPE_FLOOR_PCT = 5.0

#: Source bins for the envelope fit, spaced by QUANTILE rather than by value: a fluorescence channel is
#: mostly background, so equal-width value bins put almost every voxel in the first one and fit the line
#: through a single point.
ENVELOPE_BINS = 40

#: A bin holding fewer voxels than this cannot support a 5th percentile, so it is dropped.
ENVELOPE_MIN_PER_BIN = 50


def envelope_slope(x, y, bins=ENVELOPE_BINS, floor_pct=ENVELOPE_FLOOR_PCT,
                   min_per_bin=ENVELOPE_MIN_PER_BIN):
    """Slope through the LOWER ENVELOPE of ``y`` against ``x`` — the proportional part of the pair.

    The conservative counterpart to `tls_slope`, and the one to subtract with **when cells may carry
    both markers** — see `correction_utils.af_bleedthrough_alphas`, which picks between the two on the
    combination's `exclusive` flag. Where nothing is legitimately co-located there is nothing above the
    floor to protect and the TOTAL slope is the coefficient; using this one there under-removes, by 5x
    on `WIaUjL/p6t4mC`. Bleedthrough is
    proportional and GLOBAL — a property of the filter set, so every voxel where the source is bright
    gets the same fraction added — which makes it a straight line through the *floor* of the joint
    distribution. Anything co-present for a biological reason sits ABOVE that floor and varies from
    structure to structure, so it does not move the envelope.

    That is why the two estimators differ and why both are worth having: `tls_slope` fits ALL the
    signal, so genuine co-positive structures drag it up, and subtracting it would over-subtract.
    Measured on `WIaUjL/p6t4mC` (CH3 -> CH2), the envelope gives 0.025 where the TLS slope gives 0.113.
    The gap between them is whatever sits ABOVE the floor — a co-positive population where one exists,
    and on that image (two reporters, two cell types, confirmed no overlap) leak that this estimator
    does not see. On synthetic data with no co-labelling the two agree to within 3%, so the divergence
    there is a property of real data, not of the definition; recorded rather than explained.

    **Two calibration choices, both forced by measurement rather than taste** (the sweep is in
    `test_coloc_utils.EnvelopeSlopeTest`, injected alpha 0 to 0.2 on 200k synthetic voxels):

    * The bin's source coordinate is the SAME low quantile as the target's, not the bin mean. Pairing a
      bin's mean source with its floor target recovered only **27-30%** of an injected alpha — the floor
      of the target within a bin is attained at the floor of the source, so the two must be read off
      the same place.
    * The line carries a FREE INTERCEPT and only its slope is used, rather than being forced through the
      origin. The target's own noise floor is a near-constant offset across bins, and folding it into
      the slope biased the estimate up most where alpha is smallest — with no leak at all, through the
      origin reported 0.0064 against **0.0002** here. Reporting no leak when there is none is the result
      this has to get right, because it is the one that licenses subtracting nothing.

    Residual bias after both: recovery runs **+10% to +55%** of the true alpha, worst at small alpha,
    and a co-positive population inflates it further (0.220 -> 0.270 at alpha=0.2). It errs HIGH, so a
    caller must clamp the subtraction at zero and should treat the number as an upper bound.

    Returns ``(alpha, r_squared, n_bins_used)``; ``(nan, nan, 0)`` when there is not enough above-zero
    source to fit. **``r_squared`` is not decoration** — a least-squares fit returns a slope for any
    input, so it is the only thing separating a leak from a coincidence. Measured: two independent
    channels give R^2 ~0.02-0.03 (and alpha ~0.0001), an injected proportional leak gives ~0.997, and a
    real one on `WIaUjL/p6t4mC` gives 0.43 at alpha 0.023. A caller must gate on it.
    """
    x = np.asarray(x, dtype=np.float64).ravel()
    y = np.asarray(y, dtype=np.float64).ravel()
    m = x > 0
    if int(m.sum()) < min_per_bin * 5:
        return float('nan'), float('nan'), 0
    xs_all, ys_all = x[m], y[m]
    edges = np.percentile(xs_all, np.linspace(0.0, 100.0, int(bins) + 1))
    xs, ys = [], []
    for i in range(int(bins)):
        lo, hi = edges[i], edges[i + 1]
        sel = (xs_all >= lo) & (xs_all < hi) if i < bins - 1 else (xs_all >= lo)
        if int(sel.sum()) < min_per_bin:
            continue
        # the SAME quantile on both axes — see the docstring for what the bin mean cost here
        xs.append(float(np.percentile(xs_all[sel], floor_pct)))
        ys.append(float(np.percentile(ys_all[sel], floor_pct)))
    if len(xs) < 5:
        return float('nan'), float('nan'), len(xs)
    xs, ys = np.asarray(xs), np.asarray(ys)
    if float(xs.std()) < 1e-12:
        return float('nan'), float('nan'), len(xs)
    # slope of a free-intercept least-squares fit through the binned floor points; the intercept is
    # the target's own noise floor and is deliberately absorbed rather than folded into the slope
    design = np.vstack([xs, np.ones_like(xs)]).T
    coef = np.linalg.lstsq(design, ys, rcond=None)[0]
    resid = float(((ys - design @ coef) ** 2).sum())
    total = float(((ys - ys.mean()) ** 2).sum())
    r2 = (1.0 - resid / total) if total > 0 else float('nan')
    return float(coef[0]), float(r2), len(xs)
