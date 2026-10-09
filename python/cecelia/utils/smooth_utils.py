"""The smoothing task's per-frame compute, shared by the run and the preview.

`app/src/tasks/cleanupImages/smooth_run.py` streams a whole store through these; the preview worker
(`preview/preview_worker.py` → `_preview_smooth`) runs the SAME calls over the visible region, so what
you judge before a run is the arithmetic the run will do (CLEANUP_FACTS_PLAN D2, TASK_PREVIEW_PLAN
Decision 1). Everything here works on planes and plane-readers, never on a store: the caller owns I/O.

The engine itself is `coastal.smooth` (array-only, imports nothing from cecelia), imported lazily like
every other coastal use in this package. The two invariants it depends on — spatial BEFORE temporal,
and ONE shared kernel (and gain) for every smoothed channel — are documented in `smooth_run.py`'s
header, where they were measured; this module only has to not break them.
"""
from __future__ import annotations

import cv2
import numpy as np

#: How many (t, z) planes to sample when estimating the dynamic-range gain. The gain only needs the
#: right order of magnitude, and a sample keeps this from being a second full pass over the store.
GAIN_SAMPLE_PLANES = 24


def _anscombe(x):
    """Poisson variance-stabilising transform: `y = 2 sqrt(x + 3/8)` (Anscombe 1948)."""
    return 2.0 * np.sqrt(np.clip(x, 0.0, None) + 3.0 / 8.0)


def _inv_anscombe(s):
    """Unbiased closed-form inverse of `_anscombe`. Mäkitalo & Foi, IEEE T-IP 2011."""
    s = np.clip(s, 1e-6, None)
    inv = ((s / 2.0) ** 2 - 1.0 / 8.0
           + (np.sqrt(3.0 / 2.0) / 4.0) / s
           - (11.0 / 8.0) / (s ** 2)
           + (5.0 * np.sqrt(3.0 / 2.0) / 8.0) / (s ** 3))
    return np.clip(inv, 0.0, None).astype(np.float32)


def _bilateral_vst(frame, sigma_color, sigma_spatial, polish_sigma=0.0):
    """Anscombe VST → cv2 bilateral → unbiased inverse [→ gentle Gaussian polish]. All the "one shared
    kernel per channel" invariant needs is one filter with fixed params applied identically — that
    holds here, and the polish (a fixed-sigma Gaussian) preserves it.

    `polish_sigma` breaks bilateral's bimodal weight distribution near transitions: after
    variance-stabilised bilateral, single-pixel jitter shows up because the color-weight is ~1 on the
    same-brightness side of an edge and ~0 across it, so quantising back to the integer store leaves
    isolated pixels sticking out. A sub-pixel Gaussian on top of the inverse averages that jitter into
    the neighbours the bilateral already agreed with, without pulling in the cross-edge neighbours it
    excluded. Measured on `zolIMa/fXgbTl` — see docs/todo/SMOOTHING_PLAN.md.
    """
    a = _anscombe(frame)
    b = cv2.bilateralFilter(a.astype(np.float32), d=-1,
                            sigmaColor=float(sigma_color),
                            sigmaSpace=float(sigma_spatial))
    out = _inv_anscombe(b)
    if polish_sigma > 0:
        # ksize=0 lets cv2 derive an odd kernel width from sigma. Sub-pixel sigmas need at least 3.
        out = cv2.GaussianBlur(out, ksize=(0, 0), sigmaX=float(polish_sigma),
                               sigmaY=float(polish_sigma))
    return out


def build_spatial_fn(method, sigma, bilateral_color, bilateral_reach, bilateral_polish):
    """Return the per-frame spatial callable used by the streaming loop AND the gain estimator.

    One callable so both paths run identical arithmetic — the gain the estimator picks is the gain
    the streaming loop needs. `gaussian` routes to coastal.smooth's `spatial_smooth` unchanged.
    """
    if method == "bilateral_vst":
        return lambda frame: _bilateral_vst(frame, bilateral_color, bilateral_reach,
                                            bilateral_polish)
    from coastal.smooth import spatial_smooth
    return lambda frame: spatial_smooth(frame, sigma)


def estimate_gain(read_plane, spatial_fn, sel, nt, nz):
    """`(gain, hi_in, hi_smoothed)` — ONE gain for every smoothed channel, from a fixed sample.

    Averaging lowers the maximum, so writing the result back at the input dtype throws away the
    precision the AF background estimate needs: measured on fXgbTl, smoothed nuc-GFP has p99=15
    and max 59, i.e. ~59 integer levels for the whole channel, and the background sits at 2.6 —
    one integer step is 38% of it. ONE gain across all smoothed channels restores the range
    without touching cross-channel ratios. Seeded, so the run and a preview of the same image pick
    the same planes and therefore the same gain. `read_plane(t, c, z)` returns one float32 plane.
    """
    rng = np.random.default_rng(0)
    picks = [(int(rng.integers(0, nt)), int(rng.integers(0, nz)))
             for _ in range(min(GAIN_SAMPLE_PLANES, nt * nz))]
    hi_in, hi_sm = [], []
    for t, z in picks:
        for c in sel:
            raw = read_plane(t, c, z)
            hi_in.append(np.percentile(raw, 99.99))
            hi_sm.append(np.percentile(spatial_fn(raw), 99.99))
    hi_in, hi_sm = float(np.mean(hi_in)), float(np.mean(hi_sm))
    return (max(1.0, hi_in / hi_sm) if hi_sm > 0 else 1.0), hi_in, hi_sm


def estimate_gate_sigma(read_plane, spatial_fn, sel, nt, nz):
    """The gated statistic's noise scale, estimated ONCE from a sample and handed to every frame.

    `gated_frames` would otherwise estimate it per window — a 3-9 frame sample — so the gate's
    strictness would drift between z-planes and timepoints for no physical reason. The noise level is
    a property of the acquisition, not of the window we happen to be holding. The guide is the SUM
    over smoothed channels, so the estimate is taken on that same quantity. Returns `(sigma, n)`.
    """
    from coastal.smooth import noise_sigma
    rng = np.random.default_rng(1)
    zs = sorted({int(rng.integers(0, nz)) for _ in range(min(3, nz))})
    span = min(nt, 8)
    samples = []
    for z in zs:
        slab = np.stack([sum(spatial_fn(read_plane(t, c, z)) for c in sel) for t in range(span)])
        samples.append(noise_sigma(slab))
    return float(np.median(samples)), len(samples)


def smooth_timepoint(spatial_at, sel, t, half, frames, stat, gate_sigma=None,
                     farneback_clamp=8.0):
    """`{channel: float plane}` — the smoothed frame at timepoint `t` for every channel in `sel`,
    BEFORE the gain. `spatial_at(t, c)` returns the spatially smoothed plane, clamped to the movie
    (the caller caches it: every window re-reads its neighbours).

    `gated` and `farneback` need every selected channel's window at once: the match (or the flow) is
    derived once from their SUM and applied to each — the AF-ratio invariant, for an adaptive kernel.
    One call for every channel rather than one per channel because the match is the expensive half:
    measured on a real 4-channel plane, 588 ms -> 155 ms, i.e. 33.5 min -> 8.9 min over a 180t x 19z
    movie. A dim channel thereby inherits the warp found in the bright signal.
    """
    gated = stat == "gated" and half > 0
    farneback = stat == "farneback" and half > 0
    if gated or farneback:
        from coastal.smooth import gated_frames, flow_warped_frames
        wins = {c: np.stack([spatial_at(tt, c) for tt in range(t - half, t + half + 1)])
                for c in sel}
        guide = None
        for w in wins.values():
            guide = w.copy() if guide is None else guide + w
        order = list(sel)
        frames_out = (gated_frames([wins[c] for c in order], guide=guide, sigma=gate_sigma)
                      if gated else
                      flow_warped_frames([wins[c] for c in order], guide=guide,
                                         max_shift_px=farneback_clamp))
        return dict(zip(order, frames_out))
    if half == 0:
        return {c: spatial_at(t, c) for c in sel}
    from coastal.smooth import temporal_smooth
    # spatial FIRST (cached), then the temporal statistic across the window
    return {c: temporal_smooth(np.stack([spatial_at(tt, c) for tt in range(t - half, t + half + 1)]),
                               frames, stat, time_axis=0)[half]
            for c in sel}


def apply_gain(out, gain, dtype_max):
    """`(plane, n_clipped)` — the smoothed plane scaled by the shared gain and, for an integer store,
    rounded and clipped to the dtype. `dtype_max` is `None` for a float store (no clip)."""
    out = out * gain
    if dtype_max is None:
        return out, 0
    clipped = int((out > dtype_max).sum())
    return np.clip(np.rint(out), 0, dtype_max), clipped
