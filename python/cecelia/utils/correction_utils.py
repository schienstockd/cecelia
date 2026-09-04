"""
Image correction utilities: autofluorescence and drift correction.

Ported from the original R/Python cecelia package. Uses scipy and skimage
instead of dask-image and pyclesperanto (neither of which is in the venv).
All channel-level operations materialise to numpy internally; output is
returned as a dask array so create_multiscales can use it directly.
"""

import collections
from copy import copy
import os
import numpy as np
import shutil

import dask.array as da
import scipy.fft
import scipy.ndimage
import skimage.restoration
import skimage.morphology
import skimage.filters
from skimage.registration import phase_cross_correlation

import cecelia.utils.zarr_utils as zarr_utils
import cecelia.utils.cpu_utils as cpu_utils
import cecelia.utils.slice_utils as slice_utils
import cecelia.utils.intensity_utils as intensity_utils
import cecelia.utils.script_utils as script_utils


# ── Drift correction ──────────────────────────────────────────────────────────
#
# Estimating the drift is by far the expensive half of the task (95 s of a 122 s run on
# `4kS67f/fHqhyb`), and all of it is FFTs. Three properties of THIS problem make the obvious loop
# several times slower than it needs to be, all measured on `zolIMa/ldYr8J` (181×4×31×1024×1024),
# 20 frame pairs:
#
#   as written, float64, one pair at a time                     57.7 s
#   + float32                                                   26.6 s   (2.2x)
#   + reuse each frame's FFT instead of recomputing it           ^ included above
#   + multithreaded FFT (scipy.fft workers)                      16.5 s   (3.5x)
#
# float32 is free accuracy-wise: the resulting trajectory differs by 0.02 px cumulative over 20
# frames, and the writer rounds placements to whole pixels anyway. The FFT reuse matters because
# every frame is BOTH the moving image of one pair and the reference of the next — the old loop
# read it from the store twice and transformed it twice. `upsample_factor` is NOT worth tuning
# (2.38 s/pair at 1x vs 2.79 at 100x on the same data); the transform dominates, not the peak
# refinement.

# How many threads scipy.fft may use. Deliberately not `cpu_count()`: this runs inside a `cpu` pool
# slot alongside other tasks, and the win is most of the way there by 8 (16.5 s at 32 workers vs
# 18.9 s at 8 on the benchmark above).
_FFT_WORKERS = max(1, min(8, os.cpu_count() or 1))

# Per-frame position measurements are taken not only between neighbours but across short gaps, and
# the whole trajectory is then solved at once. See `estimate_drift`.
DRIFT_DEFAULT_MAX_LAG = 3
DRIFT_DEFAULT_SMOOTHNESS = 0.5

# Default cap on |angle| per frame for the `sitkRigid` estimator (see `sitk_estimate_rigid`).
# Above this, the frame is marked interpolated and its (angle, translation) is predicted from its
# neighbours. 5° is well above any real stage bump on the author's rigs (usually sub-degree);
# above 10°, the fit is almost always dominated by a moving object in the reference channel
# rather than by the frame itself. See docs/todo/DRIFT_RIGID_PLAN.md Decision 6.
DRIFT_DEFAULT_MAX_ANGLE = 5.0

# Post-solve gaussian smoothing on the cumulative trajectory (per axis). 0 = off.
# The writer places frames at integer pixels (`drift_correct_im` rounds
# `cumsum(shifts)`), so a trajectory with sub-pixel noise around a small true
# drift produces per-frame integer-pixel *jumps* in the corrected movie —
# visible as jitter on `zolIMa/2h06xA`, where the underlying sample barely
# moves (~1 px peak-to-peak) but the estimator's own noise floor produces 107
# integer transitions across 181 frames. Smoothing the *positions* (not the
# deltas: a per-delta threshold amplifies drift when noise partially cancels
# real deltas, verified in the same audit) at σ=6 collapses that to 11
# transitions on 2h06xA, and is nearly transparent on movies with real motion
# — on `d5vw7z/ttRMjQ` (peak drift 168 px, real ~2 px/frame) it changes the
# peak by 3% and reduces transitions by 1%.
#
# API default is 0 so direct callers keep the strict "recover the input drift"
# contract that the estimator tests pin. The task-runner default is set in the
# task's params JSON so GUI runs get the fix without every unit test having to
# opt out.
DRIFT_DEFAULT_SMOOTH_SIGMA = 0.0
DRIFT_TASK_SMOOTH_SIGMA = 6.0

# ── Within-stack XY alignment ────────────────────────────────────────────────
#
# For each timepoint's Z stack, each plane gets its own XY shift relative to
# a reference plane, so a sample that moved DURING acquisition of the stack
# comes out with all planes at the same lateral position. The measurement is
# whole-plane phase correlation on a chosen channel; the guards below stop
# the aligner from forcing structural Z differences into per-plane shifts
# on movies where planes are just at different depths.
#
# Measured on `d5vw7z/c91ICQ` (formerly ttRMjQ), 126 timepoints × 6 z-planes,
# 5 µm Z spacing:
#   • median shift 0.9 px, max 7.8 px — realistic sample motion.
#   • ~40% of non-reference planes reject through the confidence gate. Those
#     are the top/bottom edge planes at 10-15 µm from the reference; on
#     this movie they read as structurally distinct (different tissue at
#     depth), so the gate correctly leaves them alone.
#   • ref choice matters when the reference plane itself is smeared during
#     a breath: `sharpest` (highest per-plane sharpness in the stack) picks
#     an unaffected anchor and avoids spreading the smear laterally.

# Reject a plane's fit if PC confidence (1 - NRMSE) is below this. Empirical:
# on `d5vw7z/c91ICQ` middle-vs-adjacent-plane confidences sit around 0.5-0.6,
# while structurally different edge planes drop below 0.35. Not a physical
# threshold, so exposed as a task param.
STACK_ALIGN_DEFAULT_MIN_CONF = 0.35

# Reject a plane's fit if the estimated shift exceeds this many pixels — a
# large shift on a real 30-µm-tall stack is almost always PC latching onto a
# wrong peak, not real motion. Also exposed.
STACK_ALIGN_DEFAULT_MAX_SHIFT_PX = 8.0


def _plane_sharpness(plane):
    """A scalar sharpness proxy for one plane: mean |∇I| of the DoG-whitened
    image. Used by the `sharpest` reference option to pick an anchor plane
    that isn't itself motion-blurred."""
    x = plane.astype(np.float32)
    lo, hi = np.percentile(x, (1, 99))
    if hi - lo < 1e-6:
        return 0.0
    x = np.clip((x - lo) / (hi - lo), 0, 1)
    w = skimage.filters.difference_of_gaussians(x, 1.0, 6.0)
    return float(np.abs(np.diff(w, axis=0)).mean() + np.abs(np.diff(w, axis=1)).mean())


def _pc_prep(plane):
    """DoG-whitened, contrast-stretched plane for phase correlation. Same
    prep the smear audit and the σ-smoother probes use — sharpens PC's
    correlation peak on faint, low-contrast tissue."""
    x = plane.astype(np.float32)
    lo, hi = np.percentile(x, (1, 99))
    x = np.clip((x - lo) / (hi - lo + 1e-8), 0, 1)
    x = skimage.filters.difference_of_gaussians(x, 1.0, 6.0)
    lo, hi = np.percentile(x, (1, 99))
    return np.clip((x - lo) / (hi - lo + 1e-8), 0, 1).astype(np.float32)


StackAlignment = collections.namedtuple('StackAlignment', [
    'shifts',    # (T, Z, 2) — (dy, dx) per (t, z). Zero for the reference plane
                 # and for planes the gate rejected (see `applied`).
    'confidence',# (T, Z) — PC confidence 1-NRMSE; 1.0 at the reference by construction
    'applied',   # (T, Z) bool — True where the fit was applied (or is the ref);
                 # False where the gate rejected it and the plane was left as-is
    'ref_idx',   # (T,) — the reference plane index used for each timepoint
])


def _pick_ref(stack, mode):
    """Reference plane index for one stack. `mode` is `'middle'`, `'sharpest'`,
    or an explicit int. Raises on out-of-range int rather than silently
    clamping — a bad param should fail loudly, not register the whole movie
    against a plane the user didn't mean."""
    z = stack.shape[0]
    if isinstance(mode, (int, np.integer)):
        m = int(mode)
        if not 0 <= m < z:
            raise ValueError(f"stack-align reference {m} out of range for Z={z}")
        return m
    if mode == 'middle':
        return z // 2
    if mode == 'sharpest':
        return int(np.argmax([_plane_sharpness(stack[i]) for i in range(z)]))
    raise ValueError(f"unknown stack-align reference '{mode}' "
                     f"(middle | sharpest | int)")


def estimate_stack_alignment(image_array, align_channel, dim_utils,
                             reference='middle',
                             min_conf=STACK_ALIGN_DEFAULT_MIN_CONF,
                             max_shift_px=STACK_ALIGN_DEFAULT_MAX_SHIFT_PX,
                             upsample_factor=20,
                             time_idx=None, channel_idx=None,
                             on_progress=None):
    """Per-plane XY shift for every (t, z), computed on ``align_channel``.

    Two guards protect against forcing structural Z differences into shifts:
    a confidence gate on the PC peak (below ``min_conf`` → don't apply) and
    a magnitude clamp (|shift| > ``max_shift_px`` → don't apply). Skipped
    planes come back with shift (0, 0) and `applied=False`; the reference
    plane comes back with shift (0, 0) and `applied=True`.

    ``reference`` = 'middle' | 'sharpest' | int:
      - 'middle' (default) — cheap, deterministic. Fine when the middle
        plane is sharp on most frames.
      - 'sharpest' — per-timepoint pick, so if the middle plane itself is
        motion-blurred during a breath the aligner anchors elsewhere.
      - int — force a specific plane; useful for a diagnostic re-run.

    Returns a `StackAlignment`. The rest of this module treats it exactly
    like `estimate_drift`'s `DriftEstimate`: numbers a caller banks into QC,
    then feeds back to the writer.
    """
    if time_idx is None:
        time_idx = dim_utils.dim_idx('T')
    if channel_idx is None:
        channel_idx = dim_utils.dim_idx('C')
    z_idx = dim_utils.dim_idx('Z')

    n_t = dim_utils.dim_val('T')
    n_z = dim_utils.dim_val('Z')

    shifts = np.zeros((n_t, n_z, 2), dtype=np.float32)
    conf = np.zeros((n_t, n_z), dtype=np.float32)
    applied = np.zeros((n_t, n_z), dtype=bool)
    ref_out = np.zeros(n_t, dtype=np.int32)

    for t in range(n_t):
        # Load ONE (t, ch) stack as [Z, Y, X].
        sl = [slice(None)] * len(image_array.shape)
        sl[time_idx] = slice(t, t + 1)
        sl[channel_idx] = slice(align_channel, align_channel + 1)
        stack = np.squeeze(zarr_utils.fortify(image_array[tuple(sl)]),
                           axis=(time_idx, channel_idx)).astype(np.float32)
        # After the squeeze the axis order over the SPATIAL dims is what
        # dim_utils reports for the source; we want [Z, Y, X] regardless.
        # Reorder if the source stored Z somewhere other than the leading
        # spatial axis. Cheap; a no-op on the (T, C, Z, Y, X) canonical case.
        spatial = list(dim_utils.spatial_axis())
        if spatial != ['Z', 'Y', 'X']:
            perm = [spatial.index(ax) for ax in ('Z', 'Y', 'X')]
            stack = np.transpose(stack, perm)

        ref = _pick_ref(stack, reference)
        ref_out[t] = ref
        ref_pc = _pc_prep(stack[ref])
        conf[t, ref] = 1.0
        applied[t, ref] = True

        for z in range(n_z):
            if z == ref:
                continue
            plane_pc = _pc_prep(stack[z])
            (dy, dx), err, _ = phase_cross_correlation(
                ref_pc, plane_pc, upsample_factor=upsample_factor,
                normalization=None)
            c = max(0.0, 1.0 - float(err))
            conf[t, z] = c
            mag = float(np.hypot(dy, dx))
            if c >= min_conf and mag <= max_shift_px:
                shifts[t, z] = (dy, dx)
                applied[t, z] = True
            # else: shifts stays (0,0), applied stays False — the plane is
            # left where it was and QC records it as skipped.
        if on_progress is not None:
            on_progress(t + 1, n_t)

    return StackAlignment(shifts=shifts, confidence=conf,
                          applied=applied, ref_idx=ref_out)


def apply_stack_alignment(image_array, alignment, dim_utils, out=None,
                          on_progress=None):
    """Write a stack-aligned copy of ``image_array`` — every plane shifted
    by ``alignment.shifts[t, z]`` (subpixel, cubic). Output shape matches
    the input; unlike `drift_correct_im`, no canvas expansion, because the
    per-plane shifts are small and confined to the stack. Content clipped
    against the frame edge becomes zero (same edge policy the writer's
    integer-shifted `drift_correct_im` uses for its expanded canvas)."""
    t_idx = dim_utils.dim_idx('T')
    c_idx = dim_utils.dim_idx('C')
    z_idx = dim_utils.dim_idx('Z')

    n_t = dim_utils.dim_val('T')
    n_c = dim_utils.dim_val('C')
    n_z = dim_utils.dim_val('Z')

    result_dtype = out.dtype if out is not None else image_array.dtype
    result = out if out is not None else np.zeros(image_array.shape, dtype=result_dtype)

    for t in range(n_t):
        for z in range(n_z):
            dy, dx = alignment.shifts[t, z]
            if not alignment.applied[t, z] or (dy == 0 and dx == 0):
                # Copy verbatim — skipped-by-gate planes AND the reference
                # take this branch. Same code path so a byte-exact "no
                # alignment happened" case stays trivially reproducible.
                for c in range(n_c):
                    sl = [slice(None)] * len(image_array.shape)
                    sl[t_idx] = slice(t, t + 1)
                    sl[c_idx] = slice(c, c + 1)
                    sl[z_idx] = slice(z, z + 1)
                    src = zarr_utils.fortify(image_array[tuple(sl)])
                    result[tuple(sl)] = src.astype(result_dtype, copy=False)
                continue
            for c in range(n_c):
                sl = [slice(None)] * len(image_array.shape)
                sl[t_idx] = slice(t, t + 1)
                sl[c_idx] = slice(c, c + 1)
                sl[z_idx] = slice(z, z + 1)
                src = np.squeeze(zarr_utils.fortify(image_array[tuple(sl)]),
                                 axis=(t_idx, c_idx, z_idx)).astype(np.float32)
                # After the squeeze `src` is 2D — (Y, X) in the source's
                # order. Build a length-2 shift matching that.
                shifted = scipy.ndimage.shift(
                    src, shift=(float(dy), float(dx)),
                    order=3, mode='constant', cval=0)
                # Clip back to result dtype range so uint16 doesn't wrap on
                # negative undershoots from the cubic spline.
                if np.issubdtype(result_dtype, np.integer):
                    info = np.iinfo(result_dtype)
                    shifted = np.clip(shifted, info.min, info.max)
                out_slab = shifted.astype(result_dtype, copy=False)
                # Restore the T/C/Z axes so the assignment lines up.
                out_slab = np.expand_dims(out_slab, axis=(t_idx, c_idx, z_idx))
                result[tuple(sl)] = out_slab
        if on_progress is not None:
            on_progress(t + 1, n_t)

    return result

DriftEstimate = collections.namedtuple('DriftEstimate', [
    'shifts',        # (T-1, D) per-frame deltas — what the writer applies. The historic return value.
    'positions',     # (T, D) absolute position of each frame, positions[0] == 0
    'axes',          # ['Z','Y','X'] or ['Y','X'] — what the D columns mean
    'estimator',     # 'multiLag' | 'chain' | 'sitkRigid'
    'max_lag',
    'n_pairs',       # how many pairwise measurements went in
    'n_rejected',    # …and how many the robust fit outvoted
    'interpolated',  # frame indices with no surviving measurement — position predicted, not measured
    # px, cycle consistency (see `drift_residuals`) — the reliability number, and **None when there
    # was no redundancy to measure it from** (max_lag 1, i.e. the `chain` estimator). Not 0.0: with
    # only neighbour measurements the residual is identically zero by construction, so banking it
    # would report a flawless registration for the one estimator that cannot check itself.
    'residual_rms',
    'residual_p90',
    # Rigid estimator only: (T,) degrees, ``angles[0] == 0``. None for translation-only estimators
    # (multiLag / chain) so existing consumers ignore it. Same "None means not measured, not 0.0"
    # discipline as ``residual_rms`` — see docs/todo/DRIFT_RIGID_PLAN.md Decision 4.
    'angles',
])


def _drift_frame(image_array, dim_utils, phase_shift_channel, t,
                 time_idx=None, channel_idx=None):
    """One timepoint of the reference channel as a float32 spatial array."""
    if channel_idx is None:
        channel_idx = dim_utils.dim_idx('C')
    if time_idx is None:
        time_idx = dim_utils.dim_idx('T')
    sl = [slice(None)] * len(image_array.shape)
    if channel_idx is not None:
        sl[channel_idx] = slice(phase_shift_channel, phase_shift_channel + 1, 1)
    sl[time_idx] = slice(t, t + 1, 1)
    return np.squeeze(zarr_utils.fortify(image_array[tuple(sl)])).astype(np.float32)


def _drift_pair_measurements(image_array, dim_utils, phase_shift_channel, n_t, max_lag,
                             upsample_factor=100, normalisation=None,
                             time_idx=None, channel_idx=None, on_progress=None):
    """``[(i, j, shift)]`` for every frame pair with ``0 < j - i <= max_lag``.

    One read and one FFT per frame, held in a ring buffer of ``max_lag + 1`` — so raising the lag
    buys redundancy at the cost of extra *correlations*, not extra transforms (the expensive part).
    Memory is bounded by the ring, which matters: a 31×1024×1024 frame's spectrum is 260 MB.

    Two thread budgets, because two different libraries do the work and `_FFT_WORKERS` only bounds
    one of them. The sub-pixel refinement inside `phase_cross_correlation` is a matmul, so it goes
    to BLAS — which takes every core regardless, and is *slower* for it here: many small matmuls
    spend more on thread fan-out than on arithmetic. 1.8x alone, 4.4x with four tasks running.
    Numbers and the reasoning: `cpu_utils.limit_blas_threads`.
    """
    out, ring = [], {}
    with cpu_utils.limit_blas_threads(), scipy.fft.set_workers(_FFT_WORKERS):
        for t in range(n_t):
            ring[t] = scipy.fft.fftn(_drift_frame(
                image_array, dim_utils, phase_shift_channel, t,
                time_idx=time_idx, channel_idx=channel_idx))
            for k in range(1, max_lag + 1):
                if t - k < 0:
                    continue
                # space='fourier' is why the transforms can be cached at all — skimage otherwise
                # re-transforms both images on every call.
                shift, _, _ = phase_cross_correlation(
                    ring[t - k], ring[t], upsample_factor=upsample_factor,
                    normalization=normalisation, space='fourier')
                out.append((t - k, t, np.asarray(shift, dtype=float)))
            ring.pop(t - max_lag, None)
            if on_progress is not None:
                on_progress(t + 1, n_t)
    return out


def _solve_drift_trajectory(pairs, n_t, n_dim, smoothness=DRIFT_DEFAULT_SMOOTHNESS,
                            robust=True, n_iter=8):
    """Absolute per-frame positions from redundant pairwise measurements.

    Each measurement is one linear equation, ``pos[j] - pos[i] = shift``, and with more than one lag
    the system is overdetermined — so the pairs can disagree, and how much they disagree is a
    reliability signal the pairwise chain simply does not have.

    **What this actually protects against.** Not a single bad *frame*: phase correlation still
    assigns that frame some best-fit position `p`, and a chain uses it twice with opposite signs
    (`p − pos[t-1]`, then `pos[t+1] − p`), so `p` cancels and the tail is unharmed — pinned in
    `test_drift_estimate.py`, which caught this being asserted the wrong way round. The failure that
    does accumulate is a set of measurements that NO assignment of per-frame positions can satisfy,
    which is what a movie whose frames barely correlate produces. A chain has no way to notice that
    — it believes each measurement in turn — while a least-squares fit over redundant measurements
    both resists it and, through the residual, measures it.

    Two additions to a plain least squares:

    * **Robust reweighting** (IRLS, Huber-style). The residual scale is estimated from the inlier
      bulk (MAD), so the threshold adapts to a movie's own noise rather than a constant that would
      be far too tight on a clean movie and far too loose on a shaky one. Floored, so a movie whose
      pairs agree to 0.1 px is not trimmed for the sake of it.
    * **A second-difference penalty** weighted by ``smoothness``. Real stage/thermal drift is
      smooth, so this is prior knowledge rather than cosmetic filtering — and it is what makes the
      system solvable for a frame every measurement rejected (its position is then predicted from
      its neighbours, which is the best available answer for a frame that cannot be registered).
      This is the "smooth the shift trajectory" follow-up recorded in
      `docs/todo/SMOOTHING_PLAN.md` → *Also worth changing*, done as a prior rather than as a filter
      applied after the fact.

    Returns ``(positions, weights)`` — ``positions[0]`` is the origin by construction.
    """
    n = len(pairs)
    A = np.zeros((n, n_t))
    b = np.zeros((n, n_dim))
    for r, (i, j, s) in enumerate(pairs):
        A[r, j] = 1.0
        A[r, i] = -1.0
        b[r] = s

    if smoothness > 0 and n_t > 2:
        S = np.zeros((n_t - 2, n_t))
        for t in range(1, n_t - 1):
            S[t - 1, t - 1] = 1.0
            S[t - 1, t] = -2.0
            S[t - 1, t + 1] = 1.0
        S = S * float(smoothness)
        sb = np.zeros((n_t - 2, n_dim))
    else:
        S = np.zeros((0, n_t))
        sb = np.zeros((0, n_dim))

    w = np.ones(n)
    positions = np.zeros((n_t, n_dim))
    for _ in range(n_iter if robust else 1):
        # pos[0] is pinned to the origin by dropping its column — the measurements only ever
        # constrain differences, so without a pin the system is rank-deficient.
        M = np.vstack([A * w[:, None], S])[:, 1:]
        y = np.vstack([b * w[:, None], sb])
        sol, *_ = np.linalg.lstsq(M, y, rcond=None)
        positions = np.vstack([np.zeros(n_dim), sol])
        if not robust:
            break
        res = np.linalg.norm(A @ positions - b, axis=1)
        scale = max(1.4826 * np.median(np.abs(res - np.median(res))), 0.25)
        w = np.clip(2.5 * scale / np.maximum(res, 1e-9), 0.0, 1.0)
    return positions, w


def drift_residuals(pairs, positions):
    """Per-measurement disagreement with a fitted trajectory, in pixels.

    This is *cycle consistency*: ``shift(a→b) + shift(b→c)`` has to equal ``shift(a→c)``, and the
    gap needs no ground truth to compute — three measurements of the same geometry either agree or
    they do not. Measured across this machine's movies it separates the two cases by ~60x (RMS
    0.13–0.39 px where registration works, 24 px on `4kS67f/fHqhyb`, where it does not), which is
    why the task banks it as QC rather than leaving "did that register?" to the eye.

    Note what it does NOT claim: the residual is zero for any self-consistent set of measurements,
    however wrong the drift they describe. It answers "are these measurements OF something", not
    "is that something the right answer" — which is the honest limit of a check with no ground
    truth, and still enough to separate every good movie here from the broken one.
    """
    if not len(pairs):
        return np.zeros(0)
    A = np.zeros((len(pairs), positions.shape[0]))
    b = np.zeros((len(pairs), positions.shape[1]))
    for r, (i, j, s) in enumerate(pairs):
        A[r, j] = 1.0
        A[r, i] = -1.0
        b[r] = s
    return np.linalg.norm(A @ positions - b, axis=1)


def _smooth_positions(positions, sigma):
    """Post-solve gaussian smoothing on the cumulative trajectory, per axis.

    Runs on `positions`, not on the deltas: a per-delta deadzone or threshold
    can *amplify* the cumulative drift when small noise deltas partially
    cancel real spikes (measured on `zolIMa/2h06xA` Z — 6 real spikes of
    −0.8 px with a bath of ±0.1 noise gave cumulative −2 px raw vs cumulative
    −5 px after a τ=0.5 delta deadzone). Filtering the cumulative trajectory
    with a symmetric kernel commutes with the writer's rounding in a
    predictable way and has the property we want: it collapses to zero when
    the trajectory is dominated by noise, and is nearly transparent when it
    is dominated by real motion — no per-movie tuning."""
    if sigma <= 0:
        return positions
    out = positions.astype(np.float64, copy=True)
    for j in range(out.shape[1]):
        out[:, j] = scipy.ndimage.gaussian_filter1d(
            out[:, j], sigma=sigma, mode="nearest")
    return out


def estimate_drift(image_array, phase_shift_channel, dim_utils,
                   upsample_factor=100, normalisation=None,
                   estimator='multiLag', max_lag=DRIFT_DEFAULT_MAX_LAG,
                   max_angle_deg=DRIFT_DEFAULT_MAX_ANGLE,
                   smoothness=DRIFT_DEFAULT_SMOOTHNESS,
                   trajectory_smooth_sigma=DRIFT_DEFAULT_SMOOTH_SIGMA,
                   robust=True,
                   time_idx=None, channel_idx=None, on_progress=None):
    """Per-frame drift of ``image_array`` on ``phase_shift_channel``, as a `DriftEstimate`.

    ``estimator``:

    * ``'multiLag'`` (default) — measure every pair up to ``max_lag`` apart and solve the whole
      trajectory at once, robustly. Measured on `4kS67f/fHqhyb`, whose frames barely correlate
      (pairwise measurements disagreeing by 24 px RMS): the XY excursion the chain reports
      collapses from 242 px to 37 px and the output store from 9.26x the input to 3.55x, while the
      two movies that register cleanly (`kSUFux/mkh3Tu`, `zolIMa/fXgbTl`) move by under a pixel.
      It does not rescue such a movie — `fHqhyb` still reports 7 px p90 against ~0.5 px on a good
      one, and still trips `drift.unreliable` — it stops the estimate running away.
    * ``'chain'`` — neighbours only, integrated in order. What the task did before, kept so a
      banked trajectory can be reproduced. Note it is not bit-identical to pre-2026-08 runs: the
      transforms are float32 now (≤0.02 px cumulative, below the whole-pixel placement grid).
    * ``'sitkRigid'`` — 2D rigid (translation + rotation) via SimpleITK's `ImageRegistrationMethod`.
      For movies where the stage picked up rotation as well as translation, which phase correlation
      cannot see at all. Each frame is fit directly against ``t = 0`` seeded by the previous
      frame's transform, and any per-frame ``|angle| > max_angle_deg`` is rejected and
      interpolated from its neighbours. Design: `docs/todo/DRIFT_RIGID_PLAN.md`.

    ``on_progress(n, total)`` is called once per frame transformed.
    """
    if estimator not in ('multiLag', 'chain', 'sitkRigid'):
        raise ValueError(
            f"unknown drift estimator '{estimator}' (multiLag | chain | sitkRigid)")

    n_t = dim_utils.dim_val('T')
    axes = ['Z', 'Y', 'X'] if dim_utils.is_3D() else ['Y', 'X']

    if estimator == 'sitkRigid':
        # In-plane rotation only, on 2D OR 3D volumes (option B). Positions come back in the same
        # (Y, X) / (Z, Y, X) axis order as the translation estimators, so the writer treats the
        # result the same way.
        positions, angles, interpolated, n_rejected = sitk_estimate_rigid(
            image_array, phase_shift_channel, dim_utils, n_t,
            max_angle_deg=max_angle_deg,
            time_idx=time_idx, channel_idx=channel_idx, on_progress=on_progress)
        # Same jitter mechanism as the translation path — direct-to-t=0 fits still float around a
        # sub-pixel noise floor and the writer still rounds. Smooth positions only; angles keep
        # their own contract (a cap-and-interpolate policy, not a low-pass one).
        positions = _smooth_positions(positions, trajectory_smooth_sigma)
        n_dim = positions.shape[1]
        return DriftEstimate(
            shifts=np.diff(positions, axis=0) if n_t > 1 else np.zeros((0, n_dim)),
            positions=positions,
            axes=axes,
            estimator=estimator,
            max_lag=1,                    # every fit is direct-to-t=0, so "lag" doesn't apply
            n_pairs=max(0, n_t - 1),      # one per frame after 0
            n_rejected=n_rejected,
            interpolated=interpolated,
            residual_rms=None,            # direct-to-reference fits have no redundancy — see Decision 4
            residual_p90=None,
            angles=angles,
        )

    lag = 1 if estimator == 'chain' else max(1, int(max_lag))

    pairs = _drift_pair_measurements(
        image_array, dim_utils, phase_shift_channel, n_t, lag,
        upsample_factor=upsample_factor, normalisation=normalisation,
        time_idx=time_idx, channel_idx=channel_idx, on_progress=on_progress)

    n_dim = len(axes)
    if estimator == 'chain':
        deltas = np.vstack([s for i, j, s in pairs]) if pairs else np.zeros((0, n_dim))
        positions = np.vstack([np.zeros(n_dim), np.cumsum(deltas, axis=0)]) if len(deltas) \
            else np.zeros((n_t, n_dim))
        weights = np.ones(len(pairs))
    else:
        positions, weights = _solve_drift_trajectory(
            pairs, n_t, n_dim, smoothness=smoothness, robust=robust)

    # The `smoothness` prior above pulls the trajectory toward its own second
    # difference, which is not enough when the true drift is at or below the
    # PC noise floor (2h06xA: 107 integer-pixel writer transitions across
    # 181 frames from a ~1 px peak-to-peak trajectory). A wider low-pass on
    # the cumulative trajectory has the property we want: it goes to zero
    # when the trajectory is dominated by noise and is nearly transparent
    # when it is dominated by real motion, without a per-movie threshold.
    # Residuals are computed on the *unsmoothed* solution so the QC metric
    # still measures the estimator's self-consistency, not the smoother's.
    res = drift_residuals(pairs, positions) if lag > 1 else np.zeros(0)
    positions = _smooth_positions(positions, trajectory_smooth_sigma)
    rejected = weights < 0.5
    # A frame nothing survived for is placed where its neighbours say it should be — flagged so a
    # reader of the QC sidecar knows that position was predicted rather than measured.
    measured = set()
    for keep, (i, j, _) in zip(~rejected, pairs):
        if keep:
            measured.add(i)
            measured.add(j)
    interpolated = [t for t in range(n_t) if t not in measured]

    return DriftEstimate(
        shifts=np.diff(positions, axis=0),
        positions=positions,
        axes=axes,
        estimator=estimator,
        max_lag=lag,
        n_pairs=len(pairs),
        n_rejected=int(rejected.sum()),
        interpolated=interpolated,
        residual_rms=float(np.sqrt(np.mean(res ** 2))) if len(res) else None,
        residual_p90=float(np.percentile(res, 90)) if len(res) else None,
        angles=None,
    )


# ── Rigid (rotation-aware) drift estimation ──────────────────────────────────
#
# For movies where the stage picked up rotation as well as translation. Phase correlation cannot
# see rotation at all — the correlation peak sits on whatever translation best aligns the two
# frames' rotational average, which is a lie the eye picks up immediately once you plot a track
# through the corrected canvas. This section fits a per-frame `Euler2DTransform` with SimpleITK's
# `ImageRegistrationMethod` instead: (angle, ty, tx) per frame, in frame 0's coordinate system.
#
# Design lives in docs/todo/DRIFT_RIGID_PLAN.md. In particular:
# - Direct-to-t=0 fits, seeded by the previous frame's fit (Decisions 3, 7). No chain composition:
#   the fit at frame t returns the answer we want (T_t maps frame_0 → frame_t) directly, so we
#   never compose Euler2Ds through 3x3 matrices and re-extract angles.
# - `sitk.Resample(frame_t, frame_0_reference, T_t)` warps frame t into frame 0's canvas — that
#   is what `apply_shifts` will do in P3.
# - Rotation centre is the frame centre, shared by the fit and the applier via `_rigid_centre`.

def _rigid_pyramid(shape):
    """Shrink factors + smoothing sigmas for `ImageRegistrationMethod`'s multi-resolution pyramid,
    adapted to the smallest spatial axis. `SmoothingRecursiveGaussianImageFilter` needs at least 4
    samples per axis, and a confocal Z-stack routinely has 3–20 slices, so the default
    ``[4, 2, 1] / [2, 1, 0]`` blows up on a 4-slice volume. Shape is checked once for the movie —
    all frames come from the same store, so the pyramid does not vary per frame.
    """
    m = int(min(shape))
    if m >= 16:
        return [4, 2, 1], [2.0, 1.0, 0.0]
    if m >= 8:
        return [2, 1], [1.0, 0.0]
    # 4–7 voxels along the tightest axis — no shrink, no smoothing. Convergence is slower but the
    # fit still runs; a stack this thin has little content to lose in a pyramid anyway.
    return [1], [0.0]


def _rigid_centre(shape):
    """Rotation centre used by both the fit and the applier — the frame centre in SimpleITK's
    order (x, y) for 2D or (x, y, z) for 3D. Shared between the fit and the applier so a rejected
    fit and its interpolated replacement land in the same coordinate system."""
    if len(shape) == 2:
        h, w = shape
        return ((float(w) - 1.0) / 2.0, (float(h) - 1.0) / 2.0)
    if len(shape) == 3:
        d, h, w = shape
        return ((float(w) - 1.0) / 2.0, (float(h) - 1.0) / 2.0, (float(d) - 1.0) / 2.0)
    raise ValueError(f"_rigid_centre expects 2D or 3D shape, got {shape}")


def _interpolate_rigid_gaps(positions, angles, rejected):
    """Fill in ``positions[t]`` and ``angles[t]`` for frames the cap rejected.

    Linear interpolation between the two nearest kept frames on either side; nearest-copy at the
    ends. Same rule the multi-lag translation estimator uses for a frame no measurement reached —
    the position is predicted from its neighbours, which is the best answer available for a frame
    that could not be registered on its own.
    """
    n_t = positions.shape[0]
    kept = np.where(~rejected)[0]
    if len(kept) == 0:                                 # every frame rejected — nothing to predict from
        return positions, angles
    out_p = positions.copy()
    out_a = angles.copy()
    for t in range(n_t):
        if not rejected[t]:
            continue
        # nearest kept neighbours
        prev = kept[kept < t]
        nxt = kept[kept > t]
        if len(prev) and len(nxt):
            a, b = int(prev.max()), int(nxt.min())
            w = (t - a) / (b - a)
            out_p[t] = (1 - w) * positions[a] + w * positions[b]
            out_a[t] = (1 - w) * angles[a] + w * angles[b]
        elif len(prev):
            out_p[t] = positions[int(prev.max())]
            out_a[t] = angles[int(prev.max())]
        else:
            out_p[t] = positions[int(nxt.min())]
            out_a[t] = angles[int(nxt.min())]
    return out_p, out_a


def _sitk_rigid_pair(fixed_np, moving_np, init_angle_rad=0.0, init_translation=(0.0, 0.0),
                     centre=None):
    """Fit a rigid transform mapping fixed → moving, seeded at ``(init_angle, init_translation)``.

    Dispatches on ``fixed_np.ndim``:
    - **2D input** → `Euler2DTransform` (1 in-plane angle + 2 translations). ``init_translation``
      is ``(tx, ty)`` in SimpleITK's (x, y) order.
    - **3D input** → `Euler3DTransform` (1 in-plane angle around Z + 3 translations), with
      rotation around X and Y **frozen** via ``SetOptimizerWeights([0, 0, 1, 1, 1, 1])``. This is
      option B in DRIFT_RIGID_PLAN.md: a rigid stage bump is in-plane by construction, and letting
      the fit try X/Y rotations makes it trade small tilts against noise on a clean movie.
      ``init_translation`` is ``(tx, ty, tz)`` in (x, y, z) order.

    Returns ``(angle_rad, translation)`` where ``translation`` has the same length as
    ``init_translation``. SimpleITK images are built with unit spacing so translations are in
    pixels/voxels — matching the translation estimators, whose Z-shifts are already in slice units.
    """
    import SimpleITK as sitk
    ndim = fixed_np.ndim
    if ndim not in (2, 3):
        raise ValueError(f"_sitk_rigid_pair expects 2D or 3D input, got ndim={ndim}")
    if centre is None:
        centre = _rigid_centre(fixed_np.shape)

    fixed = sitk.GetImageFromArray(fixed_np.astype(np.float32, copy=False))
    moving = sitk.GetImageFromArray(moving_np.astype(np.float32, copy=False))

    if ndim == 2:
        tx = sitk.Euler2DTransform()
        tx.SetCenter(centre)
        tx.SetAngle(float(init_angle_rad))
        tx.SetTranslation(tuple(float(v) for v in init_translation))
        weights = None                      # nothing to freeze in 2D
    else:
        tx = sitk.Euler3DTransform()
        tx.SetCenter(centre)
        # Euler3DTransform parameters: (angleX, angleY, angleZ, tx, ty, tz).
        tx.SetRotation(0.0, 0.0, float(init_angle_rad))
        tx.SetTranslation(tuple(float(v) for v in init_translation))
        # 0 weight = the optimiser cannot move that parameter. Freezes X/Y rotation to zero so
        # the fit stays in-plane, matching option B — real stage bumps are only in-plane. See
        # DRIFT_RIGID_PLAN.md Decisions 3 and 6. If a user later reports a movie with genuine
        # sample tilting (option A follow-up), the weights vector is the only knob that changes.
        weights = [0.0, 0.0, 1.0, 1.0, 1.0, 1.0]

    reg = sitk.ImageRegistrationMethod()
    reg.SetMetricAsMeanSquares()
    reg.SetMetricSamplingStrategy(reg.NONE)
    reg.SetInterpolator(sitk.sitkLinear)
    reg.SetOptimizerAsRegularStepGradientDescent(
        learningRate=1.0, minStep=1e-4, numberOfIterations=200,
        gradientMagnitudeTolerance=1e-8)
    # Scale the parameters so a 1-unit step in each is roughly the same effect on the metric —
    # otherwise the angle (in radians) is thousands of times "cheaper" to move than a pixel and the
    # optimiser wanders into large rotations before touching the translation.
    reg.SetOptimizerScalesFromPhysicalShift()
    if weights is not None:
        reg.SetOptimizerWeights(weights)
    shrinks, sigmas = _rigid_pyramid(fixed_np.shape)
    reg.SetShrinkFactorsPerLevel(shrinks)
    reg.SetSmoothingSigmasPerLevel(sigmas)
    reg.SmoothingSigmasAreSpecifiedInPhysicalUnitsOff()
    # inPlace=True so `tx` itself receives the converged parameters — `Execute` otherwise returns
    # a generic `Transform` that cannot be re-wrapped for a typed `.GetAngle()` read.
    reg.SetInitialTransform(tx, inPlace=True)
    reg.Execute(fixed, moving)

    if ndim == 2:
        return float(tx.GetAngle()), tuple(float(v) for v in tx.GetTranslation())
    # Euler3DTransform.GetAngleZ() returns the in-plane rotation; X/Y are 0 by construction.
    return float(tx.GetAngleZ()), tuple(float(v) for v in tx.GetTranslation())


def sitk_estimate_rigid(image_array, phase_shift_channel, dim_utils, n_t,
                        max_angle_deg=DRIFT_DEFAULT_MAX_ANGLE,
                        time_idx=None, channel_idx=None, on_progress=None):
    """Per-frame rigid drift of a timelapse against ``t = 0``, in-plane rotation only.

    Returns ``(positions, angles, interpolated, n_rejected)`` where:

    - ``positions`` is ``(T, 2)`` in (Y, X) order for a 2D movie, or ``(T, 3)`` in (Z, Y, X) for a
      3D movie — matching the axis order the translation estimators use, so the writer treats the
      result the same way.
    - ``angles`` is ``(T,)`` in **degrees** — the single in-plane rotation per frame. 3D movies
      report ONE angle (the Z-axis rotation); X/Y rotations are frozen at zero at fit time. See
      DRIFT_RIGID_PLAN.md Decision 3 ("option B — in-plane rotation only, on a 3D volume") for
      the rationale: a rigid stage bump is in-plane by construction, and letting the fit try X/Y
      rotations makes it trade small tilts against noise on a clean movie. If a user reports a
      movie with genuine sample tilting, that is the "option A" follow-up.
    - ``interpolated`` is the sorted list of frame indices whose fit was rejected by the angle
      cap and predicted from neighbours instead; ``n_rejected == len(interpolated)``.

    Each frame is fit **directly against frame 0**, seeded by the previous frame's converged
    transform as the initial guess. Adjacent-frame overlap is the property that makes chain
    estimators robust; the seeded direct fit gets the same property without accumulating
    chain-bias — see docs/todo/DRIFT_RIGID_PLAN.md Decision 3.
    """
    if channel_idx is None:
        channel_idx = dim_utils.dim_idx('C')
    if time_idx is None:
        time_idx = dim_utils.dim_idx('T')

    frame0 = _drift_frame(image_array, dim_utils, phase_shift_channel, 0,
                          time_idx=time_idx, channel_idx=channel_idx)
    ndim = frame0.ndim
    if ndim not in (2, 3):
        raise ValueError(f"sitkRigid needs 2D or 3D spatial frames, got ndim={ndim}")
    # SimpleITK's `ImageRegistrationMethod` uses a `RecursiveGaussianImageFilter` internally for
    # the metric's gradient regardless of pyramid settings, and that filter needs ≥4 samples per
    # axis. Confocal Z-stacks routinely have 4-40 slices so this only fires on a genuinely
    # degenerate input (2- or 3-slice stack), where the honest answer is to refuse rather than
    # silently max-project or plane-select.
    if min(frame0.shape) < 4:
        raise ValueError(
            f"sitkRigid needs at least 4 samples along every spatial axis "
            f"(got shape {frame0.shape}). For a 2- or 3-slice stack, use 'multiLag' or 'chain'.")
    centre = _rigid_centre(frame0.shape)

    angles_rad = np.zeros(n_t, dtype=float)
    # SimpleITK's translation order: (x, y) in 2D, (x, y, z) in 3D. Stored in that order here and
    # rearranged to the writer's (Y, X) / (Z, Y, X) order once at the end, so this loop doesn't
    # have to remember two conventions.
    translations = np.zeros((n_t, ndim), dtype=float)
    max_angle_rad = float(np.deg2rad(max_angle_deg))
    rejected = np.zeros(n_t, dtype=bool)

    prev_angle = 0.0
    prev_translation = tuple(0.0 for _ in range(ndim))
    for t in range(1, n_t):
        frame_t = _drift_frame(image_array, dim_utils, phase_shift_channel, t,
                               time_idx=time_idx, channel_idx=channel_idx)
        angle, translation = _sitk_rigid_pair(
            frame0, frame_t,
            init_angle_rad=prev_angle,
            init_translation=prev_translation,
            centre=centre)

        if abs(angle) > max_angle_rad:
            rejected[t] = True
            # Do NOT seed the next frame from a rejected fit — a runaway would poison the whole
            # tail. Keep the previous good seed instead.
        else:
            angles_rad[t] = angle
            translations[t] = translation
            prev_angle = angle
            prev_translation = translation

        if on_progress is not None:
            on_progress(t + 1, n_t)

    angles_deg = np.rad2deg(angles_rad)
    if ndim == 2:
        # (x, y) → (y, x)
        positions = np.column_stack([translations[:, 1], translations[:, 0]])
    else:
        # (x, y, z) → (z, y, x)
        positions = np.column_stack([translations[:, 2], translations[:, 1], translations[:, 0]])

    positions, angles_deg = _interpolate_rigid_gaps(positions, angles_deg, rejected)
    interpolated = [int(t) for t in np.where(rejected)[0]]
    return positions, angles_deg, interpolated, int(rejected.sum())


def drift_correction_shifts(
        image_array, phase_shift_channel, dim_utils,
        timepoints=None, upsample_factor=100,
        normalisation=None, time_idx=None, channel_idx=None,
        estimator='multiLag', max_lag=DRIFT_DEFAULT_MAX_LAG):
    """Just the per-frame deltas — `estimate_drift` without the diagnostics.

    Kept because it is the historic entry point and an external consumer may call it; the task
    itself uses `estimate_drift`, which carries the numbers QC needs. ``timepoints`` is accepted
    for signature compatibility and ignored: the estimator needs a consecutive run from frame 0
    (positions accumulate), which is the constraint the writer has always had too.
    """
    return estimate_drift(
        image_array, phase_shift_channel, dim_utils,
        upsample_factor=upsample_factor, normalisation=normalisation,
        estimator=estimator, max_lag=max_lag,
        time_idx=time_idx, channel_idx=channel_idx).shifts


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
        upsample_factor=100, shifts=None, chunk_size=None, out=None,
        on_progress=None):
    if timepoints is None:
        timepoints = range(dim_utils.dim_val('T'))

    if shifts is None:
        shifts = drift_correction_shifts(
            input_array, phase_shift_channel, dim_utils,
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

    # Where each frame lands. Shape arithmetic only, so it is computed up front and shared with
    # every other consumer of this store (the QC sidecar records it) instead of living inline here
    # — see drift_frame_slices.
    frame_slices = drift_frame_slices(input_array, dim_utils, shifts, timepoints)

    t_idx = dim_utils.dim_idx('T')
    # Assign the frame straight into its destination window rather than building a full canvas
    # frame around it and writing that. The canvas is mostly padding — 1.03–9.26x the input across
    # the movies on this machine — and the padding is already what an unwritten chunk reads as, so
    # materialising and compressing it was work with no output. Pixels are identical either way
    # (asserted in `test_drift_geometry.py`); on `4kS67f/fHqhyb` the write phase goes 22.2 s → 19.1 s
    # and the untouched padding chunks are never created at all.
    timepoints = list(timepoints)
    for n, i in enumerate(timepoints):
        im_slices = [slice(None)] * len(drift_im_shape_round)
        im_slices[t_idx] = slice(i, i + 1, 1)
        src = zarr_utils.fortify(input_array[tuple(im_slices)])

        dest = list(frame_slices[i])
        dest[t_idx] = slice(i, i + 1, 1)
        result[tuple(dest)] = src

        if on_progress is not None:
            on_progress(n + 1, len(timepoints))

    return result


# ── Rigid (rotation-aware) drift application ─────────────────────────────────
#
# Applies a per-frame `(angle, translation)` trajectory to a timelapse. The translation-only
# applier above places each frame at a slice offset — no resampling, so the pixels are copied
# verbatim. Rigid needs a real resample per frame (SimpleITK `Euler2D/3DTransform`), and the
# canvas is the union of the axis-aligned bounding boxes of every rotated frame in frame 0's
# coord system — which is bigger than the translation-only canvas for the same trajectory.
#
# Design lives in docs/todo/DRIFT_RIGID_PLAN.md. The transform's rotation centre is the frame
# centre — the same `_rigid_centre` the fit used, so the geometry and the applier stay honest to
# each other. Frame-t → canvas maps through `T_t.GetInverse()`, since SimpleITK's Euler transform
# maps fixed→moving (frame_0 → frame_t) and we want the direction that goes the other way.


def _rigid_transform(angle_deg, translation_yx_or_zyx, centre):
    """Build the `Euler2DTransform` / `Euler3DTransform` for a per-frame `(angle, translation)`.

    ``translation_yx_or_zyx`` is (Y, X) for 2D or (Z, Y, X) for 3D — the axis order the writer
    stores in ``DriftEstimate.positions``. This helper flips it to SimpleITK's (x, y[, z]) order
    once, so callers do not have to remember two conventions.
    """
    import SimpleITK as sitk
    trans = np.asarray(translation_yx_or_zyx, dtype=float)
    if trans.size == 2:
        tx = sitk.Euler2DTransform()
        tx.SetCenter(centre)                                       # already (x, y)
        tx.SetAngle(float(np.deg2rad(angle_deg)))
        tx.SetTranslation((float(trans[1]), float(trans[0])))      # (y, x) → (x, y)
        return tx
    if trans.size == 3:
        tx = sitk.Euler3DTransform()
        tx.SetCenter(centre)                                       # already (x, y, z)
        tx.SetRotation(0.0, 0.0, float(np.deg2rad(angle_deg)))     # in-plane only — option B
        tx.SetTranslation((float(trans[2]), float(trans[1]), float(trans[0])))
        return tx
    raise ValueError(f"_rigid_transform expects 2 or 3 translation components, got {trans.size}")


def _rigid_frame_bbox(shape_yx_or_zyx, angle_deg, translation_yx_or_zyx, centre):
    """Axis-aligned bbox of the source rectangle rotated + translated back into frame 0's coord
    system. Returns ``{'Y': (yMin, yMax), 'X': (xMin, xMax), 'Z': ...}`` as **floats** — the
    canvas-shape helper rounds; the applier uses the same floats to place its frames.

    The transform SimpleITK fitted maps fixed (frame 0) → moving (frame t). The inverse maps
    frame t → frame 0, which is the direction we need to say "where does this source pixel end up
    in the canvas". Uses `.GetInverse()` on the SimpleITK transform so the math stays honest to
    what `sitk.Resample` will actually paint.
    """
    tx = _rigid_transform(angle_deg, translation_yx_or_zyx, centre)
    inv = tx.GetInverse()
    ndim = len(shape_yx_or_zyx)
    if ndim == 2:
        h, w = shape_yx_or_zyx
        corners = [(0.0, 0.0), (float(w) - 1.0, 0.0),
                   (0.0, float(h) - 1.0), (float(w) - 1.0, float(h) - 1.0)]
        mapped = [inv.TransformPoint(c) for c in corners]           # (x, y)
        xs = [p[0] for p in mapped]; ys = [p[1] for p in mapped]
        return {'Y': (min(ys), max(ys)), 'X': (min(xs), max(xs))}
    if ndim == 3:
        d, h, w = shape_yx_or_zyx
        # 8 corners of the box, in SimpleITK's (x, y, z) order
        corners = [(0.0, 0.0, 0.0), (float(w) - 1.0, 0.0, 0.0),
                   (0.0, float(h) - 1.0, 0.0), (float(w) - 1.0, float(h) - 1.0, 0.0),
                   (0.0, 0.0, float(d) - 1.0), (float(w) - 1.0, 0.0, float(d) - 1.0),
                   (0.0, float(h) - 1.0, float(d) - 1.0),
                   (float(w) - 1.0, float(h) - 1.0, float(d) - 1.0)]
        mapped = [inv.TransformPoint(c) for c in corners]           # (x, y, z)
        xs = [p[0] for p in mapped]; ys = [p[1] for p in mapped]; zs = [p[2] for p in mapped]
        return {'Z': (min(zs), max(zs)), 'Y': (min(ys), max(ys)), 'X': (min(xs), max(xs))}
    raise ValueError(f"_rigid_frame_bbox expects 2D or 3D shape, got {shape_yx_or_zyx}")


def rigid_correct_geometry(input_array, dim_utils, positions, angles):
    """Canvas shape and per-frame valid box for a rigid-corrected timelapse.

    Returns ``(canvas_shape_round, canvas_origin_xyz, frame_bboxes)`` where:
    - ``canvas_shape_round`` matches the layout of ``input_array.shape`` — non-spatial axes
      unchanged, spatial axes expanded to hold every frame's rotated bbox.
    - ``canvas_origin_xyz`` is SimpleITK's (x, y) or (x, y, z) origin for the canvas — the
      applier passes it as `SetOrigin` on the reference image so a `sitk.Resample` from frame_0
      coordinates lands each frame in the right place.
    - ``frame_bboxes`` is ``{t: {axis: (start, stop)}}`` in **canvas index space, integer** — the
      valid region per frame, ready for `zarr_utils.write_valid_box`.

    Pure shape arithmetic; no pixels read. Same discipline as `drift_frame_slices`.
    """
    src_shape = tuple(getattr(input_array, 'shape', input_array))
    spatial_axes = list(dim_utils.spatial_axis())                    # ['Z','Y','X'] or ['Y','X']
    is_3d = dim_utils.is_3D()

    # Frame's spatial shape in ZYX / YX order (numpy)
    if is_3d:
        frame_shape = (src_shape[dim_utils.dim_idx('Z')],
                       src_shape[dim_utils.dim_idx('Y')],
                       src_shape[dim_utils.dim_idx('X')])
    else:
        frame_shape = (src_shape[dim_utils.dim_idx('Y')],
                       src_shape[dim_utils.dim_idx('X')])
    centre = _rigid_centre(frame_shape)

    # Union of per-frame bboxes in frame_0's coord frame (unshifted).
    axes_bounds = {ax: [np.inf, -np.inf] for ax in spatial_axes}
    per_frame = []
    for t in range(positions.shape[0]):
        b = _rigid_frame_bbox(frame_shape, float(angles[t]), positions[t], centre)
        per_frame.append(b)
        for ax in spatial_axes:
            lo, hi = b[ax]
            if lo < axes_bounds[ax][0]:
                axes_bounds[ax][0] = lo
            if hi > axes_bounds[ax][1]:
                axes_bounds[ax][1] = hi

    # Canvas origin: the negative of the lowest per-axis extent, so every frame's bbox is
    # non-negative in canvas space. In SimpleITK (x, y[, z]) order for the reference image origin.
    if is_3d:
        canvas_origin_xyz = (axes_bounds['X'][0], axes_bounds['Y'][0], axes_bounds['Z'][0])
    else:
        canvas_origin_xyz = (axes_bounds['X'][0], axes_bounds['Y'][0])

    # Canvas shape per axis: ceil(hi - lo) + 1, at least the source shape.
    canvas_shape = list(src_shape)
    for ax in spatial_axes:
        lo, hi = axes_bounds[ax]
        n = int(np.ceil(hi - lo)) + 1
        src_n = src_shape[dim_utils.dim_idx(ax)]
        canvas_shape[dim_utils.dim_idx(ax)] = max(n, src_n)

    # Per-frame bbox in canvas-index space (int).
    frame_bboxes = {}
    for t, b in enumerate(per_frame):
        entry = {}
        for ax in spatial_axes:
            lo, hi = b[ax]
            axis_origin = canvas_origin_xyz[
                {'X': 0, 'Y': 1, 'Z': 2}[ax] if is_3d else {'X': 0, 'Y': 1}[ax]]
            entry[ax] = (int(np.floor(lo - axis_origin)),
                         int(np.ceil(hi - axis_origin)) + 1)
        frame_bboxes[t] = entry
    return tuple(canvas_shape), canvas_origin_xyz, frame_bboxes


def rigid_correct_im(input_array, dim_utils, positions, angles,
                     timepoints=None, chunk_size=None, out=None, on_progress=None):
    """Apply a rigid trajectory to every channel of a timelapse and stream the result into ``out``.

    Same streaming pattern as `drift_correct_im`: create the canvas once, fill one timepoint at a
    time so the expanded corrected image never lives in RAM. Per timepoint, per channel:
    `sitk.Resample` maps canvas → source using the `Euler2D/3DTransform` that carries this frame's
    `(angle, translation)`, and the resampled slab lands at the whole-frame position (no
    per-channel offset — a rigid trajectory is the same for every channel by construction).

    Returns whichever it wrote into (`out` if provided, otherwise a freshly allocated numpy).
    """
    import SimpleITK as sitk

    if timepoints is None:
        timepoints = range(dim_utils.dim_val('T'))
    timepoints = list(timepoints)

    canvas_shape, canvas_origin_xyz, _ = rigid_correct_geometry(
        input_array, dim_utils, positions, angles)
    result_dtype = out.dtype if out is not None else input_array.dtype
    result = out if out is not None else np.zeros(canvas_shape, dtype=result_dtype)

    t_idx = dim_utils.dim_idx('T')
    c_idx = dim_utils.dim_idx('C')
    is_3d = dim_utils.is_3D()

    # Frame's spatial shape (Z,Y,X or Y,X) — same layout the fit used.
    if is_3d:
        frame_shape = (input_array.shape[dim_utils.dim_idx('Z')],
                       input_array.shape[dim_utils.dim_idx('Y')],
                       input_array.shape[dim_utils.dim_idx('X')])
    else:
        frame_shape = (input_array.shape[dim_utils.dim_idx('Y')],
                       input_array.shape[dim_utils.dim_idx('X')])
    centre = _rigid_centre(frame_shape)

    # SimpleITK canvas shape: (x, y[, z]) order — the reverse of numpy's (z, y, x). Same reference
    # image reused for every frame; only its default pixel value + input differ per resample.
    if is_3d:
        canvas_size_sitk = (canvas_shape[dim_utils.dim_idx('X')],
                            canvas_shape[dim_utils.dim_idx('Y')],
                            canvas_shape[dim_utils.dim_idx('Z')])
    else:
        canvas_size_sitk = (canvas_shape[dim_utils.dim_idx('X')],
                            canvas_shape[dim_utils.dim_idx('Y')])

    ref = sitk.Image(canvas_size_sitk, sitk.sitkFloat32)
    ref.SetOrigin(canvas_origin_xyz)
    # spacing = 1 by default; pixel values in `ref` are irrelevant (only its geometry is used).

    n_channels = dim_utils.dim_val('C')
    channels = list(range(n_channels)) if n_channels else [0]

    for n, t in enumerate(timepoints):
        tx = _rigid_transform(float(angles[t]), positions[t], centre)
        for c in channels:
            # Pull one (t, c) spatial slab out of the source.
            sl_src = [slice(None)] * len(input_array.shape)
            sl_src[t_idx] = slice(t, t + 1, 1)
            if c_idx is not None:
                sl_src[c_idx] = slice(c, c + 1, 1)
            src = np.squeeze(zarr_utils.fortify(input_array[tuple(sl_src)]))
            # `Resample` needs a float image; the OUTPUT is cast back to the store's dtype below.
            src_img = sitk.GetImageFromArray(src.astype(np.float32, copy=False))
            src_img.SetOrigin(tuple(0.0 for _ in canvas_size_sitk))

            out_img = sitk.Resample(src_img, ref, tx, sitk.sitkLinear, 0.0)
            painted = sitk.GetArrayFromImage(out_img)                # (Z, Y, X) or (Y, X)

            # Cast + clip to the destination dtype. `result_dtype` is uint8/uint16 for microscopy
            # stores; the linear resampler produces floats in the same range so rounding is enough.
            if np.issubdtype(result_dtype, np.integer):
                info = np.iinfo(result_dtype)
                painted = np.clip(np.round(painted), info.min, info.max).astype(result_dtype)
            else:
                painted = painted.astype(result_dtype, copy=False)

            # Write back into result at (t, c, spatial).
            sl_dst = [slice(None)] * len(canvas_shape)
            sl_dst[t_idx] = slice(t, t + 1, 1)
            if c_idx is not None:
                sl_dst[c_idx] = slice(c, c + 1, 1)
            # `painted` has no T/C axes — expand to fit the destination slab shape.
            expanded_shape = list(canvas_shape)
            expanded_shape[t_idx] = 1
            if c_idx is not None:
                expanded_shape[c_idx] = 1
            result[tuple(sl_dst)] = painted.reshape(expanded_shape)

        if on_progress is not None:
            on_progress(n + 1, len(timepoints))

    return result


def rigid_frame_origins(input_array, dim_utils, positions, angles):
    """`rigid_correct_geometry`'s per-frame bboxes in the same shape `drift_frame_origins`
    returns: ``{t: {axis: [start, stop]}}`` as plain ints, for `zarr_utils.write_valid_box`.
    Kept structurally parallel to the translation helper so the writer doesn't branch."""
    _, _, bboxes = rigid_correct_geometry(input_array, dim_utils, positions, angles)
    return {t: {ax: [int(bboxes[t][ax][0]), int(bboxes[t][ax][1])]
                for ax in bboxes[t]}
            for t in bboxes}


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


#: The GLOBAL values the correction needs before it can touch a single voxel: a background level per
#: participating channel, and a BLEEDTHROUGH coefficient per ordered channel pair. Whole-image by
#: definition, which is why they are a separate, cacheable step — see `af_weight_stats`. ``saturated``
#: rides along because the same pass already has the histograms, and input saturation is the one thing
#: about this correction worth warning about.
AfWeightStats = collections.namedtuple('AfWeightStats',
                                       'backgrounds alphas saturated exponent nbins')

#: Paired voxels sampled for the bleedthrough fit, across the whole derivation. `alpha` is ONE SCALAR
#: PER CHANNEL PAIR over the entire movie, so this is a statistical question and not a per-voxel one:
#: measured on `WIaUjL/p6t4mC`, 3.1M pooled voxels gave 0.0231 and the per-plane estimates over the same
#: movie ran 0.020-0.045, i.e. the pooled number is already tighter than the frame-to-frame spread. The
#: cap exists so the sample cannot grow with the movie — 2M float64 pairs is ~16 MB per channel.
AF_ALPHA_MAX_SAMPLES = 2_000_000

#: Below this, a fitted coefficient is reported as ZERO rather than applied. Two reasons, and the second
#: is the load-bearing one. A fit always returns *something*, so without a floor every channel pair
#: acquires a small spurious leak and the correction starts subtracting noise from signal. And
#: `coloc_utils.envelope_slope` is measured to err HIGH at small alpha (+55% at 0.01), which is exactly
#: the regime where a fitted number is least trustworthy.
#:
#: 0.005 has margin on both sides, measured. On INDEPENDENT channels with realistic statistics the
#: estimate never exceeded **0.0009** at any sample size from 5k to 3M voxels (8 seeds each), and on the
#: real four-channel `WIaUjL/p6t4mC` eleven of the twelve ordered pairs came back at exactly 0.0000. A
#: genuine leak on that image sits at **0.023**. So the floor is ~5x above the noise and ~4x below the
#: smallest real leak seen. (Uniformly-distributed synthetic channels DO produce spurious fits around
#: 0.01, but a fluorescence channel is mostly background and nothing like uniform — the same degenerate
#: input `af_weight_stats` already warns about for the triangle threshold.)
AF_ALPHA_MIN = 0.005

#: A leak cannot exceed its source: `alpha >= 1` would mean the target receives more than the competing
#: channel emits. Physical rather than tuned, and load-bearing for the exclusive path — `tls_slope` is
#: symmetric, so both directions of a pair always fit (at `a` and `1/a`), and this bound is what leaves
#: exactly the one running from the brighter channel into the dimmer.
AF_ALPHA_MAX = 1.0

#: **There is deliberately no fit-quality gate to go with it, and that is worth writing down because the
#: obvious one does not work.** R^2 of the envelope fit looks like the way to tell a leak from a
#: coincidence, and it is not: a leak-free pair has a FLAT floor, which a flat line fits beautifully, so
#: a synthetic pair with no leak at all scores 0.47 while the real CH3->CH2 leak scores 0.43. R^2 says
#: how well determined the slope is, never whether it is non-zero. `envelope_slope` still returns it,
#: for reporting; gating on it would have rejected the real leak and admitted the null.

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
        timepoints=None, spatial_stride=(1, 1), exponent=AF_WEIGHT_EXPONENT,
        on_progress=None, exclusive=None):
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

    # Per-frame quota for the PAIRED sample the bleedthrough fit needs. A histogram is a marginal and
    # cannot answer a joint question, so the pairs have to be carried out of this loop — the alternative
    # is a second full pass over the movie for one scalar per channel pair.
    quota = max(1, int(AF_ALPHA_MAX_SAMPLES) // max(1, len(ts))) if len(channels) > 1 else 0

    hists = {ch: np.zeros(nbins, np.int64) for ch in channels}
    pairs = {ch: [] for ch in channels}
    # `on_progress(n, total)` per TIMEPOINT, not per channel-slab: this pass is one of the two
    # minutes-long spans in the task and it used to report nothing at all. The unit matches the
    # correction loop's below so a single scale can span both — see `af_progress_total`.
    for n_done, t in enumerate(ts, start=1):
        take = None
        for ch in channels:
            a = _stride(_af_slab(data, dim_utils, ch, t))
            hists[ch] += np.bincount(np.clip(a, 0, nbins - 1).astype(np.int64).ravel(),
                                     minlength=nbins)[:nbins]
            if quota:
                flat = a.ravel()
                # the SAME positions in every channel, or the samples are not pairs at all. Evenly
                # spaced rather than random: no seed to carry, and a fluorescence frame has no
                # structure at the sampling period that a regular grid could alias with.
                if take is None:
                    take = (np.arange(min(quota, flat.size), dtype=np.int64) *
                            max(1, flat.size // max(1, min(quota, flat.size))))
                    take = take[take < flat.size]
                pairs[ch].append(flat[take].astype(np.float64))
        if on_progress is not None:
            on_progress(n_done, len(ts))

    backgrounds = {ch: float(intensity_utils.background_threshold(hists[ch], background_method))
                   for ch in channels}
    saturated = {ch: float(hists[ch][nbins - 1]) / max(1.0, float(hists[ch].sum()))
                 for ch in channels}
    samples = ({ch: np.concatenate(v) for ch, v in pairs.items()} if quota and any(pairs.values())
               else {})
    alphas = af_bleedthrough_alphas(samples, backgrounds, exclusive) if samples else {}
    return AfWeightStats(backgrounds=backgrounds, alphas=alphas, saturated=saturated,
                         exponent=int(exponent), nbins=nbins)


def af_bleedthrough_alphas(samples, backgrounds, exclusive=None):
    """Bleedthrough coefficient per ORDERED channel pair, from paired background-subtracted samples.

    ``samples`` maps a channel index to a flat array of voxel values, all arrays sampled at the SAME
    positions. Returns ``{(src, dst): alpha}`` for every ordered pair whose fit clears `AF_ALPHA_MIN`;
    a pair with no detectable leak is simply absent, which is the honest way to say "nothing to
    subtract" and is what makes an all-zero result cost nothing downstream.

    **``exclusive`` maps a TARGET channel to whether a voxel can carry both markers, and it chooses the
    estimator — because that is a question about the experiment, not about the data.**

    * ``True`` (different cell types, no co-labelling) → `coloc_utils.tls_slope`. With nothing
      legitimately co-located, the ENTIRE proportional relationship between the two channels is leak,
      so the total regression is the estimate and there is nothing for it to over-remove.
    * ``False`` (cells may carry both) → `coloc_utils.envelope_slope`. The floor of the joint
      distribution is the part attributable to the optics; anything above it may be real co-labelling
      and must survive (`docs/todo/AF_CORRECTION_AUDIT.md`).

    The two answers differ by ~5x on real data and the choice is not inferable from the pixels. On
    `WIaUjL/p6t4mC` — two reporters, two cell types, no overlap — the envelope gave **0.024** and left
    the residual on the CH3-brightest voxels at **2.5x** the target's level elsewhere, i.e. visibly
    uncorrected; the TLS slope gave **0.13**, which levels it out while leaving 100% of the target's own
    signal (those voxels have no competitor to subtract). Defaulting to co-labelled would have made that
    the experience of every mutually-exclusive pair, which is the common case.

    **Why this is a separate job from the dominance weight.** The task has two of them
    (`docs/todo/AF_CORRECTION_AUDIT.md` → *The task has TWO jobs, and only one mechanism*): removing
    intensity present in more than one channel because the tissue is autofluorescent, and removing
    intensity present in more than one channel because the filter set leaks. They are separable because
    only one is proportional — bleedthrough is a property of the optics, so every voxel where the source
    is bright gets the same fraction added, which is a straight line through the FLOOR of the joint
    distribution. Broadband autofluorescence varies structure to structure and sits above that floor.
    `coloc_utils.envelope_slope` fits the floor and is therefore blind to the autofluorescence, which is
    the property the whole split rests on and is pinned by
    `test_coloc_utils.test_a_co_present_structure_barely_moves_the_envelope`.

    **Why it must run BEFORE the weight, not instead of it.** A dominance weight answers "which channel
    owns this voxel" by *scaling*, so where a real target cell overlaps a brighter competitor it removes
    a fraction of a genuine cell rather than the leaked amount. On `WIaUjL/p6t4mC` (CH3 into CH2, alpha
    0.023) that was the difference between keeping **5.6-7.4%** of co-positive CH2 and keeping
    **82-85%** of it — while the leak-only voxels still lost 93-98% of their signal. Subtracting an
    amount and scaling by a fraction are not interchangeable, and the leak is an amount.

    A coefficient is deliberately NOT derived for a pair whose source is too dim to fit against: the
    reverse direction of a real one-way leak is exactly that case, and it is where a fit invents numbers
    (measured on the same image, CH3->CH2 came back 0.020-0.045 across timepoints while CH2->CH3 swung
    0.000-0.179). `AF_ALPHA_MIN` plus `envelope_slope`'s own nan-on-too-little-data is what stops those
    reaching a voxel.
    """
    from cecelia.utils import coloc_utils

    b = {int(ch): np.maximum(np.asarray(v, dtype=np.float64) - float(backgrounds.get(int(ch), 0.0)), 0)
         for ch, v in samples.items()}
    exclusive = dict(exclusive or {})
    fits = {}
    for src in sorted(b):
        for dst in sorted(b):
            if src == dst:
                continue
            if bool(exclusive.get(dst, True)):
                # symmetric by construction: fitting the other way round returns 1/a. The physical
                # bound below is what picks the direction — a leak cannot exceed 100% of its source,
                # so of the two reciprocals only the one from the brighter channel can survive.
                alpha, _intercept = coloc_utils.tls_slope(b[src], b[dst])
                r2 = 1.0        # no separate fit quality; the direction rule below decides instead
            else:
                alpha, r2, _bins = coloc_utils.envelope_slope(b[src], b[dst])
            if np.isfinite(alpha) and AF_ALPHA_MIN <= alpha < AF_ALPHA_MAX:
                fits[(src, dst)] = (float(alpha), float(r2) if np.isfinite(r2) else 0.0)

    # ONE DIRECTION per pair. A proportional leak has a direction — the filter set passes some of the
    # brighter emitter into the dimmer channel's band — and two channels cannot each be a fixed fraction
    # of the other. When both directions fit, the relationship is shared structure rather than an
    # identifiable leak, and subtracting both would take a bite out of each channel on the strength of
    # the other. Keep the better-supported direction only.
    #
    # This is NOT a general solution to mutual bleedthrough. Genuine two-way spectral overlap is a
    # linear system and wants an inverted mixing matrix, not two independent subtractions; that is out
    # of scope here and the one-way case is what real filter sets mostly present. Measured on
    # `WIaUjL/p6t4mC`, exactly one of the twelve ordered pairs among four channels fitted at all.
    out = {}
    for (src, dst), (alpha, r2) in fits.items():
        rev = fits.get((dst, src))
        # Both surviving means neither channel is clearly the source. Keep the SMALLER coefficient: a
        # leak runs from the brighter channel into the dimmer one, so the smaller fraction is the
        # physically sensible reading and the larger would take a bite out of the source itself.
        if rev is None or alpha < rev[0] or (alpha == rev[0] and r2 >= rev[1]):
            out[(src, dst)] = alpha
    return out


def af_correct_frame(slabs, target, stats, out_dtype):
    """Correct ONE frame of ONE channel — two jobs, in the one order that works.

        b_i    = max(raw_i - background_i, 0)          for the target and every competing channel
        b_i   <- max(b_i - Σ_j α_ji · b_j, 0)          (a) BLEEDTHROUGH: subtract the leaked amount
        out_t  = b_t * b_t^p / Σ_i b_i^p               (b) CO-PRESENCE: keep the share this one owns

    (a) removes intensity that is in the target because the FILTER SET leaks — proportional, global, a
    property of the optics (`af_bleedthrough_alphas`). (b) removes intensity that is in more than one
    channel because the TISSUE is autofluorescent — structure-specific, not proportional. The task has
    always had both jobs and, until this, only mechanism (b); see
    `docs/todo/AF_CORRECTION_AUDIT.md` → *The task has TWO jobs, and only one mechanism*.

    **Subtracting an amount is not interchangeable with scaling by a fraction, and using (b) for (a)'s
    job is what broke `WIaUjL/p6t4mC`.** CH3 leaked 2.3% into CH2 and was ~7x brighter, so the weight
    read every co-positive voxel as CH3's and kept **5.6-7.4%** of the target's own signal there —
    corrected CH2 came out 98-99% zero and segmenting it found CH3. Unmixing first keeps **82-85%** of
    that co-positive signal while the leak-only voxels still lose 93-98%.

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

    # (a) BLEEDTHROUGH — subtract the leaked AMOUNT, before anything scales anything. Order matters and
    # is not a preference: the weight below multiplies, so a leak left in place would be partly kept
    # wherever the target dominates and would drag the target's own share down wherever it does not.
    # Clamped at zero because `envelope_slope` errs high by design (see its docstring), so the honest
    # reading of a negative result is "nothing left", not "negative fluorescence".
    # attribute access, not `getattr(..., default)`: a stats object without `alphas` is one derived by
    # code that predates the unmix, and silently treating it as "no leak" is exactly the quiet wrong
    # answer the `backgrounds` guard above refuses to give
    alphas = stats.alphas or {}
    if alphas:
        leaked = {}
        for (src, dst), alpha in alphas.items():
            if src in b and dst in b:
                leaked[dst] = leaked.get(dst, 0.0) + float(alpha) * b[src]
        # every participating channel is unmixed, not just the target: a competitor still carrying the
        # target's leak would claim the target's own voxels in the weight below
        for ch, amount in leaked.items():
            b[ch] = np.maximum(b[ch] - amount, 0)

    # (b) CO-PRESENCE — the dominance weight, over the competitors the unmix did NOT already account for.
    #
    # **A channel is one or the other, never both, and getting this wrong is what makes (a) pointless.**
    # Once `src` has been unmixed out of `target`, what remains in the target is by construction the part
    # NOT explained by src — there is nothing left for a dominance contest against src to decide. Leaving
    # it in the denominator just re-removes the same overlap, multiplicatively this time, and the
    # subtraction buys nothing: measured on `WIaUjL/p6t4mC`, unmix-then-weight kept **6.4%** of
    # co-positive target signal against **7.4%** for the weight alone and **84.9%** for the unmix alone.
    # The weight, not the leak, was doing the erasing.
    #
    # The partition is DERIVED, not configured, and it is derived by the thing that distinguishes the two
    # jobs in the first place: a competitor whose contribution fits a proportional law is optics, and one
    # whose does not is autofluorescence. `AF_ALPHA_MIN` is where that decision is actually made.
    unmixed_from_target = {src for (src, dst) in alphas if dst == target}
    num = b[target] ** p
    den = num.copy()
    for ch, v in b.items():
        if ch != target and ch not in unmixed_from_target:
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
                              background_method='triangle', stats=None, logfile_utils=None,
                              on_progress=None):
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
        leaks = ', '.join(f'ch{s}->ch{d} {a:.4f}' for (s, d), a in sorted(stats.alphas.items())
                          if d == int(channel_idx))
        logfile_utils.log(
            f'>> ch{channel_idx}: background {stats.backgrounds.get(int(channel_idx), 0.0):.0f} '
            f'({background_method}), competing against {others}, p={stats.exponent}, '
            f'bleedthrough {leaks or "none detected"}')

    # Output histogram, accumulated as we go — the objective signal the QC exemption in af_correct.jl
    # said was missing. Free: one bincount per frame we already hold.
    H_out = np.zeros(stats.nbins, np.int64)

    for t in range(T):
        corrected = af_correct_frame(_af_slabs(data, dim_utils, channels, t),
                                     channel_idx, stats, out.dtype)
        if on_progress is not None:
            on_progress(t + 1, T)
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
        # Bleedthrough INTO this channel, per source. The audit's point about alpha is that its value to
        # a user is diagnostic as much as corrective: a non-zero coefficient says the filter set leaks
        # (go look at the optics), a zero one says what was removed was tissue autofluorescence
        # (nothing to fix). "No bleedthrough detected" is a real result, so an empty dict is reported
        # rather than the key being absent.
        'bleedthrough': {str(src): float(a) for (src, dst), a in sorted(stats.alphas.items())
                         if dst == target},
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


def _copy_channel(input_image, out, i, dim_utils, on_progress=None):
    """Carry a channel no combination covers through unchanged, one timepoint at a time.

    It used to denoise, gaussian-blur, rolling-ball and top-hat these channels — by DEFAULT, since
    `applyGaussianToOthers` was true, so the AF task silently filtered channels it wasn't correcting.
    A correction task has no business modifying a channel nobody asked it to touch.
    """
    T = dim_utils.dim_val('T') if dim_utils.is_timeseries() else 1
    for t in range(T):
        _af_write_slab(out, dim_utils, i, t,
                       _af_slab(input_image, dim_utils, i, t).astype(out.dtype))
        # reported on the same scale as a CORRECTED channel: copying a channel is most of a pass over
        # the movie, so leaving it out made the bar stall for exactly as long as it takes
        if on_progress is not None:
            on_progress(t + 1, T)


def af_participating_channels(af_combinations):
    """Every channel any combination reads — targets and competitors, deduped, in order.

    The set the globals are derived over. One pure function because two callers need to agree on it:
    `af_correct_image`, which derives once over exactly this set, and `af_progress_total`, which has to
    predict whether a derivation pass will happen at all.
    """
    out = []
    for target, spec in sorted({int(i): x for i, x in af_combinations.items()}.items()):
        competing = (spec or {}).get('competingChannels', []) or []
        if not competing:
            continue
        for ch in [target] + [int(c) for c in competing]:
            if ch not in out:
                out.append(ch)
    return out


def af_progress_total(dim_utils, af_combinations, nscales=1):
    """Total progress units for one AF run. **One unit = one timepoint of one pass over the movie.**

    Every span of this task is a per-timepoint loop, so a single unit spans all of them and the bar
    moves at roughly one rate throughout:

    * the global derivation — ONE pass, over the participating channels together (`af_weight_stats`)
    * one pass per channel written — corrected (`_stream_corrected_channel`) or carried through
      (`_copy_channel`), which costs a pass either way
    * one pass per pyramid level (`zarr_utils.write_multiscale_pyramid`)

    Here rather than in the runner so the reporter and the total cannot disagree — the runner needs the
    number before the work starts, and the failure mode of two copies of this formula is a bar that
    stops at 80% or claims to finish early. Pinned by `test_af_progress_total_matches_the_ticks`.
    """
    n_t = int(dim_utils.dim_val('T')) if dim_utils.is_timeseries() else 1
    n_c = int(dim_utils.dim_val('C'))
    derivation = 1 if af_participating_channels(af_combinations) else 0
    return n_t * (derivation + n_c + max(0, int(nscales) - 1))


def af_correct_image(input_image, af_combinations, dim_utils, logfile_utils,
                     background_method='triangle', out=None, output_stats=None,
                     on_progress=None, progress_total=None, progress_offset=0):
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

    ``on_progress(n, total)`` is called per timepoint across EVERY span — see `af_progress_total` for
    the unit. ``progress_total``/``progress_offset`` let a caller place these ticks inside a larger
    scale that also covers the pyramid build, which is the other minutes-long span.
    """
    n_channels = dim_utils.dim_val('C')
    af_combinations = {int(i): x for i, x in af_combinations.items()}

    if out is None:   # legacy/small: allocate the full output (compute still streams per frame)
        out = np.zeros(af_correction_output_shape(input_image, dim_utils, af_combinations),
                       dtype=zarr_utils.native_dtype(input_image.dtype))

    total = progress_total or af_progress_total(dim_utils, af_combinations)
    done = int(progress_offset)

    def _tick(n, _sub_total):
        if on_progress is not None:
            on_progress(done + n, total)

    # ONE derivation, over every participating channel at once, reused by every combination.
    #
    # This used to run per corrected channel (`_stream_corrected_channel` derives its own when handed
    # `stats=None`), and the numbers were identical every time: a background is a property of a channel
    # and an alpha of a channel PAIR, so neither depends on which combination asked. A two-combination
    # setup over one channel pair therefore paid for two full passes over the movie to compute the same
    # answer twice — exactly the waste `PreviewState.af_stats` documents on the preview side (measured
    # there at 80.7 s against one pass). Deriving here also makes the progress total predictable, which
    # it cannot be while the number of passes depends on the combination count.
    participating = af_participating_channels(af_combinations)
    stats = None
    if participating:
        if logfile_utils is not None:
            logfile_utils.log(f'>> derive globals over channels {participating} '
                              f'({background_method})')
        # `exclusive` is per TARGET channel, which is per combination — it says whether a voxel of
        # this channel can also legitimately be the competing one, and that is what picks the
        # bleedthrough estimator (see `af_bleedthrough_alphas`). Defaults True: different cell types is
        # the common case, and the other default leaves a mutually-exclusive pair visibly uncorrected.
        exclusive = {int(t): bool((spec or {}).get('exclusive', True))
                     for t, spec in af_combinations.items()}
        if logfile_utils is not None:
            co = sorted(t for t, ex in exclusive.items() if not ex)
            logfile_utils.log(f'>> derive globals over channels {participating} '
                              f'({background_method})' +
                              (f'; channels {co} may be co-labelled' if co else
                               '; all targets are distinct cell types'))
        stats = af_weight_stats(input_image, dim_utils, participating,
                                background_method=background_method, on_progress=_tick,
                                exclusive=exclusive)
        done += int(dim_utils.dim_val('T')) if dim_utils.is_timeseries() else 1

    output_stats = {} if output_stats is None else output_stats
    for i in range(n_channels):
        x = af_combinations.get(i)
        competing = x.get('competingChannels', []) if x is not None else []
        if competing:
            output_stats[str(i)] = _stream_corrected_channel(
                input_image, out, dim_utils, channel_idx=i, out_ch=i,
                competing_channel_idx=competing,
                background_method=background_method,
                stats=stats,
                logfile_utils=logfile_utils,
                on_progress=_tick)
        else:
            _copy_channel(input_image, out, i, dim_utils, on_progress=_tick)
        done += int(dim_utils.dim_val('T')) if dim_utils.is_timeseries() else 1
    return out
