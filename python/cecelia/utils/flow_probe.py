"""Does this engine still produce the flow features the model was fitted on?

The manifest records the feature CONFIG — `temporalScales`, `cumulativeWindow`, `droppedMetrics`,
`metricKeys` — and inference checks every one of them. None of that describes how a metric plane is
*computed*. Swap the optical-flow estimator, change a Farneback parameter, reintroduce an 8-bit cast
in front of it, and every one of those fields still matches while the network is handed a different
input distribution. Measured on zolIMa/fXgbTl mem-TOM: `cv2.DISOpticalFlow` (which only accepts
8-bit input, so it forces back the cast coastal removed in PR #19) gives a magnitude field
CORRELATED 0.00 with the current one, and moves the cell/background separation from 3.68 to 0.84 —
i.e. background flowing faster than cells. Nothing in the manifest would have noticed.

`coastalBuild` is not the answer. It changes on every coastal commit, so as a check it fires on
docstring edits, and MODEL_VAULT_PLAN.md says outright that it "makes a discrepancy explainable; it
does not prevent one".

So the check is NUMERIC. Run the real inference entry point — `coastal.flow.flow_metrics_for_frame`,
which is asserted elementwise-equal to the training path's `prepare_data_for_unet`, so one probe
covers both — on a fixed synthetic window, and record a summary of every metric plane it returns.
Anything that changes the numbers changes the summary; anything that does not, does not. That is the
property a source hash or a version string cannot give.
"""

import math

import numpy as np

# Bump when the probe itself changes. A recorded fingerprint from a different probe version is not
# comparable, and saying so is the point — silently comparing two different measurements would be a
# worse failure than not checking.
VERSION = 1

# Deliberately SMALL and fixed. The probe runs once per model per run and its only job is to be
# sensitive to the recipe, so it costs ~10 ms rather than being representative of a real plane.
SHAPE = (6, 48, 48)

# Fixed, and NOT the model's own values: this measures the engine, not the configuration. The
# configuration is recorded and checked separately (`temporal_config`), and mixing the two would mean
# two models trained at different scales could never be compared against one engine.
SCALES = (1, 2)
CUMULATIVE = 3
CENTER = 2

# Blobs that MOVE, and at different velocities, so the deformation metrics (strain, shear,
# acceleration, direction_stability) have something to measure. A static or uniformly-translating
# field would leave most of the stack constant, and a constant plane cannot detect a change to how
# it is computed. Positions and velocities in pixels; drift is deliberately sub-pixel on some blobs,
# because sub-pixel interpolation is exactly where estimators differ.
_BLOBS = (
    # (y, x, sigma, amplitude, vy, vx)
    (12.0, 12.0, 3.0, 200.0,  0.7, 0.3),
    (12.0, 34.0, 4.0, 255.0, -0.4, 1.1),
    (32.0, 16.0, 2.5, 160.0,  1.3, -0.6),
    (34.0, 34.0, 5.0, 220.0, -0.25, -0.25),
    (24.0, 24.0, 2.0, 120.0,  0.0, 0.0),
)

# Comparison tolerance. Two runs of the same engine on the same machine agree exactly; across
# machines, cv2 and numpy pick different SIMD paths and the last bits move. 1e-3 relative is orders
# of magnitude above that drift and orders of magnitude below any change of recipe — the DIS
# measurement above moves these summaries by tens of percent.
RTOL = 1e-3
# For a summary that is legitimately ~0 (a metric that is constant on the probe), relative
# comparison is meaningless and this carries the test instead.
ATOL = 1e-6


def probe_window():
    """The fixed `[W, H, W]` float32 window the probe measures on.

    Analytic rather than seeded-random: `np.random`'s stream is not part of numpy's compatibility
    promise across versions, so a fixture that depends on it would drift for a reason that has
    nothing to do with the flow engine — which is precisely the false positive this exists to avoid.
    Scaled to 0–255 because that is what `_project_window` hands inference (`PROJECTION_MAX`).
    """
    w, h, x = SHAPE
    yy, xx = np.mgrid[0:h, 0:x].astype(np.float32)
    out = np.zeros(SHAPE, dtype=np.float32)
    for t in range(w):
        plane = out[t]
        for y0, x0, sigma, amp, vy, vx in _BLOBS:
            dy = yy - (y0 + vy * t)
            dx = xx - (x0 + vx * t)
            plane += amp * np.exp(-(dy * dy + dx * dx) / (2.0 * sigma * sigma))
    return out


def _summarise(plane):
    """Two positive scale statistics per metric plane.

    The MEAN is deliberately not one of them: several metrics are signed and near-symmetric, so
    their mean sits at ~0 where a relative comparison carries no information and an absolute one
    fires on noise. `std` and the 99th percentile of |value| are strictly positive for any
    non-constant plane, which makes one tolerance work for the whole stack.
    """
    arr = np.asarray(plane, dtype=np.float64)
    if not arr.size:
        return [0.0, 0.0]
    return [float(arr.std()), float(np.percentile(np.abs(arr), 99.0))]


def fingerprint():
    """`{version, metrics: {name: [std, p99abs]}}` for the engine in this process — `{}` on failure.

    Best-effort, like `_coastal_build`: this is provenance, and a coastal version that cannot be
    probed must not fail a training run that otherwise succeeded. `{}` reads downstream as "no
    fingerprint", which is the same case as a model trained before this field existed.
    """
    try:
        from coastal.flow import flow_metrics_for_frame
        _, metrics = flow_metrics_for_frame(
            probe_window(), CENTER, temporal_scales=list(SCALES),
            cumulative_window=CUMULATIVE)
        return {'version': VERSION,
                'metrics': {str(k): _summarise(v) for k, v in sorted(metrics.items())}}
    except Exception:
        return {}


# How many metric names a warning spells out. A whole-stack difference is 13 of them, and a line
# that long stops being read — the count is the actionable part, the first few say which end changed.
_MAX_NAMED = 3


def _names(names):
    if len(names) <= _MAX_NAMED:
        return ', '.join(names)
    return f'{", ".join(names[:_MAX_NAMED])} and {len(names) - _MAX_NAMED} more'


def _close(a, b):
    return abs(a - b) <= RTOL * max(abs(a), abs(b)) + ATOL


def compare(recorded, current):
    """`None` when the two fingerprints agree — otherwise one line naming the worst disagreement.

    Returns None for anything unmeasurable (either side missing or a probe-version mismatch) rather
    than a warning: the caller distinguishes "checked and agreed" from "could not be checked", and
    inventing a mismatch out of a missing measurement would train the user to ignore this.
    """
    if not recorded or not current:
        return None
    if int(recorded.get('version', -1)) != int(current.get('version', -2)):
        return None

    rec = recorded.get('metrics') or {}
    cur = current.get('metrics') or {}
    if not rec or not cur:
        return None

    gone = sorted(set(rec) - set(cur))
    added = sorted(set(cur) - set(rec))
    if gone or added:
        parts = []
        if gone:
            parts.append(f'no longer produces {_names(gone)}')
        if added:
            parts.append(f'now also produces {_names(added)}')
        return 'the flow engine ' + ' and '.join(parts)

    worst, worst_dev = None, 0.0
    for name in sorted(rec):
        for i, what in enumerate(('spread', '99th percentile')):
            a, b = float(rec[name][i]), float(cur[name][i])
            if _close(a, b):
                continue
            dev = abs(a - b) / max(abs(a), abs(b), ATOL)
            if dev > worst_dev:
                worst, worst_dev = (name, what, a, b), dev
    if worst is None:
        return None

    name, what, a, b = worst
    return (f'the flow engine computes {name} differently — {what} {a:.4g} at training, '
            f'{b:.4g} here ({worst_dev * 100:.0f}% apart)')
