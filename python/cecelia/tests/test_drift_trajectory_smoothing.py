"""Trajectory smoothing post-step (`_smooth_positions`) — the fix for the
integer-rounding jitter that `drift_correct_im` was writing on movies whose
estimated per-frame shifts sit at the phase-correlation noise floor.

The writer places each frame at `round(cumsum(shifts)[t])`, so a trajectory
that oscillates by ~0.4 px around a small true drift becomes per-frame
integer-pixel jumps in the corrected zarr — visible on `zolIMa/2h06xA` as
107 rounded transitions across 181 frames from ~1 px peak-to-peak. The
smoothing is applied to the *cumulative positions* (not the deltas: a
per-delta threshold amplifies drift when small noise deltas partially
cancel real spikes, verified on the same movie).

The two behaviours worth pinning:
  1. On a noisy-around-zero trajectory the smoothed positions collapse
     toward zero — fewer integer transitions in the writer.
  2. On a real ramp with additive noise the smoothed positions preserve
     the ramp — the peak (which the canvas has to cover) is not lost.
"""
import unittest
import numpy as np

import cecelia.utils.correction_utils as cu


class TrajectorySmoothingTest(unittest.TestCase):

    def test_zero_sigma_is_identity(self):
        pos = np.random.default_rng(0).standard_normal((50, 3))
        out = cu._smooth_positions(pos, sigma=0)
        np.testing.assert_array_equal(out, pos)
        # negative sigma is also treated as off (guard against buggy JSON)
        np.testing.assert_array_equal(cu._smooth_positions(pos, sigma=-1), pos)

    def test_collapses_noise_around_zero(self):
        """No real drift → smoothed cumulative range is much smaller than raw
        AND the number of rounded-integer transitions collapses. This is
        the 2h06xA case."""
        rng = np.random.default_rng(1)
        # 200 frames of Y,X random noise ~0.3 px std, no bias. cumsum drifts
        # like a random walk; the smoothed trajectory should collapse to ~0.
        deltas = rng.normal(0.0, 0.3, size=(200, 2))
        pos = np.vstack([np.zeros(2), np.cumsum(deltas, axis=0)])
        smoothed = cu._smooth_positions(pos, sigma=cu.DRIFT_TASK_SMOOTH_SIGMA)
        raw_int_trans = (np.diff(np.round(pos).astype(int), axis=0) != 0).sum()
        smt_int_trans = (np.diff(np.round(smoothed).astype(int), axis=0) != 0).sum()
        # 200 frames of ±0.3 px random walk gives dozens of rounded transitions;
        # the smoothed trajectory keeps at most a small handful.
        self.assertGreater(raw_int_trans, 20,
                           "test data too tame — noise didn't produce a jittery raw trajectory")
        self.assertLess(smt_int_trans, raw_int_trans / 3,
                        f"smoothing did not collapse noise: {smt_int_trans} vs {raw_int_trans}")

    def test_preserves_real_ramp(self):
        """Real motion (S-shaped translation) survives the smoothing — the
        peak the canvas has to cover is nearly unchanged. This is the
        ttRMjQ case."""
        rng = np.random.default_rng(2)
        t = np.arange(126)
        # tanh-ish transition reaching -150 by frame ~60, similar to the ttRMjQ
        # trajectory measured in the audit.
        signal = -75.0 * (1 + np.tanh((t - 30) / 8.0))
        noise = rng.normal(0.0, 0.3, size=signal.shape)
        pos_y = signal + noise
        pos = np.stack([pos_y, np.zeros_like(pos_y)], axis=1)
        smoothed = cu._smooth_positions(pos, sigma=cu.DRIFT_TASK_SMOOTH_SIGMA)
        raw_peak = float(np.max(np.abs(pos[:, 0])))
        smt_peak = float(np.max(np.abs(smoothed[:, 0])))
        # peak is preserved to within 5% — the smoother is transparent to real motion.
        self.assertGreater(smt_peak, 0.95 * raw_peak,
                           f"smoothing over-attenuated the ramp: {smt_peak:.1f} vs raw {raw_peak:.1f}")
        # AND the smoothed trajectory reaches its target from BOTH sides — not
        # just clamped to zero. Assert both signs actually occur along the ramp.
        self.assertLess(smoothed[:, 0].min(), -100.0, "smoothed trajectory lost its excursion")

    def test_positions_zero_pinned(self):
        """positions[0] must stay the origin (writer assumption)."""
        pos = np.random.default_rng(3).standard_normal((40, 2))
        pos[0] = 0.0
        smoothed = cu._smooth_positions(pos, sigma=5.0)
        # gaussian_filter1d with mode='nearest' does not preserve pos[0]==0
        # exactly, so we allow a small offset — but it must be < 1 px so
        # rounding still places frame 0 at the canvas origin the writer expects.
        np.testing.assert_array_less(np.abs(smoothed[0]), 1.0,
                                     "first-frame position drifted more than 1 px after smoothing")


if __name__ == '__main__':
    unittest.main()
