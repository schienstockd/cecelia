"""Drift with STEPS — a stage bump or refocus moves the sample tens of pixels between two frames.

The other drift tests use smooth ramps, and a smooth ramp is the one shape that every part of the
multi-lag pipeline agrees with. A step is the shape that two parts disagreed with, each in its own way:

  * the solver's second-difference prior bent the fit away from the step, and the robust reweighting
    then threw out exactly the pairs that measured it — because they disagreed with the bent fit;
  * the post-solve gaussian on the trajectory smeared what survived over ±3σ frames.

Both were invisible on ramps and both left tens of pixels of misalignment on real two-photon movies
with stage bumps, where phase correlation had measured every jump correctly. These tests hold the
expected positions to the truth the movie was built from — never to the estimator's own output.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import unittest

import numpy as np

import cecelia.utils.correction_utils as cu
from cecelia.tests.test_drift_estimate import _movie


def _stepped_truth(n_t=40, steps=((10, 25.0), (22, -18.0), (31, 30.0)), ramp=0.4):
    """Y trajectory: a slow ramp plus jumps at the given frames. Returns (n_t, 3) Z/Y/X positions."""
    y = ramp * np.arange(n_t, dtype=float)
    for t, size in steps:
        y[t:] += size
    return np.stack([np.zeros(n_t), y, np.zeros(n_t)], axis=1)


def _pairs(truth, max_lag, noise=0.0, corrupt=(), seed=0):
    rng = np.random.default_rng(seed)
    out = []
    for j in range(len(truth)):
        for k in range(1, max_lag + 1):
            i = j - k
            if i < 0:
                continue
            s = truth[j] - truth[i] + rng.normal(0.0, noise, 3) * np.array([0, 1, 1])
            if i in corrupt or j in corrupt:
                s = s + np.array([0.0, 60.0, -60.0])
            out.append((i, j, s))
    return out


class StepSolverTest(unittest.TestCase):
    def test_steps_are_kept_by_the_robust_fit(self):
        """Exact measurements of a stepped trajectory: the fit must be the trajectory, and no pair
        that measured a step may be thrown out as an outlier."""
        truth = _stepped_truth()
        pairs = _pairs(truth, cu.DRIFT_DEFAULT_MAX_LAG)
        pos, w = cu._solve_drift_trajectory(pairs, len(truth), 3)
        self.assertLess(np.abs(pos - truth).max(), 1.0,
                        f'steps lost by the solver: max error {np.abs(pos - truth).max():.1f} px')
        self.assertEqual(int((w < 0.5).sum()), 0, 'correct step measurements were rejected')

    def test_a_corrupted_frame_is_still_outvoted_next_to_steps(self):
        """Releasing the prior at a step must not release it everywhere: a frame whose every pair is
        wrong is still outvoted, and the steps around it are still kept."""
        truth = _stepped_truth()
        pairs = _pairs(truth, cu.DRIFT_DEFAULT_MAX_LAG, noise=0.1, corrupt={16})
        pos, w = cu._solve_drift_trajectory(pairs, len(truth), 3)
        err = np.abs(pos - truth).max(axis=1)
        self.assertGreater(int((w < 0.5).sum()), 0, 'the corrupted frame was not outvoted')
        self.assertLess(np.delete(err, 16).max(), 1.5,
                        f'error away from the bad frame: {np.delete(err, 16).max():.1f} px')


class StepDetectionTest(unittest.TestCase):
    def test_a_step_is_a_move_beyond_the_threshold_not_at_it(self):
        """On a still trajectory the threshold is the floor (median and MAD of the moves are 0).
        "Beyond" is the definition: a move of exactly the floor is not a step, anything more is."""
        floor = 3.0
        at = np.array([[0.0], [0.0], [0.0], [floor], [floor], [floor]])
        beyond = at * (1 + 1e-6)
        self.assertEqual(cu._trajectory_steps(at, floor_px=floor), [0, 6])
        self.assertEqual(cu._trajectory_steps(beyond, floor_px=floor), [0, 3, 6])


class StepSmoothingTest(unittest.TestCase):
    def test_trajectory_smoothing_does_not_smear_a_step(self):
        """The task-default smoothing on a stepped trajectory with sub-pixel noise: every frame stays
        within a pixel of the truth — the step is not spread over its neighbours."""
        rng = np.random.default_rng(4)
        truth = _stepped_truth()[:, 1:]
        noisy = truth + rng.normal(0.0, 0.3, truth.shape)
        noisy[0] = 0.0
        smoothed = cu._smooth_positions(noisy, cu.DRIFT_TASK_SMOOTH_SIGMA)
        self.assertLess(np.abs(smoothed - truth).max(), 1.0,
                        f'smoothing smeared a step: max error {np.abs(smoothed - truth).max():.1f} px')

    def test_smoothing_follows_a_fast_ramp_to_its_end(self):
        """A ramp of 2 px/frame is real motion, not noise: smoothing must not drag the last frames
        toward a plateau (the edge bias of a gaussian with mode='nearest')."""
        t = np.arange(30, dtype=float)
        truth = np.stack([2.0 * t, np.zeros_like(t)], axis=1)
        smoothed = cu._smooth_positions(truth, cu.DRIFT_TASK_SMOOTH_SIGMA)
        self.assertLess(np.abs(smoothed - truth).max(), 0.5,
                        f'ramp bent at the ends: max error {np.abs(smoothed - truth).max():.1f} px')


class PairQualityTest(unittest.TestCase):
    def test_quality_is_the_normalised_correlation_at_the_shift(self):
        """Against the definition, computed in pixel space: Pearson-style correlation of the
        mean-free frames, the second rolled back by the measured shift (circular, like PCC)."""
        import scipy.fft
        import scipy.ndimage
        rng = np.random.default_rng(7)
        a = scipy.ndimage.gaussian_filter(rng.random((32, 48)), 2)
        b = np.roll(a, (5, -3), axis=(0, 1)) + 0.02 * rng.standard_normal(a.shape)
        shift = np.array([-5.0, 3.0])                       # PCC convention: b moved by (5, -3)
        am, bm = a - a.mean(), b - b.mean()
        bb = np.roll(bm, (-5, 3), axis=(0, 1))
        expected = float((am * bb).sum() / np.sqrt((am ** 2).sum() * (bm ** 2).sum()))
        got = cu._pair_quality(scipy.fft.fftn(a), scipy.fft.fftn(b), shift)
        self.assertAlmostEqual(got, expected, places=4)

    def test_a_dead_frame_is_placed_on_the_trend_and_reported(self):
        """A frame of pure noise: its pairs agree with each other, so geometry alone placed it
        wherever its noise peak fell. The gate sets it aside: the prior places it from its
        neighbours, and it is listed as interpolated."""
        arr, du, expected = _movie([[0, 2 * t, 0] for t in range(10)], noise_frames={4})
        est = cu.estimate_drift(arr, 0, du, estimator='multiLag')
        self.assertLess(np.abs(est.positions[4] - expected[4]).max(), 1.0,
                        f'dead frame placed at {est.positions[4]}, trend says {expected[4]}')
        self.assertIn(4, est.interpolated)


class UnmeasuredEndsTest(unittest.TestCase):
    def test_lost_last_frames_after_a_bump_hold_instead_of_flying_off(self):
        """A stage bump, then the last two frames lose signal. Nothing measures them, so the only
        defensible position is the last measured one — the prior alone carries the bump's velocity
        on and puts them 2 × the bump further out."""
        arr, du, expected = _movie([[0, t + (20 if t >= 14 else 0), 0] for t in range(18)],
                                   noise_frames={16, 17})
        est = cu.estimate_drift(arr, 0, du, estimator='multiLag')
        self.assertEqual(est.interpolated, [16, 17])
        np.testing.assert_allclose(est.positions[16:], est.positions[[15, 15]], atol=1e-9)
        self.assertLess(np.abs(est.positions[15] - expected[15]).max(), 1.0)


class PerPlaneProgressTest(unittest.TestCase):
    def test_per_plane_reports_one_tick_per_frame_fetched(self):
        """The per-plane estimate reports progress on the same scale as the volume path: one tick
        per frame read, ending at (n_t, n_t) — the contract the task's single progress bar uses."""
        arr, du, _ = _movie([[0, t, 0] for t in range(6)])
        ticks = []
        cu.estimate_drift(arr, 0, du, estimator='multiLag', per_plane=True,
                          on_progress=lambda n, total: ticks.append((n, total)))
        self.assertEqual(ticks, [(t + 1, 6) for t in range(6)])


class StepEstimateTest(unittest.TestCase):
    def test_estimate_drift_recovers_a_stage_bump_at_task_defaults(self):
        """End to end, through phase correlation, at the settings a GUI run uses: a movie with a
        slow drift and one 20 px bump comes out within a pixel of the truth on every frame."""
        rolls = [[0, t + (20 if t >= 10 else 0), 0] for t in range(24)]
        arr, du, expected = _movie(rolls)
        est = cu.estimate_drift(arr, 0, du, estimator='multiLag',
                                trajectory_smooth_sigma=cu.DRIFT_TASK_SMOOTH_SIGMA)
        err = np.abs(est.positions - expected).max()
        self.assertLess(err, 1.0, f'stage bump not recovered: max error {err:.1f} px')
        self.assertEqual(est.n_rejected, 0, 'pairs that measured the bump were outvoted')


if __name__ == '__main__':
    unittest.main()
