"""RESULT-REVIEW: result-c0dcab47

The smallest smoothing scale the slider offers (σ = 0.5) must smooth. Its window rounded to 3
frames, under the 5 a quadratic fit needs to smooth anything, so it returned the trajectory
unchanged while the tip promised only that 0 is off.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import unittest

import numpy as np

import cecelia.utils.correction_utils as cu


class SmallestSigmaSmoothsTest(unittest.TestCase):
    def test_every_positive_sigma_on_the_slider_changes_a_noisy_trajectory(self):
        rng = np.random.default_rng(0)
        p = np.cumsum(rng.normal(0, 0.3, (40, 2)), axis=0)
        total_variation = lambda x: np.abs(np.diff(x, axis=0)).sum()
        for sigma in (0.5, 1.0, 1.5):
            with self.subTest(sigma=sigma):
                out = cu._smooth_positions(p, sigma)
                self.assertGreater(np.abs(out - p).max(), 0.0)
                self.assertLess(total_variation(out), total_variation(p))


if __name__ == '__main__':
    unittest.main()
