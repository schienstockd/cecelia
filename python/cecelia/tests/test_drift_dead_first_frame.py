"""RESULT-REVIEW: result-6bca44e0

Dead frames at the START of a movie. The frames after them are measured exactly, so only the dead
frames themselves may be reported as predicted — the boundary between the dead run and the first
measured frame is the dead run's own gap, not a second, unbridged one. The end of the movie already
behaved: a dead last frame reports only itself.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import unittest

import cecelia.utils.correction_utils as cu
from cecelia.tests.test_drift_estimate import _movie


class DeadFirstFramesTest(unittest.TestCase):
    def test_only_the_dead_frames_are_reported(self):
        for dead in ({0}, {0, 1}, {9}):
            with self.subTest(dead=sorted(dead)):
                arr, du, _ = _movie([[0, 2 * t, 0] for t in range(10)], noise_frames=dead)
                est = cu.estimate_drift(arr, 0, du, estimator='multiLag')
                self.assertEqual(est.interpolated, sorted(dead))


if __name__ == '__main__':
    unittest.main()
