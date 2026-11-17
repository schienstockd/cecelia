"""RESULT-REVIEW: result-1ec265db

A stage bump bigger than about half the frame. The frames before and after it share little image,
so every pair spanning the bump scores a low correlation quality — and gating them all left nothing
linking the two halves: the prior joined them and the bump was erased with no frame reported.

Beyond half the frame phase correlation cannot measure the bump at all — circular correlation
resolves shifts only within ±N/2, and a jump of s reads as s − N (37 px on a 64 px frame reads as
−27). So the guarantee is not recovery but disclosure: the frame after the bump is reported as
predicted. Below half the frame the bump is recovered. Expected positions are the offsets the movie
was cropped at, not the estimator's output.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import unittest

import numpy as np
import ome_types
import scipy.ndimage

import cecelia.utils.correction_utils as cu
from cecelia.utils.dim_utils import DimUtils
from cecelia.tests.test_drift_estimate import _OME


def _movie(offs, size=64, z=4, seed=0):
    """Frames cropped from one larger texture at Y offset ``offs[t]`` — no wraparound, so a big
    offset really does cut the overlap."""
    rng = np.random.default_rng(seed)
    big = scipy.ndimage.gaussian_filter(rng.random((z, 400, 400)) * 200, (1, 2, 2))
    frames = [big[:, 150 + o:150 + o + size, 150:150 + size] for o in offs]
    n = len(offs)
    arr = np.stack(frames).reshape(n, 1, z, size, size).astype(np.uint8)
    du = DimUtils(ome_types.from_xml(_OME.format(t=n, z=z, y=size, x=size)), use_channel_axis=True)
    du.calc_image_dimensions(arr.shape)
    return arr, du


def _bumped(bump):
    return [t + (bump if t >= 12 else 0) for t in range(24)]


class BumpBiggerThanHalfTheFrameTest(unittest.TestCase):
    def test_a_bump_under_half_the_frame_is_recovered(self):
        offs = _bumped(20)
        arr, du = _movie(offs)
        est = cu.estimate_drift(arr, 0, du, estimator='multiLag',
                                trajectory_smooth_sigma=cu.DRIFT_TASK_SMOOTH_SIGMA)
        jump = est.positions[12, 1] - est.positions[11, 1]
        self.assertAlmostEqual(jump, offs[12] - offs[11], delta=1.0)
        self.assertEqual(est.interpolated, [])

    def test_a_bump_over_half_the_frame_is_reported_not_erased_silently(self):
        for bump in (36, 40, 48):
            with self.subTest(bump=bump):
                arr, du = _movie(_bumped(bump))
                est = cu.estimate_drift(arr, 0, du, estimator='multiLag',
                                        trajectory_smooth_sigma=cu.DRIFT_TASK_SMOOTH_SIGMA)
                self.assertIn(12, est.interpolated)

    def test_per_plane_reports_it_too(self):
        arr, du = _movie(_bumped(36))
        est = cu.estimate_drift(arr, 0, du, estimator='multiLag', per_plane=True,
                                trajectory_smooth_sigma=cu.DRIFT_TASK_SMOOTH_SIGMA)
        for z in range(4):
            self.assertIn(12, est.interpolated.get(z, []))


if __name__ == '__main__':
    unittest.main()
