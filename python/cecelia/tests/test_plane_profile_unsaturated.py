"""RESULT-REVIEW: result-42f64b0f

A channel whose brightest voxels are ordinary signal (no detector pile-up, `saturated=False`) had every
voxel at its maximum counted as "clipped" in the per-depth profile, while the same channel reported 0 %
clipped. The ceiling only means clipping when the saturation check says the channel clipped.
"""
import unittest

import numpy as np

from cecelia.utils import intensity_utils


class UnsaturatedProfileTest(unittest.TestCase):
    def test_no_ceiling_voxels_when_the_channel_did_not_clip(self):
        rng = np.random.default_rng(1)
        arr = rng.integers(0, 2000, size=(2, 3, 32, 32)).astype(np.uint16)   # T, Z: a smooth tail
        prof = intensity_utils.PlaneProfile([0, 1], t_axis=0, z_axis=1)
        hist = intensity_utils.channel_histograms(arr, None, on_plane=prof)[0]
        stats = intensity_utils.saturation_stats(hist)
        self.assertFalse(stats['saturated'])
        s = prof.summary(0, intensity_utils.profile_ceiling(stats))
        self.assertEqual(s['byZ']['clipped'], [0, 0, 0])
        self.assertEqual(s['byT']['clipped'], [0, 0])


if __name__ == '__main__':
    unittest.main()
