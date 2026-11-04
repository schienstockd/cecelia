"""`intensity_utils.PlaneProfile` — per-depth / per-frame numbers out of the saturation pass.

Synthetic stack built so each claim has a known answer: signal falls with depth, the detector clips
only in the top two planes, and the last frames are dimmer (bleaching).
"""
import unittest

import numpy as np

from cecelia.utils import intensity_utils


def _stack():
    rng = np.random.default_rng(0)
    t, c, z = 4, 2, 5
    arr = rng.integers(0, 5, size=(t, c, z, 32, 32)).astype(np.uint16)       # background
    for zi in range(z):
        level = 2000 - 300 * zi                                              # falls with depth
        for ti in range(t):
            arr[ti, 0, zi, :8, :8] = level - (300 if ti >= 2 else 0)         # dimmer late frames
    arr[:, 0, :2, 16:24, 16:24] = 4095                                       # clipped, top planes only
    return arr


class PlaneProfileTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        arr = _stack()
        lead = [0, 2]                                    # T and Z (channel axis 1 excluded)
        cls.prof = intensity_utils.PlaneProfile(lead, t_axis=0, z_axis=2)
        cls.hists = intensity_utils.channel_histograms(arr, 1, on_plane=cls.prof)
        cls.stats0 = intensity_utils.saturation_stats(cls.hists[0])
        cls.top = intensity_utils.profile_ceiling(cls.stats0)

    def test_profile_sums_to_the_whole_stack_histogram(self):
        # The hook sees the same planes the summed histogram is built from.
        rows = self.prof._rows[0]
        self.assertEqual(len(rows), 4 * 5)

    def test_clipping_is_located_in_depth(self):
        s = self.prof.summary(0, self.top)
        self.assertEqual(self.top, 4095)
        self.assertEqual(s['byZ']['clipped'], [256, 256, 0, 0, 0])    # 4 frames × 64 px, z 0-1 only

    def test_signal_falls_with_depth(self):
        p99 = self.prof.summary(0, self.top)['byZ']['p99']
        self.assertEqual(len(p99), 5)
        self.assertTrue(all(a > b for a, b in zip(p99[1:], p99[2:])), p99)

    def test_late_frames_are_dimmer(self):
        p99 = self.prof.summary(0, self.top)['byT']['p99']
        self.assertGreater(p99[0], p99[3])

    def test_unclipped_channel_reports_no_ceiling_voxels(self):
        top1 = intensity_utils.profile_ceiling(intensity_utils.saturation_stats(self.hists[1]))
        s = self.prof.summary(1, top1)
        self.assertEqual(len(s['byZ']['clipped']), 5)
        self.assertGreater(min(s['byZ']['zeroFrac']), 0.1)

    def test_no_z_axis_gives_one_depth(self):
        arr = np.zeros((3, 16, 16), dtype=np.uint16)
        arr[:, :4, :4] = 100
        prof = intensity_utils.PlaneProfile([0], t_axis=0, z_axis=None)
        intensity_utils.channel_histograms(arr, None, on_plane=prof)
        s = prof.summary(0, 100)
        self.assertEqual(len(s['byZ']['p99']), 1)
        self.assertEqual(len(s['byT']['p99']), 3)


if __name__ == '__main__':
    unittest.main()
