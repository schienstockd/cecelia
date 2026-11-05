"""RESULT-REVIEW: result-163e5cfe

An analog channel whose background sits above 0 (pedestal), plus exact zeros from padding (a stitched
mosaic, a drift-corrected canvas, masked borders), also has an empty run above 0 and then a peak. It
must not read as photon-counted: the "step" would be the background level and the photon counts made
up. Photon counting shows a second peak at twice the step; a background does not."""
import unittest

import numpy as np

from cecelia.utils import intensity_utils as iu


def _padded_analog(n_zero):
    rng = np.random.default_rng(0)
    bg = np.clip(rng.normal(100, 10, 300_000).round(), 1, None)
    sig = np.clip(rng.normal(600, 30, 30_000).round(), 1, None)
    v = np.concatenate([np.zeros(n_zero), bg, sig]).astype(np.int64)
    return np.bincount(v, minlength=4096)


class PaddedAnalogFloorTest(unittest.TestCase):
    def test_mostly_padding_is_not_photon_counted(self):
        h = _padded_analog(500_000)                                  # ~60 % zeros
        fl = iu.floor_stats(h)
        assert fl['kind'] != 'photon-counted', fl
        assert fl['photons'] is None
        assert iu.noise_stats(h, floor=fl)['untestable'] != 'photon-counted: zeros are voxels with no photon'

    def test_some_padding_keeps_the_measured_snr(self):
        h = _padded_analog(140_000)                                  # ~30 % zeros: noise is measurable
        fl = iu.floor_stats(h)
        assert fl['kind'] != 'photon-counted', fl
        assert iu.noise_stats(h, floor=fl)['snr'] is not None


if __name__ == '__main__':
    unittest.main()
