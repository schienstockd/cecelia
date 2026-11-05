"""RESULT-REVIEW: result-d2d67209

Padding zeros plus an analog background above 0 (N(100, 10)) and signal at twice it (N(200, 20)) have
an empty run, a peak and a second peak at twice its value — the photon comb, by coincidence. What
photon counting has and this does not: the empty run ends at a CUT (the discriminator), so the first
occupied value already holds a few % of the first peak; an analog tail fades out to single voxels."""
import unittest

import numpy as np

from cecelia.utils import intensity_utils as iu


def _two_populations(n_zero):
    rng = np.random.default_rng(3)
    bg = np.clip(rng.normal(100, 10, 300_000).round(), 1, None)
    sig = np.clip(rng.normal(200, 20, 100_000).round(), 1, None)
    v = np.concatenate([np.zeros(n_zero), bg, sig]).astype(np.int64)
    return np.bincount(v, minlength=4096)


class AnalogTwoPopulationsFloorTest(unittest.TestCase):
    def test_mostly_padding(self):
        fl = iu.floor_stats(_two_populations(550_000))                 # ~58 % zeros
        assert fl['kind'] != 'photon-counted', fl

    def test_some_padding_keeps_the_measured_snr(self):
        h = _two_populations(155_000)                                  # ~28 % zeros
        fl = iu.floor_stats(h)
        assert fl['kind'] != 'photon-counted', fl
        assert iu.noise_stats(h, floor=fl)['snr'] is not None


if __name__ == '__main__':
    unittest.main()
