"""RESULT-REVIEW: result-b81b1443

A background clipped at 0 by an offset well below it, with a narrow read-noise spread (N(-5, 3)), falls
from 1 as steeply as raw photon counts do. The histogram cannot tell the two apart, so it must not
claim either: never photon-counted (made-up photon numbers from raw intensities)."""
import unittest

import numpy as np

from cecelia.utils import intensity_utils as iu


class NarrowClippedTest(unittest.TestCase):
    def test_narrow_clipped_background_is_not_called_photon_counted(self):
        rng = np.random.default_rng(7)
        for mu, sd in ((-5, 3), (-4, 2), (-8, 4)):
            with self.subTest(mu=mu, sd=sd):
                bg = np.clip(rng.normal(mu, sd, 300_000).round(), 0, None)
                sig = rng.normal(300, 40, 30_000).round()
                fl = iu.floor_stats(np.bincount(np.concatenate([bg, sig]).astype(np.int64), minlength=4096))
                assert fl['kind'] != 'photon-counted', fl
                assert fl['photons'] is None


if __name__ == '__main__':
    unittest.main()
