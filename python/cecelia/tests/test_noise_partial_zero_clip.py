"""`noise_stats` on a background partly clipped at 0 (under the 50 % gate). Leaving the zeros out of
the background would drop its clipped lower tail, so the median would read high, the spread low, and
the SNR inflated (39 against a true 29) with no `untestable` flag."""
import unittest

import numpy as np

from cecelia.utils import intensity_utils as iu


def _channel(bg_mean, bg_sd=10.0, sig=300.0, n_bg=400_000, n_sig=40_000, seed=0):
    rng = np.random.default_rng(seed)
    bg = np.clip(rng.normal(bg_mean, bg_sd, n_bg).round(), 0, None)
    s = np.clip(rng.normal(sig, 30, n_sig).round(), 0, None)
    return np.bincount(np.concatenate([bg, s]).astype(np.int64), minlength=4096)


class NoisePartialZeroClipTest(unittest.TestCase):
    def test_spread_and_snr_hold_while_part_of_the_background_is_clipped(self):
        for mean in (15, 8, 5):                       # ~7 %, ~21 %, ~31 % of the background at 0
            n = iu.noise_stats(_channel(mean))
            with self.subTest(bg_mean=mean):
                self.assertIsNone(n['untestable'])
                self.assertLess(abs(n['bgSpread'] - 10.0), 1.5)
                true_snr = (300.0 - mean) / 10.0
                self.assertLess(abs(n['snr'] - true_snr) / true_snr, 0.12)


if __name__ == '__main__':
    unittest.main()
