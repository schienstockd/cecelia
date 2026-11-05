"""RESULT-REVIEW: result-d56e38cb

A background clipped at 0 whose mean sits a little above 0 (30–40 % zeros) is still rising at 1, so
"counts highest at 1" misses it. What every clipped background has: everything at or below 0 piles
into the 0 bin, a spike far above the values next to it, with no gap after it."""
import unittest

import numpy as np

from cecelia.utils import intensity_utils as iu


def _clipped(mu, n=300_000, seed=4):
    rng = np.random.default_rng(seed)
    bg = np.clip(rng.normal(mu, 10, n).round(), 0, None)
    sig = np.clip(rng.normal(200, 30, n // 10).round(), 0, None)
    return np.bincount(np.concatenate([bg, sig]).astype(np.int64), minlength=4096)


class ClippedPositiveMeanTest(unittest.TestCase):
    def test_clipped_backgrounds_with_a_mean_above_zero(self):
        for mu in (-5, 0, 2, 5, 10):
            with self.subTest(mu=mu):
                assert iu.floor_stats(_clipped(mu))['kind'] == 'clipped'

    def test_an_unclipped_background_with_a_few_zeros_is_not_called_clipped(self):
        # a continuous distribution that merely reaches 0: the 0 bin is like its neighbours
        rng = np.random.default_rng(5)
        v = np.round(rng.uniform(0, 400, 400_000)).astype(np.int64)
        assert iu.floor_stats(np.bincount(v, minlength=4096))['kind'] != 'clipped'


if __name__ == '__main__':
    unittest.main()
