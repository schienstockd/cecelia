"""RESULT-REVIEW: result-67ddc29e

A discriminator below half the per-photon step (threshold 8 or 10, step 32, jitter 9·√k): searching the
one-photon peak only in [firstValue, 2·firstValue) stops on its rising flank, and the real peak is then
taken for the two-photon one — step ~15–19 instead of 32, photon counts inflated ~2×. The first peak is
the first local maximum above the cut, wherever it sits."""

import unittest

import numpy as np

from cecelia.utils import intensity_utils as iu


class LowDiscriminatorTest(unittest.TestCase):
    def test_step_is_the_one_photon_peak(self):
        rng = np.random.default_rng(8)
        for thr in (8, 10):
            with self.subTest(threshold=thr):
                k = rng.poisson(0.3, 400_000)
                v = np.round(k * 32 + rng.normal(0, 9, k.size) * np.sqrt(k)).astype(np.int64)
                v[v < thr] = 0
                fl = iu.floor_stats(np.bincount(v, minlength=4096))
                # this much jitter merges the two-photon peak into the first one's tail, so the comb
                # is not visible and the honest call is indeterminate; what must never happen is a
                # claimed step that is the flank of the first peak
                if fl['kind'] == 'photon-counted':
                    assert abs(fl['photonStep'] - 32) <= 3, fl
                    assert 0.85 <= fl['photons']['p50'] <= 1.15, fl
                else:
                    assert fl['kind'] == 'indeterminate' and fl['photons'] is None, fl

    def test_a_resolved_comb_with_a_low_discriminator_gets_the_right_step(self):
        rng = np.random.default_rng(9)
        k = rng.poisson(0.6, 400_000)
        # cut at 15, under half the step: the old [firstValue, 2·firstValue) window ended at 29
        v = np.round(k * 32 + rng.normal(0, 6, k.size) * np.sqrt(k)).astype(np.int64)
        v[v < 15] = 0
        fl = iu.floor_stats(np.bincount(v, minlength=4096))
        assert fl['kind'] == 'photon-counted' and abs(fl['photonStep'] - 32) <= 2, fl


if __name__ == '__main__':
    unittest.main()
