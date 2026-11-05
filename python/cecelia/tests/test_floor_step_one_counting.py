"""RESULT-REVIEW: result-398db41d

A detector in counting mode that writes the photon count itself (step 1: values 0, 1, 2, … in a
Poisson ladder) has no empty run above 0 and a spike at 0, so the spike rule alone reads it as a
clipped background — and offset advice would follow. A Poisson ladder falls steeply from 1
(h[2]/h[1] = λ/2); a clipped background runs on from 1."""
import unittest

import numpy as np

from cecelia.utils import intensity_utils as iu


class StepOneCountingTest(unittest.TestCase):
    def test_raw_photon_counts_are_not_called_clipped(self):
        rng = np.random.default_rng(6)
        for lam in (0.1, 0.3, 1.0):
            with self.subTest(lam=lam):
                fl = iu.floor_stats(np.bincount(rng.poisson(lam, 400_000), minlength=4096))
                # not clipped: offset advice must not follow. Not photon-counted either: a narrow
                # clipped background falls the same way (test_floor_narrow_clipped.py)
                assert fl['kind'] == 'indeterminate', fl


if __name__ == '__main__':
    unittest.main()
