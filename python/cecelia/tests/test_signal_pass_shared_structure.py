"""RESULT-REVIEW: result-8666cc42

Two independent channels that both dim with depth must not read as sharing structure: the import
probe's signal pass (`data_path.measure_signal`) correlates WITHIN planes. Pooled raw across planes,
the shared depth trend alone gave r 0.99. The repro the result review ran.
"""
import unittest

import numpy as np
from scipy import ndimage as ndi

from cecelia.utils import data_path, photon_utils


class SharedStructureWithinPlanesTest(unittest.TestCase):
    def test_independent_channels_that_both_dim_with_depth_do_not_correlate(self):
        rng = np.random.default_rng(0)
        n_z, size = 16, 96
        depth = np.exp(-np.arange(n_z) / 4.0)
        a = [ndi.gaussian_filter(rng.random((size, size)), 2).ravel() * depth[z] for z in range(n_z)]
        b = [ndi.gaussian_filter(rng.random((size, size)), 2).ravel() * depth[z] for z in range(n_z)]
        r = photon_utils.shared_structure({0: a, 1: b})[0]['r']
        assert abs(r) < 0.2, r
        # pooled raw, the shared depth trend alone makes them look co-labelled
        pooled = photon_utils.shared_structure({0: np.concatenate(a), 1: np.concatenate(b)})[0]['r']
        assert pooled > 0.9, pooled

    def test_the_signal_pass_correlates_within_planes(self):
        rng = np.random.default_rng(1)
        n_t, n_z, size = 2, 8, 96
        depth = np.exp(-np.arange(n_z) / 3.0)
        arr = np.zeros((n_t, 2, n_z, size, size), dtype=np.uint16)
        for t in range(n_t):
            for c in range(2):
                for z in range(n_z):
                    tex = ndi.gaussian_filter(rng.random((size, size)), 2)
                    arr[t, c, z] = np.round(1000 + 3000 * tex * depth[z])
        chans = [{'saturated': False, 'topValue': int(arr[:, c].max()), 'noise': {'bgMedian': 1000.0}}
                 for c in range(2)]
        out = data_path.measure_signal(arr, {'C': 1, 'T': 0, 'Z': 2}, None, chans)
        assert abs(out['sharedStructure'][0]['r']) < 0.2, out['sharedStructure']


if __name__ == '__main__':
    unittest.main()
