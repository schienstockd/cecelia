"""RESULT-REVIEW: result-949824b9

Two channels with unrelated objects under the same vignette (brightness falling toward the field's
edge, shared through the optics) must not read as sharing structure. Centring each plane left the
in-plane gradient in, which alone gave r 0.97; the signal pass now correlates `structure_plane`
(cell-scale band-pass). The repro the result review ran, plus a control that real shared cells still
correlate.
"""
import unittest

import numpy as np
from scipy import ndimage as ndi

from cecelia.utils import photon_utils


def _vignette(size):
    yy, xx = np.mgrid[:size, :size]
    r2 = ((yy - size / 2) ** 2 + (xx - size / 2) ** 2) / (size / 2) ** 2
    return np.exp(-1.5 * r2)


def _cells(rng, size, n=60, sigma=4.0):
    im = np.zeros((size, size))
    im[rng.integers(0, size, n), rng.integers(0, size, n)] = rng.uniform(50, 200, n)
    return ndi.gaussian_filter(im, sigma)


class VignetteIsNotSharedStructureTest(unittest.TestCase):
    def test_unrelated_cells_under_one_vignette_do_not_correlate(self):
        rng = np.random.default_rng(0)
        size, v = 256, _vignette(256)
        planes = [(1000 * v * (1 + _cells(rng, size)), 1000 * v * (1 + _cells(rng, size))) for _ in range(4)]
        band = photon_utils.shared_structure({
            0: [photon_utils.structure_plane(a).ravel() for a, _ in planes],
            1: [photon_utils.structure_plane(b).ravel() for _, b in planes]})[0]['r']
        assert abs(band) < 0.2, band
        # smoothed only, the shared vignette alone reads as shared structure
        raw = photon_utils.shared_structure({
            0: [photon_utils.smooth_plane(a).ravel() for a, _ in planes],
            1: [photon_utils.smooth_plane(b).ravel() for _, b in planes]})[0]['r']
        assert raw > 0.5, raw

    def test_the_same_cells_in_two_channels_still_correlate(self):
        rng = np.random.default_rng(1)
        size, v = 256, _vignette(256)
        cells = [_cells(rng, size) for _ in range(4)]
        r = photon_utils.shared_structure({
            0: [photon_utils.structure_plane(1000 * v * (1 + c)).ravel() for c in cells],
            1: [photon_utils.structure_plane(300 * v * (1 + c)).ravel() for c in cells]})[0]['r']
        assert r > 0.9, r


if __name__ == '__main__':
    unittest.main()
