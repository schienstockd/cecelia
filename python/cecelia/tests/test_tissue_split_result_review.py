"""Tissue detection must not read glass as tissue, and must report a tissue's own area.

RESULT-REVIEW: result-45af59fe
RESULT-REVIEW: result-c42365e0

A channel with no tissue contrast (a blank cycle, a camera offset) or a bright fluorescence background
used to lift the glass over the threshold, so the
whole slide came out as one "tissue" and the run wrote it unmasked. And the reported area was measured
on the smoothed outline, 14% large for a 0.6 mm core. Expected values come from how the slides are
built: two sections → 2 tissues; a disc of radius r → π r²."""
import unittest

import numpy as np

from cecelia.utils import tissue_split_utils as T

PX = 10.0   # µm per pixel


def _two_sections(rng):
    """300 × 400 px slide (3 × 4 mm): two 1 mm² sections well apart, glass around them."""
    m = np.zeros((300, 400), bool)
    m[50:150, 50:150] = True
    m[150:250, 250:350] = True
    return m


def _count(planes):
    _, areas = T.split_tissue_labels(T.tissue_signal(planes), PX, T.SplitParams())
    return len(areas)


class TestGlassIsNotTissue(unittest.TestCase):

    def setUp(self):
        self.rng = np.random.default_rng(0)
        self.m = _two_sections(self.rng)

    def _marker(self, tissue=1000.0, glass=50.0, sd=10.0):
        return np.where(self.m, tissue, glass) + self.rng.normal(0, sd, self.m.shape)

    def test_markers_alone_find_both(self):
        self.assertEqual(_count(np.stack([self._marker() for _ in range(6)])), 2)

    def test_a_flat_channel_does_not_merge_the_slide(self):
        # glass at 30% of the tissue in the markers; the flat channel scaled to its own top reads ~1 on
        # glass, which lifted the averaged glass over half of Otsu
        markers = [self._marker(tissue=1000.0, glass=300.0) for _ in range(6)]
        flat = 500.0 + self.rng.normal(0, 10, self.m.shape)       # offset, no tissue in it
        self.assertEqual(_count(np.stack(markers)), 2)
        self.assertEqual(_count(np.stack(markers + [flat])), 2)

    def test_bright_fluorescence_background(self):
        planes = np.stack([self._marker(tissue=1000.0, glass=400.0) for _ in range(3)])
        self.assertEqual(_count(planes), 2)

    def test_skipped_scanner_tiles_are_not_glass(self):
        # a tile the scanner skipped (exact zeros, darker than any glass) must not set the glass level
        planes = np.stack([self._marker(tissue=1000.0, glass=400.0) for _ in range(3)])
        planes[:, 0:60, 300:400] = 0
        self.assertEqual(_count(planes), 2)


class TestAreaIsTheTissue(unittest.TestCase):

    def test_disc_area_is_pi_r_squared(self):
        for r_um in (300.0, 600.0):
            r = r_um / PX
            yy, xx = np.mgrid[:200, :200]
            img = ((yy - 100) ** 2 + (xx - 100) ** 2 <= r * r).astype(np.float32)
            _, areas = T.split_tissue_labels(T.tissue_signal(img[None]), PX,
                                             T.SplitParams(minAreaMm2=0.01, marginUm=0.0))
            (a,) = areas.values()
            true = np.pi * (r_um / 1000.0) ** 2
            self.assertAlmostEqual(a / true, 1.0, delta=0.03, msg=f"r={r_um} µm")


if __name__ == '__main__':
    unittest.main()
