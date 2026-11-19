"""Tissue detection when a section runs along the slide edge, and the area of textured tissue.

RESULT-REVIEW: result-9d5fa2c1
RESULT-REVIEW: result-8c6d2b81

A section covering most of the slide edge made the edge's median read as the glass level, which
turned the glass into the one "tissue". And counting only the pixels above the threshold made a
speckled section (bright nuclei, dark gaps) report 40% of its area, so a small core fell under the
size floor. Expected values come from how the slides are built."""
import unittest

import numpy as np

from cecelia.utils import tissue_split_utils as T

PX = 10.0   # µm per pixel


def _split(planes, **kw):
    p = T.SplitParams()
    for k, v in kw.items():
        setattr(p, k, v)
    return T.split_tissue_labels(T.tissue_signal(planes), PX, p)


class TestEdgeTouchingSection(unittest.TestCase):

    def test_section_along_the_edge_is_tissue_not_glass(self):
        rng = np.random.default_rng(1)
        m = np.zeros((300, 400), bool)
        m[:, 0:260] = True                         # 7.8 mm², touching three edges
        m[100:190, 300:390] = True                 # 0.81 mm², on glass
        img = np.where(m, 1000.0, 50.0) + rng.normal(0, 10, m.shape)
        lab, areas = _split(img[None])
        self.assertEqual(len(areas), 2)
        self.assertEqual(lab[100, 280], 0)                         # the glass between them
        self.assertNotEqual(lab[150, 100], 0)
        self.assertNotEqual(lab[150, 340], 0)
        self.assertAlmostEqual(sorted(areas.values())[-1], 7.8, delta=0.3)


class TestTexturedTissueArea(unittest.TestCase):

    def test_speckled_section_reports_its_whole_area(self):
        rng = np.random.default_rng(2)
        m = np.zeros((300, 400), bool)
        m[50:150, 50:150] = True                   # 1 mm²
        r = 1.95e2 / PX                            # a 0.12 mm² core (radius 195 µm)
        yy, xx = np.mgrid[:300, :400]
        m |= (yy - 200) ** 2 + (xx - 300) ** 2 <= r * r
        speck = rng.random(m.shape) < 0.4          # 40% of the tissue's pixels bright
        img = np.where(m & speck, 1000.0, 50.0) + rng.normal(0, 10, m.shape)
        _, areas = _split(img[None], marginUm=0.0)                 # default 0.1 mm² floor
        self.assertEqual(len(areas), 2)                            # the core is kept
        big, small = sorted(areas.values(), reverse=True)
        self.assertAlmostEqual(big, 1.0, delta=0.1)
        self.assertAlmostEqual(small, 0.12, delta=0.03)


if __name__ == '__main__':
    unittest.main()
