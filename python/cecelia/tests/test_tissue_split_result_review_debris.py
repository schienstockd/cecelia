"""Bright debris must not move the glass level onto a section that runs along the slide edge.

RESULT-REVIEW: result-b8645e10

Specks far brighter than tissue (dust, precipitate) pulled the channel's Otsu cut above the tissue,
so the edge's "glass" took in the edge-touching section and the glass merged with both sections into
one tissue. Expected values come from how the slide is built: two sections of 7.8 and 0.81 mm²."""
import unittest

import numpy as np

from cecelia.utils import tissue_split_utils as T

PX = 10.0   # µm per pixel


class TestDebrisAndEdgeSection(unittest.TestCase):

    def test_two_sections_whatever_the_debris(self):
        # up to 0.5% of pixels; past ~1% the specks also set the 99.5th-percentile scale (a known limit)
        for frac in (0.0, 0.001, 0.003, 0.005):
            rng = np.random.default_rng(3)
            m = np.zeros((300, 400), bool)
            m[:, 0:260] = True                     # 7.8 mm², touching three edges
            m[100:190, 300:390] = True             # 0.81 mm², on glass
            img = np.where(m, 1000.0, 50.0) + rng.normal(0, 10, m.shape)
            img[rng.random(m.shape) < frac] = 20000.0
            lab, areas = T.split_tissue_labels(T.tissue_signal(img[None]), PX, T.SplitParams())
            self.assertEqual(len(areas), 2, msg=f"debris {frac}")
            self.assertEqual(lab[100, 280], 0, msg=f"debris {frac}")     # the glass between them


if __name__ == '__main__':
    unittest.main()
