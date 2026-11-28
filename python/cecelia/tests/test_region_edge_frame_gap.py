"""A region near — but not touching — the image frame keeps its own edge there.

RESULT-REVIEW: result-b188e6bf — the closing counted the frame as region (`border_value=1`), so empty
glass narrower than the closing radius between the tissue and the frame was filled: that real edge
vanished, its cells read the distance to the far side of the tissue, and glass read as inside.
Expected values come from the layout (cells every 10 µm over [60, 940)² or from x=40, 1000 µm frame,
75 µm radius), not from the code's output.
"""
import unittest

import numpy as np

from cecelia.utils import region_edge_utils as reu


def _grid_cells(y0, y1, x0, x1, spacing=10.0):
    ys, xs = np.meshgrid(np.arange(y0, y1, spacing), np.arange(x0, x1, spacing), indexing="ij")
    return np.c_[ys.ravel(), xs.ravel()]


class TestFrameGap(unittest.TestCase):
    def test_tissue_inset_from_every_side_keeps_its_edges(self):
        cells = _grid_cells(60, 940, 60, 940)               # last row/column at 930
        mask, step = reu.region_mask(cells, (1000, 1000), close_um=75)
        inside, d = reu.edge_distances(mask, step, np.array([[500.0, 60.0], [500.0, 500.0], [500.0, 10.0]]))
        self.assertLess(d[0], 2 * step)                     # a cell on the tissue edge: ~0
        self.assertAlmostEqual(d[1], 430, delta=2 * step)   # the centre: to the nearest side (930 − 500)
        self.assertFalse(inside[2])                         # the glass strip is outside

    def test_glass_strip_to_one_side_stays_outside(self):
        cells = _grid_cells(0, 1000, 40, 1000)              # touches three sides, 40 µm short of the left
        mask, step = reu.region_mask(cells, (1000, 1000), close_um=75)
        inside, d = reu.edge_distances(mask, step, np.array([[500.0, 40.0], [500.0, 0.0]]))
        self.assertLess(d[0], 2 * step)                     # the tissue's left edge is an edge
        self.assertFalse(inside[1])                         # the glass at x=0 is not tissue

    def test_tissue_cut_by_the_frame_is_not_eroded_there(self):
        cells = _grid_cells(0, 1000, 0, 500)                # runs off the top, bottom and left
        mask, step = reu.region_mask(cells, (1000, 1000), close_um=75)
        self.assertTrue(mask[:, 0].all() and mask[0, : int(480 / step)].all())
        _, d = reu.edge_distances(mask, step, np.array([[500.0, 0.0]]))
        self.assertAlmostEqual(d[0], 490, delta=2 * step)   # the frame is not boundary: to x=490


if __name__ == "__main__":
    unittest.main()
