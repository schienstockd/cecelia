"""region_edge_utils — a region's shape from its cells, distances to its boundary, and the boundary lines.

Expected values come from the geometry of the synthetic layouts (a filled square of cells, a square
with a torn-out hole, a slab cut by the frame), not from the code's own output.
"""
import unittest

import numpy as np

from cecelia.utils import region_edge_utils as reu


def _grid_cells(y0, y1, x0, x1, spacing=10.0):
    ys, xs = np.meshgrid(np.arange(y0, y1, spacing), np.arange(x0, x1, spacing), indexing="ij")
    return np.c_[ys.ravel(), xs.ravel()]


class TestRegionMask(unittest.TestCase):
    def test_square_of_cells_reads_distance_to_its_side(self):
        # cells every 10 µm over [200, 600)² — rows and columns at 200 … 590 — in a 1000 µm frame:
        # a cell at (400, 400) is 190 µm from the nearest side (the column at 590); a corner cell ~0
        cells = _grid_cells(200, 600, 200, 600)
        mask, step = reu.region_mask(cells, (1000, 1000), close_um=30)
        inside, d = reu.edge_distances(mask, step, np.array([[400.0, 400.0], [200.0, 200.0]]))
        self.assertTrue(inside.all())
        self.assertAlmostEqual(d[0], 190, delta=2 * step)
        self.assertLess(d[1], 2 * step)

    def test_outside_cell_reads_distance_to_the_region(self):
        cells = _grid_cells(200, 600, 200, 600)
        mask, step = reu.region_mask(cells, (1000, 1000), close_um=30)
        inside, d = reu.edge_distances(mask, step, np.array([[400.0, 800.0]]))
        self.assertFalse(inside[0])
        self.assertAlmostEqual(d[0], 800 - 590, delta=2 * step)   # last column of cells at x=590

    def test_tear_is_filled_unless_asked_not_to(self):
        cells = _grid_cells(200, 600, 200, 600)
        hole = (np.abs(cells[:, 0] - 400) < 60) & (np.abs(cells[:, 1] - 400) < 60)
        cells = cells[~hole]
        probe = np.array([[400.0, 400.0]])
        filled, step = reu.region_mask(cells, (1000, 1000), close_um=20, fill_holes=True)
        kept, _ = reu.region_mask(cells, (1000, 1000), close_um=20, fill_holes=False)
        ins_f, d_f = reu.edge_distances(filled, step, probe)
        ins_k, d_k = reu.edge_distances(kept, step, probe)
        self.assertTrue(ins_f[0])
        self.assertAlmostEqual(d_f[0], 190, delta=2 * step)        # to the outer side, not the tear
        self.assertFalse(ins_k[0])                                 # the tear is outside the shape

    def test_frame_is_not_boundary(self):
        # tissue fills the left half and runs off the frame at x=0, top and bottom: the only visible
        # edge is at x≈500, so a cell at the frame reads ~500 µm, not 0
        cells = _grid_cells(0, 1000, 0, 500)
        mask, step = reu.region_mask(cells, (1000, 1000), close_um=30)
        _, d = reu.edge_distances(mask, step, np.array([[500.0, 0.0], [0.0, 250.0]]))
        self.assertAlmostEqual(d[0], 490, delta=2 * step)
        self.assertAlmostEqual(d[1], 240, delta=2 * step)

    def test_no_visible_boundary_is_nan(self):
        cells = _grid_cells(0, 1000, 0, 1000)
        mask, step = reu.region_mask(cells, (995, 995), close_um=30)
        _, d = reu.edge_distances(mask, step, np.array([[500.0, 500.0]]))
        self.assertTrue(np.isnan(d[0]))

    def test_gap_wider_than_the_radius_splits_the_region(self):
        a = _grid_cells(200, 400, 100, 300)
        b = _grid_cells(200, 400, 500, 700)                        # 210 µm gap
        narrow, _ = reu.region_mask(np.r_[a, b], (600, 800), close_um=50)
        wide, _ = reu.region_mask(np.r_[a, b], (600, 800), close_um=150)
        self.assertEqual(reu.n_pieces(narrow), 2)
        self.assertEqual(reu.n_pieces(wide), 1)

    def test_3d_ball(self):
        zz, yy, xx = np.meshgrid(*(np.arange(100, 300, 10.0),) * 3, indexing="ij")
        cells = np.c_[zz.ravel(), yy.ravel(), xx.ravel()]
        mask, step = reu.region_mask(cells, (400, 400, 400), close_um=30)
        self.assertEqual(mask.ndim, 3)
        _, d = reu.edge_distances(mask, step, np.array([[195.0, 195.0, 195.0]]))
        self.assertAlmostEqual(d[0], 95, delta=2 * step)


class TestSmallPieces(unittest.TestCase):
    def test_specks_are_counted_by_the_cells_in_them(self):
        body = _grid_cells(200, 600, 200, 600)                    # 1600 cells, one piece
        specks = np.array([[900.0, 100.0], [100.0, 900.0]])        # two lone cells far away
        cells = np.r_[body, specks]
        mask, step = reu.region_mask(cells, (1000, 1000), close_um=30)
        self.assertEqual(reu.n_pieces(mask), 3)
        self.assertAlmostEqual(reu.frac_in_small_pieces(mask, step, cells), 2 / len(cells))
        scattered = specks.repeat(1, axis=0)
        m2, _ = reu.region_mask(scattered, (1000, 1000), close_um=30)
        self.assertEqual(reu.frac_in_small_pieces(m2, step, scattered), 1.0)


class TestEdgeContours(unittest.TestCase):
    def test_contour_follows_the_square(self):
        cells = _grid_cells(200, 600, 200, 600)
        mask, step = reu.region_mask(cells, (1000, 1000), close_um=30)
        lines = reu.edge_contours(mask, step)
        self.assertEqual(len(lines), 1)
        pts = lines[0]
        # the outermost cells sit at 200 and 590: the line runs just outside them
        self.assertAlmostEqual(pts[:, 0].min(), 200, delta=2 * step)
        self.assertAlmostEqual(pts[:, 1].max(), 590, delta=2 * step)

    def test_frame_stretch_is_dropped(self):
        cells = _grid_cells(0, 1000, 0, 500)
        mask, step = reu.region_mask(cells, (1000, 1000), close_um=30)
        lines = reu.edge_contours(mask, step)
        pts = np.vstack(lines)
        # only the inner edge at x≈500 is drawn: no point on the left frame
        self.assertGreater(pts[:, 1].min(), 400)


if __name__ == "__main__":
    unittest.main()
