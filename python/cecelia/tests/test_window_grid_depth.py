"""RESULT-REVIEW: result-f4fb0871

Windows of 50 µm on an intravital stack whose cells span 55 µm in Z: the grid must stay one layer of
columns through the stack, as it does at 45 µm. Keeping Z from one window's depth made 55 µm stacks
cubes and 45 µm stacks columns, so images of one set landed on either side by their Z spread and were
clustered together on composition and density with no warning. Z is a grid axis only from two windows'
depth on, and the region run refuses a set whose window grids span different axes."""

import unittest

import numpy as np

from cecelia.utils.spatial_utils import window_grid


def _stack(depth, n=400, seed=0):
    rng = np.random.default_rng(seed)
    return np.column_stack([rng.uniform(0, depth, n), rng.uniform(0, 265, n), rng.uniform(0, 265, n)])


class WindowGridDepthTest(unittest.TestCase):
    def test_a_shallow_stack_is_columns_on_either_side_of_one_window(self):
        for depth in (45.0, 55.0, 99.0):
            g = window_grid(_stack(depth), ["z", "y", "x"], 50.0)
            self.assertEqual(g["gridAxes"], ["y", "x"], f"{depth} µm deep")

    def test_a_stack_two_windows_deep_is_tiled_in_z(self):
        g = window_grid(_stack(110.0), ["z", "y", "x"], 50.0)
        self.assertEqual(g["gridAxes"], ["z", "y", "x"])


if __name__ == "__main__":
    unittest.main()
