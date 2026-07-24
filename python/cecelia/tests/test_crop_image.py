"""Unit tests for the pure crop slice-tuple helper (cecelia.utils.slice_utils).

Pure/headless — no zarr, no napari. Pins that channels (and any un-cropped or absent axis) are kept
full, and that the half-open pixel bounds land on the right array axes."""
import unittest

from cecelia.utils.slice_utils import crop_slice_tuple


class TestCropSliceTuple(unittest.TestCase):
    # a T,C,Z,Y,X image
    AXIS = {'X': 4, 'Y': 3, 'Z': 2, 'T': 0}

    def test_slices_xyz_keeps_channel_full(self):
        bounds = {'X': (10, 20), 'Y': (5, 15), 'Z': (1, 3), 'T': (-1, -1)}
        s = crop_slice_tuple(5, self.AXIS, bounds)
        self.assertEqual(s, (slice(None), slice(None), slice(1, 3), slice(5, 15), slice(10, 20)))

    def test_time_trim(self):
        bounds = {'X': (0, 8), 'Y': (0, 8), 'Z': (-1, -1), 'T': (2, 6)}
        s = crop_slice_tuple(5, self.AXIS, bounds)
        self.assertEqual(s[0], slice(2, 6))          # T sliced
        self.assertEqual(s[2], slice(None))          # Z left full (-1)

    def test_absent_axis_left_full(self):
        # a Y,X image (no Z/T/C): only X/Y have indices
        axis = {'X': 1, 'Y': 0, 'Z': None, 'T': None}
        bounds = {'X': (2, 6), 'Y': (1, 4), 'Z': (0, 3), 'T': (0, 3)}
        s = crop_slice_tuple(2, axis, bounds)
        self.assertEqual(s, (slice(1, 4), slice(2, 6)))

    def test_degenerate_bounds_skipped(self):
        # lo<0, or hi<=lo → keep that axis full
        bounds = {'X': (-1, 10), 'Y': (5, 5), 'Z': (8, 2), 'T': (-1, -1)}
        s = crop_slice_tuple(5, self.AXIS, bounds)
        self.assertEqual(s, (slice(None),) * 5)


if __name__ == '__main__':
    unittest.main()
