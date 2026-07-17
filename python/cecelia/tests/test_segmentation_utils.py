"""Unit tests for the pure segment-count helper (cecelia.utils.segmentation_utils.count_labels).

Pure/headless — no zarr, no cellpose. Pins that the objective cell count = distinct non-zero label
IDs (background 0 ignored, repeats collapsed, non-contiguous IDs handled), which is what the QC
sidecar banks for segmentation."""
import unittest

import numpy as np

from cecelia.utils.segmentation_utils import count_labels


class TestCountLabels(unittest.TestCase):
    def test_counts_distinct_nonzero(self):
        arr = np.array([[0, 1, 1], [2, 2, 0], [3, 0, 3]], dtype=np.uint32)
        self.assertEqual(count_labels(arr), 3)          # ids 1,2,3

    def test_background_only_is_zero(self):
        self.assertEqual(count_labels(np.zeros((4, 4), dtype=np.uint32)), 0)

    def test_non_contiguous_ids(self):
        # ids need not be 1..N (post-processing removes objects → gaps); count the distinct ones
        arr = np.array([10, 10, 500, 0, 7], dtype=np.uint32)
        self.assertEqual(count_labels(arr), 3)          # ids 7,10,500

    def test_3d_timecourse_like(self):
        # globally-unique ids across a stack → total object instances
        arr = np.zeros((2, 3, 3), dtype=np.uint32)
        arr[0, 0, 0] = 1
        arr[0, 1, 1] = 2
        arr[1, 2, 2] = 3
        self.assertEqual(count_labels(arr), 3)


if __name__ == '__main__':
    unittest.main()
