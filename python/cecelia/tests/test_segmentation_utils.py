"""Unit tests for the pure segment-count helper (cecelia.utils.segmentation_utils.count_labels).

Pure/headless — no zarr, no cellpose. Pins that the objective cell count = distinct non-zero label
IDs (background 0 ignored, repeats collapsed, non-contiguous IDs handled), which is what the QC
sidecar banks for segmentation."""
import unittest

import dask.array as da
import numpy as np


class SubsampleTimeTest(unittest.TestCase):
    """`_subsample_time` — the preview's frame budget for the whole-image normalisation statistic.

    The statistic costs ~30 s on a single-level timelapse, which is the entire latency of a preview
    whose inference is 0.35 s. Striding time is safe in a specific way worth pinning: it trades only
    TEMPORAL coverage, so it is an **identity** on a single-timepoint image — a large tiled mosaic,
    which is exactly the case that most needs a global window (per-tile normalisation there makes the
    segmentation visibly patchy).
    """

    class _Seg:
        def __init__(self, t_idx):
            self.dim_utils = type("D", (), {"dim_idx": lambda _s, ax: t_idx if ax == "T" else None})()
        _subsample_time = None      # bound below

    def _seg(self, t_idx):
        from cecelia.utils.segmentation_utils import SegmentationUtils
        s = self._Seg(t_idx)
        s._subsample_time = SegmentationUtils._subsample_time.__get__(s)
        return s

    def test_no_budget_is_exact(self):
        arr = da.zeros((201, 2, 8, 8), chunks=(1, 1, 8, 8))
        for budget in (None, 0):
            self.assertEqual(self._seg(0)._subsample_time(arr, budget).shape[0], 201)

    def test_strides_down_to_at_most_the_budget(self):
        arr = da.zeros((201, 2, 8, 8), chunks=(1, 1, 8, 8))
        for budget in (50, 20, 10, 5, 2):
            got = self._seg(0)._subsample_time(arr, budget).shape[0]
            self.assertLessEqual(got, budget, f"budget {budget} exceeded ({got} frames)")
            self.assertGreater(got, 0)

    def test_identity_when_there_are_fewer_frames_than_the_budget(self):
        arr = da.zeros((5, 2, 8, 8), chunks=(1, 1, 8, 8))
        self.assertEqual(self._seg(0)._subsample_time(arr, 20).shape[0], 5)

    def test_identity_for_a_single_timepoint_mosaic(self):
        # the large-tiled-image case: nothing is given up, so a global window stays global
        arr = da.zeros((1, 2, 4096, 4096), chunks=(1, 1, 512, 512))
        out = self._seg(0)._subsample_time(arr, 20)
        self.assertEqual(out.shape, arr.shape)

    def test_identity_when_the_image_has_no_time_axis(self):
        arr = da.zeros((2, 64, 64), chunks=(1, 64, 64))
        self.assertEqual(self._seg(None)._subsample_time(arr, 20).shape, arr.shape)

    def test_spatial_coverage_is_never_reduced(self):
        # every frame it keeps is kept WHOLE — that is what makes this safe for tiled images
        arr = da.from_array(np.arange(201 * 4 * 6, dtype=np.uint16).reshape(201, 1, 4, 6))
        out = self._seg(0)._subsample_time(arr, 10)
        self.assertEqual(out.shape[1:], arr.shape[1:])
        self.assertTrue(np.array_equal(np.asarray(out[0]), np.asarray(arr[0])))

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
