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


class PostProcessOnACropTest(unittest.TestCase):
    """`post_process` on a CROP — the task preview's case.

    The preview runs the run's own label modifications over the visible region, so tuning
    `minCellSize`/`labelExpansion` shows something. Two of those steps read the array edge as the
    IMAGE edge, which on a crop it usually isn't, and both errors point the same way: fewer cells than
    the run produces. `real_border` is what tells them apart, and `None` must reproduce the run's
    behaviour exactly — that is the part a regression would break silently.

    Headless: `post_process` needs only the params it reads off `self`, so it is driven against a stub.
    """

    @staticmethod
    def _seg(**params):
        from cecelia.utils.segmentation_utils import SegmentationUtils
        seg = SegmentationUtils.__new__(SegmentationUtils)     # no zarr/taskDir needed
        for k, v in dict(label_erosion=0, label_expansion=0, min_cell_size=0, cell_size_max=0,
                         clear_depth=False, clear_touching_border=False).items():
            setattr(seg, k, params.get(k, v))
        return seg

    @staticmethod
    def _mask():
        """A 10×10 plane: one label per interesting position."""
        a = np.zeros((10, 10), dtype=np.uint32)
        a[4:6, 0:2] = 1        # touches X-lo only
        a[0:2, 4:6] = 2        # touches Y-lo only
        a[4:6, 4:6] = 3        # interior
        a[4:6, 8:10] = 4       # touches X-hi only
        return a

    def _run(self, seg, arr, real_border=None):
        out = seg.post_process(arr, ['Y', 'X'], None, 1, False, real_border=real_border)
        return set(int(x) for x in np.unique(out) if x > 0)

    # ── the run's behaviour must be untouched ────────────────────────────────
    def test_no_real_border_clears_every_edge_as_before(self):
        seg = self._seg(clear_touching_border=True)
        self.assertEqual(self._run(seg, self._mask()), {3})     # only the interior label survives

    def test_no_real_border_judges_every_label_on_its_own_size(self):
        seg = self._seg(min_cell_size=5)                        # every label is 4 px
        self.assertEqual(self._run(seg, self._mask()), set())

    # ── the crop's behaviour ─────────────────────────────────────────────────
    def test_only_a_real_image_edge_is_cleared(self):
        # the crop sits at the image's top edge; its left/right/bottom are interior
        seg = self._seg(clear_touching_border=True)
        real = {'Y': (True, False), 'X': (False, False)}
        self.assertEqual(self._run(seg, self._mask(), real), {1, 3, 4})   # 2 (at Y-lo) cleared

    def test_no_real_edge_at_all_clears_nothing(self):
        # fully zoomed in: every edge is just where the user stopped looking
        seg = self._seg(clear_touching_border=True)
        real = {'Y': (False, False), 'X': (False, False)}
        self.assertEqual(self._run(seg, self._mask(), real), {1, 2, 3, 4})

    def test_a_clipped_label_is_exempt_from_the_size_filter(self):
        # THE artefact: a cell is small only because the crop cut it, and would be dropped for it
        seg = self._seg(min_cell_size=5)
        real = {'Y': (False, False), 'X': (False, False)}
        self.assertEqual(self._run(seg, self._mask(), real), {1, 2, 4})   # 3, interior, is really small

    def test_a_clipped_label_at_a_REAL_edge_is_still_judged(self):
        # not an artefact: the run sees the same clipping at the image edge, so the filter applies
        seg = self._seg(min_cell_size=5)
        real = {'Y': (True, False), 'X': (False, False)}
        self.assertEqual(self._run(seg, self._mask(), real), {1, 4})      # 2 at Y-lo now dropped too

    def test_the_max_size_filter_exempts_clipped_labels_too(self):
        # symmetric, and the direction that matters: a clipped cell reads SMALLER, so cellSizeMax
        # would wrongly KEEP one the run drops. Exempting is the honest answer either way — the
        # preview must not claim to know a size the crop hid.
        arr = np.zeros((10, 10), dtype=np.uint32)
        arr[0:9, 0:2] = 1       # 18 px, clipped by X-lo
        arr[4:6, 4:6] = 2       # 4 px, interior
        seg = self._seg(cell_size_max=10)
        real = {'Y': (False, False), 'X': (False, False)}
        self.assertEqual(self._run(seg, arr, real), {1, 2})
        self.assertEqual(self._run(seg, arr), {2})        # as the run: 1 is over the max, dropped

    def test_erosion_is_unaffected_by_the_crop(self):
        # purely local, so it needs no edge awareness — pinned so nobody "fixes" it into the mask
        arr = np.zeros((10, 10), dtype=np.uint32)
        arr[3:8, 3:8] = 7
        seg = self._seg(label_erosion=1)
        for real in (None, {'Y': (False, False), 'X': (False, False)}):
            out = seg.post_process(arr.copy(), ['Y', 'X'], None, 1, False, real_border=real)
            self.assertEqual(int((out == 7).sum()), 9)   # 5×5 eroded by 1 → 3×3

    def test_a_partial_real_border_dict_defaults_to_image_edge(self):
        # a caller naming only one axis must not silently get "interior" for the other — defaulting to
        # the image edge keeps the run's meaning when in doubt
        seg = self._seg(clear_touching_border=True)
        self.assertEqual(self._run(seg, self._mask(), {'X': (False, False)}), {1, 3, 4})


if __name__ == '__main__':
    unittest.main()
