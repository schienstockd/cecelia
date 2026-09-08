"""Unit tests for the `label.split` branch of the correction runner —
`app/src/tasks/segment/correct_run.py`.

The runner is executed by path via `run_py`, never imported, so nothing in the package suite
touches it. This file loads it once and exercises the pure helpers (`_bresenham`,
`_rasterise_polyline`, `_apply_split_inplace`) without spinning up the whole zarr write path —
those are covered end-to-end by manual eyeballing on a real project (docs/todo/CORRECTION_PLAN.md
→ Phase 4).

Skipped when `app/` is absent (external `pip install cecelia` consumers).
"""

import importlib.util
import unittest
from pathlib import Path

import numpy as np

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'segment' / 'correct_run.py')


def _load_runner():
    spec = importlib.util.spec_from_file_location('correct_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class _StubLog:
    """Minimal log adaptor — captures messages so tests can assert on them."""
    def __init__(self): self.messages = []
    def log(self, msg): self.messages.append(str(msg))


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class BresenhamTest(unittest.TestCase):
    def setUp(self):
        self.runner = _load_runner()

    def test_horizontal_line_covers_every_x(self):
        pts = list(self.runner._bresenham(2, 5, 8, 5))
        # a horizontal line at y=5 from x=2..8 must include every intermediate x.
        self.assertEqual(sorted(pts), [(5, x) for x in range(2, 9)])

    def test_vertical_line_covers_every_y(self):
        pts = list(self.runner._bresenham(3, 1, 3, 6))
        self.assertEqual(sorted(pts), [(y, 3) for y in range(1, 7)])

    def test_diagonal_is_monotonic_and_length_max_of_dx_dy(self):
        pts = list(self.runner._bresenham(0, 0, 5, 3))
        # Bresenham for (dx=5, dy=3) fires max(|dx|,|dy|)+1 = 6 pixels — that is the algorithm's
        # contract, not a tuning choice, and a regression here would betray the whole cut logic.
        self.assertEqual(len(pts), 6)
        self.assertEqual(pts[0], (0, 0))
        self.assertEqual(pts[-1], (3, 5))


class _StubDim:
    """A DimUtils shim exposing only what `_t_axis` reads."""
    def __init__(self, order):
        self.im_dim_order = list(order)


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class TAxisTest(unittest.TestCase):
    """`_t_axis` returns the T index in the LABELS array, accounting for the fact that a labels
    store drops the C axis. F2+F3 regression: the correction runner iterates by t on axis 0; a
    labels store where T isn't on axis 0 must be flagged, not silently truncated.
    """
    def setUp(self):
        self.runner = _load_runner()

    def test_t_axis_zero_for_canonical_tzyx_labels(self):
        # (T, Z, Y, X) shape=(4, 3, 10, 10). Intensity order carries a C axis after T which the
        # labels store drops — helper compensates.
        t = self.runner._t_axis(_StubDim(['T', 'C', 'Z', 'Y', 'X']), (4, 3, 10, 10))
        self.assertEqual(t, 0)

    def test_t_axis_one_for_ztyx_labels_flags_wrong_layout(self):
        # A foreign pipeline producing (Z, T, Y, X) would return t_idx=1 — the runner's guard
        # then refuses the run rather than iterating over the wrong axis.
        t = self.runner._t_axis(_StubDim(['Z', 'T', 'Y', 'X']), (3, 4, 10, 10))
        self.assertEqual(t, 1)

    def test_t_axis_none_for_still_image(self):
        t = self.runner._t_axis(_StubDim(['Z', 'Y', 'X']), (3, 10, 10))
        self.assertIsNone(t)


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class RasterisePolylineTest(unittest.TestCase):
    def setUp(self):
        self.runner = _load_runner()

    def test_out_of_bounds_pixels_are_clipped_not_thrown(self):
        # A caller can pass a polyline that extends beyond the frame (buildCentroidSplitOp does,
        # by ±4096 px). The rasteriser must clip, never index-error — the runner receives
        # pre-validated ints so a throw would mean silent data loss on a legit cut.
        mask = self.runner._rasterise_polyline([-10, 200], [5, 5], (20, 50))
        self.assertEqual(mask.shape, (20, 50))
        # Row 5 has a contiguous run inside the frame (x = 0..49).
        self.assertTrue(bool(mask[5].all()))
        # Every other row is untouched.
        self.assertFalse(bool(mask[np.r_[:5, 6:20]].any()))

    def test_polyline_multiple_segments(self):
        # Two-segment L-shape: (2,2)→(8,2) horizontal, then (8,2)→(8,6) vertical.
        mask = self.runner._rasterise_polyline([2, 8, 8], [2, 2, 6], (10, 12))
        self.assertTrue(bool(mask[2, 2:9].all()))
        self.assertTrue(bool(mask[2:7, 8].all()))


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class ApplySplitInplaceTest(unittest.TestCase):
    def setUp(self):
        self.runner = _load_runner()
        self.log = _StubLog()

    def _frame_with_label_5(self):
        """Build a 20×20 frame with label 5 filling the middle rectangle (rows 5..14, cols 3..16).
        Everything else is background (0). Label 5 spans 10 rows × 14 cols = 140 pixels."""
        frame = np.zeros((20, 20), dtype=np.int32)
        frame[5:15, 3:17] = 5
        return frame

    def test_horizontal_cut_splits_label_into_two_fragments(self):
        frame = self._frame_with_label_5()
        # Cut horizontally along y=9 (middle of the label). Pass endpoints beyond the label to
        # ensure a full traversal — this mirrors `buildCentroidSplitOp`'s ±4096 padding.
        op = {'op': 'label.split', 't': 0, 'id': 5, 'xs': [0, 19], 'ys': [9, 9]}
        n_pix = self.runner._apply_split_inplace(frame, op, self.log)
        # The larger fragment (rows 5..8, 4 rows × 14 cols = 56 px) keeps id 5.
        # Actually with a 1-px cut at y=9, we split into rows 5..8 (4 rows) and rows 10..14 (5 rows).
        # Larger = rows 10..14 (70 px) keeps id 5; smaller = rows 5..8 (56 px) gets a new id.
        # Cut pixels (row 9) return to the larger fragment (id 5) so no pixel goes to background.
        self.assertGreater(n_pix, 0)
        ids = set(int(x) for x in np.unique(frame) if int(x) != 0)
        self.assertEqual(len(ids), 2)                             # exactly two labels now
        self.assertIn(5, ids)                                     # id 5 preserved on the larger side

    def test_cut_that_misses_the_label_is_a_noop(self):
        frame = self._frame_with_label_5()
        # y=19 is outside the label's row range (5..14) — cut passes through background only.
        op = {'op': 'label.split', 't': 0, 'id': 5, 'xs': [0, 19], 'ys': [19, 19]}
        original = frame.copy()
        n_pix = self.runner._apply_split_inplace(frame, op, self.log)
        self.assertEqual(n_pix, 0)
        self.assertTrue(np.array_equal(frame, original))          # frame untouched
        self.assertTrue(any('not present' in m or 'did not divide' in m
                            for m in self.log.messages))

    def test_cut_along_the_edge_leaves_one_component_and_is_a_noop(self):
        frame = self._frame_with_label_5()
        # A cut that doesn't cross the label's interior yields 1 component after subtraction —
        # the runner refuses to invent a division that the user didn't draw.
        op = {'op': 'label.split', 't': 0, 'id': 5, 'xs': [0, 19], 'ys': [4, 4]}
        n_pix = self.runner._apply_split_inplace(frame, op, self.log)
        self.assertEqual(n_pix, 0)
        self.assertTrue(any('did not divide' in m or 'not present' in m
                            for m in self.log.messages))

    def test_cut_pixels_stay_with_parent_after_split(self):
        # F5 regression: the cut line's pixels must retain the parent id after a successful split
        # (they were part of the label before the cut and belong to the largest fragment). An
        # earlier revision computed a `keep` mask and discarded it — the invariant only held
        # by accident, and a future reorder of the fragment loop would silently zero them. Pin
        # the guarantee here.
        frame = self._frame_with_label_5()
        op = {'op': 'label.split', 't': 0, 'id': 5, 'xs': [0, 19], 'ys': [9, 9]}
        n_pix = self.runner._apply_split_inplace(frame, op, self.log)
        self.assertGreater(n_pix, 0)
        # Every pixel along the cut row that fell INSIDE the label's original column range must
        # still read as id 5 — nothing on the cut line goes to background or a new id.
        self.assertTrue(bool((frame[9, 3:17] == 5).all()))

    def test_missing_label_id_reports_and_returns_zero(self):
        frame = self._frame_with_label_5()
        op = {'op': 'label.split', 't': 0, 'id': 99, 'xs': [0, 19], 'ys': [9, 9]}
        n_pix = self.runner._apply_split_inplace(frame, op, self.log)
        self.assertEqual(n_pix, 0)
        self.assertTrue(any('not present' in m for m in self.log.messages))


if __name__ == '__main__':
    unittest.main()
