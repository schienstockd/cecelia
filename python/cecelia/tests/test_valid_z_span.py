"""`SegmentationUtils._valid_z_span` — how much of a padded z-stack is worth segmenting.

A drift-corrected canvas holds each frame at its own offset and zeroes the rest — 3-56% of the z
planes across the movies on this machine. The whole stack goes to cellpose in ONE call (it stitches
across z internally), so those planes cost GPU time and produce nothing.

The safety argument is structural, not empirical: a valid box is a CONTIGUOUS [start, stop), so
narrowing z to it can only drop LEADING/TRAILING planes. Interior planes inside the span survive, and
the dropped ones are all-zero, so they offer no labels for `stitch_threshold` to link across.

Every ambiguous case must widen, never narrow: missing cells is a real cost, doing the work anyway is
merely the status quo.

Part of the Python (analysis-env) test suite — run with `pixi run test-py`.
"""
import unittest

from cecelia.utils.segmentation_utils import SegmentationUtils as SU


class ValidZSpanTest(unittest.TestCase):

    def test_narrows_to_the_box(self):
        self.assertEqual(SU._valid_z_span({'Z': (5, 13)}, 22), (5, 13))

    def test_no_box_means_the_whole_stack(self):
        for box in (None, {}, {'Y': (0, 4)}):
            self.assertEqual(SU._valid_z_span(box, 22), (0, 22))

    def test_a_full_width_box_is_a_no_op(self):
        self.assertEqual(SU._valid_z_span({'Z': (0, 22)}, 22), (0, 22))

    def test_clamps_to_the_stack(self):
        """A box in level-0 coordinates against a smaller level must never index out of range."""
        self.assertEqual(SU._valid_z_span({'Z': (-3, 40)}, 22), (0, 22))
        self.assertEqual(SU._valid_z_span({'Z': (18, 40)}, 22), (18, 22))

    def test_a_degenerate_span_widens_rather_than_segmenting_nothing(self):
        """An empty or inverted range is a broken box, not an instruction to segment no planes."""
        for rng in ((7, 7), (9, 4), (0, 0)):
            self.assertEqual(SU._valid_z_span({'Z': rng}, 22), (0, 22))

    def test_a_single_plane_span_widens(self):
        """One plane is not meaningfully a 3D stack; cellpose's z-stitching has nothing to stitch."""
        self.assertEqual(SU._valid_z_span({'Z': (10, 11)}, 22), (0, 22))
        self.assertEqual(SU._valid_z_span({'Z': (10, 12)}, 22), (10, 12))   # two is enough

    def test_a_2d_image_is_untouched(self):
        self.assertEqual(SU._valid_z_span({'Z': (0, 1)}, 1), (0, 1))

    def test_the_real_measured_cases(self):
        """kSUFux: 8 valid planes in canvases of 11-18 — the case that motivated this."""
        self.assertEqual(SU._valid_z_span({'Z': (7, 15)}, 22), (7, 15))     # 64% skipped
        self.assertEqual(SU._valid_z_span({'Z': (0, 8)}, 12), (0, 8))       # leading-only
        self.assertEqual(SU._valid_z_span({'Z': (4, 12)}, 12), (4, 12))     # trailing-only
