"""Unit tests for cecelia.utils.intensity_utils (16→8-bit rescale on import).

Golden values pin the per-channel histogram, the true-min/max + percentile window, the uint8
rescale, and the clip stats. Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import unittest

import numpy as np

import cecelia.utils.intensity_utils as iu


class TestIntensityUtils(unittest.TestCase):
    def setUp(self):
        # C=2, Y=2, X=3 — channel axis 0
        self.arr = np.array([
            [[0, 10, 20], [30, 40, 50]],          # ch0: spread 0..50
            [[100, 100, 100], [100, 100, 100]],   # ch1: flat at 100
        ], dtype=np.uint16)
        self.caxis = 0

    def test_histograms(self):
        hists = iu.channel_histograms(self.arr, self.caxis)
        self.assertEqual(len(hists), 2)
        self.assertEqual(hists[0][0], 1)
        self.assertEqual(hists[0][50], 1)
        self.assertEqual(hists[1][100], 6)

    def test_histograms_rejects_float(self):
        with self.assertRaises(ValueError):
            iu.channel_histograms(self.arr.astype(np.float32), self.caxis)

    def test_range_true_minmax(self):
        hists = iu.channel_histograms(self.arr, self.caxis)
        self.assertEqual(iu.range_from_hist(hists[0], 0.0, 100.0), (0.0, 50.0))
        self.assertEqual(iu.range_from_hist(hists[1], 0.0, 100.0), (100.0, 100.0))

    def test_range_percentile(self):
        # 6 distinct values 0,10,…,50; 50th percentile → cdf reaches 3 at value 20
        hists = iu.channel_histograms(self.arr, self.caxis)
        _, vmax = iu.range_from_hist(hists[0], 0.0, 50.0)
        self.assertEqual(vmax, 20.0)

    def test_channel_ranges_percentile_matches_the_per_channel_default(self):
        hists = iu.channel_histograms(self.arr, self.caxis)
        self.assertEqual(iu.channel_ranges(hists, 0.0, 100.0),
                         [iu.range_from_hist(h, 0.0, 100.0) for h in hists])

    def test_channel_ranges_fixed_is_the_same_window_for_every_channel(self):
        """The point of the fixed window: two channels with very different content still get the
        SAME map, so their values stay comparable. The percentile default does the opposite — here
        it gives ch0 a span of 50 and ch1 a span of 0, i.e. a ~infinitely different gain."""
        hists = iu.channel_histograms(self.arr, self.caxis)
        self.assertEqual(iu.channel_ranges(hists, fixed=(20, 120)), [(20.0, 120.0), (20.0, 120.0)])

        out = iu.rescale_stack_to_uint8(self.arr, self.caxis, iu.channel_ranges(hists, fixed=(0, 100)))
        # one shared map [0,100] -> [0,255]: ch1's flat 100 now reads 255, not 0, and ch0's 50
        # reads half of it — the ratio between the channels survives.
        np.testing.assert_array_equal(out[1], np.full((2, 3), 255, np.uint8))
        self.assertEqual(int(out[0][1, 2]), 127)

    def test_fixed_window_clip_stats_still_report_what_was_clipped(self):
        """A fixed window can clip, unlike the true-min/max default — so the QC numbers are the
        thing that tells you whether it was set sensibly. They must stay meaningful."""
        hists = iu.channel_histograms(self.arr, self.caxis)
        s = iu.clip_stats(hists[0], 0.0, 25.0)        # ch0 spans 0..50, so half the values clip
        self.assertGreater(s['clipHighFrac'], 0.0)
        self.assertEqual(s['trueMax'], 50)

    def test_rescale_golden(self):
        hists = iu.channel_histograms(self.arr, self.caxis)
        ranges = [iu.range_from_hist(h, 0.0, 100.0) for h in hists]
        out = iu.rescale_stack_to_uint8(self.arr, self.caxis, ranges)
        self.assertEqual(out.dtype, np.uint8)
        # ch0: linear map [0,50] → [0,255]; ch1 flat → 0 (denom guarded to 1, value==vmin → 0)
        np.testing.assert_array_equal(out[0], np.array([[0, 51, 102], [153, 204, 255]], np.uint8))
        np.testing.assert_array_equal(out[1], np.zeros((2, 3), np.uint8))

    def test_clip_stats_no_clip(self):
        hists = iu.channel_histograms(self.arr, self.caxis)
        vmin, vmax = iu.range_from_hist(hists[0], 0.0, 100.0)
        s = iu.clip_stats(hists[0], vmin, vmax)
        self.assertEqual(s["clipHighFrac"], 0.0)
        self.assertEqual(s["clipLowFrac"], 0.0)
        self.assertEqual(s["rangeSpan"], 50.0)
        self.assertEqual(s["trueMax"], 50)

    def test_clip_stats_trimmed_high(self):
        # trimming the top to value 20 clips the 3 pixels above it (30,40,50) → 0.5 of 6
        hists = iu.channel_histograms(self.arr, self.caxis)
        s = iu.clip_stats(hists[0], 0.0, 20.0)
        self.assertAlmostEqual(s["clipHighFrac"], 0.5)

    def test_no_channel_axis(self):
        # single-channel (no C axis) path: one range applied to the whole array
        flat = self.arr[0]                      # (2,3), values 0..50
        hists = iu.channel_histograms(flat, None)
        self.assertEqual(len(hists), 1)
        ranges = [iu.range_from_hist(hists[0], 0.0, 100.0)]
        out = iu.rescale_stack_to_uint8(flat, None, ranges)
        np.testing.assert_array_equal(out, np.array([[0, 51, 102], [153, 204, 255]], np.uint8))


if __name__ == "__main__":
    unittest.main()
