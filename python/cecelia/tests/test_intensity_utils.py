"""Unit tests for cecelia.utils.intensity_utils (whole-stack intensity statistics).

Golden values pin the streamed per-channel histogram, the outlier-rejected max, the derived
background threshold and the clip stats — the primitives AF correction and segmentation
normalisation are built on. Part of the Python (analysis-env) suite — run with `pixi run test-py`.

`robust_hist_max` is under test because `correction_utils.af_division_stats` depends on its outlier
rejection: a single hot pixel must not decide a derived ceiling.
"""
import unittest

import numpy as np

import cecelia.utils.intensity_utils as iu


class RobustMaxTest(unittest.TestCase):
    """One hot pixel must not set a derived ceiling.

    Production call site: `correction_utils.af_division_stats`, which uses `robust_hist_max` for the
    AF ratio ceiling. Measured independently twice: #440 ("the real signal used only ~15% of the
    range" with a literal max) and the AF work (the top six ratio bins held ONE voxel each, out of
    5.88 G voxels).
    """

    @staticmethod
    def _hist(nbins=65536):
        """A realistic dim channel: a broad low peak, plus ONE saturated voxel."""
        h = np.zeros(nbins, np.int64)
        h[0:200] = 5000          # background
        h[200:900] = 400         # signal, up to raw 900
        h[65535] = 1             # a single hot pixel
        return h

    def test_a_lone_hot_pixel_does_not_set_the_ceiling(self):
        vmax = iu.robust_hist_max(self._hist())
        self.assertLess(vmax, 1000, 'the ceiling was pinned by the hot pixel')
        self.assertGreaterEqual(vmax, 800, 'real signal must not be rejected as an outlier')

    def test_a_small_histogram_falls_back_to_the_true_max(self):
        # the floor: too few voxels for a tail to mean anything, so don't reject anything as an
        # outlier. Keeps synthetic and tiny cases predictable.
        h = np.zeros(256, np.int64)
        h[5] = 1
        h[200] = 1
        self.assertEqual(iu.robust_hist_max(h), 200)

    def test_min_count_can_be_given_directly(self):
        h = np.zeros(1024, np.int64)
        h[10] = 1_000
        h[500] = 50
        h[900] = 5
        self.assertEqual(iu.robust_hist_max(h, min_count=1), 900)
        self.assertEqual(iu.robust_hist_max(h, min_count=10), 500)
        self.assertEqual(iu.robust_hist_max(h, min_count=100), 10)

    def test_an_empty_histogram_is_zero(self):
        self.assertEqual(iu.robust_hist_max(np.zeros(4096, np.int64)), 0)


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

    def test_histograms_of_a_channel_subset(self):
        # segmentation normalises only its cell/nuc channels — it must not pay to scan every channel
        hists = iu.channel_histograms(self.arr, self.caxis, channels=[1])
        self.assertEqual(len(hists), 1)
        self.assertEqual(hists[0][100], 6)

    def test_no_channel_axis(self):
        flat = self.arr[0]                      # (2,3), values 0..50
        hists = iu.channel_histograms(flat, None)
        self.assertEqual(len(hists), 1)
        self.assertEqual(hists[0][50], 1)

    def test_hist_percentile(self):
        hists = iu.channel_histograms(self.arr, self.caxis)
        self.assertEqual(iu.hist_percentile(hists[0], 100), 50)
        self.assertEqual(iu.hist_percentile(hists[1], 50), 100)
        self.assertEqual(iu.hist_percentile(np.zeros(16, np.int64), 50), 0)

    def test_clip_stats_no_clip(self):
        hists = iu.channel_histograms(self.arr, self.caxis)
        s = iu.clip_stats(hists[0], 0.0, 50.0)          # ch0's own full span — nothing to clip
        self.assertEqual(s["clipHighFrac"], 0.0)
        self.assertEqual(s["clipLowFrac"], 0.0)
        self.assertEqual(s["rangeSpan"], 50.0)
        self.assertEqual(s["trueMax"], 50)

    def test_clip_stats_trimmed_high(self):
        # trimming the top to value 20 clips the 3 pixels above it (30,40,50) → 0.5 of 6
        hists = iu.channel_histograms(self.arr, self.caxis)
        s = iu.clip_stats(hists[0], 0.0, 20.0)
        self.assertAlmostEqual(s["clipHighFrac"], 0.5)

    def test_clip_stats_on_an_empty_histogram(self):
        s = iu.clip_stats(np.zeros(256, np.int64), 0.0, 10.0)
        self.assertEqual(s["total"], 0)
        self.assertEqual(s["clipHighFrac"], 0.0)

    @staticmethod
    def _tail(ceiling, n=4096):
        """A realistic fluorescence histogram: a background peak and a DECAYING signal tail."""
        h = np.zeros(n, dtype=np.int64)
        h[100] = 5_000_000
        vals = np.arange(101, ceiling + 1)
        h[101:ceiling + 1] = (200_000 * np.exp(-(vals - 101) / 250.0)).astype(np.int64)
        return h

    def test_background_threshold_sits_between_the_peak_and_the_tail(self):
        """Triangle over a background peak with a long tail — the default, and what the AF correction
        derives its background pair from instead of two hand-tuned percentiles."""
        t = iu.background_threshold(self._tail(2000))
        self.assertGreater(t, 100.0)
        self.assertLess(t, 2000.0)

    def test_background_threshold_ignores_the_zero_bin(self):
        """Load-bearing: measured on real channels, 91-95% of voxels are EXACTLY zero (already
        background-subtracted upstream), so including bin 0 collapses every threshold to 0 and the
        AF correction then divides by sensor noise."""
        h = self._tail(2000)
        h[0] = 500_000_000                                   # dwarf everything else
        self.assertGreater(iu.background_threshold(h, ignore_zero=True), 100.0)
        # with bin 0 in play the peak IS bin 0, so the threshold collapses to the very bottom of the
        # histogram (1.0 here) — i.e. into sensor noise, not onto the real background level
        self.assertLess(iu.background_threshold(h, ignore_zero=False), 10.0)

    def test_background_threshold_none_disables_it(self):
        self.assertEqual(iu.background_threshold(self._tail(2000), method='none'), 0.0)

    def test_background_threshold_rejects_an_unknown_method(self):
        with self.assertRaises(ValueError):
            iu.background_threshold(self._tail(2000), method='mean')

    def test_triangle_threshold_degenerate_histograms(self):
        self.assertEqual(iu.triangle_threshold(np.zeros(16, np.int64)), 0.0)
        one = np.zeros(16, np.int64); one[7] = 5
        self.assertEqual(iu.triangle_threshold(one), 7.0)


if __name__ == "__main__":
    unittest.main()
