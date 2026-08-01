"""Unit tests for cecelia.utils.intensity_utils (16→8-bit rescale on import).

Golden values pin the per-channel histogram, the true-min/max + percentile window, the uint8
rescale, and the clip stats. Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import unittest

import numpy as np

import cecelia.utils.intensity_utils as iu


class RobustMaxWindowTest(unittest.TestCase):
    """One hot pixel must not set the 8-bit window.

    This is the live default path: `rescaleFixedMax` is 0 (off) unless someone sets it, so every
    16→8-bit import lands on `range_from_hist(h, 0, 100)`. With the literal true max, a single
    saturated voxel pins the top of the window and the real signal collapses into a fraction of the
    output range — permanently, since the conversion is one-way. Measured independently twice: #440
    ("the real signal used only ~15% of the range") and the AF work (top six ratio bins held one voxel
    each, in 5.88 G voxels).

    Only production call site: `channel_ranges` → `rescale_to_8bit_run.py`.
    """

    @staticmethod
    def _hist(nbins=65536):
        """A realistic dim channel: a broad low peak, plus ONE saturated voxel."""
        h = np.zeros(nbins, np.int64)
        h[0:200] = 5000          # background
        h[200:900] = 400         # signal, up to raw 900
        h[65535] = 1             # a single hot pixel
        return h

    def test_a_lone_hot_pixel_does_not_set_the_window(self):
        h = self._hist()
        _, vmax = iu.range_from_hist(h, 0.0, 100.0)
        self.assertLess(vmax, 1000.0, 'the window was pinned by the hot pixel')
        self.assertGreaterEqual(vmax, 800.0, 'real signal must not be clipped away')

    def test_the_literal_true_max_is_still_available(self):
        h = self._hist()
        _, vmax = iu.range_from_hist(h, 0.0, 100.0, robust=False)
        self.assertEqual(vmax, 65535.0)

    def test_a_small_histogram_falls_back_to_the_true_max(self):
        # the floor: too few voxels for a tail to mean anything, so behave exactly as before. This is
        # what keeps the golden values below valid rather than needing a re-bless.
        h = np.zeros(256, np.int64)
        h[5] = 1
        h[200] = 1
        self.assertEqual(iu.range_from_hist(h, 0.0, 100.0), (5.0, 200.0))

    def test_an_explicit_percentile_is_untouched(self):
        h = self._hist()
        self.assertEqual(iu.range_from_hist(h, 0.0, 50.0)[1], iu.hist_percentile(h, 50))

    def test_min_count_can_be_given_directly(self):
        h = np.zeros(1024, np.int64)
        h[10] = 1_000
        h[500] = 50
        h[900] = 5
        self.assertEqual(iu.robust_hist_max(h, min_count=1), 900)
        self.assertEqual(iu.robust_hist_max(h, min_count=10), 500)
        self.assertEqual(iu.robust_hist_max(h, min_count=100), 10)

    def test_the_fixed_window_still_bypasses_all_of_this(self):
        # #440's option is untouched: it buys comparability between channels and images, which no
        # per-image estimate can give. The robust max fixes the DEFAULT, not the fixed path.
        h = self._hist()
        self.assertEqual(iu.channel_ranges([h, h], fixed=(20, 60000)),
                         [(20.0, 60000.0), (20.0, 60000.0)])


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

    @staticmethod
    def _tail(ceiling, clip_at=None, n=4096):
        """A realistic fluorescence histogram: a background peak and a DECAYING tail. With
        `clip_at`, everything above it is accumulated into that bin — what a saturating detector
        does. Crude two-spike fixtures cannot tell the two cases apart, which is the whole point."""
        h = np.zeros(n, dtype=np.int64)
        h[100] = 5_000_000
        vals = np.arange(101, ceiling + 1)
        counts = (200_000 * np.exp(-(vals - 101) / 250.0)).astype(np.int64)
        h[101:ceiling + 1] = counts
        if clip_at is not None:
            spill = int(h[clip_at + 1:].sum())
            h[clip_at + 1:] = 0
            h[clip_at] += spill
        return h

    def test_is_saturated_distinguishes_clipping_from_a_decaying_tail(self):
        """A tail decays, so its top bin is the sparsest. Clipping inverts that — everything the
        detector could not represent piles into the top bin."""
        self.assertFalse(iu.is_saturated(self._tail(2000)))                  # natural end
        self.assertTrue(iu.is_saturated(self._tail(4000, clip_at=1200)))     # pile-up
        self.assertFalse(iu.is_saturated(np.zeros(4096, dtype=np.int64)))
        hot = self._tail(2000); hot[4095] = 1                                # lone hot pixel
        self.assertFalse(iu.is_saturated(hot))

    def test_reference_window_ignores_a_saturated_channel(self):
        """Measured on a real movie: one channel saturated at the 12-bit ceiling. Letting it choose
        the window put every other image's cells at 68/255 instead of 170 — a 2.5x contrast loss
        across the whole set, to protect pixels the detector had already destroyed."""
        good = self._tail(1600)                            # decays to a natural end
        sat  = self._tail(4095, clip_at=1200)              # everything above 1200 piles into it
        self.assertTrue(iu.is_saturated(sat))
        _, hi = iu.reference_window([good, sat], leeway=1.5)
        self.assertAlmostEqual(hi, 1600 * 1.5)             # the unsaturated channel set it

    def test_reference_window_falls_back_when_every_channel_is_saturated(self):
        """Nothing better to go on — a window is still better than none."""
        sat = self._tail(4095, clip_at=1200)
        _, hi = iu.reference_window([sat, sat], leeway=1.0)
        self.assertEqual(hi, 1200.0)                       # the clip point, not nothing at all

    def test_reference_window_is_shared_across_channels_and_floored_at_zero(self):
        """Derived from a reference image, applied to its whole set. Shared across channels because
        the ratio between them is the thing that has to survive; floored at 0 because the camera
        offset is real baseline and clipping into it biases every background estimate."""
        bright = np.zeros(4096, dtype=np.int64); bright[100] = 10_000; bright[800] = 5_000
        dim    = np.zeros(4096, dtype=np.int64); dim[100]    = 10_000; dim[300] = 5_000
        lo, hi = iu.reference_window([dim, bright], leeway=1.5)
        self.assertEqual(lo, 0.0)                       # floor is 0, not the image minimum
        self.assertAlmostEqual(hi, 800 * 1.5)           # the BRIGHTEST channel sets it, plus leeway

    def test_reference_window_ignores_a_hot_pixel(self):
        """The failure this replaces: one voxel at the 12-bit ceiling deciding the whole set's
        window. A leeway multiplier on a hot pixel would be worse than no leeway at all."""
        h = np.zeros(4096, dtype=np.int64); h[100] = 10_000; h[600] = 5_000; h[4095] = 1
        lo, hi = iu.reference_window([h], leeway=1.5)
        self.assertAlmostEqual(hi, 600 * 1.5)
        self.assertLess(hi, 4095)

    def test_reference_window_none_when_there_is_no_signal(self):
        """A caller must be able to fall back rather than write a degenerate [0, 0] window."""
        self.assertIsNone(iu.reference_window([np.zeros(4096, dtype=np.int64)]))

    def test_leeway_trades_contrast_for_headroom(self):
        """Leeway is not free and the test says so: more headroom means the reference's own signal
        lands lower. It buys not clipping a brighter image in the same set."""
        h = np.zeros(4096, dtype=np.int64); h[100] = 10_000; h[600] = 5_000
        _, tight = iu.reference_window([h], leeway=1.0)
        _, loose = iu.reference_window([h], leeway=2.0)
        self.assertEqual(tight, 600.0)
        self.assertEqual(loose, 1200.0)
        # the reference's own peak lands at half the 8-bit value under 2x leeway
        self.assertAlmostEqual(600 / tight * 255, 2 * (600 / loose * 255))

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
