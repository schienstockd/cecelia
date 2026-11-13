"""`noise_stats`, `range_stats` and `is_saturated`: their documented contracts, at the boundaries
(docs/todo/TEST_VALUE_PLAN.md → P2).

Expected values come from the docstrings: "more than NOISE_MAX_ZERO_FRAC", "MORE voxels than its
neighbours", "at least max(floor, total·frac) voxels", "the run of occupied bins just below",
"p1, p50 and p99.9 over NON-ZERO voxels", "a COMPACT_HIST_BINS-bin histogram over [0, max]".
"""
import unittest

import numpy as np

from cecelia.utils import intensity_utils as iu

NOISE_KEYS = {'bgLevel', 'bgMedian', 'bgSpread', 'snr', 'untestable', 'estimator'}


def background_and_signal(zeros=0):
    """A spread background around 20 and a signal block at 200."""
    h = np.zeros(256, np.int64)
    h[0] = zeros
    for v, c in zip(range(12, 29), [5, 20, 60, 140, 260, 400, 520, 600, 620, 600, 520, 400, 260, 140, 60, 20, 5]):
        h[v] = c
    h[195:206] = 30
    return h


class NoiseStats(unittest.TestCase):
    def test_every_path_returns_the_same_keys_and_untestable_says_why(self):
        empty = iu.noise_stats([5, 0, 0])
        self.assertEqual(set(empty), NOISE_KEYS)
        self.assertEqual(empty['untestable'], 'no non-zero voxels')
        ok = iu.noise_stats(background_and_signal())
        self.assertEqual(set(ok), NOISE_KEYS)
        self.assertIsNone(ok['untestable'])
        self.assertGreater(ok['snr'], 10)
        flat = np.zeros(64, np.int64); flat[10] = 1000; flat[50] = 10
        r = iu.noise_stats(flat)
        self.assertEqual(set(r), NOISE_KEYS)
        self.assertIn(r['untestable'], ('background spread is 0', 'no voxels above background',
                                        'no voxels in the background'))
        self.assertIsNone(r['snr'])

    def test_snr_is_signal_median_minus_background_median_over_the_upper_spread(self):
        r = iu.noise_stats(background_and_signal())
        self.assertEqual(r['bgMedian'], 20.0)                 # symmetric bump centred on 20
        self.assertAlmostEqual(r['snr'], (200 - r['bgMedian']) / r['bgSpread'], places=6)

    def test_exactly_half_zeros_is_not_more_than_half(self):
        base = background_and_signal()
        nonzero = int(base.sum())
        half = iu.noise_stats(background_and_signal(zeros=nonzero))          # 0.5 exactly
        self.assertIsNotNone(half['bgMedian'])
        over = iu.noise_stats(background_and_signal(zeros=nonzero + 1))
        self.assertEqual(over['untestable'], 'most voxels are 0')
        self.assertIsNone(over['bgSpread'])

    def test_the_zeros_are_named_from_the_floor_when_given(self):
        h = background_and_signal(zeros=10 ** 6)
        self.assertEqual(iu.noise_stats(h, floor={'kind': 'clipped'})['untestable'], 'background clipped at 0')
        self.assertTrue(iu.noise_stats(h, floor={'kind': 'photon-counted'})['untestable'].startswith('photon-counted'))

    def test_the_background_method_is_the_one_asked_for(self):
        h = background_and_signal()
        for method in iu.BACKGROUND_METHODS:
            with self.subTest(method=method):
                r = iu.noise_stats(h, background_method=method)
                self.assertEqual(r['bgLevel'], float(iu.background_threshold(h, method=method)))

    def test_one_non_zero_value_has_no_spread(self):
        h = np.zeros(32, np.int64); h[0] = 1; h[1] = 1
        r = iu.noise_stats(h)
        self.assertEqual(set(r), NOISE_KEYS)
        self.assertIsNone(r['snr'])


class RangeStats(unittest.TestCase):
    def test_percentiles_are_over_non_zero_voxels_and_the_histogram_covers_zero_to_max(self):
        h = np.zeros(128, np.int64)
        h[0] = 10 ** 6                                         # zeros must not pull p1/p50 to 0
        h[1:101] = 1                                           # values 1..100 once each
        r = iu.range_stats(h)
        self.assertEqual(set(r), {'p1', 'p50', 'p999', 'max', 'hist'})
        self.assertEqual((r['p1'], r['p50'], r['p999'], r['max']), (1, 50, 100, 100))
        self.assertEqual(set(r['hist']), {'binWidth', 'counts'})
        self.assertEqual(r['hist']['binWidth'], 101 / iu.COMPACT_HIST_BINS)
        self.assertEqual(len(r['hist']['counts']), iu.COMPACT_HIST_BINS)
        self.assertEqual(sum(r['hist']['counts']), int(h.sum()))    # all voxels, zeros included
        self.assertEqual(r['hist']['counts'][0], 10 ** 6 + 3)       # [0, 3.16): 0, 1, 2, 3

    def test_an_empty_channel(self):
        r = iu.range_stats(np.zeros(8, np.int64))
        self.assertEqual((r['p1'], r['p50'], r['p999'], r['max']), (None, None, None, 0))
        self.assertEqual(r['hist']['binWidth'], 1 / iu.COMPACT_HIST_BINS)


class IsSaturated(unittest.TestCase):
    def test_a_top_bin_equal_to_the_bins_below_is_not_a_pile_up(self):
        h = np.zeros(50, np.int64); h[30:40] = 100
        self.assertFalse(iu.is_saturated(h, min_count=1))     # top == median below
        h[39] = 101
        self.assertTrue(iu.is_saturated(h, min_count=1))

    def test_the_count_floor_is_inclusive(self):
        h = np.zeros(50, np.int64); h[30:39] = 10; h[39] = 64
        self.assertTrue(iu.is_saturated(h, min_count=64))
        self.assertFalse(iu.is_saturated(h, min_count=65))

    def test_two_occupied_bins_are_enough(self):
        self.assertTrue(iu.is_saturated(np.array([0, 5, 0, 500]), min_count=1))
        self.assertFalse(iu.is_saturated(np.array([0, 0, 0, 500]), min_count=1))

    def test_only_the_ten_occupied_bins_below_the_top_count(self):
        h = np.zeros(60, np.int64)
        h[10] = 10 ** 6                                        # an 11th bin below: far away, huge
        h[40:45] = 50; h[45:50] = 150                          # the ten below the top: median 100
        h[50] = 120                                            # top: above 100, below the 11-bin median
        self.assertTrue(iu.is_saturated(h, min_count=1))

    def test_the_bin_just_below_the_top_counts(self):
        h = np.zeros(60, np.int64)
        h[40:49] = 50                                          # nine bins of 50
        h[49] = 10 ** 6                                        # the bin just below the top
        h[50] = 60                                             # median of ten below is 50 → pile-up …
        self.assertTrue(iu.is_saturated(h, min_count=1))
        h[30] = 10 ** 6                                        # … an 11th bin further down changes nothing
        self.assertTrue(iu.is_saturated(h, min_count=1))

    def test_the_default_floor_scales_with_the_voxel_count(self):
        n = 10 ** 9                                            # floor = max(64, 1e-6 · 1e9) = 1000
        def with_top(top):
            h = np.zeros(60, np.int64)
            h[38:48] = 10                                      # the ten occupied bins below the top
            h[50] = top
            h[1] = n - int(h.sum())                            # the rest of the voxels, far below
            return h
        self.assertFalse(iu.is_saturated(with_top(999)))
        self.assertTrue(iu.is_saturated(with_top(1000)))

if __name__ == '__main__':
    unittest.main()
