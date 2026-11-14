"""`floor_stats` at each of its thresholds (docs/todo/TEST_VALUE_PLAN.md → P2).

The expected side of each boundary comes from the wording of the constants' comments in
`intensity_utils`: "Minimum empty values" (a gap of exactly FLOOR_MIN_GAP counts), "stand this far
above" / "at least this share" / "at least this many times" (equal counts), "below this" and "each next
ratio smaller" (equal does not), "the counts at 1–5".
"""
import unittest

import numpy as np

from cecelia.utils import intensity_utils as iu

KEYS = {'kind', 'firstValue', 'photonStep', 'photons', 'evidence'}


def comb(gap, step, first=200, peak=1500, trough=300, second=900, zeros=10000, n=None):
    """Photon-counted shape: `zeros` at 0, nothing from 1 to `gap`, a cut edge of `first` at gap+1 that
    rises linearly to a one-photon `peak` at `step`, falls to `trough` at 1.5·step, rises to a
    two-photon peak `second` at 2·step and falls to 1 by 3·step."""
    n = n or 3 * step + 1
    pts = [(gap + 1, first), (step, peak), (int(1.5 * step), trough), (2 * step, second), (3 * step, 1)]
    h = np.zeros(n, np.int64)
    h[0] = zeros
    for (x0, y0), (x1, y1) in zip(pts, pts[1:]):
        for v in range(x0, min(x1 + 1, n)):
            h[v] = int(round(y0 + (y1 - y0) * (v - x0) / (x1 - x0)))
    return h


class FloorRecordShape(unittest.TestCase):
    def test_every_path_returns_the_same_keys_and_says_why(self):
        cases = {
            'none': [0, 5, 5],
            'all zero': [9, 0, 0],
            'clipped': [600, 100, 90, 85, 80, 78, 75],
            'ladder': [500, 100, 50, 20, 5],
            'gap, no comb': [500, 0, 0, 0, 0, 10, 50, 100, 50, 10, 2, 1],
            'photon-counted': comb(gap=10, step=30),
        }
        for name, h in cases.items():
            with self.subTest(name):
                r = iu.floor_stats(h)
                self.assertEqual(set(r), KEYS)
                if r['kind'] != 'none':
                    self.assertIsInstance(r['evidence'], str)
                    self.assertTrue(r['evidence'])

    def test_the_evidence_names_the_empty_run_and_the_peak(self):
        r = iu.floor_stats(comb(gap=10, step=30))
        self.assertEqual(r['kind'], 'photon-counted')
        self.assertIn('1 to 10', r['evidence'])
        self.assertIn(str(r['photonStep']), r['evidence'])
        r = iu.floor_stats([500, 0, 0, 0, 0, 10, 50, 100, 50, 10, 2, 1])
        self.assertIn('1 to 4', r['evidence'])

    def test_photons_count_non_zero_voxels_only(self):
        # p50 of the non-zero voxels sits on the one-photon peak, however many zeros there are
        for zeros in (10, 10 ** 7):
            r = iu.floor_stats(comb(gap=10, step=30, zeros=zeros))
            self.assertAlmostEqual(r['photons']['p50'], 1.0, delta=0.35)

    def test_a_single_zero_bin_has_nothing_to_read(self):
        self.assertEqual(iu.floor_stats([7])['kind'], 'indeterminate')
        self.assertEqual(iu.floor_stats([7])['evidence'], 'every voxel is 0')


class GapThreshold(unittest.TestCase):
    def test_a_gap_of_exactly_the_minimum_counts(self):
        g = iu.FLOOR_MIN_GAP
        self.assertEqual(iu.floor_stats(comb(gap=g, step=30))['kind'], 'photon-counted')
        self.assertNotEqual(iu.floor_stats(comb(gap=g - 1, step=30))['kind'], 'photon-counted')

    def test_first_value_is_the_smallest_occupied_value(self):
        self.assertEqual(iu.floor_stats(comb(gap=7, step=30))['firstValue'], 8)


class PeakAndEdgeThresholds(unittest.TestCase):
    def test_peak_exactly_the_rise_above_the_first_value_counts(self):
        h = comb(gap=10, step=30, first=1000)                      # peak 1500 = 1.5 × first value
        self.assertEqual(h[30], iu.FLOOR_PEAK_RISE * h[11])
        self.assertEqual(iu.floor_stats(h)['kind'], 'photon-counted')
        h = comb(gap=10, step=30, first=1001)                      # peak just under 1.5 ×
        self.assertNotEqual(iu.floor_stats(h)['kind'], 'photon-counted')

    def test_edge_exactly_the_minimum_share_of_the_peak_counts(self):
        h = comb(gap=10, step=30, first=100, peak=10000, trough=2000, second=6000)   # edge = 1 % of the peak
        self.assertEqual(h[11], iu.FLOOR_EDGE_MIN * h[30])
        self.assertEqual(iu.floor_stats(h)['kind'], 'photon-counted')
        h[11] -= 1
        self.assertNotEqual(iu.floor_stats(h)['kind'], 'photon-counted')


class SecondPhotonPeak(unittest.TestCase):
    def test_no_two_photon_peak_when_the_histogram_ends_before_its_window(self):
        # the one-photon peak at 30 is there, but the histogram stops before 2.4 × 30: no comb is seen
        h = comb(gap=10, step=30)[:int(round(iu.FLOOR_COMB_WINDOW[1] * 30)) + 1]
        self.assertNotEqual(iu.floor_stats(h)['kind'], 'photon-counted')

    def test_a_flat_run_after_the_first_peak_is_not_a_second_peak(self):
        h = comb(gap=10, step=30)
        h[45:] = h[45]                                             # no second rise, just a plateau
        self.assertNotEqual(iu.floor_stats(h)['kind'], 'photon-counted')


class ClippedSpike(unittest.TestCase):
    def test_spike_exactly_the_factor_over_counts_at_one_to_five(self):
        h = np.array([300, 100, 95, 92, 90, 88, 86, 84], np.int64)  # runs on from 1, no steep fall
        self.assertEqual(iu.floor_stats(h)['kind'], 'clipped')     # 300 = 3 × 100
        h[0] -= 1
        self.assertNotEqual(iu.floor_stats(h)['kind'], 'clipped')

    def test_the_spike_is_measured_against_every_value_from_one_to_five(self):
        base = np.array([300, 60, 59, 58, 57, 56, 55, 54], np.int64)
        for v in range(1, 6):
            with self.subTest(value=v):
                h = base.copy(); h[v] = 101                        # one value in 1–5 over a third of 0
                self.assertNotEqual(iu.floor_stats(h)['kind'], 'clipped')
        h = base.copy(); h[6] = 101                                # value 6 is outside the window
        self.assertEqual(iu.floor_stats(h)['kind'], 'clipped')


class PoissonLadder(unittest.TestCase):
    def test_a_fall_of_exactly_the_ratio_is_not_steep(self):
        f = iu.FLOOR_LADDER_FALL
        steep = [1000, 100, int(100 * f) - 1, 10, 1]
        self.assertIn('fall steeply', iu.floor_stats(steep)['evidence'])
        exact = [1000, 100, int(100 * f), 10, 1]                   # h2 / h1 = 0.6 exactly: not below
        self.assertNotIn('fall steeply', iu.floor_stats(exact)['evidence'])

    def test_each_next_ratio_must_be_smaller(self):
        self.assertIn('fall steeply', iu.floor_stats([1000, 100, 50, 24, 1])['evidence'])
        # h3/h2 = h2/h1 (0.5 = 0.5): not smaller
        self.assertNotIn('fall steeply', iu.floor_stats([1000, 100, 50, 25, 1])['evidence'])

    def test_the_ladder_needs_value_three(self):
        self.assertNotIn('fall steeply', iu.floor_stats([1000, 100, 50])['evidence'] or '')


class MinimumShare(unittest.TestCase):
    def test_a_value_with_exactly_the_minimum_share_is_occupied(self):
        h = np.zeros(200, np.int64)
        h[0] = 10
        h[150] = 10 ** 6 - 10                                      # the bulk
        h[100] = 10                                                # 10 / 10^6 = FLOOR_MIN_SHARE exactly
        self.assertEqual(int(h[1:].sum()), 10 ** 6)
        self.assertEqual(iu.floor_stats(h)['firstValue'], 100)
        h[100] = 9; h[150] += 1
        self.assertEqual(iu.floor_stats(h)['firstValue'], 150)


if __name__ == '__main__':
    unittest.main()
