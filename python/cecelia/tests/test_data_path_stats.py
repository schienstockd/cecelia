"""`intensity_utils.range_stats` / `noise_stats` — the data-path panel's per-channel numbers
(docs/todo/ACQUISITION_ADVISOR_PLAN.md → P4b)."""
import unittest

import numpy as np

from cecelia.utils import intensity_utils as iu


def _hist(values, size=4096):
    return np.bincount(np.asarray(values, dtype=np.int64), minlength=size)


class DataPathStatsTest(unittest.TestCase):
    def test_range_ignores_exact_zeros_and_reports_max(self):
        # 90 % zeros, the rest spread 100..199: percentiles describe the signal, not the zeros
        vals = np.concatenate([np.zeros(9000, int), np.repeat(np.arange(100, 200), 10)])
        r = iu.range_stats(_hist(vals))
        assert r['max'] == 199
        assert r['p1'] == 100 and 148 <= r['p50'] <= 150 and r['p999'] == 199
        assert len(r['hist']['counts']) == iu.COMPACT_HIST_BINS
        assert sum(r['hist']['counts']) == vals.size            # every voxel lands in one bin
        assert r['hist']['counts'][0] == 9000                    # the zeros sit in the first bin

    def test_range_of_an_empty_channel(self):
        r = iu.range_stats(_hist([0, 0, 0]))
        assert r['max'] == 0 and r['p50'] is None

    def test_noise_floor_and_snr_on_a_measurable_background(self):
        rng = np.random.default_rng(0)
        bg = np.clip(rng.normal(100, 10, 200_000).round(), 1, None)       # sigma 10 around 100
        sig = np.clip(rng.normal(600, 30, 20_000).round(), 1, None)
        n = iu.noise_stats(_hist(np.concatenate([bg, sig]).astype(int)))
        assert n['untestable'] is None
        assert abs(n['bgMedian'] - 100) <= 2
        assert 8 <= n['bgSpread'] <= 12                                    # p84 - median ~ sigma
        assert 40 <= n['snr'] <= 60                                        # (600 - 100) / 10

    def test_noise_is_untestable_when_most_voxels_are_zero(self):
        # what the zeros are is floor_stats' call (test_floor_stats.py); here: no number is invented
        vals = np.concatenate([np.zeros(9000, int), np.arange(20, 220)])
        n = iu.noise_stats(_hist(vals))
        assert n['snr'] is None and n['bgSpread'] is None
        assert n['untestable'] == 'most voxels are 0'
        assert n['bgLevel'] is not None                                    # the level is still reported

    def test_noise_of_an_all_zero_channel(self):
        assert iu.noise_stats(_hist([0] * 10))['untestable'] == 'no non-zero voxels'


if __name__ == '__main__':
    unittest.main()
