"""`intensity_utils.floor_stats`: what a channel's exact zeros are — photon-counted (no photon) or a
background clipped at 0 — and the noise reason that follows (docs/todo/ACQUISITION_ADVISOR_PLAN.md → P4b)."""
import json
import os
import unittest

import numpy as np

from cecelia.utils import intensity_utils as iu

FIXTURE = os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data', 'acquisition',
                       'floor_photon_counted_fv4000.json')


def _hist(values, size=4096):
    return np.bincount(np.asarray(values, dtype=np.int64), minlength=size)


def _photon_counted(mean, step=30, n=400_000, threshold=19, seed=0):
    """A photon-counting detector: Poisson photons per voxel, each adding ~`step` (pulse-height jitter),
    a discriminator that writes 0 below `threshold`."""
    rng = np.random.default_rng(seed)
    k = rng.poisson(mean, n)
    v = np.round(k * step + rng.normal(0, 4, n) * np.sqrt(k)).astype(np.int64)
    v[v < threshold] = 0
    return v


class FloorStatsTest(unittest.TestCase):
    def test_real_fv4000_store_is_photon_counted(self):
        with open(FIXTURE, encoding='utf-8') as f:
            chans = json.load(f)['channels']
        for ch in chans:
            h = np.asarray(ch['counts'])
            fl = iu.floor_stats(h)
            assert fl['kind'] == 'photon-counted', (ch['index'], fl)
            assert fl['firstValue'] == 19 and 24 <= fl['photonStep'] <= 34, fl
            assert iu.noise_stats(h, floor=fl)['untestable'] == 'photon-counted: zeros are voxels with no photon'

    def test_synthetic_photon_limited_channel(self):
        fl = iu.floor_stats(_hist(_photon_counted(0.2)))
        assert fl['kind'] == 'photon-counted'
        assert abs(fl['photonStep'] - 30) <= 2
        assert 0.9 <= fl['photons']['p50'] <= 1.1                 # most non-zero voxels: one photon

    def test_brighter_photon_counted_channel_keeps_the_single_photon_step(self):
        # mean 2.5 photons: the two-photon peak is taller than the one-photon peak
        fl = iu.floor_stats(_hist(_photon_counted(2.5)))
        assert fl['kind'] == 'photon-counted' and abs(fl['photonStep'] - 30) <= 2

    def test_offset_clipped_gaussian_background(self):
        rng = np.random.default_rng(1)
        v = np.clip(rng.normal(-5, 10, 300_000).round(), 0, None).astype(int)   # ~70 % at 0
        fl = iu.floor_stats(_hist(v))
        assert fl['kind'] == 'clipped', fl
        assert iu.noise_stats(_hist(v), floor=fl)['untestable'] == 'background clipped at 0'

    def test_hard_threshold_on_analog_data_is_not_photon_counted(self):
        # values below 40 set to 0 on a falling analog distribution: a gap, but no peak above it
        rng = np.random.default_rng(2)
        v = np.round(rng.exponential(25, 300_000)).astype(int)
        v[v < 40] = 0
        assert iu.floor_stats(_hist(v))['kind'] == 'indeterminate'

    def test_no_zeros(self):
        assert iu.floor_stats(_hist(np.arange(1, 500)))['kind'] == 'none'

    def test_noise_reason_without_a_floor_does_not_guess_the_cause(self):
        vals = np.concatenate([np.zeros(9000, int), np.arange(20, 220)])
        assert iu.noise_stats(_hist(vals))['untestable'] == 'most voxels are 0'


if __name__ == '__main__':
    unittest.main()
