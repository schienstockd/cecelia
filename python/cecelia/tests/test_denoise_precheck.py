"""Pure-helper tests for the SUPPORT training precheck — `channel_snr_proxy` and `_resolve_mode`
in `app/src/tasks/opticalFlow/train_support_denoise_run.py`.

The precheck is what turns `trainMode = auto` into either pooled or perChannel, so its behaviour is
worth pinning without needing GPU or torch weights. See SUPPORT_PERCHANNEL_PLAN.md → D4(a).
"""
import importlib.util
import unittest
from pathlib import Path

import numpy as np
import torch

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'opticalFlow' / 'train_support_denoise_run.py')


def _load_runner():
    spec = importlib.util.spec_from_file_location('train_support_denoise_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class _StubLog:
    def __init__(self):
        self.lines = []
    def log(self, s):
        self.lines.append(s)


@unittest.skipUnless(_RUNNER.exists(), '`app/` not vendored (external cecelia install)')
class ChannelSnrProxyTest(unittest.TestCase):

    def setUp(self):
        self.mod = _load_runner()

    def test_empty_channel_returns_nan(self):
        s = self.mod.channel_snr_proxy([])
        self.assertTrue(np.isnan(s['worstSnr']))
        self.assertEqual(s['perVolume'], [])

    def test_mostly_dark_frame_uses_nonzero_median_not_p50(self):
        # Sparse-signal microscopy: half the pixels are 0 (background), rest carries the signal.
        # Raw p50 = 0 would pin sqrt(max(0,1)) = 1 as noise σ and pass everything; nonzero-median
        # sees the real signal floor (fXgbTl 2026-09-07 was the ground truth for this bug).
        rng = np.random.default_rng(0)
        v = np.zeros((8, 32, 32), dtype=np.float32)
        signal_mask = rng.random(v.shape) < 0.3
        v[signal_mask] = rng.integers(1, 4, size=signal_mask.sum())
        s = self.mod.channel_snr_proxy([torch.from_numpy(v)])
        self.assertEqual(len(s['perVolume']), 1)
        self.assertGreater(s['perVolume'][0]['p50'], 0.0,
                           'nonzero-median must be > 0 when signal pixels exist')

    def test_photon_starved_channel_fails_threshold(self):
        rng = np.random.default_rng(0)
        v = rng.integers(0, 10, size=(8, 32, 32)).astype(np.float32)
        s = self.mod.channel_snr_proxy([torch.from_numpy(v)])
        self.assertLess(s['worstSnr'], self.mod.SNR_FAIL_THRESHOLD,
                        f'photon-starved channel scored worstSnr={s["worstSnr"]:.2f}, expected <3')

    def test_well_illuminated_channel_passes(self):
        rng = np.random.default_rng(0)
        v = rng.integers(20, 200, size=(8, 32, 32)).astype(np.float32)
        s = self.mod.channel_snr_proxy([torch.from_numpy(v)])
        self.assertGreater(s['worstSnr'], self.mod.SNR_FAIL_THRESHOLD,
                           f'well-illuminated channel scored worstSnr={s["worstSnr"]:.2f}, expected >3')

    def test_one_weak_movie_in_a_cohort_of_strong_ones_still_flags(self):
        # The 1-in-20 case: one weak movie in a training set of otherwise-fine ones must fire the
        # auto-switch, because a pooled prior will smooth that movie's weak-channel patches away.
        # `worstSnr` = min across movies, not mean, exactly to catch this.
        rng = np.random.default_rng(0)
        strong = [torch.from_numpy(rng.integers(20, 200, size=(8, 32, 32)).astype(np.float32))
                  for _ in range(19)]
        weak_arr = rng.integers(0, 10, size=(8, 32, 32)).astype(np.float32)
        s = self.mod.channel_snr_proxy(strong + [torch.from_numpy(weak_arr)])
        self.assertEqual(len(s['perVolume']), 20)
        # mean is dragged up by the 19; worst captures the outlier
        self.assertGreater(s['meanSnr'], self.mod.SNR_FAIL_THRESHOLD)
        self.assertLess(s['worstSnr'], self.mod.SNR_FAIL_THRESHOLD)


@unittest.skipUnless(_RUNNER.exists(), '`app/` not vendored (external cecelia install)')
class ResolveModeTest(unittest.TestCase):

    def setUp(self):
        self.mod = _load_runner()

    def _precheck(self, worst_snrs):
        # Minimal shape the resolver needs — perVolume is not consulted here (that's for logging).
        return {i: {'perVolume': [], 'worstSnr': float(s), 'meanSnr': float(s)}
                for i, s in enumerate(worst_snrs)}

    def test_forced_pooled_stays_pooled_even_if_channel_fails(self):
        mode, _ = self.mod._resolve_mode('pooled', self._precheck([1.0, 10.0]), _StubLog())
        self.assertEqual(mode, 'pooled')

    def test_forced_perchannel_stays_perchannel(self):
        mode, _ = self.mod._resolve_mode('perChannel', self._precheck([10.0, 10.0]), _StubLog())
        self.assertEqual(mode, 'perChannel')

    def test_auto_switches_to_perchannel_when_any_channel_fails(self):
        mode, _ = self.mod._resolve_mode('auto', self._precheck([1.0, 10.0]), _StubLog())
        self.assertEqual(mode, 'perChannel')

    def test_auto_stays_pooled_when_all_channels_pass(self):
        mode, _ = self.mod._resolve_mode('auto', self._precheck([10.0, 10.0]), _StubLog())
        self.assertEqual(mode, 'pooled')


if __name__ == '__main__':
    unittest.main()
