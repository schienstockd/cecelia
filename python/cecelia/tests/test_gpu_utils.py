"""Tests for the shared torch GPU-detection + VRAM pre-flight helpers."""

import unittest
from unittest import mock

import torch

from cecelia.utils.gpu_utils import torch_device, free_vram_gb, require_free_vram_gb


class TorchDeviceTest(unittest.TestCase):
    def test_cuda_preferred(self):
        with mock.patch.object(torch.cuda, 'is_available', return_value=True):
            use_gpu, device = torch_device()
        self.assertTrue(use_gpu)
        self.assertEqual(device.type, 'cuda')

    def test_mps_fallback_when_no_cuda(self):
        with mock.patch.object(torch.cuda, 'is_available', return_value=False), \
             mock.patch.object(torch.backends.mps, 'is_available', return_value=True):
            use_gpu, device = torch_device()
        self.assertTrue(use_gpu)
        self.assertEqual(device.type, 'mps')

    def test_cpu_fallback(self):
        with mock.patch.object(torch.cuda, 'is_available', return_value=False), \
             mock.patch.object(torch.backends.mps, 'is_available', return_value=False):
            use_gpu, device = torch_device()
        self.assertFalse(use_gpu)
        self.assertIsNone(device)


class FreeVramGbTest(unittest.TestCase):
    def test_none_on_non_cuda(self):
        """MPS + CPU have no queryable free/total pair — helper says so instead of guessing."""
        with mock.patch.object(torch.cuda, 'is_available', return_value=False), \
             mock.patch.object(torch.backends.mps, 'is_available', return_value=True):
            self.assertIsNone(free_vram_gb())
        with mock.patch.object(torch.cuda, 'is_available', return_value=False), \
             mock.patch.object(torch.backends.mps, 'is_available', return_value=False):
            self.assertIsNone(free_vram_gb())

    def test_reports_free_bytes_in_gb_on_cuda(self):
        # mem_get_info returns (free, total) bytes on the CUDA device.
        with mock.patch.object(torch.cuda, 'is_available', return_value=True), \
             mock.patch.object(torch.cuda, 'mem_get_info', return_value=(int(4.2e9), int(8e9))):
            self.assertAlmostEqual(free_vram_gb(), 4.2, places=2)


class RequireFreeVramGbTest(unittest.TestCase):
    """The pre-flight refuses cleanly instead of letting CUDA OOM halfway through training."""

    def _cuda_ctx(self, free_bytes):
        return [
            mock.patch.object(torch.cuda, 'is_available', return_value=True),
            mock.patch.object(torch.cuda, 'mem_get_info', return_value=(int(free_bytes), int(8e9))),
        ]

    def _run_with_mocks(self, free_bytes, **kwargs):
        ctxs = self._cuda_ctx(free_bytes)
        with ctxs[0], ctxs[1]:
            return require_free_vram_gb(**kwargs)

    def test_passes_when_free_meets_minimum(self):
        # 6 GB free, need 5 GB — no raise, no SystemExit
        msgs = []
        self._run_with_mocks(6e9, min_gb=5.0, task_label='Model size "large"',
                             log=mock.MagicMock(log=msgs.append))
        self.assertEqual(msgs, [])   # silent on success — no noise in the log

    def test_raises_system_exit_when_free_below_minimum(self):
        msgs = []
        with self.assertRaises(SystemExit):
            self._run_with_mocks(2e9, min_gb=5.0, task_label='Model size "large"',
                                 log=mock.MagicMock(log=msgs.append))
        # message names the offender + the actionable fix — the whole point of the pre-flight
        joined = '\n'.join(msgs)
        self.assertIn('Model size "large"', joined)
        self.assertIn('2.0 GB free', joined)
        self.assertIn('5.0 GB needed', joined)
        self.assertIn('smaller', joined.lower())

    def test_noop_on_non_cuda(self):
        """MPS/CPU return None from free_vram_gb — the require helper must NOT raise there."""
        with mock.patch.object(torch.cuda, 'is_available', return_value=False), \
             mock.patch.object(torch.backends.mps, 'is_available', return_value=False):
            require_free_vram_gb(min_gb=1000.0, task_label='x')   # would raise if it looked


if __name__ == '__main__':
    unittest.main()
