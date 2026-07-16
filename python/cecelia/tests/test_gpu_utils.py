"""Tests for the shared torch GPU-detection helper."""

import unittest
from unittest import mock

import torch

from cecelia.utils.gpu_utils import torch_device


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


if __name__ == '__main__':
    unittest.main()
