"""Halo mode: a second label store grown from the final base labels, same ids.

The base keeps its outline (nuclear stain, shape); the halo collects a membrane marker around it, and
Measure pairs the two by id (`halo_mean_intensity_N`). A ring by default (`haloSize`), the whole grown
object on request (`haloWholeCell`).

Run with `pixi run test-py`.
"""
import os
import tempfile
import unittest

import numpy as np
import ome_types
import zarr
from skimage.segmentation import expand_labels

from cecelia.utils.dim_utils import DimUtils
from cecelia.utils.segmentation_utils import SegmentationUtils
from cecelia.tests.test_segmentation_streaming import _ome_xml


class _TwoBlobSeg(SegmentationUtils):
    """Two small separated squares per tile, so there is background to grow into."""

    def predict_slice(self, tile, model_params, norm_params=None):
        masks = np.zeros(tile.shape[-2:], dtype=np.uint32)
        masks[4:8, 4:8] = 1
        masks[14:18, 10:14] = 2
        return masks


def _seg(params=None):
    p = {'taskDir': tempfile.gettempdir(), 'haloSize': 1.0}   # 0.5 µm px → 2 px
    p.update(params or {})
    du = DimUtils(ome_types.from_xml(_ome_xml(1, 1, 1, 24, 20)), use_channel_axis=True)
    return _TwoBlobSeg(p, du)


class HaloLabelsTest(unittest.TestCase):
    base = np.zeros((24, 20), dtype=np.uint32)
    base[4:8, 4:8] = 1
    base[14:18, 10:14] = 2

    def test_ring_is_grown_minus_base(self):
        seg = _seg()
        self.assertEqual(seg.halo_size, 2)
        ring = seg.halo_labels(self.base)
        want = expand_labels(self.base, 2)
        want[self.base > 0] = 0
        np.testing.assert_array_equal(ring, want)
        self.assertFalse((ring[self.base > 0]).any(), 'a ring must not cover its own object')
        self.assertEqual(ring.dtype, self.base.dtype)

    def test_whole_cell_is_the_grown_object(self):
        whole = _seg({'haloWholeCell': True}).halo_labels(self.base)
        np.testing.assert_array_equal(whole, expand_labels(self.base, 2))

    def test_halo_keeps_the_ids(self):
        ring = _seg().halo_labels(self.base)
        self.assertEqual(set(np.unique(ring)) - {0}, {1, 2})


class HaloStoreTest(unittest.TestCase):
    """Through `predict_from_zarr`: the store is written beside the base one, ids match per frame."""

    def _run(self, tmp, **extra):
        sizes, arr_shape = (2, 1, 2, 24, 20), (2, 2, 24, 20)
        du = DimUtils(ome_types.from_xml(_ome_xml(*sizes)), use_channel_axis=True)
        du.calc_image_dimensions(arr_shape)
        im0 = np.random.default_rng(0).integers(0, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        params = {
            'taskDir': tmp, 'outputValueName': 'h',
            'blockSize': 64, 'overlap': 4,           # one tile: no seams to reason about
            'labelOverlap': 0.0, 'matchThreshold': 0.1, 'removeUnmatched': False,
            'normaliseToWhole': False,
            'models': {'0': {'matchAs': 'base', 'cellChannels': [0]}},
        }
        params.update(extra)
        counts = _TwoBlobSeg(params, du).predict_from_zarr([im0])
        labels = os.path.join(tmp, 'labels')
        read = lambda name: zarr.open_group(os.path.join(labels, name), mode='r')['0'][:]
        return counts, read, sorted(os.listdir(labels))

    def test_halo_store_is_written_with_the_base_ids(self):
        with tempfile.TemporaryDirectory() as d:
            counts, read, files = self._run(d, haloSize=1.0)
            self.assertIn('h_halo.zarr', files)
            base, halo = read('h.zarr'), read('h_halo.zarr')
            self.assertEqual(base.shape, halo.shape)
            for t in range(base.shape[0]):
                self.assertEqual(set(np.unique(halo[t])) - {0}, set(np.unique(base[t])) - {0})
                self.assertFalse(halo[t][base[t] > 0].any())
            self.assertEqual(counts['halo'], counts['base'])

    def test_whole_cell_halo_contains_the_base(self):
        with tempfile.TemporaryDirectory() as d:
            _, read, _ = self._run(d, haloSize=1.0, haloWholeCell=True)
            base, halo = read('h.zarr'), read('h_halo.zarr')
            np.testing.assert_array_equal(halo[base > 0], base[base > 0])
            self.assertGreater((halo > 0).sum(), (base > 0).sum())

    def test_no_halo_store_when_off(self):
        with tempfile.TemporaryDirectory() as d:
            counts, _, files = self._run(d)
            self.assertEqual(files, ['h.zarr'])
            self.assertNotIn('halo', counts)


if __name__ == '__main__':
    unittest.main()
