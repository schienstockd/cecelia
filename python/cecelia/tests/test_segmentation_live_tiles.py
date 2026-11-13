"""A frame's tiles reach the staging store as they finish, so a live preview fills in tile by tile.

Segmentation writes a frame to its label store once the frame is stitched, so a single-plane image —
a big multiplex scan, one frame — showed the live preview nothing until the run's last second. Each
tile's raw labels now go to the staging store's level 0 as soon as the tile is done
(`SegmentationUtils._stream_tile`), and the frame write then overwrites them; the final store must be
exactly what it was without the streaming.

Part of the Python (analysis-env) test suite — run with `pixi run test-py`.
"""
import os
import tempfile
import unittest
from unittest import mock

import numpy as np
import zarr

from cecelia.utils import zarr_utils
from cecelia.utils.segmentation_utils import SegmentationUtils
from cecelia.tests.test_single_plane_tiling import _dim_utils, _params


class _Watcher(SegmentationUtils):
    """Labels one blob per tile, and on every call records how much of the staging store is filled."""

    def predict_slice(self, tile, model_params, norm_params=None):
        store = os.path.join(self.task_dir, 'labels', 'seg.zarr' + zarr_utils.STAGING_SUFFIX)
        self.filled = getattr(self, 'filled', []) + [
            int((zarr.open_group(store, mode='r')['0'][:] > 0).sum())]
        out = np.zeros(tile.shape[-2:], np.uint32)
        out[2:-2, 2:-2] = 1
        return out


class LiveTileStreamingTest(unittest.TestCase):

    def _run(self, stream=True, t=1):
        du = _dim_utils(t, 1, 2, 36, 40)
        im = np.random.default_rng(0).integers(1000, 4000, size=tuple(du.im_dim), dtype=np.uint16)
        with tempfile.TemporaryDirectory() as tmp:
            seg = _Watcher(_params(tmp), du)
            if stream:
                seg.predict_from_zarr([im])
            else:
                with mock.patch.object(SegmentationUtils, '_stream_tile'):
                    seg.predict_from_zarr([im])
            labels = zarr.open_group(os.path.join(tmp, 'labels', 'seg.zarr'), mode='r')['0'][:]
        return seg.filled, labels

    def test_each_tile_is_on_disk_before_the_next_runs(self):
        filled, _ = self._run()
        self.assertEqual(len(filled), 12, '36x40 at 12 px → 3x4 tiles')
        self.assertEqual(filled[0], 0, 'nothing before the first tile')
        # strictly growing: every finished tile added its labels before the next tile was predicted
        self.assertTrue(all(b > a for a, b in zip(filled, filled[1:])), filled)

    def test_without_streaming_nothing_shows_until_the_frame_is_done(self):
        filled, _ = self._run(stream=False)
        self.assertEqual(set(filled), {0})

    def test_the_final_store_is_unchanged_by_streaming(self):
        _, streamed = self._run()
        _, plain = self._run(stream=False)
        np.testing.assert_array_equal(streamed, plain)

    def test_a_timeseries_too(self):
        _, streamed = self._run(t=2)
        _, plain = self._run(stream=False, t=2)
        np.testing.assert_array_equal(streamed, plain)


if __name__ == '__main__':
    unittest.main()
