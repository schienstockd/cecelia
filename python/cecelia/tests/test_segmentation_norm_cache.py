"""Segmentation's global normalisation range, cached — `_compute_norm_params` + `norm_cache`.

The statistic is a property of the IMAGE and the percentile, not of the run: scale-to-whole exists so
that a tile's normalisation cannot depend on which tile it is, which is exactly what makes it
recomputable-from-scratch-every-time and therefore worth keeping. Measured on `zolIMa/fXgbTl`
(31 × 4 × 32 × 420 × 441, single level, so the streaming path) it is 4.65 s per channel — of a
608 s `segment.coastalMeasure` run, and paid again by every rerun that changes a threshold.

Two model groups over the same channel used to pay for it twice inside ONE run. They now share.

What needs pinning is not the speed, it is the two ways a cached range could be the WRONG range:

  * the two derivation paths disagree. A pyramided store takes the percentile from its lowest-res
    level; a single-level store streams a histogram over full resolution. Same question, different
    answers — and WHICH one runs depends on how many levels the caller opened, so no fingerprint of
    the store can catch it. Hence `variant`.
  * the preview subsamples time (`max_frames`) and the run does not. A preview must not leave its
    approximation behind for a real run to pick up.
"""
import json
import os
import tempfile
import unittest

import numpy as np

from cecelia.utils import norm_cache
from cecelia.utils.segmentation_utils import SegmentationUtils


class _Dims:
    """Only what `SegmentationUtils.__init__` and `_compute_norm_params` ask for."""

    def __init__(self, order='TCZYX', shape=(3, 2, 4, 20, 16)):
        self.im_dim_order = list(order)
        self._vals = dict(zip(self.im_dim_order, shape))

    def dim_idx(self, ax):
        return self.im_dim_order.index(ax) if ax in self.im_dim_order else None

    def dim_val(self, ax):
        return self._vals.get(ax)

    def is_timeseries(self):
        return 'T' in self.im_dim_order

    def im_physical_size(self, _ax, default=1.0):
        return default

    def im_physical_unit(self, _ax):
        return 'µm'


def _store_dir(tmp, name='ccidSmoothed.ome.zarr'):
    """A directory the fingerprint can read. The pixels are passed in as arrays — this only has to
    carry level-0 metadata, which is what identifies the store."""
    path = os.path.join(tmp, name)
    os.makedirs(os.path.join(path, '0'), exist_ok=True)
    with open(os.path.join(path, '0', '.zarray'), 'w', encoding='utf-8') as fh:
        json.dump({'shape': [3, 2, 4, 20, 16], 'dtype': '<u2'}, fh)
    return path


def _pixels(shape=(3, 2, 4, 20, 16), seed=7):
    """Two channels with clearly different ranges, and plenty of background zeros — the zeros matter,
    because this path drops them and the training path does not."""
    rng = np.random.default_rng(seed)
    arr = rng.integers(0, 60, size=shape).astype(np.uint16)
    arr[:, 1] = rng.integers(0, 300, size=arr[:, 1].shape)
    arr[arr < 12] = 0
    return arr


class NormParamsCacheTest(unittest.TestCase):
    MODEL = {'cellChannels': [0, 1], 'normalise': 99.9}

    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.path = _store_dir(self._tmp.name)
        self.pixels = _pixels()
        self.dims = _Dims()
        self.seg = SegmentationUtils(
            {'imPath': self.path, 'taskDir': self._tmp.name}, self.dims)

    def tearDown(self):
        self._tmp.cleanup()

    def _call(self, model=None, **kw):
        return self.seg._compute_norm_params([self.pixels], model or self.MODEL, **kw)

    def test_the_second_call_returns_the_same_ranges(self):
        first = self._call()
        self.assertTrue(first, 'the fixture produced no ranges at all')
        self.assertEqual(self._call(), first)

    def test_the_second_call_reads_them_rather_than_recomputing(self):
        """The point of the change. Planting a different range in the file and getting it back is the
        only way to prove the second call did not just redo the work and agree by luck."""
        self._call()
        doc_path = norm_cache.path_for(self.path)
        with open(doc_path, encoding='utf-8') as fh:
            doc = json.load(fh)
        doc['entries'] = {k: [1.0, 999.0] for k in doc['entries']}
        with open(doc_path, 'w', encoding='utf-8') as fh:
            json.dump(doc, fh)
        self.assertEqual(self._call(), {0: (1.0, 999.0), 1: (1.0, 999.0)})

    def test_two_groups_over_the_same_channel_compute_it_once(self):
        """His `flowTom` shape: two model groups, one channel. The second used to repeat the whole
        streaming read."""
        self._call({'cellChannels': [1], 'normalise': 99.9})
        with open(norm_cache.path_for(self.path), encoding='utf-8') as fh:
            entries = json.load(fh)['entries']
        entries.update({k: [2.0, 42.0] for k in entries})
        with open(norm_cache.path_for(self.path), 'w', encoding='utf-8') as fh:
            json.dump({'version': norm_cache.VERSION,
                       'fingerprint': norm_cache.fingerprint(
                           self.path, self.pixels.shape, self.pixels.dtype),
                       'entries': entries}, fh)
        self.assertEqual(self._call({'cellChannels': [1], 'normalise': 99.9}), {1: (2.0, 42.0)})

    def test_a_different_percentile_is_not_reused(self):
        a = self._call({'cellChannels': [1], 'normalise': 99.9})
        b = self._call({'cellChannels': [1], 'normalise': 99.0})
        self.assertNotEqual(a, b, 'two percentiles must not share one cached range')

    def test_the_pyramid_and_streaming_paths_do_not_share_a_key(self):
        """The trap a fingerprint cannot catch: which path runs depends on how many levels the CALLER
        opened, not on anything about the store. A lowest-res proxy served to a call that would have
        streamed full resolution is a silently different photometric range."""
        # A real lowest-res level, big enough to clear the `> 100` sample guard — otherwise the
        # pyramid path returns nothing and this test passes for the wrong reason.
        low = self.pixels[:, :, :, ::2, ::2]
        streamed = self._call()
        pyramided = self.seg._compute_norm_params([self.pixels, low], self.MODEL)
        with open(norm_cache.path_for(self.path), encoding='utf-8') as fh:
            keys = set(json.load(fh)['entries'])
        self.assertEqual(len(keys), 4, 'two channels x two derivation paths, kept apart')
        self.assertTrue(any('pyr' in k for k in keys))
        self.assertTrue(any('stream' in k for k in keys))
        # and the numbers really are different, which is why they must not share
        self.assertNotEqual(streamed, pyramided)

    def test_a_previews_subsampled_range_is_not_left_for_the_run(self):
        """`max_frames` trades temporal coverage for latency and only the preview passes it. If it
        shared a key, opening the preview once would change what the next real run normalises by."""
        self._call(max_frames=1)
        with open(norm_cache.path_for(self.path), encoding='utf-8') as fh:
            keys = set(json.load(fh)['entries'])
        self.assertTrue(all('stream1' in k for k in keys), keys)
        self.assertFalse(any(k.endswith('/stream') for k in keys))

    def test_an_image_with_no_path_still_computes(self):
        """`imPath` is how the cache is addressed; without it there is nothing to key on, and the
        answer must still be the same answer."""
        seg = SegmentationUtils({'taskDir': self._tmp.name}, self.dims)
        self.assertEqual(seg._compute_norm_params([self.pixels], self.MODEL), self._call())

    def test_a_channel_too_empty_to_have_a_range_is_retried_not_remembered(self):
        """The `> 100` guards mean some channels legitimately produce nothing. Caching that absence
        would make it permanent for an image that gets re-imported with real data at the same path."""
        empty = np.zeros_like(self.pixels)
        self.assertEqual(self.seg._compute_norm_params([empty], {'cellChannels': [0]}), {})
        self.assertFalse(os.path.exists(norm_cache.path_for(self.path)),
                         'nothing was derived, so nothing should have been written')

    def test_the_cached_ranges_match_what_an_uncached_run_computes(self):
        """The whole thing is only worth having if it is the same statistic."""
        want = SegmentationUtils({'taskDir': self._tmp.name}, self.dims)._compute_norm_params(
            [self.pixels], self.MODEL)
        self.assertEqual(self._call(), want)
        self.assertEqual(self._call(), want)


if __name__ == '__main__':
    unittest.main()
