"""The per-image normalisation-percentile cache — `python/cecelia/utils/norm_cache.py`.

What is worth pinning here is not the round trip; it is every way the cache must REFUSE to answer.
A wrong `(lo, hi)` does not fail — it trains a model at the wrong photometric range, or segments an
image at one, and nothing downstream says so. So the tests are mostly about misses: a rewritten
store, a changed setting, a file from a future version, a half-edited entry.

The design decision they enforce: staleness is handled by the KEY and the FINGERPRINT, never by a
warning. Every input that changes the number makes the lookup miss, so there is no state in which a
user has to be told their cache is out of date.
"""
import json
import os
import tempfile
import unittest

from cecelia.utils import norm_cache


def _store(tmp, shape=(4, 2, 8, 6), name='ccidSmoothed.ome.zarr'):
    """A directory that looks enough like a zarr store to be fingerprinted: level-0 metadata only."""
    path = os.path.join(tmp, name)
    os.makedirs(os.path.join(path, '0'), exist_ok=True)
    with open(os.path.join(path, '0', '.zarray'), 'w', encoding='utf-8') as fh:
        json.dump({'shape': list(shape), 'dtype': '<u2'}, fh)
    return path


class PathTest(unittest.TestCase):
    def test_the_sidecar_sits_beside_the_store_not_inside_it(self):
        """Inside would make a consumer a writer of another task's store. Outside is the fingerprint's
        whole reason for existing."""
        with tempfile.TemporaryDirectory() as tmp:
            p = _store(tmp)
            side = norm_cache.path_for(p)
            self.assertEqual(os.path.dirname(side), os.path.dirname(p))
            self.assertFalse(side.startswith(p + os.sep),
                             'the cache file must not live inside the zarr store')

    def test_the_name_is_not_a_json_sidecar(self):
        """THE ratchet on the name. Sidecar discovery in several places is `readdir` +
        `endswith(".json")`, so a file that is not a population/QC/stats sidecar must not end in it —
        the same reasoning as `write_atomic`'s temp suffix."""
        with tempfile.TemporaryDirectory() as tmp:
            self.assertFalse(norm_cache.path_for(_store(tmp)).endswith('.json'))


class KeyTest(unittest.TestCase):
    def test_every_setting_that_changes_the_number_changes_the_key(self):
        base = norm_cache.key(2, 20, 99.99)
        self.assertNotEqual(base, norm_cache.key(1, 20, 99.99), 'channel')
        self.assertNotEqual(base, norm_cache.key(2, 21, 99.99), 'plane')
        self.assertNotEqual(base, norm_cache.key(2, 20, 99.9), 'percentile')
        self.assertNotEqual(base, norm_cache.key(2, 20, 99.99, norm_cache.ZEROS_EXCLUDED),
                            'zero-handling policy')

    def test_a_flat_movie_is_not_plane_zero(self):
        """`None` means "no Z axis", which is a different statistic from the first plane of a stack —
        and they would otherwise share a key on any 2D/3D mix-up."""
        self.assertNotEqual(norm_cache.key(0, None, 99.9), norm_cache.key(0, 0, 99.9))

    def test_the_percentile_is_not_truncated_into_the_key(self):
        """99.9 and 99.99 differ by an order of magnitude in the tail they cut, and `%g` must keep
        both — a key that rounded would serve one run's range to the other."""
        self.assertNotEqual(norm_cache.key(0, 0, 99.9), norm_cache.key(0, 0, 99.99))


class RoundTripTest(unittest.TestCase):
    def test_entries_survive_a_write_and_read(self):
        with tempfile.TemporaryDirectory() as tmp:
            p = _store(tmp)
            fp = norm_cache.fingerprint(p, (4, 2, 8, 6), 'uint16')
            k = norm_cache.key(2, 20, 99.99)
            self.assertTrue(norm_cache.write(p, fp, {k: (0.0, 351.0)}))
            self.assertEqual(norm_cache.read(p, fp), {k: (0.0, 351.0)})

    def test_an_absent_file_is_a_miss_not_an_error(self):
        with tempfile.TemporaryDirectory() as tmp:
            p = _store(tmp)
            self.assertEqual(norm_cache.read(p, norm_cache.fingerprint(p, (4,), 'uint16')), {})

    def test_nothing_is_written_for_an_empty_set(self):
        """An empty file would be indistinguishable from a store whose ranges are all zero."""
        with tempfile.TemporaryDirectory() as tmp:
            p = _store(tmp)
            fp = norm_cache.fingerprint(p, (4,), 'uint16')
            self.assertFalse(norm_cache.write(p, fp, {}))
            self.assertFalse(os.path.exists(norm_cache.path_for(p)))


class StalenessTest(unittest.TestCase):
    """The cases that must MISS. Each one is a way a cached range could be wrong for the pixels."""

    def setUp(self):
        self._tmp = tempfile.TemporaryDirectory()
        self.path = _store(self._tmp.name)
        self.shape = (4, 2, 8, 6)
        self.fp = norm_cache.fingerprint(self.path, self.shape, 'uint16')
        self.key = norm_cache.key(2, 20, 99.99)
        norm_cache.write(self.path, self.fp, {self.key: (0.0, 351.0)})

    def tearDown(self):
        self._tmp.cleanup()

    def test_a_rewritten_store_drops_the_whole_file(self):
        """The case a path-only key gets silently wrong: re-running the smoothing that produced the
        source writes a new store to the SAME path, at the same shape, with different pixels."""
        meta = os.path.join(self.path, '0', '.zarray')
        os.utime(meta, ns=(0, 1_700_000_000_000_000_000))
        fresh = norm_cache.fingerprint(self.path, self.shape, 'uint16')
        self.assertNotEqual(fresh, self.fp, 'the mtime must reach the fingerprint')
        self.assertEqual(norm_cache.read(self.path, fresh), {})

    def test_a_different_shape_is_a_miss(self):
        self.assertEqual(norm_cache.read(self.path, norm_cache.fingerprint(
            self.path, (8, 2, 8, 6), 'uint16')), {})

    def test_a_different_dtype_is_a_miss(self):
        self.assertEqual(norm_cache.read(self.path, norm_cache.fingerprint(
            self.path, self.shape, 'uint8')), {})

    def test_a_store_with_no_metadata_is_uncacheable(self):
        """`None` never matches, so an unrecognisable store recomputes rather than being assumed
        unchanged — the safe direction."""
        with tempfile.TemporaryDirectory() as tmp:
            bare = os.path.join(tmp, 'x.ome.zarr')
            os.makedirs(bare)
            self.assertIsNone(norm_cache.fingerprint(bare, (4,), 'uint16'))
            self.assertEqual(norm_cache.read(bare, None), {})
            self.assertFalse(norm_cache.write(bare, None, {'k': (0.0, 1.0)}))

    def test_a_file_from_another_version_is_ignored(self):
        with open(norm_cache.path_for(self.path), encoding='utf-8') as fh:
            doc = json.load(fh)
        doc['version'] = norm_cache.VERSION + 1
        with open(norm_cache.path_for(self.path), 'w', encoding='utf-8') as fh:
            json.dump(doc, fh)
        self.assertEqual(norm_cache.read(self.path, self.fp), {})

    def test_malformed_json_is_a_miss(self):
        with open(norm_cache.path_for(self.path), 'w', encoding='utf-8') as fh:
            fh.write('{not json')
        self.assertEqual(norm_cache.read(self.path, self.fp), {})

    def test_one_broken_entry_does_not_cost_the_others(self):
        """A hand-edited or truncated pair must be skipped, not passed on as a string or a None to
        fail deep inside the normalisation arithmetic."""
        with open(norm_cache.path_for(self.path), encoding='utf-8') as fh:
            doc = json.load(fh)
        doc['entries'] = {self.key: [0.0, 351.0], 'bad/a': ['x', 2],
                          'bad/b': [1.0], 'bad/c': None}
        with open(norm_cache.path_for(self.path), 'w', encoding='utf-8') as fh:
            json.dump(doc, fh)
        self.assertEqual(norm_cache.read(self.path, self.fp), {self.key: (0.0, 351.0)})

    def test_a_non_finite_range_is_dropped(self):
        """`inf`/`nan` would propagate silently through `(arr - lo) / (hi - lo)` and blank the frame
        rather than raise."""
        with open(norm_cache.path_for(self.path), encoding='utf-8') as fh:
            doc = json.load(fh)
        doc['entries'] = {'a/1': [0.0, float('inf')], 'a/2': [float('nan'), 1.0]}
        with open(norm_cache.path_for(self.path), 'w', encoding='utf-8') as fh:
            json.dump(doc, fh)
        self.assertEqual(norm_cache.read(self.path, self.fp), {})


class UnwritableTest(unittest.TestCase):
    def test_a_read_only_location_costs_a_recompute_not_the_run(self):
        """Source images legitimately sit on read-only mounts. A run that has already done the work
        must not fail at the point of saving a convenience."""
        with tempfile.TemporaryDirectory() as tmp:
            p = _store(tmp)
            fp = norm_cache.fingerprint(p, (4,), 'uint16')
            os.chmod(tmp, 0o500)
            try:
                self.assertFalse(norm_cache.write(p, fp, {'k': (0.0, 1.0)}))
            finally:
                os.chmod(tmp, 0o700)


if __name__ == '__main__':
    unittest.main()
