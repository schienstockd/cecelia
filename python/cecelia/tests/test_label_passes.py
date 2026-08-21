"""Which segmentation PASS produced each label.

A stacked run writes ONE label store, and the merged labels cannot answer "did the cell pass or the
fragment pass find this object" — which is the whole point of running two passes. Object size is not
a substitute: the cell pass can return a small object and the fragment pass a large one.

Recorded as label-id RANGES, because that is what the producer naturally knows: every group's write
takes the next block off one monotonic counter. Nothing per-pixel or per-label is carried through
stitching, smoothing and the size filters — the ids themselves are the record, and those three only
merge into an existing id or delete outright.
"""
import os
import tempfile
import unittest

import numpy as np
import zarr

from cecelia.utils import zarr_utils


def _store(path, shape=(4, 8, 8)):
    g = zarr.open_group(path, mode='w', zarr_format=2)
    g.create_array('0', shape=shape, chunks=shape, dtype='uint32')
    return g


class LabelPassRoundTripTest(unittest.TestCase):

    def test_ranges_survive_a_round_trip(self):
        with tempfile.TemporaryDirectory() as d:
            p = os.path.join(d, 'labels.zarr')
            _store(p)
            entries = [{'group': '0', 'from': 1, 'to': 120},
                       {'group': '1', 'from': 121, 'to': 260}]
            self.assertTrue(zarr_utils.write_label_passes(p, entries))
            self.assertEqual(zarr_utils.read_label_passes(p), entries)

    def test_a_store_with_none_reads_empty_not_one_pass(self):
        """Empty is the honest answer for a single-pass run and for every store written before this
        existed. Claiming 'all one pass' would be a claim the store does not make."""
        with tempfile.TemporaryDirectory() as d:
            p = os.path.join(d, 'labels.zarr')
            _store(p)
            self.assertEqual(zarr_utils.read_label_passes(p), [])

    def test_it_does_not_clobber_a_valid_box(self):
        """Both live under the same `cecelia` attrs namespace."""
        with tempfile.TemporaryDirectory() as d:
            p = os.path.join(d, 'labels.zarr')
            _store(p)
            zarr_utils.write_valid_box(p, ['Z'], {'Z': (1, 3)})
            zarr_utils.write_label_passes(p, [{'group': '0', 'from': 1, 'to': 9}])
            self.assertTrue(zarr_utils.read_label_passes(p))
            self.assertIsNotNone(zarr_utils.read_valid_box(p))

    def test_an_empty_or_degenerate_range_is_not_written(self):
        with tempfile.TemporaryDirectory() as d:
            p = os.path.join(d, 'labels.zarr')
            _store(p)
            self.assertFalse(zarr_utils.write_label_passes(p, []))
            # a group that produced nothing this tile: to < from
            self.assertFalse(zarr_utils.write_label_passes(
                p, [{'group': '0', 'from': 5, 'to': 4}]))


class LabelPassLookupTest(unittest.TestCase):

    def setUp(self):
        self.lookup = zarr_utils.label_pass_lookup([
            {'group': '0', 'from': 1, 'to': 3},
            {'group': '1', 'from': 4, 'to': 6},
            {'group': '0', 'from': 7, 'to': 9},      # groups interleave: tiles loop inside t
        ])

    def test_each_id_resolves_to_the_group_that_wrote_it(self):
        self.assertEqual([self.lookup(i) for i in range(1, 10)],
                         ['0', '0', '0', '1', '1', '1', '0', '0', '0'])

    def test_an_id_no_range_covers_is_unknown_not_guessed(self):
        self.assertIsNone(self.lookup(0))
        self.assertIsNone(self.lookup(10))

    def test_background_is_never_attributed(self):
        self.assertIsNone(self.lookup(0))

    def test_a_gap_between_ranges_is_unknown(self):
        lookup = zarr_utils.label_pass_lookup([{'group': '0', 'from': 1, 'to': 2},
                                               {'group': '1', 'from': 10, 'to': 11}])
        self.assertIsNone(lookup(5))
        self.assertEqual(lookup(11), '1')

    def test_no_entries_attributes_nothing(self):
        lookup = zarr_utils.label_pass_lookup([])
        self.assertIsNone(lookup(1))


if __name__ == '__main__':
    unittest.main()
