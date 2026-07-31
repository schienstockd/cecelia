"""Unit tests for `cecelia.utils.block_transfer` — moving one mask block between processes.

This is the transport the task preview replaced its scratch zarr store with, so the properties that
matter are the ones a store gave for free and now have to be asserted: the bytes survive exactly
(including dtype and byte order), the block lands at the right place in the full label extent, and
building that full extent stays LAZY — a preview of one plane must never materialise the volume.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import unittest

import numpy as np

from cecelia.utils import block_transfer as bt

AXES = ('T', 'Z', 'Y', 'X')
FULL = (201, 21, 544, 548)


def _mask(h=40, w=50, cells=7, dtype=np.uint32):
    """A block shaped like a real preview result: a length-1 T and Z, background plus a few labels."""
    m = np.zeros((1, 1, h, w), dtype=dtype)
    for i in range(1, cells + 1):
        m[0, 0, i * 3:i * 3 + 2, i * 4:i * 4 + 3] = i
    return m


class CodecTest(unittest.TestCase):
    def test_roundtrip_is_exact(self):
        m = _mask()
        out = bt.decode_block(bt.encode_block(m))
        self.assertTrue(np.array_equal(out, m))
        self.assertEqual(out.dtype, m.dtype)
        self.assertEqual(out.shape, m.shape)

    def test_decoded_block_is_writable(self):
        """`np.frombuffer` over bytes is read-only, which fails only later at the assignment."""
        out = bt.decode_block(bt.encode_block(_mask()))
        self.assertTrue(out.flags.writeable)
        out[0, 0, 0, 0] = 5          # must not raise

    def test_byte_order_survives(self):
        """dtype carries endianness — a big-endian producer must not decode to garbage."""
        m = _mask(dtype=np.dtype('>u4'))
        out = bt.decode_block(bt.encode_block(m))
        self.assertTrue(np.array_equal(out, m))
        self.assertEqual(out.dtype.byteorder, m.dtype.byteorder)

    def test_payload_is_json_safe(self):
        import json
        p = bt.encode_block(_mask())
        self.assertTrue(np.array_equal(bt.decode_block(json.loads(json.dumps(p))), _mask()))

    def test_a_label_plane_compresses(self):
        """The reason a whole block can go over the WS protocol at all. Not a tuned threshold — 5× is
        far below the ~21× measured on a realistic mask, and only fails if compression broke."""
        m = _mask(h=590, w=590, cells=200)
        payload = bt.encode_block(m)
        self.assertLess(len(payload['data']), m.nbytes / 5)

    def test_a_truncated_payload_raises_instead_of_reshaping(self):
        p = bt.encode_block(_mask())
        p['shape'] = [1, 1, 40, 49]              # one column short of the real data
        with self.assertRaises(ValueError):
            bt.decode_block(p)

    def test_an_absurd_shape_is_refused_before_allocating(self):
        p = bt.encode_block(_mask())
        p['shape'] = [10 ** 6, 10 ** 6]
        with self.assertRaises(ValueError):
            bt.decode_block(p)


class PlaceBlockTest(unittest.TestCase):
    def _region(self, t=44, z=9, y=100, x=50, h=40, w=50):
        return {'T': [t, t + 1], 'Z': [z, z + 1], 'Y': [y, y + h], 'X': [x, x + w]}

    def test_block_lands_at_the_region_and_nowhere_else(self):
        m = _mask()
        out = bt.place_block_lazy(m, FULL, AXES, self._region())
        self.assertEqual(tuple(out.shape), FULL)
        self.assertEqual(out.dtype, m.dtype)
        placed = out[44, 9, 100:140, 50:100].compute()
        self.assertTrue(np.array_equal(placed, m[0, 0]))
        self.assertEqual(out[44, 9, :, :].compute().sum(), m.sum())   # nothing outside the block
        self.assertEqual(out[43, 9, :, :].compute().sum(), 0)         # neighbouring plane empty
        self.assertEqual(out[44, 8, :, :].compute().sum(), 0)

    def test_t_and_z_are_chunked_to_single_planes(self):
        """The property that keeps a preview cheap. A chunk is the atomic unit of computation, so a
        chunk spanning T/Z would materialise the whole volume to draw one plane — 4.8 GB here."""
        out = bt.place_block_lazy(_mask(), FULL, AXES, self._region())
        self.assertEqual(out.chunks[0], (1,) * FULL[0])
        self.assertEqual(out.chunks[1], (1,) * FULL[1])
        largest = int(np.prod([max(cs) for cs in out.chunks]))    # biggest single chunk, in elements
        self.assertLess(largest * out.dtype.itemsize, 8 * 1024 * 1024)

    def test_the_full_extent_is_never_materialised(self):
        """A 4.8 GB nominal array must cost the block's bytes, not the extent's."""
        out = bt.place_block_lazy(_mask(), FULL, AXES, self._region())
        nominal = np.prod(FULL) * 4
        self.assertGreater(nominal, 4e9)                       # the shape really is that big
        self.assertLess(len(out.dask), 50_000)                 # graph stays small (measured ~8.4k)

    def test_an_axis_outside_the_region_must_be_covered_in_full(self):
        """Guards the silent-misplacement case: a block that doesn't span an unspecified axis would
        otherwise be pinned at 0 along it and look like a correct result on the wrong plane."""
        region = self._region()
        del region['Z']
        with self.assertRaises(ValueError):
            bt.place_block_lazy(_mask(), FULL, AXES, region)

    def test_a_block_running_past_the_extent_raises(self):
        region = self._region(x=530)                    # 530 + 50 > 548
        with self.assertRaises(ValueError):
            bt.place_block_lazy(_mask(), FULL, AXES, region)

    def test_works_without_a_z_axis(self):
        """A 2D timelapse's label extent has no Z at all."""
        axes, full = ('T', 'Y', 'X'), (10, 544, 548)
        m = _mask()[:, 0]                               # (1, 40, 50)
        out = bt.place_block_lazy(m, full, axes, {'T': [3, 4], 'Y': [0, 40], 'X': [0, 50]})
        self.assertEqual(tuple(out.shape), full)
        self.assertTrue(np.array_equal(out[3, 0:40, 0:50].compute(), m[0]))
        self.assertEqual(out[4].compute().sum(), 0)


if __name__ == '__main__':
    unittest.main()
