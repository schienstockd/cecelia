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


if __name__ == '__main__':
    unittest.main()
