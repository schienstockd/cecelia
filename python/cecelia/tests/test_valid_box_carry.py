"""`carry_valid_box` — propagating the valid box onto a derived store.

The box only pays off if it SURVIVES the pipeline. Two ways it failed to: dropped silently
(`af_correct`, `cellpose_correct`), and carried through `read_valid_box(path)` — which on a per-frame
box returns the UNION over frames, nearly the whole canvas once the window drifts (`smooth`). Both
left the store people actually segment reporting "all valid".

Part of the Python (analysis-env) test suite — run with `pixi run test-py`.
"""
import shutil
import tempfile
import unittest
from pathlib import Path

import dask.array as da
import numpy as np

import cecelia.utils.zarr_utils as zu


class CarryValidBoxTest(unittest.TestCase):

    def setUp(self):
        self.dir = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, self.dir, ignore_errors=True)

    def _store(self, name, shape, axes):
        p = str(self.dir / name)
        zu.create_multiscales(da.zeros(shape, dtype=np.uint16, chunks=(1,) * (len(shape) - 2) + shape[-2:]),
                              p, axes=list(axes), nscales=1)
        return p

    def _per_frame_src(self, n_t=3):
        src = self._store('src.ome.zarr', (n_t, 2, 8, 6, 6), 'tczyx')
        zu.write_valid_box(src, ['Z', 'Y', 'X'],
                           {t: {'Z': (t, t + 4), 'Y': (0, 6), 'X': (0, 6)} for t in range(n_t)})
        return src

    def test_a_per_frame_box_stays_per_frame(self):
        """The union bug: every frame's own span must survive, not their envelope."""
        src = self._per_frame_src()
        dst = self._store('dst.ome.zarr', (3, 2, 8, 6, 6), 'tczyx')
        self.assertTrue(zu.carry_valid_box(src, dst))
        for t in range(3):
            self.assertEqual(zu.read_valid_box(dst, timepoint=t)['Z'], (t, t + 4))
        # …and the union is genuinely wider, so this test would pass trivially if it were not per-frame
        self.assertEqual(zu.read_valid_box(dst)['Z'], (0, 6))

    def test_carries_onto_a_label_store_that_dropped_the_channel_axis(self):
        """A box on Z/Y/X still describes a store with no C — comparing FULL shapes would refuse."""
        src = self._per_frame_src()
        dst = self._store('labels.ome.zarr', (3, 8, 6, 6), 'tzyx')
        self.assertTrue(zu.carry_valid_box(src, dst))
        self.assertEqual(zu.read_valid_box(dst, timepoint=1)['Z'], (1, 5))

    def test_refuses_a_z_projected_store(self):
        """`segment.branching` flattening in 2D: Z is gone, so a Z box is a lie about this store."""
        src = self._per_frame_src()
        dst = self._store('flat.ome.zarr', (3, 6, 6), 'tyx')
        self.assertFalse(zu.carry_valid_box(src, dst))
        self.assertIsNone(zu.read_valid_box(dst))

    def test_refuses_a_crop(self):
        """A crop moves the coordinates — the parent's box would be precise and wrong."""
        src = self._per_frame_src()
        dst = self._store('crop.ome.zarr', (3, 2, 8, 4, 4), 'tczyx')
        self.assertFalse(zu.carry_valid_box(src, dst))

    def test_refuses_when_time_collapsed_under_a_per_frame_box(self):
        """A per-frame box is keyed by frame index; without those frames the keys mean nothing."""
        src = self._per_frame_src()
        dst = self._store('flat_t.ome.zarr', (2, 8, 6, 6), 'czyx')
        self.assertFalse(zu.carry_valid_box(src, dst))

    def test_a_static_box_carries_too(self):
        src = self._store('s2.ome.zarr', (2, 8, 6, 6), 'czyx')
        zu.write_valid_box(src, ['Z'], {'Z': (2, 5)})
        dst = self._store('d2.ome.zarr', (2, 8, 6, 6), 'czyx')
        self.assertTrue(zu.carry_valid_box(src, dst))
        self.assertEqual(zu.read_valid_box(dst)['Z'], (2, 5))

    def test_a_source_with_no_box_is_a_no_op(self):
        """Most stores never padded; carrying nothing must not invent a box."""
        src = self._store('nobox.ome.zarr', (2, 8, 6, 6), 'czyx')
        dst = self._store('d3.ome.zarr', (2, 8, 6, 6), 'czyx')
        self.assertFalse(zu.carry_valid_box(src, dst))
        self.assertIsNone(zu.read_valid_box(dst))
