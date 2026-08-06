"""Zarr v2 and v3 (OME-NGFF 0.4 and 0.5) read the same way.

Why this needs pinning: NGFF 0.5 moved every attribute one level down, under `ome`. Nothing errors
when a reader misses that — `read_axes`/`read_scale` simply return None, and downstream that becomes
"1 µm, 1 second per frame" (CLAUDE.md → *Calibration — three copies, one stamp*). A v3 store would
look like it worked and produce wrong physical numbers, which is the failure mode these tests exist
to make impossible. See docs/todo/ZARR_V3_PLAN.md.

Stores are built in-process with zarr-python (no bioformats2raw needed), shaped to match what
bioformats2raw 0.12.1 actually writes — verified against a real conversion.
"""
import json
import os
import shutil
import tempfile
import unittest

import numpy as np
import zarr

from cecelia.utils import zarr_utils


def _write_store(root, zarr_format, nest_under_ome):
    """A bioformats2raw-shaped series store: `root/0` group carrying multiscales, level-0 array at `0/0`."""
    os.makedirs(root, exist_ok=True)
    g = zarr.open_group(os.path.join(root, '0'), mode='w', zarr_format=zarr_format)
    a = g.create_array('0', shape=(2, 1, 1, 8, 8), dtype='uint16', chunks=(1, 1, 1, 8, 8))
    a[:] = np.arange(2 * 8 * 8, dtype='uint16').reshape(2, 1, 1, 8, 8)
    ms = [{
        'axes': [{'name': 't', 'type': 'time', 'unit': 'second'},
                 {'name': 'c', 'type': 'channel'},
                 {'name': 'z', 'type': 'space', 'unit': 'micrometer'},
                 {'name': 'y', 'type': 'space', 'unit': 'micrometer'},
                 {'name': 'x', 'type': 'space', 'unit': 'micrometer'}],
        'datasets': [{'path': '0',
                      'coordinateTransformations': [{'type': 'scale', 'scale': [30.0, 1.0, 3.0, 0.5, 0.5]}]}],
    }]
    g.attrs.update({'ome': {'version': '0.5', 'multiscales': ms}} if nest_under_ome
                   else {'multiscales': ms})
    return root


class NgffAttrsTest(unittest.TestCase):
    def test_unwraps_the_ome_nesting(self):
        self.assertEqual({'multiscales': [1]}, zarr_utils.ngff_attrs({'ome': {'multiscales': [1]}}))

    def test_passes_a_flat_0_4_dict_through(self):
        self.assertEqual({'multiscales': [1]}, zarr_utils.ngff_attrs({'multiscales': [1]}))

    def test_a_non_dict_ome_value_is_not_treated_as_the_wrapper(self):
        # a store that happens to carry a scalar `ome` key must not lose its top-level attrs
        self.assertEqual({'ome': '0.5', 'multiscales': [1]},
                         zarr_utils.ngff_attrs({'ome': '0.5', 'multiscales': [1]}))


class ReadBothFormatsTest(unittest.TestCase):
    """The same logical store, written as 0.4/v2 and 0.5/v3, must read IDENTICALLY."""

    @classmethod
    def setUpClass(cls):
        cls.tmp = tempfile.mkdtemp()
        cls.v2 = _write_store(os.path.join(cls.tmp, 'v2.ome.zarr'), 2, nest_under_ome=False)
        cls.v3 = _write_store(os.path.join(cls.tmp, 'v3.ome.zarr'), 3, nest_under_ome=True)

    @classmethod
    def tearDownClass(cls):
        shutil.rmtree(cls.tmp, ignore_errors=True)

    def test_store_format_is_discovered(self):
        self.assertEqual(2, zarr_utils.store_format(self.v2))
        self.assertEqual(3, zarr_utils.store_format(self.v3))
        self.assertIsNone(zarr_utils.store_format(os.path.join(self.tmp, 'nope')))

    def test_series_base_steps_into_the_wrapper_for_both(self):
        for p in (self.v2, self.v3):
            self.assertEqual(os.path.join(p, '0'), zarr_utils.series_base(p))

    def test_read_axes_agrees(self):
        self.assertEqual(['t', 'c', 'z', 'y', 'x'], zarr_utils.read_axes(self.v2))
        self.assertEqual(zarr_utils.read_axes(self.v2), zarr_utils.read_axes(self.v3))

    def test_read_scale_agrees(self):
        self.assertEqual([30.0, 1.0, 3.0, 0.5, 0.5], list(zarr_utils.read_scale(self.v2)))
        self.assertEqual(list(zarr_utils.read_scale(self.v2)), list(zarr_utils.read_scale(self.v3)))

    def test_read_time_increment_agrees(self):
        # the t axis carries an explicit `second` unit, so it is a reading, not a placeholder
        self.assertEqual(30.0, zarr_utils.read_time_increment(self.v2))
        self.assertEqual(30.0, zarr_utils.read_time_increment(self.v3))

    def test_read_axis_units_agrees(self):
        self.assertEqual(zarr_utils.read_axis_units(self.v2), zarr_utils.read_axis_units(self.v3))

    def test_open_as_zarr_returns_the_pyramid_not_the_group(self):
        # the v3 regression: `multiscales` under `ome` was missed, so this fell through to the
        # bare-array branch and raised `'GroupInfo' object has no attribute 'obj'`
        for p in (self.v2, self.v3):
            levels, _ = zarr_utils.open_as_zarr(p)
            self.assertEqual(1, len(levels), p)
            self.assertEqual((2, 1, 1, 8, 8), levels[0].shape, p)

    def test_pixels_are_identical_across_formats(self):
        v2 = np.asarray(zarr_utils.open_as_zarr(self.v2)[0][0][:])
        v3 = np.asarray(zarr_utils.open_as_zarr(self.v3)[0][0][:])
        np.testing.assert_array_equal(v2, v3)


if __name__ == '__main__':
    unittest.main()
