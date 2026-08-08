"""The bridge's `set_z_view` — whole z stack as a 3D render, or one z slice in 2D.

Why it is ONE switch covering both the image and the mask layers, rather than a per-layer projection:
napari's `Labels.projection_mode` accepts only `'none'` (`'max'` raises `ValueError`), so a mask cannot
be flattened over z at all. Flattening just the channels with a thick slice would put a projected image
next to a single-plane mask, which answers no question. The volumetric render is the only thing that
shows the whole stack for both.

Driven against a real headless `ViewerModel`, because every property under test is napari's own: that
`ndisplay` flips, that the z axis is found by NAME in `_display_axes` (not by position — a store can be
`[t,z,y,x]` or `[z,y,x]`), and that an out-of-range slice is clamped against `dims.nsteps`. A stub
viewer would assert nothing about any of it.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import importlib.util as iu
import os
import sys
import unittest

import numpy as np

_BRIDGE = os.path.join(os.path.dirname(__file__), '..', '..', '..', 'napari', 'napari_bridge.py')

#: Building a real napari Labels layer SIGSEGVs on the macOS runner — see test_preview_layers.py.
_LABELS_CRASH_ON_MACOS = sys.platform == 'darwin'


def _load_bridge():
    """The bridge module, or None when napari/qtpy is absent. Only ImportError is tolerated: a renamed
    symbol must FAIL rather than silently skip the file."""
    try:
        spec = iu.spec_from_file_location('napari_bridge_z_view', os.path.abspath(_BRIDGE))
        m = iu.module_from_spec(spec)
        spec.loader.exec_module(m)
        return m
    except ImportError:
        return None


class SetZViewTest(unittest.TestCase):
    #: 4 timepoints x 5 z planes; signal only on z=3, so a wrong slice is visible as well as wrong.
    SHAPE = (4, 5, 8, 8)

    def setUp(self):
        self.mod = _load_bridge()
        if self.mod is None:
            self.skipTest('napari not installed')
        from napari.components import ViewerModel
        self.state = self.mod.NapariState.__new__(self.mod.NapariState)
        self.state._viewer = ViewerModel()
        self.state._axes = ['t', 'z', 'y', 'x']
        self.data = np.zeros(self.SHAPE, dtype='float32')
        self.data[:, 3] = 9.0
        self.state._im_data = [self.data]
        self.state._viewer.add_image(self.data, name='im')
        if not _LABELS_CRASH_ON_MACOS:
            self.state._viewer.add_labels((self.data > 0).astype('uint32'), name='(a) Labels')

    def test_slice_selects_a_z_in_2d(self):
        self.assertEqual(self.state.set_z_view(show_3d=False, z=3), {'ndisplay': 2, 'z': 3})
        self.assertEqual(self.state._viewer.dims.ndisplay, 2)

    def test_3d_shows_the_whole_stack(self):
        self.assertEqual(self.state.set_z_view(show_3d=True), {'ndisplay': 3, 'z': None})
        self.assertEqual(self.state._viewer.dims.ndisplay, 3)

    def test_3d_and_back_again(self):
        # a recording flips between cells, so the return trip has to restore a usable 2D slice
        self.state.set_z_view(show_3d=True)
        self.assertEqual(self.state.set_z_view(show_3d=False, z=1), {'ndisplay': 2, 'z': 1})

    def test_an_out_of_range_slice_is_clamped(self):
        # the z can come from a config that outlived the image it was written against — a cropped
        # version has fewer planes, and a hard failure there would kill a whole batch
        self.assertEqual(self.state.set_z_view(show_3d=False, z=99)['z'], self.SHAPE[1] - 1)
        self.assertEqual(self.state.set_z_view(show_3d=False, z=-5)['z'], 0)

    def test_no_z_keeps_the_slice_showing(self):
        self.state.set_z_view(show_3d=False, z=2)
        self.assertEqual(self.state.set_z_view(show_3d=False, z=None)['z'], 2)

    def test_a_flat_image_refuses_3d(self):
        # same guard open_image applies to its show_3d flag: forcing a single plane into a rotatable
        # 3D view helps nobody, and a mixed 2D/3D set must not break on the 2D members
        self.state._im_data = [np.zeros((4, 1, 8, 8), dtype='float32')]
        self.assertEqual(self.state.set_z_view(show_3d=True)['ndisplay'], 2)

    def test_the_z_axis_is_found_by_name_not_position(self):
        # a store without T puts z FIRST; indexing dims positionally would move the wrong slider
        self.state._axes = ['z', 'y', 'x']
        vol = np.zeros((5, 8, 8), dtype='float32')
        self.state._im_data = [vol]
        self.state._viewer.layers.clear()
        self.state._viewer.add_image(vol, name='im')
        self.assertEqual(self.state.set_z_view(show_3d=False, z=2)['z'], 2)


if __name__ == '__main__':
    unittest.main()
