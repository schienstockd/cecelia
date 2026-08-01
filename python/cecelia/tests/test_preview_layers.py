"""The bridge's layered preview path, driven against a real (headless) napari ViewerModel.

A preview reply carries a LIST of layers, each with its own kind — `labels` for a segmentation mask,
`image` for a corrected channel. One task can produce several: AF correction returns one image layer
per corrected channel so they sit BESIDE the originals and can be flipped against them, which is the
judgement being made. A single mask field plus a type flag could not express that.

Two properties are worth pinning and neither is obvious:

* an image layer aligns to the viewer by AXIS NAME — the block is channel-less ``[T, Z, Y, X]`` while
  the image layers it sits beside include C, and napari otherwise reads a short layer's axes as the
  viewer's TRAILING ones, which renders time as Z;
* a second preview must remove the layers the FIRST one added, by name. A parameter change can alter
  which channels a task even outputs, so a suffix scan would either miss layers (leaving one parameter
  set's result on screen beside another's) or delete the user's own.

``ViewerModel`` needs no Qt and no display, so this runs in CI. Part of the Python (analysis-env)
suite — run with `pixi run test-py`.
"""
import importlib.util as iu
import os
import unittest

import numpy as np

_BRIDGE = os.path.join(os.path.dirname(__file__), '..', '..', '..', 'napari', 'napari_bridge.py')

AXES = ['T', 'Z', 'Y', 'X']
FULL = [4, 3, 32, 30]
REGION = {'T': [1, 2], 'Z': [1, 2], 'Y': [4, 12], 'X': [5, 15]}


def _load_bridge():
    """The bridge module, or None when napari/qtpy is absent.

    Only ImportError is tolerated. A renamed symbol must FAIL rather than silently skip the file — an
    earlier version of a sibling test skipped all eight of its cases for exactly that reason and looked
    green for it.
    """
    try:
        spec = iu.spec_from_file_location('napari_bridge_under_test', os.path.abspath(_BRIDGE))
        m = iu.module_from_spec(spec)
        spec.loader.exec_module(m)
        return m
    except ImportError:
        return None


class PreviewLayersTest(unittest.TestCase):
    def setUp(self):
        self.nb = _load_bridge()
        if self.nb is None:
            self.skipTest('napari bridge not importable in this environment')
        from napari.components import ViewerModel
        self.v = ViewerModel()
        # two channels, as a real image gives: [T, C, Z, Y, X] split into per-channel layers
        self.v.add_image(np.zeros((4, 2, 3, 32, 30), dtype='uint16'),
                         channel_axis=1, name=['CH1', 'CH2'])
        st = self.nb.NapariState.__new__(self.nb.NapariState)
        st._viewer = self.v
        st._preview_layers = set()
        st._im_scale = [1.0, 1.0, 1.0, 1.0]
        st._im_units = ['um'] * 4
        st._axes = ['T', 'C', 'Z', 'Y', 'X']
        st._im_data = [np.zeros((4, 2, 3, 32, 30), dtype='uint16')]
        st._on_view_change = None
        st._view_listener_url = None
        st._invalidate_colcol_cache = lambda vn: None
        self.st = st

    @staticmethod
    def _block(dtype, val):
        from cecelia.utils import block_transfer
        return block_transfer.encode_block(np.full((1, 1, 8, 10), val, dtype=dtype))

    def _layer(self, kind, name, dtype, val):
        return {'kind': kind, 'name': name, 'block': self._block(dtype, val),
                'shape': FULL, 'axes': AXES}

    def _show(self, layers, **kw):
        return self.nb.NapariState.show_task_preview(
            self.st, value_name='default', layers=layers, region=REGION, **kw)

    def names(self):
        return [l.name for l in self.v.layers]

    def test_both_kinds_build_the_right_layer_type(self):
        added = self._show([self._layer('labels', 'Preview', 'uint32', 7),
                            self._layer('image', 'CH1 AF', 'uint16', 900)])
        self.assertEqual([type(l).__name__ for l in added], ['Labels', 'Image'])
        self.assertEqual([l.name for l in added], ['(default) Preview', '(default) CH1 AF'])

    def test_a_corrected_channel_sits_beside_the_original(self):
        # the whole point: comparing corrected against raw IS the judgement, so they must coexist
        self._show([self._layer('image', 'CH1 AF', 'uint16', 900)])
        self.assertIn('CH1', self.names())
        self.assertIn('(default) CH1 AF', self.names())

    def test_the_block_lands_where_the_region_says(self):
        added = self._show([self._layer('labels', 'Preview', 'uint32', 7)])
        data = added[0].data
        self.assertEqual(int(np.asarray(data[1, 1, 5, 6])), 7)      # inside the region
        self.assertEqual(int(np.asarray(data[0, 0, 0, 0])), 0)      # outside it

    def test_a_channel_less_block_aligns_to_the_viewer_by_axis_name(self):
        added = self._show([self._layer('image', 'CH1 AF', 'uint16', 900)])
        self.assertEqual(added[0].data.ndim, self.v.dims.ndim)

    def test_a_narrower_second_preview_leaves_nothing_stale(self):
        self._show([self._layer('image', 'CH1 AF', 'uint16', 900),
                    self._layer('image', 'CH2 AF', 'uint16', 400)])
        self._show([self._layer('image', 'CH1 AF', 'uint16', 1200)])
        self.assertIn('(default) CH1 AF', self.names())
        self.assertNotIn('(default) CH2 AF', self.names())

    def test_hiding_removes_every_preview_layer_and_nothing_else(self):
        self._show([self._layer('labels', 'Preview', 'uint32', 7),
                    self._layer('image', 'CH1 AF', 'uint16', 900)])
        self.nb.NapariState.show_task_preview(self.st, value_name='default', layers=None, show=False)
        self.assertEqual(self.names(), ['CH1', 'CH2'])

    def test_an_unknown_kind_is_refused_rather_than_guessed(self):
        with self.assertRaises(ValueError):
            self._show([self._layer('heatmap', 'X', 'uint16', 1)])

    def test_a_layer_without_geometry_is_refused(self):
        bad = self._layer('image', 'CH1 AF', 'uint16', 900)
        bad.pop('shape')
        with self.assertRaises(ValueError):
            self._show([bad])


if __name__ == '__main__':
    unittest.main()
