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
import re
import sys
import unittest

import numpy as np

_BRIDGE = os.path.join(os.path.dirname(__file__), '..', '..', '..', 'napari', 'napari_bridge.py')

#: Building a real napari **Labels** layer SIGSEGVs on the macOS runner (exit 139, killed mid-test).
#: This file is the first place in the suite to build one — every other napari test uses a stub viewer —
#: so it is a napari/macOS interaction rather than anything about this code, which is pure array and
#: name bookkeeping. The Image path, the one this change adds, runs on all three platforms; the Labels
#: path is additionally exercised live on Linux. Narrowed to the affected cases rather than skipping the
#: file, so macOS keeps covering what it can.
_LABELS_CRASH_ON_MACOS = sys.platform == 'darwin'

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

    def _layer(self, kind, name, dtype, val, source=None):
        spec = {'kind': kind, 'name': name, 'block': self._block(dtype, val),
                'shape': FULL, 'axes': AXES}
        if source:
            spec['source'] = source
        return spec

    def _show(self, layers, **kw):
        return self.nb.NapariState.show_task_preview(
            self.st, value_name='default', layers=layers, region=REGION, **kw)

    def names(self):
        return [l.name for l in self.v.layers]

    @unittest.skipIf(_LABELS_CRASH_ON_MACOS, 'napari Labels layer SIGSEGVs on the macOS runner')
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

    @unittest.skipIf(_LABELS_CRASH_ON_MACOS, 'napari Labels layer SIGSEGVs on the macOS runner')
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

    @unittest.skipIf(_LABELS_CRASH_ON_MACOS, 'napari Labels layer SIGSEGVs on the macOS runner')
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

    def test_a_corrected_channel_inherits_its_original_colour(self):
        """Grey is the wrong default here: comparing corrected against raw IS the judgement, and a
        grey copy of a magenta channel reads as a different measurement rather than the same one."""
        self.v.layers['CH1'].colormap = 'magenta'
        added = self._show([self._layer('image', 'CH1 AF', 'uint16', 900, source='CH1')])
        self.assertEqual(added[0].colormap.name, self.v.layers['CH1'].colormap.name)
        self.assertEqual(added[0].colormap.name, 'magenta')

    def test_each_corrected_channel_follows_its_OWN_source(self):
        self.v.layers['CH1'].colormap = 'magenta'
        self.v.layers['CH2'].colormap = 'green'
        added = self._show([self._layer('image', 'CH1 AF', 'uint16', 900, source='CH1'),
                            self._layer('image', 'CH2 AF', 'uint16', 400, source='CH2')])
        self.assertEqual([l.colormap.name for l in added], ['magenta', 'green'])

    def test_contrast_limits_are_NOT_inherited(self):
        """The corrected values are a ratio rescaled to the dtype — on a different scale entirely, so
        the original's window would usually render the preview black."""
        self.v.layers['CH1'].colormap = 'magenta'
        self.v.layers['CH1'].contrast_limits = (0, 10)
        added = self._show([self._layer('image', 'CH1 AF', 'uint16', 900, source='CH1')])
        self.assertNotEqual(tuple(added[0].contrast_limits), (0, 10))

    def test_a_contrast_window_survives_a_re_preview(self):
        """THE BUG. A re-preview removes and re-adds its layers, so every one reset the contrast the
        user had just set — and a re-preview is what moving the T or Z slider triggers, while scrolling
        through t and z is exactly how you judge a correction. Set it once, scroll, lose it.
        """
        added = self._show([self._layer('image', 'CH1 AF', 'uint16', 900, source='CH1')])
        added[0].contrast_limits = (12, 640)                 # the user dials in a window
        again = self._show([self._layer('image', 'CH1 AF', 'uint16', 900, source='CH1')])
        self.assertEqual(tuple(again[0].contrast_limits), (12, 640))

    def test_a_hand_picked_colormap_outranks_the_source_mirror(self):
        """The mirror is a DEFAULT, not a policy. Re-imposing the source's colour on every scroll would
        undo a deliberate choice the user can only have made after seeing the mirrored one."""
        self.v.layers['CH1'].colormap = 'magenta'
        added = self._show([self._layer('image', 'CH1 AF', 'uint16', 900, source='CH1')])
        self.assertEqual(added[0].colormap.name, 'magenta')  # mirrored on first show
        added[0].colormap = 'green'                          # ...then overridden by hand
        again = self._show([self._layer('image', 'CH1 AF', 'uint16', 900, source='CH1')])
        self.assertEqual(again[0].colormap.name, 'green')

    def test_a_hidden_preview_layer_stays_hidden(self):
        added = self._show([self._layer('image', 'CH1 AF', 'uint16', 900, source='CH1')])
        added[0].visible = False
        again = self._show([self._layer('image', 'CH1 AF', 'uint16', 900, source='CH1')])
        self.assertFalse(again[0].visible)

    def test_props_are_restored_by_NAME_so_a_changed_channel_set_gets_defaults(self):
        """Keyed by layer name: a parameter change that corrects a different channel finds nothing to
        restore, rather than inheriting a window dialled in for another channel's scale."""
        first = self._show([self._layer('image', 'CH1 AF', 'uint16', 900, source='CH1')])
        first[0].contrast_limits = (12, 640)
        other = self._show([self._layer('image', 'CH2 AF', 'uint16', 400, source='CH2')])
        self.assertNotEqual(tuple(other[0].contrast_limits), (12, 640))

    def test_an_unknown_or_absent_source_still_previews(self):
        """Best-effort: a closed channel, or a pre-protocol-3 worker sending no `source`, must cost the
        colour and nothing else. The preview is the point; the colour is a courtesy."""
        added = self._show([self._layer('image', 'CH1 AF', 'uint16', 900, source='NoSuchChannel')])
        self.assertEqual(added[0].name, '(default) CH1 AF')
        bare = self._show([self._layer('image', 'CH2 AF', 'uint16', 400)])   # no source at all
        self.assertEqual(bare[0].name, '(default) CH2 AF')

    def test_the_command_dispatcher_speaks_the_same_protocol(self):
        """Go through `execute_command`, not the method — the boundary a real preview crosses.

        THE BUG THIS EXISTS FOR: the dispatcher kept forwarding the pre-layers protocol
        (`mask=`/`label_shape=`/`label_axes=`) long after the method took `layers=`, so every preview
        died with `unexpected keyword argument 'mask'` and surfaced as a bare "Preview failed". Every
        other case in this file calls `show_task_preview` directly and so proved nothing about the path
        production actually uses — the method and its only caller can disagree, and did.
        """
        cmd = {'type': 'show_task_preview', 'value_name': 'default', 'region': REGION,
               'layers': [self._layer('image', 'CH1 AF', 'uint16', 900)], 'show': True}
        self.nb.execute_command(self.st, cmd)
        self.assertIn('(default) CH1 AF', self.names())

        # ...and the hide direction, which is the same command with no layers
        self.nb.execute_command(self.st, {'type': 'show_task_preview',
                                          'value_name': 'default', 'show': False})
        self.assertEqual(self.names(), ['CH1', 'CH2'])

    def test_the_dispatcher_forwards_no_stale_keyword(self):
        """The signatures must agree by construction, not by someone remembering to update both."""
        import inspect
        accepted = set(inspect.signature(self.nb.NapariState.show_task_preview).parameters)
        src = inspect.getsource(self.nb.execute_command)
        block = src[src.index('elif t == "show_task_preview"'):]
        block = block[:block.index('elif t ==', 10)]
        passed = set(re.findall(r'(\w+)=cmd\.get', block))
        self.assertTrue(passed, 'found no forwarded kwargs — did the dispatch shape change?')
        self.assertEqual(passed - accepted, set(),
                         f'dispatcher forwards kwargs show_task_preview does not accept: '
                         f'{sorted(passed - accepted)}')


if __name__ == '__main__':
    unittest.main()
