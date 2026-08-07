"""Layer props carry the channel's COLOUR, not just its colormap name.

Why this needs pinning: the Julia preview renderer (`api/src/image_render.jl`) has to colourise each
channel itself, and it used to do that by looking the napari colormap name up in its own table. napari
has ~30 colormaps and the user can pick any of them, so that table could never be complete — and it
wasn't: `bop blue` was missing, fell through to the unknown-name fallback (WHITE), and every intravital
image's SHG channel rendered white instead of blue, additively washing the whole preview out.

napari owns its palette, so it exports the actual LUT and the renderer only interpolates. These tests
pin that the export is present, black-anchored for the additive primaries, faithful for the perceptual
maps, and JSON-native (the props file is read by Julia, so a numpy scalar in there is a hard failure).

Headless: only the module-level/static helpers are exercised — no viewer, no Qt. Skipped where napari
isn't importable (the bridge is a runtime process, not a package dep). Part of `pixi run test-py`.
"""
import json
import os
import sys
import unittest

_BRIDGE_DIR = os.path.join(os.path.dirname(__file__), '..', '..', '..', 'napari')


def _load_bridge():
    """The bridge module, or None when napari/qtpy aren't installed. Only an ImportError is tolerated —
    a renamed helper must FAIL rather than silently skip."""
    sys.path.insert(0, os.path.abspath(_BRIDGE_DIR))
    try:
        import napari_bridge
    except ImportError:
        return None
    finally:
        sys.path.pop(0)
    return napari_bridge


class ColormapLutExportTest(unittest.TestCase):
    def setUp(self):
        self.bridge = _load_bridge()
        if self.bridge is None:
            self.skipTest('napari not importable here')
        from napari.utils.colormaps import AVAILABLE_COLORMAPS
        self.cmaps = AVAILABLE_COLORMAPS
        self.lut = self.bridge.NapariState._colormap_lut

    def test_bop_blue_is_blue(self):
        """The exact regression: `bop blue` must export blue, not the white a name table fell back to."""
        stops = self.lut(self.cmaps['bop blue'])
        r, g, b = stops[-1]
        self.assertGreater(b, 0.9)
        self.assertLess(r, 0.2)
        self.assertGreater(b - r, 0.5, 'unmistakably blue, not white')

    def test_additive_primaries_are_two_stop_black_anchored_ramps(self):
        """red/green/…/bop* start at black, so the renderer's `n * base` interpolation is exact."""
        for name in ('red', 'green', 'blue', 'cyan', 'magenta', 'yellow', 'gray',
                     'bop blue', 'bop orange', 'bop purple'):
            stops = self.lut(self.cmaps[name])
            self.assertGreaterEqual(len(stops), 2, name)
            self.assertEqual([0.0, 0.0, 0.0], stops[0], f'{name} must be anchored at black')

    def test_white_to_colour_maps_are_exported_faithfully(self):
        """napari's `I *` set runs WHITE→colour. No name table could express that; the LUT must."""
        stops = self.lut(self.cmaps['I Blue'])
        self.assertEqual([1.0, 1.0, 1.0], stops[0])
        self.assertGreater(stops[-1][2], stops[-1][0], 'ends blue-dominant')

    def test_every_colormap_exports_and_stays_within_the_stop_cap(self):
        cap = self.bridge.NapariState._LUT_MAX_STOPS
        for name, cm in self.cmaps.items():
            with self.subTest(colormap=name):
                stops = self.lut(cm)
                self.assertGreaterEqual(len(stops), 1)
                self.assertLessEqual(len(stops), cap)
                for stop in stops:
                    self.assertEqual(3, len(stop))
                    for v in stop:
                        # JSON-native and in range — Julia reads this file, a numpy scalar would break it
                        self.assertIsInstance(v, float)
                        self.assertGreaterEqual(v, 0.0)
                        self.assertLessEqual(v, 1.0)

    def test_the_export_is_json_serialisable(self):
        payload = {n: self.lut(cm) for n, cm in self.cmaps.items()}
        self.assertIsInstance(json.dumps(payload), str)

    def test_resampling_a_256_entry_map_stays_visually_faithful(self):
        """A perceptual map is downsampled to the cap; linear interpolation back must stay under ~2/255."""
        import numpy as np
        for name in ('viridis', 'turbo', 'inferno', 'twilight'):
            with self.subTest(colormap=name):
                stops = np.asarray(self.lut(self.cmaps[name]))
                full = np.asarray(self.cmaps[name].colors)[:, :3]
                x = np.linspace(0, 1, 256)
                approx = np.stack([np.interp(x, np.linspace(0, 1, len(stops)), stops[:, k])
                                   for k in range(3)], axis=1)
                want = np.stack([np.interp(x, np.linspace(0, 1, len(full)), full[:, k])
                                 for k in range(3)], axis=1)
                self.assertLess(float(np.abs(approx - want).max()) * 255, 2.5)


class ColormapLutIsWiredIntoSaveTest(unittest.TestCase):
    """Source-level: the LUT must actually be written into the props file, and the NAME kept alongside it
    (the viewer's own restore path sets `colormap` by name)."""

    def test_save_layer_props_writes_both_the_name_and_the_lut(self):
        src_path = os.path.join(_BRIDGE_DIR, 'napari_bridge.py')
        with open(src_path, encoding='utf-8') as fh:
            src = fh.read()
        head = src.split('def save_layer_props', 1)
        self.assertEqual(2, len(head), 'save_layer_props was renamed — update this test')
        body = head[1].split('\n    def ', 1)[0]
        self.assertIn('colormap_lut', body)
        self.assertIn('_colormap_lut', body)
        self.assertIn('"colormap"', body, 'the colormap NAME must still be saved for napari\'s restore')


if __name__ == '__main__':
    unittest.main()
