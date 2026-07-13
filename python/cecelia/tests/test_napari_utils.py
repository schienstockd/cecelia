"""Unit tests for the generic napari display helpers (cecelia.utils.napari_utils).

Headless / napari-free: we patch `require_napari` and pass a fake viewer + duck-typed layer, so these
run without importing napari (per docs/todo/CECELIA_NAPARI_UPSTREAM_PLAN.md). They pin the wrapping
conventions the cecelia bridge AND coastal both rely on: the list-name guard, contrast-from-sample
percentiles, and the kwargs forwarded to viewer.add_image / add_labels / add_tracks.
"""
import unittest
import numpy as np

from cecelia.utils import napari_utils


class FakeLayer:
    def __init__(self, data, visible=True):
        self.data = data
        self.visible = visible
        self.contrast_limits = None
        self.reset_called = False

    def reset_contrast_limits(self):
        self.reset_called = True


class FakeViewer:
    def __init__(self, image_result=None):
        self.calls = {}
        self._image_result = image_result

    def add_image(self, data, **kw):
        self.calls['add_image'] = (data, kw)
        return self._image_result

    def add_labels(self, data, **kw):
        self.calls['add_labels'] = (data, kw)
        return FakeLayer(data)

    def add_tracks(self, data, **kw):
        self.calls['add_tracks'] = (data, kw)
        return FakeLayer(data)


class ImageLayerNameTest(unittest.TestCase):
    def test_list_collapses_when_no_channel_axis(self):
        self.assertEqual(napari_utils.image_layer_name(['A', 'B'], None), 'A')
        self.assertIsNone(napari_utils.image_layer_name([], None))

    def test_list_kept_with_channel_axis(self):
        self.assertEqual(napari_utils.image_layer_name(['A', 'B'], 1), ['A', 'B'])

    def test_scalar_passthrough(self):
        self.assertEqual(napari_utils.image_layer_name('X', None), 'X')
        self.assertIsNone(napari_utils.image_layer_name(None, None))


class ContrastTest(unittest.TestCase):
    def test_percentiles_from_positive_middle_sample(self):
        # [T, Z, Y, X]; the middle T=1, Z=1 plane holds a 1..1000 ramp, the rest is background (0)
        vol = np.zeros((3, 3, 40, 40), dtype=np.float64)
        ramp = np.linspace(1, 1000, 40 * 40)
        vol[1, 1] = ramp.reshape(40, 40)
        layer = FakeLayer(vol)
        napari_utils.set_contrast_from_sample(layer)
        self.assertIsNotNone(layer.contrast_limits)
        lo, hi = layer.contrast_limits
        self.assertGreater(hi, lo)
        self.assertAlmostEqual(lo, float(np.percentile(ramp, 1.0)), delta=1.0)
        self.assertAlmostEqual(hi, float(np.percentile(ramp, 99.9)), delta=1.0)

    def test_multiscale_uses_coarsest_level(self):
        # a LIST of pyramid levels → the coarsest ([-1]) is sampled; all-equal → cmax==cmin → reset
        layer = FakeLayer([np.zeros((2, 2, 50, 50)), np.full((2, 2, 12, 12), 5.0)])
        napari_utils.set_contrast_from_sample(layer)
        self.assertIsNone(layer.contrast_limits)
        self.assertTrue(layer.reset_called)

    def test_fallback_on_bad_data(self):
        layer = FakeLayer(None)   # None has no .ndim → sampling raises → reset fallback
        napari_utils.set_contrast_from_sample(layer)
        self.assertTrue(layer.reset_called)


class AddHelpersTest(unittest.TestCase):
    def setUp(self):
        self._orig = napari_utils.require_napari
        napari_utils.require_napari = lambda: None   # headless: never import napari

    def tearDown(self):
        napari_utils.require_napari = self._orig

    def test_add_image_forwards_and_guards_name(self):
        v = FakeViewer(image_result=FakeLayer(np.zeros((2, 2)), visible=False))
        napari_utils.add_image(v, 'DATA', scale=(1, 2, 3), units=('um', 'um', 'um'),
                               channel_axis=None, channel_names=['only'], colormaps='red')
        data, kw = v.calls['add_image']
        self.assertEqual(data, 'DATA')
        self.assertEqual(kw['name'], 'only')            # list collapsed (no channel axis)
        self.assertEqual(kw['blending'], 'additive')
        self.assertEqual(kw['scale'], (1, 2, 3))
        self.assertEqual(kw['units'], ('um', 'um', 'um'))
        self.assertIsNone(kw['channel_axis'])

    def test_add_image_contrast_skips_invisible(self):
        layer = FakeLayer(np.zeros((4, 4)), visible=False)
        v = FakeViewer(image_result=layer)
        napari_utils.add_image(v, 'D', scale=(1, 1), contrast=True)
        self.assertIsNone(layer.contrast_limits)        # invisible → not sampled
        self.assertFalse(layer.reset_called)

    def test_add_image_omits_units_when_none(self):
        v = FakeViewer(image_result=[])
        napari_utils.add_image(v, 'D', scale=(1, 1), contrast=False)
        _, kw = v.calls['add_image']
        self.assertNotIn('units', kw)

    def test_add_labels_defaults(self):
        v = FakeViewer()
        napari_utils.add_labels(v, 'L', scale=(1, 2, 3), name='seg')
        _, kw = v.calls['add_labels']
        self.assertEqual(kw['opacity'], 0.7)
        self.assertEqual(kw['name'], 'seg')
        self.assertEqual(kw['scale'], (1, 2, 3))

    def test_add_tracks_categorical_wins_over_named(self):
        v = FakeViewer()
        napari_utils.add_tracks(v, 'T', scale=(1, 1, 1, 1), colormaps_dict={0: (1, 1, 1, 1)})
        _, kw = v.calls['add_tracks']
        self.assertIn('colormaps_dict', kw)
        self.assertNotIn('colormap', kw)

    def test_add_tracks_named_colormap_and_defaults(self):
        v = FakeViewer()
        napari_utils.add_tracks(v, 'T', scale=(1, 1, 1, 1), colormap='viridis',
                                properties={'track_id': [1, 2]})
        _, kw = v.calls['add_tracks']
        self.assertEqual(kw['colormap'], 'viridis')
        self.assertNotIn('colormaps_dict', kw)
        self.assertEqual(kw['color_by'], 'track_id')
        self.assertEqual(kw['tail_width'], 4)
        self.assertEqual(kw['tail_length'], 30)
        self.assertEqual(kw['properties'], {'track_id': [1, 2]})


import enum
import json


class _Cmap:
    """duck-typed colormap: capture reads its .name; apply just setattrs a string back."""
    def __init__(self, name):
        self.name = name


class _Color(enum.Enum):
    A = 'additive'


class FakeVLayer:
    def __init__(self, name, **attrs):
        self.name = name
        for k, v in attrs.items():
            setattr(self, k, v)


class FakeLayerList(list):
    """list of layers that also supports `name in layers` and `layers[name]` (like napari's)."""
    def __contains__(self, key):
        if isinstance(key, str):
            return any(l.name == key for l in list(self))
        return super().__contains__(key)

    def __getitem__(self, key):
        if isinstance(key, str):
            for l in list(self):
                if l.name == key:
                    return l
            raise KeyError(key)
        return super().__getitem__(key)


class _Cam:
    def __init__(self):
        self.center = (0.0, 1.0, 2.0)
        self.zoom = 1.0
        self.angles = (0.0, 0.0, 0.0)
        self.perspective = 0.0


class _Dims:
    def __init__(self, nsteps=(5, 1, 1)):
        self.ndisplay = 2
        self.order = (0, 1, 2)
        self.current_step = (0, 0, 0)
        self.point = (0.0, 0.0, 0.0)
        self.nsteps = nsteps


class FakeVViewer:
    def __init__(self, layers):
        self.camera = _Cam()
        self.dims = _Dims()
        self.layers = FakeLayerList(layers)


class JsonScalarTest(unittest.TestCase):
    def test_coercions(self):
        import numpy as np
        self.assertEqual(napari_utils._json_scalar(np.array([1.0, 2.0])), [1.0, 2.0])
        self.assertEqual(napari_utils._json_scalar((np.int64(3), np.float64(4.5))), [3, 4.5])
        self.assertEqual(napari_utils._json_scalar(_Color.A), 'additive')
        self.assertEqual(napari_utils._json_scalar('x'), 'x')

    def test_unexpected_type_stringified_not_raised(self):
        class Weird:
            def __str__(self): return 'weird'
        self.assertEqual(napari_utils._json_scalar(Weird()), 'weird')


class CaptureViewStateTest(unittest.TestCase):
    def _viewer(self):
        return FakeVViewer([
            FakeVLayer('ch0', visible=True, opacity=1.0, blending=_Color.A, gamma=1.0,
                       contrast_limits=[50.0, 800.0], colormap=_Cmap('green')),
            FakeVLayer('ch1', visible=False, colormap=_Cmap('blue'), contrast_limits=[0.0, 100.0]),
        ])

    def test_capture_is_json_safe_and_scalar(self):
        snap = napari_utils.capture_view_state(self._viewer())
        json.dumps(snap)  # must not raise
        self.assertEqual(snap['camera']['center'], [0.0, 1.0, 2.0])   # tuple → list
        self.assertEqual(snap['camera']['zoom'], 1.0)
        self.assertEqual(snap['dims']['current_step'], [0, 0, 0])
        self.assertEqual(snap['layers']['ch0']['colormap'], 'green')  # by NAME, not the object
        self.assertEqual(snap['layers']['ch0']['blending'], 'additive')  # enum → value
        self.assertEqual(snap['layers']['ch0']['contrast_limits'], [50.0, 800.0])
        self.assertFalse(snap['layers']['ch1']['visible'])


class ApplyViewStateTest(unittest.TestCase):
    def test_round_trip_and_colormap_change(self):
        src = FakeVViewer([FakeVLayer('ch0', visible=True, colormap=_Cmap('green'),
                                      contrast_limits=[50.0, 800.0])])
        src.camera.zoom = 4.2
        snap = json.loads(json.dumps(napari_utils.capture_view_state(src)))  # survive a JSON round-trip

        dst = FakeVViewer([FakeVLayer('ch0', visible=True, colormap=_Cmap('red'),
                                      contrast_limits=[0.0, 1000.0])])
        dst.camera.zoom = 1.0
        napari_utils.apply_view_state(dst, snap)
        self.assertEqual(dst.camera.zoom, 4.2)
        self.assertEqual(dst.layers['ch0'].contrast_limits, [50.0, 800.0])
        self.assertEqual(dst.layers['ch0'].colormap, 'green')          # red → green, the headline edit

    def test_missing_layer_skipped(self):
        dst = FakeVViewer([FakeVLayer('ch0', visible=True)])
        napari_utils.apply_view_state(dst, {'layers': {'ghost': {'visible': False}}})  # no error
        self.assertTrue(dst.layers['ch0'].visible)

    def test_current_step_clamped_to_nsteps(self):
        dst = FakeVViewer([FakeVLayer('ch0', visible=True)])   # dims.nsteps = (5,1,1)
        napari_utils.apply_view_state(dst, {'dims': {'current_step': [99, 0, 0]}})
        self.assertEqual(dst.dims.current_step, (4, 0, 0))     # clamped to nsteps-1


if __name__ == '__main__':
    unittest.main()
