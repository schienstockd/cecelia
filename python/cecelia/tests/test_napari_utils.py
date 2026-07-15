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


class TestIsCategoricalColumn(unittest.TestCase):
    """The shared colour-by categorical rule — MUST stay in step with Julia `_is_categorical_col`
    (app/src/tracking/track_props.jl): clusters.* name-rule, else integer with ≤20 levels."""

    def test_clusters_name_rule_always_categorical(self):
        # a high-resolution cluster run exceeds the level cap, but codes are never a count
        many = np.arange(50, dtype=float)
        self.assertTrue(napari_utils.is_categorical_column('clusters', many))
        self.assertTrue(napari_utils.is_categorical_column('clusters.default', many))
        self.assertTrue(napari_utils.is_categorical_column('clusters.tracks', np.array([0., 1.])))

    def test_small_integer_set_is_categorical(self):
        self.assertTrue(napari_utils.is_categorical_column('live.cell.hmm.state', np.array([1., 2., 3.])))

    def test_level_cap_boundary(self):
        self.assertTrue(napari_utils.is_categorical_column('some.count', np.arange(20, dtype=float)))
        self.assertFalse(napari_utils.is_categorical_column('some.count', np.arange(21, dtype=float)))

    def test_continuous_is_numeric(self):
        # non-integer floats → numeric
        self.assertFalse(napari_utils.is_categorical_column('live.cell.speed',
                                                            np.array([0.1, 1.2, 3.14, 9.9])))
        # wide integer spread (e.g. area / neighbour counts, >20 distinct levels) → numeric.
        # NB few integer levels (even {10, 250, 1200}) ARE categorical by the rule — only the level
        # count matters for integer columns, not the spread.
        self.assertFalse(napari_utils.is_categorical_column('area', np.arange(0., 100., 2.0)))


class TestHexRgbaConversion(unittest.TestCase):
    """Colour hex↔RGBA float — the ONE parse shared across every napari colour path (labels/tracks
    colormaps, points face_color, the solid track colormap); the bridge delegates here."""

    def test_hex_to_rgba_parses(self):
        self.assertEqual(napari_utils.hex_to_rgba('#ffffff'), (1.0, 1.0, 1.0, 1.0))
        self.assertEqual(napari_utils.hex_to_rgba('#000000'), (0.0, 0.0, 0.0, 1.0))
        r, g, b, a = napari_utils.hex_to_rgba('#ff8000')      # tolerates a missing leading '#' too
        self.assertAlmostEqual(r, 1.0); self.assertAlmostEqual(b, 0.0); self.assertEqual(a, 1.0)
        self.assertEqual(napari_utils.hex_to_rgba('ff8000'), napari_utils.hex_to_rgba('#ff8000'))

    def test_hex_to_rgba_malformed_is_none(self):
        for bad in (None, '', '#fff', '#gggggg', 'not-a-colour'):
            self.assertIsNone(napari_utils.hex_to_rgba(bad))

    def test_rgba_to_hex_clamps_and_drops_alpha(self):
        self.assertEqual(napari_utils.rgba_to_hex((1.0, 1.0, 1.0, 1.0)), '#ffffff')
        self.assertEqual(napari_utils.rgba_to_hex((0.0, 0.0, 0.0)), '#000000')
        self.assertEqual(napari_utils.rgba_to_hex((2.0, -1.0, 0.5, 0.3)), '#ff0080')  # clamped to 0..255

    def test_round_trip(self):
        for hx in ('#123456', '#abcdef', '#00ff88'):
            self.assertEqual(napari_utils.rgba_to_hex(napari_utils.hex_to_rgba(hx)), hx)


class TestBroadcastTrackToCells(unittest.TestCase):
    """Colour cells by their TRACK's value (clusters.* broadcast) — the track↔cell join behind
    'colour tracks by their cluster/population' (ports R split_tracks)."""

    def test_broadcasts_track_value_to_its_cells(self):
        # tracks 1→cluster 0, 2→cluster 1; cells carry track_id
        out = napari_utils.broadcast_track_to_cells(
            np.array([1, 1, 2, 2, 2]), np.array([1, 2]), np.array([0, 1]))
        self.assertEqual(list(out), [0, 0, 1, 1, 1])

    def test_untracked_and_missing_are_nan(self):
        # track_id 0 (untracked), NaN, and an id absent from the track table → NaN
        out = napari_utils.broadcast_track_to_cells(
            np.array([0.0, np.nan, 1.0, 99.0]), np.array([1, 2]), np.array([7, 8]))
        self.assertTrue(np.isnan(float(out[0])))          # track_id 0 → untracked
        self.assertTrue(np.isnan(float(out[1])))          # NaN track_id
        self.assertEqual(out[2], 7)                       # track 1 → 7
        self.assertTrue(np.isnan(float(out[3])))          # track 99 not in table

    def test_string_track_values_preserved(self):
        out = napari_utils.broadcast_track_to_cells(
            np.array([1, 2, 1]), np.array([1, 2]), np.array(['A', 'B'], dtype=object))
        self.assertEqual(list(out), ['A', 'B', 'A'])


class TestRecordTimelapse(unittest.TestCase):
    """The T-sweep movie primitive — keyframes at first/last T + one interpolated frame per
    timepoint, then animate(). Napari-free: we stub the Animation class and a duck-typed viewer."""

    class _FakeAnim:
        instances = []
        def __init__(self, viewer):
            self.viewer = viewer; self.keyframe_steps = []; self.animated = None
            TestRecordTimelapse._FakeAnim.instances.append(self)
        def capture_keyframe(self, steps=15, **kw): self.keyframe_steps.append(steps)
        def animate(self, path, **kw): self.animated = (path, kw)

    class _FakeDims:
        def __init__(self, ndim): self.current_step = tuple([0] * ndim)

    class _FakeViewer:
        def __init__(self, ndim=3): self.dims = TestRecordTimelapse._FakeDims(ndim)

    def _with_fake_anim(self, fn):
        orig = napari_utils._require_napari_animation
        self._FakeAnim.instances = []
        napari_utils._require_napari_animation = lambda: self._FakeAnim
        try: return fn()
        finally: napari_utils._require_napari_animation = orig

    def test_sweeps_full_range_and_animates(self):
        v = self._FakeViewer(ndim=3)   # [t, y, x]
        n = self._with_fake_anim(lambda: napari_utils.record_timelapse(
            v, '/tmp/x.mp4', t_axis_index=0, n_timepoints=5, fps=10))
        self.assertEqual(n, 5)                                   # 5 timepoints
        anim = self._FakeAnim.instances[0]
        self.assertEqual(anim.keyframe_steps, [15, 4])           # first (default) + steps = t1-t0 = 4
        self.assertEqual(anim.animated[0], '/tmp/x.mp4')
        self.assertEqual(anim.animated[1]['fps'], 10)
        self.assertEqual(v.dims.current_step[0], 4)              # slider left at the last timepoint

    def test_single_timepoint_raises(self):
        # fails fast before the dep is required (no napari-animation needed)
        with self.assertRaises(ValueError):
            napari_utils.record_timelapse(self._FakeViewer(), '/tmp/x.mp4',
                                          t_axis_index=0, n_timepoints=1)


class TestRecordKeyframes(unittest.TestCase):
    """Keyframe animation render — apply each saved view + capture with `steps` tween frames from the
    previous. Napari-free: stub the Animation class and apply_view_state."""

    def _run(self, keyframes):
        anim = TestRecordTimelapse._FakeAnim  # reuse the fake Animation (records keyframe steps)
        anim.instances = []
        applied = []
        orig_a = napari_utils._require_napari_animation
        orig_v = napari_utils.apply_view_state
        napari_utils._require_napari_animation = lambda: anim
        napari_utils.apply_view_state = lambda viewer, vs: applied.append(vs)
        try:
            n = napari_utils.record_keyframes(object(), '/tmp/a.mp4', keyframes, fps=12)
        finally:
            napari_utils._require_napari_animation = orig_a
            napari_utils.apply_view_state = orig_v
        return n, anim.instances[0], applied

    def test_applies_each_view_and_tweens_from_previous(self):
        n, anim, applied = self._run([
            {'viewState': {'a': 1}},                 # first: starts the sequence (steps ignored)
            {'viewState': {'a': 2}, 'steps': 10},
            {'viewState': {'a': 3}, 'steps': 5},
        ])
        self.assertEqual(applied, [{'a': 1}, {'a': 2}, {'a': 3}])   # every view applied, in order
        self.assertEqual(anim.keyframe_steps, [15, 10, 5])          # first default; then per-keyframe tween
        self.assertEqual(anim.animated[1]['fps'], 12)
        self.assertEqual(n, 10 + 5 + 1)                            # tween frames (skip first) + 1

    def test_needs_two_keyframes(self):
        with self.assertRaises(ValueError):
            napari_utils.record_keyframes(object(), '/tmp/a.mp4', [{'viewState': {}}])


class _ClipLayer:
    """Duck-typed layer for the pure clip-plane geometry (only ndim/scale/translate are read)."""
    def __init__(self, ndim, scale, translate=None):
        self.ndim = ndim
        self.scale = scale
        self.translate = translate if translate is not None else [0.0] * ndim


class TestAxisAlignedClipPlanes(unittest.TestCase):
    # display axes for a t,z,y,x viewer; the crop box only names spatial axes
    DISP = ['t', 'z', 'y', 'x']

    def test_three_d_vectors_over_displayed_zyx(self):
        # image layer (t,z,y,x), scale 1µm/px on t, 2 on z, 0.5 on y/x. Clipping planes are 3-D:
        # position/normal are 3-vectors over the displayed z,y,x (t is dropped), NOT ndim-length.
        layer = _ClipLayer(4, [1.0, 2.0, 0.5, 0.5])
        box = {'z': (4.0, 20.0), 'y': (5.0, 50.0), 'x': (10.0, 100.0)}
        planes = napari_utils.axis_aligned_clip_planes(layer, box, self.DISP)
        self.assertEqual(len(planes), 6)                         # 2 planes × 3 axes
        for p in planes:
            self.assertEqual(len(p['position']), 3)              # 3-D, not 4
            self.assertEqual(len(p['normal']), 3)
        # z is the first displayed dim (slot 0): lo 4µm/2 = 2px (normal +), hi 20µm/2 = 10px (normal -)
        z_lo, z_hi = planes[0], planes[1]
        self.assertEqual(z_lo['position'][0], 2.0)
        self.assertEqual(z_lo['normal'], (1.0, 0.0, 0.0))
        self.assertEqual(z_hi['position'][0], 10.0)
        self.assertEqual(z_hi['normal'], (-1.0, 0.0, 0.0))
        # x is the third displayed dim (slot 2): lo 10µm/0.5 = 20px
        self.assertEqual(planes[4]['position'][2], 20.0)
        self.assertTrue(all(p['enabled'] for p in planes))

    def test_trailing_alignment_for_fewer_dims(self):
        # a (z,y,x) layer in a (t,z,y,x) viewer: dims are right-aligned, so 'z' is layer dim 0 = slot 0
        layer = _ClipLayer(3, [2.0, 0.5, 0.5])
        planes = napari_utils.axis_aligned_clip_planes(layer, {'z': (4.0, 20.0)}, self.DISP)
        self.assertEqual(len(planes), 2)
        self.assertEqual(planes[0]['position'][0], 2.0)          # 4µm / 2 = 2px at slot 0
        self.assertEqual(planes[0]['normal'], (1.0, 0.0, 0.0))

    def test_translate_offset_applied(self):
        # (z,y,x) with y translated +10µm; y is displayed slot 1
        layer = _ClipLayer(3, [1.0, 1.0, 1.0], translate=[0.0, 10.0, 0.0])
        planes = napari_utils.axis_aligned_clip_planes(layer, {'y': (10.0, 30.0)}, self.DISP)
        self.assertEqual(len(planes), 2)
        self.assertEqual(planes[0]['position'][1], 0.0)          # (10-10)/1 = 0px
        self.assertEqual(planes[1]['position'][1], 20.0)         # (30-10)/1 = 20px

    def test_two_d_layer_returns_empty(self):
        # clipping planes are a 3-D volume feature — a 2-D (y,x) layer has nothing to clip
        layer = _ClipLayer(2, [0.5, 0.5])
        planes = napari_utils.axis_aligned_clip_planes(layer, {'y': (0.0, 5.0), 'x': (0.0, 5.0)}, self.DISP)
        self.assertEqual(planes, [])


if __name__ == '__main__':
    unittest.main()
