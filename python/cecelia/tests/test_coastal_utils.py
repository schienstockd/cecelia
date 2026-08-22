"""`CoastalUtils` — the parts that fail SILENTLY if they are wrong.

Coastal itself is not exercised here: it is a pinned git dependency whose inference path needs a
trained checkpoint and a GPU, and the pinned revision may lag what this file targets. What IS tested
is the glue, and specifically the three ways this glue can produce a plausible wrong answer with no
error at all:

  1. a temporal radius one too small, so the truncated window at t=0 drops the largest scale's
     metric plane and shifts every later channel;
  2. per-tile photometric scaling, so the same cell normalises differently depending on which tile
     read it;
  3. a metric set that does not match the model's manifest.

Coastal's own contract for (1) and (3) is asserted in coastal's `test_flow_metric_count.py`; these
tests pin the cecelia side that has to respect it.
"""
import json
import os
import tempfile
import unittest

import numpy as np


def _window(frames, index, wid=0, start=0, tile=(0, 16, 0, 16), channels=None):
    """A `TemporalWindow` for a test, with everything but the frames defaulted.

    The point of the object is that a test says only what it is about — a stack of frames and which
    one is the tile — instead of restating six positional facts at every call site.
    """
    from cecelia.utils.segmentation_utils import TemporalWindow
    return TemporalWindow(frames=frames, index=index, start=start, tile=tile,
                          channels=channels, id=wid)


class _DimUtils:
    """Enough of DimUtils for the base's __init__ and the temporal guard."""

    def __init__(self, n_t=30, order='TCZYX', shape=(30, 2, 4, 64, 64), frame_interval=None,
                 frame_interval_unit='s'):
        self.im_dim_order = list(order)
        self._shape = shape
        self._n_t = n_t
        # None = the OME file carried no TimeIncrement, which is a real and common case and the one
        # that must NOT be filled with an invented 1.0 — see `manifest_frame_interval`.
        self._dt = frame_interval
        self._dt_unit = frame_interval_unit

    def is_timeseries(self):
        return 'T' in self.im_dim_order

    def is_3D(self):
        return 'Z' in self.im_dim_order

    def dim_val(self, ax):
        return self._shape[self.im_dim_order.index(ax)]

    def dim_idx(self, ax):
        return self.im_dim_order.index(ax)

    def im_physical_size(self, ax, default=1.0):
        return default

    def im_time_increment(self, default=None):
        return self._dt if self._dt is not None else default

    def im_time_increment_unit(self, default='s'):
        return self._dt_unit or default


class _StubInference:
    """Records what it was handed; returns two labelled quadrants."""

    def __init__(self):
        self.calls = []

    def predict_frame(self, frame, metrics):
        self.calls.append((np.asarray(frame).copy(), dict(metrics)))
        h, w = frame.shape
        inst = np.zeros((h, w), np.uint32)
        inst[: h // 2, : w // 2] = 1
        inst[h // 2:, w // 2:] = 2
        return None, inst, None


class _PlaneIdInference:
    """Labels the whole plane with the value carried in `mag_1` — i.e. its z index."""

    def predict_frame(self, frame, metrics):
        value = int(np.asarray(metrics['mag_1']).flat[0])
        return None, np.full(frame.shape, value, np.uint32), None


def _utils(model_params=None, dim_utils=None, task_dir='/tmp', params_extra=None,
           manifest=None):
    """A CoastalUtils with the coastal calls stubbed out.

    `manifest`, when given, is returned by `_manifest` via a subclass — it has to be in place BEFORE
    construction, because `__init__` resolves the temporal config from it and that is what sets
    `TEMPORAL_RADIUS`. Assigning to `_manifest_cache` after the fact arrives too late (and `__init__`
    clears it anyway).
    """
    from cecelia.utils.coastal_utils import CoastalUtils

    mp = {'model': '/nonexistent/model.pt', 'cellChannels': [0]}
    mp.update(model_params or {})
    params = {'taskDir': task_dir, 'models': {'0': mp}}
    params.update(params_extra or {})

    if manifest is not None:
        class _WithManifest(CoastalUtils):
            def _manifest(self, _model_params):
                return manifest
        cu = _WithManifest(params, dim_utils or _DimUtils())
    else:
        cu = CoastalUtils(params, dim_utils or _DimUtils())
    stub = _StubInference()
    cu._get_inference = lambda _mp: stub
    cu._stub = stub
    # `flow_metrics_for_frame` stands in for coastal: return the centre frame plus one plane, and
    # record the window it was given.
    cu._seen = []

    def _metrics(window, center, scales, cumulative, **_):
        cu._seen.append((np.asarray(window).copy(), center, tuple(scales), cumulative))
        return np.asarray(window)[center], {'mag_1': np.zeros_like(window[center]),
                                            'divergence': np.zeros_like(window[center])}

    cu._flow_metrics = _metrics
    return cu


class ManifestTest(unittest.TestCase):

    def test_missing_manifest_falls_back_to_coastal_training_defaults(self):
        from cecelia.utils import coastal_utils

        scales, cumulative, dropped = coastal_utils.temporal_config(
            coastal_utils.read_manifest('/nonexistent/model.pt'))
        self.assertEqual(scales, coastal_utils.DEFAULT_TEMPORAL_SCALES)
        self.assertEqual(cumulative, coastal_utils.DEFAULT_CUMULATIVE_WINDOW)
        self.assertEqual(dropped, ())

    def test_manifest_is_read_from_the_sidecar_beside_the_checkpoint(self):
        from cecelia.utils import coastal_utils

        with tempfile.TemporaryDirectory() as d:
            pt = os.path.join(d, 'gc_memtom.pt')
            with open(coastal_utils.manifest_path(pt), 'w', encoding='utf-8') as f:
                json.dump({'temporalScales': [1, 2, 4], 'cumulativeWindow': 3,
                           'droppedMetrics': ['divergence', 'vorticity']}, f)

            scales, cumulative, dropped = coastal_utils.temporal_config(
                coastal_utils.read_manifest(pt))
            self.assertEqual(scales, [1, 2, 4])
            self.assertEqual(cumulative, 3)
            self.assertEqual(dropped, ('divergence', 'vorticity'))

    def test_a_group_with_no_model_configures_the_feature_set_itself(self):
        """The flow-metrics contact sheet has no checkpoint to read scales from.

        It computes the metric planes so the user can decide which are worth training on, so it must
        honour the scales they picked rather than falling back to coastal's defaults.
        """
        cu = _utils(model_params={'model': '', 'temporalScales': [1, 3],
                                  'cumulativeWindow': 7, 'droppedMetrics': ['strain']})
        self.assertEqual(cu._manifest({'model': '', 'temporalScales': [1, 3],
                                       'cumulativeWindow': 7, 'droppedMetrics': ['strain']}),
                         {'temporalScales': [1, 3], 'cumulativeWindow': 7,
                          'droppedMetrics': ['strain']})
        self.assertEqual(cu.TEMPORAL_RADIUS, 3)

    def test_a_real_manifest_beats_whatever_the_caller_sent(self):
        """Inference must match TRAINING. A task param that could override the manifest is exactly
        the silent channel shift the manifest exists to prevent."""
        from cecelia.utils.coastal_utils import CoastalUtils, manifest_path

        with tempfile.TemporaryDirectory() as d:
            pt = os.path.join(d, 'trained.pt')
            with open(manifest_path(pt), 'w', encoding='utf-8') as f:
                json.dump({'temporalScales': [1, 2], 'droppedMetrics': ['vorticity']}, f)

            mp = {'model': pt, 'cellChannels': [0],
                  'temporalScales': [16], 'droppedMetrics': ['strain']}
            cu = CoastalUtils({'taskDir': d, 'models': {'0': mp}}, _DimUtils())
            self.assertEqual(cu._manifest(mp),
                             {'temporalScales': [1, 2], 'droppedMetrics': ['vorticity']})
            self.assertEqual(cu.TEMPORAL_RADIUS, 2)


class TemporalRadiusTest(unittest.TestCase):
    """The radius must be max(scales), NOT max(scales)-1.

    A window truncated at t=0 is r+1 frames long, and coastal needs scale+1 frames to produce
    `mag_{scale}`. At r = max(scales)-1 that is one frame short at both ends, the plane vanishes, and
    every metric sorting after it shifts down a channel — with no error raised.
    """

    def test_radius_is_the_largest_scale(self):
        cu = _utils()
        self.assertEqual(cu.TEMPORAL_RADIUS, 8)

    def test_truncated_start_window_is_still_long_enough_for_the_largest_scale(self):
        cu = _utils()
        largest = cu.TEMPORAL_RADIUS
        # what the base builds at t=0: range(max(0, 0-r), min(T-1, 0+r) + 1)
        window_len = len(range(max(0, 0 - cu.TEMPORAL_RADIUS), min(29, 0 + cu.TEMPORAL_RADIUS) + 1))
        self.assertGreaterEqual(window_len, largest + 1,
                                'truncated start window would drop the largest scale')

    def test_stacked_groups_take_the_widest_window(self):
        """One window serves every group, so a narrow-scale group must not shrink it."""
        from cecelia.utils.coastal_utils import CoastalUtils

        with tempfile.TemporaryDirectory() as d:
            small = os.path.join(d, 'small.pt')
            with open(os.path.splitext(small)[0] + '.json', 'w', encoding='utf-8') as f:
                json.dump({'temporalScales': [1, 2]}, f)
            params = {'taskDir': d, 'models': {
                '0': {'model': small, 'cellChannels': [0]},
                '1': {'model': os.path.join(d, 'big.pt'), 'cellChannels': [0]},   # no manifest → [1,2,4,8]
            }}
            cu = CoastalUtils(params, _DimUtils())
            self.assertEqual(cu.TEMPORAL_RADIUS, 8)

    def test_a_movie_shorter_than_the_largest_scale_raises(self):
        from cecelia.utils.coastal_utils import CoastalUtils

        params = {'taskDir': '/tmp', 'models': {'0': {'model': '/nope.pt', 'cellChannels': [0]}}}
        with self.assertRaises(ValueError) as ctx:
            CoastalUtils(params, _DimUtils(shape=(6, 2, 4, 64, 64)))
        self.assertIn('timepoints', str(ctx.exception))

    def test_a_still_image_raises_rather_than_segmenting_nothing(self):
        from cecelia.utils.coastal_utils import CoastalUtils

        params = {'taskDir': '/tmp', 'models': {'0': {'model': '/nope.pt', 'cellChannels': [0]}}}
        with self.assertRaises(ValueError) as ctx:
            CoastalUtils(params, _DimUtils(order='CZYX', shape=(2, 4, 64, 64)))
        self.assertIn('time series', str(ctx.exception))


class ProjectionTest(unittest.TestCase):

    @staticmethod
    def _overlapping_tiles():
        """Two OVERLAPPING tile windows of one image whose halves differ in brightness.

        The overlap is the same pixels read twice, so it is exactly where a tile-dependent
        normalisation shows up — and the tiles must differ in composition (tile A is mostly bright,
        tile B mostly dim) or their local percentiles coincide and the test proves nothing.
        """
        rng = np.random.default_rng(0)
        image = rng.random((5, 1, 16, 32)).astype(np.float32) * 4000
        image[..., 16:] *= 0.1
        return image[..., 0:24], image[..., 8:32]        # overlap = image cols 8:24

    def test_global_norm_params_make_the_projection_tile_independent(self):
        """The same pixels must project identically whichever tile's window they arrive in."""
        cu = _utils()
        a_tile, b_tile = self._overlapping_tiles()
        norm = {0: (0.0, 4000.0)}

        a = cu._project_window(a_tile, {'cellChannels': [0]}, norm)
        b = cu._project_window(b_tile, {'cellChannels': [0]}, norm)
        np.testing.assert_allclose(a[:, :, 8:24], b[:, :, 0:16], rtol=0, atol=0)

    def test_without_global_norm_params_the_projection_is_tile_local(self):
        """Sanity for the test above: the drift it prevents has to be real."""
        cu = _utils()
        a_tile, b_tile = self._overlapping_tiles()

        a = cu._project_window(a_tile, {'cellChannels': [0]}, None)
        b = cu._project_window(b_tile, {'cellChannels': [0]}, None)
        self.assertFalse(np.allclose(a[:, :, 8:24], b[:, :, 0:16]))

    def test_channels_merge_by_maximum_and_land_in_0_255(self):
        cu = _utils()
        ctx = np.zeros((3, 2, 8, 8), np.float32)
        ctx[:, 0, :4] = 1000.0
        ctx[:, 1, 4:] = 1000.0
        out = cu._project_window(ctx, {'cellChannels': [0, 1]}, {0: (0.0, 1000.0),
                                                                 1: (0.0, 1000.0)})
        self.assertEqual(out.shape, (3, 8, 8))
        self.assertAlmostEqual(float(out.max()), 255.0, places=3)
        self.assertTrue(np.all(out > 254.0), 'max-merge should fill the whole frame')


class ProjectionInPlaceTest(unittest.TestCase):
    """The projection was rewritten to work in place. Two things must not have changed."""

    def _reference(self, context, channels, norm):
        """The expression form the in-place version replaced, verbatim."""
        from cecelia.utils.coastal_utils import PROJECTION_MAX
        projected = None
        for ch in channels:
            arr = np.asarray(context[:, ch], dtype=np.float32)
            lo, hi = norm[ch]
            arr = np.clip((arr - lo) / (hi - lo + 1e-8), 0.0, 1.0)
            projected = arr if projected is None else np.maximum(projected, arr)
        return (projected * PROJECTION_MAX).astype(np.float32)

    def test_bit_identical_to_the_expression_form(self):
        """`assert_array_equal`, not `allclose`: this was a performance change and the output is
        somebody's segmentation. Folding the 255 into the scale before the clip is 1.5e-5 different,
        which is why the operations are in the order they are."""
        cu = _utils()
        rng = np.random.default_rng(3)
        ctx = (rng.random((4, 3, 6, 7)) * 5000).astype(np.uint16)
        norm = {0: (12.0, 3771.0), 1: (0.0, 4999.0), 2: (100.0, 200.0)}
        for channels in ([0], [0, 1], [2, 0, 1]):
            np.testing.assert_array_equal(
                cu._project_window(ctx, {'cellChannels': list(channels)}, norm),
                self._reference(ctx, channels, norm), f'channels {channels}')

    def test_a_float32_input_is_not_mutated(self):
        """`np.asarray` on a float32 store returns the input itself, and in-place arithmetic would
        then rewrite the caller's pixels. Today's stores are uint16, which is what would have made
        this silent."""
        cu = _utils()
        rng = np.random.default_rng(4)
        ctx = (rng.random((3, 2, 5, 5)) * 1000).astype(np.float32)
        before = ctx.copy()
        cu._project_window(ctx, {'cellChannels': [0, 1]}, {0: (0.0, 1000.0), 1: (0.0, 1000.0)})
        np.testing.assert_array_equal(ctx, before)


class SharedProjectionBetweenGroupsTest(unittest.TestCase):
    """One window, several model groups: the projection must be computed once.

    It sat outside `_feature_key`'s cache, so on a two-group run it was the largest repeated line in
    the timepoint — and the two arrays were `np.array_equal`.
    """

    def _cu(self, groups, shape=(30, 3, 4, 8, 8)):
        from cecelia.utils.coastal_utils import CoastalUtils
        cu = CoastalUtils({'taskDir': '/tmp', 'models': groups},
                          _DimUtils(shape=shape))
        cu.calls = []
        real = cu._project_window

        def counting(context, model_params, norm_params, context_channels=None):
            cu.calls.append(tuple(cu._model_channels(model_params)))
            return real(context, model_params, norm_params, context_channels)

        cu._project_window = counting
        return cu

    @staticmethod
    def _frames(shape=(4, 3, 4, 8, 8)):
        rng = np.random.default_rng(7)
        return (rng.random(shape) * 4000).astype(np.uint16)

    def test_two_groups_on_the_same_channels_project_once(self):
        g = {'0': {'model': 'm.pt', 'cellChannels': [1]},
             '1': {'model': 'm.pt', 'cellChannels': [1]}}
        cu = self._cu(g)
        w = _window(self._frames(), 2, wid=1)
        norm = {1: (0.0, 4000.0)}
        a = cu._cached_projection(w, g['0'], norm)
        b = cu._cached_projection(w, g['1'], norm)
        self.assertEqual(len(cu.calls), 1)
        self.assertIs(a, b)

    def test_groups_on_different_channels_do_not_share(self):
        g = {'0': {'model': 'm.pt', 'cellChannels': [1]},
             '1': {'model': 'm.pt', 'cellChannels': [2]}}
        cu = self._cu(g)
        w = _window(self._frames(), 2, wid=1)
        norm = {1: (0.0, 4000.0), 2: (0.0, 4000.0)}
        a = cu._cached_projection(w, g['0'], norm)
        b = cu._cached_projection(w, g['1'], norm)
        self.assertEqual(len(cu.calls), 2)
        self.assertFalse(np.array_equal(a, b))

    def test_the_same_channels_at_different_clip_ranges_do_not_share(self):
        """The bounds reach the pixels, so two groups that disagree about them are not looking at
        the same frame."""
        g = {'0': {'model': 'm.pt', 'cellChannels': [1]},
             '1': {'model': 'm.pt', 'cellChannels': [1]}}
        cu = self._cu(g)
        w = _window(self._frames(), 2, wid=1)
        a = cu._cached_projection(w, g['0'], {1: (0.0, 4000.0)})
        b = cu._cached_projection(w, g['1'], {1: (500.0, 2000.0)})
        self.assertEqual(len(cu.calls), 2)
        self.assertFalse(np.array_equal(a, b))

    def test_a_new_window_drops_the_previous_projection(self):
        """The largest single allocation this class makes; holding two windows of it is the bug."""
        g = {'0': {'model': 'm.pt', 'cellChannels': [1]},
             '1': {'model': 'm.pt', 'cellChannels': [1]}}
        cu = self._cu(g)
        norm = {1: (0.0, 4000.0)}
        for wid in (1, 2, 3):
            cu._cached_projection(_window(self._frames(), 2, wid=wid), g['0'], norm)
            self.assertEqual(len(cu._projection_cache), 1, 'one window at a time')
        self.assertEqual(len(cu.calls), 3)

    def test_a_single_group_caches_nothing(self):
        """The common case must not carry the memory — the same rule `_feature_key` follows."""
        g = {'0': {'model': 'm.pt', 'cellChannels': [1]}}
        cu = self._cu(g)
        w = _window(self._frames(), 2, wid=1)
        norm = {1: (0.0, 4000.0)}
        cu._cached_projection(w, g['0'], norm)
        cu._cached_projection(w, g['0'], norm)
        self.assertEqual(len(cu.calls), 2)
        self.assertEqual(cu._projection_cache, {})

    def test_the_shared_array_is_what_the_uncached_path_returns(self):
        """Sharing must not change the pixels — only how often they are computed."""
        g = {'0': {'model': 'm.pt', 'cellChannels': [1, 2]},
             '1': {'model': 'm.pt', 'cellChannels': [1, 2]}}
        cu = self._cu(g)
        frames = self._frames()
        w = _window(frames, 2, wid=1)
        norm = {1: (10.0, 3000.0), 2: (0.0, 4000.0)}
        shared = cu._cached_projection(w, g['1'], norm)
        direct = cu._project_window(frames, g['1'], norm, None)
        np.testing.assert_array_equal(shared, direct)


class ContextChannelsTest(unittest.TestCase):
    """Reading fewer channels renumbers the window's channel axis — the failure mode is silent.

    Dropping the channels coastal never projects is most of the biggest read in the task, but it
    means `context[:, 2]` no longer means image channel 2. These pin that the narrowing asks for
    the right set and that the projection lands on the SAME pixels either way.
    """

    def _utils_for(self, groups):
        from cecelia.utils.coastal_utils import CoastalUtils
        params = {'taskDir': '/tmp', 'models': groups}
        return CoastalUtils(params, _DimUtils(shape=(30, 4, 4, 16, 16)))

    def test_the_union_across_groups_is_requested(self):
        cu = self._utils_for({'0': {'model': 'm.pt', 'cellChannels': [2]},
                              '1': {'model': 'm.pt', 'cellChannels': [0, 2]}})
        self.assertEqual(cu._context_channels(), (0, 2),
                         'one window serves every group; a per-group answer starves the others')

    def test_a_narrowed_window_projects_the_same_pixels(self):
        cu = self._utils_for({'0': {'model': 'm.pt', 'cellChannels': [2]}})
        rng = np.random.default_rng(0)
        full = rng.random((5, 4, 8, 8)).astype(np.float32) * 1000
        norm = {2: (0.0, 1000.0)}

        wide = cu._project_window(full, {'cellChannels': [2]}, norm)
        narrow = cu._project_window(full[:, [0, 2]], {'cellChannels': [2]}, norm,
                                    context_channels=(0, 2))
        np.testing.assert_array_equal(wide, narrow)

    def test_a_group_whose_channel_is_absent_raises(self):
        """Better a stopped task than a label set segmented on the wrong channel."""
        cu = self._utils_for({'0': {'model': 'm.pt', 'cellChannels': [2]}})
        window = np.zeros((5, 2, 8, 8), np.float32)
        with self.assertRaises(ValueError):
            cu._project_window(window, {'cellChannels': [3]}, None, context_channels=(0, 2))


class SharedFlowBetweenTimepointsTest(unittest.TestCase):
    """Consecutive windows overlap by all but one frame; the flow they share must be computed once.

    The cache is the one place in this class that carries state ACROSS calls, so the tests that
    matter are the ones about when it must NOT be used: a different tile is different pixels, and a
    lag pair is never asked for twice and so must not be kept.
    """

    def _utils(self, n_z=2):
        from cecelia.utils.coastal_utils import CoastalUtils
        params = {'taskDir': '/tmp',
                  'models': {'0': {'model': 'm.pt', 'cellChannels': [0]}}}
        cu = CoastalUtils(params, _DimUtils(shape=(30, 2, n_z, 16, 16)))
        cu._get_inference = lambda _mp: _StubInference()
        cu._match_3d = lambda planes, threshold: planes
        cu._pairs = []

        def _metrics(window, center, scales, cumulative, flow_cache=None, window_offset=0):
            # stand in for coastal: ask the cache for the pairs a real call would
            for scale in scales:
                cu._pairs.append(('ask', window_offset + center - 1,
                                  window_offset + center - 1 + scale))
            for k in range(center - cumulative // 2, center + cumulative // 2):
                key = (window_offset + k, window_offset + k + 1)
                if flow_cache is not None and key in flow_cache:
                    continue
                cu._pairs.append(('compute', *key))
                if flow_cache is not None:
                    flow_cache[key] = ('flow', key)
            return np.asarray(window)[center], {'mag_1': np.zeros_like(window[center])}

        cu._flow_metrics = _metrics
        return cu

    def _step(self, cu, t, tile=(0, 16, 0, 16), n_z=2):
        ctx = np.ones((9, 2, n_z, 16, 16), np.float32) * 500
        cu.predict_slice(ctx[3], {'model': 'm.pt', 'cellChannels': [0]},
                         norm_params={0: (0.0, 1000.0)},
                         window=_window(ctx, 3, wid=t, start=t - 3, tile=tile))

    def _computed(self, cu):
        return [p for p in cu._pairs if p[0] == 'compute']

    def test_a_later_timepoint_reuses_the_overlap(self):
        cu = self._utils()
        self._step(cu, 10); first = len(self._computed(cu))
        self._step(cu, 11); second = len(self._computed(cu)) - first
        self.assertGreater(first, 0)
        self.assertLess(second, first,
                        'consecutive windows share all but one frame — most pairs were already had')

    def test_moving_to_another_tile_starts_over(self):
        """A flow is a property of the PIXELS. Reusing one tile's flow for another segments the
        wrong motion, and nothing downstream would report it."""
        cu = self._utils()
        self._step(cu, 10, tile=(0, 16, 0, 16)); first = len(self._computed(cu))
        self._step(cu, 11, tile=(16, 32, 0, 16)); second = len(self._computed(cu)) - first
        self.assertEqual(second, first, 'a different tile must not be served from the old one')
        self.assertEqual(cu._flow_cache_tile, (16, 32, 0, 16))

    def test_only_consecutive_pairs_are_kept(self):
        """A lag pair moves with the centre frame, so it is never requested twice — keeping it grows
        the cache by three dead entries per timepoint per z-plane."""
        cu = self._utils()
        self._step(cu, 10)
        for cache in cu._flow_caches.values():
            for (i, j) in cache:
                self.assertEqual(j, i + 1, f'({i}, {j}) is not a consecutive pair')

    def test_the_cache_does_not_grow_without_bound(self):
        cu = self._utils()
        sizes = []
        for t in range(10, 30):
            self._step(cu, t)
            sizes.append(max(len(c) for c in cu._flow_caches.values()))
        self.assertLessEqual(max(sizes[5:]), 6,
                             f'entries a later timepoint can never read are not being pruned: {sizes}')

    def test_each_z_plane_has_its_own(self):
        cu = self._utils(n_z=3)
        self._step(cu, 10, n_z=3)
        self.assertEqual(sorted(cu._flow_caches), [0, 1, 2])


class SharedFlowBetweenPassesTest(unittest.TestCase):
    """Two-pass runs are a second `models` group, and both groups read the same window.

    The optical flow derived from that window is the most expensive thing in the task and does not
    depend on the group, so computing it twice is pure waste — but sharing it is only safe when the
    two groups really would have computed the same planes. These pin both halves.
    """

    def _two_group_utils(self, second=None):
        from cecelia.utils.coastal_utils import CoastalUtils

        base = {'model': '/nonexistent/model.pt', 'cellChannels': [0]}
        params = {'taskDir': '/tmp',
                  'models': {'0': dict(base), '1': dict(base, **(second or {}))}}
        cu = CoastalUtils(params, _DimUtils(shape=(30, 2, 3, 16, 16)))
        cu._get_inference = lambda _mp: _StubInference()
        cu._match_3d = lambda planes, threshold: planes
        cu._seen = []

        def _metrics(window, center, scales, cumulative, **_):
            cu._seen.append(center)
            return np.asarray(window)[center], {'mag_1': np.zeros_like(window[center])}

        cu._flow_metrics = _metrics
        return cu, params['models']

    def _run(self, cu, mp, wid):
        ctx = np.ones((9, 2, 3, 16, 16), np.float32) * 500
        return cu.predict_slice(ctx[3], mp, norm_params={0: (0.0, 1000.0)},
                                window=_window(ctx, 3, wid=wid))

    def test_a_second_group_reuses_the_first_groups_flow(self):
        cu, models = self._two_group_utils()
        self._run(cu, models['0'], wid=7)
        after_first = len(cu._seen)
        self._run(cu, models['1'], wid=7)

        self.assertEqual(after_first, 3, 'one flow computation per z on the first pass')
        self.assertEqual(len(cu._seen), 3,
                         'the second pass recomputed the flow it was handed by the first')

    def test_a_new_window_is_not_served_from_the_old_one(self):
        cu, models = self._two_group_utils()
        self._run(cu, models['0'], wid=7)
        self._run(cu, models['0'], wid=8)
        self.assertEqual(len(cu._seen), 6, 'a different window must be recomputed, not reused')

    def test_groups_reading_different_channels_do_not_share(self):
        """The cached planes are derived from the PROJECTED window, so a different channel set is a
        different frame — sharing there would segment pass 2 on pass 1's pixels."""
        cu, models = self._two_group_utils(second={'cellChannels': [1]})
        self._run(cu, models['0'], wid=7)
        self._run(cu, models['1'], wid=7)
        self.assertEqual(len(cu._seen), 6)

    def test_a_single_group_run_caches_nothing(self):
        cu = _utils(dim_utils=_DimUtils(shape=(30, 2, 3, 16, 16)))
        cu._match_3d = lambda planes, threshold: planes
        ctx = np.ones((9, 2, 3, 16, 16), np.float32) * 500
        cu.predict_slice(ctx[3], {'model': 'm.pt', 'cellChannels': [0]},
                         norm_params={0: (0.0, 1000.0)}, window=_window(ctx, 3, wid=1))
        self.assertEqual(cu._feature_cache, {},
                         'the common case must not carry the memory of a cache it cannot use')


class PredictSliceTest(unittest.TestCase):

    def test_window_and_centre_reach_the_metric_call(self):
        cu = _utils()
        ctx = np.ones((9, 2, 32, 32), np.float32) * 500
        tile = ctx[3]
        out = cu.predict_slice(tile, {'model': 'm.pt', 'cellChannels': [0]},
                               norm_params={0: (0.0, 1000.0)}, window=_window(ctx, 3))
        self.assertEqual(out.shape, (32, 32))
        self.assertEqual(out.dtype, np.uint32)
        (window, center, scales, cumulative), = cu._seen
        self.assertEqual(window.shape, (9, 32, 32), 'window must stay at TILE extent')
        self.assertEqual(center, 3)
        self.assertEqual(scales, (1, 2, 4, 8))
        self.assertEqual(cumulative, 5)

    def test_3d_runs_one_plane_per_z(self):
        cu = _utils()
        stitched = []
        cu._match_3d = lambda planes, threshold: stitched.append(threshold) or planes

        ctx = np.ones((9, 2, 3, 16, 16), np.float32) * 500
        out = cu.predict_slice(ctx[3], {'model': 'm.pt', 'cellChannels': [0],
                                        'stitchThreshold': 0.25},
                               norm_params={0: (0.0, 1000.0)},
                               window=_window(ctx, 3))

        self.assertEqual(stitched, [0.25], 'Z stitching must get the task param, not a constant')
        self.assertEqual(out.shape, (3, 16, 16))
        self.assertEqual(len(cu._seen), 3, 'one metric computation per z plane')
        for window, center, _, _ in cu._seen:
            self.assertEqual(window.shape, (9, 16, 16))
            self.assertEqual(center, 3)

    def test_dropped_metrics_are_removed_before_the_model_sees_them(self):
        cu = _utils()
        cu._manifest_cache['m.pt'] = {'droppedMetrics': ['divergence']}
        ctx = np.ones((9, 2, 16, 16), np.float32) * 500
        cu.predict_slice(ctx[3], {'model': 'm.pt', 'cellChannels': [0]},
                         norm_params={0: (0.0, 1000.0)}, window=_window(ctx, 3))
        (_, metrics), = cu._stub.calls
        self.assertNotIn('divergence', metrics)
        self.assertIn('mag_1', metrics)

    def test_z_planes_come_back_in_z_order(self):
        """The per-z work is threaded; `_match_3d` stitches neighbours, so ORDER is load-bearing.

        `ThreadPoolExecutor.map` yields in input order regardless of completion order, which is the
        whole reason it is used here rather than `as_completed`. Asserted with a stub whose label
        value IS the plane index and a deliberately uneven per-plane cost, so a result assembled in
        completion order would come back shuffled.
        """
        import time

        cu = _utils(dim_utils=_DimUtils(shape=(30, 2, 8, 16, 16)))
        cu._match_3d = lambda planes, threshold: planes

        n_z = 8
        counter = {'z': 0}

        def _slow_metrics(window, center, scales, cumulative, **_):
            z = counter['z']
            counter['z'] += 1
            time.sleep(0.02 * (n_z - z))     # earlier planes finish LAST
            return np.asarray(window)[center], {'mag_1': np.full_like(window[center], z)}

        cu._flow_metrics = _slow_metrics
        cu._get_inference = lambda _mp: _PlaneIdInference()

        ctx = np.ones((9, 2, n_z, 16, 16), np.float32) * 500
        out = cu.predict_slice(ctx[3], {'model': 'm.pt', 'cellChannels': [0]},
                               norm_params={0: (0.0, 1000.0)}, window=_window(ctx, 3))

        self.assertEqual(out.shape, (n_z, 16, 16))
        np.testing.assert_array_equal(
            [int(out[z][0, 0]) for z in range(n_z)], list(range(n_z)),
            'planes came back out of z order — stitching would pair the wrong neighbours')

    def test_threaded_and_serial_agree(self):
        """Widening the pools is a scheduling change; it must not move a single label."""
        import cecelia.utils.coastal_utils as mod

        ctx = np.linspace(0, 1000, 9 * 2 * 6 * 16 * 16, dtype=np.float32).reshape(9, 2, 6, 16, 16)
        mp = {'model': 'm.pt', 'cellChannels': [0]}

        results = {}
        for workers in ((1, 1), (4, 3)):
            cu = _utils(dim_utils=_DimUtils(shape=(30, 2, 6, 16, 16)))
            cu._match_3d = lambda planes, threshold: planes
            before = (mod.FLOW_WORKERS, mod.PREDICT_WORKERS)
            mod.FLOW_WORKERS, mod.PREDICT_WORKERS = workers
            try:
                results[workers] = cu.predict_slice(
                    ctx[3], mp, norm_params={0: (0.0, 1000.0)}, window=_window(ctx, 3))
            finally:
                mod.FLOW_WORKERS, mod.PREDICT_WORKERS = before

        np.testing.assert_array_equal(results[(1, 1)], results[(4, 3)])

    def test_missing_context_is_an_error_not_a_silent_single_frame_run(self):
        cu = _utils()
        with self.assertRaises(ValueError):
            cu.predict_slice(np.zeros((2, 16, 16), np.float32),
                             {'model': 'm.pt', 'cellChannels': [0]})


if __name__ == '__main__':
    unittest.main()


class FrameIntervalFromManifestTest(unittest.TestCase):
    """What frame rate a model was trained at — or a refusal to guess.

    `physicalScales` records one entry per source movie, unconverted, with its unit (MODEL_VAULT_PLAN
    P0). Turning that into one number is only sound in a narrow case, and every other case has to
    return None rather than something plausible: the caller uses this to decide whether to REWRITE
    the model's temporal scales, so a wrong interval silently feeds the network the wrong motion.
    """

    @staticmethod
    def _fi(manifest):
        from cecelia.utils import coastal_utils
        return coastal_utils.manifest_frame_interval(manifest)

    def test_one_source_movie_in_seconds(self):
        self.assertEqual(self._fi({'physicalScales': {'a': {'t': 15.0, 'tUnit': 's'}}}), 15.0)

    def test_several_movies_that_agree(self):
        self.assertEqual(self._fi({'physicalScales': {'a': {'t': 15.0, 'tUnit': 's'},
                                                      'b': {'t': 15.0, 'tUnit': 's'}}}), 15.0)

    def test_movies_that_disagree_are_unresolvable(self):
        """A model fitted across intervals has no single scale to convert FROM. A mean would be a
        number nobody chose, applied to every scale."""
        self.assertIsNone(self._fi({'physicalScales': {'a': {'t': 15.0, 'tUnit': 's'},
                                                       'b': {'t': 5.0, 'tUnit': 's'}}}))

    def test_a_unit_that_is_not_seconds_is_refused(self):
        """There is no unit converter in this codebase, and inventing one to run over metadata is
        the silent numeric error P0 avoided by recording units in the first place."""
        self.assertIsNone(self._fi({'physicalScales': {'a': {'t': 0.25, 'tUnit': 'min'}}}))

    def test_a_manifest_without_the_field(self):
        self.assertIsNone(self._fi({'temporalScales': [1, 2, 4]}))
        self.assertIsNone(self._fi({}))

    def test_an_entry_missing_t(self):
        """An image whose OME carried no TimeIncrement — `physicalScaleSource: "partial"`."""
        self.assertIsNone(self._fi({'physicalScales': {'a': {'x': 0.33, 'xUnit': 'um'}}}))

    def test_a_nonsense_interval(self):
        self.assertIsNone(self._fi({'physicalScales': {'a': {'t': 0.0, 'tUnit': 's'}}}))


class ResolveScalesForIntervalTest(unittest.TestCase):
    """Re-expressing a model's frame offsets as the same DURATIONS on another movie."""

    @staticmethod
    def _r(scales, cumulative, dt_model, dt_target):
        from cecelia.utils import coastal_utils
        return coastal_utils.resolve_scales_for_interval(scales, cumulative, dt_model, dt_target)

    def test_the_same_interval_changes_nothing_and_says_nothing(self):
        scales, cum, note = self._r([1, 2, 4, 8], 5, 15.0, 15.0)
        self.assertEqual((scales, cum, note), ([1, 2, 4, 8], 5, ''))

    def test_a_faster_movie_needs_more_frames_for_the_same_time(self):
        # 15 s/frame trained, 5 s/frame here: scale 4 was 60 s, which is 12 frames now.
        scales, cum, note = self._r([1, 2, 4, 8], 5, 15.0, 5.0)
        self.assertEqual(scales, [3, 6, 12, 24])
        self.assertEqual(cum, 15)
        self.assertIn('15 s/frame trained, 5 s/frame here', note)

    def test_a_slower_movie_collapses_durations_and_says_so(self):
        # 5 s/frame trained, 15 s/frame here: 5 s, 10 s and 20 s all land on 1 frame.
        scales, cum, note = self._r([1, 2, 4, 8], 5, 5.0, 15.0)
        self.assertEqual(scales, [1, 3])
        self.assertIn('collapsed', note)
        self.assertIn('clamped to 1', note)

    def test_duplicates_never_survive(self):
        """Two declared durations on one frame offset would feed the model the same plane twice
        under two channel names — worse than one plane, and it shifts every later channel."""
        scales, _, _ = self._r([2, 3], 5, 5.0, 15.0)
        self.assertEqual(len(scales), len(set(scales)))

    def test_an_unknown_interval_leaves_everything_alone(self):
        for dt_model, dt_target in ((None, 5.0), (15.0, None), (None, None), (0, 5.0)):
            scales, cum, note = self._r([1, 2, 4], 5, dt_model, dt_target)
            self.assertEqual((scales, cum, note), ([1, 2, 4], 5, ''),
                             f'{dt_model!r}/{dt_target!r} must be a no-op')


class TemporalScaleModeTest(unittest.TestCase):
    """The mode is a run-level decision, and the default must reproduce the old behaviour exactly."""

    _MANIFEST = {'temporalScales': [1, 2, 4, 8], 'cumulativeWindow': 5,
                 'physicalScales': {'a': {'t': 15.0, 'tUnit': 's'}}}

    def test_frames_mode_keeps_the_trained_offsets(self):
        cu = _utils(manifest=self._MANIFEST, dim_utils=_DimUtils(frame_interval=5.0))
        self.assertEqual(cu._temporal_for({'model': '/nonexistent/model.pt'})[0], [1, 2, 4, 8])
        self.assertEqual(cu.TEMPORAL_RADIUS, 8)

    def test_seconds_mode_resolves_them_and_widens_the_window(self):
        """`TEMPORAL_RADIUS` follows, which is the consequence to notice: on a 3x faster movie the
        window is 3x deeper, so the run reads and holds 3x the frames per tile."""
        cu = _utils(manifest=self._MANIFEST, dim_utils=_DimUtils(frame_interval=5.0),
                    params_extra={'temporalScaleMode': 'seconds'})
        self.assertEqual(cu._temporal_for({'model': '/nonexistent/model.pt'})[0], [3, 6, 12, 24])
        self.assertEqual(cu.TEMPORAL_RADIUS, 24)

    def test_the_two_modes_agree_at_the_trained_rate(self):
        for mode in ('frames', 'seconds'):
            cu = _utils(manifest=self._MANIFEST, dim_utils=_DimUtils(frame_interval=15.0),
                        params_extra={'temporalScaleMode': mode})
            self.assertEqual(cu._temporal_for({'model': '/nonexistent/model.pt'})[0],
                             [1, 2, 4, 8], mode)

    def test_an_unknown_mode_raises_rather_than_falling_back(self):
        """A typo must not silently pick a behaviour — the two modes feed the network different data."""
        with self.assertRaises(ValueError):
            _utils(manifest=self._MANIFEST, dim_utils=_DimUtils(frame_interval=5.0),
                   params_extra={'temporalScaleMode': 'Seconds'})

    def test_a_movie_with_no_interval_is_never_rewritten(self):
        """Even in seconds mode: nothing is known, so nothing may be converted."""
        cu = _utils(manifest=self._MANIFEST, dim_utils=_DimUtils(frame_interval=None),
                    params_extra={'temporalScaleMode': 'seconds'})
        self.assertEqual(cu._temporal_for({'model': '/nonexistent/model.pt'})[0], [1, 2, 4, 8])

    def test_a_non_second_time_unit_is_never_rewritten(self):
        cu = _utils(manifest=self._MANIFEST,
                    dim_utils=_DimUtils(frame_interval=5.0, frame_interval_unit='ms'),
                    params_extra={'temporalScaleMode': 'seconds'})
        self.assertEqual(cu._temporal_for({'model': '/nonexistent/model.pt'})[0], [1, 2, 4, 8])
