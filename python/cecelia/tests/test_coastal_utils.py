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


class _DimUtils:
    """Enough of DimUtils for the base's __init__ and the temporal guard."""

    def __init__(self, n_t=30, order='TCZYX', shape=(30, 2, 4, 64, 64)):
        self.im_dim_order = list(order)
        self._shape = shape
        self._n_t = n_t

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


def _utils(model_params=None, dim_utils=None, task_dir='/tmp'):
    """A CoastalUtils with the coastal calls stubbed out."""
    from cecelia.utils.coastal_utils import CoastalUtils

    mp = {'model': '/nonexistent/model.pt', 'cellChannels': [0]}
    mp.update(model_params or {})
    params = {'taskDir': task_dir, 'models': {'0': mp}}

    cu = CoastalUtils(params, dim_utils or _DimUtils())
    stub = _StubInference()
    cu._get_inference = lambda _mp: stub
    cu._stub = stub
    # `flow_metrics_for_frame` stands in for coastal: return the centre frame plus one plane, and
    # record the window it was given.
    cu._seen = []

    def _metrics(window, center, scales, cumulative):
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

        def _metrics(window, center, scales, cumulative):
            cu._seen.append(center)
            return np.asarray(window)[center], {'mag_1': np.zeros_like(window[center])}

        cu._flow_metrics = _metrics
        return cu, params['models']

    def _run(self, cu, mp, context_id):
        ctx = np.ones((9, 2, 3, 16, 16), np.float32) * 500
        return cu.predict_slice(ctx[3], mp, norm_params={0: (0.0, 1000.0)},
                                context=ctx, context_index=3, context_id=context_id)

    def test_a_second_group_reuses_the_first_groups_flow(self):
        cu, models = self._two_group_utils()
        self._run(cu, models['0'], context_id=7)
        after_first = len(cu._seen)
        self._run(cu, models['1'], context_id=7)

        self.assertEqual(after_first, 3, 'one flow computation per z on the first pass')
        self.assertEqual(len(cu._seen), 3,
                         'the second pass recomputed the flow it was handed by the first')

    def test_a_new_window_is_not_served_from_the_old_one(self):
        cu, models = self._two_group_utils()
        self._run(cu, models['0'], context_id=7)
        self._run(cu, models['0'], context_id=8)
        self.assertEqual(len(cu._seen), 6, 'a different window must be recomputed, not reused')

    def test_groups_reading_different_channels_do_not_share(self):
        """The cached planes are derived from the PROJECTED window, so a different channel set is a
        different frame — sharing there would segment pass 2 on pass 1's pixels."""
        cu, models = self._two_group_utils(second={'cellChannels': [1]})
        self._run(cu, models['0'], context_id=7)
        self._run(cu, models['1'], context_id=7)
        self.assertEqual(len(cu._seen), 6)

    def test_a_single_group_run_caches_nothing(self):
        cu = _utils(dim_utils=_DimUtils(shape=(30, 2, 3, 16, 16)))
        cu._match_3d = lambda planes, threshold: planes
        ctx = np.ones((9, 2, 3, 16, 16), np.float32) * 500
        cu.predict_slice(ctx[3], {'model': 'm.pt', 'cellChannels': [0]},
                         norm_params={0: (0.0, 1000.0)}, context=ctx, context_index=3,
                         context_id=1)
        self.assertEqual(cu._feature_cache, {},
                         'the common case must not carry the memory of a cache it cannot use')


class PredictSliceTest(unittest.TestCase):

    def test_window_and_centre_reach_the_metric_call(self):
        cu = _utils()
        ctx = np.ones((9, 2, 32, 32), np.float32) * 500
        tile = ctx[3]
        out = cu.predict_slice(tile, {'model': 'm.pt', 'cellChannels': [0]},
                               norm_params={0: (0.0, 1000.0)}, context=ctx, context_index=3)
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
                               context=ctx, context_index=3)

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
                         norm_params={0: (0.0, 1000.0)}, context=ctx, context_index=3)
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

        def _slow_metrics(window, center, scales, cumulative):
            z = counter['z']
            counter['z'] += 1
            time.sleep(0.02 * (n_z - z))     # earlier planes finish LAST
            return np.asarray(window)[center], {'mag_1': np.full_like(window[center], z)}

        cu._flow_metrics = _slow_metrics
        cu._get_inference = lambda _mp: _PlaneIdInference()

        ctx = np.ones((9, 2, n_z, 16, 16), np.float32) * 500
        out = cu.predict_slice(ctx[3], {'model': 'm.pt', 'cellChannels': [0]},
                               norm_params={0: (0.0, 1000.0)}, context=ctx, context_index=3)

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
                    ctx[3], mp, norm_params={0: (0.0, 1000.0)}, context=ctx, context_index=3)
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
