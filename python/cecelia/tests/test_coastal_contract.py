"""The INSTALLED coastal must accept the calls cecelia makes.

`pixi.toml` pins coastal by rev and calls that rev a HARD FLOOR — cecelia passes arguments that do
not exist before it. Its own comment names the hole this closes: *"Nothing catches a rollback past
the `segment.coastal` / `opticalFlow.train` calls above."* Nothing did, and it bit — a rev bump was
left out of the PR that started passing `flow_cache=`, so `main` had cecelia calling a coastal that
raises `TypeError: flow_metrics_for_frame() got an unexpected keyword argument 'flow_cache'` on the
first tile of every optical-flow run. Nothing failed until someone segmented an image.

Signature-only, deliberately: a call test would need a trained checkpoint and a GPU, which is why
the rest of the coastal-side tests stub coastal out. What breaks a floor is an argument that is not
there, and that is exactly what `inspect.signature` sees.

SKIPPED when coastal is absent, because it legitimately is — the other tests stub it and CI does not
install it. A skip here means "not checked", which is honest; asserting against a stub would be
worse than not testing at all.
"""
import inspect
import unittest


def _coastal():
    try:
        import coastal  # noqa: F401
    except ImportError:
        raise unittest.SkipTest('coastal is not installed — nothing to check against')


def _accepts(fn, names):
    """Which of `names` the callable does NOT accept. `**kwargs` accepts everything."""
    sig = inspect.signature(fn)
    if any(p.kind is inspect.Parameter.VAR_KEYWORD for p in sig.parameters.values()):
        return []
    return [n for n in names if n not in sig.parameters]


class InstalledCoastalContractTest(unittest.TestCase):

    def setUp(self):
        _coastal()

    def test_flow_metrics_for_frame_takes_the_cache_arguments(self):
        """`CoastalUtils._flow_metrics` passes both on every plane of every tile."""
        from coastal.flow import flow_metrics_for_frame
        missing = _accepts(flow_metrics_for_frame,
                           ['temporal_scales', 'cumulative_window', 'value_range',
                            'flow_cache', 'window_offset'])
        self.assertEqual(missing, [], f'installed coastal is older than the pinned floor: '
                                      f'flow_metrics_for_frame lacks {missing}')

    def test_learned_affinity_inference_takes_the_parameters_the_task_exposes(self):
        """Every one of these is a control in `segment.coastal`'s spec, so a missing one is a param
        the GUI offers and the run rejects."""
        from coastal.segment import LearnedAffinityInference
        missing = _accepts(LearnedAffinityInference.__init__,
                           ['affinity_threshold', 'merge_affinity_threshold', 'merge_max_distance',
                            'prob_weight', 'seed_size', 'prob_threshold', 'embedding_blur_sigma',
                            'prob_blur_sigma', 'seed_blur_sigma', 'max_iter',
                            'min_component_size'])
        self.assertEqual(missing, [], f'LearnedAffinityInference lacks {missing}')

    def test_the_smoothing_engine_is_there(self):
        """`smooth_run.py` imports these at MODULE scope, so a rollback errors on load."""
        from coastal.smooth import gated_frames, noise_sigma      # noqa: F401
        from coastal.smooth import spatial_smooth, temporal_smooth  # noqa: F401

    def test_training_takes_what_opticalFlow_train_passes(self):
        from coastal.train import train_with_metrics, load_model  # noqa: F401
        missing = _accepts(train_with_metrics, ['val_frames', 'on_epoch'])
        self.assertEqual(missing, [], f'train_with_metrics lacks {missing}')


if __name__ == '__main__':
    unittest.main()
