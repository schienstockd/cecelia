"""What `CellposeUtils.predict_slice` sends to cellpose 4, pinned with a fake model.

Cellpose 4 (Cellpose-SAM) removed two arguments and made a third conditional, and it is *lenient*
about the first two — it warns and ignores them. So a stale call does not fail, it silently
segments something else, and only the third combination raises. Verified against
`cellpose==4.2.1.1`; see docs/todo/CELLPOSE_V4_PLAN.md.

  * `channels=`  → warning, ignored. Must not be sent.
  * `model_type=`→ warning, ignored; the model then falls back to `cpsam_v2`. Must not be sent.
  * `z_axis=`    → `ValueError: 2D image processing selected, but z_axis is not None` unless
                   `do_3D` or `stitch_threshold > 0`. That is exactly the call v3 used for
                   "independent 2D slices", so it is the one hard break in the migration.

No cellpose import here: the point is the kwargs, and a real model is 1.2 GB of ViT-L.
Part of the Python (analysis-env) test suite — run with `pixi run test-py`.
"""
import unittest

import numpy as np

from cecelia.utils.cellpose_utils import CellposeUtils


class _FakeModel:
    """Stands in for `cellpose.models.CellposeModel`, recording every `eval` call.

    Reproduces v4's *rejection* rule (z_axis only in 3D mode) so the test fails the same way real
    cellpose would, rather than just asserting on a dict we built ourselves.
    """

    def __init__(self):
        self.calls = []

    def eval(self, x, **kw):
        self.calls.append((x, kw))
        if kw.get('z_axis') is not None and not (kw.get('do_3D') or kw.get('stitch_threshold', 0) > 0):
            raise ValueError('2D image processing selected, but z_axis is not None.')
        if isinstance(x, list):
            return [np.ones(p.shape[:2], np.uint32) for p in x], None, None
        arr = np.asarray(x)
        shape = arr.shape[:-1] if kw.get('channel_axis') in (-1, arr.ndim - 1) else arr.shape
        return np.ones(shape, np.uint32), None, None


class TestCellposeV4CallPath(unittest.TestCase):

    def setUp(self):
        self.seg = CellposeUtils({'taskDir': '/tmp'}, None)   # dim_utils=None → 1 µm/px
        self.model = _FakeModel()
        self.seg._model_cache['cpsam_v2'] = self.model        # skip the real load
        self.plane = np.zeros((8, 8), np.uint16)
        self.plane[2:6, 2:6] = 1000

    def _params(self, **over):
        p = {'model': 'cpsam_v2', 'cellChannels': [0], 'cellDiameter': 4, 'normalise': 99.9}
        p.update(over)
        return p

    def _last_kwargs(self):
        return self.model.calls[-1][1]

    def test_no_retired_kwargs_in_2d(self):
        self.seg.predict_slice(self.plane[None], self._params())
        kw = self._last_kwargs()
        self.assertNotIn('channels', kw)       # ignored by v4 — sending it means we did not migrate
        self.assertNotIn('model_type', kw)
        self.assertIsNone(kw.get('z_axis'))    # 2D: never
        self.assertEqual(kw['diameter'], 4.0)  # µm → px, still a real v4 argument

    def test_single_channel_has_no_channel_axis(self):
        self.seg.predict_slice(self.plane[None], self._params())
        self.assertIsNone(self._last_kwargs()['channel_axis'])

    def test_two_channels_stack_last_and_declare_the_axis(self):
        tile = np.stack([self.plane, self.plane])
        self.seg.predict_slice(tile, self._params(nucChannels=[1]))
        x, kw = self.model.calls[-1]
        self.assertEqual(kw['channel_axis'], -1)
        self.assertEqual(np.asarray(x).shape[-1], 2)   # v4 zero-pads 2 → 3 itself

    def test_3d_with_stitching_passes_z_axis(self):
        tile = np.stack([np.stack([self.plane] * 3)])   # [C=1, Z=3, Y, X]
        out = self.seg.predict_slice(tile, self._params(stitchThreshold=0.2))
        kw = self._last_kwargs()
        self.assertEqual(kw['z_axis'], 0)
        self.assertEqual(kw['stitch_threshold'], 0.2)
        self.assertFalse(kw['do_3D'])
        self.assertEqual(out.shape, (3, 8, 8))

    def test_3d_without_stitching_goes_plane_by_plane(self):
        """The regression: `z_axis` + `stitch_threshold=0` is a ValueError in v4.

        `stitchThreshold` is user-settable to 0 and its tip says "0 = independent 2D slices", so
        this is a reachable GUI state, not a corner case.
        """
        tile = np.stack([np.stack([self.plane] * 3)])
        out = self.seg.predict_slice(tile, self._params(stitchThreshold=0.0))
        x, kw = self.model.calls[-1]
        self.assertIsInstance(x, list)                 # a list of planes, not a volume
        self.assertEqual(len(x), 3)
        self.assertIsNone(kw.get('z_axis'))
        self.assertEqual(out.shape, (3, 8, 8))
        self.assertEqual(out.dtype, np.uint32)

    def test_3d_two_channels_without_stitching(self):
        tile = np.stack([np.stack([self.plane] * 3), np.stack([self.plane] * 3)])
        out = self.seg.predict_slice(tile, self._params(nucChannels=[1], stitchThreshold=0.0))
        x, kw = self.model.calls[-1]
        self.assertEqual(kw['channel_axis'], -1)
        self.assertEqual(np.asarray(x[0]).shape, (8, 8, 2))   # per plane: [Y, X, C]
        self.assertEqual(out.shape, (3, 8, 8))

    def test_default_model_is_a_v4_name(self):
        """The `.get('model', …)` fallback. A v3 name here would load `cpsam_v2` anyway, silently."""
        self.seg._model_cache['cpsam_v2'] = self.model
        self.seg.predict_slice(self.plane[None], {'cellChannels': [0], 'cellDiameter': 4})
        self.assertEqual(len(self.model.calls), 1)


if __name__ == '__main__':
    unittest.main()
