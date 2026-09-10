"""What `CellposeUtils.predict_slice` sends to cellpose 3 in the opt-in `cellpose-v3` env.

The v3 API is what v4 REMOVED: `channels=[cyto, nuc]` (1-indexed positional pair, `[0, 0]` for
grayscale) and `z_axis=0, do_3D=False` for independent 2D slices (the exact call v4 raises on).
This test pins that the branch translates the internal `[cell, nuc]` last-axis stack into the right
v3 `channels` pair, keeps `z_axis` legal in the two 3D shapes, and never drips a v4-only kwarg.

Run under the DEFAULT env (v4 cellpose is present); the branch decision reads
`cecelia.utils.cellpose_utils._CELLPOSE_MAJOR`, which we monkey-patch to `3` here — no real cellpose
3 import, since neither env has both versions. See docs/todo/CELLPOSE_V3_OPTIN_PLAN.md.
"""
import unittest
from unittest import mock

import numpy as np

from cecelia.utils import cellpose_utils as cpu


class _FakeV3Model:
    """Stands in for `cellpose.models.CellposeModel` under cellpose 3. Records eval() calls."""

    def __init__(self):
        self.calls = []

    def eval(self, x, **kw):
        self.calls.append((x, kw))
        arr = np.asarray(x)
        return np.ones(arr.shape if kw.get('channel_axis') is None else arr.shape[:-1], np.uint32), None, None


class TestCellposeV3CallPath(unittest.TestCase):

    def setUp(self):
        self.major_patch = mock.patch.object(cpu, '_CELLPOSE_MAJOR', 3)
        self.major_patch.start()
        self.seg = cpu.CellposeUtils({'taskDir': '/tmp'}, None)
        self.model = _FakeV3Model()
        self.seg._model_cache['cyto3'] = self.model            # skip the real load
        self.plane = np.zeros((8, 8), np.uint16)
        self.plane[2:6, 2:6] = 1000

    def tearDown(self):
        self.major_patch.stop()

    def _params(self, **over):
        p = {'model': 'cyto3', 'cellChannels': [0], 'cellDiameter': 4, 'normalise': 99.9}
        p.update(over)
        return p

    def _last_kwargs(self):
        return self.model.calls[-1][1]

    def test_no_nuc_sends_grayscale_channels_pair(self):
        self.seg.predict_slice(self.plane[None], self._params())
        self.assertEqual(self._last_kwargs()['channels'], [0, 0])   # grayscale

    def test_with_nuc_sends_cell_and_nuc_pair(self):
        tile = np.stack([self.plane, self.plane])
        self.seg.predict_slice(tile, self._params(nucChannels=[1]))
        self.assertEqual(self._last_kwargs()['channels'], [1, 2])

    def test_2d_never_sends_z_axis(self):
        self.seg.predict_slice(self.plane[None], self._params())
        self.assertIsNone(self._last_kwargs().get('z_axis'))

    def test_3d_without_stitching_uses_z_axis_zero(self):
        """The call v4 rejects: `z_axis=0, do_3D=False, stitch_threshold=0` is what v3 takes for
        independent 2D slices — one eval() call, not a list-of-planes fallback."""
        tile = np.stack([np.stack([self.plane] * 3)])   # [C=1, Z=3, Y, X]
        self.seg.predict_slice(tile, self._params(stitchThreshold=0.0))
        kw = self._last_kwargs()
        self.assertEqual(kw['z_axis'], 0)
        self.assertFalse(kw['do_3D'])
        # Not a list-of-planes shim: v3 handles the Z axis natively.
        self.assertEqual(len(self.model.calls), 1)

    def test_3d_with_stitching_passes_threshold(self):
        tile = np.stack([np.stack([self.plane] * 3)])
        self.seg.predict_slice(tile, self._params(stitchThreshold=0.4))
        kw = self._last_kwargs()
        self.assertEqual(kw['z_axis'], 0)
        self.assertEqual(kw['stitch_threshold'], 0.4)
        self.assertFalse(kw['do_3D'])

    def test_unknown_v3_model_name_rejected(self):
        """Only cyto2/cyto3 are shipped in the v3 env; anything else raises with a clear message
        rather than silently loading something unexpected — v3 does not warn+substitute the way v4
        does, but the picker could send a stale name from a chain saved before v3 was added."""
        seg = cpu.CellposeUtils({'taskDir': '/tmp'}, None)
        with self.assertRaises(ValueError) as ctx:
            seg._get_model('nuclei')
        self.assertIn('cyto2', str(ctx.exception))
        self.assertIn('cyto3', str(ctx.exception))


class TestCellposeMajorVersion(unittest.TestCase):

    def test_major_version_is_an_int(self):
        # The whole branching decision hangs off this — a bad parse must never silently take the
        # wrong path. The fallback in _cellpose_major_version is 4 (default env), so under any
        # sane env this is 3 or 4.
        self.assertIn(cpu._cellpose_major_version(), (3, 4))

    def test_v3_builtins_are_the_two_we_ship(self):
        # If the picker adds a v3 name, add it here too — the _get_model guard reads this set.
        self.assertEqual(set(cpu._CELLPOSE_V3_BUILTINS), {'cyto2', 'cyto3'})


if __name__ == '__main__':
    unittest.main()
