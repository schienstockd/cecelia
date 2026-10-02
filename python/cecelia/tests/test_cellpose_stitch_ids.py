"""One label = one object after cellpose's Z stitching (`split_z_gaps`).

cellpose's `stitch3D` reuses ids across an empty plane before its first match: plane 0's cell 1 and a
different cell two planes down both come back as label 1. See the `cellpose_utils` module docstring.

Part of the Python (analysis-env) test suite — run with `pixi run test-py`.
"""
import unittest

import numpy as np

from cecelia.utils.cellpose_utils import CellposeUtils, split_z_gaps


def _collided():
    """Two cells on planes 0 and 2 with an empty plane between them, sharing label 1."""
    m = np.zeros((3, 10, 10), np.uint32)
    m[0, 1:3, 1:3] = 1
    m[2, 7:9, 7:9] = 1
    return m


class TestSplitZGaps(unittest.TestCase):

    def test_gap_splits_into_two_labels(self):
        out = split_z_gaps(_collided())
        self.assertEqual(out[0, 1, 1], 1)                # the lowest run keeps its id
        self.assertNotIn(out[2, 7, 7], (0, 1))           # the other run gets a fresh one
        np.testing.assert_array_equal(out > 0, _collided() > 0)   # no voxel gained or lost

    def test_contiguous_label_is_untouched(self):
        m = np.zeros((3, 6, 6), np.uint32)
        m[0:3, 1:4, 1:4] = 5
        m[1, 4:6, 4:6] = 2
        np.testing.assert_array_equal(split_z_gaps(m), m)

    def test_three_runs_get_three_ids(self):
        m = np.zeros((5, 4, 4), np.uint32)
        m[0, 0, 0] = m[2, 1, 1] = m[4, 2, 2] = 3
        out = split_z_gaps(m)
        self.assertEqual(len({out[0, 0, 0], out[2, 1, 1], out[4, 2, 2]}), 3)

    def test_fresh_ids_clear_every_existing_label(self):
        m = _collided()
        m[1, 5, 5] = 9                                   # the current max
        out = split_z_gaps(m)
        self.assertGreater(out[2, 7, 7], 9)

    def test_returns_uint32_and_does_not_mutate_the_input(self):
        m = _collided().astype(np.uint16)
        out = split_z_gaps(m)
        self.assertEqual(out.dtype, np.uint32)
        np.testing.assert_array_equal(m, _collided())


class TestAgainstRealStitch3D(unittest.TestCase):
    """The guard against the bug it is for: real cellpose output, not a mask we built ourselves."""

    def setUp(self):
        try:
            from cellpose.utils import stitch3D
        except ImportError:
            self.skipTest('cellpose not installed in this env')
        self.stitch3D = stitch3D

    def test_stitch3d_collision_is_repaired(self):
        # Fresh per-plane ids, as cellpose's per-plane eval produces them.
        stitched = self.stitch3D(_collided(), stitch_threshold=0.2)
        out = split_z_gaps(stitched)
        self.assertNotEqual(out[0, 1, 1], out[2, 7, 7])

    def test_a_real_stitch_survives(self):
        m = np.zeros((3, 10, 10), np.uint32)
        m[:, 2:6, 2:6] = 1                               # one cell through all three planes
        stitched = self.stitch3D(m.copy(), stitch_threshold=0.2)
        out = split_z_gaps(stitched)
        self.assertEqual(len(np.unique(out[out > 0])), 1)


class _CollidingModel:
    """A cellpose model whose stitched output carries the collision; per-plane output is all 1s."""

    def eval(self, x, **kw):
        if isinstance(x, list):
            return [np.ones(p.shape[:2], np.uint32) for p in x], None, None
        return _collided(), None, None


class TestPredictSliceApplies(unittest.TestCase):

    def setUp(self):
        self.seg = CellposeUtils({'taskDir': '/tmp'}, None)
        self.seg._model_cache['cpsam_v2'] = _CollidingModel()
        self.tile = np.zeros((1, 3, 10, 10), np.uint16)
        self.tile[0, :, 2:6, 2:6] = 1000

    def _params(self, **over):
        p = {'model': 'cpsam_v2', 'cellChannels': [0], 'cellDiameter': 4, 'normalise': 99.9}
        p.update(over)
        return p

    def test_stitched_3d_is_split(self):
        out = self.seg.predict_slice(self.tile, self._params(stitchThreshold=0.2))
        self.assertNotEqual(out[0, 1, 1], out[2, 7, 7])

    def test_independent_slices_keep_per_plane_ids(self):
        """`stitchThreshold=0` numbers labels per plane BY DESIGN; that is not a collision to repair."""
        out = self.seg.predict_slice(self.tile, self._params(stitchThreshold=0.0))
        self.assertTrue((out == 1).all())


if __name__ == '__main__':
    unittest.main()
