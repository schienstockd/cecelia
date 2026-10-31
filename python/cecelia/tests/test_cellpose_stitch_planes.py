"""`stitch_planes` makes the same 3D objects as cellpose's `stitch3D`, without its dense matrix.

`stitch3D` builds a dense (labels on this plane) × (largest id so far) overlap matrix per plane pair,
which grows with the square of the tile area: 16 GB and 20 minutes on one 26-plane 2024 px tile.
`stitch_planes` counts only the pairs that share a pixel. The thing to pin is that the OBJECTS — which
voxels belong together — are identical, compared as partitions (ids may differ).

Ground truth is cellpose's own `stitch3D` + our `split_z_gaps` (the production path before), run on
the same per-plane labels. Run with `pixi run test-py`.
"""
import unittest

import numpy as np

from cecelia.utils.cellpose_utils import split_z_gaps, stitch_planes


def _same_partition(a, b):
    """True when a and b group the voxels identically, whatever the ids."""
    if not np.array_equal(a > 0, b > 0):
        return False
    fg = a > 0
    pairs = np.unique(np.stack([a[fg], b[fg]]), axis=1)
    # a bijection between ids: every id on each side appears in exactly one pair
    return (len(np.unique(pairs[0])) == pairs.shape[1]
            and len(np.unique(pairs[1])) == pairs.shape[1])


def _random_planes(rng, n_z=8, size=64, n_blobs=25, empty=()):
    """Per-plane 2D labels from 3D blobs that drift a little between planes — so stitching has
    real decisions to make: partial overlaps, splits, and blobs starting and ending mid-stack."""
    vol = np.zeros((n_z, size, size), np.uint32)
    for i in range(n_blobs):
        z0, z1 = sorted(rng.integers(0, n_z, 2))
        cy, cx = rng.integers(6, size - 6, 2)
        r = rng.integers(3, 7)
        for z in range(z0, z1 + 1):
            cy += rng.integers(-2, 3)
            cx += rng.integers(-2, 3)
            yy, xx = np.ogrid[:size, :size]
            disk = (yy - cy) ** 2 + (xx - cx) ** 2 <= r * r
            vol[z][disk & (vol[z] == 0)] = i + 1
    planes = np.zeros_like(vol)
    for z in range(n_z):
        if z in empty:
            continue
        planes[z] = _renumber(vol[z])          # 1..n per plane, as cellpose's per-plane output is
    return planes


def _renumber(plane):
    ids = np.unique(plane[plane > 0])
    lut = np.zeros(int(plane.max()) + 1, np.uint32)
    lut[ids] = np.arange(1, len(ids) + 1)
    return lut[plane]


class StitchPlanesTest(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        try:
            from cellpose.utils import stitch3D
        except ImportError:                              # pragma: no cover
            raise unittest.SkipTest('cellpose not installed')
        cls.stitch3D = staticmethod(stitch3D)

    def _reference(self, planes, thr):
        return split_z_gaps(self.stitch3D(planes.copy(), stitch_threshold=thr))

    def test_same_objects_as_stitch3D_on_random_stacks(self):
        rng = np.random.default_rng(0)
        for trial in range(40):
            planes = _random_planes(rng)
            for thr in (0.1, 0.25, 0.5):
                ours = stitch_planes(planes, thr)
                self.assertTrue(_same_partition(ours, self._reference(planes, thr)),
                                f'trial {trial}, threshold {thr}')

    def test_same_objects_across_empty_planes(self):
        """The case `stitch3D` gets wrong on its own (id reuse) and `split_z_gaps` repairs."""
        rng = np.random.default_rng(1)
        for empty in ((0,), (2,), (0, 1, 5), (3, 4)):
            planes = _random_planes(rng, empty=empty)
            ours = stitch_planes(planes, 0.25)
            self.assertTrue(_same_partition(ours, self._reference(planes, 0.25)), f'empty {empty}')
            # and ours needs no repair: split_z_gaps leaves it alone
            self.assertTrue(np.array_equal(split_z_gaps(ours), ours))

    def test_a_threshold_above_every_iou_joins_nothing(self):
        planes = np.zeros((3, 8, 8), np.uint32)
        planes[:, 2:6, 2:6] = 1
        planes[1, 2:6, 2:4] = 0                        # plane 1 half-covers: IoU 0.5 to each side
        out = stitch_planes(planes, 0.9)
        self.assertEqual(len(np.unique(out[out > 0])), 3)
        out = stitch_planes(planes, 0.4)
        self.assertEqual(len(np.unique(out[out > 0])), 1)

    def test_cost_does_not_grow_with_the_stack_id_range(self):
        """The reason this exists: `stitch3D`'s matrix width is the largest id so far. Ours only
        touches co-occurring pairs, so ids far apart in value cost nothing extra."""
        planes = np.zeros((2, 4, 4), np.uint32)
        planes[0, :2, :2] = 3_000_000                  # a huge id below
        planes[1, :2, :2] = 1
        out = stitch_planes(planes, 0.25)
        self.assertEqual(int(out[1, 0, 0]), 3_000_000)


if __name__ == '__main__':
    unittest.main()
