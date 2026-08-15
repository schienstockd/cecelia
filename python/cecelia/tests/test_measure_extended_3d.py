"""`MeasureUtils._extended_3d_measures` meshes each cell from its BOUNDING BOX, not the volume.

The original form ran `trimesh.voxel.ops.matrix_to_marching_cubes(vol == lb)` once per cell — over
the WHOLE label volume each time. That is O(n_cells x volume), and it was the single largest cost in
the segment+measure pipeline: 9.88 s of an 11.35 s timepoint measured on a 32x420x441 crop
(zolIMa/fXgbTl). It scales the wrong way twice, because a bigger image has both more voxels and more
cells — 6.88x the volume measured 6.54x the per-cell cost — so a full 37x1039x1060 x 181 T movie
projected to roughly 27 hours of measurement for one image.

Two things are pinned here, because the fix is only safe if BOTH hold:

1. **The numbers do not move.** Every mesh measure is identical to the whole-volume form. The vertex
   OFFSET back into the full-volume frame is what makes that true and is easy to drop as redundant:
   the mesh measures themselves are translation-invariant, but the convex hull is computed by qhull,
   which is sensitive to the coordinate range it is handed. Without the offset the three hull-derived
   axis lengths drift by ~1e-3 relative — small enough to pass a sloppy tolerance and wrong. So this
   asserts EXACT equality, and `test_offset_is_what_makes_the_hull_match` fails if the offset goes.

2. **The cost does not follow the volume.** Padding the same cells into a much larger array must not
   make the measurement meaningfully slower. Without that, correctness alone would happily pass a
   reintroduced whole-volume loop.

Part of the Python (analysis-env) suite — run with `pixi run test-py`.
"""
import unittest

import numpy as np
import pandas as pd

try:
    import trimesh  # noqa: F401
    _HAS_TRIMESH = True
except ImportError:
    _HAS_TRIMESH = False

from cecelia.utils.measure_utils import MeasureUtils


#: Cells are placed AWAY FROM THE ORIGIN, because that is the only condition under which the vertex
#: offset is observable. qhull's hull vertices are exact for blobs sitting at coordinates 0-90, so a
#: volume built at the origin cannot tell the offset apart from its absence and would green-light
#: dropping it. At coordinates ~110+ (and every real image is far past that — zolIMa is 1039x1060)
#: the hull axes shift by ~1e-1 without it. Measured: 0.0 at the origin, 1.4e-1 at offset 110.
_CELL_ORIGIN = 110
_VOL_SHAPE = (20, 200, 200)


def _blobs(shape=_VOL_SHAPE, n=12, r=4, seed=0, origin=_CELL_ORIGIN):
    """`n` non-touching spherical-ish blobs of radius `r`, placed away from the array origin."""
    rng = np.random.default_rng(seed)
    vol = np.zeros(shape, dtype=np.uint32)
    zz, yy, xx = np.ogrid[-r:r + 1, -r:r + 1, -r:r + 1]
    ball = (zz ** 2 + yy ** 2 + xx ** 2) <= r ** 2
    placed = 0
    lb = 0
    for _ in range(2000):
        if placed >= n:
            break
        z = int(rng.integers(r + 1, shape[0] - r - 1))
        y = int(rng.integers(origin, shape[1] - r - 1))
        x = int(rng.integers(origin, shape[2] - r - 1))
        sl = (slice(z - r, z + r + 1), slice(y - r, y + r + 1), slice(x - r, x + r + 1))
        if vol[sl].any():
            continue
        lb += 1
        vol[sl][ball] = lb
        placed += 1
    return vol


def _whole_volume_reference(vol, labels, offset_vertices):
    """The ORIGINAL implementation: marching cubes over the full volume, once per label.

    `offset_vertices` mirrors what the bounding-box path does, so the two spellings can be compared
    with the offset present and absent.
    """
    import trimesh
    rows = {}
    for lb in labels:
        mask = (vol == lb)
        if mask.sum() < 4:
            continue
        mesh = trimesh.voxel.ops.matrix_to_marching_cubes(mask)
        if not offset_vertices:
            mesh.vertices -= mesh.bounds[0]        # collapse to the origin, as a bbox crop would
        if not mesh.is_watertight:
            mesh.fill_holes()
        ch = mesh.convex_hull
        verts = np.array(ch.vertices)
        eig = np.sort(np.linalg.eigvalsh(np.cov(verts.T)))[::-1]
        radii = np.sqrt(np.maximum(eig, 0)) * 2
        rows[lb] = dict(
            surface_area=float(mesh.area), volume_mesh=float(mesh.volume),
            convex_hull_area=float(ch.area), convex_hull_volume=float(ch.volume),
            euler_number_mesh=float(mesh.euler_number),
            major_axis_length=float(radii[0]), interm_axis_length=float(radii[1]),
            minor_axis_length=float(radii[2]),
            feret_diameter_max_mesh=float(np.max(mesh.bounding_box.extents)))
    return pd.DataFrame.from_dict(rows, orient='index')


def _measure(vol):
    """Run the real `_extended_3d_measures` over every label in `vol`."""
    labels = [int(x) for x in np.unique(vol[vol > 0])]
    df = pd.DataFrame(index=pd.Index(labels, name='label'))
    mu = MeasureUtils({'taskDir': '/tmp', 'extendedMeasures': True}, dim_utils=None)

    class _Log:
        def log(self, _m):
            pass

    return mu._extended_3d_measures(vol, df, _Log())


@unittest.skipUnless(_HAS_TRIMESH, 'trimesh not installed')
class ExtendedMeasuresMatchWholeVolumeTest(unittest.TestCase):

    def test_every_mesh_measure_is_identical_to_the_whole_volume_form(self):
        """EXACT equality, not a tolerance — and on cells placed away from the origin, so that
        dropping the vertex offset fails here rather than passing on a technicality."""
        vol = _blobs()
        labels = [int(x) for x in np.unique(vol[vol > 0])]
        got = _measure(vol)
        want = _whole_volume_reference(vol, labels, offset_vertices=True)

        self.assertEqual(sorted(got.index), sorted(want.index))
        for col in want.columns:
            self.assertIn(col, got.columns, f'{col} missing from the bounding-box path')
            np.testing.assert_array_equal(
                got.loc[want.index, col].to_numpy(float), want[col].to_numpy(float),
                err_msg=f'{col} differs from the whole-volume form')

    def test_offset_is_what_makes_the_hull_match(self):
        """Pin the PREMISE of the offset, separately from the behaviour above.

        The test above would also pass if qhull were offset-insensitive and the offset were merely
        harmless. This asserts the offset is doing real work at these coordinates, so that if a
        future qhull makes it a no-op, this fails and says so — a prompt to re-verify the rationale
        in `_extended_3d_measures`, not a licence to delete the offset.
        """
        vol = _blobs()
        labels = [int(x) for x in np.unique(vol[vol > 0])]
        no_offset = _whole_volume_reference(vol, labels, offset_vertices=False)
        with_offset = _whole_volume_reference(vol, labels, offset_vertices=True)
        drift = max(
            float(np.abs(no_offset[c] - with_offset[c]).max())
            for c in ('major_axis_length', 'interm_axis_length', 'minor_axis_length'))
        self.assertGreater(
            drift, 0.0,
            'hull axes no longer depend on the coordinate offset — re-check the offset rationale')

    def test_never_compares_a_label_against_the_whole_volume(self):
        """The cost property, pinned STRUCTURALLY rather than by a stopwatch.

        Two weaker forms were tried and rejected, both of which pass on a reintroduced whole-volume
        loop and so would have been worse than nothing:

        * *padding the array* — `find_objects` is one O(volume) pass, so with a handful of synthetic
          cells that single pass dominates and a 6x-larger array legitimately costs ~5x more;
        * *timing against the whole-volume form* — `mesh.convex_hull` is roughly half the cost of the
          bounding-box path (0.71 s of 1.41 s, measured) and is identical in both, so on a volume
          small enough for a fast test the two times converge (0.122 s vs 0.125 s, observed).

        What actually distinguishes them is not how long it takes but WHAT IS TOUCHED: the old form
        evaluated `vol == lb` across every voxel, once per cell. So count the size of every equality
        comparison and require that none of them spans the whole volume. Deterministic, ~instant, and
        it fails for the right reason.
        """
        vol = _blobs()
        seen: list[int] = []

        class _CountingVolume(np.ndarray):
            """Records the extent of every `== label` comparison made against it."""

            def __eq__(self, other):
                seen.append(self.size)
                # plain ndarray back, so downstream (trimesh, .sum()) sees nothing unusual
                return np.asarray(self).__eq__(other)

            def __ne__(self, other):
                return np.logical_not(self.__eq__(other))

            __hash__ = None

        counted = vol.view(_CountingVolume)
        df = _measure(counted)

        self.assertGreater(len(df), 0, 'nothing was measured — the test volume is wrong')
        self.assertTrue(seen, 'no label comparison happened at all — did the mesh path run?')
        self.assertLess(
            max(seen), vol.size,
            f'a label was compared against all {vol.size} voxels of the volume; the per-cell mesh '
            f'must be built from that label\'s bounding box (mesh_utils.build_label_meshes), not '
            f'from `vol == lb`')


if __name__ == '__main__':
    unittest.main()
