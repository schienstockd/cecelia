"""`utils/skeleton_paths.py`: Ramer–Douglas–Peucker simplification and the branch table's ragged
polyline store, round-tripped through `segment.branching`'s writer and `LabelPropsView`."""

import os
import tempfile
import unittest

import numpy as np

from cecelia.utils.skeleton_paths import (pack_polylines, simplify_polyline, skeleton_polylines,
                                          unpack_polylines)


class SimplifyPolylineTest(unittest.TestCase):
    def test_straight_run_collapses_to_its_endpoints(self):
        line = np.stack([np.zeros(20), np.arange(20)], axis=1)
        np.testing.assert_array_equal(simplify_polyline(line, 0.5), [[0, 0], [0, 19]])

    def test_corner_is_kept(self):
        l = np.array([[0, 0], [0, 1], [0, 2], [0, 3], [1, 3], [2, 3], [3, 3]], float)
        np.testing.assert_array_equal(simplify_polyline(l, 0.5), [[0, 0], [0, 3], [3, 3]])

    def test_every_dropped_point_is_within_tolerance(self):
        rng = np.random.default_rng(0)
        walk = np.cumsum(rng.integers(-1, 2, size=(300, 3)), axis=0).astype(float)
        for tol in (0.5, 1.0, 2.0):
            kept = simplify_polyline(walk, tol)
            self.assertLess(len(kept), len(walk))
            # distance from every original point to the simplified polyline
            segs = list(zip(kept[:-1], kept[1:]))
            def dist(p):
                best = np.inf
                for a, b in segs:
                    ab = b - a; s = np.clip((p - a) @ ab / max(ab @ ab, 1e-12), 0, 1)
                    best = min(best, np.linalg.norm(p - (a + s * ab)))
                return best
            self.assertLessEqual(max(dist(p) for p in walk), tol + 1e-9)

    def test_zero_tolerance_keeps_every_point(self):
        pts = np.array([[0, 0], [0, 1], [0, 2]], float)
        self.assertEqual(len(simplify_polyline(pts, 0)), 3)

    def test_long_path_does_not_recurse(self):
        t = np.linspace(0, 40 * np.pi, 20000)
        spiral = np.stack([t * np.cos(t), t * np.sin(t)], axis=1)
        self.assertGreater(len(simplify_polyline(spiral, 0.5)), 2)


class PackTest(unittest.TestCase):
    def test_round_trip(self):
        lines = [np.array([[0, 0, 0], [1, 2, 3]], np.float32), np.array([[5, 5, 5]] * 3, np.float32)]
        back = unpack_polylines(pack_polylines(lines, 3))
        self.assertEqual(len(back), 2)
        for a, b in zip(lines, back):
            np.testing.assert_array_equal(a, b)

    def test_empty(self):
        p = pack_polylines([], 2)
        self.assertEqual(p["offsets"].tolist(), [0])
        self.assertEqual(p["coords"].shape, (0, 2))


class BranchingWritesPathsTest(unittest.TestCase):
    def test_skan_paths_land_in_the_branch_table_row_aligned(self):
        import skan
        from cecelia.tests.test_branching_anisotropy import _load_runner
        from cecelia.utils.label_props_utils import LabelPropsView
        mod = _load_runner()
        im = np.zeros((21, 41), dtype=bool)
        im[10, 2:39] = True
        im[2:11, 20] = True
        thin = mod._skeletonise(im, pre=0, is_3d=False)
        df, arr, lines = mod._summarise_paths(thin, t_index=3)
        df, arr, _ = mod._globalise_labels(df, arr, 0)
        self.assertEqual(len(lines), len(df))
        with tempfile.TemporaryDirectory() as d:
            out = os.path.join(d, "SHG__branch.h5ad")
            mod._write_branch_h5ad(df, is_3d=False, has_time=True, out_path=out, polylines=lines)
            got, axes = LabelPropsView(out).branch_paths()
        self.assertEqual(axes, ["y", "x"])
        self.assertEqual(len(got), len(df))
        sk = skan.Skeleton(thin)
        for i, line in enumerate(got):
            # the endpoints are skan's, and every simplified point is a skeleton voxel
            full = sk.path_coordinates(i)
            np.testing.assert_array_equal(line[0], full[0])
            np.testing.assert_array_equal(line[-1], full[-1])
            for p in line:
                self.assertTrue(thin[int(p[0]), int(p[1])])
        # the bar's straight halves are two points each
        self.assertTrue(all(len(l) == 2 for l in got))


if __name__ == "__main__":
    unittest.main()
