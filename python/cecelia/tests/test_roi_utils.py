"""Unit tests for the ROI auto-discovery core (cecelia.utils.roi_utils).

Pure — synthetic points and tiny arrays, no stores. Pins the behaviours the design rests on
(docs/todo/ROI_TRIAGE_PLAN.md): zero cues stay zero after normalisation, per-frame density and its
change across t±1, the time-window split of a sessile hotspot, stable ROI ids, and the mask's
per-plane hole fill (an engulfed fragment is a hole in the macrophage signal)."""
import unittest

import numpy as np

from cecelia.utils import roi_utils


class TestCues(unittest.TestCase):
    def test_rank_normalise_keeps_zeros_zero(self):
        out = roi_utils.rank_normalise([0, 0, 0, 1, 3, 2, np.nan])
        np.testing.assert_allclose(out, [0, 0, 0, 1 / 3, 1, 2 / 3, 0])
        self.assertTrue((roi_utils.rank_normalise([0, 0]) == 0).all())

    def test_density_is_per_frame_and_turnover_compares_neighbouring_frames(self):
        # frame 0: three points together; frame 1: one point at the same place; frame 2: none
        xyz = [[0, 0, 0], [1, 0, 0], [0, 1, 0], [0, 0, 0], [100, 0, 0]]
        t = [0, 0, 0, 1, 2]
        now, prev, nxt = roi_utils.local_density(xyz, t, radius=5)
        np.testing.assert_array_equal(now, [2, 2, 2, 0, 0])
        self.assertTrue(np.isnan(prev[0]))           # no frame -1
        self.assertEqual(nxt[0], 1)                  # t=1 has one point within 5 µm of (0,0,0)
        self.assertEqual(prev[3], 3)                 # t=0 had three points there
        turn = roi_utils.turnover(now, prev, nxt)
        self.assertEqual(turn[0], 1)                 # 2 now vs 1 next
        self.assertEqual(turn[3], 1.5)               # 0 now vs mean(3 before, 0 after: t=2's point is far)
        self.assertTrue(np.isfinite(turn).all())

    def test_nearest_distance_same_frame_and_self_excluded(self):
        xyz = [[0, 0, 0], [0, 0, 0]]
        t = [0, 1]
        d = roi_utils.nearest_distance(xyz, t, [[3, 4, 0], [0, 0, 0]], [0, 5])
        self.assertAlmostEqual(d[0], 5.0)
        self.assertTrue(np.isinf(d[1]))              # no target in frame 1
        # a source that is also a target does not match itself
        d2 = roi_utils.nearest_distance([[0, 0, 0]], [0], [[0, 0, 0], [6, 8, 0]], [0, 0],
                                        exclude_ids=[7], target_ids=[7, 8])
        self.assertAlmostEqual(d2[0], 10.0)

    def test_proximity_is_zero_beyond_reach(self):
        p = roi_utils.proximity([0, 2, 7, np.inf], scale=2, reach=3)
        self.assertAlmostEqual(p[0], 1.0)
        self.assertGreater(p[1], 0)
        self.assertEqual(p[2], 0)
        self.assertEqual(p[3], 0)

    def test_score_weights_and_zero_weight_error(self):
        cues = {"contact": [0, 1], "density": [1, 0]}
        s, normed = roi_utils.score_objects(cues, {"contact": 3, "density": 1})
        np.testing.assert_allclose(s, [0.25, 0.75])
        self.assertIn("contact", normed)
        with self.assertRaises(ValueError):
            roi_utils.score_objects(cues, {"contact": 0, "density": 0})


class TestClustering(unittest.TestCase):
    def test_split_windows_cuts_a_sessile_hotspot(self):
        # one cluster spanning 30 frames → windows of 12: frames 0-11, 12-23, 24-29
        cl = np.zeros(30, dtype=int)
        t = np.arange(30)
        out = roi_utils.split_windows(cl, t, window=12, min_samples=3)
        self.assertEqual(sorted(set(out.tolist())), [0, 1, 2])
        self.assertTrue((out[:12] == out[0]).all())
        # a piece smaller than min_samples becomes noise
        out2 = roi_utils.split_windows(cl, t, window=12, min_samples=7)
        self.assertTrue((out2[24:] == -1).all())
        np.testing.assert_array_equal(roi_utils.split_windows(cl, t, 0, 3), cl)

    def test_cluster_and_build_rois_ranked_with_stable_ids(self):
        rng = np.random.default_rng(0)
        a = rng.normal([10, 10, 5], 0.5, size=(8, 3))
        b = rng.normal([80, 80, 5], 0.5, size=(5, 3))
        xyz = np.vstack([a, b])
        t = np.array([3] * 8 + [9] * 5)
        cl = roi_utils.cluster_4d(xyz, t, eps=3, time_scale=2, min_samples=3)
        self.assertEqual(len(set(cl[cl >= 0])), 2)
        score = np.ones(len(xyz))
        normed = {"density": np.ones(len(xyz))}
        shape = {"t": 20, "z": 10, "y": 100, "x": 100}
        rois = roi_utils.build_rois(cl, xyz, t, score, normed, {"density": 1}, shape, (2, 2, 1))
        self.assertEqual([r["rank"] for r in rois], [1, 2])
        self.assertEqual(rois[0]["nObjects"], 8)          # bigger summed score first
        self.assertEqual((rois[0]["t0"], rois[0]["t1"]), (2, 4))
        self.assertEqual(rois[0]["type"], "density")
        box = rois[0]["box"]
        self.assertTrue(box["x0"] >= 0 and box["x1"] <= 99 and box["z1"] <= 9)
        again = roi_utils.build_rois(cl, xyz, t, score, normed, {"density": 1}, shape, (2, 2, 1))
        self.assertEqual([r["id"] for r in rois], [r["id"] for r in again])
        self.assertEqual(roi_utils.roi_id(2, 4, box), rois[0]["id"])


class TestSites(unittest.TestCase):
    def test_overlapping_boxes_share_the_best_rank_as_site(self):
        b = lambda x0: {"x0": x0, "x1": x0 + 9, "y0": 0, "y1": 9, "z0": 0, "z1": 4}
        rois = [{"rank": 1, "box": b(0)}, {"rank": 2, "box": b(50)}, {"rank": 3, "box": b(1)},
                {"rank": 4, "box": b(52)}]
        roi_utils.assign_sites(rois)
        self.assertEqual([r["site"] for r in rois], [1, 2, 1, 2])
        self.assertAlmostEqual(roi_utils.box_iou(b(0), b(0)), 1.0)
        self.assertEqual(roi_utils.box_iou(b(0), b(50)), 0.0)


class TestMask(unittest.TestCase):
    def test_hole_fill_puts_an_engulfed_fragment_inside(self):
        # a bright ring (the macrophage) around a dark core (the engulfed fragment), 3 planes
        stack = np.zeros((3, 21, 21), dtype=np.float32)
        yy, xx = np.mgrid[:21, :21]
        r = np.hypot(yy - 10, xx - 10)
        stack[:, (r > 3) & (r < 8)] = 20
        mask, thr = roi_utils.channel_mask(stack, threshold=10, sigma_px=(0, 0, 0), min_voxels=10)
        self.assertEqual(thr, 10)
        self.assertTrue(mask[1, 10, 10])               # the core is filled
        labels = np.zeros_like(stack, dtype=np.int32)
        labels[1, 9:12, 9:12] = 4                      # the fragment, in the core
        labels[1, 0:2, 0:2] = 9                        # a free object, far away
        frac, dist = roi_utils.mask_cues(labels, mask, [4, 9, 99], [[1, 10, 10], [1, 0.5, 0.5], [1, 0, 0]],
                                         sampling=(2.0, 0.5, 0.5))
        self.assertEqual(frac[0], 1.0)
        self.assertEqual(frac[1], 0.0)
        self.assertEqual(frac[2], 0.0)                 # absent label
        self.assertEqual(dist[0], 0.0)
        self.assertGreater(dist[1], 0.0)

    def test_auto_threshold_ignores_the_zero_background(self):
        # 95% exactly-zero voxels, dim bleed at 3, a bright cell at 30: Otsu over the non-zero voxels
        # separates the cell from the bleed instead of the bleed from the zeros
        stack = np.zeros((1, 40, 40), dtype=np.float32)
        stack[0, :2, :] = 3
        stack[0, 20:24, 20:24] = 30
        mask, thr = roi_utils.channel_mask(stack, threshold=0, sigma_px=(0, 0, 0), min_voxels=1)
        self.assertTrue(3 <= thr < 30)
        self.assertTrue(mask[0, 22, 22] and not mask[0, 0, 0])
        empty, thr0 = roi_utils.channel_mask(np.zeros((1, 5, 5)), threshold=0, sigma_px=(0, 0, 0))
        self.assertFalse(empty.any())
        self.assertTrue(np.isinf(thr0))

    def test_small_specks_dropped_and_empty_mask_gives_inf(self):
        stack = np.zeros((2, 10, 10), dtype=np.float32)
        stack[0, 0, 0] = 50                            # one-voxel bleed-through speck
        mask, _ = roi_utils.channel_mask(stack, threshold=10, sigma_px=(0, 0, 0), min_voxels=5)
        self.assertFalse(mask.any())
        labels = np.zeros((2, 10, 10), dtype=np.int32)
        labels[1, 5, 5] = 3
        _, dist = roi_utils.mask_cues(labels, mask, [3], [[1, 5, 5]], (1, 1, 1))
        self.assertTrue(np.isinf(dist[0]))


if __name__ == "__main__":
    unittest.main()
