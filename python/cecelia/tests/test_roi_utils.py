"""Unit tests for the ROI discovery core (cecelia.utils.roi_utils).

Pure — synthetic points, no stores. Pins the behaviours the design rests on
(docs/todo/ROI_TRIAGE_PLAN.md): groups found with no radius (a tight group and a loose one of the same
count both count), a place objects keep returning to standing out once its window is pooled, windows
kept apart, ranking by summed weight, stable ROI ids, and sites."""
import unittest

import numpy as np

from cecelia.utils import roi_utils


def _blob(rng, centre, n, sd):
    return rng.normal(centre, sd, size=(n, 3))


class TestWeights(unittest.TestCase):
    def test_percentile_rank_and_weights(self):
        np.testing.assert_allclose(roi_utils.percentile_rank([3, 1, 2, np.nan]), [1, 1 / 3, 2 / 3, 0])
        np.testing.assert_allclose(roi_utils.object_weights([], 3), [1, 1, 1])
        w = roi_utils.object_weights([[1, 2], [20, 10]], 2)        # units differ, ranks average
        np.testing.assert_allclose(w, [0.75, 0.75])


class TestGroups(unittest.TestCase):
    def test_groups_at_any_density_need_no_radius(self):
        rng = np.random.default_rng(1)
        tight = _blob(rng, [10, 10, 5], 12, 0.3)          # a fragment cluster: sub-µm spacing
        loose = _blob(rng, [200, 200, 5], 12, 6.0)        # a T-cell cluster: ~6 µm spacing
        bg = rng.uniform([0, 0, 0], [400, 400, 10], size=(40, 3))
        xyz = np.vstack([tight, loose, bg])
        g = roi_utils.window_groups(xyz, np.zeros(len(xyz)), min_objects=5)
        main = lambda s: np.bincount(s[s >= 0]).argmax() if (s >= 0).any() else -1
        gt, gl = main(g[:12]), main(g[12:24])
        self.assertTrue(gt >= 0 and gl >= 0 and gt != gl)
        self.assertGreaterEqual((g[:12] == gt).sum(), 9)      # the core of each, tight or loose
        self.assertGreaterEqual((g[12:24] == gl).sum(), 9)
        self.assertFalse(np.isin(g[24:], [gt, gl]).any())     # no background pulled in

    def test_a_place_objects_keep_returning_to_piles_up_over_the_window(self):
        # each frame: 3 objects at one spot (too few to group alone) among 15 scattered ones;
        # pooled over 12 frames the spot holds 36 and stands out, the scatter does not
        rng = np.random.default_rng(3)
        pts, t = [], []
        for f in range(12):
            pts += [_blob(rng, [50, 50, 5], 3, 0.5), rng.uniform([0, 0, 0], [300, 300, 10], size=(15, 3))]
            t += [f] * 18
        xyz, t = np.vstack(pts), np.array(t)
        spot = np.tile(np.r_[np.ones(3, bool), np.zeros(15, bool)], 12)
        g = roi_utils.window_groups(xyz, t, min_objects=10)
        self.assertGreaterEqual((g[spot] >= 0).mean(), 0.8)
        # HDBSCAN finds SOME groups in the scatter too (it is relative); the spot is the biggest
        sizes = np.bincount(g[g >= 0])
        self.assertEqual(sizes.argmax(), np.bincount(g[spot][g[spot] >= 0]).argmax())
        # windows are separate: the same spot in frames 12-23 is another group
        g2 = roi_utils.window_groups(np.vstack([xyz, xyz]), np.r_[t, t + 12], min_objects=10)
        self.assertNotEqual(np.bincount(g2[:len(t)][g2[:len(t)] >= 0]).argmax(),
                            np.bincount(g2[len(t):][g2[len(t):] >= 0]).argmax())


class TestRois(unittest.TestCase):
    def test_find_rois_ranks_by_summed_weight_with_stable_ids(self):
        rng = np.random.default_rng(0)
        frame = lambda: [_blob(rng, [20, 20, 5], 10, 1.0), _blob(rng, [80, 80, 5], 6, 1.0),
                         rng.uniform([0, 0, 0], [100, 100, 10], size=(15, 3))]
        xyz = np.vstack(frame() + frame())
        t = np.array([3] * 31 + [4] * 31)
        shape = {"t": 20, "z": 10, "y": 100, "x": 100}
        rois, _ = roi_utils.find_rois(xyz, xyz, t, shape, min_objects=5)
        self.assertEqual([r["rank"] for r in rois], list(range(1, len(rois) + 1)))
        top = rois[0]                                      # the big group first, both frames pooled
        self.assertTrue(14 <= top["nObjects"] <= 22 and top["nFrames"] == 2)
        self.assertTrue(abs(top["centroid"]["x"] - 20) < 2)
        self.assertEqual((top["t0"], top["t1"]), (2, 5))
        box = rois[0]["box"]
        self.assertTrue(box["x0"] >= 0 and box["x1"] <= 99 and box["z1"] <= 9)
        again, _ = roi_utils.find_rois(xyz, xyz, t, shape, min_objects=5)
        self.assertEqual([r["id"] for r in rois], [r["id"] for r in again])
        self.assertEqual(roi_utils.roi_id(2, 5, box), rois[0]["id"])
        # a measure can outrank size: the small group's objects score highest
        w = np.array(([0.1] * 10 + [1.0] * 6 + [0.1] * 15) * 2)
        ranked, _ = roi_utils.find_rois(xyz, xyz, t, shape, min_objects=5, weight=w)
        self.assertTrue(abs(ranked[0]["centroid"]["x"] - 80) < 2)

    def test_excess_ranks_a_compact_group_above_a_bigger_loose_one(self):
        # a loose group of 40 no denser than the background vs a compact group of 15: by count the
        # loose one wins; by excess over the background the compact one does and the loose one,
        # holding fewer than its box would at the average density, is dropped
        rng = np.random.default_rng(4)
        compact = _blob(rng, [20, 20, 5], 15, 0.5)
        loose = rng.normal([120, 120, 5], [12, 12, 1], size=(40, 3))
        bg = rng.uniform([0, 0, 0], [200, 200, 10], size=(2000, 3))
        xyz = np.vstack([compact, loose, bg])
        groups = np.r_[np.zeros(15, int), np.ones(40, int), np.full(2000, -1)]
        t = np.zeros(len(xyz), int)
        w = np.ones(len(xyz))
        exp = roi_utils.background_expected(groups, xyz, t, w, voxel_um=(1, 1, 1))
        self.assertLess(exp[0], 2)
        self.assertGreater(exp[1], 40)
        shape = {"t": 1, "z": 10, "y": 200, "x": 200}
        rois = roi_utils.build_rois(groups, xyz, t, w, shape, expected=exp)
        self.assertEqual(len(rois), 1)
        self.assertTrue(abs(rois[0]["centroid"]["x"] - 20) < 2)
        self.assertAlmostEqual(rois[0]["score"], 15 - exp[0])
        self.assertEqual(roi_utils.build_rois(groups, xyz, t, w, shape, expected={0: 99, 1: 99}), [])

    def test_overlapping_boxes_share_the_best_rank_as_site(self):
        b = lambda x0: {"x0": x0, "x1": x0 + 9, "y0": 0, "y1": 9, "z0": 0, "z1": 4}
        rois = [{"rank": 1, "box": b(0)}, {"rank": 2, "box": b(50)}, {"rank": 3, "box": b(1)},
                {"rank": 4, "box": b(52)}]
        roi_utils.assign_sites(rois)
        self.assertEqual([r["site"] for r in rois], [1, 2, 1, 2])
        self.assertAlmostEqual(roi_utils.box_iou(b(0), b(0)), 1.0)
        self.assertEqual(roi_utils.box_iou(b(0), b(50)), 0.0)


if __name__ == "__main__":
    unittest.main()
