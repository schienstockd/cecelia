# RESULT-REVIEW: result-453994c7
"""Find ROIs, Group by an event id + Rank by a measure: an event ranks by HOW MUCH of the measure it has,
not by its size. Summing per-object percentile weights (never 0) ranked a 10-cell event no intact cell
preceded above a 3-cell event that was fully preceded — "rank by preceded" was ranking by size."""

import unittest

import numpy as np

import cecelia.utils.roi_utils as roi_utils


class RankByPrecededTest(unittest.TestCase):
    def test_a_fully_preceded_small_event_outranks_an_unpreceded_big_one(self):
        ids = np.array([1] * 10 + [2] * 3, dtype=float)
        preceded = np.array([0.0] * 10 + [1.0] * 3)
        t = np.arange(13)
        xyz = np.array([[10, 10, 1]] * 10 + [[60, 60, 1]] * 3, dtype=float)
        shape = {"t": 20, "z": 3, "y": 100, "x": 100}
        w = roi_utils.object_weights([preceded], len(preceded))
        rois, _ = roi_utils.find_rois(xyz, xyz, t, shape, min_objects=2, weight=w, group_ids=ids)
        self.assertEqual(rois[0]["nObjects"], 3)
        # with no measure every weight is 1: the bigger event still ranks first, by count
        rois, _ = roi_utils.find_rois(xyz, xyz, t, shape, min_objects=2, group_ids=ids)
        self.assertEqual(rois[0]["nObjects"], 10)


if __name__ == "__main__":
    unittest.main()
