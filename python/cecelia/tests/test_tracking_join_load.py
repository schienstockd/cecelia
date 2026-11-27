"""btrack's optimiser is stopped before it starts when the join radius swamps it — `_check_join_load`.

The optimiser runs with no time limit, so a `distThresh` far above the object spacing does not fail:
it never returns (a dense 2D movie at 10 µm offered each track end thousands of joins). The run must
stop before the optimiser with a suggested `distThresh`, and the count it judges by must be the one
btrack's hypothesis engine actually builds.
"""
import os
import unittest
from collections import Counter
from unittest import mock

import numpy as np
import pandas as pd
import btrack
from btrack import config as btrack_config, utils as btrack_utils
from btrack.optimise import optimiser

from cecelia.utils import tracking_utils as tu

_REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
_CONFIG = os.path.join(_REPO, "app", "src", "tasks", "tracking", "cell_config.json")


class _Log:
    def __init__(self):
        self.lines = []

    def log(self, msg, *_a, **_k):
        self.lines.append(msg)


def _params(**over):
    p = {'taskDir': '/tmp', 'btrackConfig': _CONFIG, 'maxSearchRadius': 0.5, 'maxLost': 1,
         'trackBranching': False, 'minTimepoints': 2, 'accuracy': 0.8, 'probToAssign': 0.8,
         'noiseInital': 300, 'noiseProcessing': 100, 'noiseMeasurements': 100, 'distThresh': 10,
         'timeThresh': 5, 'segmentationMissRate': 0.1, 'lambdaLink': 5, 'lambdaBranch': 50,
         'lambdaTime': 5, 'lambdaDist': 5, 'thetaTime': 5, 'thetaDist': 5,
         'physicalSizes': [1.0, 1.0, 1.0]}
    p.update(over)
    return p


class _Tracklet:
    """The fields of a btrack `Tracklet` that `tracklet_ends` reads: a one-object track."""
    def __init__(self, t, x, y):
        self.t, self.x, self.y, self.z = [float(t)], [x], [y], [0.0]


def _dense(n=150, frames=12, side=40.0, seed=1):
    """Puncta ~3 µm apart that re-appear at random each frame, so tracklets stay short and many."""
    rng = np.random.default_rng(seed)
    rows, lab = [], 1
    for t in range(frames):
        for x, y in rng.uniform(0, side, (n, 2)):
            rows.append((float(t), x, y, 0.0, lab))
            lab += 1
    return pd.DataFrame(rows, columns=["t", "x", "y", "z", "label_id"])


def _track_frame_to_frame(tracker, df, dist, branching):
    cfg = btrack_config.load_config(_CONFIG)
    cfg.hypothesis_model.dist_thresh = dist
    cfg.hypothesis_model.time_thresh = 5
    if branching:
        cfg.hypothesis_model.hypotheses = list(cfg.hypothesis_model.hypotheses) + ["P_branch"]
    tracker.configure(cfg)
    tracker.max_search_radius = 0.5
    tracker.append(btrack_utils.objects_from_dict({k: df[k].values for k in df.columns}))
    tracker.track(step_size=100)


class TestJoinLoad(unittest.TestCase):
    def test_count_matches_btrack_hypotheses(self):
        df = _dense()
        for dist, branching in ((1.0, True), (3.0, False)):
            with btrack.BayesianTracker() as tracker:
                _track_frame_to_frame(tracker, df, dist, branching)
                kinds = Counter(h.hypothesis_type.name for h in tracker.hypotheses())
                starts, ends = tu.tracklet_ends(tracker.tracks)
            expected = kinds["LINK"] + kinds.get("DIVIDE", 0)
            self.assertGreater(expected, 0)
            self.assertEqual(tu.join_candidates(starts, ends, dist, 5, branching), expected,
                             f"distThresh={dist} branching={branching}")

    def test_oversized_join_radius_stops_before_the_optimiser(self):
        log = _Log()
        utils = tu.BayesianTrackingUtils(_params(distThresh=10), log)
        with mock.patch.object(optimiser.TrackOptimiser, "optimise",
                               side_effect=AssertionError("optimiser started")):
            with self.assertRaises(SystemExit) as stop:
                utils._track(_dense())
        msg = str(stop.exception)
        self.assertIn("Distance threshold 10 µm", msg)
        self.assertRegex(msg, r"set Distance threshold to [0-9.]+ µm")
        self.assertTrue(any(line.startswith(">> Optimiser:") for line in log.lines))

    def test_suggestion_is_within_the_healthy_load(self):
        with btrack.BayesianTracker() as tracker:
            _track_frame_to_frame(tracker, _dense(), 10, False)
            starts, ends = tu.tracklet_ends(tracker.tracks)
        fit = tu.suggest_dist_thresh(starts, ends, 10, 5, False)
        self.assertIsNotNone(fit)
        self.assertLess(fit, 10)
        self.assertGreaterEqual(fit, tu.MIN_DIST_THRESH)
        n = len(starts)
        self.assertLessEqual(tu.join_candidates(starts, ends, fit, 5, False) / n,
                             tu._HEALTHY_JOINS_PER_TRACKLET)

    def test_sub_unit_dist_thresh_is_refused(self):
        """Below 1 btrack's bins are 0 wide (undefined, see `MIN_DIST_THRESH`): refused before it runs."""
        with mock.patch.object(tu.btrack, "BayesianTracker",
                               side_effect=AssertionError("btrack started")):
            with self.assertRaises(SystemExit) as stop:
                tu.BayesianTrackingUtils(_params(distThresh=0.5), _Log())._track(_dense())
        self.assertIn("cannot join track ends below 1 µm", str(stop.exception))

    def test_suggestion_is_the_largest_healthy_1_2_5_step(self):
        """Ten tracklets end at one spot; one starts 15 µm away, three 30 µm away. Per tracklet (14):
        10/14 joins within 20 µm, 40/14 within 50 — so 20 is the largest 1-2-5 step at most 2."""
        trk = [_Tracklet(0, 0.0, 0.0) for _ in range(10)] + [_Tracklet(1, 15.0, 0.0)] + \
              [_Tracklet(1, x, y) for x, y in ((-30.0, 0.0), (0.0, 30.0), (0.0, -30.0))]
        starts, ends = tu.tracklet_ends(trk)
        self.assertEqual(tu.suggest_dist_thresh(starts, ends, 100, 5, False), 20)

    def test_branching_hint_only_when_no_distance_is_enough(self):
        # one end, 30 starts within 1 µm: 30 links + 435 divisions over 31 tracklets, at any distance
        crowded = [_Tracklet(0, 0.0, 0.0)] + [_Tracklet(1, 0.03 * k, 0.0) for k in range(30)]
        utils = tu.BayesianTrackingUtils(_params(distThresh=5, trackBranching=True), _Log())
        with self.assertRaises(SystemExit) as stop:
            utils._check_join_load(crowded)
        self.assertIn("turn off Track branching", str(stop.exception))
        self.assertIn("even 1 µm is too far", str(stop.exception))

        # one partner within 2 µm, 30 more 3-4 µm out: 2 µm fits, so no branching hint
        spread = [_Tracklet(0, 0.0, 0.0), _Tracklet(1, 0.5, 0.0)] + \
                 [_Tracklet(1, 3.0 + 0.03 * k, 0.0) for k in range(30)]
        with self.assertRaises(SystemExit) as stop:
            utils._check_join_load(spread)
        self.assertIn("set Distance threshold to 2 µm", str(stop.exception))
        self.assertNotIn("branching", str(stop.exception))

    def test_fractional_threshold_warns_only_when_joins_are_dropped(self):
        """End at 0, one start 0.5 away: inside btrack's 1-wide bins, so 1.5 drops nothing."""
        log = _Log()
        tu.BayesianTrackingUtils(_params(distThresh=1.5), log)._check_join_load(
            [_Tracklet(0, 0.2, 0.2), _Tracklet(1, 0.7, 0.2)])
        self.assertFalse(any("[WARN]" in line for line in log.lines), log.lines)

    def test_ordinary_spacing_runs_the_optimiser(self):
        """Cells far apart relative to distThresh: no stop, tracks come back."""
        rows = [(float(t), 10.0 + 40 * k + 2.0 * t, 20.0 + k, 0.0, 1 + k + 3 * t)
                for t in range(10) for k in range(3)]
        df = pd.DataFrame(rows, columns=["t", "x", "y", "z", "label_id"])
        out = tu.BayesianTrackingUtils(_params(maxSearchRadius=20), _Log())._track(df)
        self.assertEqual(out["track_id"].nunique(), 3)


if __name__ == "__main__":
    unittest.main()
