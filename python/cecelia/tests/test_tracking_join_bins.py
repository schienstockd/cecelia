"""At a fractional `distThresh` btrack proposes fewer joins than are in range; the count says so.

RESULT-REVIEW: result-c2965fd2

btrack's hypothesis engine looks for link partners in `HypercubeBin` cells `int(dist_thresh)` wide
(src/hyperbin.cc), so at 1.5 it misses pairs between 1 and 1.5 apart. The `>> Optimiser:` count and
the over-limit stop must judge what btrack actually builds (pinned against `tracker.hypotheses()`),
and the run must warn that joins in range were left out.
"""
import unittest
from collections import Counter

import btrack

from cecelia.tests.test_tracking_join_load import _Log, _dense, _params, _track_frame_to_frame
from cecelia.utils import tracking_utils as tu


class TestFractionalDistThresh(unittest.TestCase):
    def test_count_is_what_btrack_proposes(self):
        df = _dense()
        for dist in (1.5, 1.9):
            with btrack.BayesianTracker() as tracker:
                _track_frame_to_frame(tracker, df, dist, False)
                links = Counter(h.hypothesis_type.name for h in tracker.hypotheses())["LINK"]
                starts, ends = tu.tracklet_ends(tracker.tracks)
            in_range = int(tu.join_counts(starts, ends, dist, 5, btrack_bins=False).sum())
            self.assertLess(links, in_range, f"distThresh={dist}: the premise, btrack drops some")
            self.assertEqual(tu.join_candidates(starts, ends, dist, 5, False), links)

    def test_dropped_joins_are_logged(self):
        log = _Log()
        tu.BayesianTrackingUtils(_params(distThresh=1.9), log)._track(_dense())
        self.assertTrue(any("btrack searches whole µm" in line for line in log.lines), log.lines)


if __name__ == "__main__":
    unittest.main()
