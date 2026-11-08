"""btrack's global optimiser runs with no time limit — `BayesianTrackingUtils._track`.

RESULT-REVIEW: result-9c9da354

A GLPK time limit makes the result depend on machine speed: when it fires, btrack returns the
tracks unmerged with only a log warning. `tracker.optimise()` with no `options` falls back to
btrack's `GLPK_OPTIONS = {"tm_lim": 60_000}`, a 60 s limit, so the call must pass options itself
with no `tm_lim` in them.
"""
import os
import unittest
from unittest import mock

import numpy as np
import pandas as pd
from btrack.optimise import optimiser

from cecelia.utils.tracking_utils import BayesianTrackingUtils

_REPO = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
_CONFIG = os.path.join(_REPO, "app", "src", "tasks", "tracking", "cell_config.json")


class _Log:
    def log(self, *_a, **_k):
        pass


def _utils():
    params = {'taskDir': '/tmp', 'btrackConfig': _CONFIG, 'maxSearchRadius': 20,
              'maxLost': 1, 'trackBranching': False, 'minTimepoints': 2, 'accuracy': 0.8,
              'probToAssign': 0.8, 'noiseInital': 300, 'noiseProcessing': 100,
              'noiseMeasurements': 100, 'distThresh': 10, 'timeThresh': 5,
              'segmentationMissRate': 0.1, 'lambdaLink': 5, 'lambdaBranch': 50, 'lambdaTime': 5,
              'lambdaDist': 5, 'thetaTime': 5, 'thetaDist': 5}
    return BayesianTrackingUtils(params, _Log())


def _centroids():
    """Three cells drifting along x over ten frames, in `_centroids_from_view`'s schema."""
    rows = [(t, 10.0 + 40 * k + 2.0 * t, 20.0 + k, 0.0, 1 + k + 3 * t)
            for t in range(10) for k in range(3)]
    return pd.DataFrame(rows, columns=["t", "x", "y", "z", "label_id"])


class TestOptimiserHasNoTimeLimit(unittest.TestCase):
    def test_glpk_options_carry_no_time_limit(self):
        seen = []
        real = optimiser.TrackOptimiser

        def spy(*args, **kwargs):
            seen.append(kwargs.get("options", args[0] if args else None))
            return real(*args, **kwargs)

        with mock.patch.object(optimiser, "TrackOptimiser", side_effect=spy):
            track_df = _utils()._track(_centroids())

        self.assertEqual(len(seen), 1, "the optimiser ran once")
        self.assertIsNotNone(seen[0])
        self.assertNotIn("tm_lim", seen[0])
        self.assertEqual(sorted(np.unique(track_df["track_id"])).__len__(), 3)


if __name__ == "__main__":
    unittest.main()
