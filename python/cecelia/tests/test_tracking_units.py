"""Tracking coordinates reach btrack in µm — `cecelia.utils.tracking_utils`.

Nothing covered this, and the gap was the bug. btrack works in whatever space it is handed, so
`maxSearchRadius` meant PIXELS while `track_measures` — computing speed on the very same tracks —
reported µm/min from `img_physical_sizes`. One pipeline, two coordinate systems, and the form said
neither: "radius 8" and "an 8 µm jump" could both be true at once.

The Z half is the part a per-param conversion could never fix. At 0.33 µm XY and 2 µm Z, pixel-space
tracking scored a one-plane hop as 0.33 µm of motion when it is 2 µm — a 6x under-count, in exactly
the direction that links cells at different depths.
"""
import unittest

import numpy as np
import pandas as pd

from cecelia.utils.tracking_utils import BayesianTrackingUtils


class _View:
    """The two methods `_centroids_from_view` uses, over a fixed frame."""

    def __init__(self, df, temporal=('centroid_t',)):
        self._df, self._t = df, list(temporal)

    def temporal_columns(self):
        return self._t

    def view_centroid_cols(self):
        return self

    def as_df(self):
        return self._df


class _Log:
    def log(self, *_a, **_k):
        pass


def _utils(physical_sizes=None):
    params = {'taskDir': '/tmp', 'btrackConfig': '/tmp/cfg.json', 'maxSearchRadius': 20,
              'maxLost': 1, 'trackBranching': False, 'minTimepoints': 5, 'accuracy': 0.8,
              'probToAssign': 0.8, 'noiseInital': 300, 'noiseProcessing': 100,
              'noiseMeasurements': 100, 'distThresh': 10, 'timeThresh': 5,
              'segmentationMissRate': 0.1, 'lambdaLink': 5, 'lambdaBranch': 50, 'lambdaTime': 5,
              'lambdaDist': 5, 'thetaTime': 5, 'thetaDist': 5}
    if physical_sizes is not None:
        params['physicalSizes'] = physical_sizes
    return BayesianTrackingUtils(params, _Log())


def _frame(z=True):
    d = {'centroid_t': [0.0, 1.0], 'centroid_y': [10.0, 20.0], 'centroid_x': [30.0, 40.0],
         'label': [1, 2]}
    if z:
        d['centroid_z'] = [2.0, 5.0]
    return pd.DataFrame(d)


class TrackingUnitsTest(unittest.TestCase):
    def test_centroids_are_scaled_to_microns(self):
        # [sz, sy, sx] — skimage order, as `img_physical_sizes` returns it
        got = _utils([2.0, 0.5, 0.25])._centroids_from_view(_View(_frame()))
        np.testing.assert_allclose(got['x'], [30 * 0.25, 40 * 0.25])
        np.testing.assert_allclose(got['y'], [10 * 0.5, 20 * 0.5])
        np.testing.assert_allclose(got['z'], [2 * 2.0, 5 * 2.0])

    def test_the_axis_order_is_z_y_x_not_x_y_z(self):
        """The one way to get this silently wrong. With distinct sizes per axis, swapping the order
        still produces plausible coordinates — just the wrong distances, on the wrong axis."""
        got = _utils([2.0, 0.5, 0.25])._centroids_from_view(_View(_frame()))
        # x used the LAST size (0.25); had the order been read x,y,z it would be 30*2.0 = 60
        self.assertAlmostEqual(got['x'].iloc[0], 7.5)
        self.assertNotAlmostEqual(got['x'].iloc[0], 60.0)

    def test_anisotropic_z_is_no_longer_under_counted(self):
        """The 6x. One Z plane at 2 µm must not score as 0.33 µm of motion."""
        px, pz = 0.33, 2.0
        got = _utils([pz, px, px])._centroids_from_view(_View(_frame()))
        step_z = abs(got['z'].iloc[1] - got['z'].iloc[0])
        self.assertAlmostEqual(step_z, 3 * pz)                 # 3 planes → 6 µm
        self.assertGreater(step_z / (3 * px), 5.9)             # ~6x what pixel space scored it

    def test_time_is_left_in_frames(self):
        """btrack's `t` is a frame index and the config's time params are in frames; scaling it here
        would silently redefine `maxLost`/`timeThresh`. Minutes are `track_measures`' business."""
        got = _utils([2.0, 0.5, 0.25])._centroids_from_view(_View(_frame()))
        np.testing.assert_allclose(got['t'], [0.0, 1.0])

    def test_no_sizes_means_unscaled_not_a_fake_micron(self):
        """An uncalibrated image keeps the old pixel behaviour rather than pretending 1 px = 1 µm."""
        for sizes in (None, []):
            got = _utils(sizes)._centroids_from_view(_View(_frame()))
            np.testing.assert_allclose(got['x'], [30.0, 40.0])
            np.testing.assert_allclose(got['z'], [2.0, 5.0])

    def test_2d_has_no_z_column_and_still_scales_xy(self):
        got = _utils([2.0, 0.5, 0.25])._centroids_from_view(_View(_frame(z=False)))
        np.testing.assert_allclose(got['z'], [0.0, 0.0])
        np.testing.assert_allclose(got['x'], [7.5, 10.0])

    def test_labels_survive_unscaled(self):
        got = _utils([2.0, 0.5, 0.25])._centroids_from_view(_View(_frame()))
        np.testing.assert_array_equal(got['label_id'], [1, 2])

    def test_a_missing_temporal_axis_still_stops_the_run(self):
        with self.assertRaises(SystemExit):
            _utils([1.0, 1.0, 1.0])._centroids_from_view(_View(_frame(), temporal=()))


if __name__ == '__main__':
    unittest.main()
