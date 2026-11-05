"""RESULT-REVIEW: result-cb22947d

A complete movie registered for z-drift before import: each frame is shifted by s(t) planes and the
deep end zero-padded. With steady drift the shift steps up on the last frame, so one plane (z21) is
blank only there — it held data in every earlier frame. `partial_last_timepoint` called that a stopped
acquisition and the import, by default, dropped a fully acquired timepoint. Padding that comes and goes
across earlier frames marks a registered store, which is never reported.
"""
import unittest

import numpy as np

import cecelia.utils.intensity_utils as iu


class DriftPaddingIsNotAStop(unittest.TestCase):

    def test_a_drift_step_on_the_last_frame_is_not_a_stopped_acquisition(self):
        n_z = 26
        shifts = [0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5]
        occ = np.array([[z < n_z - s for z in range(n_z)] for s in shifts], dtype=bool)
        self.assertIsNone(iu.partial_last_timepoint(occ))

    def test_a_real_stop_is_still_reported(self):
        occ = np.ones((20, 26), dtype=bool)
        occ[-1, 14:] = False                      # the stopped-early OIR: z14-z25 of the last frame
        self.assertEqual(list(range(14, 26)), iu.partial_last_timepoint(occ)['emptyZ'])


if __name__ == '__main__':
    unittest.main()
