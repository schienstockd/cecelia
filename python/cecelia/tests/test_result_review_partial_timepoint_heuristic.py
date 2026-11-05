"""RESULT-REVIEW: result-78d839c7

A complete recording whose deep z-planes held data in only ONE earlier timepoint (a z-drift, or a stray
voxel in otherwise blank padding) was reported as stopped mid-volume, so import deleted a real
timepoint by default. A plane can only be missing if every earlier timepoint recorded it.
"""
import unittest

import numpy as np

import cecelia.utils.intensity_utils as iu


class OnlyConsistentlyRecordedPlanesCanBeMissingTest(unittest.TestCase):

    def test_planes_with_data_in_one_earlier_timepoint_are_not_a_stop(self):
        occ = np.zeros((20, 26), dtype=bool)
        occ[:, :20] = True          # z0-19 recorded throughout, including the last timepoint
        occ[5, 20:] = True          # z20-25 hold data only at t5
        self.assertIsNone(iu.partial_last_timepoint(occ))

    def test_a_real_stop_is_still_found(self):
        occ = np.ones((20, 26), dtype=bool)
        occ[-1, 14:] = False
        self.assertEqual(list(range(14, 26)), iu.partial_last_timepoint(occ)['emptyZ'])


if __name__ == '__main__':
    unittest.main()
