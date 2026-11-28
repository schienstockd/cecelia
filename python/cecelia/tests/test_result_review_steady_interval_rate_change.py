"""RESULT-REVIEW: result-783073c2

An irregular series takes its interval from its steady part (`ims_meta.steady_interval`). A series
that changes rate — 12 gaps of 30 s, then 8 of 33 s — has a short run straddling the change whose
median (31.5 s) sits within `tol` of BOTH rates. Ranked by how many gaps its interval "covers", that
run won, so the image got 31.5 s (wrong for every frame) and no timepoints flagged. The steady part is
the 30 s majority, with the 33 s frames flagged.
"""
import unittest

from cecelia.utils.ims_meta import deviating_frames, steady_interval

RATE_CHANGE = [30.0] * 12 + [33.0] * 8


class SteadyIntervalRateChangeTest(unittest.TestCase):
    def test_a_rate_change_keeps_the_majority_rate_and_flags_the_rest(self):
        for gaps, resolution in ((RATE_CHANGE, 0.0), (RATE_CHANGE, 1.0), ([30.0] * 12 + [31.6] * 8, 0.0)):
            interval, run, deviating = steady_interval(gaps, resolution=resolution)
            self.assertEqual(interval, 30.0, gaps)
            self.assertEqual(run, (0, 12))
            self.assertEqual(deviating_frames(deviating), [[12, 20]])

    def test_equal_halves_still_have_no_interval(self):
        self.assertIsNone(steady_interval([30.0] * 10 + [33.0] * 10))


if __name__ == '__main__':
    unittest.main()
