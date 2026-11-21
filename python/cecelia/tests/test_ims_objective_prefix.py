# RESULT-REVIEW: result-4ba7e724
"""An objective name with a number-and-x before the real `MAGx/NA` must still give MAG and NA:
`Objective 1 XLUMPLFLN 20x/1.0 W` is a 20× / NA 1.0 lens, not 1×, and losing the NA silently turns
off the sampling check. Expected values come from the lens names themselves.

Run with `pixi run test-py`.
"""
import unittest

import cecelia.utils.ims_meta as ims_meta


class ObjectivePrefixTest(unittest.TestCase):
    def test_mag_na_pair_wins_over_an_earlier_bare_x(self):
        o = ims_meta._objective('Objective 1 XLUMPLFLN 20x/1.0 W', None)
        self.assertEqual((o['magnification'], o['na']), (20.0, 1.0))
        o = ims_meta._objective('HC PL APO 2 x 63x/1.40', None)
        self.assertEqual((o['magnification'], o['na']), (63.0, 1.4))

    def test_bare_magnification_any_x_case(self):
        self.assertEqual(ims_meta._objective('UPLSAPO 60X', None)['magnification'], 60.0)
        o = ims_meta._objective('UPLSAPO 60X/1.35 O', None)
        self.assertEqual((o['magnification'], o['na']), (60.0, 1.35))
        self.assertEqual(ims_meta._objective('Plan 40× W', 1.1)['magnification'], 40.0)
        self.assertEqual(ims_meta._objective('20xLWD', None)['magnification'], 20.0)
        # a word that merely starts with x after a number is not a magnification
        self.assertIsNone(ims_meta._objective('Objective 1 XLUMPLFLN', None)['magnification'])


if __name__ == '__main__':
    unittest.main()
