# RESULT-REVIEW: result-e5eba910
"""A `.` right before the magnification (`Obj.20x/0.8`) must not hide it: with no `NumericalAperture`
attribute the NA comes from the name, and losing it silently skips the sampling check. Expected values
come from the lens names themselves.

Run with `pixi run test-py`.
"""
import unittest

import cecelia.utils.ims_meta as ims_meta


class ObjectiveDotPrefixTest(unittest.TestCase):
    def test_dot_before_magnification(self):
        o = ims_meta._objective('Obj.20x/0.8', None)
        self.assertEqual((o['magnification'], o['na']), (20.0, 0.8))
        o = ims_meta._objective('UPLSAPO.20x/0.75', None)
        self.assertEqual((o['magnification'], o['na']), (20.0, 0.75))
        self.assertEqual(ims_meta._objective('Obj.20x', None)['magnification'], 20.0)

    def test_decimal_magnification(self):
        o = ims_meta._objective('Plan 1.5x/0.05', None)
        self.assertEqual((o['magnification'], o['na']), (1.5, 0.05))
        self.assertEqual(ims_meta._objective('Plan 1.5x', None)['magnification'], 1.5)


if __name__ == '__main__':
    unittest.main()
