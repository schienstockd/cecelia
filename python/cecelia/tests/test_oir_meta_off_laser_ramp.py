"""RESULT-REVIEW: result-746d89db

A laser switched off for the acquisition still has `laserParam` entries in the stored Bright Z points.
The ramp listed it under its raw id, so the dialog showed a power ramp for a laser that never fired.
Built from the real FV4000 settings document with the InSight disabled.
"""
import os
import unittest

from cecelia.utils import oir_meta

_DOC = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data',
                                     'acquisition', 'oir_fv4000_3.2.1_imageProperties.xml'))


class OffLaserRampTest(unittest.TestCase):
    def test_ramp_lists_only_lasers_that_fired(self):
        with open(_DOC, 'rb') as f:
            doc = f.read()
        off = doc.replace(b'enable="true" id="INSIGHT X3-OL_131328Imaging_main_phase_1"',
                          b'enable="false" id="INSIGHT X3-OL_131328Imaging_main_phase_1"', 1)
        self.assertNotEqual(off, doc)
        points = oir_meta.parse_settings(off)['brightZ']['points']
        for p in points:
            self.assertEqual(list(p['transmissionPct']), ['MaiTai e HP DS-OL'])


if __name__ == '__main__':
    unittest.main()
