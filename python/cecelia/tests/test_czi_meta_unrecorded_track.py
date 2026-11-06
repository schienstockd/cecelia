"""RESULT-REVIEW: result-b2e6cb3b — a scan setting one channel records and another does not is unknown for the
image (None): the recorded channel's value must not stand for the silent one."""
import os
import unittest
import xml.etree.ElementTree as ET  # edits a real fixture into the case

from cecelia.utils import czi_meta

_FIX = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data', 'acquisition'))


class UnrecordedTrackTest(unittest.TestCase):
    def test_one_track_silent_other_averaged(self):
        with open(os.path.join(_FIX, 'czi_lsm800_zenblue_metadata.xml'), 'rb') as f:
            r = ET.fromstring(f.read())
        chans = r.find('Metadata/Information/Image/Dimensions/Channels')
        first = chans.find('Channel')
        two = ET.fromstring(ET.tostring(first))
        two.find('LaserScanInfo/Averaging').text = '4'
        first.find('LaserScanInfo').remove(first.find('LaserScanInfo/Averaging'))
        first.remove(first.find('PinholeSize'))
        chans.append(two)
        s = czi_meta.parse_settings(ET.tostring(r))
        self.assertIsNone(s['scanner']['integration'])
        self.assertIsNone(s['pinholeUm'])


if __name__ == '__main__':
    unittest.main()
