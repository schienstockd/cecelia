"""RESULT-REVIEW: result-183d5bb9 — when tracks set averaging, dwell or pinhole differently, the image-level value is
unknown (None), not track 1's: one number must not stand for channels it does not describe."""
import os
import unittest
import xml.etree.ElementTree as ET  # edits a real fixture into the case

from cecelia.utils import czi_meta

_FIX = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data', 'acquisition'))


def _read(name):
    with open(os.path.join(_FIX, name), 'rb') as f:
        return f.read()


class TracksDifferTest(unittest.TestCase):
    def test_zen_black_multitrack_pinholes_differ(self):
        # Real LSM 780 file: four tracks, detector pinholes 29.6 / 36.2 / 31.1 / 27.7 µm.
        self.assertIsNone(czi_meta.parse_settings(_read('czi_lsm780_multitrack_metadata.xml'))['pinholeUm'])

    def test_zen_blue_second_track_with_other_averaging_and_pinhole(self):
        r = ET.fromstring(_read('czi_lsm800_zenblue_metadata.xml'))
        chans = r.find('Metadata/Information/Image/Dimensions/Channels')
        two = ET.fromstring(ET.tostring(chans.find('Channel')))
        two.find('LaserScanInfo/Averaging').text = '4'
        two.find('PinholeSize').text = '40'
        chans.append(two)
        s = czi_meta.parse_settings(ET.tostring(r))
        self.assertIsNone(s['scanner']['integration'])
        self.assertIsNone(s['pinholeUm'])
        self.assertEqual(s['scanner']['zoom'], 4.0)      # what they share is still reported


if __name__ == '__main__':
    unittest.main()
