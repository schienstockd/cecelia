"""RESULT-REVIEW: result-29904f44 — a channel excited by two laser lines at once lists both, each with its own
transmission; reading only the first left a firing laser out of the light path."""
import os
import unittest
import xml.etree.ElementTree as ET  # edits a real fixture into the case

from cecelia.utils import czi_meta

_FIX = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data', 'acquisition'))


class TwoLinesTest(unittest.TestCase):
    def test_both_lines_are_read(self):
        with open(os.path.join(_FIX, 'czi_lsm800_zenblue_metadata.xml'), 'rb') as f:
            r = ET.fromstring(f.read())
        lss = r.find('Metadata/Information/Image/Dimensions/Channels/Channel/LightSourcesSettings')
        second = ET.fromstring(ET.tostring(lss.find('LightSourceSettings')))
        second.find('Wavelength').text = '640'
        second.find('Transmission').text = '0.05'
        lss.append(second)
        s = czi_meta.parse_settings(ET.tostring(r))
        self.assertEqual([(l['wavelengthNm'], l['transmissionPct']) for l in s['lasers']], [(488.0, 0.2), (640.0, 5.0)])
        self.assertEqual(s['channels'][0]['laser'], '488 nm + 640 nm')


if __name__ == '__main__':
    unittest.main()
