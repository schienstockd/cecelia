"""RESULT-REVIEW: result-9aded902

Two channels on ONE detector (sequential groups, a different laser each) carry their own HV. Detector
settings were keyed by detector, so the last `<pmt>` for that detector was reported for both channels.
Built from the real FV4000 settings document with CH3 moved onto CH1's detector and given HV 600.
"""
import os
import unittest

from cecelia.utils import oir_meta

_DOC = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data',
                                     'acquisition', 'oir_fv4000_3.2.1_imageProperties.xml'))
_CH3 = b'bb8d4d5d-5dad-43bd-b536-39ab647b597b'


def _ch3_on_detector_1(doc):
    # Every place CH3 names its detector, plus its own pmt voltage.
    doc = doc.replace(b'channelId="' + _CH3 + b'" detectorId="__FV40-EXRSD_D_3__"',
                      b'channelId="' + _CH3 + b'" detectorId="__FV40-EXBSD_D_1__"')
    doc = doc.replace(b'id="' + _CH3 + b'" order="3" enable="true" detectorId="__FV40-EXRSD_D_3__"',
                      b'id="' + _CH3 + b'" order="3" enable="true" detectorId="__FV40-EXBSD_D_1__"')
    head, sep, tail = doc.partition(b'channelId="' + _CH3 + b'" detectorId="__FV40-EXBSD_D_1__">')
    assert sep, 'CH3 pmt not found in fixture'
    tail = tail.replace(b'>0</', b'>600</', 1)          # first value inside CH3's <pmt>: voltage
    return head + sep + tail


class SharedDetectorTest(unittest.TestCase):
    def test_each_channel_keeps_its_own_hv(self):
        with open(_DOC, 'rb') as f:
            doc = _ch3_on_detector_1(f.read())
        chans = {c['name']: c for c in oir_meta.parse_settings(doc)['channels']}
        self.assertEqual(chans['CH3']['detectorId'], '__FV40-EXBSD_D_1__')
        self.assertEqual(chans['CH3']['hv'], 600.0)
        self.assertEqual(chans['CH1']['hv'], 0.0)


if __name__ == '__main__':
    unittest.main()
