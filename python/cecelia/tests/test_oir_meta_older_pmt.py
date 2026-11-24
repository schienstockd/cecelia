"""RESULT-REVIEW: result-256dd785

Older `.oir` files write `<pmt>` / Bright Z `<pmtParam>` with only a detectorId. A detector shared by
two channels — a disabled one, or one in a later phase — then can't say whose HV a `<pmt>` is, and the
last one in the file used to win: CH1 reported another channel's HV, and the Bright Z ramp mixed
another channel's (or a disabled channel's) values into its series. Built from the real FV4000
settings document with every channelId dropped from `<pmt>` / `<pmtParam>`.
"""
import os
import re
import unittest

from cecelia.utils import oir_meta

_DOC = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data',
                                     'acquisition', 'oir_fv4000_3.2.1_imageProperties.xml'))
_CH3 = b'bb8d4d5d-5dad-43bd-b536-39ab647b597b'
_D1, _D3 = b'__FV40-EXBSD_D_1__', b'__FV40-EXRSD_D_3__'


def _doc():
    with open(_DOC, 'rb') as f:
        return f.read()


def _ch3_on(doc, det, enable=b'true', hv=b'600'):
    """CH3 on `det`, enabled or not, with HV `hv` in its `<pmt>` and every Bright Z `<pmtParam>`."""
    doc = doc.replace(b'channelId="' + _CH3 + b'" detectorId="' + _D3 + b'"',
                      b'channelId="' + _CH3 + b'" detectorId="' + det + b'"')
    doc = doc.replace(b'id="' + _CH3 + b'" order="3" enable="true" detectorId="' + _D3 + b'"',
                      b'id="' + _CH3 + b'" order="3" enable="' + enable + b'" detectorId="' + det + b'"')
    return re.sub(rb'(channelId="' + _CH3 + rb'" detectorId="[^"]+">\s*<lsmparam:voltage>)0<',
                  rb'\g<1>' + hv + b'<', doc)


def _older(doc):
    """As an older file writes it: no channelId on `<pmt>` / `<pmtParam>`."""
    return re.sub(rb'(<lsmparam:pmt(?:Param)? )channelId="[^"]+" ', rb'\1', doc)


def _second_phase(doc, det):
    """A second acquisition phase whose one channel is on `det`, with its `<pmt>` (HV 600) last."""
    phase = (b'<commonimage:phase xsi:type="lsmimage:LSMPhase" id="phase-2"><commonphase:group id="g2">'
             b'<commonphase:channel id="p2-ch1" order="1" enable="true" detectorId="' + det + b'">'
             b'<commonphase:name>P2CH1</commonphase:name></commonphase:channel>'
             b'</commonphase:group></commonimage:phase>')
    pmt = (b'<lsmparam:pmt detectorId="' + det + b'"><lsmparam:voltage>600</lsmparam:voltage>'
           b'<lsmparam:offset>0</lsmparam:offset><lsmparam:gain>1.0</lsmparam:gain></lsmparam:pmt>')
    doc = doc.replace(b'</commonimage:acquisition>', phase + b'</commonimage:acquisition>')
    return doc.replace(b'<lsmparam:method>', pmt + b'<lsmparam:method>', 1)


def _channels(doc):
    return {c['name']: c for c in oir_meta.parse_settings(doc)['channels']}


class OlderSharedDetectorChannelsTest(unittest.TestCase):
    def test_disabled_channel_on_the_detector_does_not_lend_its_hv(self):
        chans = _channels(_older(_ch3_on(_doc(), _D1, enable=b'false')))
        self.assertNotIn('CH3', chans)
        self.assertIsNone(chans['CH1']['hv'])        # can't tell CH1's <pmt> from CH3's: unknown
        self.assertEqual(chans['CH2']['hv'], 0.0)    # an unshared detector still reads

    def test_later_phase_channel_on_the_detector_does_not_lend_its_hv(self):
        s = oir_meta.parse_settings(_older(_second_phase(_doc(), _D1)))
        self.assertEqual(s['phaseCount'], 2)
        chans = {c['name']: c for c in s['channels']}
        self.assertIsNone(chans['CH1']['hv'])
        self.assertIsNone(chans['CH1']['gain'])
        self.assertEqual(chans['CH2']['hv'], 0.0)

    def test_unshared_detector_reads_without_a_channel_id(self):
        chans = _channels(_older(_doc()))
        self.assertEqual({c['hv'] for c in chans.values()}, {0.0})


class OlderSharedDetectorBrightZTest(unittest.TestCase):
    def _hv(self, doc):
        return [p['hv'] for p in oir_meta.parse_settings(doc)['brightZ']['points']]

    def test_shared_detector_point_is_not_the_disabled_channels(self):
        for hv in self._hv(_older(_ch3_on(_doc(), _D1, enable=b'false'))):
            self.assertNotIn('__FV40-EXBSD_D_1__', hv)   # was CH3's 600 at every point
            self.assertEqual(set(hv), {'__FV40-EXBSD_D_2__', '__FV40-EXRSD_D_4__'})

    def test_disabled_channel_is_left_out_of_the_ramp(self):
        for hv in self._hv(_older(_ch3_on(_doc(), _D3, enable=b'false'))):
            self.assertNotIn('__FV40-EXRSD_D_3__', hv)
        for hv in self._hv(_ch3_on(_doc(), _D3, enable=b'false')):     # with channelIds too
            self.assertNotIn(_CH3.decode(), hv)

    def test_each_channel_keyed_by_its_id_when_recorded(self):
        for hv in self._hv(_ch3_on(_doc(), _D1)):
            self.assertEqual(hv[_CH3.decode()], 600.0)
            self.assertEqual(hv['7a0605e9-2826-4c42-a02d-32ab5b8930b1'], 0.0)


if __name__ == '__main__':
    unittest.main()
