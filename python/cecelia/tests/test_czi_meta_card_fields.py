"""`czi_meta` per-channel fields for the settings card laid out by track (reader version 3), against
metadata from real files (test-data/acquisition/czi_*):

- `czi_lsm980_airyscan_metadata.xml` — LSM 980 Airyscan, ZEN blue 3.7, four tracks, a different main
  beam splitter per track.
- `czi_lsm780_multitrack_metadata.xml` — LSM 780, ZEN black, four tracks, track 4 with two channels.
- `czi_lsm800_zen2.3_airyscan_metadata.xml` — LSM 800 Airyscan, ZEN 2.3 blue. `Information` + `Scaling`
  only, serial numbers blanked. Its `Attenuation` values (1.9, 2.6, 0.95) have no `Transmission`
  beside them; reading them as 1 − attenuation gave −90 % and −160 % laser power.
Expected values are read from the fixtures' own elements, not typed from the parser.
"""
import os
import unittest
import xml.etree.ElementTree as ET  # reads the fixtures' own elements to cross-check the parser

from cecelia.utils import czi_meta

_FIX = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data', 'acquisition'))


def _load(name):
    with open(os.path.join(_FIX, name), 'rb') as f:
        raw = f.read()
    root = ET.fromstring(raw)
    return czi_meta.parse_settings(raw), root, list(root.iterfind('Metadata/Information/Image/Dimensions/Channels/Channel'))


class TestTracksZenBlue(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.s, cls.root, cls.xml = _load('czi_lsm980_airyscan_metadata.xml')

    def test_track_from_the_channel_name(self):
        self.assertEqual([c['name'] for c in self.s['channels']], [x.get('Name') for x in self.xml])
        self.assertEqual([c['track'] for c in self.s['channels']], [int(x.get('Name').rsplit('-T', 1)[1]) for x in self.xml])

    def test_lines_band_and_pinhole_per_channel(self):
        for c, x in zip(self.s['channels'], self.xml):
            with self.subTest(channel=c['name']):
                ls = x.find('LightSourcesSettings/LightSourceSettings')
                self.assertEqual(c['laserLines'], [{'wavelengthNm': float(ls.findtext('Wavelength')),
                                                    'pct': round(float(ls.findtext('Transmission')) * 100, 3)}])
                lo, hi = x.findtext('DetectionWavelength/Ranges').split('-')
                self.assertEqual(c['emissionFilter'], f'{round(float(lo))}-{round(float(hi))} nm')
                self.assertEqual(c['pinholeAiry'], round(float(x.findtext('PinholeSizeAiry')), 2))

    def test_beam_splitters_named_and_kept_per_channel_when_tracks_differ(self):
        names = {d.get('Id'): d.get('Name') for d in self.root.iterfind('Metadata/Information/Instrument/Dichroics/Dichroic')}
        main = [names[x.findtext('LaserScanInfo/MainBeamSplitterVis')] for x in self.xml]
        self.assertEqual([c['beamSplitters'][0] for c in self.s['channels']], main)
        self.assertGreater(len(set(main)), 1)
        self.assertEqual(self.s['dichroics'], [])        # not one image-level value when tracks differ


class TestTracksZenBlack(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.s, _, cls.xml = _load('czi_lsm780_multitrack_metadata.xml')

    def test_two_channels_share_a_track(self):
        tracks = [c['track'] for c in self.s['channels']]
        self.assertEqual(tracks, [int(x.get('Name').rsplit('-T', 1)[1]) for x in self.xml])
        self.assertEqual(sum(t == 4 for t in tracks), 2)


class TestAttenuationAboveOne(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.s, _, cls.xml = _load('czi_lsm800_zen2.3_airyscan_metadata.xml')

    def test_fixture_has_the_unit_problem(self):
        att = [float(x.findtext('LightSourcesSettings/LightSourceSettings/Attenuation')) for x in self.xml]
        self.assertTrue(any(a > 1 for a in att))
        self.assertTrue(all(x.find('LightSourcesSettings/LightSourceSettings/Transmission') is None for x in self.xml))

    def test_power_not_read_never_negative(self):
        self.assertEqual([l['transmissionPct'] for l in self.s['lasers']], [None] * len(self.s['lasers']))
        self.assertTrue(all(l['pct'] is None for c in self.s['channels'] for l in c['laserLines']))
        self.assertEqual(self.s['technology'], 'airyscan')
