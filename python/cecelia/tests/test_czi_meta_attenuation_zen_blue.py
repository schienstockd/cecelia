"""RESULT-REVIEW: result-39a51961

A ZEN 2.3 blue `.czi` whose every `Attenuation` is at or below 1 (one track, 0.95 — the same software
and value as the EGFP track of `czi_lsm800_zen2.3_airyscan_metadata.xml`) was still read as
1 − attenuation = 5 %: the guard only fired when some value in the same file was above 1, though the
unit is unknown for that software either way. Attenuation-only power is read for ZEN black files only.
"""
import os
import unittest
import xml.etree.ElementTree as ET  # edits the fixture's own elements to make the one-track file

from cecelia.utils import czi_meta

_FIX = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data', 'acquisition'))


class TestZenBlueAttenuationOnly(unittest.TestCase):
    def test_one_track_at_or_below_one_is_not_read_as_a_fraction(self):
        with open(os.path.join(_FIX, 'czi_lsm800_zen2.3_airyscan_metadata.xml'), 'rb') as f:
            root = ET.fromstring(f.read())
        chans = root.find('Metadata/Information/Image/Dimensions/Channels')
        for ch in list(chans):
            if ch.findtext('LightSourcesSettings/LightSourceSettings/Attenuation').strip() != '0.949999999999995':
                chans.remove(ch)
        self.assertEqual(len(chans), 1)
        s = czi_meta.parse_settings(ET.tostring(root))
        self.assertEqual([l['transmissionPct'] for l in s['lasers']], [None])
        self.assertEqual(s['channels'][0]['laserLines'], [{'wavelengthNm': 488.0, 'pct': None}])

    def test_zen_black_attenuation_only_is_still_read(self):
        with open(os.path.join(_FIX, 'czi_lsm780_multitrack_metadata.xml'), 'rb') as f:
            s = czi_meta.parse_settings(f.read())
        self.assertTrue(all(l['transmissionPct'] is not None for l in s['lasers']))
