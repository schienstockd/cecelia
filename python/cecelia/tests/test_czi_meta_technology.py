"""`czi_meta` beyond laser-scanning confocal: technology, modes, camera fields and the LSM model name,
against metadata from real files (test-data/acquisition/czi_*):

- `czi_lsm980_airyscan_metadata.xml` — LSM 980 Airyscan z-stack, ZEN blue 3.7. Trimmed: the 929 KB
  `Experiment` dropped (so Bright Z reads unknown here), `HardwareSetting` cut to the imaging device,
  serial numbers blanked.
- `czi_axioscan_z1_widefield_metadata.xml` — Axio Scan.Z1 slide scan, Axiocam 506, 255 tiles.
  `Information` + `Scaling` only.
- `czi_lsm880_spectral_metadata.xml` — LSM 880 lambda stack (33 channels), ZEN black.
Expected values are read from the fixtures' own elements, not typed from the parser.
"""
import os
import unittest
import xml.etree.ElementTree as ET  # reads the fixtures' own elements to cross-check the parser

from cecelia.utils import czi_meta

_FIX = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data', 'acquisition'))


def _read(name):
    with open(os.path.join(_FIX, name), 'rb') as f:
        return f.read()


class TestAiryscan(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.raw = _read('czi_lsm980_airyscan_metadata.xml')
        cls.s = czi_meta.parse_settings(cls.raw)

    def test_model_is_the_imaging_device_not_the_system_field(self):
        root = ET.fromstring(self.raw)
        self.assertEqual(root.findtext('Metadata/Information/Instrument/Microscopes/Microscope/System'), 'LSM 800')
        self.assertEqual(self.s['system'], 'LSM 980')
        self.assertEqual(self.s['microscopeBody'], 'Axio Observer.Z1 / 7')

    def test_technology_and_mode(self):
        self.assertEqual(self.s['technology'], 'airyscan')
        self.assertEqual({c['airyscan'] for c in self.s['channels']}, {'FastAiryScanSheppardSum'})
        self.assertEqual(self.s['readerVersion'], czi_meta.READER_VERSION)
        self.assertIsNone(self.s['camera'])


class TestSlideScanner(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.raw = _read('czi_axioscan_z1_widefield_metadata.xml')
        cls.s = czi_meta.parse_settings(cls.raw)
        cls.xml_chans = list(ET.fromstring(cls.raw).iterfind('Metadata/Information/Image/Dimensions/Channels/Channel'))

    def test_widefield_tiled_and_named_by_its_stand(self):
        s = self.s
        self.assertEqual((s['technology'], s['modes'], s['system']), ('widefield', ['tiled'], 'Axio Scan.Z1'))
        self.assertIsNone(s['microscopeBody'])
        self.assertEqual(s['lasers'], [])

    def test_camera_and_channel_fields(self):
        self.assertEqual(self.s['camera']['name'], 'Axiocam506m')
        self.assertEqual(self.s['camera']['binning'], 1)
        for c, x in zip(self.s['channels'], self.xml_chans):
            with self.subTest(channel=c['name']):
                self.assertAlmostEqual(c['exposureMs'], float(x.findtext('ExposureTime')) / 1e6)   # ns → ms
                self.assertEqual(c['filterCube'], x.findtext('Reflector'))
                self.assertIsNone(c['hv'])                 # a camera has no HV


class TestSpectral(unittest.TestCase):
    def test_lambda_stack_is_confocal_spectral(self):
        s = czi_meta.parse_settings(_read('czi_lsm880_spectral_metadata.xml'))
        self.assertEqual((s['technology'], s['modes']), ('confocal', ['spectral']))
        self.assertEqual(len(s['channels']), 33)
        self.assertEqual(s['system'], 'LSM 880, AxioObserver')   # ZEN black: no imaging-device name, System kept


class TestTwoPhotonStillTwoPhoton(unittest.TestCase):
    def test_lsm780_nlo(self):
        s = czi_meta.parse_settings(_read('czi_lsm780_nlo_metadata.xml'))
        self.assertEqual(s['technology'], 'two-photon')


if __name__ == '__main__':
    unittest.main()
