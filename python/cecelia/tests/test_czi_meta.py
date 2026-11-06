"""`czi_meta` against metadata from real `.czi` files (test-data/acquisition/czi_*).

The ZEN black file is an LSM 780 two-photon stack (Chameleon at 775 nm, four non-descanned and
transmitted detectors). Its expected HV and laser transmission are the values its OWN track setup
records (`MultiTrackSetup/.../Detector/Voltage`, `Attenuator/Transmission`), read separately from the
channel block the parser uses — so the two readings in the module docstring are checked, not assumed.
"""
import os
import struct
import tempfile
import unittest
import xml.etree.ElementTree as ET  # reads the fixture's own track setup to cross-check the parser

from cecelia.utils import czi_meta

_FIX = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data', 'acquisition'))
ZEN_BLACK = os.path.join(_FIX, 'czi_lsm780_nlo_metadata.xml')
ZEN_BLUE = os.path.join(_FIX, 'czi_lsm800_zenblue_metadata.xml')
HEADERS = os.path.join(_FIX, 'czi_lsm780_nlo_headers.bin')   # file header (512 B) + metadata segment header (288 B)


def _read(p):
    with open(p, 'rb') as f:
        return f.read()


class TestZenBlackTwoPhoton(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.s = czi_meta.parse_settings(_read(ZEN_BLACK))
        track = ET.fromstring(_read(ZEN_BLACK)).find(
            'Metadata/Experiment/ExperimentBlocks/AcquisitionBlock/MultiTrackSetup/TrackSetup')
        cls.voltages = [float(d.findtext('Voltage')) for d in track.findall('Detectors/Detector')]
        cls.transmission = float(track.findtext('Attenuators/Attenuator/Transmission'))

    def test_system_objective_and_stack(self):
        s = self.s
        self.assertEqual(s['source'], 'czi')
        self.assertEqual(s['system'], 'LSM 780, AxioObserver')
        self.assertAlmostEqual(s['objective']['na'], 1.15)
        self.assertEqual(s['objective']['immersion'], 'water')
        self.assertEqual(s['zStack'], {'stepUm': 0.37, 'count': 120})
        self.assertNotIn('timelapse', s)

    def test_one_infrared_laser_at_the_track_setups_transmission(self):
        self.assertEqual(self.s['lasers'], [
            {'name': 'Chameleon', 'wavelengthNm': 775.0, 'transmissionPct': self.transmission * 100}])

    def test_channel_gain_is_the_track_setups_hv(self):
        self.assertEqual([c['hv'] for c in self.s['channels']], self.voltages)
        self.assertEqual([c['name'] for c in self.s['channels']], ['NDD R1', 'NDD R2', 'NDD R3', 'T PMT'])
        self.assertEqual({c['laser'] for c in self.s['channels']}, {'Chameleon'})
        self.assertIsNone(self.s['channels'][0]['detector'])     # ZEN black records no detector model

    def test_scanner_averaging_is_not_called_a_sum_or_mean(self):
        sc = self.s['scanner']
        self.assertEqual(sc['pixelDwellUs'], 0.9848)
        self.assertEqual(sc['integration'], {'type': 'Average', 'count': 4, 'average': None})

    def test_brightness_correction_off_and_no_pinhole_for_non_descanned(self):
        self.assertEqual(self.s['brightZ'], {'enabled': False, 'points': []})
        self.assertIsNone(self.s['pinholeUm'])


class TestZenBlue(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.s = czi_meta.parse_settings(_read(ZEN_BLUE))

    def test_voltage_transmission_and_detector_mode(self):
        s = self.s
        self.assertEqual(s['system'], 'LSM 800')
        ch = s['channels'][0]
        self.assertAlmostEqual(ch['hv'], 649.98626708984)
        self.assertEqual(ch['detector'], 'MA-Pmt1')
        self.assertEqual(ch['detectorMode'], 'Integration')
        self.assertEqual(s['lasers'][0]['wavelengthNm'], 488.0)
        self.assertAlmostEqual(s['lasers'][0]['transmissionPct'], 0.2)
        self.assertEqual(s['pinholeUm'], 28.58)

    def test_time_lapse_and_auto_brightness_flag(self):
        self.assertEqual(self.s['timelapse']['count'], 5)
        self.assertAlmostEqual(self.s['timelapse']['intervalS'], 21.18925)
        self.assertEqual(self.s['brightZ'], {'enabled': False, 'points': []})


class TestContainer(unittest.TestCase):
    """The real file and metadata-segment headers, laid out at the real file's metadata offset."""

    def test_reads_the_metadata_segment_the_file_header_points_at(self):
        head = _read(HEADERS)
        file_head, seg_head = head[:512], head[512:]
        pos = struct.unpack_from('<q', file_head, 92)[0]
        doc = _read(ZEN_BLACK)
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, 'x.czi')
            with open(path, 'wb') as f:
                f.write(file_head)
                f.write(b'\0' * (pos - len(file_head)))      # pixel segments in a real file
                f.write(seg_head)
                f.write(doc)
                f.write(os.urandom(1000))                    # attachments follow
            self.assertEqual(czi_meta.read_settings_xml(path), doc)
            self.assertEqual(czi_meta.acquisition_settings(path)['system'], 'LSM 780, AxioObserver')

    def test_not_a_czi_and_missing_file_report_a_reason(self):
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, 'x.czi')
            with open(path, 'wb') as f:
                f.write(os.urandom(4096))
            self.assertIn('reason', czi_meta.acquisition_settings(path))
            self.assertIn('reason', czi_meta.acquisition_settings(os.path.join(d, 'missing.czi')))


if __name__ == '__main__':
    unittest.main()
