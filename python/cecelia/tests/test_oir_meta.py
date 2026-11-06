"""`oir_meta` against settings documents cut from real `.oir` files (test-data/acquisition/).

Expected values were read off the same files independently (Bio-Formats' original-metadata table and
the raw XML) before the parser existed; see test-data/README.md → acquisition/.
"""
import os
import tempfile
import unittest

from cecelia.utils import oir_meta

_FIX = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data', 'acquisition'))
FV4000 = os.path.join(_FIX, 'oir_fv4000_3.2.1_imageProperties.xml')
FVMPE = os.path.join(_FIX, 'oir_fvmpe-rs_2.3.2_imageProperties.xml')


def _read(path):
    with open(path, 'rb') as f:
        return f.read()


class TestFV4000(unittest.TestCase):
    """Resonant two-photon time-lapse with Bright Z on."""

    @classmethod
    def setUpClass(cls):
        cls.s = oir_meta.parse_settings(_read(FV4000))

    def test_system(self):
        self.assertEqual(self.s['system'], 'FV4000')
        self.assertEqual(self.s['softwareVersion'], '3.2.1.85')

    def test_bright_z_on_with_its_ramp(self):
        bz = self.s['brightZ']
        self.assertTrue(bz['enabled'])
        self.assertEqual([p['zFromStartUm'] for p in bz['points']], [0.0, 60.0])
        self.assertEqual(bz['points'][0]['transmissionPct']['MaiTai e HP DS-OL'], 14.4)
        self.assertEqual(bz['points'][1]['transmissionPct']['MaiTai e HP DS-OL'], 21.0)
        self.assertEqual(bz['points'][1]['transmissionPct']['INSIGHT X3-OL'], 20.4)

    def test_lasers_carry_wavelength_and_base_power(self):
        by_name = {l['name']: l for l in self.s['lasers']}
        self.assertEqual(by_name['MaiTai e HP DS-OL']['wavelengthNm'], 900.0)
        self.assertEqual(by_name['INSIGHT X3-OL']['wavelengthNm'], 1140.0)
        self.assertEqual(by_name['INSIGHT X3-OL']['transmissionPct'], 14.9)

    def test_scanner_timing_is_the_resonant_block(self):
        # Both a galvo and a resonant block are stored; only the resonant one acquired.
        sc = self.s['scanner']
        self.assertEqual(sc['type'], 'Resonant')
        self.assertEqual(sc['zoom'], 1.5)
        self.assertAlmostEqual(sc['pixelDwellUs'], 0.033)
        # `average="false"` on this file: the two passes are summed, not averaged.
        self.assertEqual(sc['integration'], {'type': 'Frame', 'count': 2.0, 'average': False})

    def test_channels_map_to_laser_and_detector(self):
        chans = self.s['channels']
        self.assertEqual([c['name'] for c in chans], ['CH1', 'CH2', 'CH3', 'CH4'])
        self.assertEqual([c['laser'] for c in chans],
                         ['MaiTai e HP DS-OL', 'MaiTai e HP DS-OL', 'INSIGHT X3-OL', 'INSIGHT X3-OL'])
        self.assertEqual(chans[0]['detector'], 'BNDD1')
        # FV4000 detectors record HV 0 with auto HV on; reported as recorded, not "fixed".
        self.assertTrue(self.s['autoHV'])
        self.assertEqual({c['hv'] for c in chans}, {0.0})

    def test_one_phase(self):
        self.assertEqual(self.s['phaseCount'], 1)

    def test_axes(self):
        self.assertEqual(self.s['zStack'], {'stepUm': 2.0, 'count': 31.0})
        self.assertEqual(self.s['timelapse'], {'intervalS': 15.0, 'count': 181.0})

    def test_date_and_body(self):
        self.assertEqual(self.s['acquiredAt'], '2026-07-16T13:44:05.228+10:00')
        self.assertEqual(self.s['microscopeBody'], 'BX63LF')

    def test_emission_filter_per_channel(self):
        # From the image-info block, matched to the acquisition channel by id, not by position.
        self.assertEqual([c['emissionFilter'] for c in self.s['channels']],
                         ['BA410-455', 'BA495-540', 'ET585/40M', 'ET720/60M'])

    def test_dichroics_in_the_path(self):
        # `dmSlider` is Empty on this file and is left out.
        self.assertEqual(self.s['dichroics'], [
            {'slot': 'excitationDM', 'name': 'MIRROR'},
            {'slot': 'irLaserDichroic', 'name': 'LCDM690-1050'},
            {'slot': 'firstMirrorCube', 'name': 'DM760'},
            {'slot': 'secondMirrorCube', 'name': 'MIRROR'},
        ])


class TestFVMPE(unittest.TestCase):
    """Galvo two-photon time-lapse with Bright Z OFF but ramp points still stored."""

    @classmethod
    def setUpClass(cls):
        cls.s = oir_meta.parse_settings(_read(FVMPE))

    def test_bright_z_off_even_though_points_are_stored(self):
        bz = self.s['brightZ']
        self.assertFalse(bz['enabled'])
        self.assertEqual(len(bz['points']), 2)

    def test_galvo_timing_and_hv(self):
        self.assertEqual(self.s['scanner']['type'], 'Galvano')
        self.assertAlmostEqual(self.s['scanner']['pixelDwellUs'], 2.0)
        self.assertEqual({c['hv'] for c in self.s['channels']}, {550.0})

    def test_interval_bio_formats_dropped(self):
        # This file's converted store has no TimeIncrement; the settings document has it.
        self.assertEqual(self.s['timelapse']['intervalS'], 10.0)

    def test_active_z_axis_is_the_enabled_one(self):
        # A second, inactive ZSTACK axis (3 um x 42) is also stored.
        self.assertEqual(self.s['zStack'], {'stepUm': 5.0, 'count': 8.0})

    def test_date_filters_dichroics(self):
        self.assertEqual(self.s['acquiredAt'], '2021-12-01T17:49:23.561+11:00')
        self.assertEqual([c['emissionFilter'] for c in self.s['channels']],
                         ['BA642-709', 'BA570-616', 'BA500-550', 'BA415-485'])
        self.assertEqual([d['name'] for d in self.s['dichroics']], ['LCDM690-870', 'DM760', 'MIRROR'])


class TestEdits(unittest.TestCase):
    """Variants of a real document, edited the way a different acquisition would differ."""

    def test_a_laser_switched_off_is_not_listed(self):
        doc = _read(FV4000).replace(
            b'enable="true" id="INSIGHT X3-OL_131328Imaging_main_phase_1"',
            b'enable="false" id="INSIGHT X3-OL_131328Imaging_main_phase_1"', 1)
        self.assertNotEqual(doc, _read(FV4000))
        names = [l['name'] for l in oir_meta.parse_settings(doc)['lasers']]
        self.assertEqual(names, ['MaiTai e HP DS-OL'])

    def test_filter_follows_the_channel_id_not_the_position(self):
        # Swap the image-info order of CH1 and CH2: each acquisition channel keeps its own filter.
        doc = _read(FV4000)
        a = b'<fvCommonphase:filterName>BA410-455</fvCommonphase:filterName>'
        b = b'<fvCommonphase:filterName>BA495-540</fvCommonphase:filterName>'
        doc = doc.replace(a, b'@@A@@', 1).replace(b, a, 1).replace(b'@@A@@', b, 1)
        chans = oir_meta.parse_settings(doc)['channels']
        self.assertEqual([c['emissionFilter'] for c in chans[:2]], ['BA495-540', 'BA410-455'])

    def test_no_image_info_means_no_filter(self):
        doc = _read(FV4000).replace(b'<fvCommonphase:filterName>BA410-455</fvCommonphase:filterName>', b'', 1)
        self.assertIsNone(oir_meta.parse_settings(doc)['channels'][0]['emissionFilter'])


class TestContainer(unittest.TestCase):
    """Finding the document inside the binary container."""

    def _container(self, doc):
        # Frame bytes before it, another XML document before it, LUT-like bytes after — the order seen
        # in real files.
        return (os.urandom(5000) + b'<?xml version="1.0"?><fileinfo:fileInfomation>x</fileinfo:fileInfomation>'
                + os.urandom(300) + doc + os.urandom(4000))

    def test_extract_skips_other_documents(self):
        doc = _read(FV4000)
        self.assertEqual(oir_meta.extract_settings_xml(self._container(doc)), doc)

    def test_read_across_chunk_boundaries(self):
        doc = _read(FVMPE)
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, 'x.oir')
            with open(path, 'wb') as f:
                f.write(self._container(doc))
            self.assertEqual(oir_meta.read_settings_xml(path, chunk=4096), doc)
            self.assertEqual(oir_meta.acquisition_settings(path)['system'], 'FVMPE-RS')

    def test_no_document_and_missing_file_report_a_reason(self):
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, 'empty.oir')
            with open(path, 'wb') as f:
                f.write(os.urandom(2048))
            self.assertIn('reason', oir_meta.acquisition_settings(path))
            self.assertIn('reason', oir_meta.acquisition_settings(os.path.join(d, 'missing.oir')))


if __name__ == '__main__':
    unittest.main()
