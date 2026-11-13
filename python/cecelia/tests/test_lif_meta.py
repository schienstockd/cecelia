"""`lif_meta` against the header XML of a real `.lif` (test-data/acquisition/lif_*).

The fixture is the header of a LAS X 3.4.2 file from a Leica DM5500B widefield with a DFC365 FX
camera, cut to three of its images (Image001, Image002, Image007) and otherwise unchanged. Image007
holds ONE recorded channel while its configuration still lists two: the LUT match is checked on it.
Expected values are read from the fixture's own attributes in the test, not typed from the parser.
"""
import os
import struct
import tempfile
import unittest
import xml.etree.ElementTree as ET  # reads the fixture's own attributes to cross-check the parser

from cecelia.utils import lif_meta

_FIX = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data', 'acquisition'))
HEADER = os.path.join(_FIX, 'lif_dm5500b_las-x-3.4.2_header.xml')


def _xml():
    with open(HEADER, encoding='utf-8') as f:
        return f.read()


def write_lif(xml, path, magic=0x70):
    """A `.lif` holding `xml` as its header block (int32 magic, int32 length, byte 0x2A, int32 chars,
    UTF-16LE XML), then one 1-byte stand-in data block per image element: readlif (`LifFile.__init__`)
    refuses a file whose image list and data blocks differ in number. Block: int32 magic, int32 0,
    byte 0x2A, int32 length, byte 0x2A, int32 description chars (0), the data."""
    data = xml.encode('utf-16-le')
    n_images = sum(1 for e in ET.fromstring(xml).iter('Element') if e.find('Data/Image') is not None)
    with open(path, 'wb') as f:
        f.write(struct.pack('<iiBi', magic, len(data) + 5, 0x2A, len(xml)) + data)
        for _ in range(n_images):
            f.write(struct.pack('<iiBiBi', 0x70, 0, 0x2A, 1, 0x2A, 0) + b'\0')
    return path


class _LifCase(unittest.TestCase):
    def lif(self, xml, magic=0x70):
        fd, path = tempfile.mkstemp(suffix='.lif')
        os.close(fd)
        self.addCleanup(os.remove, path)
        return write_lif(xml, path, magic)

    def settings(self, xml, series):
        return lif_meta.acquisition_settings(self.lif(xml), series=series)


def _element(name):
    return next(e for e in ET.fromstring(_xml()).iter('Element') if e.get('Name') == name)


class TestWidefieldCamera(_LifCase):
    def setUp(self):
        self.s = self.settings(_xml(), series=1)          # Image002, two channels
        self.infos = list(_element('Image002').iter('WideFieldChannelInfo'))

    def test_series_is_readlifs_index(self):
        self.assertEqual([self.settings(_xml(), i)['imageName'] for i in range(3)], ['Image001', 'Image002', 'Image007'])

    def test_system_technology_and_objective(self):
        s = self.s
        self.assertEqual((s['source'], s['technology'], s['system']), ('lif', 'widefield', 'DM5500B'))
        self.assertEqual(s['readerVersion'], lif_meta.READER_VERSION)
        self.assertEqual(s['softwareVersion'], 'LAS X 3.4.2.18368')
        self.assertEqual(s['objective'], {'name': 'HC PL FLUOTAR 20x/0.50 DRY', 'magnification': 20.0,
                                          'na': 0.5, 'immersion': 'dry'})
        self.assertEqual(s['lasers'], [])

    def test_camera_without_serial(self):
        self.assertEqual(self.s['camera'], {'name': 'DFC365FX', 'binning': 2, 'bitDepth': 12, 'sensorPixelUm': 6.45})

    def test_channel_settings_as_recorded(self):
        chans = self.s['channels']
        self.assertEqual([c['filterCube'] for c in chans], [w.get('FluoCubeName') for w in self.infos])
        for c, w in zip(chans, self.infos):
            self.assertAlmostEqual(c['exposureMs'], float(w.get('ExposureTime')) * 1000, places=2)
            self.assertAlmostEqual(c['gain'], float(w.get('Gain')))
            self.assertEqual(c['illuminationIntensity'], float(w.get('IL_Light-Intensity')))
            self.assertEqual((c['detector'], c['hv'], c['bitDepth']), ('DFC365FX', None, 12))

    def test_acquired_at_is_the_filetime_stamp(self):
        ticks = int(_element('Image002').findtext('Data/Image/TimeStampList').split()[0], 16)
        self.assertEqual(ticks // 10_000_000 - 11_644_473_600,   # FILETIME seconds → Unix seconds
                         int(__import__('datetime').datetime.fromisoformat(self.s['acquiredAt']).timestamp()))
        self.assertTrue(self.s['acquiredAt'].endswith('+00:00'))

    def test_single_plane_has_no_stack_or_timelapse(self):
        self.assertNotIn('zStack', self.s)
        self.assertNotIn('timelapse', self.s)


class TestFewerRecordedThanConfigured(_LifCase):
    def test_one_recorded_channel_gets_the_matching_lut_settings(self):
        s = self.settings(_xml(), series=2)              # Image007
        self.assertEqual(s['imageName'], 'Image007')
        self.assertEqual(len(s['channels']), 1)                    # data holds one; config lists two
        lut = _element('Image007').find('.//ChannelDescription').get('LUTName')
        want = next(w for w in _element('Image007').iter('WideFieldChannelInfo') if w.get('LUT') == lut)
        self.assertEqual(s['channels'][0]['filterCube'], want.get('FluoCubeName'))


class TestFile(_LifCase):
    def test_not_a_lif_and_bad_series_give_reasons(self):
        self.assertIn('reason', lif_meta.acquisition_settings(self.lif(_xml(), magic=0x71)))
        self.assertIn('not in the file', self.settings(_xml(), series=3)['reason'])
        self.assertIn('reason', lif_meta.acquisition_settings('/nonexistent/x.lif'))

    def test_neither_camera_nor_confocal_is_a_reason(self):
        xml = _xml().replace('ATLCameraSettingDefinition', 'ATLUnknownSettingDefinition')
        self.assertIn('no camera settings', self.settings(xml, series=0)['reason'])


if __name__ == '__main__':
    unittest.main()


CONFOCAL = os.path.join(_FIX, 'lif_sp8_las-x-3.5.7_header.xml')


class TestConfocalSequential(_LifCase):
    """A TCS SP8 sequential z-stack (header cut to two images, WT and Mer1; paths scrubbed). Four
    sequences, one channel each, all on PMT 2; the channel order in the data is Red, Green, Magenta,
    Gray. Expected values are read from each sequence's own XML."""

    @classmethod
    def setUpClass(cls):
        with open(CONFOCAL, encoding='utf-8') as f:
            cls.xml = f.read()
        root = ET.fromstring(cls.xml)
        wt = next(e for e in root.iter('Element') if e.get('Name') == 'WT')
        cls.seqs = wt.findall("Data/Image/Attachment[@Name='HardwareSetting']/LDM_Block_Sequential/"
                              "LDM_Block_Sequential_List/ATLConfocalSettingDefinition")

    def setUp(self):
        self.s = self.settings(self.xml, series=0)

    def test_system_technology_and_stack(self):
        s = self.s
        self.assertEqual((s['technology'], s['system'], s['microscopeBody']), ('confocal', 'TCS SP8', 'DMI6000B-CS'))
        self.assertTrue(s['sequential'])
        self.assertEqual(s['objective']['na'], 1.3)
        self.assertEqual(s['zStack']['count'], 27)
        self.assertEqual(s['pinholeAiry'], 1.0)

    def test_each_channel_takes_its_own_sequence(self):
        self.assertEqual(len(self.seqs), 4)
        self.assertEqual(len(self.s['channels']), 4)
        for k, (ch, seq) in enumerate(zip(self.s['channels'], self.seqs)):
            det = next(d for d in seq.iter('Detector') if d.get('IsActive') == '1')
            on = [int(float(l.get('LaserLine'))) for l in seq.iter('LaserLineSetting') if float(l.get('IntensityDev')) > 0]
            with self.subTest(channel=ch['name']):
                self.assertAlmostEqual(ch['hv'], float(det.get('Gain')))          # PMT: gain is its voltage
                self.assertEqual([l['wavelengthNm'] for l in ch['laserLines']], on)
                self.assertEqual(ch['detector'], det.get('Name'))
                self.assertEqual(ch['sequence'], k + 1)     # the card groups channels by it
        self.assertEqual([c['dyePreset'] for c in self.s['channels']], ['dTomato', 'EGFP', 'ALEXA 647', 'DAPI'])

    def test_a_second_image_has_its_own_settings(self):
        mer = self.settings(self.xml, series=1)
        self.assertEqual(mer['imageName'], 'Mer1')
        self.assertNotEqual([c['hv'] for c in mer['channels']], [c['hv'] for c in self.s['channels']])
