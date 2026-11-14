"""`lif_meta` on the real STELLARIS 8 header, one recorded value changed per test, where the
fixtures as recorded can't tell a right reading from a wrong one (docs/todo/TEST_VALUE_PLAN.md → P2).

Expected values come from the module docstring and the shape `_integration` names: channels pair with
detectors by LUT colour, a line at or above 680 nm is two-photon, step = z length / (planes − 1),
interval = t length / (time points − 1), and the first averaging or accumulation above 1 is the one read.
"""
import os
import tempfile
import unittest
import xml.etree.ElementTree as ET

from cecelia.utils import lif_meta
from cecelia.tests.test_lif_meta import write_lif

_HEADER = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data',
                                        'acquisition', 'lif_stellaris8_las-x-4.8.2_header.xml'))
_MOVIE = 1   # Series004: 6 z × 126 t; Series001 is a single plane


class _Stellaris(unittest.TestCase):
    def setUp(self):
        self.root = ET.parse(_HEADER).getroot()

    def image(self, series=_MOVIE):
        return [e for e in self.root.iter('Element') if e.find('Data/Image') is not None][series].find('Data/Image')

    def top(self, series=_MOVIE):
        return self.image(series).find("Attachment[@Name='HardwareSetting']/ATLConfocalSettingDefinition")

    def read(self, series=_MOVIE):
        with tempfile.TemporaryDirectory() as d:
            path = write_lif(ET.tostring(self.root, encoding='unicode'), os.path.join(d, 'x.lif'))
            return lif_meta.acquisition_settings(path, series=series)


class Integration(_Stellaris):
    def test_as_recorded_line_average_2(self):
        self.assertEqual(self.read()['scanner']['integration'], {'type': 'Line', 'count': 2, 'average': True})

    def test_each_kind_reads_as_its_own_type_and_mode(self):
        cases = {'LineAverage': ('Line', True), 'FrameAverage': ('Frame', True),
                 'Line_Accumulation': ('Line', False), 'FrameAccumulation': ('Frame', False)}
        for key, (kind, average) in cases.items():
            with self.subTest(key=key):
                self.setUp()
                for k in cases:
                    self.top().set(k, '3' if k == key else '1')
                self.assertEqual(self.read()['scanner']['integration'],
                                 {'type': kind, 'count': 3, 'average': average})

    def test_all_one_is_none(self):
        for k in ('LineAverage', 'FrameAverage', 'Line_Accumulation', 'FrameAccumulation'):
            self.top().set(k, '1')
        self.assertEqual(self.read()['scanner']['integration'], {'type': 'None', 'count': 1, 'average': None})


class ChannelPairing(_Stellaris):
    def test_channels_follow_their_lut_not_their_position(self):
        channels = self.image().find('ImageDescription/Channels')
        recorded = list(channels)
        for c in recorded:
            channels.remove(c)
        for c in reversed(recorded):                       # Red, Green, Blue
            channels.append(c)
        got = [(c['name'], c['detector']) for c in self.read()['channels']]
        self.assertEqual(got, [('Red', 'HyD NDD 3'), ('Green', 'HyD NDD 2'), ('Blue', 'HyD NDD 1')])


class Technology(_Stellaris):
    def set_lines(self, nm):
        for line in self.image().iter('LaserLineSetting'):
            if float(line.get('IntensityDev') or 0) > 0:
                line.set('LaserLine', str(nm))

    def test_680_nm_is_two_photon_679_is_not(self):
        self.set_lines(680)
        self.assertEqual(self.read()['technology'], 'two-photon')
        self.set_lines(679)
        self.assertEqual(self.read()['technology'], 'confocal')


class Axes(_Stellaris):
    def dims(self):
        return {d.get('DimID'): d for d in self.image().iterfind('ImageDescription/Dimensions/DimensionDescription')}

    def test_as_recorded(self):
        r = self.read()
        self.assertEqual(r['zStack'], {'stepUm': 4.9998, 'count': 6})                  # 2.499885e-05 m / 5
        self.assertEqual(r['timelapse'], {'intervalS': 30.2952, 'count': 126})          # 3786.902 s / 125

    def test_two_planes_and_two_time_points_are_a_stack_and_a_movie(self):
        d = self.dims()
        d['3'].set('NumberOfElements', '2')
        d['4'].set('NumberOfElements', '2')
        r = self.read()
        self.assertEqual(r['zStack']['count'], 2)
        self.assertAlmostEqual(r['zStack']['stepUm'], 24.99885, places=3)          # the whole z length
        self.assertEqual(r['timelapse'], {'intervalS': 3786.902, 'count': 2})

    def test_one_time_point_is_not_a_movie(self):
        self.dims()['4'].set('NumberOfElements', '1')
        self.assertNotIn('timelapse', self.read())


class Reasons(_Stellaris):
    def test_no_hardware_settings_is_a_reason(self):
        image = self.image()
        image.remove(image.find("Attachment[@Name='HardwareSetting']"))
        self.assertEqual(self.read(), {'reason': 'no hardware settings recorded for this image'})

    def test_no_recorded_channels_is_a_reason(self):
        channels = self.image().find('ImageDescription/Channels')
        for c in list(channels):
            channels.remove(c)
        # readlif needs channels to list the image; parse the header directly
        r = lif_meta.parse_settings(self.root, [{'path': '20251219_Mouse47_D14_CARs only.xlef/', 'name': 'Series004'}], 0)
        self.assertEqual(r, {'reason': 'no channels in the image description'})


if __name__ == '__main__':
    unittest.main()
