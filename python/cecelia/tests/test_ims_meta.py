"""Tests for Imaris source-calibration recovery (`ims_meta`).

The interval these produce is written into `ccid.json` and the store's NGFF/OME-XML calibration, and
everything downstream — track speeds, dwell times, every per-minute rate — is computed from it. So the
cases that matter most are the ones where it must return NOTHING: an irregular series has no single
interval, and a plausible-looking median would be indistinguishable from a measured one.

Run with `pixi run test-py`.
"""
import os
import shutil
import tempfile
import unittest

import cecelia.utils.ims_meta as ims_meta

try:
    import h5py
    import numpy as np
    HAVE_H5PY = True
except ImportError:                       # h5py is a pixi-tier dep, not part of the IO tier
    HAVE_H5PY = False

_TIME_DTYPE = [('ID', '<i8'), ('Birth', '<i8'), ('Death', '<i8'), ('IDTimeBegin', '<i8')]


def _chars(s):
    """An Imaris string attribute: an array of one-byte chars, not a string."""
    return np.frombuffer(s.encode(), dtype='S1')


def _ims(path, births_s=None, stamps=None, oir_step=None, lsm_interval=None):
    """A miniature .ims carrying whichever of the timing sources the test needs."""
    with h5py.File(path, 'w') as f:
        if births_s is not None:
            rows = [(i, int(b * 1e9), int(b * 1e9) + 1, 0) for i, b in enumerate(births_s)]
            f.create_dataset('DataSetTimes/Time', data=np.array(rows, dtype=_TIME_DTYPE))
        ti = f.create_group('DataSetInfo/TimeInfo')
        if stamps is not None:
            ti.attrs.create('DatasetTimePoints', _chars(str(len(stamps))))
            for i, s in enumerate(stamps, start=1):
                ti.attrs.create(f'TimePoint{i}', _chars(s))
        if oir_step is not None:
            g = f.create_group('DataSetInfo/OIR Dataset Size')
            g.attrs.create('Time Step', _chars(oir_step))
        if lsm_interval is not None:
            g = f.create_group('DataSetInfo/LSM Dataset Size')
            g.attrs.create('Time Interval [s]', _chars(lsm_interval))


class SummariseGapsTest(unittest.TestCase):
    """Pure logic — no HDF5 needed, so it runs everywhere."""

    def test_uniform_series_yields_its_interval(self):
        interval, uniform, spread = ims_meta.summarise_gaps([30.0, 30.0, 30.0])
        self.assertEqual(interval, 30.0)
        self.assertTrue(uniform)
        self.assertEqual(spread, 0.0)

    def test_small_jitter_is_still_uniform(self):
        # Acquisition clocks wobble by a fraction of a frame; that is not an irregular series.
        interval, uniform, _ = ims_meta.summarise_gaps([30.0, 30.4, 29.7, 30.0])
        self.assertTrue(uniform)
        self.assertEqual(interval, 30.0)

    def test_paused_acquisition_is_not_uniform(self):
        _, uniform, spread = ims_meta.summarise_gaps([30.0, 30.0, 600.0, 30.0])
        self.assertFalse(uniform)
        self.assertGreater(spread, 1.0)

    def test_empty_and_degenerate_input(self):
        self.assertEqual(ims_meta.summarise_gaps([]), (None, False, None))
        self.assertEqual(ims_meta.summarise_gaps([0.0, 0.0]), (None, False, None))


@unittest.skipUnless(HAVE_H5PY, 'h5py not installed')
class TimeIncrementTest(unittest.TestCase):
    def setUp(self):
        self.d = tempfile.mkdtemp()

    def tearDown(self):
        shutil.rmtree(self.d, ignore_errors=True)

    def _p(self, name='a.ims'):
        return os.path.join(self.d, name)

    def test_dataset_times_is_preferred(self):
        # All three present and disagreeing: the numeric per-timepoint record wins.
        _ims(self._p(), births_s=[0, 30, 60, 90],
             stamps=['2026-07-29 14:00:00.000', '2026-07-29 14:00:11.000',
                     '2026-07-29 14:00:22.000', '2026-07-29 14:00:33.000'],
             oir_step='99.0')
        r = ims_meta.time_increment(self._p())
        self.assertEqual(r['TimeIncrement'], 30.0)
        self.assertEqual(r['source'], 'DataSetTimes/Time')
        self.assertEqual(r['frames'], 4)

    def test_falls_back_to_timepoint_stamps(self):
        _ims(self._p(), stamps=['2026-07-29 14:00:00.000', '2026-07-29 14:00:30.000',
                                '2026-07-29 14:01:00.000'], oir_step='99.0')
        r = ims_meta.time_increment(self._p())
        self.assertEqual(r['TimeIncrement'], 30.0)
        self.assertEqual(r['source'], 'DataSetInfo/TimeInfo')

    def test_stamps_without_fractional_seconds_parse(self):
        _ims(self._p(), stamps=['2026-07-29 14:00:00', '2026-07-29 14:00:30',
                                '2026-07-29 14:01:00'])
        self.assertEqual(ims_meta.time_increment(self._p())['TimeIncrement'], 30.0)

    def test_nanosecond_stamps_parse(self):
        # Imaris 7.7 writes nine fractional digits for a file converted from a Zeiss .lsm — the first
        # three stamps of Immunemap video 2JBABTBNRF8MR978J016GM606A, verbatim. `%f` takes six, so
        # these once failed to parse and the whole series read as "no timing in the file".
        _ims(self._p(), stamps=['2012-11-07 12:20:54.742000000', '2012-11-07 12:21:24.743000000',
                                '2012-11-07 12:21:54.743000000'])
        r = ims_meta.time_increment(self._p())
        self.assertAlmostEqual(r['TimeIncrement'], 30.0, places=2)
        self.assertEqual(r['source'], 'DataSetInfo/TimeInfo')

    def test_falls_back_to_the_nominal_lsm_interval(self):
        # `DataSetInfo/LSM Dataset Size` of the same file: the Zeiss counterpart of the Olympus step.
        _ims(self._p(), lsm_interval='30.000')
        r = ims_meta.time_increment(self._p())
        self.assertEqual(r['TimeIncrement'], 30.0)
        self.assertTrue(r['nominal'])
        self.assertEqual(r['source'], 'DataSetInfo/LSM Dataset Size')

    def test_no_unit_is_returned_with_the_interval(self):
        # The value is seconds by construction and the unit is the STORING tier's business — ccid/NGFF
        # want `second`, OME-XML wants `s`. Emitting one here is the bypass `test_ome_unit_symbols`
        # exists to catch.
        _ims(self._p(), births_s=[0, 30, 60])
        self.assertNotIn('TimeIncrementUnit', ims_meta.time_increment(self._p()))

    def test_falls_back_to_the_nominal_oir_step(self):
        _ims(self._p(), oir_step='30.000')
        r = ims_meta.time_increment(self._p())
        self.assertEqual(r['TimeIncrement'], 30.0)
        self.assertTrue(r['nominal'])       # declared, not measured — the caller says so in the log

    def test_irregular_series_yields_no_interval(self):
        # THE case this module exists to get right: never flatten a paused acquisition to a median.
        _ims(self._p(), births_s=[0, 30, 60, 600, 630])
        r = ims_meta.time_increment(self._p())
        self.assertNotIn('TimeIncrement', r)
        self.assertFalse(r['uniform'])
        self.assertIn('no single interval', r['reason'])

    def test_irregular_measured_series_does_not_fall_through_to_the_nominal_value(self):
        # An irregular series is an ANSWER — "there is no one interval" — not a failed lookup. Falling
        # through would report the requested interval as though it were what happened.
        _ims(self._p(), births_s=[0, 30, 60, 600], oir_step='30.000')
        self.assertNotIn('TimeIncrement', ims_meta.time_increment(self._p()))

    def test_single_timepoint_is_not_a_timelapse(self):
        _ims(self._p(), births_s=[0], stamps=['2026-07-29 14:00:00.000'])
        r = ims_meta.time_increment(self._p())
        self.assertNotIn('TimeIncrement', r)
        self.assertEqual(r['reason'], 'no timing in the file')

    def test_file_without_timing_reports_a_reason(self):
        _ims(self._p())
        self.assertEqual(ims_meta.time_increment(self._p())['reason'], 'no timing in the file')

    def test_missing_file_is_reported_not_raised(self):
        self.assertEqual(ims_meta.time_increment(self._p('gone.ims'))['reason'], 'file not found')

    def test_non_hdf5_file_is_reported_not_raised(self):
        with open(self._p(), 'wb') as f:
            f.write(b'not hdf5')
        self.assertIn('could not read', ims_meta.time_increment(self._p())['reason'])

    def test_unparseable_stamp_invalidates_the_series(self):
        _ims(self._p(), stamps=['2026-07-29 14:00:00.000', 'not a date',
                                '2026-07-29 14:01:00.000'])
        self.assertNotIn('TimeIncrement', ims_meta.time_increment(self._p()))



# `DataSetInfo/Image` of Immunemap video 2JBABTBNRF8MR978J016GM606A (Obino-Movie1-SteadyState.ims),
# copied verbatim from the downloaded file — a 10x overview, so 1.8 µm pixels.
_REAL_IMAGE_ATTRS = {'ExtMin0': '0', 'ExtMin1': '0', 'ExtMin2': '0',
                     'ExtMax0': '921.363', 'ExtMax1': '921.363', 'ExtMax2': '36',
                     'X': '512', 'Y': '512', 'Z': '9', 'Unit': 'um'}


def _ims_image(path, attrs):
    with h5py.File(path, 'w') as f:
        g = f.create_group('DataSetInfo/Image')
        for k, v in attrs.items():
            g.attrs.create(k, _chars(v))


@unittest.skipUnless(HAVE_H5PY, 'h5py not installed')
class VoxelSizeTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.path = os.path.join(self.tmp, 'x.ims')

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def test_real_header_gives_extent_over_count(self):
        _ims_image(self.path, _REAL_IMAGE_ATTRS)
        r = ims_meta.voxel_size(self.path)
        x, y, z = r['voxelSizeXYZ']
        self.assertAlmostEqual(x, 921.363 / 512)   # 1.7995 — what Immunemap publishes as 1.7995
        self.assertAlmostEqual(y, 921.363 / 512)
        self.assertAlmostEqual(z, 4.0)

    def test_nanometre_unit_is_converted(self):
        _ims_image(self.path, {**_REAL_IMAGE_ATTRS, 'Unit': 'nm', 'ExtMax0': '921363'})
        self.assertAlmostEqual(ims_meta.voxel_size(self.path)['voxelSizeXYZ'][0], 921.363 / 512)

    def test_missing_extent_yields_nothing_not_a_guess(self):
        attrs = {k: v for k, v in _REAL_IMAGE_ATTRS.items() if k != 'ExtMax2'}
        _ims_image(self.path, attrs)
        r = ims_meta.voxel_size(self.path)
        self.assertIsNone(r['voxelSizeXYZ'])
        self.assertIn('reason', r)

    def test_missing_file(self):
        self.assertIsNone(ims_meta.voxel_size(os.path.join(self.tmp, 'nope.ims'))['voxelSizeXYZ'])



# `DataSetInfo` groups of the same Immunemap video, verbatim (an Imaris 7.7 file converted from a
# Zeiss .lsm): what `acquisition_settings` reads.
_REAL_INFO = {
    'Image': {**_REAL_IMAGE_ATTRS, 'Description': 'Objective: Plan-Neofluar 10x/0.3',
              'NumericalAperture': '0.3', 'RecordingDate': '2012-11-07 12:20:54.742'},
    'Channel 0': {'Name': 'ChD-T1', 'LSMExcitationWavelength': '890.000',
                  'LSMEmissionWavelength': 'BP 612/69', 'LSMPinhole': '0.000',
                  'Objective': 'Plan-Neofluar 10x/0.3'},
    'Channel 1': {'Name': 'NDD2-T1', 'LSMEmissionWavelength': 'BP 520/35', 'LSMPinhole': '0.000',
                  'Objective': 'Plan-Neofluar 10x/0.3'},
    'Manufactor Information': {'Manufactor Model': 'LSM 510 or LSM 710', 'Manufactor String': 'Zeiss',
                               'Manufactor Type': 'LSM', 'Microscope Mode': 'not known'},
    'LSM Dataset Size': {'Number of Timepoints (Dimension T)': '60',
                         'Number of Z-Slices (Dimension Z)': '9', 'Time Interval [s]': '30.000',
                         'Voxelsize Z [um]': '4.000'},
}


@unittest.skipUnless(HAVE_H5PY, 'h5py not installed')
class AcquisitionSettingsTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.mkdtemp()
        self.path = os.path.join(self.tmp, 'x.ims')

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def _write(self, groups):
        with h5py.File(self.path, 'w') as f:
            for name, attrs in groups.items():
                g = f.create_group(f'DataSetInfo/{name}')
                for k, v in attrs.items():
                    g.attrs.create(k, _chars(v))

    def test_real_lsm_sourced_file(self):
        self._write(_REAL_INFO)
        a = ims_meta.acquisition_settings(self.path)
        self.assertEqual(a['source'], 'ims')
        self.assertEqual(a['system'], 'Zeiss LSM 510 or LSM 710')
        self.assertEqual(a['acquiredAt'], '2012-11-07T12:20:54')
        self.assertEqual(a['objective']['magnification'], 10.0)
        self.assertEqual(a['objective']['na'], 0.3)
        self.assertEqual([c['emissionFilter'] for c in a['channels']], ['BP 612/69', 'BP 520/35'])
        # only the channel whose excitation is RECORDED says so — the other is not assumed to share it
        self.assertEqual([c['laser'] for c in a['channels']], ['890 nm', None])
        self.assertEqual(a['lasers'], [{'name': '890 nm', 'wavelengthNm': 890.0, 'transmissionPct': None}])
        self.assertIsNone(a['channels'][0]['pinholeUm'])      # 0 = non-descanned, not a 0 µm pinhole
        self.assertEqual(a['zStack'], {'stepUm': 4.0, 'count': 9})
        self.assertEqual(a['timelapse'], {'intervalS': 30.0, 'count': 60})

    def test_no_metadata_is_a_reason_not_an_error(self):
        self._write({'Imaris': {'Version': '7.7'}})
        self.assertIn('reason', ims_meta.acquisition_settings(self.path))


if __name__ == '__main__':
    unittest.main()
