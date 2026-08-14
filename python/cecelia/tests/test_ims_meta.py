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


def _ims(path, births_s=None, stamps=None, oir_step=None):
    """A miniature .ims carrying whichever of the three timing sources the test needs."""
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


if __name__ == '__main__':
    unittest.main()
