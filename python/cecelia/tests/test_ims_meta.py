"""Tests for Imaris source-calibration recovery (`ims_meta`).

The interval these produce is written into `ccid.json` and the store's NGFF/OME-XML calibration, and
everything downstream — track speeds, dwell times, every per-minute rate — is computed from it. So the
cases that matter most are the ones where it must return NOTHING: an irregular series has no single
interval, and a plausible-looking median would be indistinguishable from a measured one.

Run with `pixi run test-py`.
"""
import os
import random
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

    def test_whole_second_rounding_is_not_irregular(self):
        # A steady ~5.1 s series stamped to the whole second (a Zeiss Elyra SIM file via Imaris) reads
        # back as 5 s and 6 s gaps: 20% apart, but only rounding, so within one resolution.
        gaps = [5.0, 5.0, 6.0, 5.0, 5.0, 6.0, 5.0, 5.0, 5.0, 6.0]
        self.assertFalse(ims_meta.summarise_gaps(gaps)[1])
        interval, uniform, _ = ims_meta.summarise_gaps(gaps, resolution=1.0)
        self.assertTrue(uniform)
        self.assertAlmostEqual(interval, 5.3)          # the mean — the rounding cancels over the span

    def test_whole_second_rounding_still_catches_a_pause(self):
        _, uniform, _ = ims_meta.summarise_gaps([5.0, 6.0, 5.0, 60.0, 5.0], resolution=1.0)
        self.assertFalse(uniform)

    def test_whole_second_rounding_still_catches_a_rate_change(self):
        # 5 s then 7 s: no steady ~6 s series rounds to that, so it is a rate change, not rounding.
        self.assertFalse(ims_meta.summarise_gaps([5.0] * 4 + [7.0] * 4, resolution=1.0)[1])
        self.assertTrue(ims_meta.summarise_gaps([5.0, 6.0] * 4, resolution=1.0)[1])

    def test_whole_second_rounding_still_catches_a_fast_start(self):
        # The opening of the Elyra file: three fast frames before the steady ~5.1 s. Not rounding.
        steady = [6.0, 5.0, 5.0, 6.0, 5.0, 5.0, 6.0, 5.0, 5.0, 6.0, 5.0, 5.0, 5.0, 6.0, 5.0, 5.0, 5.0]
        self.assertTrue(ims_meta.summarise_gaps(steady, resolution=1.0)[1])
        self.assertFalse(ims_meta.summarise_gaps([2.0, 3.0, 2.0] + steady, resolution=1.0)[1])

    def test_spread_is_the_largest_deviation_over_the_median(self):
        # 6 against a median of 5: one fifth.
        self.assertAlmostEqual(ims_meta.summarise_gaps([5.0, 5.0, 6.0])[2], 0.2)

    def test_empty_and_degenerate_input(self):
        self.assertEqual(ims_meta.summarise_gaps([]), (None, False, None))
        self.assertEqual(ims_meta.summarise_gaps([0.0, 0.0]), (None, False, None))


# The `TimePointN` gaps of VYZ9TQ (Zeiss Elyra SIM via Imaris, project I10ZTq), read from its store's
# OME original metadata: three fast frames, then a steady ~5.26 s stamped to the whole second.
_VYZ9TQ_GAPS = [2.0, 3.0, 2.0] + [float(g) for g in
                '655655655565565556555655565556555655556556555655565555']


class SteadyIntervalTest(unittest.TestCase):
    """An irregular series' interval from its steady part (`steady_interval`) — pure, no HDF5."""

    def test_real_fast_start_yields_the_steady_interval(self):
        interval, (start, stop), deviating = ims_meta.steady_interval(_VYZ9TQ_GAPS, resolution=1.0)
        self.assertAlmostEqual(interval, 5.26, delta=0.01)
        self.assertEqual((start, stop), (3, 57))
        self.assertEqual(ims_meta.deviating_frames(deviating), [[0, 3]])

    def test_exact_fast_start_yields_the_steady_median(self):
        interval, (start, stop), deviating = ims_meta.steady_interval([2.0, 3.0, 2.0] + [5.26] * 20)
        self.assertEqual(interval, 5.26)
        self.assertEqual((start, stop), (3, 23))
        self.assertEqual(deviating, [0, 1, 2])

    def test_a_pause_mid_series_keeps_the_interval_and_flags_the_pause(self):
        interval, _, deviating = ims_meta.steady_interval([30.0] * 10 + [600.0] + [30.0] * 9)
        self.assertEqual(interval, 30.0)
        self.assertEqual(ims_meta.deviating_frames(deviating), [[10, 11]])

    def test_random_gaps_have_no_steady_part(self):
        rng = random.Random(7)
        for _ in range(50):
            gaps = [rng.uniform(1.0, 60.0) for _ in range(40)]
            self.assertIsNone(ims_meta.steady_interval(gaps))
            self.assertIsNone(ims_meta.steady_interval([float(round(g)) for g in gaps], resolution=1.0))

    def test_two_equal_halves_at_different_rates_have_no_steady_part(self):
        self.assertIsNone(ims_meta.steady_interval([30.0] * 5 + [60.0] * 5))

    def test_too_few_gaps(self):
        self.assertIsNone(ims_meta.steady_interval([]))
        self.assertIsNone(ims_meta.steady_interval([5.0, 9.0]))

    def test_a_steady_run_of_the_last_two_gaps_counts(self):
        # Two of three gaps at 5 s is a majority, wherever they sit.
        self.assertEqual(ims_meta.steady_interval([9.0, 5.0, 5.0]), (5.0, (1, 3), [0]))

    def test_a_gap_exactly_at_the_tolerance_is_steady(self):
        # `tol` is inclusive, as in `summarise_gaps`: 21 s is exactly 5% off 20 s.
        self.assertEqual(ims_meta.steady_interval([20.0, 20.0, 20.0, 21.0]), (20.0, (0, 4), []))

    def test_equal_runs_either_side_of_a_pause_measure_on_the_first(self):
        _, run, deviating = ims_meta.steady_interval([30.0] * 5 + [600.0] + [30.0] * 5)
        self.assertEqual(run, (0, 5))
        self.assertEqual(deviating, [5])

    def test_consecutive_deviating_gaps_merge_into_one_frame_range(self):
        self.assertEqual(ims_meta.deviating_frames([0, 1, 2, 7, 9, 10]), [[0, 3], [7, 8], [9, 11]])


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

    def test_dataset_times_are_exact_so_get_no_rounding_allowance(self):
        # Nanosecond births of 5/6 s gaps are a real irregularity, not rounding.
        _ims(self._p(), births_s=[0, 5, 11, 16, 22])
        self.assertNotIn('TimeIncrement', ims_meta.time_increment(self._p()))

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

    def test_whole_second_stamps_with_rounded_gaps_yield_the_mean_interval(self):
        # Gaps of 5 and 6 s from stamps with no fractional part: rounding, not an irregular series.
        secs = [0, 5, 10, 16, 21, 26, 32, 37, 42, 47, 53]
        _ims(self._p(), stamps=[f'2025-09-29 01:{8 + (58 + s) // 60:02d}:{(58 + s) % 60:02d}'
                                for s in secs])
        r = ims_meta.time_increment(self._p())
        self.assertAlmostEqual(r['TimeIncrement'], 5.3)
        self.assertEqual(r['source'], 'DataSetInfo/TimeInfo')

    def test_millisecond_stamps_keep_the_strict_tolerance(self):
        # The same 5/6 s gaps stamped to the millisecond ARE irregular: the allowance is for rounding.
        _ims(self._p(), stamps=['2026-07-29 14:00:00.000', '2026-07-29 14:00:05.000',
                                '2026-07-29 14:00:11.000', '2026-07-29 14:00:16.000'])
        self.assertNotIn('TimeIncrement', ims_meta.time_increment(self._p()))

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

    def test_irregular_series_without_a_steady_part_yields_no_interval(self):
        # THE case this module exists to get right: never flatten an irregular series to a median.
        # Two steady gaps of four is no majority, so there is no steady part to take it from.
        _ims(self._p(), births_s=[0, 30, 60, 600, 630])
        r = ims_meta.time_increment(self._p())
        self.assertNotIn('TimeIncrement', r)
        self.assertFalse(r['uniform'])
        self.assertEqual(r['frames'], 5)
        self.assertGreater(r['spread'], 1.0)
        self.assertIn('no steady interval', r['reason'])

    def test_fast_start_then_steady_yields_the_steady_interval_flagged(self):
        # VYZ9TQ (Zeiss Elyra SIM via Imaris): whole-second stamps 2, 3, 2 s apart, then a steady
        # ~5.26 s (rounded to 5/6 s gaps). The interval is the steady part's, with frames 0–3 flagged.
        secs, t = [0, 2, 5, 7], 7.0
        for _ in range(30):
            t += 5.26
            secs.append(int(t))
        _ims(self._p(), stamps=[f'2025-09-29 01:{s // 60:02d}:{s % 60:02d}' for s in secs])
        r = ims_meta.time_increment(self._p())
        self.assertAlmostEqual(r['TimeIncrement'], 5.26, delta=0.05)
        self.assertEqual((r['source'], r['frames']), ('DataSetInfo/TimeInfo', 34))
        self.assertGreater(r['spread'], 0.5)       # 2 s against a 5 s median
        self.assertFalse(r['uniform'])
        self.assertEqual(r['steadyFrames'], [3, 33])
        self.assertEqual(r['irregularFrames'], [[0, 3]])

    def test_irregular_measured_series_does_not_fall_through_to_the_nominal_value(self):
        # An irregular series is an ANSWER — "there is no one interval" — not a failed lookup. Falling
        # through would report the requested interval as though it were what happened.
        # Gaps of 30, 60, 210 s: no steady part either.
        _ims(self._p(), births_s=[0, 30, 90, 300], oir_step='30.000')
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

    def test_objective_without_na_suffix_still_gives_magnification(self):
        # NA recorded only in `NumericalAperture`: the name carries no `/NA`
        self._write({'Image': {**_REAL_IMAGE_ATTRS, 'Objective': 'UPlanSApo 20x',
                               'NumericalAperture': '0.75'}})
        o = ims_meta.acquisition_settings(self.path)['objective']
        self.assertEqual((o['name'], o['magnification'], o['na']), ('UPlanSApo 20x', 20.0, 0.75))
        self.assertEqual(ims_meta._objective('20x LWD', None),
                         {'name': '20x LWD', 'magnification': 20.0, 'na': None, 'immersion': None})
        self.assertEqual(ims_meta._objective('Plan-Apochromat 63x/1.40 Oil', None)['na'], 1.4)
        self.assertIsNone(ims_meta._objective('Plan-Neofluar', None)['magnification'])

    def test_no_metadata_is_a_reason_not_an_error(self):
        self._write({'Imaris': {'Version': '7.7'}})
        self.assertIn('reason', ims_meta.acquisition_settings(self.path))


if __name__ == '__main__':
    unittest.main()
