"""RESULT-REVIEW: result-787119c5

A series the whole-series rule rejects (gaps 30.9, 30.9, 28.8, 31.2, 30.0 s: 6.8% off their median)
can still have every gap within `tol` of its longest uniform run's interval (30 s). The steady-part
reader then found no deviating gap and returned the series as `uniform`, so the import recorded a
clean interval with no irregular flag. It must stay irregular and name the gaps the steady run did not
measure.
"""
import importlib.util
import os
import shutil
import tempfile
import unittest

from cecelia.utils import ims_meta

GAPS = [30.9, 30.9, 28.8, 31.2, 30.0]


class SteadyIntervalAllCoveredTest(unittest.TestCase):
    def test_the_series_is_irregular_by_the_whole_series_rule(self):
        self.assertFalse(ims_meta.summarise_gaps(GAPS)[1])

    def test_gaps_outside_the_steady_run_are_flagged(self):
        interval, run, deviating = ims_meta.steady_interval(GAPS)
        self.assertEqual(run, (2, 5))
        self.assertEqual(deviating, [0, 1])

    @unittest.skipUnless(importlib.util.find_spec('h5py'), 'h5py not installed')
    def test_through_the_reader_it_stays_irregular_and_flagged(self):
        from cecelia.tests.test_ims_meta import _ims
        d = tempfile.mkdtemp()
        try:
            p = os.path.join(d, 'a.ims')
            births = [0.0]
            for g in GAPS:
                births.append(births[-1] + g)
            _ims(p, births_s=births)
            r = ims_meta.time_increment(p)
            self.assertFalse(r['uniform'])
            self.assertEqual(r['irregularFrames'], [[0, 2]])
        finally:
            shutil.rmtree(d, ignore_errors=True)


if __name__ == '__main__':
    unittest.main()
