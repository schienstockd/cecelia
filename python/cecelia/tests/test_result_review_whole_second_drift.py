"""RESULT-REVIEW: result-e55db656

Whole-second stamps get a rounding allowance (`ims_meta.summarise_gaps(resolution=…)`), but it must
only excuse the rounding. Centred on the mean gap with the full `tol` either side, it passed series the
exact rule refuses: a 30 s series that slows to 32 s, or a 33 s gap every fourth frame — rounding to the
second can move a gap by under 1 s, and can never make 33 s out of a ~30.75 s series. Those came back
as a measured interval, here and through the Immunemap import.
"""
import unittest
from datetime import datetime, timedelta, timezone

import cecelia.utils.immunemap as im
from cecelia.utils.ims_meta import summarise_gaps

SLOWS = [30.0] * 12 + [32.0] * 8
STALLS = [30.0, 30.0, 30.0, 33.0] * 5
SLOWS_60 = [60.0] * 14 + [64.0] * 6


def _stamps(gaps):
    t = datetime(2013, 1, 1, tzinfo=timezone.utc)
    out = [t]
    for g in gaps:
        t += timedelta(seconds=g)
        out.append(t)
    return [s.strftime('%Y-%m-%dT%H:%M:%S%z') for s in out]


class WholeSecondDriftTest(unittest.TestCase):
    def test_exact_stamps_refuse_them(self):
        for gaps in (SLOWS, STALLS, SLOWS_60):
            self.assertFalse(summarise_gaps(gaps)[1])

    def test_whole_second_stamps_refuse_them_too(self):
        for gaps in (SLOWS, STALLS, SLOWS_60):
            self.assertFalse(summarise_gaps(gaps, resolution=1.0)[1], gaps)

    def test_a_series_the_exact_rule_accepts_stays_accepted(self):
        # 3.2% either side of a 31 s median: uniform by the per-gap rule, rounded or not.
        gaps = [30.0] * 10 + [32.0] * 10
        self.assertTrue(summarise_gaps(gaps)[1])
        self.assertEqual(summarise_gaps(gaps, resolution=1.0)[:2], (31.0, True))
        self.assertEqual(im.time_step_interval(_stamps(gaps)), (31.0, False))

    def test_immunemap_reads_a_rounded_steady_series(self):
        # A steady 5.3 s series: its whole-second stamps step 5 or 6 s apart.
        interval, irregular = im.time_step_interval(_stamps([5, 5, 6, 5, 5, 6, 5, 5, 5, 6]))
        self.assertAlmostEqual(interval, 5.3)
        self.assertFalse(irregular)

    def test_immunemap_declares_nothing(self):
        self.assertEqual(im.time_step_interval(_stamps(STALLS)), (None, True))
        self.assertEqual(im.time_step_interval(_stamps(SLOWS)), (None, True))


if __name__ == '__main__':
    unittest.main()
