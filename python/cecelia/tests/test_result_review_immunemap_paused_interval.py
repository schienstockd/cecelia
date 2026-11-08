"""RESULT-REVIEW: result-ee2a25f0

A paused acquisition — frames 30 s apart, a 10-min gap, then 30 s again — has no single frame
interval, and the file readers rightly refuse one. The Immunemap import declared the MEDIAN of its
time steps (30 s) anyway, and `omezarr` applied it, so every speed across the pause came out ~20×
too fast with only an info line. Now irregular time steps declare nothing (and flag it, so the
nominal frame rate isn't used instead), and the import never applies a declared interval over a
file whose own timestamps it refused (`omezarr/task.jl`).
"""
import unittest
from datetime import datetime, timedelta, timezone

import cecelia.utils.immunemap as im


def _stamps(gaps):
    t = datetime(2013, 1, 1, tzinfo=timezone.utc)
    out = [t]
    for g in gaps:
        t += timedelta(seconds=g)
        out.append(t)
    return [s.strftime('%Y-%m-%dT%H:%M:%S%z') for s in out]


PAUSED = _stamps([30] * 9 + [600] + [30] * 9)


class PausedIntervalTest(unittest.TestCase):
    def test_paused_series_yields_no_interval(self):
        self.assertEqual(im.time_step_interval(PAUSED), (None, True))
        self.assertIsNone(im.interval_from_time_steps(PAUSED))

    def test_uniform_series_still_does(self):
        self.assertEqual(im.time_step_interval(_stamps([30] * 19)), (30.0, False))

    def test_video_record_flags_it(self):
        acq = {'id': 1, 'name': 'v.ims', 'video_details': {
            'uid': 'U', 'size': {'fps': 1 / 30, 'number_of_channels': 1}, 'time_steps': PAUSED}}
        r = im.video_record(acq, {'id': 2, 'name': 'g'})
        self.assertIsNone(r['intervalFromTimeStepsSec'])
        self.assertTrue(r['timeStepsIrregular'])


if __name__ == '__main__':
    unittest.main()
