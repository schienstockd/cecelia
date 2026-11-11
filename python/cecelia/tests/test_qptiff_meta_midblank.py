"""RESULT-REVIEW: result-b3eacf9c

A blank cycle between two marker cycles writes no page, so the rank of the pages' write times undercounts
the cycles after it. With the run's `.xpd`, a marker's cycle is its well's run position, blanks counted.
The real Fusion 2.4.0 run's `.xpd` (test-data/acquisition/qptiff_fusion-2.4.0_cyclic.xpd) with a blank
well inserted between C1 and C2.
"""
import json
import os
import tempfile
import unittest

from cecelia.tests.test_qptiff_meta import CYCLIC, XPD, _pages, _QptiffCase
from cecelia.utils import qptiff_meta


class TestBlankBetweenCycles(_QptiffCase):
    def test_cycle_counts_the_blank_wells_that_ran(self):
        with open(XPD, encoding='utf-8') as f:
            xpd = json.load(f)
        blank = json.loads(json.dumps(xpd['wells'][0]))   # the leading blank well, copied
        blank['wellName'] = 'H7'
        xpd['wells'].insert(2, blank)                       # H5 blank, C1, H7 blank, C2, H6 blank
        fd, path = tempfile.mkstemp(suffix='.xpd')
        os.close(fd)
        self.addCleanup(os.remove, path)
        with open(path, 'w', encoding='utf-8') as f:
            json.dump(xpd, f)
        s = qptiff_meta.acquisition_settings(self.qptiff(_pages(CYCLIC), xpd=path))
        cycles = {c['name']: c['cycle'] for c in s['channels']}
        self.assertEqual((cycles['DAPI'], cycles['Podoplanin'], cycles['CD45'], cycles['HIF1A']), (1, 2, 4, 4))
        self.assertEqual((s['cycles']['planned'], s['cycles']['blank']), (5, 3))


if __name__ == '__main__':
    unittest.main()
