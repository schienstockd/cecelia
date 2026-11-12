"""RESULT-REVIEW: result-6a0bf0c8

With the run's `.xpd`, a page whose marker the `.xpd` does not name (spelled differently there) takes the
cycle of the pages written with it — never the write-time rank, which would put one image on two cycle
scales. The real Fusion 2.4.0 run (test-data/acquisition/qptiff_fusion-2.4.0_cyclic_*), a blank well
inserted between C1 and C2, HLA-DR spelled "HLADR" in the `.xpd`.
"""
import json
import os
import tempfile
import unittest

from cecelia.tests.test_qptiff_meta import CYCLIC, XPD, _pages, _QptiffCase
from cecelia.utils import qptiff_meta


class TestUnnamedMarkerKeepsTheScale(_QptiffCase):
    def test_unnamed_page_takes_its_write_times_cycle(self):
        with open(XPD, encoding='utf-8') as f:
            xpd = json.load(f)
        blank = json.loads(json.dumps(xpd['wells'][0]))
        blank['wellName'] = 'H7'
        xpd['wells'].insert(2, blank)
        next(it for w in xpd['wells'] for it in w['items'] if it['markerName'] == 'HLA-DR')['markerName'] = 'HLADR'
        fd, path = tempfile.mkstemp(suffix='.xpd')
        os.close(fd)
        self.addCleanup(os.remove, path)
        with open(path, 'w', encoding='utf-8') as f:
            json.dump(xpd, f)
        s = qptiff_meta.acquisition_settings(self.qptiff(_pages(CYCLIC), xpd=path))
        got = {c['name']: (c['cycle'], c['clone']) for c in s['channels']}
        self.assertEqual((got['CD45'], got['HLA-DR'], got['HIF1A']), ((4, 'D9M81'), (4, None), (4, 'EP1215Y')))


if __name__ == '__main__':
    unittest.main()
