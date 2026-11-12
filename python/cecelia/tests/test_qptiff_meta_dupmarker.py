"""RESULT-REVIEW: result-bfea0747

A marker imaged in two cycles: each page gets the `.xpd` item of its own cycle (the marker's n-th page in
write-time order ↔ its n-th well in run order), not the first item of that name. The real Fusion 2.4.0
run (test-data/acquisition/qptiff_fusion-2.4.0_cyclic_*) with CD45 in cycle 3 renamed CD68 on its page
and in its well, under another clone and the page's own 150 ms.
"""
import json
import os
import tempfile
import unittest

from cecelia.tests.test_qptiff_meta import CYCLIC, XPD, _pages, _QptiffCase
from cecelia.utils import qptiff_meta


class TestMarkerInTwoCycles(_QptiffCase):
    def test_each_page_gets_its_own_cycles_item(self):
        pages = [dict(p) for p in _pages(CYCLIC)]
        k = next(i for i, p in enumerate(pages) if '<Biomarker>CD45</Biomarker>' in p['description'])
        pages[k]['description'] = pages[k]['description'].replace('<Biomarker>CD45</Biomarker>', '<Biomarker>CD68</Biomarker>')
        with open(XPD, encoding='utf-8') as f:
            xpd = json.load(f)
        item = next(it for w in xpd['wells'] for it in w['items'] if it['markerName'] == 'CD45')
        item.update({'markerName': 'CD68', 'clone': 'CLONE-B', 'barcode': 'BX999'})
        fd, path = tempfile.mkstemp(suffix='.xpd')
        os.close(fd)
        self.addCleanup(os.remove, path)
        with open(path, 'w', encoding='utf-8') as f:
            json.dump(xpd, f)
        s = qptiff_meta.acquisition_settings(self.qptiff(pages, xpd=path))
        cd68 = [(c['cycle'], c['clone'], c['barcode'], c['exposureMs'], c['plannedExposureMs'])
                for c in s['channels'] if c['name'] == 'CD68']
        self.assertEqual(cd68, [(2, 'KP1', 'BX015', 100.0, 100), (3, 'CLONE-B', 'BX999', 150.0, 150)])


if __name__ == '__main__':
    unittest.main()
