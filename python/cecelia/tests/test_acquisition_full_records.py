"""Every field each acquisition reader returns, for every real fixture in test-data/acquisition/.

Each fixture's whole record is pinned in test-data/acquisition/expected/<case>.json; how those values
were checked against the source documents is in test-data/README.md → acquisition/expected/. Why:
docs/todo/TEST_VALUE_PLAN.md → P2.
"""
import json
import os
import tempfile
import unittest

from cecelia.utils import czi_meta, lif_meta, oir_meta, qptiff_meta
from cecelia.tests.test_lif_meta import write_lif
from cecelia.tests.test_qptiff_meta import _QptiffCase, _pages

_FIX = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'test-data', 'acquisition'))
_EXPECTED = os.path.join(_FIX, 'expected')


def _expected(case):
    with open(os.path.join(_EXPECTED, case + '.json'), encoding='utf-8') as f:
        return json.load(f)


def _plain(record):
    """The record as JSON would carry it (tuples → lists), which is how it leaves the reader."""
    return json.loads(json.dumps(record))


def _read(name):
    with open(os.path.join(_FIX, name), 'rb') as f:
        return f.read()


class _Records(unittest.TestCase):
    def check(self, case, record):
        with self.subTest(case=case):
            self.assertEqual(_plain(record), _expected(case))


class TestCziRecords(_Records):
    def test_every_czi_fixture(self):
        for name in ['czi_axioscan_z1_widefield', 'czi_lsm780_multitrack', 'czi_lsm780_nlo',
                     'czi_lsm800_zenblue', 'czi_lsm880_spectral', 'czi_lsm980_airyscan']:
            self.check(name, czi_meta.parse_settings(_read(name + '_metadata.xml')))


class TestOirRecords(_Records):
    def test_every_oir_fixture(self):
        for name in ['oir_fv4000_3.2.1', 'oir_fvmpe-rs_2.3.2']:
            self.check(name, oir_meta.parse_settings(_read(name + '_imageProperties.xml')))


class TestLifRecords(_Records):
    def test_every_image_of_every_lif_fixture(self):
        with tempfile.TemporaryDirectory() as d:
            for name, n_images in [('lif_dm5500b_las-x-3.4.2', 3), ('lif_sp8_las-x-3.5.7', 2)]:
                path = write_lif(_read(name + '_header.xml').decode('utf-8'), os.path.join(d, name + '.lif'))
                for series in range(n_images):
                    self.check(f'{name}_s{series}', lif_meta.acquisition_settings(path, series=series))


class TestQptiffRecords(_QptiffCase, _Records):
    def test_every_qptiff_fixture(self):
        for name, xpd in [('qptiff_fusion-2.4.1_pages', None),
                          ('qptiff_fusion-2.4.0_cyclic_pages', os.path.join(_FIX, 'qptiff_fusion-2.4.0_cyclic.xpd')),
                          ('qptiff_fusion-2.3.0_brightfield_pages', None)]:
            pages = _pages(os.path.join(_FIX, name + '.json'))
            self.check(name, qptiff_meta.acquisition_settings(self.qptiff(pages, xpd=xpd), series=0))


if __name__ == '__main__':
    unittest.main()
