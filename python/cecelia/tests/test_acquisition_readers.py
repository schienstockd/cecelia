"""The acquisition reader registry (`app/src/tasks/importImages/acquisition_readers.json`) is the one list of
settings readers: every row names a module that exists, takes the reader contract
`acquisition_settings(path, series=0)`, and stamps the `source` and version the row says. The Julia side
reads the same file (`ACQUISITION_READERS`, app/src/tasks/importImages/omezarr/acquisition.jl).
"""
import importlib
import inspect
import json
import os
import unittest

REGISTRY = os.path.normpath(os.path.join(os.path.dirname(__file__), '..', '..', '..', 'app', 'src', 'tasks',
                                         'importImages', 'acquisition_readers.json'))


def _rows():
    with open(REGISTRY, encoding='utf-8') as f:
        return json.load(f)['readers']


class TestRegistry(unittest.TestCase):
    def test_rows_are_unique_and_lowercase(self):
        rows = _rows()
        exts = [r['ext'] for r in rows]
        self.assertEqual(len(exts), len(set(exts)))
        self.assertEqual(len({r['source'] for r in rows}), len(rows))
        for e in exts:
            self.assertTrue(e.startswith('.') and e == e.lower(), e)

    def test_every_module_meets_the_reader_contract(self):
        for r in _rows():
            with self.subTest(module=r['module']):
                mod = importlib.import_module(f'cecelia.utils.{r["module"]}')
                params = inspect.signature(mod.acquisition_settings).parameters
                self.assertIn('series', params)
                self.assertEqual(params['series'].default, 0)
                self.assertEqual(getattr(mod, 'READER_VERSION', 1), r['version'])

    def test_missing_file_is_a_reason_not_an_error(self):
        for r in _rows():
            with self.subTest(module=r['module']):
                mod = importlib.import_module(f'cecelia.utils.{r["module"]}')
                self.assertIn('reason', mod.acquisition_settings(f'/nonexistent/x{r["ext"]}', series=0))


if __name__ == '__main__':
    unittest.main()
