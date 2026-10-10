"""Importing a `cecelia.utils` module must not import `dask.array`.

`import dask.array` costs ~0.5 s warm — it drags in xarray → pandas → pyarrow, `scipy.fft` and
`scipy.sparse` — which was two thirds of the time to import `zarr_utils`, paid by every Python
process that touches a store, interactive one-shot spawns included. A function that genuinely needs
dask imports it locally; one that only has to recognise a dask array uses `zarr_utils.is_dask`, which
never imports it. See docs/todo/DASK_NARROW_PLAN.md (Decision 2, 3).

This checks what actually lands in `sys.modules` in a fresh interpreter rather than grepping for
`import dask`, so it also catches dask arriving through another cecelia module.

Run with `pixi run test-py`.
"""
import concurrent.futures as cf
import os
import subprocess
import sys
import unittest

_PY_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
_UTILS = os.path.join(_PY_ROOT, 'cecelia', 'utils')

# Modules whose dask comes from a THIRD-PARTY import we can't defer: anndata imports dask.array at
# load time (`anndata/compat`), and scanpy/squidpy import anndata. Every entry must still load dask —
# a stale entry fails `test_allowlist_is_not_stale`, so the list can only shrink truthfully.
_THIRD_PARTY_DASK = {
    'label_props_utils',   # anndata
    'obs_utils',           # label_props_utils → anndata
    'tracking_utils',      # anndata
    'measure_utils',       # anndata
    'centroid_migrate',    # label_props_utils → anndata
    'clustering_utils',    # scanpy
}


def _modules():
    return sorted(f[:-3] for f in os.listdir(_UTILS)
                  if f.endswith('.py') and f != '__init__.py')


def _loads_dask(name):
    """(loads dask.array?, stderr) for `import cecelia.utils.<name>` in a fresh interpreter."""
    code = (f"import sys, cecelia.utils.{name}; "
            "sys.stdout.write('1' if 'dask.array' in sys.modules else '0')")
    env = dict(os.environ, PYTHONPATH=_PY_ROOT + os.pathsep + os.environ.get('PYTHONPATH', ''))
    r = subprocess.run([sys.executable, '-c', code], capture_output=True, text=True,
                       encoding='utf-8', env=env, cwd=_PY_ROOT, timeout=300)
    if r.returncode != 0:
        return None, r.stderr.strip().splitlines()[-1:] or ['(no stderr)']
    return r.stdout.strip() == '1', None


class TestDaskImportIsolation(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        with cf.ThreadPoolExecutor(max_workers=min(8, os.cpu_count() or 1)) as ex:
            cls.results = dict(zip(_modules(), ex.map(_loads_dask, _modules())))

    def test_no_utils_module_imports_dask(self):
        offenders = sorted(m for m, (loads, _) in self.results.items()
                           if loads and m not in _THIRD_PARTY_DASK)
        self.assertEqual(offenders, [],
                         'these cecelia.utils modules import dask.array at load time — import it '
                         'inside the function that needs it, and use zarr_utils.is_dask to recognise '
                         'a dask array (docs/todo/DASK_NARROW_PLAN.md Decision 2)')

    def test_allowlist_is_not_stale(self):
        stale = sorted(m for m in _THIRD_PARTY_DASK if self.results.get(m, (None,))[0] is False)
        self.assertEqual(stale, [], 'no longer load dask — drop them from _THIRD_PARTY_DASK')

    def test_every_module_imports(self):
        # A module that fails to import says nothing about dask; don't let it pass silently.
        failed = {m: err for m, (loads, err) in self.results.items() if loads is None}
        self.assertEqual(failed, {})


if __name__ == '__main__':
    unittest.main()
