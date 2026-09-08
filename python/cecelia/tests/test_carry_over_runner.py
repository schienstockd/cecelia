"""End-to-end test of the obs carry-over runner —
`app/src/tasks/segment/carry_over_run.py`.

**Why this file exists.** The runner is executed by path via `run_py`, never imported, so nothing
in the package suite touches it. Its job is to snapshot every non-measurement obs column of a
labelProps h5ad before `segment.measureLabels` re-writes the file (a total obs replace, dropping
`track_id`, `live.*`, cluster ids, HMM states, gating pops), then restore those columns after —
skipping any column measureLabels has re-produced so its fresh morphology wins.

Two scenarios cover the surface:
  1. snapshot → simulate measureLabels rewrite → restore, and assert non-measurement obs is back on
     surviving labels while measureLabels' own columns are untouched.
  2. label.remove semantics: rows dropped by the rewrite have no target on restore and the runner
     doesn't error — LabelPropsView.add_obs silently skips labels absent from the fresh file
     (label_props_utils.py:288).

Skipped when `app/` is absent (external `pip install cecelia` consumers).
"""

import importlib.util
import json
import os
import shutil
import tempfile
import unittest
from pathlib import Path

import anndata as ad
import numpy as np
import pandas as pd

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'segment' / 'carry_over_run.py')


def _load_runner():
    spec = importlib.util.spec_from_file_location('carry_over_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _make_labelprops(path, labels, obs_cols=None, cat_cols=None, measure_cols=None):
    """A minimal labelProps h5ad with a few numeric obs, a categorical obs, and measurement-like
    columns in X (var_names). Mirrors the shape measure_labels_run.py writes."""
    n = len(labels)
    var = measure_cols or ['area', 'mean_intensity_0']
    X = np.zeros((n, len(var)), dtype=np.float64)
    obs = pd.DataFrame(index=[str(l) for l in labels])
    for c, vals in (obs_cols or {}).items():
        obs[c] = np.asarray(vals, dtype=float)
    for c, vals in (cat_cols or {}).items():
        obs[c] = pd.Categorical(vals)
    adata = ad.AnnData(X=X, obs=obs)
    adata.var_names = list(var)
    adata.uns['intensity_measure'] = 'mean'
    os.makedirs(os.path.dirname(path), exist_ok=True)
    adata.write_h5ad(path)


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class CarryOverRunnerTest(unittest.TestCase):
    VN = 'memTom'

    def setUp(self):
        self.task_dir = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.task_dir, ignore_errors=True)
        self.lp_path = os.path.join(self.task_dir, 'labelProps', f'{self.VN}.h5ad')
        self.snap = os.path.join(self.task_dir, 'snap.json')
        self.result = os.path.join(self.task_dir, 'result.json')
        self.runner = _load_runner()

    def _snapshot(self):
        self.runner.run({
            'phase':        'snapshot',
            'taskDir':      self.task_dir,
            'valueName':    self.VN,
            'snapshotFile': self.snap,
            'resultFile':   self.result,
        })

    def _restore(self):
        self.runner.run({
            'phase':        'restore',
            'taskDir':      self.task_dir,
            'valueName':    self.VN,
            'snapshotFile': self.snap,
            'resultFile':   self.result,
        })

    def test_snapshot_then_restore_preserves_non_measurement_obs(self):
        # Pre-correction: three cells, each with a track_id and live.cell.speed (numeric) plus a
        # pop.hmm categorical. Two measurement columns in X.
        _make_labelprops(
            self.lp_path,
            labels=[10, 20, 30],
            obs_cols={'track_id': [42.0, 43.0, 44.0],
                      'live.cell.speed': [0.1, 0.2, 0.3]},
            cat_cols={'pop.hmm': ['s1', 's2', 's1']},
        )
        self._snapshot()

        with open(self.snap, 'r', encoding='utf-8') as f:
            snap = json.load(f)
        self.assertEqual(snap['labels'], [10, 20, 30])
        self.assertIn('track_id', snap['numeric'])
        self.assertIn('live.cell.speed', snap['numeric'])
        self.assertIn('pop.hmm', snap['categorical'])

        # Simulate segment.measureLabels total-replace: rewrite the h5ad with only measurement obs
        # + fresh X. Labels survive, non-measurement obs is gone.
        _make_labelprops(
            self.lp_path,
            labels=[10, 20, 30],
            obs_cols={},
            cat_cols={},
            measure_cols=['area', 'mean_intensity_0'],
        )
        # Add a measurement-only obs column that measureLabels would write — restore must NOT touch it.
        adata = ad.read_h5ad(self.lp_path)
        adata.obs['area'] = np.array([100.0, 200.0, 300.0])
        adata.write_h5ad(self.lp_path)

        self._restore()
        back = ad.read_h5ad(self.lp_path)
        self.assertIn('track_id', back.obs.columns)
        self.assertIn('live.cell.speed', back.obs.columns)
        self.assertIn('pop.hmm', back.obs.columns)
        # values aligned by label
        self.assertEqual(back.obs.loc['10', 'track_id'], 42.0)
        self.assertEqual(back.obs.loc['20', 'live.cell.speed'], 0.2)
        self.assertEqual(back.obs.loc['30', 'pop.hmm'], 's1')
        # measurement obs untouched (restore skips cols present in fresh)
        self.assertEqual(back.obs.loc['10', 'area'], 100.0)

    def test_removed_label_is_silently_skipped_on_restore(self):
        # Snapshot with 3 cells, then simulate a label.remove that drops id 20 from the h5ad.
        # Restore must not raise; the two surviving rows must carry their obs back.
        _make_labelprops(
            self.lp_path,
            labels=[10, 20, 30],
            obs_cols={'track_id': [42.0, 43.0, 44.0]},
        )
        self._snapshot()

        _make_labelprops(self.lp_path, labels=[10, 30])  # id 20 gone
        self._restore()
        back = ad.read_h5ad(self.lp_path)
        self.assertEqual(list(back.obs.index), ['10', '30'])
        self.assertEqual(back.obs.loc['10', 'track_id'], 42.0)
        self.assertEqual(back.obs.loc['30', 'track_id'], 44.0)

    def test_merged_id_inherits_its_own_pre_op_obs(self):
        # A label.merge (ids [3, 5] into 3) leaves id 3 alive with the merged pixels; id 5 is gone.
        # After measureLabels re-measures, id 3's row exists with fresh morphology. Restore must put
        # id 3's PRE-op obs (track_id, cluster) back onto id 3's row. Id 5 is silently skipped.
        _make_labelprops(
            self.lp_path,
            labels=[3, 5, 7],
            obs_cols={'track_id': [100.0, 101.0, 102.0]},
            cat_cols={'clust': ['A', 'B', 'C']},
        )
        self._snapshot()

        # Post-merge: 5 is gone; 3 and 7 remain with a fresh area.
        _make_labelprops(self.lp_path, labels=[3, 7])
        adata = ad.read_h5ad(self.lp_path)
        adata.obs['area'] = np.array([250.0, 90.0])
        adata.write_h5ad(self.lp_path)

        self._restore()
        back = ad.read_h5ad(self.lp_path)
        self.assertEqual(list(back.obs.index), ['3', '7'])
        # id 3 keeps ITS own pre-op obs — not id 5's. Codebase precedent: no invented averaging.
        self.assertEqual(back.obs.loc['3', 'track_id'], 100.0)
        self.assertEqual(back.obs.loc['3', 'clust'], 'A')
        self.assertEqual(back.obs.loc['7', 'track_id'], 102.0)

    def test_snapshot_handles_nullable_int64_and_boolean_and_string_dtypes(self):
        """F6 regression: nullable extension dtypes must not crash the snapshotter.

        Before the fix, `_is_categorical` returned False for `pd.Int64Dtype` / `pd.BooleanDtype`
        / `pd.StringDtype`; `_serialise_numeric` then called `np.asarray(series, dtype=float)`
        which raises `TypeError` on `pd.NA`. Verify snapshot completes and each dtype ends up on
        the sensible side (nullable numerics → numeric with NaN, string extension → categorical).
        """
        # Build the h5ad by hand so we can attach extension dtypes anndata's shortcuts don't set.
        labels = [1, 2, 3]
        obs = pd.DataFrame(index=[str(l) for l in labels])
        obs['nullable_int']  = pd.array([10, pd.NA, 30], dtype='Int64')
        obs['nullable_bool'] = pd.array([True, False, pd.NA], dtype='boolean')
        obs['string_ext']    = pd.array(['a', pd.NA, 'c'], dtype='string')
        adata = ad.AnnData(X=np.zeros((3, 1), dtype=np.float64), obs=obs)
        adata.var_names = ['area']
        os.makedirs(os.path.dirname(self.lp_path), exist_ok=True)
        adata.write_h5ad(self.lp_path)

        # Snapshot must not raise.
        self._snapshot()
        with open(self.snap, 'r', encoding='utf-8') as f:
            snap = json.load(f)

        # Nullable numerics ride the numeric channel with NA → None (round-trips as NaN via add_obs).
        self.assertIn('nullable_int',  snap['numeric'])
        self.assertIn('nullable_bool', snap['numeric'])
        self.assertEqual(snap['numeric']['nullable_int'][1], None)
        self.assertEqual(snap['numeric']['nullable_bool'][2], None)
        self.assertEqual(snap['numeric']['nullable_int'][0], 10.0)
        # True → 1.0, False → 0.0 (matches the existing "int obs become float64" convention).
        self.assertEqual(snap['numeric']['nullable_bool'][0], 1.0)
        self.assertEqual(snap['numeric']['nullable_bool'][1], 0.0)

        # String extension dtype snapshots as categorical — restore goes via `add_categorical_obs`.
        self.assertIn('string_ext', snap['categorical'])
        self.assertEqual(snap['categorical']['string_ext'], ['a', None, 'c'])

    def test_restore_is_noop_without_snapshot(self):
        _make_labelprops(self.lp_path, labels=[1, 2])
        # No snapshot file exists — restore must exit cleanly with a zero result.
        self.assertFalse(os.path.exists(self.snap))
        self._restore()
        self.assertTrue(os.path.exists(self.result))
        with open(self.result, 'r', encoding='utf-8') as f:
            r = json.load(f)
        self.assertEqual(r['nColsCarried'], 0)


if __name__ == '__main__':
    unittest.main()
