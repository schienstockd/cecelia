"""The cell-clustering RUNNER — `app/src/tasks/clustPops/cluster_run.py` — pools each segment's OWN
columns for the first segment's channels.

Intensity columns are named by channel index; Julia (`cluster.jl` → `_cluster_segments`) matches the
channels by name and hands a segment whose channel order differs its own `featureCols`. The runner must
read those and pool them under the first segment's names, so one feature is one marker. The scanpy
engine and the write-back are stubbed: this pins the pooled matrix, not the clustering.

Skipped when `app/` is absent — an external `pip install cecelia` consumer gets the IO library only.
"""
import importlib.util
import os
import tempfile
import unittest
from unittest import mock
from pathlib import Path

import numpy as np

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'clustPops' / 'cluster_run.py')


def _load_runner():
    spec = importlib.util.spec_from_file_location('cluster_pops_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def _props(path, cd4, cd8):
    """A cell table whose channel columns hold the given marker values, in the given index order."""
    import anndata as ad
    import pandas as pd
    n = len(cd4)
    a = ad.AnnData(X=np.column_stack([cd4, cd8, np.arange(n, dtype=float)]).astype(np.float32),
                   obs=pd.DataFrame(index=[str(i + 1) for i in range(n)]))
    a.var_names = ['mean_intensity_0', 'mean_intensity_1', 'area']
    a.write_h5ad(path)


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class ClusterPopsRunnerChannelNameTest(unittest.TestCase):
    def test_a_segment_with_another_channel_order_pools_by_name(self):
        mod = _load_runner()
        seen = {}
        cu = mod.clustering_utils                  # the shared module — patch only for this run
        with mock.patch.object(cu, 'find_populations', lambda adata, **_: seen.setdefault('adata', adata)), \
             mock.patch.object(cu, 'split_back_and_write', lambda *a, **k: {'nClusters': 0, 'perSegment': []}), \
             tempfile.TemporaryDirectory() as td:
            ref, swapped = os.path.join(td, 'X.h5ad'), os.path.join(td, 'Y.h5ad')
            _props(ref, cd4=[1.0, 2.0], cd8=[10.0, 20.0])                 # CD4 at 0, CD8 at 1
            # Y holds CD8 at index 0 and CD4 at index 1
            import anndata as ad
            import pandas as pd
            y = ad.AnnData(X=np.array([[30.0, 3.0, 0.0], [40.0, 4.0, 1.0]], dtype=np.float32),
                           obs=pd.DataFrame(index=['1', '2']))
            y.var_names = ['mean_intensity_0', 'mean_intensity_1', 'area']
            y.write_h5ad(swapped)
            mod.run({
                'suffix': 't', 'featureCols': ['mean_intensity_0', 'area'],
                'segments': [
                    {'uID': 'X', 'valueName': 'B', 'propsPath': ref, 'labels': None},
                    {'uID': 'Y', 'valueName': 'B', 'propsPath': swapped, 'labels': None,
                     'featureCols': ['mean_intensity_1', 'area']}],
                'createUmap': False})
            a = seen['adata']
            self.assertEqual(list(a.var_names), ['mean_intensity_0', 'area'])
            cd4 = a[:, 'mean_intensity_0'].X.ravel()
            # every pooled CD4 value is a CD4 value: X's column 0, then Y's column 1 (not Y's CD8 30/40)
            np.testing.assert_array_equal(cd4, [1.0, 2.0, 3.0, 4.0])
            np.testing.assert_array_equal(a.obs['uID'].to_numpy(), ['X', 'X', 'Y', 'Y'])


if __name__ == '__main__':
    unittest.main()
