"""End-to-end test of the region-clustering RUNNER — `app/src/tasks/clustRegions/cluster_run.py`.

The runner is executed by path (`run_py`), never imported, so it is loaded the same way here. Pins
the QC contract Julia reads back (`clustRegions/cluster.jl` → `_region_graph_findings`): per image,
the metrics of the neighbour graph the run actually loaded, so a sparse graph is flagged on the run.

Skipped when `app/` is absent — an external `pip install cecelia` consumer gets the IO library only.
"""
import importlib.util
import json
import os
import tempfile
import unittest
from pathlib import Path

import numpy as np

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'clustRegions' / 'cluster_run.py')


def _load_runner():
    spec = importlib.util.spec_from_file_location('cluster_regions_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class ClusterRegionsRunnerQcTest(unittest.TestCase):
    def test_region_qc_banks_per_image_graph_metrics(self):
        n = 8
        with tempfile.TemporaryDirectory() as td:
            # a CHAIN graph: interior cells have 2 neighbours, ends 1 → mean degree 1.75 (sparse)
            qc_path = self._run_chain(td)
            with open(qc_path, encoding='utf-8') as f:
                qc = json.load(f)
            m = qc['graphMetrics']['img1']
            self.assertEqual(m['nCells'], n)
            self.assertEqual(m['nEdges'], n - 1)
            self.assertAlmostEqual(m['meanDegree'], 2 * (n - 1) / n)
            self.assertEqual(m['isolatedFrac'], 0.0)
            # k-means is sklearn on the CPU, no fallback to report: the env is the seed alone, and
            # Julia adds that (`_cluster_run_env`) — so no `device` here
            self.assertEqual(qc['runEnv'], {})

    def test_leiden_reports_the_device_that_ran(self):
        """`find_populations(backend="auto")` can fall back GPU → CPU silently; the runner hands the
        path that actually ran back in the QC sidecar → the run log's `env`."""
        from unittest import mock
        import pandas as pd
        mod = _load_runner()
        cu = mod.clustering_utils

        def fake_find(adata, run_env=None, **_kw):
            adata.obs['clusters'] = pd.Categorical(['0'] * adata.n_obs)
            run_env['device'] = 'cpu'

        with mock.patch.object(cu, 'find_populations', side_effect=fake_find), \
             mock.patch.object(cu, 'split_back_and_write',
                               lambda *a, **k: {'nClusters': 1, 'perSegment': []}), \
             tempfile.TemporaryDirectory() as td:
            qc_path = self._run_chain(td, clusterMethod='leiden')
            with open(qc_path, encoding='utf-8') as f:
                qc = json.load(f)
        self.assertEqual(qc['runEnv'], {'device': 'cpu'})

    def _run_chain(self, td, **over):
        """An 8-cell CHAIN neighbour graph over one segment, clustered once; returns the QC path."""
        import anndata as ad
        import pandas as pd
        import scipy.sparse as sp
        from cecelia.utils.spatial_utils import save_graph
        n = 8
        labels = list(range(1, n + 1))
        rows = [i for i in range(n - 1)] + [i + 1 for i in range(n - 1)]
        cols = [i + 1 for i in range(n - 1)] + [i for i in range(n - 1)]
        g = ad.AnnData(np.zeros((n, 2), dtype=np.float32))
        g.obsp['spatial_connectivities'] = sp.csr_matrix((np.ones(len(rows)), (rows, cols)), shape=(n, n))
        graph_path = os.path.join(td, 'spatialGraph', 'default.h5ad')
        save_graph(g, pd.DataFrame({'valueName': ['B'] * n, 'label': labels}), graph_path)
        props_path = os.path.join(td, 'B.h5ad')
        props = ad.AnnData(X=np.zeros((n, 1)), obs=pd.DataFrame(index=[str(l) for l in labels]))
        props.var_names = ['f0']
        props.write_h5ad(props_path)
        qc_path = os.path.join(td, 'region_qc.json')
        params = {
            'suffix': 'reg', 'basis': ['A', 'C'], 'graphPaths': {'img1': graph_path},
            'graphSuffix': 'default',
            'segments': [{'uID': 'img1', 'valueName': 'B', 'propsPath': props_path,
                          'labels': labels, 'popCodes': [0, 1] * (n // 2)}],
            'compCols': ['spatial.comp.A.reg', 'spatial.comp.C.reg'], 'includeOther': False,
            'clusterMethod': 'kmeans', 'numClusters': 2, 'createUmap': False,
            'randomState': 0, 'qcOutPath': qc_path}
        params.update(over)
        _load_runner().run(params)
        return qc_path


if __name__ == '__main__':
    unittest.main()
