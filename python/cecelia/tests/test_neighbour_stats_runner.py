"""End-to-end test of the neighbour-stats RUNNER — `app/src/tasks/spatialAnalysis/cell_neighbour_stats_run.py`.

The runner is executed by path (`run_py`), never imported, so it is loaded the same way here. Pins the
QC contract Julia reads back (`neighbourStats.jl` → `_neighbour_stats_graph_findings`): `nEdges` is the
basis-pair contact count while `graphEdges` / `meanDegree` / `isolatedFrac` describe the graph as loaded.

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
           / 'app' / 'src' / 'tasks' / 'spatialAnalysis' / 'cell_neighbour_stats_run.py')


def _load_runner():
    spec = importlib.util.spec_from_file_location('cell_neighbour_stats_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class NeighbourStatsRunnerQcTest(unittest.TestCase):
    def test_qc_keeps_graph_edges_apart_from_pair_contacts(self):
        import anndata as ad
        import pandas as pd
        import scipy.sparse as sp
        from cecelia.utils.spatial_utils import save_graph

        n = 8
        labels = list(range(1, n + 1))
        with tempfile.TemporaryDirectory() as td:
            # a CHAIN graph (7 edges, mean degree 1.75); only the first 4 cells are in the basis, so the
            # basis-pair contacts (3) differ from the graph's edge count
            rows = list(range(n - 1)) + list(range(1, n))
            cols = list(range(1, n)) + list(range(n - 1))
            g = ad.AnnData(np.zeros((n, 2), dtype=np.float32))
            g.obsp['spatial_connectivities'] = sp.csr_matrix((np.ones(len(rows)), (rows, cols)), shape=(n, n))
            graph_path = os.path.join(td, 'spatialGraph', 'default.h5ad')
            save_graph(g, pd.DataFrame({'valueName': ['B'] * n, 'label': labels}), graph_path)

            qc_path = os.path.join(td, 'qc.json')
            _load_runner().run({
                'graphPath': graph_path, 'graphSuffix': 'default', 'basis': ['A', 'C'],
                'segments': [{'valueName': 'B', 'labels': labels[:4], 'popCodes': [0, 1, 0, 1]}],
                'statsPath': os.path.join(td, 'stats.json'), 'nPermutations': 0,
                'randomState': 0, 'qcOutPath': qc_path})

            with open(qc_path, encoding='utf-8') as f:
                qc = json.load(f)
            self.assertEqual(qc['graphEdges'], n - 1)
            self.assertEqual(qc['nEdges'], 3)
            self.assertAlmostEqual(qc['meanDegree'], 2 * (n - 1) / n)
            self.assertEqual(qc['isolatedFrac'], 0.0)


if __name__ == '__main__':
    unittest.main()
