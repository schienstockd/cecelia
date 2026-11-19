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

    def test_pooling_per_track_gives_every_cell_its_tracks_region(self):
        """Two tracks over two frames, each in its own neighbourhood: track 1 among pop A, track 2
        among pop C. Pooled per track, all of a track's cells share one region, the two tracks
        differ, and the untracked cell gets none (NaN)."""
        from cecelia.utils.label_props_utils import LabelPropsView
        with tempfile.TemporaryDirectory() as td:
            props_path = self._run_tracks(td)
            d = LabelPropsView(props_path).view_cols(['regions.reg', 'track_id']).as_df()
        reg = dict(zip(d['label'], d['regions.reg']))
        self.assertEqual(reg[1], reg[2])                 # track 1, frames 0 and 1
        self.assertEqual(reg[5], reg[6])                 # track 2, frames 0 and 1
        self.assertNotEqual(reg[1], reg[5])
        self.assertTrue(np.isnan(reg[9]))                # not on a track → no region

    def test_pooling_per_track_refuses_a_graph_pooled_over_frames(self):
        with tempfile.TemporaryDirectory() as td:
            props_path = self._run_tracks(td, per_timepoint=False)
            from cecelia.utils.label_props_utils import LabelPropsView
            d = LabelPropsView(props_path).as_df()
        self.assertNotIn('regions.reg', d.columns)       # nothing written

    def _run_tracks(self, td, per_timepoint=True):
        """Cells 1,2 = track 1 (frames 0,1); 5,6 = track 2; 3,4 neighbours of track 1 (pop A),
        7,8 neighbours of track 2 (pop C); 9 untracked, beside track 1. Basis codes: tracked cells
        and their neighbours are in the basis; the graph links each cell to its frame's neighbours."""
        import anndata as ad
        import pandas as pd
        import scipy.sparse as sp
        from cecelia.utils.spatial_utils import save_graph
        labels = [1, 2, 3, 4, 5, 6, 7, 8, 9]
        t      = [0, 1, 0, 1, 0, 1, 0, 1, 0]
        edges = [(1, 3), (2, 4), (5, 7), (6, 8), (9, 3)]
        idx = {l: i for i, l in enumerate(labels)}
        rows = [idx[a] for a, b in edges] + [idx[b] for a, b in edges]
        cols = [idx[b] for a, b in edges] + [idx[a] for a, b in edges]
        n = len(labels)
        g = ad.AnnData(np.zeros((n, 2), dtype=np.float32))
        g.obsp['spatial_connectivities'] = sp.csr_matrix((np.ones(len(rows)), (rows, cols)), shape=(n, n))
        obs = pd.DataFrame({'valueName': ['T'] * n, 'label': labels})
        if per_timepoint:
            obs['_t'] = t
        graph_path = os.path.join(td, 'spatialGraph', 'default.h5ad')
        save_graph(g, obs, graph_path)
        props_path = os.path.join(td, 'T.h5ad')
        props = ad.AnnData(X=np.zeros((n, 1)), obs=pd.DataFrame(
            {'track_id': [1, 1, np.nan, np.nan, 2, 2, np.nan, np.nan, np.nan]},
            index=[str(l) for l in labels]))
        props.var_names = ['f0']
        props.write_h5ad(props_path)
        # pop A = code 0 (track 1 + its neighbours + the untracked cell), pop C = code 1 (track 2 side)
        codes = [0, 0, 0, 0, 1, 1, 1, 1, 0]
        _load_runner().run({
            'suffix': 'reg', 'basis': ['A', 'C'], 'graphPaths': {'img1': graph_path},
            'graphSuffix': 'default', 'pooling': 'track',
            'segments': [{'uID': 'img1', 'valueName': 'T', 'propsPath': props_path,
                          'labels': labels, 'popCodes': codes}],
            'compCols': ['spatial.comp.A.reg', 'spatial.comp.C.reg'], 'includeOther': False,
            'clusterMethod': 'kmeans', 'numClusters': 2, 'createUmap': False,
            'randomState': 0, 'qcOutPath': os.path.join(td, 'region_qc.json')})
        return props_path

    def test_window_graph_over_the_movie_gives_cells_their_places_region(self):
        """Left half of the field holds pop A, right half pop C, over two frames. Pooled over the
        movie on 20 µm windows, the two halves come out as two regions, and each cell carries its
        window's cell count."""
        from cecelia.utils.label_props_utils import LabelPropsView
        with tempfile.TemporaryDirectory() as td:
            props_path, labels, left = self._run_windows(td, 'movie')
            d = LabelPropsView(props_path).view_cols(['regions.reg', 'spatial.comp.cells_per_window.reg']).as_df()
        reg = dict(zip(d['label'], d['regions.reg']))
        left_regs = {reg[l] for l, is_left in zip(labels, left) if is_left}
        right_regs = {reg[l] for l, is_left in zip(labels, left) if not is_left}
        self.assertEqual(len(left_regs), 1)
        self.assertEqual(len(right_regs), 1)
        self.assertNotEqual(left_regs, right_regs)
        self.assertTrue((d['spatial.comp.cells_per_window.reg'] > 0).all())

    def test_window_graph_refuses_track_pooling(self):
        from cecelia.utils.label_props_utils import LabelPropsView
        with tempfile.TemporaryDirectory() as td:
            props_path, _, _ = self._run_windows(td, 'track')
            d = LabelPropsView(props_path).as_df()
        self.assertNotIn('regions.reg', d.columns)

    def _run_windows(self, td, pooling):
        import anndata as ad
        import pandas as pd
        import scipy.sparse as sp
        from cecelia.utils.spatial_utils import save_graph, window_grid
        rng = np.random.default_rng(0)
        n = 80
        xy = np.column_stack([rng.uniform(0, 100, n), rng.uniform(0, 100, n)])   # y, x µm
        left = xy[:, 1] < 50
        labels = list(range(1, n + 1))
        g = ad.AnnData(np.zeros((n, 2), dtype=np.float32))
        g.obsm['spatial'] = xy
        g.obsp['spatial_connectivities'] = sp.csr_matrix((n, n))
        g.uns.update(window_grid(xy, ['y', 'x'], 20.0))
        graph_path = os.path.join(td, 'spatialGraph', 'win.h5ad')
        save_graph(g, pd.DataFrame({'valueName': ['T'] * n, 'label': labels, '_t': [i % 2 for i in range(n)]}),
                   graph_path, meta={'method': 'windows'})
        props_path = os.path.join(td, 'T.h5ad')
        props = ad.AnnData(X=np.zeros((n, 1)), obs=pd.DataFrame(index=[str(l) for l in labels]))
        props.var_names = ['f0']
        props.write_h5ad(props_path)
        _load_runner().run({
            'suffix': 'reg', 'basis': ['A', 'C'], 'graphPaths': {'img1': graph_path},
            'graphSuffix': 'win', 'pooling': pooling,
            'segments': [{'uID': 'img1', 'valueName': 'T', 'propsPath': props_path,
                          'labels': labels, 'popCodes': [0 if l else 1 for l in left]}],
            'compCols': ['spatial.comp.A.reg', 'spatial.comp.C.reg'], 'includeOther': False,
            'densityCol': 'spatial.comp.cells_per_window.reg',
            'clusterMethod': 'kmeans', 'numClusters': 2, 'createUmap': False,
            'randomState': 0, 'qcOutPath': os.path.join(td, 'region_qc.json')})
        return props_path, labels, left

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
