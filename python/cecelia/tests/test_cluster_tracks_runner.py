"""The track-clustering RUNNER — `app/src/tasks/clustTracks/cluster_run.py` — reports the device
that actually clustered.

`find_populations(backend="auto")` falls back from the GPU to the CPU silently (clustering_utils.py),
so the run log's `env` (app/src/run_log.jl) has to hear from the runner which path ran. The scanpy
engine, the behaviour axes and the write-back are stubbed: this pins the QC sidecar, not clustering.

Skipped when `app/` is absent — an external `pip install cecelia` consumer gets the IO library only.
"""
import importlib.util
import json
import os
import tempfile
import unittest
from pathlib import Path
from unittest import mock

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'clustTracks' / 'cluster_run.py')


def _load_runner():
    spec = importlib.util.spec_from_file_location('cluster_tracks_run', _RUNNER)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class ClusterTracksRunnerRunEnvTest(unittest.TestCase):
    def test_qc_sidecar_names_the_device_that_ran(self):
        mod = _load_runner()
        cu = mod.clustering_utils

        def fake_find(adata, run_env=None, **_kw):
            run_env['device'] = 'cpu'        # what find_populations sets after a GPU fallback

        with mock.patch.object(cu, 'find_populations', side_effect=fake_find), \
             mock.patch.object(cu, 'behaviour_axes', return_value=None), \
             mock.patch.object(cu, 'split_back_and_write',
                               lambda *a, **k: {'nClusters': 1, 'perSegment': []}), \
             tempfile.TemporaryDirectory() as td:
            qc_path = os.path.join(td, 'cluster_qc.json')
            mod.run({'suffix': 't', 'featureCols': ['speed'],
                     'segments': [{'uID': 'X', 'valueName': 'T', 'propsPath': 'unused'}],
                     'X': [[1.0], [2.0]], 'uIDs': ['X', 'X'], 'valueNames': ['T', 'T'],
                     'labels': [1, 2], 'createUmap': False, 'qcOutPath': qc_path})
            with open(qc_path, encoding='utf-8') as f:
                qc = json.load(f)
        self.assertEqual(qc['runEnv'], {'device': 'cpu'})


if __name__ == '__main__':
    unittest.main()
