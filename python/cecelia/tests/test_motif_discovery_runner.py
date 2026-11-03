"""End-to-end test of the motif-discovery RUNNER — `app/src/tasks/behaviour/motif_discovery_run.py`.

Guards the span-broadcast row mapping (docs/todo/HARDENING_RESULT_CORRECTNESS.md, motif row-indexing
row): the pooled frame Julia hands over (`pop_df_multi`) is not per-track contiguous, so an instance's
window must map through ITS track's ordered pooled row ids — walking `start_row + k` in pooled order
puts its labels on `window` UNRELATED tracks' cells at the same pool positions.

Known answer: three tracks interleaved round-robin in pooled order (row i belongs to track i % 3),
an 8-frame snippet planted four times in track 1 only, the other tracks noise. The planted copies
are the only matrix-profile minima, so with topK=4 every instance comes from track 1 — every
labelled cell must be a track-1 cell, and each instance must cover `window` consecutive frames of
ONE track. Pre-fix, each instance's labels spread over all three tracks.

The runner is launched the way `run_py` launches it — its own process, `python <runner> --params
<file>` with `PYTHONPATH=python/` — never imported into the test process: STUMPY's numba/OpenMP
runtime segfaults (OMP pthread_mutex_init) when it meets the OpenMP runtime earlier tests loaded.
Skipped when `app/` is absent — an external `pip install cecelia` consumer gets the IO library only.
"""
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

import numpy as np

_RUNNER = (Path(__file__).resolve().parents[3]
           / 'app' / 'src' / 'tasks' / 'behaviour' / 'motif_discovery_run.py')

N_TRACKS = 3
N_FRAMES = 60
WINDOW = 8
MOTIF_TRACK = 1
MOTIF_STARTS = (2, 16, 30, 44)   # four non-overlapping copies, separated by noise


def _run_runner(params, td):
    """Run the runner in its own process, as `run_py` does; return its results JSON."""
    pfile = os.path.join(td, 'params.json')
    with open(pfile, 'w', encoding='utf-8') as f:
        json.dump(params, f)
    env = dict(os.environ, PYTHONPATH=str(_RUNNER.parents[4] / 'python'))
    env.pop('CECELIA_PY_CONTRACT', None)
    proc = subprocess.run([sys.executable, str(_RUNNER), '--params', pfile], env=env,
                          capture_output=True, text=True, encoding='utf-8')
    if proc.returncode != 0 or not os.path.isfile(params['resultsOutPath']):
        raise AssertionError(f'runner failed ({proc.returncode}):\n{proc.stdout[-2000:]}\n{proc.stderr[-2000:]}')
    with open(params['resultsOutPath'], encoding='utf-8') as f:
        return json.load(f)


def _interleaved_pool():
    """Pooled per-cell columns, tracks interleaved round-robin: row i = track (i % 3) + 1, frame i // 3."""
    rng = np.random.default_rng(7)
    motif = np.stack([np.sin(np.linspace(0, 2 * np.pi, WINDOW)) * 5 + 6,      # speed
                      np.linspace(-1.5, 1.5, WINDOW),                          # angle
                      np.array([0, 0, 1, 1, 2, 2, 1, 0], dtype=float)], axis=1)  # hmm state
    per_track = {}
    for tid in range(1, N_TRACKS + 1):
        stream = np.stack([rng.uniform(0, 12, N_FRAMES),
                           rng.uniform(-3, 3, N_FRAMES),
                           rng.integers(0, 3, N_FRAMES).astype(float)], axis=1)
        if tid == MOTIF_TRACK:
            for s in MOTIF_STARTS:
                stream[s: s + WINDOW] = motif
        per_track[tid] = stream
    track_ids, ts, labels, feats = [], [], [], []
    for t in range(N_FRAMES):
        for tid in range(1, N_TRACKS + 1):
            track_ids.append(tid)
            ts.append(t)
            labels.append(len(labels) + 1)
            feats.append(per_track[tid][t])
    feats = np.asarray(feats)
    return track_ids, ts, labels, feats


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class MotifDiscoverySpanBroadcastTest(unittest.TestCase):
    def test_instances_land_on_their_own_track_only(self):
        track_ids, ts, labels, feats = _interleaved_pool()
        n = len(labels)
        with tempfile.TemporaryDirectory() as td:
            out = os.path.join(td, 'results.json')
            res = _run_runner({
                'suffix': 'T', 'windowSize': WINDOW, 'topK': len(MOTIF_STARTS), 'numClasses': 2,
                'resolution': 0.5, 'randomState': 0, 'resultsOutPath': out,
                'uIDs': ['img1'] * n, 'valueNames': ['T'] * n, 'labels': labels,
                'trackIds': track_ids, 'ts': ts,
                'speed': feats[:, 0].tolist(), 'angle': feats[:, 1].tolist(),
                'hmmState': feats[:, 2].tolist()}, td)

        inst = res['instance_id_by_cell']
        self.assertEqual(len(inst), n)
        labelled = [i for i in range(n) if inst[i] is not None]
        self.assertTrue(labelled, 'no cell received a motif label')
        # fails if: an instance's labels walk pooled order (start_row + k) onto other tracks
        self.assertEqual(sorted({track_ids[i] for i in labelled}), [MOTIF_TRACK],
                         'motif labels landed on tracks the motif was not found in')
        # each instance covers consecutive frames of ONE track (overlap may trim, never scatter)
        by_inst = {}
        for i in labelled:
            by_inst.setdefault(inst[i], []).append(i)
        for iid, rows in by_inst.items():
            self.assertEqual(len({track_ids[i] for i in rows}), 1, f'instance {iid} spans tracks')
            frames = sorted(ts[i] for i in rows)
            self.assertEqual(frames, list(range(frames[0], frames[0] + len(frames))),
                             f'instance {iid} is not a contiguous run of frames')
        # the four planted copies (MP == 0) are exactly what got labelled
        planted = {s + k for s in MOTIF_STARTS for k in range(WINDOW)}
        self.assertEqual(sorted(ts[i] for i in labelled), sorted(planted))


@unittest.skipUnless(_RUNNER.is_file(), f'runner not present at {_RUNNER}')
class PairwiseDtwFallbackTest(unittest.TestCase):
    """Without dtaidistance's compiled C library (the macOS wheels), the DTW matrix still comes out —
    and equal to the fast path's. Loads only `_pairwise_dtw` (no STUMPY), so in-process is safe."""

    def test_no_c_library_gives_the_same_matrix(self):
        import importlib.util
        from unittest import mock
        from dtaidistance import dtw_ndim
        from dtaidistance.exceptions import CythonException
        spec = importlib.util.spec_from_file_location('motif_discovery_run', _RUNNER)
        mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(mod)
        rng = np.random.default_rng(3)
        windows = [rng.normal(size=(WINDOW, 3)) for _ in range(5)]
        ref = mod._pairwise_dtw(windows, lambda *_: None)

        def no_c(*_a, **_k):
            raise CythonException('The compiled dtaidistance C library is not available.')
        with mock.patch.object(dtw_ndim, 'distance_matrix_fast', no_c):
            got = mod._pairwise_dtw(windows, lambda *_: None)
        np.testing.assert_allclose(got, ref, rtol=1e-6)
        self.assertTrue(np.allclose(got, got.T) and np.all(np.diag(got) == 0))


if __name__ == '__main__':
    unittest.main()
