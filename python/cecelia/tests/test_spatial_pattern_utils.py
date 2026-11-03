"""Tests for the spatial-pattern statistics (cecelia.utils.spatial_pattern_utils) —
docs/todo/SPATIAL_PATTERN_PLAN.md Decisions 2–4."""
import math
import unittest

import numpy as np

from cecelia.utils.spatial_pattern_utils import (
    _expected_pairs, _frame_stats, clark_evans_r, pattern_test)
from cecelia.utils.atomic_io import json_safe
from cecelia.utils.spatial_utils import permutation_groups


def _poisson(n, d=2, seed=0, side=1000.0):
    return np.random.default_rng(seed).uniform(0, side, size=(n, d))


class TestNullCalibration(unittest.TestCase):
    def test_random_labels_read_as_random(self):
        """Targets drawn at random from the reference: ratio ≈ 1 and p roughly uniform."""
        coords = _poisson(1500)
        ps, ratios = [], []
        for s in range(30):
            mask = np.zeros(1500, bool)
            mask[np.random.default_rng(100 + s).choice(1500, 200, replace=False)] = True
            out = pattern_test(coords, mask, n_permutations=99, random_state=s)
            ps.append(out["pValue"]); ratios.append(out["nnRatio"])
        self.assertAlmostEqual(float(np.mean(ratios)), 1.0, delta=0.03)
        # 30 null datasets at α=0.05: expect ~1.5 rejections; 6+ would mean a mis-calibrated null
        self.assertLessEqual(sum(p < 0.05 for p in ps), 5)

    def test_clustered_target_reads_as_clustered(self):
        """Targets = the reference nodes inside two small discs (a follicle-like pattern)."""
        coords = _poisson(3000, seed=1)
        centres = np.array([[250.0, 250.0], [700.0, 600.0]])
        d = np.min(np.linalg.norm(coords[:, None, :] - centres[None], axis=2), axis=1)
        mask = d < 120
        out = pattern_test(coords, mask, radii=np.linspace(10, 200, 20), n_permutations=199)
        self.assertLess(out["nnRatio"], 0.8)
        self.assertAlmostEqual(out["pValue"], 1 / 200)          # the +1-corrected floor
        self.assertTrue(all(g > 1.5 for g in out["pairRatio"][-5:]))   # clustered at large r
        self.assertTrue(all(g > h for g, h in zip(out["pairRatio"][5:], out["envHi"][5:])))


class TestPairExpectation(unittest.TestCase):
    def test_analytic_expectation_equals_shuffle_mean(self):
        """pairs_ref(r)·k(k−1)/(n(n−1)) is the exact mean of the shuffle, not an approximation."""
        # small counts on purpose: the naive k²/n² differs from k(k−1)/(n(n−1)) by ~12% here
        coords = _poisson(60, seed=2)
        mask = np.zeros(60, bool); mask[:8] = True
        radii = np.array([150.0, 300.0, 500.0])
        frames = permutation_groups(np.arange(60))
        exp = _expected_pairs(frames, coords, mask, radii)
        rng = np.random.default_rng(3)
        sims = np.array([_frame_stats(frames, coords, rng.permutation(mask), radii)[2]
                         for _ in range(10000)])
        np.testing.assert_allclose(sims.mean(axis=0), exp, rtol=0.04)


class TestClarkEvans(unittest.TestCase):
    def test_square_lattice_golden(self):
        """m×m lattice, spacing a: observed NN = a, hull area = ((m−1)a)², λ = m²/area, so
        R = a / (0.5/√λ) = 2·m/(m−1) exactly."""
        m, a = 40, 5.0
        g = np.stack(np.meshgrid(np.arange(m) * a, np.arange(m) * a), -1).reshape(-1, 2)
        mask = np.ones(len(g), bool)
        r, size, d = clark_evans_r(g, mask, permutation_groups(np.arange(len(g))))
        self.assertEqual(d, 2)
        self.assertAlmostEqual(size, ((m - 1) * a) ** 2, places=6)
        self.assertAlmostEqual(r, 2 * m / (m - 1), places=6)

    def test_poisson_near_one_in_2d_and_3d(self):
        for d, n in ((2, 8000), (3, 8000)):
            coords = _poisson(n, d=d, seed=4)
            mask = np.zeros(n, bool); mask[::2] = True
            r, _, dims = clark_evans_r(coords, mask, permutation_groups(np.arange(n)))
            self.assertEqual(dims, d)
            self.assertAlmostEqual(r, 1.0, delta=0.05)   # no edge correction → slightly > 1

    def test_sparse_frames_are_not_scored(self):
        """Six reference cells per frame: a hull that small makes R meaningless, so none is reported."""
        rng = np.random.default_rng(10)
        coords = rng.uniform(0, 1000, size=(60, 3)); times = np.repeat(np.arange(10), 6)
        mask = np.tile([True, True, True, False, False, False], 10)
        r, _, d = clark_evans_r(coords, mask, permutation_groups(np.arange(60), times))
        self.assertTrue(math.isnan(r)) and self.assertEqual(d, 0)
        r2, _, _ = clark_evans_r(coords, mask, permutation_groups(np.arange(60), times), min_reference=3)
        self.assertGreater(r2, 1.5)                    # what the unguarded number looked like

    def test_flat_axis_is_dropped(self):
        coords = np.column_stack([np.zeros(2000), _poisson(2000, seed=5)])   # z = 0 everywhere
        mask = np.zeros(2000, bool); mask[::3] = True
        r, _, dims = clark_evans_r(coords, mask, permutation_groups(np.arange(2000)))
        self.assertEqual(dims, 2)
        self.assertTrue(math.isfinite(r))


class TestWithinFrame(unittest.TestCase):
    def test_shuffle_never_moves_a_label_across_frames(self):
        """Frame 0's nodes are ALL target, frame 1's none. Within-frame shuffling cannot change that,
        so every null equals the observation: ratio exactly 1, p = 1. A global shuffle would not."""
        a = _poisson(300, seed=6); b = _poisson(300, seed=7)
        coords = np.vstack([a, b])
        times = np.r_[np.zeros(300), np.ones(300)]
        mask = np.r_[np.ones(300, bool), np.zeros(300, bool)]
        out = pattern_test(coords, mask, times=times, n_permutations=50)
        self.assertEqual(out["nFrames"], 2)
        self.assertAlmostEqual(out["nnRatio"], 1.0, places=12)
        self.assertEqual(out["pValue"], 1.0)
        glob = pattern_test(coords, mask, times=None, n_permutations=50)
        self.assertNotAlmostEqual(glob["nnRatio"], 1.0, places=3)


class TestEdgeCases(unittest.TestCase):
    def test_skipped_permutations_and_json(self):
        coords = _poisson(100, seed=8); mask = np.zeros(100, bool); mask[:10] = True
        out = pattern_test(coords, mask, radii=np.array([50.0]), n_permutations=0)
        self.assertTrue(math.isnan(out["nnRatio"]))
        safe = json_safe(out)
        self.assertIsNone(safe["nnRatio"]); self.assertIsNone(safe["envLo"][0])

    def test_single_target(self):
        coords = _poisson(50, seed=9); mask = np.zeros(50, bool); mask[0] = True
        out = pattern_test(coords, mask, n_permutations=10)
        self.assertTrue(math.isnan(out["nnObserved"]))
        self.assertEqual(out["nTarget"], 1)


if __name__ == "__main__":
    unittest.main()
