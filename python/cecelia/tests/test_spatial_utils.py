"""Unit tests for the pure neighbourhood-composition helper
(cecelia.utils.spatial_utils.neighbourhood_composition).

Pure/headless — no squidpy, no anndata, no I/O. Pins the composition formula: per cell, the
normalised frequency of each population among its graph neighbours (the CytoMAP / cecelia "i-niche"
feature; legacy cellRegionsStats `freq = n / sum(n)`). See docs/todo/SPATIAL_REGIONS_PLAN.md."""
import unittest

import numpy as np
import scipy.sparse as sp

from cecelia.utils.spatial_utils import neighbourhood_composition, pairwise_contact_logodds


class TestNeighbourhoodComposition(unittest.TestCase):
    def _chain(self, n):
        # undirected path graph 0-1-2-...-(n-1)
        rows, cols = [], []
        for i in range(n - 1):
            rows += [i, i + 1]
            cols += [i + 1, i]
        return sp.csr_matrix((np.ones(len(rows)), (rows, cols)), shape=(n, n))

    def test_two_populations_fractions(self):
        # 4 cells in a chain; codes alternate pop 0/1. Each interior cell has 2 neighbours.
        conn = self._chain(4)
        codes = np.array([0, 1, 0, 1])
        comp = neighbourhood_composition(conn, codes, 2)
        # cell 1's neighbours are cells 0 (pop0) and 2 (pop0) → all pop0
        np.testing.assert_allclose(comp[1], [1.0, 0.0])
        # cell 2's neighbours are cells 1 (pop1) and 3 (pop1) → all pop1
        np.testing.assert_allclose(comp[2], [0.0, 1.0])
        # rows sum to 1 for non-isolated cells
        self.assertTrue(np.allclose(comp.sum(axis=1), 1.0))

    def test_mixed_neighbourhood(self):
        # star: centre 0 connected to 1(pop0), 2(pop1), 3(pop1)
        conn = sp.csr_matrix(([1, 1, 1, 1, 1, 1],
                              ([0, 1, 0, 2, 0, 3], [1, 0, 2, 0, 3, 0])), shape=(4, 4))
        codes = np.array([0, 0, 1, 1])
        comp = neighbourhood_composition(conn, codes, 2)
        np.testing.assert_allclose(comp[0], [1 / 3, 2 / 3])   # 1 of pop0, 2 of pop1

    def test_isolated_cell_all_zero(self):
        conn = sp.csr_matrix((3, 3))          # no edges
        comp = neighbourhood_composition(conn, np.array([0, 1, 0]), 2)
        self.assertTrue(np.allclose(comp, 0.0))   # isolated → all-zero row, no divide-by-zero
        self.assertEqual(comp.shape, (3, 2))


class TestPairwiseContactLogOdds(unittest.TestCase):
    def _clique(self, nodes):
        rows, cols = [], []
        for a in nodes:
            for b in nodes:
                if a < b:
                    rows += [a, b]; cols += [b, a]
        return rows, cols

    def test_two_cliques_associate_within_avoid_between(self):
        # two disjoint triangles: type 0 = {0,1,2}, type 1 = {3,4,5}, NO cross edges
        r0, c0 = self._clique([0, 1, 2])
        r1, c1 = self._clique([3, 4, 5])
        conn = sp.csr_matrix((np.ones(len(r0 + r1)), (r0 + r1, c0 + c1)), shape=(6, 6))
        codes = np.array([0, 0, 0, 1, 1, 1])
        obs, exp, lor = pairwise_contact_logodds(conn, codes, 2)
        # 3 within-type edges each, 0 cross-type
        self.assertEqual(obs[0, 0], 3); self.assertEqual(obs[1, 1], 3); self.assertEqual(obs[0, 1], 0)
        self.assertTrue(np.allclose(obs, obs.T))                    # symmetric
        # within-type association (>0), cross-type avoidance (<0)
        self.assertGreater(lor[0, 0], 0); self.assertGreater(lor[1, 1], 0)
        self.assertLess(lor[0, 1], 0)
        self.assertTrue(np.allclose(lor, lor.T))

    def test_bipartite_associates_between(self):
        # complete bipartite: every edge is cross-type → positive off-diagonal, negative within
        rows, cols = [], []
        for a in (0, 1):
            for b in (2, 3):
                rows += [a, b]; cols += [b, a]
        conn = sp.csr_matrix((np.ones(len(rows)), (rows, cols)), shape=(4, 4))
        codes = np.array([0, 0, 1, 1])
        obs, exp, lor = pairwise_contact_logodds(conn, codes, 2)
        self.assertEqual(obs[0, 1], 4); self.assertEqual(obs[0, 0], 0)
        self.assertGreater(lor[0, 1], 0); self.assertLess(lor[0, 0], 0)


if __name__ == "__main__":
    unittest.main()
