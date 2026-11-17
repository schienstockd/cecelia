"""A kNN neighbour graph is DIRECTED: a cell's k nearest need not list it back, so squidpy stores many
edges in one direction only. Every consumer that counts edges (`pairwise_contact_logodds`,
`graph_metrics`) must count a pair linked in EITHER direction, once (`undirected_edges`) — the upper
triangle alone drops the edges stored below the diagonal.

Expected values are worked out by hand from the matrices below, and, for the squidpy case, by brute
force over the stored entries."""
import unittest

import numpy as np
import scipy.sparse as sp

from cecelia.utils.spatial_utils import graph_metrics, pairwise_contact_logodds, undirected_edges


def _directed():
    # 0→1 and 1→0 (stored both ways), 2→0 and 3→1 (stored ONE way, below the diagonal), 2→2 (a loop)
    rows, cols = [0, 1, 2, 3, 2], [1, 0, 0, 1, 2]
    return sp.csr_matrix((np.ones(len(rows)), (rows, cols)), shape=(4, 4))


class TestUndirectedEdges(unittest.TestCase):
    def test_a_pair_in_either_direction_once_and_no_loops(self):
        row, col = undirected_edges(_directed())
        self.assertEqual(sorted(zip(row.tolist(), col.tolist())), [(0, 1), (0, 2), (1, 3)])

    def test_contacts_count_edges_stored_one_way(self):
        # types: 0 → {0, 1}, 1 → {2, 3}. Edges {0,1} (0–0), {0,2} (0–1), {1,3} (0–1)
        obs, _, _, _, _ = pairwise_contact_logodds(_directed(), np.array([0, 0, 1, 1]), 2, n_permutations=0)
        self.assertEqual(obs[0, 0], 1)
        self.assertEqual(obs[0, 1], 2)
        self.assertEqual(obs[1, 0], 2)
        self.assertEqual(obs[1, 1], 0)

    def test_graph_metrics_counts_distinct_pairs(self):
        import anndata as ad

        a = ad.AnnData(np.zeros((4, 1), np.float32))
        a.obsp["spatial_connectivities"] = _directed()
        m = graph_metrics(a)
        self.assertEqual(m["nEdges"], 3)
        self.assertAlmostEqual(m["meanDegree"], 6 / 4)        # degrees 2, 2, 1, 1
        self.assertEqual(m["isolatedFrac"], 0.0)

    def test_squidpy_knn_graph(self):
        # a real kNN graph: its stored entries are directed; the edge count is the brute-force set of
        # unordered pairs over them
        import anndata as ad
        from cecelia.utils.spatial_utils import build_spatial_graph

        rng = np.random.default_rng(0)
        pts = rng.uniform(0, 100, size=(60, 2))
        a = ad.AnnData(pts.astype(np.float32)); a.obsm["spatial"] = pts
        build_spatial_graph(a, method="knn", n_neighs=3)
        conn = a.obsp["spatial_connectivities"].tocoo()
        pairs = {(min(r, c), max(r, c)) for r, c in zip(conn.row.tolist(), conn.col.tolist()) if r != c}
        self.assertGreater(len(pairs), conn.nnz // 2)          # directed: more pairs than nnz / 2
        self.assertEqual(graph_metrics(a)["nEdges"], len(pairs))
        obs, _, _, _, _ = pairwise_contact_logodds(a.obsp["spatial_connectivities"], np.zeros(60, int), 1,
                                                   n_permutations=0)
        self.assertEqual(obs[0, 0], len(pairs))


if __name__ == "__main__":
    unittest.main()
