"""Unit tests for the pure clustering QC helper (cecelia.utils.clustering_utils.cluster_seg_stats).

Pure/headless — no scanpy, no anndata, no I/O. Pins the per-segment cluster distribution the QC
sidecar banks: point count, distinct clusters occupied, and the largest-cluster fraction
(dominance). See qc.jl cluster_qc_findings + COHORT_METRICS."""
import unittest

import numpy as np

from cecelia.utils.clustering_utils import cluster_seg_stats


class TestClusterSegStats(unittest.TestCase):
    def test_even_spread(self):
        codes = np.array([0, 0, 1, 1, 2, 2])
        s = cluster_seg_stats(codes)
        self.assertEqual(s["n"], 6)
        self.assertEqual(s["nClusters"], 3)
        self.assertAlmostEqual(s["largestClusterFrac"], 2 / 6)

    def test_single_cluster(self):
        s = cluster_seg_stats(np.zeros(10, dtype=int))
        self.assertEqual(s["nClusters"], 1)
        self.assertAlmostEqual(s["largestClusterFrac"], 1.0)

    def test_dominant_cluster(self):
        codes = np.array([0] * 9 + [1])          # 90% in cluster 0
        s = cluster_seg_stats(codes)
        self.assertEqual(s["nClusters"], 2)
        self.assertAlmostEqual(s["largestClusterFrac"], 0.9)

    def test_empty(self):
        s = cluster_seg_stats(np.array([], dtype=int))
        self.assertEqual(s, {"n": 0, "nClusters": 0, "largestClusterFrac": 0.0})

    def test_non_contiguous_codes(self):
        # cluster ids need not be 0..N — count distinct
        s = cluster_seg_stats(np.array([3, 3, 7, 7, 7]))
        self.assertEqual(s["nClusters"], 2)
        self.assertAlmostEqual(s["largestClusterFrac"], 3 / 5)


if __name__ == '__main__':
    unittest.main()
