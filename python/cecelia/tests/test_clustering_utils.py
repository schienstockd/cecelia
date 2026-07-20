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


class TestSplitBackExtraObs(unittest.TestCase):
    """split_back_and_write persists the region code + the composition columns (extra_obs), sliced
    per segment and aligned by label — the enabler for the region-composition heatmap (Decision 16)."""
    def test_regions_and_composition_written_per_segment(self):
        import os, tempfile
        import anndata as ad
        import pandas as pd
        from cecelia.utils.clustering_utils import split_back_and_write
        from cecelia.utils.label_props_utils import LabelPropsView

        tmp = tempfile.mkdtemp()
        # two segmentations (B, T), 2 cells each; minimal label-props h5ad per segment
        paths = {}
        for vn, labels in (("B", [1, 2]), ("T", [1, 2])):
            p = os.path.join(tmp, f"{vn}.h5ad")
            a = ad.AnnData(X=np.zeros((2, 1), dtype=np.float64),
                           obs=pd.DataFrame(index=[str(l) for l in labels]))
            a.var_names = ["f0"]; a.write_h5ad(p); paths[vn] = p
        segments = [{"uID": "img1", "valueName": vn, "propsPath": paths[vn]} for vn in ("B", "T")]

        # pooled clustered adata: 4 cells, composition over 2 basis pops, region codes
        X = np.array([[0.8, 0.2], [0.1, 0.9], [0.7, 0.3], [0.0, 1.0]], dtype=np.float32)
        adata = ad.AnnData(X)
        adata.obs["uID"] = ["img1"] * 4
        adata.obs["valueName"] = ["B", "B", "T", "T"]
        adata.obs["label"] = [1, 2, 1, 2]
        adata.obs["clusters"] = pd.Categorical(["0", "1", "0", "1"])
        comp = {f"spatial.comp.{b}.default": X[:, j] for j, b in enumerate(["Bqc", "Tqc"])}

        qc = split_back_and_write(adata, segments, "default", col_prefix="regions", extra_obs=comp)
        self.assertEqual(qc["nClusters"], 2)

        # read back segment B: region code + BOTH composition columns present, values aligned by label
        df = LabelPropsView(paths["B"]).as_df()
        self.assertIn("regions.default", df.columns)
        self.assertIn("spatial.comp.Bqc.default", df.columns)
        self.assertIn("spatial.comp.Tqc.default", df.columns)
        row1 = df[df["label"] == 1].iloc[0]
        self.assertAlmostEqual(float(row1["spatial.comp.Bqc.default"]), 0.8, places=5)
        self.assertAlmostEqual(float(row1["spatial.comp.Tqc.default"]), 0.2, places=5)


if __name__ == '__main__':
    unittest.main()
