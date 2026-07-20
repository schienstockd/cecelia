"""Integration test for the optional batch integration in `find_populations` (Harmony).

Heavier than the pure QC tests — exercises scanpy + harmonypy on a small synthetic matrix with a
per-batch offset. Pins that `batch_key` runs Harmony (produces the corrected `X_pca_harmony` rep and
clusters over it) rather than raw X — the cohort-integration path (docs/todo/SPATIAL_REGIONS_PLAN.md).
"""
import unittest

import numpy as np


class TestBatchIntegration(unittest.TestCase):
    def test_harmony_path_runs_and_clusters(self):
        import anndata as ad
        from cecelia.utils.clustering_utils import find_populations

        rng = np.random.RandomState(0)
        n_per = 30
        # two true groups (separated in feature space), each present in two batches with an offset
        g0 = rng.normal(0.0, 0.1, size=(n_per, 5))
        g1 = rng.normal(3.0, 0.1, size=(n_per, 5))
        X = np.vstack([g0, g1]).astype(np.float32)
        batch = np.array(["s1", "s2"] * n_per)           # interleaved batches
        X[batch == "s2"] += 1.5                            # additive per-batch shift (batch effect)

        adata = ad.AnnData(X)
        adata.obs["batch"] = batch
        find_populations(adata, resolution=1.0, axis="NONE", transformation="NONE",
                         create_umap=False, backend="cpu", batch_key="batch")

        self.assertIn("X_pca_harmony", adata.obsm)         # Harmony ran
        self.assertIn("clusters", adata.obs)               # clustered over the corrected rep
        self.assertGreaterEqual(adata.obs["clusters"].nunique(), 2)

    def test_skips_when_too_few_features(self):
        import anndata as ad
        from cecelia.utils.clustering_utils import find_populations

        rng = np.random.RandomState(0)
        X = rng.random((20, 2)).astype(np.float32)         # 2 features → <2 PCs → skip integration
        adata = ad.AnnData(X)
        adata.obs["batch"] = np.array(["s1", "s2"] * 10)
        find_populations(adata, resolution=1.0, axis="NONE", transformation="NONE",
                         create_umap=False, backend="cpu", batch_key="batch")
        self.assertNotIn("X_pca_harmony", adata.obsm)      # skipped, did not crash
        self.assertIn("clusters", adata.obs)               # still clustered on raw X


if __name__ == "__main__":
    unittest.main()
