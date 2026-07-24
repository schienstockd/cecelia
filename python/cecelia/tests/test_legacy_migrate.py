"""Unit tests for the legacy → Pineapple h5ad rewrite (cecelia.utils.legacy_migrate).

Builds tiny synthetic AnnData files (no R, no zarr) covering the two legacy shapes and pins the
schema conversion: index←label, centroids var→obsm (only when absent), and dropping the excluded
(HMM/clustering) columns while keeping segmentation+tracking.
"""
import os
import tempfile
import unittest

import numpy as np
import pandas as pd
import anndata as ad

from cecelia.utils.legacy_migrate import migrate_h5ad


def _write(tmp, adata):
    p = os.path.join(tmp, "in.h5ad")
    adata.write_h5ad(p)
    return p


class TestMigrateH5ad(unittest.TestCase):
    def test_old_var_centroids_lifted_and_index_set(self):
        # legacy "older" file: label as an obs column (1-based), centroids inside the matrix.
        with tempfile.TemporaryDirectory() as tmp:
            X = pd.DataFrame({
                "mean_intensity_0": [1.0, 2.0, 3.0],
                "centroid_z": [0.0, 1.0, 2.0],
                "centroid_y": [10.0, 11.0, 12.0],
                "centroid_x": [20.0, 21.0, 22.0],
                "centroid_t": [0.0, 0.0, 1.0],
            })
            obs = pd.DataFrame({
                "label": [1, 2, 3],
                "track_id": [5, 5, 6],
                "live.cell.speed": [0.1, 0.2, 0.3],
                "live.cell.hmm.state.movement": [1, 2, 1],   # excluded
                "live.cell.track.clusters.x": [0, 1, 0],     # excluded (clust)
            })
            a = ad.AnnData(X=X.to_numpy(dtype=np.float32),
                           obs=obs, var=pd.DataFrame(index=list(X.columns)))
            dst = os.path.join(tmp, "out.h5ad")
            summary = migrate_h5ad(_write(tmp, a), dst)
            out = ad.read_h5ad(dst)

            self.assertTrue(summary["index_set_from_label"])
            self.assertEqual(list(out.obs_names), ["1", "2", "3"])      # index = label
            self.assertNotIn("label", out.obs.columns)
            self.assertIn("spatial", out.obsm)                          # centroids lifted
            self.assertEqual(list(out.uns["spatial_cols"]), ["centroid_z", "centroid_y", "centroid_x"])
            self.assertIn("temporal", out.obsm)
            self.assertEqual(list(out.uns["temporal_cols"]), ["centroid_t"])
            self.assertNotIn("centroid_z", list(out.var_names))         # removed from matrix
            self.assertIn("track_id", out.obs.columns)                  # tracking kept
            self.assertIn("live.cell.speed", out.obs.columns)
            self.assertNotIn("live.cell.hmm.state.movement", out.obs.columns)   # hmm dropped
            self.assertNotIn("live.cell.track.clusters.x", out.obs.columns)     # clust dropped

    def test_legacy_obsm_names_relabelled(self):
        # legacy "newer" file: already obsm, but skimage positional labels — matrix stays, uns relabels.
        with tempfile.TemporaryDirectory() as tmp:
            a = ad.AnnData(
                X=np.array([[1.0], [2.0]], dtype=np.float32),
                obs=pd.DataFrame({"label": [7, 9], "track_id": [1, 1]}),
                var=pd.DataFrame(index=["mean_intensity_0"]),
            )
            a.obsm["spatial"] = np.array([[0, 1, 2], [3, 4, 5]], dtype=np.float32)
            a.uns["spatial_cols"] = np.array(["centroid-0", "centroid-1", "centroid-2"], dtype=object)
            dst = os.path.join(tmp, "out.h5ad")
            summary = migrate_h5ad(_write(tmp, a), dst)
            out = ad.read_h5ad(dst)

            self.assertEqual(list(out.obs_names), ["7", "9"])           # index = label
            self.assertEqual(out.obsm["spatial"].shape, (2, 3))         # matrix untouched
            self.assertEqual(list(out.uns["spatial_cols"]),
                             ["centroid_z", "centroid_y", "centroid_x"])   # relabelled to explicit
            self.assertTrue(summary["centroids_lifted"])                # recorded the relabel


if __name__ == "__main__":
    unittest.main()
