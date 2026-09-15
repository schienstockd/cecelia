"""Unit tests for the legacy → Feijoa h5ad rewrite (cecelia.utils.legacy_migrate).

Builds tiny synthetic AnnData files (no R, no zarr) covering the two legacy shapes and pins the
schema conversion: index←label, centroids var→obsm (only when absent), and dropping the excluded
(HMM/clustering) columns while keeping segmentation+tracking.
"""
import errno
import os
import tempfile
import unittest
import unittest.mock
from pathlib import Path

import numpy as np
import pandas as pd
import anndata as ad

import cecelia.utils.legacy_migrate as lm
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


class TestRmtreeRetry(unittest.TestCase):
    # macOS APFS occasionally raises ENOTEMPTY for a Zarr chunk dir whose files were just unlinked —
    # the fd-based walker races volume metadata coalescing. Bit a real user on the first migrate
    # attempt (v0.2.3). The retry MUST distinguish a transient ENOTEMPTY (recovers on retry) from
    # a real one (persists across every attempt), and MUST NOT swallow other OSErrors.
    def test_succeeds_when_the_directory_is_gone_first_try(self):
        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "sub"
            p.mkdir()
            (p / "f").write_text("x", encoding="utf-8")
            lm._rmtree_robust(p)
            self.assertFalse(p.exists())

    def test_retries_a_transient_ENOTEMPTY(self):
        calls = {"n": 0}
        real_rmtree = lm.shutil.rmtree

        def flaky(path, *a, **kw):
            calls["n"] += 1
            if calls["n"] == 1:
                raise OSError(errno.ENOTEMPTY, "Directory not empty", str(path))
            return real_rmtree(path, *a, **kw)

        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "sub"
            p.mkdir()
            (p / "f").write_text("x", encoding="utf-8")
            with unittest.mock.patch.object(lm.shutil, "rmtree", flaky):
                lm._rmtree_robust(p, retries=3, delay=0.0)
            self.assertEqual(calls["n"], 2)
            self.assertFalse(p.exists())

    def test_reraises_a_persistent_ENOTEMPTY(self):
        def always_fails(path, *a, **kw):
            raise OSError(errno.ENOTEMPTY, "Directory not empty", str(path))

        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "sub"; p.mkdir()
            with unittest.mock.patch.object(lm.shutil, "rmtree", always_fails):
                with self.assertRaises(OSError) as cm:
                    lm._rmtree_robust(p, retries=3, delay=0.0)
                self.assertEqual(cm.exception.errno, errno.ENOTEMPTY)

    def test_does_not_retry_a_different_OSError(self):
        calls = {"n": 0}

        def perm_denied(path, *a, **kw):
            calls["n"] += 1
            raise OSError(errno.EACCES, "Permission denied", str(path))

        with tempfile.TemporaryDirectory() as tmp:
            p = Path(tmp) / "sub"; p.mkdir()
            with unittest.mock.patch.object(lm.shutil, "rmtree", perm_denied):
                with self.assertRaises(OSError) as cm:
                    lm._rmtree_robust(p, retries=5, delay=0.0)
                self.assertEqual(cm.exception.errno, errno.EACCES)
            self.assertEqual(calls["n"], 1)   # no retry


class TestRHelperCoLocated(unittest.TestCase):
    # read_rds resolves the R helper via Path(__file__).with_name(...). A tree refactor once moved
    # the helper out from beside the module and Rscript then died with exit 2 ("cannot open input
    # script") at first scan — no test caught it because no test exercised read_rds end-to-end.
    def test_read_ccid_rds_r_ships_beside_module(self):
        helper = Path(lm.__file__).with_name("read_ccid_rds.R")
        self.assertTrue(helper.is_file(),
                        f"read_ccid_rds.R must ship next to legacy_migrate.py (looked at {helper})")


if __name__ == "__main__":
    unittest.main()
