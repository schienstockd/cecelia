"""Unit tests for the explicit centroid-axis convention: the writer namer, the generic converter
(`centroid_migrate.normalise_centroids`), the fail-fast read guard, and the by-axis resolution helper.
See docs/todo/CENTROID_AXES_PLAN.md."""
import os
import tempfile
import unittest

import numpy as np
import pandas as pd
import anndata as ad

from cecelia.utils.centroid_migrate import normalise_centroids
from cecelia.utils.label_props_utils import (
    LabelPropsView, skimage_centroid_axis_names, axis_of, physical_size_for_axis,
)


def _legacy_obsm(n_spatial=3, temporal=True):
    """A Pineapple file with skimage positional labels in uns (the pre-migration shape)."""
    a = ad.AnnData(X=np.random.rand(4, 2).astype(np.float32),
                   obs=pd.DataFrame(index=[str(i) for i in range(4)]),
                   var=pd.DataFrame(index=["area", "vol"]))
    a.obsm["spatial"] = np.random.rand(4, n_spatial).astype(np.float32)
    a.uns["spatial_cols"] = np.array([f"centroid-{i}" for i in range(n_spatial)], dtype=object)
    if temporal:
        a.obsm["temporal"] = np.random.rand(4, 1).astype(np.float32)
        a.uns["temporal_cols"] = np.array(["t"], dtype=object)
    return a


def _flat_var():
    """An old-R file with flat centroid columns in var/X (no obsm)."""
    return ad.AnnData(
        X=np.random.rand(4, 5).astype(np.float32),
        obs=pd.DataFrame(index=[str(i) for i in range(4)]),
        var=pd.DataFrame(index=["area", "centroid_z", "centroid_y", "centroid_x", "centroid_t"]))


class TestWriterNamer(unittest.TestCase):
    def test_skimage_axis_names(self):
        self.assertEqual(skimage_centroid_axis_names(3), ["centroid_z", "centroid_y", "centroid_x"])
        self.assertEqual(skimage_centroid_axis_names(2), ["centroid_y", "centroid_x"])
        self.assertEqual(skimage_centroid_axis_names(0), [])
        with self.assertRaises(ValueError):
            skimage_centroid_axis_names(4)


class TestByAxisResolution(unittest.TestCase):
    def test_physical_size_for_axis(self):
        phys = [3.0, 0.5, 0.25]   # [sz, sy, sx]
        self.assertEqual(physical_size_for_axis(phys, "z"), 3.0)
        self.assertEqual(physical_size_for_axis(phys, "y"), 0.5)
        self.assertEqual(physical_size_for_axis(phys, "x"), 0.25)
        self.assertEqual(physical_size_for_axis(phys, axis_of("centroid_x")), 0.25)


class TestNormaliseCentroids(unittest.TestCase):
    def test_case_a_relabel_matrix_untouched(self):
        a = _legacy_obsm(3)
        mat = a.obsm["spatial"].copy()
        a, changes = normalise_centroids(a)
        self.assertEqual(list(a.uns["spatial_cols"]), ["centroid_z", "centroid_y", "centroid_x"])
        self.assertEqual(list(a.uns["temporal_cols"]), ["centroid_t"])
        np.testing.assert_array_equal(a.obsm["spatial"], mat)   # matrix values unchanged
        self.assertTrue(changes)
        # idempotent
        _, again = normalise_centroids(a)
        self.assertEqual(again, [])

    def test_case_a_2d(self):
        a, _ = normalise_centroids(_legacy_obsm(2, temporal=False))
        self.assertEqual(list(a.uns["spatial_cols"]), ["centroid_y", "centroid_x"])

    def test_case_b_flat_lifted(self):
        a, changes = normalise_centroids(_flat_var())
        self.assertIn("spatial", a.obsm)
        self.assertEqual(list(a.uns["spatial_cols"]), ["centroid_z", "centroid_y", "centroid_x"])
        self.assertEqual(list(a.uns["temporal_cols"]), ["centroid_t"])
        self.assertNotIn("centroid_x", list(a.var_names))       # dropped from X
        self.assertIn("area", list(a.var_names))                # feature kept
        self.assertTrue(changes)


class TestReadGuard(unittest.TestCase):
    def test_reader_rejects_legacy_then_accepts_after_convert(self):
        with tempfile.TemporaryDirectory() as tmp:
            p = os.path.join(tmp, "x.h5ad")
            _legacy_obsm(3).write_h5ad(p)
            # pre-conversion: any read that returns a centroid must fail
            with self.assertRaises(ValueError):
                LabelPropsView(p).view_centroid_cols().as_df()
            # convert, then it reads with explicit names
            a, _ = normalise_centroids(ad.read_h5ad(p)); a.write_h5ad(p)
            df = LabelPropsView(p).view_centroid_cols().as_df()
            for c in ("centroid_z", "centroid_y", "centroid_x", "centroid_t"):
                self.assertIn(c, df.columns)


if __name__ == "__main__":
    unittest.main()
